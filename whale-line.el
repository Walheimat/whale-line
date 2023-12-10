;;; whale-line.el --- A whale-based mode-line -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.8.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; This package provides a highly modular mode-line. The `whale-line'
;; mode-line comprises a set of segments. Each of them can be freely
;; positioned or disabled by setting `whale-line-segments' (segments
;; belong either to the left or the right side). Many are
;; customizable.
;;
;; You can also create augments to change the behavior or look of an
;; existing segment.
;;
;; You may also create your own segments and augments using the
;; `whale-line-create-*' macros.
;;
;; This package takes inspiration from two other great custom
;; mode-lines: `mood-line' and `doom-modeline'.

;;; Code:

(require 'cl-lib)

;;; -- Customization

(defgroup whale-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom whale-line-segments '(major-mode
                                 buffer-identification
                                 org
                                 buffer-status
                                 position
                                 lsp
                                 debug
                                 selection
                                 process
                                 window-status
                                 |
                                 misc-info
                                 minor-modes
                                 project
                                 vc
                                 tab-bar
                                 animation)
  "Segments shown in the mode line.

Note that all symbols before symbol `|' are shown on the left,
others on the right."
  :group 'whale-line
  :type '(repeat symbol))

(defcustom whale-line-segment-strategy 'prioritize
  "Strategy used when lack of space prohibits displaying all segments.

Strategy `prioritize' filters out segments with low priority.
Strategy `elide' only displays the left side. Strategy `ignore'
will display both sides unchanged no matter the space
constraints."
  :group 'whale-line
  :type '(choice (const prioritize)
                 (const elide)
                 (const ignore)))

(defcustom whale-line-log nil
  "Whether to log."
  :group 'whale-line
  :type 'boolean)

;;; -- Variables

(defvar whale-line--segments nil)
(defvar whale-line--props nil)

(defvar whale-line--current-window nil)
(defvar whale-line--default-mode-line nil)

(defvar whale-line--padded-cache (make-hash-table))
(defvar whale-line--space-cache (make-hash-table))

(defvar whale-line--stateful-timer nil)

(defvar whale-line-setup-hook nil)
(defvar whale-line-teardown-hook nil)

;;; -- Faces

(defface whale-line-neutral
  '((t))
  "Neutral face."
  :group 'whale-line)

(defface whale-line-shadow
  '((t (:inherit (shadow))))
  "Shadow face."
  :group 'whale-line)

(defface whale-line-highlight
  '((t (:inherit (mode-line-highlight))))
  "Face used for highlight."
  :group 'whale-line)

(defface whale-line-indicate
  '((t :inherit (success)))
  "Face used for indicating (something good)."
  :group 'whale-line)

(defface whale-line-emphasis
  '((t (:inherit (mode-line-emphasis))))
  "Face used for emphasis."
  :group 'whale-line)

(defface whale-line-contrast
  '((t (:inherit (warning))))
  "Face used for contrast."
  :group 'whale-line)

(defface whale-line-notification
  '((t (:inherit (compilation-info))))
  "Face used for notification."
  :group 'whale-line)

(defface whale-line-urgent
  '((t (:inherit (compilation-error))))
  "Face used for urgency."
  :group 'whale-line)

;;; -- Formatting

(defun whale-line--format ()
  "Return a list of aligned left and right segments.

If there's not enough space, only shows the left segments and an
ellipsis."
  (pcase whale-line-segment-strategy
    ('ignore
     (whale-line--format-ignore))
    ('elide
     (whale-line--format-elide))
    ('prioritize
     (whale-line--format-prioritize))))

(defun whale-line--format-elide ()
  "Format mode line, eliding right side if space is lacking."
  (let ((lhs (whale-line--render :left))
        (rhs (whale-line--render :right))
        (rlen (length (whale-line--format-side :right)))
        (space? (whale-line--enough-space-p)))
    `(,@lhs
      ,(whale-line--space-between (if space? rlen 5))
      ,@(if space?
            rhs
          '((:eval (propertize (concat (whale-line--spacer) "..." (whale-line--spacer))
                               'face 'whale-line-shadow)))))))

(defun whale-line--format-prioritize ()
  "Format mode line, prioritizing certain segments if space is lacking."
  (if (whale-line--enough-space-p)
      (whale-line--format-ignore)
    (let ((lhs (whale-line--render :left t))
          (rhs (whale-line--render :right t))
          (rlen (length (whale-line--format-side :right t))))
      `(,@lhs ,(whale-line--space-between rlen) ,@rhs))))

(defun whale-line--format-ignore ()
  "Format mode line ignoring space constraints."
  (let ((lhs (whale-line--render :left))
        (rhs (whale-line--render :right))
        (rlen (length (whale-line--format-side :right))))
    `(,@lhs ,(whale-line--space-between rlen) ,@rhs)))

(defun whale-line--format-side (side &optional filter)
  "Get the formatted SIDE.

Optionally FILTER out low priority segments."
  (format-mode-line (whale-line--render side filter)))

;;; -- Space calculation

(defun whale-line--calculate-space ()
  "Calculate space constraints for all visible windows.

This will cache the results so the calculation is only done once
per window configuration change."
  (let ((windows (window-list-1 nil 'never 'visible)))

    (dolist (win windows)
      (with-selected-window win
        (let* ((remaining (whale-line--calculate-remaining-space)))

          (puthash win remaining whale-line--space-cache))))))

(defun whale-line--calculate-width (side)
  "Calculate the width for SIDE.

This uses `string-pixel-width' for Emacs 29+, otherwise
`window-font-width.'"
  (let ((formatted (whale-line--format-side side 'none)))

    (if (fboundp 'string-pixel-width)
        (string-pixel-width formatted)
      (* (window-font-width) (length formatted)))))

(defun whale-line--calculate-remaining-space ()
  "Calculate the space remaining between left and right side."
  (let* ((left (whale-line--calculate-width :left))
         (right (whale-line--calculate-width :right)))

    (- (window-pixel-width) (+ left right))))

(defun whale-line--enough-space-p ()
  "Calculate whether there is enough space to display both sides' segments."
  (let ((space (whale-line--space)))

    (> space 0)))

(defun whale-line--space ()
  "Get the available space between the sides."
  (or (gethash (selected-window) whale-line--space-cache)
      (let* ((remaining (whale-line--calculate-remaining-space)))

        (puthash (selected-window) remaining whale-line--space-cache)
        remaining)))

(defun whale-line--space-between (length)
  "Get the space between sides aligned using LENGTH."
  (propertize
   " "
   'display
   `((space :align-to (- right (- 0 right-margin) ,length)))))

;;; -- Windows

(defun whale-line--get-current-window ()
  "Get the current window but should exclude the child windows."
  (if (and (fboundp 'frame-parent) (frame-parent))
      (frame-selected-window (frame-parent))
    (frame-selected-window)))

(defun whale-line--set-selected-window (&rest _)
  "Set selected window appropriately."
  (let ((win (or (minibuffer-selected-window)
                 (whale-line--get-current-window))))

    (setq whale-line--current-window win)))

(defun whale-line--is-current-window-p ()
  "Check if the current window is the selected window."
  (and whale-line--current-window
       (eq (whale-line--get-current-window) whale-line--current-window)))

;;; -- Caches

(defun whale-line--clear-caches ()
  "Clear all caches."
  (clrhash whale-line--padded-cache)
  (clrhash whale-line--space-cache))

;;; -- Building segments

(defvar whale-line--last-build nil)
(defvar whale-line--rebuilding nil)

(defun whale-line--handle-build-difference ()
  "Handle the difference between the last two builds."
  (when whale-line--last-build
    (let ((added (cl-set-difference whale-line-segments whale-line--last-build))
          (removed (cl-set-difference whale-line--last-build whale-line-segments)))

      (when added
        (whale-line--log "Added segment(s) %s since last build" added)

        (let ((whale-line--rebuilding t)
              (whale-line-segments added))

          (run-hooks 'whale-line-setup-hook)))

      (when removed
        (whale-line--log "Removed segment(s) %s since last build" removed)

        (let ((whale-line--rebuilding t)
              (whale-line-segments removed))

          (run-hooks 'whale-line-teardown-hook)))))

  (setq whale-line--last-build whale-line-segments))

(defun whale-line--build-segments ()
  "Build the segments."
  (require 'whale-line-segments)

  (whale-line--handle-build-difference)

  (whale-line--clear-caches)

  (and-let* (((memq '| whale-line-segments))
             (break (cl-position '| whale-line-segments))
             (left (seq-filter #'whale-line--valid-segment-p
                              (cl-subseq whale-line-segments 0 break)))
             (right (seq-filter #'whale-line--valid-segment-p
                                (cl-subseq whale-line-segments (1+ break))))
             (props (list :left left :right right)))

    (setq whale-line--segments props)))

(defun whale-line--trigger-augments (&optional arg)
  "Trigger augments.

Sets up augments. If ARG is t tears them down instead."
  (let ((whale-line-segments (whale-line--segments-by-type 'augment)))

    (if arg
        (run-hooks 'whale-line-teardown-hook)
      (run-hooks 'whale-line-setup-hook))))

(defun whale-line--valid-segment-p (segment)
  "Check that SEGMENT can be included."
  (let ((verify-sym (whale-line-symbol--verify segment)))

    (if (fboundp verify-sym)
        (funcall verify-sym)
      t)))

;;; -- Props

(defun whale-line--set-props (segment type &optional priority dense padded)
  "Set props for SEGMENT.

These are its TYPE, PRIORITY, as well as whether it is DENSE
and/or PADDED."
  (if-let ((props (list :type type :priority (or priority t) :dense dense :padded padded))
           (existing (assoc segment whale-line--props)))
      (setcdr existing props)
    (push (cons segment props) whale-line--props)))

(defun whale-line--prop (segment prop)
  "Get PROP for SEGMENT."
  (plist-get (cdr-safe (assoc segment whale-line--props)) prop))

;; -- Symbol creation

(defun whale-line-symbol--action (name)
  "Get the symbol for action NAME."
  (intern (format "whale-line-%s--action" (symbol-name name))))

(defun whale-line-symbol--segment (name)
  "Get the symbol for segment NAME."
  (intern (format "whale-line-%s--segment" (symbol-name name))))

(defun whale-line-symbol--verify (name)
  "Get the symbol for verifying NAME."
  (intern (format "whale-line-%s--verify" (symbol-name name))))

(defun whale-line-symbol--setup (name)
  "Get the symbol for setting up NAME."
  (intern (format "whale-line-%s--setup" (symbol-name name))))

(defun whale-line-symbol--teardown (name)
  "Get the symbol for tearing down NAME."
  (intern (format "whale-line-%s--teardown" (symbol-name name))))

(defun whale-line-symbol--get-segment (name)
  "Get the symbol for getting segment NAME."
  (intern (format "whale-line-%s--get-segment" (symbol-name name))))

(defun whale-line-symbol--port (name)
  "Get the symbol for getting port for NAME."
  (intern (format "whale-line-%s--port" (symbol-name name))))

;;; -- Macros

(cl-defmacro whale-line--setup (name &key setup teardown hooks advice verify)
  "Create setup for NAME.

HOOKS is a list of functions that will call the setter.

ADVICE is a cons cell of the form (combinator .
functions-to-advise) that will also call the setter.

SETUP is the function called on setup. TEARDOWN is the function
called on teardown.

If VERIFY is t, the verify function will be called before
execution of the setup. If verification fails, the function will
return early."
  (declare (indent defun))

  (let ((setter-sym (whale-line-symbol--action name))
        (setup-sym (whale-line-symbol--setup name))
        (teardown-sym (whale-line-symbol--teardown name))
        (verify-sym (whale-line-symbol--verify name)))

    `(progn
       (cl-defun ,setup-sym (&rest _)
         ,(format "Set up %s segment." name)
         (unless ,(if verify
                      `(and (not whale-line--rebuilding) (,verify-sym))
                    `(memq ',name whale-line-segments))
           (cl-return-from ,setup-sym))
         ,@(let* ((setups (delq
                           nil
                           `(,@(mapcar (lambda (it)
                                         `(add-hook ',it #',setter-sym))
                                       hooks)

                             ,@(mapcar (lambda (it)
                                         `(advice-add ',it ,(car advice) #',setter-sym))
                                       (cdr advice))

                             ,(when setup (if (symbolp setup)
                                              `(funcall ',setup)
                                            `(funcall ,setup))))))

                  (log (if setups `(whale-line--log ,(format "Setting up %s (%%s)" name)
                                                    (whale-line--prop ',name :type))
                         `(whale-line--log ,(format "Would set up %s (%%s)" name)
                                           (whale-line--prop ',name :type)))))

             (if setups
                 `(,log ,@setups)
               `(,log))))

       (add-hook 'whale-line-setup-hook #',setup-sym)

       (cl-defun ,teardown-sym (&rest _)
         ,(format "Tear down %s segment." name)
         (unless ,(if verify
                      `(and (not whale-line--rebuilding) (,verify-sym))
                    `(memq ',name whale-line-segments))
           (cl-return-from ,teardown-sym))
         ,@(let* ((teardowns (delq
                              nil
                              `(,@(mapcar (lambda (it)
                                            `(remove-hook ',it #',setter-sym))
                                          hooks)

                                ,@(mapcar (lambda (it)
                                            `(advice-remove ',it #',setter-sym))
                                          (cdr advice))

                                ,(when teardown
                                   (if (symbolp teardown)
                                       `(funcall ',teardown)
                                     `(funcall ,teardown))))))
                  (log (if teardowns `(whale-line--log ,(format "Tearing down %s (%%s)" name)
                                                       (whale-line--prop ',name :type))
                         `(whale-line--log ,(format "Would tear down %s (%%s)" name)
                                           (whale-line--prop ',name :type)))))
             (if teardowns
                 `(,log ,@teardowns)
               `(,log))))

       (add-hook 'whale-line-teardown-hook #',teardown-sym))))

(cl-defmacro whale-line--function (sym fun docs &optional apply)
  "Create a function using symbol SYM.

The function will call FUN if it is a symbol. DOCS are used for
the docstring. If APPLY is t, use `apply' instead of `funcall'."
  (declare (indent defun))

  `(defun ,sym (&rest ,(if apply 'args '_args))
     ,docs
     ,(if apply
          (if (symbolp fun)
              `(apply ',fun args)
            `(apply ,fun args))
        (if (symbolp fun)
            `(funcall ',fun)
          `(funcall ,fun)))))

(defmacro whale-line--omit (name type)
  "Indicate that segment NAME was omitted.

TYPE controls what is emitted: a variable for stateful segments, a
function returning the empty string for stateless segments and
nothing for augments."
  (let ((segment-sym (whale-line-symbol--segment name)))
    `(progn
       ,(pcase type
          ('stateless
           `(defun ,segment-sym ()
              ,(format "Render `%s' segment as an empty string." name)
              ""))
          ('stateful
           `(defvar ,segment-sym nil))
          (_ nil))
       (unless (bound-and-true-p whale-line--testing)
         (message "Couldn't add %s `%s' segment" ',type ',name)))))

(cl-defmacro whale-line--create-stateful-segment
    (name
     &key
     getter
     hooks
     advice
     after
     verify
     setup
     teardown
     priority
     dense
     padded
     port)
  "Create a stateful segment named NAME.

Stateful segments are represented by a variable that is updated
by hooks or advice.

GETTER is the form to evaluate to get the string (the setter is
constructed by the macro).

HOOKS is a list of functions that will call the setter.

ADVICE is a cons cell of the form (COMBINATOR .
FUNCTIONS-TO-ADVISE) that will also call the setter. You're
likely best served using AFTER instead.

VERIFY is a function called before the segments are built. If it
returns nil, the segment will not be included.

SETUP is the function called on setup, TEARDOWN that during teardown.

This will also add the segment with PRIORITY or t.

If DENSE is t, the segment will not be padded.

PADDED can be either `left', `right' or `all' to document that
the segment comes pre-padded on that or all sides.

PORT is a function adapters can call to interact with the
segment."
  (declare (indent defun))

  (let* ((segment (whale-line-symbol--segment name))
         (setter (whale-line-symbol--action name))
         (getter-sym (whale-line-symbol--get-segment name))
         (verify-sym (whale-line-symbol--verify name))
         (port-sym (whale-line-symbol--port name))
         (prio (or priority t))
         (advice (cond
                  ((not (null after))
                   `(:after . (,@(whale-line--normalize-list after))))
                  (t advice))))

    (if (not (bound-and-true-p whale-line--testing))
        `(progn
           (whale-line--set-props ',name 'stateful ',prio ',dense ',padded)

           (defvar-local ,segment 'initial)

           (defun ,setter (&rest _)
             ,(format "Set %s segment." name)
             (if-let ((str (,getter-sym)))
                 (setq ,segment str)
               (setq ,segment nil)))

           ,@(delq
              nil
              `((whale-line--function ,getter-sym ,getter ,(format "Get the %s segment." name))
                (whale-line--setup ,name :setup ,setup :advice ,advice :hooks ,(whale-line--normalize-list hooks) :teardown ,teardown :verify ,(not (null verify)))
                ,(when verify
                   `(whale-line--function ,verify-sym ,verify ,(format "Verify `%s' segment." name) t))
                ,(when port
                   `(whale-line--function ,port-sym ,port ,(format "Plug into `%s'" port) t)))))
      `(progn
         (whale-line--omit ,name stateful)))))

(cl-defmacro whale-line--create-stateless-segment
    (name
     &key
     getter
     var
     condition
     verify
     setup
     teardown
     priority
     dense
     padded
     port)
  "Create a stateless segment name NAME.

A stateless segment is represented by a function that is called
on every mode-line update.

GETTER is the function to call on render. Alternatively VAR can
be used to return a variable.

CONDITION is the condition to evaluate before calling the
renderer.

VERIFY is a function called before the segments are built. If it
returns nil, the segment will not be included.

SETUP is the function called on setup, TEARDOWN that during teardown.

The segment will be added with PRIORITY or t.

If DENSE is t, the segment will not be padded.

PADDED can be either `left', `right' or `all' to document that
the segment comes pre-padded on that or all sides.

PORT is a function adapters can call to interact with the
segment."
  (declare (indent defun))

  (let ((segment (whale-line-symbol--segment name))
        (getter-sym (whale-line-symbol--get-segment name))
        (verify-sym (whale-line-symbol--verify name))
        (port-sym (whale-line-symbol--port name))
        (prio (or priority t))
        (con (or condition t)))

    (if (not (bound-and-true-p whale-line--testing))
        `(progn
           (whale-line--set-props ',name 'stateless ',prio ',dense ',padded)
           (defun ,segment ()
             ,(format "Render `%s' segment." name)
             (or (when ,con
                   ,(if var `,var `(,getter-sym)))
                 ""))
           ,@(delq
              nil
              `(,(unless var
                   `(whale-line--function ,getter-sym ,getter ,(format "Get the `%s' segment." name)))
                (whale-line--setup ,name :setup ,setup :teardown ,teardown :verify ,(not (null verify)))
                ,(when verify
                   `(whale-line--function ,verify-sym ,verify ,(format "Verify `%s' segment." name) t))
                ,(when port
                   `(whale-line--function ,port-sym ,port ,(format "Plug into `%s'." port) t)))))
      `(progn
         (whale-line--omit ,name stateless)))))

(cl-defmacro whale-line--create-augment
    (name
     &key
     action
     hooks
     advice
     after
     after-while
     setup
     teardown
     verify
     plugs-into)
  "Create augment(-or) named NAME.

ACTION is the function to call for HOOKS.

ADVICE is an cons cell of the form (ADVICE-COMBINATOR .
FUNC-LIST). Each function in FUNC-LIST will be advised using
ADIVCE-COMBINATOR to call ACTION. You are probably best served
just using AFTER or AFTER-WHILE instead to create that kind of
advice directly.

Additional SETUP and TEARDOWN function can be added for more control.

If VERIFY is t, the setup will verify before being executed.

Alternatively you may pass a segment name to PLUGS-INTO. That
segment is assumed to have defined a port function."
  (declare (indent defun))

  (let ((augment (whale-line-symbol--action name))
        (verify-sym (whale-line-symbol--verify name))
        (port-sym (whale-line-symbol--port plugs-into))
        (advice (cond
                 ((not (null after))
                  `(:after . (,@(whale-line--normalize-list after))))
                 ((not (null after-while))
                  `(:after-while . (,@(whale-line--normalize-list after-while))))
                 (t advice))))

    (if (not (bound-and-true-p whale-line--testing))
        `(progn
           (whale-line--set-props ',name 'augment)
           ,@(delq
              nil
              `(,(when action
                   `(whale-line--function ,augment ,(if plugs-into
                                                        `(lambda (&rest r)
                                                           (apply
                                                            ',port-sym
                                                            (apply ',action r)))
                                                      action)
                      ,(format "Augment function for `%s'." name) t))
                (whale-line--setup ,name :hooks ,(whale-line--normalize-list hooks) :advice ,advice :setup ,setup :teardown ,teardown :verify t)
                (whale-line--function ,verify-sym ,(or verify 'always) ,(format "Verify `%s' augment." name) t))))
      `(progn
         (whale-line--omit ,name augment)))))

;;; -- Priorities

(defvar whale-line--priorities '(t low current current-low))

(defun whale-line--update-priorities (segments priority)
  "Update priority to PRIORITY for all SEGMENTS."
  (dolist (it segments)
    (when-let ((def (assoc it whale-line--props))
               (props (cdr def)))
      (plist-put props :priority priority)
      (setcdr def props))))

(defmacro whale-line-with-priorities (&rest args)
  "Set priorities for segments.

ARGS is a list of segments followed by a priority value."
  (declare (indent 0))

  (let ((filter (lambda (it) (not (memq it whale-line--priorities))))
        commands)

    (while args
      (let ((segments (seq-take-while filter args)))

        (setq args (seq-drop-while filter args))
        (push `(whale-line--update-priorities ',segments ',(car args)) commands)
        (setq args (cdr args))))

    (macroexp-progn commands)))

;;; -- Rendering

(defun whale-line--render (side &optional filter)
  "Render SIDE.

Optionally FILTER out low priority segments."
  (let* ((segments (plist-get whale-line--segments side))
         (filtered (if (eq filter 'none)
                       segments
                     (whale-line--filter segments filter))))
    (whale-line--render-segments filtered)))

(defun whale-line--render-segments (segments)
  "Render SEGMENTS."
  (delq nil (mapcar
             (lambda (it)
               (when-let* ((sym it)
                           (segment (whale-line-symbol--segment sym))
                           (setter (whale-line-symbol--action sym)))

                 (if (functionp segment)
                     `(:eval (whale-line--pad-segment ',sym (,segment)))
                   (let* ((val (symbol-value segment))
                          (eval (pcase val
                                  ('initial `(,setter))
                                  (_ segment))))

                     `(:eval (whale-line--pad-segment ',sym ,eval))))))
             segments)))

;;; -- Padding

(defun whale-line--pad-segment (segment render)
  "Add padding to SEGMENT's RENDER based on its position."
  (let* ((dense (whale-line--prop segment :dense))
         (dense (if (functionp dense) (funcall dense) dense))
         (render (if (listp render) render (list render)))
         (side (whale-line--side segment))
         (paddings (whale-line--padding segment))
         (padded
          (delq
           nil
           `(,(when (and (eq side :left)
                         (not (car-safe paddings)))
                (whale-line--spacer dense))
             ,@render
             ,(when (and (eq side :right)
                         (not (cdr-safe paddings)))
                (whale-line--spacer dense))))))

    (if (whale-line--empty-render-p padded)
        nil
      padded)))

(defun whale-line--padding (segment)
  "Get padding of SEGMENT.

Returns a cons of (OWN-LEFT or LEFT-NEIGHBOR-RIGHT . OWN-RIGHT or
RIGHT-NEIGHBOR-LEFT).

The results are cached."
  (or (gethash segment whale-line--padded-cache)
      (let* ((own (whale-line--prop segment :padded))
             (left (whale-line--neighbor segment :left))
             (right (whale-line--neighbor segment :right))
             (padding (cons (or (memq own '(left all))
                                (memq (whale-line--prop left :padded)
                                      '(right all)))
                            (or (memq own '(right all))
                                (memq (whale-line--prop right :padded)
                                      '(left all))))))

        (puthash segment padding whale-line--padded-cache)
        padding)))

(defun whale-line--side (segment)
  "Get the side the SEGMENT is on."
  (if (memq segment (plist-get whale-line--segments :left))
      :left
    :right))

(defun whale-line--neighbor (segment side)
  "Get SEGMENT's neighbor on SIDE."
  (when-let* ((segment-side (whale-line--side segment))
              (segments (plist-get whale-line--segments segment-side))
              (index (seq-position segments segment)))

    (cond
     ((and (eq side :left)
           (> index 0))
      (nth (1- index) segments))
     ((and (eq side :right)
           (< index (1- (length segments))))
      (nth (1+ index) segments)))))

(defun whale-line--spacer (&optional dense)
  "A space used for padding.

If DENSE is t, add no padding."
  (if dense "" " "))

(defvar whale-line--empty-renders
  `((,(whale-line--spacer) ,(whale-line--spacer t))
    (,(whale-line--spacer t) ,(whale-line--spacer))
    (,(whale-line--spacer) ,(whale-line--spacer))
    (,(whale-line--spacer t) ,(whale-line--spacer t))
    (,(whale-line--spacer))))

(defun whale-line--empty-render-p (render)
  "Check if RENDER is empty."
  (member render whale-line--empty-renders))

;;; -- Filtering

(defun whale-line--filter (segments &optional low-space)
  "Filter SEGMENTS.

This filters differently for current and other window.

If LOW-SPACE is t, additional segments are filtered."
  (let ((filter (if (whale-line--is-current-window-p)
                    (whale-line--filter-for-current low-space)
                  (whale-line--filter-for-other low-space))))
    (seq-filter (lambda (it) (not (memq (whale-line--prop it :priority) filter))) segments)))

(defun whale-line--filter-for-current (&optional low-space)
  "Build the filter for current window.

If LOW-SPACE is t, filter out additional segments."
  (if low-space
      (list 'low 'current-low)
    nil))

(defun whale-line--filter-for-other (&optional low-space)
  "Build the filter for other window.

If LOW-SPACE is t, filter out additional segments."
  (if low-space
      (list 'current 'current-low 'low)
    (list 'current 'current-low)))

;;; -- Refreshing

(defun whale-line--queue-refresh ()
  "Queue a refresh.

This will refresh stateful segments."
  (when whale-line--stateful-timer
    (unless (timer--triggered whale-line--stateful-timer)
      (cancel-timer whale-line--stateful-timer)))

  (setq whale-line--stateful-timer (run-with-idle-timer 0.5 nil #'whale-line--refresh-stateful-segments)))

(defun whale-line--refresh-stateful-segments ()
  "Refresh all stateful segments.

This will call the respective segment's action."
  (let* ((interner (lambda (it) (intern-soft (format "whale-line-%s--action" it))))
         (actions (mapcar interner (whale-line--segments-by-type 'stateful))))

    (mapc #'funcall actions)))

;;; -- Helpers

(defun whale-line--segments-by-type (type)
  "Get all segments matching TYPE."
  (cl-loop for (a . b) in whale-line--props
           if (eq type (plist-get b :type))
           collect a))

(defun whale-line--normalize-list (obj)
  "If OBJ is not a list, return a list with it as the only element."
  (if (listp obj)
      obj
    (list obj)))

;;; -- Logging

(defvar whale-line--log-buffer-name " *whale-line*")

(defun whale-line--log (fmt &rest args)
  "Format FMT with ARGS."
  (when whale-line-log
    (let ((buffer (get-buffer whale-line--log-buffer-name))
          (inhibit-read-only t))

      (unless buffer
        (setq buffer (get-buffer-create whale-line--log-buffer-name))
        (with-current-buffer buffer
          (view-mode)))

      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (apply #'format fmt args))
        (insert "\n")))))

;;; -- API

;;;###autoload
(defmacro whale-line-create-stateful-segment (name &rest args)
  "Create a stateful segment called NAME.

See underlying macro for the usage of ARGS."
  (declare (indent defun))

  `(progn
     (whale-line--create-stateful-segment ,name ,@args)))

(defalias 'whale-line-create-static-segment 'whale-line-create-stateful-segment)
(make-obsolete 'whale-line-create-static-segment 'whale-line-create-stateful-segment "0.7.1")

;;;###autoload
(defmacro whale-line-create-stateless-segment (name &rest args)
  "Create a stateless segment called NAME.

See underlying macro for the usage of ARGS."
  (declare (indent defun))

  `(progn
     (whale-line--create-stateless-segment ,name ,@args)))

(defalias 'whale-line-create-dynamic-segment 'whale-line-create-stateless-segment)
(make-obsolete 'whale-line-create-dynamic-segment 'whale-line-create-stateless-segment "0.7.1")

;;;###autoload
(defmacro whale-line-create-augment (name &rest args)
  "Create an augment called NAME.

See underlying macro for the usage of ARGS."
  (declare (indent defun))

  `(progn
     (whale-line--create-augment ,name ,@args)))


(defvar whale-line--default-mode-line nil)

;;; -- Setup

(defun whale-line-mode--setup ()
  "Set up `whale-line-mode'."
  (unless (whale-line--build-segments)
    (user-error "Failed to build segments"))

  ;; Save a copy of the previous mode-line.
  (setq whale-line--default-mode-line mode-line-format)

  ;; Make setups do their thing.
  (run-hooks 'whale-line-setup-hook)
  (add-hook 'pre-redisplay-functions #'whale-line--set-selected-window)
  (add-hook 'window-configuration-change-hook #'whale-line--calculate-space)
  (add-hook 'buffer-list-update-hook #'whale-line--queue-refresh)

  ;; Set the new mode-line-format
  (setq-default mode-line-format '("%e" (:eval (whale-line--format)))))

(defun whale-line-mode--teardown ()
  "Tear down `whale-line-mode'."
  ;; Tear down everything.
  (run-hooks 'whale-line-teardown-hook)
  (remove-hook 'pre-redisplay-functions #'whale-line--set-selected-window)
  (remove-hook 'window-configuration-change-hook #'whale-line--calculate-space)
  (remove-hook 'buffer-list-update-hook #'whale-line--queue-refresh)

  ;; Restore the original mode-line format
  (setq-default mode-line-format whale-line--default-mode-line))

;;; -- API

(defun whale-line-rebuild ()
  "Rebuild the segments.

Only necessary if you have changed `whale-line-segments'."
  (interactive)

  (whale-line--build-segments))

(defun whale-line-trigger-augments (&optional arg)
  "Trigger augments.

Sets up augments (again). If ARG is t, tears them down instead."
  (interactive "P")

  (whale-line--trigger-augments arg))

(defun whale-line-pop-to-logs ()
  "Switch to the log buffer."
  (interactive)

  (let ((buffer (get-buffer whale-line--log-buffer-name)))

    (unless buffer
      (user-error "You need to set `harpoon-log' to t first"))

    (pop-to-buffer (get-buffer whale-line--log-buffer-name))))

;;;###autoload
(define-minor-mode whale-line-mode
  "Toggle `whale-line' on or off."
  :group 'whale-line
  :global t
  :lighter " wll"
  (if whale-line-mode
      (whale-line-mode--setup)
    (whale-line-mode--teardown)))

(provide 'whale-line)

;;; whale-line.el ends here
