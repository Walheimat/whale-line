;;; whale-line-core.el --- A whale-based mode-line -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.7.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; The core library provides macros to create new segments and
;; augments. It also contains the logic to render the mode line.

;;; Code:

(require 'cl-lib)
(require 'cl-extra)

;;; -- Variables

(defvar whale-line--current-window nil)
(defvar whale-line--default-mode-line nil)
(defvar whale-line--dense nil)
(defvar whale-line--priority nil)
(defvar whale-line--segments nil)
(defvar whale-line--stateful-timer nil)
(defvar whale-line--type nil)

(defvar whale-line-setup-hook nil)
(defvar whale-line-teardown-hook nil)

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
                                 dap
                                 selection
                                 process
                                 window-status
                                 |
                                 misc-info
                                 minor-modes
                                 partial-recall
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

(defun whale-line--calculate-width (side)
  "Calculate the width for SIDE.

This uses `string-pixel-width' for Emacs 29+, otherwise
`window-font-width.'"
  (let ((formatted (whale-line--format-side side)))

    (if (fboundp 'string-pixel-width)
        (string-pixel-width formatted)
      (* (window-font-width) (length formatted)))))

(defun whale-line--enough-space-p ()
  "Calculate whether there is enough space to display both sides' segments."
  (let* ((left (whale-line--calculate-width :left))
         (right (whale-line--calculate-width :right)))

    (> (- (window-pixel-width) (+ left right)) 0)))

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

;;; -- Building segments

(defun whale-line--build-segments ()
  "Build the segments."
  (let* ((break (cl-position '| whale-line-segments))
         (left (cl-subseq whale-line-segments 0 break))
         (right (cl-subseq whale-line-segments (1+ break))))

    (setq whale-line--segments (plist-put whale-line--segments :left (delq nil (mapcar #'whale-line--map-segment left))))
    (setq whale-line--segments (plist-put whale-line--segments :right (delq nil (mapcar #'whale-line--map-segment right))))))

(defun whale-line--map-segment (segment)
  "Map SEGMENT to its priority declaration."
  (when (whale-line--valid-segment-p segment)
    (assoc segment whale-line--priority)))

(defun whale-line--valid-segment-p (segment)
  "Check that SEGMENT can be included."
  (let ((verify-sym (intern (format "whale-line-%s--verify" (symbol-name segment)))))

    (if (fboundp verify-sym)
        (funcall verify-sym)
      t)))

(defun whale-line--add-segment (segment type &optional priority dense)
  "Add SEGMENT of TYPE to the list of segments.

Optionally with a PRIORITY and DENSE."
  (whale-line--set-segment-priority segment (or priority t))
  (whale-line--set-type segment type)
  (whale-line--set-dense segment dense))

(defun whale-line--set-segment-priority (segment priority)
  "Set PRIORITY of a SEGMENT."
  (if-let ((existing (assoc segment whale-line--priority)))
      (setcdr existing priority)
    (push (cons segment priority) whale-line--priority)))

(defun whale-line--set-type (segment type)
  "Set TYPE of SEGMENT."
  (if-let ((existing (assoc segment whale-line--type)))
      (setcdr existing type)
    (push (cons segment type) whale-line--type)))

(defun whale-line--set-dense (segment dense)
  "Set SEGMENT as DENSE."
  (if-let ((existing (assoc segment whale-line--dense)))
      (setcdr existing dense)
    (push (cons segment dense) whale-line--dense)))

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

  (let ((setter-sym (intern (format "whale-line-%s--action" (symbol-name name))))
        (setup-sym (intern (format "whale-line-%s--setup" (symbol-name name))))
        (teardown-sym (intern (format "whale-line-%s--teardown" (symbol-name name))))
        (verify-sym (intern (format "whale-line-%s--verify" (symbol-name name)))))

    `(progn
       (cl-defun ,setup-sym (&rest _)
         ,(format "Set up %s segment." name)

         ,(when verify
            `(unless (,verify-sym)
               (cl-return-from ,setup-sym)))

         ,@(mapcar (lambda (it)
                     `(add-hook ',it #',setter-sym))
                   hooks)

         ,@(mapcar (lambda (it)
                     `(advice-add ',it ,(car advice) #',setter-sym))
                   (cdr advice))

         ,(when setup (if (symbolp setup)
                          `(funcall ',setup)
                        `(funcall ,setup))))

       (add-hook 'whale-line-setup-hook #',setup-sym)

       (defun ,teardown-sym (&rest _)
         ,(format "Tear down %s segment." name)

         ,@(mapcar (lambda (it)
                     `(remove-hook ',it #',setter-sym))
                   hooks)

         ,@(mapcar (lambda (it)
                     `(advice-remove ',it #',setter-sym))
                   (cdr advice))

         ,(when teardown
            (if (symbolp teardown)
                `(funcall ',teardown)
              `(funcall ,teardown))))

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
  (let ((segment-sym (intern (format "whale-line-%s--segment" (symbol-name name)))))
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

(cl-defmacro wlc--create-stateful-segment (name &key getter hooks advice verify setup teardown priority dense)
  "Create a stateful segment named NAME.

Stateful segments are represented by a variable that is updated
by hooks or advice.

GETTER is the form to evaluate to get the string (the setter is
constructed by the macro).

HOOKS is a list of functions that will call the setter.

ADVICE is a cons cell of the form (COMBINATOR .
FUNCTIONS-TO-ADVISE) that will also call the setter.

VERIFY is a function called before the segments are built. If it
returns nil, the segment will not be included.

SETUP is the function called on setup, TEARDOWN that during teardown.

This will also add the segment with PRIORITY or t.

If DENSE is t, the segment will not be padded."
  (declare (indent defun))

  (let* ((sym-name (symbol-name name))
         (formatter (lambda (fs) (intern (format fs sym-name))))
         (segment (funcall formatter "whale-line-%s--segment"))
         (setter (funcall formatter "whale-line-%s--action"))
         (getter-sym (funcall formatter "whale-line-%s--get-segment"))
         (verify-sym (funcall formatter "whale-line-%s--verify"))
         (prio (or priority t)))

    (if (not (bound-and-true-p whale-line--testing))
        `(progn
           (defvar-local ,segment 'initial)

           (defun ,setter (&rest _)
             ,(format "Set %s segment." name)
             (if-let ((str (,getter-sym)))
                 (setq ,segment str)
               (setq ,segment nil)))

           (whale-line--function ,getter-sym ,getter ,(format "Get the %s segment." name))
           (whale-line--setup ,name :setup ,setup :advice ,advice :hooks ,hooks :teardown ,teardown :verify ,(not (null verify)))
           ,(when verify
              `(whale-line--function ,verify-sym ,verify ,(format "Verify `%s' segment." name) t))

           (whale-line--add-segment ',name 'stateful ',prio ',dense))
      `(progn
         (whale-line--omit ,name stateful)))))

(cl-defmacro wlc--create-stateless-segment (name &key getter condition verify setup teardown priority dense)
  "Create a stateless segment name NAME.

A stateless segment is represented by a function that is called
on every mode-line update.

GETTER is the function to call on re-render.

CONDITION is the condition to evaluate before calling the
renderer. If NEEDS-CURRENT is truthy, it will be an additional
condition.

VERIFY is a function called before the segments are built. If it
returns nil, the segment will not be included.

SETUP is the function called on setup, TEARDOWN that during teardown.

The segment will be added with PRIORITY or t.

If DENSE is t, the segment will not be padded."
  (declare (indent defun))

  (let ((segment (intern (format "whale-line-%s--segment" (symbol-name name))))
        (getter-sym (intern (format "whale-line-%s--get-segment" (symbol-name name))))
        (verify-sym (intern (format "whale-line-%s--verify" (symbol-name name))))
        (prio (or priority t))
        (con (or condition t)))

    (if (not (bound-and-true-p whale-line--testing))
        `(progn
           (defun ,segment ()
             ,(format "Render `%s' segment." name)
             (or (when ,con
                   (,getter-sym))
                 ""))

           (whale-line--function ,getter-sym ,getter ,(format "Get the `%s' segment." name))
           (whale-line--setup ,name :setup ,setup :teardown ,teardown :verify ,(not (null verify)))
           ,(when verify
              `(whale-line--function ,verify-sym ,verify ,(format "Verify `%s' segment." name) t))
           (whale-line--add-segment ',name 'stateless ',prio ',dense))
      `(progn
         (whale-line--omit ,name stateless)))))

(cl-defmacro wlc--create-augment (name &key action hooks advice setup teardown verify)
  "Create augment(-or) named NAME.

ACTION is the function to call for HOOKS.

ADVICE is an cons cell of the form combinator .
functions-to-advise to call ACTION.

Additional SETUP and TEARDOWN function can be added for more control.

If VERIFY is t, the setup will verify before being executed."
  (declare (indent defun))

  (let ((augment (intern (format "whale-line-%s--action" (symbol-name name))))
        (verify-sym (intern (format "whale-line-%s--verify" (symbol-name name)))))

    (if (not (bound-and-true-p whale-line--testing))
        `(progn
           ,(when action
              `(whale-line--function ,augment ,action ,(format "Augment function for `%s'." name) t))
           (whale-line--setup ,name :hooks ,hooks :advice ,advice :setup ,setup :teardown ,teardown :verify ,(not (null verify)))
           ,(when verify
              `(whale-line--function ,verify-sym ,verify ,(format "Verify `%s' augment." name) t))
           (whale-line--add-segment ',name 'augment))
      `(progn
         (whale-line--omit ,name augment)))))

;;; -- Rendering

(defun whale-line--render (side &optional filter)
  "Render SIDE.

Optionally FILTER out low priority segments."
  (let* ((segments (plist-get whale-line--segments side))
         (filtered (whale-line--filter segments filter)))
    (whale-line--render-segments filtered)))

(defun whale-line--render-segments (segments)
  "Render SEGMENTS."
  (delq nil (mapcar
             (lambda (it)
               (when-let* ((should-use (cdr it))
                           (sym (car it))
                           (name (symbol-name sym))
                           (segment (intern (format "whale-line-%s--segment" name)))
                           (setter (intern (format "whale-line-%s--action" name))))

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
  (let* ((dense (alist-get segment whale-line--dense))
         (dense (if (functionp dense) (funcall dense) dense))
         (render (if (listp render) render (list render)))
         (padded
          (delq
           nil
           `(,(when (and (assoc segment (plist-get whale-line--segments :left)))
                (whale-line--spacer dense))
             ,@render
             ,(when (assoc segment (plist-get whale-line--segments :right))
                (whale-line--spacer dense))))))

    (if (whale-line--empty-render-p padded)
        nil
      padded)))

(defun whale-line--spacer (&optional dense)
  "A space used for padding.

If DENSE is t, add no padding."
  (if dense "" " "))

(defun whale-line--empty-render-p (render)
  "Check if RENDER is empty."
  (or (equal render '(" " ""))
      (equal render '("" " "))
      (equal render '(" " " "))
      (equal render '(" "))
      (equal render '("" ""))))

;;; -- Filtering

(defun whale-line--filter (segments &optional low-space)
  "Filter SEGMENTS.

This filters differently for current and other window.

If LOW-SPACE is t, additional segments are filtered."
  (let ((filter (if (whale-line--is-current-window-p)
                    (whale-line--filter-for-current low-space)
                  (whale-line--filter-for-other low-space))))
    (seq-filter (lambda (it) (not (memq (cdr it) filter))) segments)))

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
         (actions (cl-loop for (a . b) in whale-line--type
                           if (eq b 'stateful)
                           collect (funcall interner a))))

    (mapc #'funcall actions)))

;;; -- API

;;;###autoload
(defmacro whale-line-create-stateful-segment (name &rest args)
  "Create a stateful segment called NAME.

See underlying macro for the usage of ARGS."
  (declare (indent defun))

  `(progn
     (whale-line-core--create-stateful-segment ,name ,@args)))

(defalias 'whale-line-create-static-segment 'whale-line-create-stateful-segment)
(make-obsolete 'whale-line-create-static-segment 'whale-line-create-stateful-segment "0.7.1")

;;;###autoload
(defmacro whale-line-create-stateless-segment (name &rest args)
  "Create a stateless segment called NAME.

See underlying macro for the usage of ARGS."
  (declare (indent defun))

  `(progn
     (whale-line-core--create-stateless-segment ,name ,@args)))

(defalias 'whale-line-create-dynamic-segment 'whale-line-create-stateless-segment)
(make-obsolete 'whale-line-create-dynamic-segment 'whale-line-create-stateless-segment "0.7.1")

;;;###autoload
(defmacro whale-line-create-augment (name &rest args)
  "Create an augment called NAME.

See underlying macro for the usage of ARGS."
  (declare (indent defun))

  `(progn
     (whale-line-core--create-augment ,name ,@args)))

(provide 'whale-line-core)

;;; whale-line-core.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wlc-" . "whale-line-core-"))
;; End:
