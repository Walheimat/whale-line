;;; whale-line-core.el --- A whale-based mode-line -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.6.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; The mode-line I use.

;;; Code:

(require 'cl-lib)
(require 'cl-extra)

;;; -- Variables

(defvar whale-line--segments nil)
(defvar whale-line--priorities nil)
(defvar whale-line--current-window nil)
(defvar whale-line--default-mode-line nil)
(defvar whale-line--default-mode-line nil)
(defvar whale-line-setup-hook nil)
(defvar whale-line-teardown-hook nil)

;;; -- Customization

(defgroup whale-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom whale-line-segments '(margin
                                 icons
                                 buffer-name
                                 org
                                 buffer-status
                                 position
                                 selection
                                 cursors
                                 process
                                 window-status
                                 |
                                 minor-modes
                                 global-mode-string
                                 project
                                 vc
                                 tab-bar
                                 animation
                                 margin)
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

;;; -- Utility

(defun whale-line--spacer (&optional big)
  "A space used for padding.

Optionally, use a BIG spacer."
  (if big "  " " "))

(defun whale-line--car-safe-until (seq compare-fn &optional default-value)
  "Inspect car of SEQ until non-list item is found.

That item is returned if COMPARE-FN yields t. Otherwise nil or an
optional DEFAULT-VALUE is returned."
  (let ((rest seq))

    (while (and rest (listp rest))
      (setq rest (car-safe rest)))

    (if (apply compare-fn (list rest))
        rest
      (or default-value nil))))

;;; -- Formatting

(defun whale-line--format-side (side &optional filter)
  "Get the formatted SIDE.

Optionally FILTER out low priority segments."
  (format-mode-line (whale-line--render side filter)))

(defun whale-line--enough-space-p ()
  "Calculate whether there is enough space to display both sides' segments."
  (let* ((f-pixel (window-font-width))
         (left (* f-pixel (length (whale-line--format-side :left))))
         (right (* f-pixel (length (whale-line--format-side :right)))))
    (> (- (window-pixel-width) (+ left right)) 0)))

(defun whale-line--format-ignore ()
  "Format mode line ignoring space constraints."
  (let ((lhs (whale-line--render :left))
        (rhs (whale-line--render :right))
        (rlen (length (whale-line--format-side :right))))
    `(,@lhs ,(whale-line--space-between rlen) ,@rhs)))

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
  (let ((win (whale-line--get-current-window)))
    (setq whale-line--current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defun whale-line--is-current-window-p ()
  "Check if the current window is the selected window."
  (and whale-line--current-window
       (eq (whale-line--get-current-window) whale-line--current-window)))

;;; -- Segments

(defun whale-line--build-segments ()
  "Build the segments."
  (let* ((break (cl-position '| whale-line-segments))
         (left (cl-subseq whale-line-segments 0 break))
         (right (cl-subseq whale-line-segments (1+ break)))
         (mapper (lambda (it) (assoc it whale-line--priorities))))

    (setq whale-line--segments (plist-put whale-line--segments :left (delq nil (mapcar mapper left))))
    (setq whale-line--segments (plist-put whale-line--segments :right (delq nil (mapcar mapper right))))))

(defun whale-line--set-segment-priority (segment priority)
  "Set PRIORITY of a SEGMENT."
  (if-let ((existing (assoc segment whale-line--priorities)))
      (setcdr existing priority)
    (push (cons segment priority) whale-line--priorities)))

(defun whale-line--add-segment (segment &optional priority)
  "Add SEGMENT to the list of segments.

Optionally with a PRIORITY."
  (let ((prio (or priority t)))
    (whale-line--set-segment-priority segment prio)))

;;; -- Macros

(cl-defmacro whale-line--setup (name &key setup teardown hooks advice)
  "Create setup for NAME.

HOOKS is a list of functions that will call the setter.

ADVICE is a cons cell of the form (combinator .
functions-to-advise) that will also call the setter.

SETUP is the function called on setup. TEARDOWN is the function
called on teardown."
  (declare (indent defun))

  (let ((setter-sym (intern (format "whale-line-%s--action" (symbol-name name))))
        (setup-sym (intern (format "whale-line-%s--setup" (symbol-name name))))
        (teardown-sym (intern (format "whale-line-%s--teardown" (symbol-name name)))))

    `(progn
       (defun ,setup-sym (&rest _)
         ,(format "Set up %s segment." name)

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
          fun))))

(defmacro whale-line--omit (name type)
  "Indicate that segment NAME was omitted.

TYPE controls what is emitted: a variable for static segments, a
function returning the empty string for dynamic segments and
nothing for augments."
  (let ((segment-sym (intern (format "whale-line-%s--segment" (symbol-name name)))))
    `(progn
       ,(pcase type
          ('dynamic
           `(defun ,segment-sym ()
              ,(format "Render `%s' segment as an empty string." name)
              ""))
          ('static
           `(defvar ,segment-sym nil))
          (_ nil))
       (unless (bound-and-true-p whale-line--testing)
         (message "Couldn't add %s `%s' segment" ',type ',name)))))

(cl-defmacro whale-line-create-static-segment (name &key getter hooks advice verify setup teardown dense priority)
  "Create a static segment named NAME.

GETTER is the form to evaluate to get the string (the setter is
constructed by the macro).

HOOKS is a list of functions that will call the setter.

ADVICE is a cons cell of the form combinator .
functions-to-advise that will also call the setter.

VERIFY is a function called before setup. If it returns nil, the
segment will not be added.

SETUP is the function called on setup, TEARDOWN that during teardown.

A left margin is added unless DENSE is t.

This will also add the segment with PRIORITY or t."
  (declare (indent defun))

  (let ((segment (intern (format "whale-line-%s--segment" (symbol-name name))))
        (setter (intern (format "whale-line-%s--action" (symbol-name name))))
        (getter-sym (intern (format "whale-line-%s--get-segment" (symbol-name name))))
        (prio (or priority t)))

    (if (and (not (bound-and-true-p whale-line--testing))
             (or (null verify) (funcall verify)))
        `(progn
           (defvar ,segment 'initial)

           (defun ,setter (&rest _)
             ,(format "Set %s segment." name)
             (if-let ((str (,getter-sym)))
                 (setq-local ,segment ,(if dense 'str '(concat (whale-line--spacer) str)))
               (setq-local ,segment nil)))

           (whale-line--function ,getter-sym ,getter ,(format "Get the %s segment." name))
           (whale-line--setup ,name :setup ,setup :advice ,advice :hooks ,hooks :teardown ,teardown)
           (whale-line--add-segment ',name ',prio))
      `(progn
         (whale-line--omit ,name static)))))

(cl-defmacro whale-line-create-dynamic-segment (name &key getter condition verify setup teardown dense priority)
  "Create a dynamic segment name NAME.

GETTER is the function to call on re-render.

CONDITION is the conndition to evaluate before calling the
renderer. If NEEDS-CURRENT is truthy, it will be an additional
condition.

VERIFY is a function called before setup. If it returns nil, the
segment will not be added.

SETUP is the function called on setup, TEARDOWN that during teardown.

A left margin is added unless DENSE is t.

The segment will be added with PRIORITY or t."
  (declare (indent defun))

  (let ((segment (intern (format "whale-line-%s--segment" (symbol-name name))))
        (getter-sym (intern (format "whale-line-%s--get-segment" (symbol-name name))))
        (prio (or priority t))
        (con (or condition t)))

    (if (and (not (bound-and-true-p whale-line--testing))
             (or (null verify) (funcall verify)))
        `(progn
           (defun ,segment ()
             ,(format "Render `%s' segment." name)
             (or (when ,con
                   ,(if dense `(,getter-sym) `(concat (whale-line--spacer) (,getter-sym))))
                 ""))

           (whale-line--function ,getter-sym ,getter ,(format "Get the `%s' segment." name))
           (whale-line--setup ,name :setup ,setup :teardown ,teardown)
           (whale-line--add-segment ',name ',prio))
      `(progn
         (whale-line--omit ,name dynamic)))))

(cl-defmacro whale-line-create-augment (name &key verify action hooks advice setup teardown)
  "Create augment(-or) named NAME.

VERIFY is an optional function called before augmenting. If that
function returns nil, a user error will indicate it didn't take
place.

ACTION is the function to call for HOOKS.

ADVICE is an cons cell of the form combinator .
functions-to-advise to call ACTION.

Additional SETUP and TEARDOWN function can be added for more control."
  (declare (indent defun))

  (let ((augment (intern (format "whale-line-%s--action" (symbol-name name)))))

    (if (and (not (bound-and-true-p whale-line--testing))
             (or (null verify) (funcall verify)))
        `(progn
           (whale-line--function ,augment ,action ,(format "Augment function for `%s'." name) t)
           (whale-line--setup ,name :hooks ,hooks :advice ,advice :setup ,setup :teardown ,teardown))
      `(progn
         (whale-line--omit ,name augment)))))

;;; -- Rendering

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
                           (name (symbol-name (car it)))
                           (segment (intern (format "whale-line-%s--segment" name)))
                           (setter (intern (format "whale-line-%s--action" name))))

                 (if (functionp segment)
                     `(:eval (,segment))
                   (let* ((val (symbol-value segment))
                          (eval (pcase val
                                  ('initial `(,setter))
                                  (_ segment))))

                     `(:eval ,eval)))))
             segments)))

(provide 'whale-line-core)

;;; whale-line-core.el ends here
