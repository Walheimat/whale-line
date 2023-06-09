;;; whale-line.el --- A whale-based mode-line -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; The mode-line I use.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defconst whale-line--all-features '(flycheck
                                     project
                                     icons
                                     vc
                                     animation
                                     minions
                                     cursors
                                     lsp
                                     org
                                     tab-bar))

(defvar whale-line--segments '(:left
                               ((margin . t)
                                (icons . nil)
                                (buffer-name . nil)
                                (org . nil)
                                (buffer-status . nil)
                                (position . nil)
                                (selection . nil)
                                (cursors . nil)
                                (process . nil)
                                (window-status . nil))

                               :right
                               ((minor-modes . nil)
                                (global-mode-string . nil)
                                (project . nil)
                                (vc . nil)
                                (tab-bar . nil)
                                (animation . nil)
                                (margin . t))))

;;;; Customization:

(defgroup whale-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom whale-line-features (copy-tree whale-line--all-features)
  "Optional features to add or enhance segments."
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

;;;; Faces:

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

;;;; Utility:

(defun whale-line--spacer (&optional big)
  "A space used for padding.

Optionally, use a BIG spacer."
  (if big "  " " "))

;; Formatting:

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

(defun whale-line--space-between (length)
  "Get the space between sides aligned using LENGTH."
  (propertize
   " "
   'display
   `((space :align-to (- right (- 0 right-margin) ,length)))))

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

;; Windows:

(defvar whale-line--current-window nil)

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

;; Priorities:

(defun whale-line--set-segment-priority (segment priority)
  "Set PRIORITY of a SEGMENT."
  (let ((left? (assoc segment (plist-get whale-line--segments :left)))
        (right? (assoc segment (plist-get whale-line--segments :right))))
    (cond
     (left?
      (setcdr left? priority))
     (right?
      (setcdr right? priority))
     (t (user-error "Unknown segment")))))

(defun whale-line-add-segment (segment &optional priority)
  "Add SEGMENT to the list of segments.

Optionally with a PRIORITY."
  (let ((prio (or priority t)))
    (whale-line--set-segment-priority segment prio)))

;; Macros:

(defvar whale-line-augment-fstring "whale-line-%s--augment")
(defvar whale-line-segment-fstring "whale-line-%s--segment")
(defvar whale-line-set-segment-fstring "whale-line-%s--set-segment")
(defvar whale-line-get-segment-fstring "whale-line-%s--get-segment")
(defvar whale-line-setup-fstring "whale-line-%s--setup")
(defvar whale-line-teardown-fstring "whale-line-%s--teardown")

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

  (defvar whale-line-segment-fstring "whale-line-%s--segment")
  (defvar whale-line-set-segment-fstring "whale-line-%s--set-segment")
  (defvar whale-line-get-segment-fstring "whale-line-%s--get-segment")
  (defvar whale-line-setup-fstring "whale-line-%s--setup")
  (defvar whale-line-teardown-fstring "whale-line-%s--teardown")

  (let ((segment (intern (format whale-line-segment-fstring (symbol-name name))))
        (setter (intern (format whale-line-set-segment-fstring (symbol-name name))))
        (setup-sym (intern (format whale-line-setup-fstring (symbol-name name))))
        (teardown-sym (intern (format whale-line-teardown-fstring (symbol-name name))))
        (getter-sym (intern (format whale-line-get-segment-fstring (symbol-name name))))
        (prio (or priority t)))

    (if (and (not (bound-and-true-p whale-line--testing))
             (or (null verify) (funcall verify)))
        `(progn
           (defvar ,segment 'initial)

           (defun ,getter-sym ()
             ,(format "Get the %s segment." name)
             ,(if (symbolp getter)
                  `(funcall ',getter)
                getter))

           (defun ,setter (&rest _)
             ,(format "Set %s segment." name)
             (if-let ((str (,getter-sym)))
                 (setq-local ,segment ,(if dense 'str '(concat (whale-line--spacer) str)))
               (setq-local ,segment nil)))

           ,(when (or setup hooks advice)
              `(progn
                 (defun ,setup-sym (&rest _)
                   ,(format "Set up %s segment." name)

                   ,@(mapcar (lambda (it)
                               `(add-hook ',it #',setter))
                             hooks)

                   ,@(mapcar (lambda (it)
                               `(advice-add ',it ,(car advice) #',setter))
                             (cdr advice))

                   ,(when setup (if (symbolp setup)
                                    `(funcall ',setup)
                                  `(funcall ,setup))))

                 (add-hook 'whale-line-setup-hook #',setup-sym)))

           ,(when (or teardown hooks advice)
              `(progn
                 (defun ,teardown-sym (&rest _)
                   ,(format "Tear down %s segment." name)

                   ,@(mapcar (lambda (it)
                               `(remove-hook ',it #',setter))
                             hooks)

                   ,@(mapcar (lambda (it)
                               `(advice-remove ',it #',setter))
                             (cdr advice))

                   ,(when teardown (if (symbolp teardown)
                                       `(funcall ',teardown)
                                     `(funcall ,teardown))))

                 (add-hook 'whale-line-teardown-hook #',teardown-sym)))

           (whale-line-add-segment ',name ,prio))
      `(progn
         (defvar ,segment nil)
         (unless (bound-and-true-p whale-line--testing)
           (message "Couldn't add `%s' segment" ',name))))))

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

  (defvar whale-line-segment-fstring "whale-line-%s--segment")
  (defvar whale-line-get-segment-fstring "whale-line-%s--get-segment")
  (defvar whale-line-setup-fstring "whale-line-%s--setup")
  (defvar whale-line-teardown-fstring "whale-line-%s--teardown")

  (let ((segment (intern (format whale-line-segment-fstring (symbol-name name))))
        (getter-sym (intern (format whale-line-get-segment-fstring (symbol-name name))))
        (setup-sym (intern (format whale-line-setup-fstring (symbol-name name))))
        (teardown-sym (intern (format whale-line-teardown-fstring (symbol-name name))))
        (prio (or priority t))
        (con (or condition t)))

    (if (and (not (bound-and-true-p whale-line--testing))
             (or (null verify) (funcall verify)))
        `(progn
           (defun ,getter-sym ()
             ,(format "Get the `%s' segment." name)
             ,(if (symbolp getter)
                  `(funcall ',getter)
                getter))

           (defun ,segment ()
             ,(format "Render `%s' segment." name)
             (or (when ,con
                   ,(if dense `(,getter-sym) `(concat (whale-line--spacer) (,getter-sym))))
                 ""))

           ,(when setup
              `(progn
                 (defun ,setup-sym (&rest _)
                   ,(format "Set up %s segment." name)
                   (funcall ,setup))

                 (add-hook 'whale-line-setup-hook #',setup-sym)))

           ,(when teardown
              `(progn
                 (defun ,teardown-sym (&rest _)
                   ,(format "Tear down %s segment." name)
                   (funcall ,teardown))

                 (add-hook 'whale-line-teardown-hook #',teardown-sym)))

           (whale-line-add-segment ',name ,prio))
      `(progn
         (defun ,segment ()
           ,(format "Render `%s' segment as an empty string." name)
           "")
         (unless (bound-and-true-p whale-line--testing)
           (message "Couldn't add `%s' segment" ',name))))))

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

  (defvar whale-line-augment-fstring "whale-line-%s--augment")
  (defvar whale-line-setup-fstring "whale-line-%s--setup")
  (defvar whale-line-teardown-fstring "whale-line-%s--teardown")

  (let ((augment (intern (format whale-line-augment-fstring (symbol-name name))))
        (setup-sym (intern (format whale-line-setup-fstring (symbol-name name))))
        (teardown-sym (intern (format whale-line-teardown-fstring (symbol-name name)))))

    (if (and (not (bound-and-true-p whale-line--testing))
             (or (null verify) (funcall verify)))
        `(progn
           (defun ,augment (&rest args)
             ,(format "Augment function for `%s'" name)

             ,(if (symbolp action)
                  `(apply ',action args)
                `(apply ,action args)))

           ,(when (or hooks advice setup)
              `(progn
                 (defun ,setup-sym (&rest _)
                   ,(format "Set up %s segment." name)
                   ,@(mapcar (lambda (it)
                               `(add-hook ',it #',augment))
                             hooks)

                   ,@(mapcar (lambda (it)
                               `(advice-add ',it ,(car advice) #',augment))
                             (cdr advice))

                   ,(when setup `(funcall ,setup)))

                 (add-hook 'whale-line-setup-hook #',setup-sym)))

           ,(when (or hooks advice setup)
              `(progn
                 (defun ,teardown-sym (&rest _)
                   ,(format "Tear down %s segment." name)

                   ,@(mapcar (lambda (it)
                               `(remove-hook ',it #',augment)) hooks)

                   ,@(mapcar (lambda (it)
                               `(advice-remove ',it #',augment)) (cdr advice))

                   ,(when teardown `(funcall ,teardown)))

                 (add-hook 'whale-line-teardown-hook #',teardown-sym))))
      `(unless (bound-and-true-p whale-line--testing)
         (message "Couldn't create `%s' augment" ',name)))))

;; Segments:

(defvar whale-line-margin--segment (whale-line--spacer))

(whale-line-create-static-segment buffer-name
  :getter
  (let* ((identification (car-safe mode-line-buffer-identification))
         (help (get-text-property 0 'help-echo identification))
         (map (get-text-property 0 'local-map identification)))

    (propertize "%b" 'help-echo help 'mouse-face 'whale-line-highlight 'local-map map))
  :hooks
  (find-file-hook
   after-save-hook
   clone-indirect-buffer-hook)
  :advice
  (:after . (not-modified rename-buffer set-visited-file-name pop-to-buffer undo)))

(whale-line-create-dynamic-segment buffer-status
  :getter
  (cond
   (buffer-read-only
    (propertize "@" 'face 'whale-line-contrast))
   ((not (buffer-file-name))
    (propertize "&" 'face 'whale-line-shadow))
   ((buffer-modified-p)
    (propertize "*" 'face 'whale-line-emphasis))
   (t ""))
  :dense t)

(whale-line-create-dynamic-segment window-status
  :getter
  (when (window-dedicated-p)
    (propertize "^" 'face 'whale-line-shadow))
  :priority 'low)

(whale-line-create-dynamic-segment position
  :getter
  (let* ((following (bound-and-true-p follow-mode))
         (str (if following "f: %l:%c %p%" "%l:%c %p%")))
    (propertize str 'face 'whale-line-shadow))
  :priority 'current)

(whale-line-create-dynamic-segment global-mode-string
  :getter
  (cons (whale-line--spacer) (cdr global-mode-string))
  :dense t
  :priority 'current-low)

(whale-line-create-dynamic-segment minor-modes
  :getter (lambda () minor-mode-alist)
  :dense t
  :priority 'low)

(whale-line-create-dynamic-segment process
  :getter
  (let ((mlp mode-line-process))
    (cond
     ((listp mlp)
      (cons (whale-line--spacer) (cdr mlp)))
     ((stringp mlp)
      (propertize (concat (whale-line--spacer) mlp) 'face 'whale-line-shadow))
     (t "")))
  :condition mode-line-process
  :dense t
  :priority 'current)

(defun whale-line-selection--get-columns (beg end)
  "Get the columns from BEG to END for displaying `rectangle-mode'."
  (abs (- (save-excursion (goto-char end)
                          (current-column))
          (save-excursion (goto-char beg)
                          (current-column)))))

(whale-line-create-dynamic-segment selection
  :condition mark-active
  :priority 'current-low
  :getter
  (let* ((beg (region-beginning))
         (end (region-end))
         (lines (count-lines beg (min end (point-max)))))
    (propertize (if (bound-and-true-p rectangle-mark-mode)
                    (let ((columns (whale-line-selection--get-columns beg end)))
                      (format " %dx%d " lines columns))
                  (format " %d " lines))
                'face 'region)))

;; Rendering

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
                           (segment (intern (format whale-line-segment-fstring name)))
                           (setter (intern (format whale-line-set-segment-fstring name))))

                 (if (functionp segment)
                     `(:eval (,segment))
                   (let* ((val (symbol-value segment))
                          (eval (pcase val
                                  ('initial `(,setter))
                                  (_ segment))))

                     `(:eval ,eval)))))
             segments)))

;;;; Disabling/enabling:

(defun whale-line--enabled-feature-p (feature)
  "Check if FEATURE is enabled."
  (memq feature whale-line-features))

(defun whale-line--disabled-features ()
  "Get the disabled features."
  (seq-filter (lambda (it) (not (whale-line--enabled-feature-p it)))
              whale-line--all-features))

(defun whale-line--enable-or-disable-feature (feature enable)
  "If ENABLE is t, enable FEATURE, otherwise disable."
  (let* ((symbol? (if (symbolp feature) feature (intern feature)))
         (suffix (if enable "--setup" "--teardown"))
         (func (intern (concat "whale-line-" (symbol-name symbol?) suffix))))

    (when (or (and enable (memq symbol? whale-line-features))
              (and (not enable)
                   (not (memq symbol? whale-line-features))))
      (user-error "Feature %s is already %s" symbol? (if enable "enabled" "disabled")))

    (whale-line--set-segment-priority symbol? enable)

    (setq whale-line-features (if (and enable (not (memq symbol? whale-line-features)))
                                  (append whale-line-features (list symbol?))
                                (delete symbol? whale-line-features)))
    (when (functionp func)
      (funcall func))))

(defvar whale-line--default-mode-line nil)

(defun whale-line-mode--setup ()
  "Set up `whale-line-mode'."
  (dolist (it whale-line-features)
    (require (intern (concat "whale-line-" (symbol-name it)))))
  ;; Save a copy of the previous mode-line.
  (setq whale-line--default-mode-line mode-line-format)

  ;; Make setups do their thing.
  (run-hooks 'whale-line-setup-hook)
  (add-hook 'pre-redisplay-functions #'whale-line--set-selected-window)

  ;; Set the new mode-line-format
  (setq-default mode-line-format '("%e" (:eval (whale-line--format)))))

(defun whale-line-mode--teardown ()
  "Tear down `whale-line-mode'."
  ;; Tear down everything.
  (run-hooks 'whale-line-teardown-hook)
  (remove-hook 'pre-redisplay-functions #'whale-line--set-selected-window)

  ;; Restore the original mode-line format
  (setq-default mode-line-format whale-line--default-mode-line))

;; Entrypoint.

(defvar whale-line--default-mode-line nil)
(defvar whale-line-setup-hook nil)
(defvar whale-line-teardown-hook nil)

(defun whale-line-disable-feature (feature)
  "Disable FEATURE."
  (interactive (list (completing-read "Disable feature: " whale-line-features)))
  (whale-line--enable-or-disable-feature feature nil))

(defun whale-line-enable-feature (feature)
  "Enable disabled FEATURE."
  (interactive (list (completing-read "Enable feature: " (whale-line--disabled-features))))
  (whale-line--enable-or-disable-feature feature t))

;;;###autoload
(define-minor-mode whale-line-mode
  "Toggle mood-line on or off."
  :group 'whale-line
  :global t
  :lighter " wll"
  (if whale-line-mode
      (whale-line-mode--setup)
    (whale-line-mode--teardown)))

(provide 'whale-line)

;;; whale-line.el ends here
