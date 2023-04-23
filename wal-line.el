;;; wal-line.el --- A whale-based mode-line -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; The mode-line I use.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defconst wal-line--all-features '(flycheck
                                   project
                                   icons
                                   vc
                                   animation
                                   minions
                                   cursors
                                   lsp
                                   org
                                   tab-bar))

(defvar wal-line--segments '(:left ((margin . t)
                                    (icons . nil)
                                    (buffer-name . nil)
                                    (org . nil)
                                    (buffer-status . nil)
                                    (position . nil)
                                    (selection . nil)
                                    (cursors . nil)
                                    (process . nil)
                                    (window-status . nil))
                             :right ((minor-modes . nil)
                                     (global-mode-string . nil)
                                     (tab-bar . nil)
                                     (project . nil)
                                     (vc . nil)
                                     (animation . nil)
                                     (margin . t))))

;;;; Customization:

(defgroup wal-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom wal-line-features (copy-tree wal-line--all-features)
  "Optional features to add or enhance segments."
  :group 'wal-line
  :type '(repeat symbol))

(defcustom wal-line-segment-strategy 'prioritize
  "Strategy used when lack of space prohibits displaying all segments.

Strategy `prioritize' filters out segments with low priority.
Strategy `elide' only displays the left side. Strategy `ignore'
will display both sides unchanged no matter the space
constraints."
  :group 'wal-line
  :type '(choice (const prioritize)
                 (const elide)
                 (const ignore)))

;;;; Faces:

(defface wal-line-neutral
  '((t))
  "Neutral face."
  :group 'wal-line)

(defface wal-line-shadow
  '((t (:inherit (shadow))))
  "Shadow face."
  :group 'wal-line)

(defface wal-line-highlight
  '((t (:inherit (mode-line-highlight))))
  "Face used for highlight."
  :group 'wal-line)

(defface wal-line-indicate
  '((t :inherit (success)))
  "Face used for indicating (something good)."
  :group 'wal-line)

(defface wal-line-emphasis
  '((t (:inherit (mode-line-emphasis))))
  "Face used for emphasis."
  :group 'wal-line)

(defface wal-line-contrast
  '((t (:inherit (warning))))
  "Face used for contrast."
  :group 'wal-line)

(defface wal-line-notification
  '((t (:inherit (compilation-info))))
  "Face used for notification."
  :group 'wal-line)

;;;; Utility:

(defun wal-line--spacer (&optional big)
  "A space used for padding.

Optionally, use a BIG spacer."
  (if big "  " " "))

;; Formatting:

(defun wal-line--format-side (side &optional filter)
  "Get the formatted SIDE.

Optionally FILTER out low priority segments."
  (format-mode-line (wal-line--render side filter)))

(defun wal-line--enough-space-p ()
  "Calculate whether there is enough space to display both sides' segments."
  (let* ((f-pixel (window-font-width))
         (left (* f-pixel (length (wal-line--format-side :left))))
         (right (* f-pixel (length (wal-line--format-side :right)))))
    (> (- (window-pixel-width) (+ left right)) 0)))

(defun wal-line--format-ignore ()
  "Format mode line ignoring space constraints."
  (let ((lhs (wal-line--render :left))
        (rhs (wal-line--render :right))
        (rlen (length (wal-line--format-side :right))))
    `(,@lhs
      ,(propertize " " 'display
                   `((space :align-to (- right (- 0 right-margin) ,rlen))))
      ,@rhs)))

(defun wal-line--format-elide ()
  "Format mode line, eliding right side if space is lacking."
  (let ((lhs (wal-line--render :left))
        (rhs (wal-line--render :right))
        (rlen (length (wal-line--format-side :right)))
        (space? (wal-line--enough-space-p)))
    `(,@lhs
      ,(propertize
        " "
        'display `((space :align-to (- right (- 0 right-margin) ,(if space? rlen 5)))))
      ,@(if space?
            rhs
          '((:eval (propertize (concat (wal-line--spacer) "..." (wal-line--spacer))
                               'face 'wal-line-shadow)))))))

(defun wal-line--format-prioritize ()
  "Format mode line, prioritizing certain segments if space is lacking."
  (if (wal-line--enough-space-p)
      (wal-line--format-ignore)
    (let ((lhs (wal-line--render :left t))
          (rhs (wal-line--render :right t))
          (rlen (length (wal-line--format-side :right t))))
      `(,@lhs
        ,(propertize " " 'display
                     `((space :align-to (- right (- 0 right-margin) ,rlen))))
        ,@rhs))))

(defun wal-line--format ()
  "Return a list of aligned left and right segments.

If there's not enough space, only shows the left segments and an
ellipsis."
  (pcase wal-line-segment-strategy
    ('ignore
     (wal-line--format-ignore))
    ('elide
     (wal-line--format-elide))
    ('prioritize
     (wal-line--format-prioritize))))

;; Windows:

(defvar wal-line--current-window nil)

(defun wal-line--get-current-window ()
  "Get the current window but should exclude the child windows."
  (if (and (fboundp 'frame-parent) (frame-parent))
      (frame-selected-window (frame-parent))
    (frame-selected-window)))

(defun wal-line--set-selected-window (&rest _)
  "Set selected window appropriately."
  (let ((win (wal-line--get-current-window)))
    (setq wal-line--current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defun wal-line--is-current-window-p ()
  "Check if the current window is the selected window."
  (and wal-line--current-window
       (eq (wal-line--get-current-window) wal-line--current-window)))

;; Priorities:

(defun wal-line--set-segment-priority (segment priority)
  "Set PRIORITY of a SEGMENT."
  (let ((left? (assoc segment (plist-get wal-line--segments :left)))
        (right? (assoc segment (plist-get wal-line--segments :right))))
    (cond
     (left?
      (setcdr left? priority))
     (right?
      (setcdr right? priority))
     (t (user-error "Unknown segment")))))

(defun wal-line-add-segment (segment &optional priority)
  "Add SEGMENT to the list of segments.

Optionally with a PRIORITY."
  (let ((prio (or priority t)))
    (wal-line--set-segment-priority segment prio)))

;; Macros:

(defvar wal-line-augment-fstring "wal-line-%s--augment")
(defvar wal-line-segment-fstring "wal-line-%s--segment")
(defvar wal-line-set-segment-fstring "wal-line-%s--set-segment")
(defvar wal-line-get-segment-fstring "wal-line-%s--get-segment")
(defvar wal-line-setup-fstring "wal-line-%s--setup")
(defvar wal-line-teardown-fstring "wal-line-%s--teardown")

(cl-defmacro wal-line-create-static-segment (name &key getter hooks advice verify setup teardown dense priority)
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

  (defvar wal-line-segment-fstring "wal-line-%s--segment")
  (defvar wal-line-set-segment-fstring "wal-line-%s--set-segment")
  (defvar wal-line-get-segment-fstring "wal-line-%s--get-segment")
  (defvar wal-line-setup-fstring "wal-line-%s--setup")
  (defvar wal-line-teardown-fstring "wal-line-%s--teardown")

  (let ((segment (intern (format wal-line-segment-fstring (symbol-name name))))
        (setter (intern (format wal-line-set-segment-fstring (symbol-name name))))
        (setup-sym (intern (format wal-line-setup-fstring (symbol-name name))))
        (teardown-sym (intern (format wal-line-teardown-fstring (symbol-name name))))
        (getter-sym (intern (format wal-line-get-segment-fstring (symbol-name name))))
        (prio (or priority t)))

    (if (or (null verify) (funcall verify))
        `(progn
           (defvar ,segment 'initial)

           (defun ,getter-sym ()
             ,(format "Get the %s segment." name)
             ,getter)

           (defun ,setter (&rest _)
             ,(format "Set %s segment." name)
             (if-let ((str (,getter-sym)))
                 (setq-local ,segment ,(if dense 'str '(concat (wal-line--spacer) str)))
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

                   ,(when setup `(funcall ,setup)))

                 (add-hook 'wal-line-setup-hook #',setup-sym)))

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

                   ,(when teardown `(funcall ,teardown)))

                 (add-hook 'wal-line-teardown-hook #',teardown-sym)))

           (wal-line-add-segment ',name ,prio))
      `(progn
         (defvar ,segment nil)
         (message "Couldn't add `%s' segment" ',name)))))

(cl-defmacro wal-line-create-dynamic-segment (name &key getter condition verify setup teardown dense priority)
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

  (defvar wal-line-segment-fstring "wal-line-%s--segment")
  (defvar wal-line-get-segment-fstring "wal-line-%s--get-segment")
  (defvar wal-line-setup-fstring "wal-line-%s--setup")
  (defvar wal-line-teardown-fstring "wal-line-%s--teardown")

  (let ((segment (intern (format wal-line-segment-fstring (symbol-name name))))
        (getter-sym (intern (format wal-line-get-segment-fstring (symbol-name name))))
        (setup-sym (intern (format wal-line-setup-fstring (symbol-name name))))
        (teardown-sym (intern (format wal-line-teardown-fstring (symbol-name name))))
        (prio (or priority t))
        (con (or condition t)))

    (if (or (null verify) (funcall verify))
        `(progn
           (defun ,getter-sym ()
             ,(format "Get the `%s' segment." name)
             ,getter)

           (defun ,segment ()
             ,(format "Render `%s' segment." name)
             (or (when ,con
                   ,(if dense `(,getter-sym) `(concat (wal-line--spacer) (,getter-sym))))
                 ""))

           ,(when setup
              `(progn
                 (defun ,setup-sym (&rest _)
                   ,(format "Set up %s segment." name)
                   (funcall ,setup))

                 (add-hook 'wal-line-setup-hook #',setup-sym)))

           ,(when teardown
              `(progn
                 (defun ,teardown-sym (&rest _)
                   ,(format "Tear down %s segment." name)
                   (funcall ,teardown))

                 (add-hook 'wal-line-teardown-hook #',teardown-sym)))

           (wal-line-add-segment ',name ,prio))
      `(progn
         (defun ,segment ()
           ,(format "Render `%s' segment as an empty string." name)
           "")
         (message "Couldn't add `%s' segment" ',name)))))

(cl-defmacro wal-line-create-augment (name &key verify action hooks advice setup teardown)
  "Create augment(-or) named NAME.

VERIFY is an optional function called before augmenting. If that
function returns nil, a user error will indicate it didn't take
place.

ACTION is the function to call for HOOKS.

ADVICE is an cons cell of the form combinator .
functions-to-advise to call ACTION.

Additional SETUP and TEARDOWN function can be added for more control."
  (declare (indent defun))

  (defvar wal-line-augment-fstring "wal-line-%s--augment")
  (defvar wal-line-setup-fstring "wal-line-%s--setup")
  (defvar wal-line-teardown-fstring "wal-line-%s--teardown")

  (let ((augment (intern (format wal-line-augment-fstring (symbol-name name))))
        (setup-sym (intern (format wal-line-setup-fstring (symbol-name name))))
        (teardown-sym (intern (format wal-line-teardown-fstring (symbol-name name)))))

    (if (or (null verify) (funcall verify))
        `(progn
           (defun ,augment (&rest args)
             ,(format "Augment function for `%s'" name)
             (apply ,action args))

           ,(when (or hooks advice setup)
              `(progn
                 (defun ,setup-sym (&rest _)
                   ,(format "Set up %s segment." name)
                   ,@(mapcar (lambda (it)
                               `(add-hook ',it #',augment)) hooks)

                   ,@(mapcar (lambda (it)
                               `(advice-add ',it ,(car advice) #',augment)) (cdr advice))

                   ,(when setup `(funcall ,setup)))

                 (add-hook 'wal-line-setup-hook #',setup-sym)))

           ,(when (or hooks advice setup)
              `(progn
                 (defun ,teardown-sym (&rest _)
                   ,(format "Tear down %s segment." name)

                   ,@(mapcar (lambda (it)
                               `(remove-hook ',it #',augment)) hooks)

                   ,@(mapcar (lambda (it)
                               `(advice-remove ',it #',augment)) (cdr advice))

                   ,(when teardown `(funcall ,teardown)))

                 (add-hook 'wal-line-teardown-hook #',teardown-sym))))
      `(message "Couldn't create `%s' augment" ',name))))

;; Segments:

(defvar wal-line-margin--segment (wal-line--spacer))

(wal-line-create-static-segment buffer-name
  :getter
  (buffer-name)
  :hooks
  (find-file-hook
   after-save-hook
   clone-indirect-buffer-hook)
  :advice
  (:after . (not-modified rename-buffer set-visited-file-name pop-to-buffer undo)))

(wal-line-create-dynamic-segment buffer-status
  :getter
  (cond
   (buffer-read-only
    (propertize "@" 'face 'wal-line-contrast))
   ((not (buffer-file-name))
    (propertize "&" 'face 'wal-line-shadow))
   ((buffer-modified-p)
    (propertize "*" 'face 'wal-line-emphasis))
   (t ""))
  :dense t)

(wal-line-create-dynamic-segment window-status
  :getter
  (when (window-dedicated-p)
    (propertize "^" 'face 'wal-line-shadow))
  :priority 'low)

(wal-line-create-dynamic-segment position
  :getter
  (let* ((following (bound-and-true-p follow-mode))
         (str (if following "f: %l:%c %p%" "%l:%c %p%")))
    (propertize str 'face 'wal-line-shadow))
  :priority 'current)

(wal-line-create-dynamic-segment global-mode-string
  :getter
  (cons (wal-line--spacer) (cdr global-mode-string))
  :dense t
  :priority 'current-low)

(wal-line-create-dynamic-segment minor-modes
  :getter
  minor-mode-alist
  :dense t
  :priority 'low)

(wal-line-create-dynamic-segment process
  :getter
  (let ((mlp mode-line-process))
    (cond
     ((listp mlp)
      (cons (wal-line--spacer) (cdr mlp)))
     ((stringp mlp)
      (propertize (concat (wal-line--spacer) mlp) 'face 'wal-line-shadow))
     (t "")))
  :condition mode-line-process
  :dense t
  :priority 'current)

(defun wal-line-selection--get-columns (beg end)
  "Get the columns from BEG to END for displaying `rectangle-mode'."
  (abs (- (save-excursion (goto-char end)
                          (current-column))
          (save-excursion (goto-char beg)
                          (current-column)))))

(wal-line-create-dynamic-segment selection
  :condition mark-active
  :priority 'current-low
  :getter
  (let* ((beg (region-beginning))
         (end (region-end))
         (lines (count-lines beg (min end (point-max)))))
    (propertize (if (bound-and-true-p rectangle-mark-mode)
                    (let ((columns (wal-line-selection--get-columns beg end)))
                      (format " %dx%d " lines columns))
                  (format " %d " lines))
                'face 'region)))

;; Rendering

(defun wal-line--filter (segments &optional low-space)
  "Filter SEGMENTS.

This filters differently for current and other window.

If LOW-SPACE is t, additional segments are filtered."
  (let ((filter (if (wal-line--is-current-window-p)
                         (wal-line--filter-for-current low-space)
                  (wal-line--filter-for-other low-space))))
    (seq-filter (lambda (it) (not (memq (cdr it) filter))) segments)))


(defun wal-line--filter-for-current (&optional low-space)
  "Build the filter for current window.

If LOW-SPACE is t, filter out additional segments."
  (if low-space
      (list 'low 'current-low)
    nil))

(defun wal-line--filter-for-other (&optional low-space)
  "Build the filter for other window.

If LOW-SPACE is t, filter out additional segments."
  (if low-space
      (list 'current 'current-low 'low)
    (list 'current 'current-low)))

(defun wal-line--render (side &optional filter)
  "Render SIDE.

Optionally FILTER out low priority segments."
  (let* ((segments (plist-get wal-line--segments side))
         (filtered (wal-line--filter segments filter)))
    (wal-line--render-segments filtered)))

(defun wal-line--render-segments (segments)
  "Render SEGMENTS."
  (delq nil (mapcar
             (lambda (it)
               (when-let* ((should-use (cdr it))
                           (name (symbol-name (car it)))
                           (segment (intern (format wal-line-segment-fstring name)))
                           (setter (intern (format wal-line-set-segment-fstring name))))

                 (if (functionp segment)
                     `(:eval (,segment))
                   (let* ((val (symbol-value segment))
                          (eval (pcase val
                                  ('initial `(,setter))
                                  (_ segment))))

                     `(:eval ,eval)))))
             segments)))

;;;; Disabling/enabling:

(defun wal-line--enabled-feature-p (feature)
  "Check if FEATURE is enabled."
  (memq feature wal-line-features))

(defun wal-line--disabled-features ()
  "Get the disabled features."
  (seq-filter (lambda (it) (not (wal-line--enabled-feature-p it)))
              wal-line--all-features))

(defun wal-line--enable-or-disable-feature (feature enable)
  "If ENABLE is t, enable FEATURE, otherwise disable."
  (let* ((symbol? (if (symbolp feature) feature (intern feature)))
         (suffix (if enable "--setup" "--teardown"))
         (func (intern (concat "wal-line-" (symbol-name symbol?) suffix))))

    (wal-line--set-segment-priority symbol? enable)

    (setq wal-line-features (if enable
                                (append wal-line-features (list symbol?))
                              (delete symbol? wal-line-features)))
    (when (fboundp func)
      (funcall func))))

;; Entrypoint.

(defvar wal-line--default-mode-line nil)
(defvar wal-line-setup-hook nil)
(defvar wal-line-teardown-hook nil)

(defun wal-line-disable-feature (feature)
  "Disable FEATURE."
  (interactive (list (completing-read "Disable feature: " wal-line-features)))
  (wal-line--enable-or-disable-feature feature nil))

(defun wal-line-enable-feature (feature)
  "Enable disabled FEATURE."
  (interactive (list (completing-read "Enable feature: " (wal-line--disabled-features))))
  (wal-line--enable-or-disable-feature feature t))

;;;###autoload
(define-minor-mode wal-line-mode
  "Toggle mood-line on or off."
  :group 'wal-line
  :global t
  :lighter " wll"
  (if wal-line-mode
      (progn
        (dolist (it wal-line-features)
          (require (intern (concat "wal-line-" (symbol-name it)))))
        ;; Save a copy of the previous mode-line.
        (setq wal-line--default-mode-line mode-line-format)

        ;; Make setups do their thing.
        (run-hooks 'wal-line-setup-hook)
        (add-hook 'pre-redisplay-functions #'wal-line--set-selected-window)

        ;; Set the new mode-line-format
        (setq-default mode-line-format '("%e" (:eval (wal-line--format)))))
    (progn
      ;; Tear down everything.
      (run-hooks 'wal-line-teardown-hook)
      (remove-hook 'pre-redisplay-functions #'wal-line--set-selected-window)

      ;; Restore the original mode-line format
      (setq-default mode-line-format wal-line--default-mode-line))))

(provide 'wal-line)

;;; wal-line.el ends here
