;;; wal-line.el --- A whale-based mode-line -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
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
                                   whale
                                   minions
                                   mc
                                   lsp
                                   org))

(defvar wal-line--segments
  '(:left ((margin . t)
           (icons . nil)
           (buffer-name . t)
           (org . t)
           (buffer-status . t)
           (position . t)
           (selection . low)
           (mc . nil)
           (process . low))
    :right ((minor-modes . t)
            (global-mode-string . low)
            (project . nil)
            (vc . nil)
            (whale . nil)
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

(defun wal-line--render (side &optional filter)
  "Render SIDE.

Optionally FILTER out low priority segments."
  (let ((segments (plist-get wal-line--segments side)))
    (if filter
        (wal-line--render-segments
         (seq-filter (lambda (it) (not (eq (cdr it) 'low))) segments))
      (wal-line--render-segments segments))))

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

;;;; Segments:

(defmacro wal-line-add-segment (segment &optional priority)
  "Add SEGMENT to the list of segments.

Optionally with a PRIORITY."
  `(let ((left? (assoc ',segment (plist-get wal-line--segments :left)))
         (right? (assoc ',segment (plist-get wal-line--segments :right))))
     (cond
      (left?
       (setcdr left? ,(or priority t)))
      (right?
       (setcdr right? ,(or priority t)))
      (t (user-error "Unknown segment")))))

(defun wal-line-margin--segment ()
  "Get a margin segment."
  (wal-line--spacer))

(defun wal-line-buffer-name--segment ()
  "Get the buffer name."
  (concat (wal-line--spacer) (buffer-name)))

(defun wal-line-buffer-status--segment ()
  "Display the buffer status."
  (cond
   (buffer-read-only
    (propertize "@" 'face 'wal-line-contrast))
   ((not (buffer-file-name))
    (propertize "&" 'face 'wal-line-shadow))
   ((buffer-modified-p)
    (propertize "*" 'face 'wal-line-emphasis))
    (t "")))

(defun wal-line-position--segment ()
  "Displays the current-position."
  (let* ((following (bound-and-true-p follow-mode))
         (str (if following "f: %l:%c %p%" "%l:%c %p%")))
    (if (or (wal-line--is-current-window-p) following)
        (propertize (concat (wal-line--spacer) str) 'face 'wal-line-shadow)
      "")))

(defun wal-line-global-mode-string--segment ()
  "Displays the `global-mode-string'."
  (if (wal-line--is-current-window-p)
      (cons (wal-line--spacer) (cdr global-mode-string))
    ""))

(defun wal-line-minor-modes--segment ()
  "Displays the minor modes."
  (if (wal-line--is-current-window-p)
      minor-mode-alist
    ""))

(defun wal-line-process--segment ()
  "Display the process."
  (let ((mlp mode-line-process))
    (if (and (wal-line--is-current-window-p) mlp)
      (cond
       ((listp mlp)
        (cons (wal-line--spacer) (cdr mlp)))
       ((stringp mlp)
        (propertize (concat (wal-line--spacer) mlp) 'face 'wal-line-shadow))
       (t
        ""))
    "")))

(defun wal-line-selection--get-columns (beg end)
  "Get the columns from BEG to END for displaying `rectangle-mode'."
  (abs (- (save-excursion (goto-char end)
                          (current-column))
          (save-excursion (goto-char beg)
                          (current-column)))))

(defun wal-line-selection--segment ()
  "Display the selection."
  (if mark-active
      (let* ((beg (region-beginning))
             (end (region-end))
             (lines (count-lines beg (min end (point-max)))))
        (concat (wal-line--spacer)
                (propertize (if (bound-and-true-p rectangle-mark-mode)
                                (let ((columns (wal-line-selection--get-columns beg end)))
                                  (format " %dx%d " lines columns))
                              (format " %d " lines))
                            'face 'region)))
    ""))

(defun wal-line--render-segments (segments)
  "Render SEGMENTS."
  (delq nil
        (mapcar (lambda (it)
                  (when (cdr it)
                    `(:eval (,(intern (concat "wal-line-" (symbol-name (car it)) "--segment"))))))
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
         (left? (assoc symbol? (plist-get wal-line--segments :left)))
         (right? (assoc symbol? (plist-get wal-line--segments :right))))
    (cond
     (left?
      (setcdr left? enable))
     (right?
      (setcdr right? enable))
     (t nil))
    (setq wal-line-features (if enable
                                (append wal-line-features `(,symbol?))
                              (delete symbol? wal-line-features)))
    (let* ((suffix (if enable "--setup" "--teardown"))
           (func (intern (concat "wal-line-" (symbol-name symbol?) suffix))))
      (when (fboundp func)
        (funcall func)))))

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
