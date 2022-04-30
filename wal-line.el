;;; wal-line.el --- A whale-based mode-line. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; The mode-line I use.

;;; Code:

(require 'wal-line-utils)

(declare-function wal-line--set-selected-window "wal-line-utils.el")
(declare-function wal-line--is-current-window-p "wal-line-utils.el")
(declare-function wal-line--spacer "wal-line-utils.el")

;;;; Customization:

(defgroup wal-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defconst wal-line--all-features '(flycheck
                                   project
                                   icons
                                   vc
                                   whale
                                   minions))
(defcustom wal-line-features (copy-tree wal-line--all-features)
  "Optional features to add or enhance segments."
  :group 'wal-line
  :type '(repeat symbol))

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
  '((t :inherit success))
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

;;;; Functionality:

(defvar wal-line--segments
  '(:left ((margin . t)
           (icons . nil)
           (buffer-name . t)
           (buffer-status . t)
           (position . t))
    :right ((minor-modes . t)
            (global-mode-string . t)
            (project . nil)
            (vc . nil)
            (whale . nil)
            (margin . t))))

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
   ((buffer-modified-p)
    (propertize "*" 'face 'wal-line-emphasis))
    (t "")))

(defun wal-line-position--segment ()
  "Displays the current-position."
  (if (wal-line--is-current-window-p)
      (propertize (concat (wal-line--spacer) "%l %p% ") 'face 'wal-line-shadow)
    ""))

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

(defun wal-line--render-segments (segments)
  "Render SEGMENTS."
  (delq nil
        (mapcar (lambda (it)
                  (when (cdr it)
                    `(:eval (,(intern (concat "wal-line-" (symbol-name (car it)) "--segment"))))))
                segments)))

(defun wal-line--disabled-features ()
  "Get the disabled features."
  (seq-filter (lambda (it) (not (memq it wal-line-features)))
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
           (teardown (intern (concat "wal-line-" (symbol-name symbol?) suffix))))
      (when (fboundp teardown)
        (funcall teardown)))))

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

(dolist (it wal-line-features)
  (require (intern (concat "wal-line-" (symbol-name it)))))

(provide 'wal-line)

;;; wal-line.el ends here
