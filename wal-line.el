;;; wal-line.el --- Yet another mode-line. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; The mode-line I use.

;;; Code:

(eval-when-compile
  (require 'wal-line-utils (expand-file-name "wal-line-utils.el")))

(declare-function wal-line--set-selected-window "wal-line-utils.el")
(declare-function wal-line--is-current-window-p "wal-line-utils.el")

;;;; Customization:

(defgroup wal-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

;;;; Faces:

(defface wal-line-neutral
  '((t))
  "Neutral face."
  :group 'wal-line)

(defface wal-line-highlight
  '((t (:inherit (mode-line-emphasis))))
  "Face used for highlight."
  :group 'wal-line)

(defface wal-line-emphasis
  '((t (:inherit (mode-line-emphasis) :bold t)))
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

(defun wal-line--segment-buffer-name ()
  "Get the buffer name."
  (buffer-name))

(defun wal-line--segment-buffer-status ()
  "Display the buffer status."
  (cond
   (buffer-read-only
    (propertize "@" 'face 'wal-line-contrast))
   ((buffer-modified-p)
    (propertize "*" 'face 'wal-line-notify))
    (t " ")))

(defun wal-line--segment-position ()
  "Displays the current-position."
  (if (wal-line--is-current-window-p)
      "%l %p%% "
    ""))

(defun wal-line--segment-global-mode-string ()
  "Displays the `global-mode-string'."
  (if (wal-line--is-current-window-p)
      global-mode-string
    ""))

(defun wal-line--render-segments (segments)
  "Render SEGMENTS."
  (mapcar (lambda (it) `(:eval (,it))) segments))

(defvar wal-line--left-side
  '(wal-line--spacer
    wal-line--segment-buffer-name
    wal-line--segment-buffer-status))

(defvar wal-line--right-side
  '(wal-line--segment-global-mode-string
    wal-line--spacer
    wal-line--segment-position
    wal-line--spacer))

;; Entrypoint.

(defvar wal-line--default-mode-line nil)
(defvar wal-line-setup-hook nil)
(defvar wal-line-teardown-hook nil)

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
        (setq-default mode-line-format
                      '((:eval
                         (wal-line--format
                          (format-mode-line (wal-line--render-segments wal-line--left-side))
                          (format-mode-line (wal-line--render-segments wal-line--right-side)))))))
    (progn
      ;; Tear down everything.
      (run-hooks 'wal-line-teardown-hook)
      (remove-hook 'pre-redisplay-functions #'wal-line--set-selected-window)

      ;; Restore the original mode-line format
      (setq-default mode-line-format wal-line--default-mode-line))))

(provide 'wal-line)

;;; wal-line.el ends here
