;;; wal-line-vc.el --- Version control segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Displays version control information in the mode line.

;;; Code:

(require 'wal-line)

(declare-function wal-line--is-current-window-p "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")

;; State:

(defvar-local wal-line-vc--state nil)

(defun wal-line-vc--update-state ()
  "Update the version control state."
  (when-let ((state (wal-line-vc--get-state)))
    (setq-local wal-line-vc--state state)))

(defun wal-line-vc--get-state ()
  "Get the version control state."
  (when-let ((backend (vc-backend buffer-file-name)))
    (vc-state (file-local-name buffer-file-name) backend)))

(defun wal-line-vc--face-for-state ()
  "Get the correct face for the state."
  (let ((state wal-line-vc--state))
    (cond ((eq state 'needs-update)
           'wal-line-contrast)
          ((eq state 'edited)
           'wal-line-indicate)
          ((memq state '(removed conflict unregistered))
           'wal-line-contrast)
          (t 'wal-line-neutral))))

;; Info:

(defvar wal-line-vc--scope-regexp "\\(feature\\|\\(\\w+\\)?fix\\|improvement\\)\\/")
(defvar-local wal-line-vc--info nil)

(defun wal-line-vc--update-info ()
  "Update version control info."
  (when-let ((info (wal-line-vc--get-info)))
    (setq-local wal-line-vc--info info)))

(defun wal-line-vc--get-info ()
  "Get version control info."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (status (if vc-display-status
                       (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                     ""))
           (str (replace-regexp-in-string wal-line-vc--scope-regexp "" status)))
      (propertize str
                  'mouse-face 'wal-line-highlight
                  'face (wal-line-vc--face-for-state)))))

;; Segment:

(wal-line-create-static-segment vc
  :getter
  (progn
    (wal-line-vc--update-state)
    (wal-line-vc--update-info)
    wal-line-vc--info)
  :setup
  (lambda ()
    (add-hook 'find-file-hook #'wal-line-vc--set-segment)
    (add-hook 'after-save-hook #'wal-line-vc--set-segment)
    (advice-add 'vc-refresh-state :after #'wal-line-vc--set-segment))
  :teardown
  (lambda ()
    (remove-hook 'find-file-hook #'wal-line-vc--set-segment)
    (remove-hook 'after-save-hook #'wal-line-vc--set-segment)
    (advice-remove 'vc-refresh-state #'wal-line-vc--set-segment)))

(provide 'wal-line-vc)

;;; wal-line-vc.el ends here
