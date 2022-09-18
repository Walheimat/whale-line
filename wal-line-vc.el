;;; wal-line-vc.el --- Version control segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Displays version control information in the mode line.

;;; Code:

(require 'wal-line)

(declare-function wal-line--is-current-window-p "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")

(defvar wal-line-vc--scope-regexp "\\(feature\\|\\(\\w+\\)?fix\\|improvement\\)\\/")

(defvar-local wal-line-vc--state nil)
(defvar-local wal-line-vc--info nil)
(defvar-local wal-line-vc--segment nil)

;; State:

(defun wal-line-vc--update-state ()
  "Update the version control state."
  (when-let ((state (wal-line-vc--get-state)))
    (setq-local wal-line-vc--state state)))

(defun wal-line-vc--get-state ()
  "Get the version control state."
  (let ((backend (vc-backend buffer-file-name)))
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

(defun wal-line-vc--update (&rest _)
  "Update and (re-)set the segment."
  (wal-line-vc--update-state)
  (wal-line-vc--update-info)

  (setq-local wal-line-vc--segment (concat (wal-line--spacer) wal-line-vc--info)))

(defvar wal-line--segments)
(wal-line-add-segment vc)

(defun wal-line-vc--setup ()
  "Set up version control segment."
  (add-hook 'find-file-hook #'wal-line-vc--update)
  (add-hook 'after-save-hook #'wal-line-vc--update)
  (advice-add #'vc-refresh-state :after #'wal-line-vc--update))

(defun wal-line-vc--teardown ()
  "Tear down version control segment."
  (remove-hook 'find-file-hook #'wal-line-vc--update)
  (remove-hook 'after-save-hook #'wal-line-vc--update)
  (advice-remove #'vc-refresh-state #'wal-line-vc--update))

(add-hook 'wal-line-setup-hook #'wal-line-vc--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-vc--teardown)

(provide 'wal-line-vc)

;;; wal-line-vc.el ends here
