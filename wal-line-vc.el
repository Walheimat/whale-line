;;; wal-line-vc.el --- Version control segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Displays version control information in the mode line.

;;; Code:

(require 'wal-line)

(declare-function wal-line--is-current-window-p "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")

;; State:

(defvar-local wlvc--state nil)

(defun wlvc--update-state ()
  "Update the version control state."
  (when-let ((state (wlvc--get-state)))
    (setq-local wlvc--state state)))

(defun wlvc--get-state ()
  "Get the version control state."
  (when-let ((backend (vc-backend buffer-file-name)))
    (vc-state (file-local-name buffer-file-name) backend)))

(defun wlvc--face-for-state ()
  "Get the correct face for the state."
  (let ((state wlvc--state))
    (cond ((eq state 'needs-update)
           'wal-line-contrast)
          ((eq state 'edited)
           'wal-line-indicate)
          ((memq state '(removed conflict unregistered))
           'wal-line-contrast)
          (t 'wal-line-neutral))))

;; Info:

(defvar wlvc--scope-regexp "\\(feature\\|\\(\\w+\\)?fix\\|improvement\\)\\/")
(defvar-local wlvc--info nil)

(defun wlvc--update-info ()
  "Update version control info."
  (when-let ((info (wlvc--get-info)))
    (setq-local wlvc--info info)))

(defun wlvc--get-info ()
  "Get version control info."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (status (if vc-display-status
                       (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                     ""))
           (str (replace-regexp-in-string wlvc--scope-regexp "" status)))
      (propertize str
                  'mouse-face 'wal-line-highlight
                  'face (wlvc--face-for-state)))))

;; Segment:

(wal-line-create-static-segment vc
  :getter
  (progn
    (wlvc--update-state)
    (wlvc--update-info)
    wlvc--info)
  :setup
  (lambda ()
    (add-hook 'find-file-hook #'wlvc--set-segment)
    (add-hook 'after-save-hook #'wlvc--set-segment)
    (advice-add 'vc-refresh-state :after #'wlvc--set-segment))
  :teardown
  (lambda ()
    (remove-hook 'find-file-hook #'wlvc--set-segment)
    (remove-hook 'after-save-hook #'wlvc--set-segment)
    (advice-remove 'vc-refresh-state #'wlvc--set-segment)))

(provide 'wal-line-vc)

;;; wal-line-vc.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wlvc-" . "wal-line-vc-"))
;; End:
