;;; wal-line-vc.el --- Version control integration. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.1
;; Package-Requires: ((emacs "28.1")(flycheck "33-cvs"))

;;; Commentary:

;; Displays version control information in the mode line.

;;; Code:

(require 'wal-line-utils)

(declare-function wal-line--is-current-window-p "wal-line-utils.el")
(declare-function wal-line--spacer "wal-line-utils.el")

(defvar wal-line-vc--scope-regexp "\\(feature\\|\\(\\w+\\)?fix\\|improvement\\)\\/")
(defvar-local wal-line-vc--info nil)
(defvar-local wal-line-vc--state nil)
(defun wal-line-vc--update-info (&rest _)
  "Update the version control info."
  (setq-local wal-line-vc--info
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state (file-local-name buffer-file-name) backend))
                 (str (if vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        "")))
            (setq-local wal-line-vc--state state)
            (propertize (replace-regexp-in-string
                         wal-line-vc--scope-regexp
                         ""
                         str)
                        'mouse-face 'wal-line-highlight
                        'face (wal-line-vc--face-for-state))))))

(defun wal-line-vc--segment ()
  "Show version control info."
  (unless wal-line-vc--info
    (wal-line-vc--update-info))
  (if (wal-line--is-current-window-p)
      wal-line-vc--info
    ""))

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

(defvar wal-line--segments)
(wal-line-add-segment vc)

(defun wal-line-vc--setup ()
  "Set up version control segment."
  (add-hook 'find-file-hook #'wal-line-vc--update-info)
  (add-hook 'after-save-hook #'wal-line-vc--update-info)
  (advice-add #'vc-refresh-state :after #'wal-line-vc--update-info))

(defun wal-line-vc--teardown ()
  "Tear down version control segment."
  (remove-hook 'find-file-hook #'wal-line-vc--update-info)
  (remove-hook 'after-save-hook #'wal-line-vc--update-info)
  (advice-remove #'vc-refresh-state #'wal-line-vc--update-info))

(add-hook 'wal-line-setup-hook #'wal-line-vc--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-vc--teardown)

(provide 'wal-line-vc)

;;; wal-line-vc.el ends here
