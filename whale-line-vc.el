;;; whale-line-vc.el --- Version control segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Displays version control information in the mode line.

;;; Code:

(require 'whale-line)

(declare-function whale-line--is-current-window-p "whale-line.el")
(declare-function whale-line--spacer "whale-line.el")

;;; -- State

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
           'whale-line-contrast)
          ((eq state 'edited)
           'whale-line-indicate)
          ((memq state '(removed conflict unregistered))
           'whale-line-contrast)
          (t 'whale-line-neutral))))

;;; -- Info

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
                  'mouse-face 'whale-line-highlight
                  'face (wlvc--face-for-state)))))

;;; -- Segment

(whale-line-create-static-segment vc
  :getter
  (progn
    (wlvc--update-state)
    (wlvc--update-info)
    wlvc--info)

  :hooks
  (find-file-hook after-save-hook)

  :advice
  (:after . (vc-refresh-state)))

(provide 'whale-line-vc)

;;; whale-line-vc.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wlvc-" . "whale-line-vc-"))
;; End:
