;;; wal-line-all-the-icons.el --- Icons for the modeline. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1")(all-the-icons "5.0.0"))

;;; Commentary:

;; File icons for the modeline.

;;; Code:

(require 'all-the-icons)

(eval-when-compile
  (require 'wal-line-utils (expand-file-name "wal-line-utils.el")))
(declare-function wal-line--spacer "wal-line-utils.el")
(declare-function wal-line--segment-project "wal-line-project.el")

;;;; Functionality:

(defvar-local wal-line--icon nil)
(defun wal-line--set-icon (&rest _)
  "Update file icon in mode-line."
  (setq-local wal-line--icon
              (if (display-graphic-p)
                  (let ((icon (all-the-icons-icon-for-buffer)))
                    (propertize (if (or (null icon) (symbolp icon))
                                    (all-the-icons-faicon "question-circle-o")
                                  icon)
                                'face 'wal-line-contrast
                                'help-echo (format "%s" (format-mode-line mode-name))
                                'display '(raise 0.01)))
                mode-name)))

(defun wal-line--segment-icon ()
  "Display the file icon for the visited file."
  (unless wal-line--icon
    (wal-line--set-icon))
  (concat wal-line--icon (wal-line--spacer)))

(defun wal-line-all-the-icons--advise-project (str)
  "Advise project segment to show a project icon before STR."
  (if (display-graphic-p)
      (concat
       (wal-line--spacer)
       (propertize
        (all-the-icons-faicon "folder-open-o")
        'face 'wal-line-emphasis
        'display '(raise 0.01))
       str)
    str))

;; Setup/teardown:

(defun wal-line-all-the-icons--setup ()
  "Set up icons."
  (advice-add
   #'wal-line--segment-project
   :filter-return #'wal-line-all-the-icons--advise-project)
  (add-hook 'find-file-hook #'wal-line--set-icon)
  (add-hook 'after-change-major-mode-hook #'wal-line--set-icon)
  (add-hook 'clone-indirect-buffer-hook #'wal-line--set-icon))

(defun wal-line-all-the-icons--teardown()
  "Tear down icons."
  (advice-remove
   #'wal-line--segment-project
   #'wal-line-all-the-icons--advise-project)
  (remove-hook 'find-file-hook #'wal-line--set-icon)
  (remove-hook 'after-change-major-mode-hook #'wal-line--set-icon)
  (remove-hook 'clone-indirect-buffer-hook #'wal-line--set-icon))

(defvar wal-line--left-side)
(wal-line-add-segment wal-line--segment-icon 'left 'wal-line--spacer)

(add-hook 'wal-line-setup-hook #'wal-line-all-the-icons--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-all-the-icons--teardown)

(provide 'wal-line-all-the-icons)

;;; wal-line-all-the-icons.el ends here
