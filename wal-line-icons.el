;;; wal-line-icons.el --- Icons for the modeline. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.1
;; Package-Requires: ((emacs "28.1")(all-the-icons "5.0.0"))

;;; Commentary:

;; Show icons in the modeline.

;;; Code:

(require 'all-the-icons)
(require 'wal-line-utils)

(declare-function wal-line--spacer "wal-line-utils.el")
(declare-function wal-line-project--segment "wal-line-project.el")

;;;; Functionality:

(defvar-local wal-line-icons--icon nil)
(defun wal-line-icons--set-icon (&rest _)
  "Update file icon in mode-line."
  (setq-local wal-line-icons--icon
              (if (display-graphic-p)
                  (let ((icon (all-the-icons-icon-for-buffer)))
                    (propertize (if (or (null icon) (symbolp icon))
                                    (all-the-icons-faicon "question-circle-o")
                                  icon)
                                'help-echo (format "%s" (format-mode-line mode-name))
                                'display '(raise -0.135)))
                mode-name)))

(defun wal-line-icons--segment ()
  "Display the file icon for the visited file."
  (unless wal-line-icons--icon
    (wal-line-icons--set-icon))
  (concat (wal-line--spacer) wal-line-icons--icon))

(defun wal-line-icons--advise-project (str)
  "Advise project segment to show a project icon before STR."
  (if (and (not (string-empty-p str)) (display-graphic-p))
      (concat
       str
       (propertize
        (all-the-icons-faicon "folder-open")
        'face 'wal-line-emphasis
        'display '(raise 0.0))
       (wal-line--spacer))
    str))

;; Setup/teardown:

(defun wal-line-icons--setup ()
  "Set up icons."
  (advice-add
   #'wal-line-project--segment
   :filter-return #'wal-line-icons--advise-project)
  (add-hook 'find-file-hook #'wal-line-icons--set-icon)
  (add-hook 'after-change-major-mode-hook #'wal-line-icons--set-icon)
  (add-hook 'clone-indirect-buffer-hook #'wal-line-icons--set-icon))

(defun wal-line-icons--teardown()
  "Tear down icons."
  (advice-remove
   #'wal-line-project--segment
   #'wal-line-icons--advise-project)
  (remove-hook 'find-file-hook #'wal-line-icons--set-icon)
  (remove-hook 'after-change-major-mode-hook #'wal-line-icons--set-icon)
  (remove-hook 'clone-indirect-buffer-hook #'wal-line-icons--set-icon))

(defvar wal-line--segments)
(wal-line-add-segment icons)

(add-hook 'wal-line-setup-hook #'wal-line-icons--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-icons--teardown)

(provide 'wal-line-icons)

;;; wal-line-icons.el ends here
