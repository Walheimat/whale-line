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
(declare-function wal-line-vc--face-for-state "wal-line-vc.el")
(declare-function wal-line-vc--segment "wal-line-vc.el")

;;;; Functionality:

(defvar-local wal-line-icons--icon nil)
(defun wal-line-icons--set-icon (&rest _)
  "Update file icon in mode-line."
  (setq-local wal-line-icons--icon
              (if (display-graphic-p)
                  (let ((icon (all-the-icons-icon-for-buffer)))
                    (propertize (if (or (null icon) (symbolp icon))
                                    (all-the-icons-faicon
                                     "question-circle"
                                     :face 'wal-line-contrast)
                                  icon)
                                'help-echo (format "%s" (format-mode-line mode-name))
                                'display '(raise -0.135)))
                mode-name)))

(defun wal-line-icons--segment ()
  "Display the file icon for the visited file."
  (unless wal-line-icons--icon
    (wal-line-icons--set-icon))
  wal-line-icons--icon)

(defun wal-line-icons--advise-project (str)
  "Advise project segment to show a project icon before STR."
  (if (and (not (string-empty-p (string-trim str))) (display-graphic-p))
      (concat
       (wal-line--spacer)
       (all-the-icons-faicon
        "folder-open"
        :face 'wal-line-emphasis
        :height 0.85
        :v-adjust 0.0)
       str)
    str))

(defun wal-line-icons--advise-vc (str)
  "Advise version control segment to show an icon before STR."
  (if (and (not (string-empty-p (string-trim str)))
           (buffer-file-name)
           (display-graphic-p))
      (concat
       (wal-line--spacer)
       (all-the-icons-faicon "code-fork"
                             :face (wal-line-vc--face-for-state)
                             :height 0.85
                             :v-adjust 0.0)
       str)
    str))

;; Setup/teardown:

(defun wal-line-icons--setup ()
  "Set up icons."
  (advice-add
   #'wal-line-project--segment
   :filter-return #'wal-line-icons--advise-project)
  (advice-add
   #'wal-line-vc--segment
   :filter-return #'wal-line-icons--advise-vc)
  (add-hook 'find-file-hook #'wal-line-icons--set-icon)
  (add-hook 'after-change-major-mode-hook #'wal-line-icons--set-icon)
  (add-hook 'clone-indirect-buffer-hook #'wal-line-icons--set-icon))

(defun wal-line-icons--teardown()
  "Tear down icons."
  (advice-remove
   #'wal-line-project--segment
   #'wal-line-icons--advise-project)
  (advice-remove
   #'wal-line-vc--segment
   #'wal-line-icons--advise-vc)
  (remove-hook 'find-file-hook #'wal-line-icons--set-icon)
  (remove-hook 'after-change-major-mode-hook #'wal-line-icons--set-icon)
  (remove-hook 'clone-indirect-buffer-hook #'wal-line-icons--set-icon))

(defvar wal-line--segments)
(wal-line-add-segment icons)

(add-hook 'wal-line-setup-hook #'wal-line-icons--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-icons--teardown)

(provide 'wal-line-icons)

;;; wal-line-icons.el ends here
