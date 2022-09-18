;;; wal-line-project.el --- Project segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Show project information using `project' or `projectile'.

;;; Code:

(require 'projectile nil t)
(require 'wal-line)

(declare-function wal-line--spacer "wal-line.el")
(declare-function projectile-project-root "ext:projectile.el")
(declare-function project-root "ext:project.el")

;;;; Customization:

(defcustom wal-line-project-provider
  (if (bound-and-true-p projectile-mode)
      'projectile
    'project)
  "The project provider."
  :group 'wal-line
  :type '(choice (const projectile)
                 (const project)))

;;;; Functionality:

(defvar wal-line-project--regexp ".+\\(\\/.+\\)\\/$")

(defvar-local wal-line-project--segment nil)

(defun wal-line-project--set-segment ()
  "Set the segment."
  (when-let ((info (wal-line-project--get-info)))
    (setq-local wal-line-project--segment (concat (wal-line--spacer) info))))

(defun wal-line-project--get-info ()
  "Get the project or root segment."
  (let ((p-root (cond
                 ((eq wal-line-project-provider 'projectile)
                  (projectile-project-root))
                 ((eq wal-line-project-provider 'project)
                  (project-root (project-current)))
                 (t nil))))
    (when (and p-root
               (buffer-file-name)
               (string-match-p wal-line-project--regexp p-root))
      (string-match wal-line-project--regexp p-root)
      (propertize (substring (match-string 1 p-root) 1) 'face 'wal-line-emphasis))))

(defvar wal-line--segments)
(wal-line-add-segment project)

(defun wal-line-project--setup ()
  "Set up project segment."
  (add-hook 'find-file-hook #'wal-line-project--set-segment))

(defun wal-line-project--teardown ()
  "Set up project segment."
  (remove-hook 'find-file-hook #'wal-line-project--set-segment))

(add-hook 'wal-line-setup-hook #'wal-line-project--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-project--teardown)

(provide 'wal-line-project)

;;; wal-line-project.el ends here
