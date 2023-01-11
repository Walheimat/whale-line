;;; wal-line-project.el --- Project segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Show project information using `project' or `projectile'.

;;; Code:

(require 'wal-line)

(declare-function wal-line--spacer "wal-line.el")
(declare-function projectile-project-root "ext:projectile.el")
(declare-function project-name "ext:project.el")
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

(wal-line-create-static-segment project
  :getter
  (when-let* ((p-root (pcase wal-line-project-provider
                        ('projectile
                         (projectile-project-root))
                        ('project
                         (when-let ((current (project-current)))
                           (project-root current)))
                        (_ nil)))
              (p-name (pcase wal-line-project-provider
                        ('projectile
                         (string-match wal-line-project--regexp p-root)
                         (substring (match-string 1 p-root) 1))
                        ('project
                         (project-name (project-current)))
                        (_ ""))))

    (propertize p-name 'face 'wal-line-emphasis))
  :setup
  (lambda () (add-hook 'find-file-hook #'wal-line-project--set-segment))
  :teardown
  (lambda () (remove-hook 'find-file-hook #'wal-line-project--set-segment)))

(provide 'wal-line-project)

;;; wal-line-project.el ends here
