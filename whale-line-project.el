;;; whale-line-project.el --- Project segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.5.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Show project information using `project' or `projectile'.

;;; Code:

(require 'whale-line)

(declare-function whale-line--spacer "whale-line.el")
(declare-function projectile-project-root "ext:projectile.el")
(declare-function project-name "ext:project.el")
(declare-function project-root "ext:project.el")

;;; -- Customization

(defcustom wlp-provider 'project
  "The project provider."
  :group 'whale-line
  :type '(choice (const project)
                 (const projectile)))

;;; -- Functionality

(defun wlp--display-for-buffer-p ()
  "Check if current buffer should show project information.

Only consider Dired buffers and file buffers."
  (with-current-buffer (current-buffer)
    (or (derived-mode-p 'dired-mode)
        (buffer-file-name))))

(defvar wlp--regexp ".+\\(\\/.+\\)\\/$")

(defun wlp--get ()
  "Get the project segment."
  (when-let* ((candidate (wlp--display-for-buffer-p))
              (p-root (pcase wlp-provider
                        ('projectile
                         (projectile-project-root))
                        ('project
                         (when-let ((current (project-current)))
                           (project-root current)))
                        (_ nil)))
              (p-name (pcase wlp-provider
                        ('projectile
                         (string-match wlp--regexp p-root)
                         (substring (match-string 1 p-root) 1))
                        ('project
                         (project-name (project-current)))
                        (_ ""))))

    (propertize p-name 'face 'whale-line-emphasis 'help-echo p-root)))

(whale-line-create-static-segment project
  :getter wlp--get

  :hooks
  (find-file-hook))

(provide 'whale-line-project)

;;; whale-line-project.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wlp-" . "whale-line-project-"))
;; End:
