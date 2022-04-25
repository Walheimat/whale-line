;;; wal-line-project.el --- Project integration. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Show project information using `project' or `projectile'.

;;; Code:

(require 'projectile nil t)

(eval-when-compile
  (require 'wal-line-utils (expand-file-name "wal-line-utils.el")))

(declare-function wal-line--spacer "wal-line-utils.el")
(declare-function project-root "ext:project.el")

;;;; Customization:

(defcustom wal-line-project-provider
  (if (featurep 'projectile)
      'projectile
    'project)
  "The project provider."
  :group 'wal-line
  :type 'symbol)

;;;; Functionality:

(defvar wal-line--root-regexp ".+\\(\\/.+\\)\\/$")

(defun wal-line--segment-project ()
  "Get the project or root segment."
  (let* ((p-root (cond
                 ((eq wal-line-project-provider 'projectile)
                  (projectile-project-root))
                 ((eq wal-line-project-provider 'project)
                  (project-root (project-current)))
                 (t nil)))
         (f-root (or p-root default-directory)))
    (if (and (buffer-file-name) (string-match-p wal-line--root-regexp f-root))
        (progn
          (string-match wal-line--root-regexp f-root)
          (concat
           (wal-line--spacer)
           (propertize (substring (match-string 1 f-root) 1) 'face 'wal-line-emphasis)
           (wal-line--spacer)))
      "")))

(defvar wal-line--left-side)
(wal-line-add-segment
 wal-line--segment-project
 'left
 'wal-line--segment-buffer-status)

(provide 'wal-line-project)

;;; wal-line-project.el ends here
