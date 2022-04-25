;;; wal-line-flycheck.el --- Flycheck integration. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1")(flycheck "33-cvs"))

;;; Commentary:

;; Flycheck integration.

;;; Code:

(require 'flycheck)

(defvar-local wal-line-flycheck--face nil)

(declare-function wal-line--segment-buffer-name "wal-line.el")

;;;; Functionality:

(defun wal-line--update-flycheck (&optional status)
  "Update flycheck segment dependent on STATUS."
  (setq-local wal-line-flycheck--face
        (pcase status
          ('finished
           (if flycheck-current-errors
               (let-alist (flycheck-count-errors flycheck-current-errors)
                 (cond
                  (.error 'flycheck-error)
                  (.warning 'flycheck-warning)
                  (.info 'flycheck-info)))
             nil))
          (_ nil))))


(defun wal-line-flycheck--advise-buffer-name (str)
  "Advise the buffer name STR."
  (propertize str 'face (if wal-line-flycheck--face
                            wal-line-flycheck--face
                          'wal-line-neutral)))

(defun wal-line-flycheck--setup ()
  "Set up flycheck integration."
  (add-hook 'flycheck-status-changed-functions #'wal-line--update-flycheck)
  (advice-add
   #'wal-line--segment-buffer-name
   :filter-return #'wal-line-flycheck--advise-buffer-name))

(defun wal-line-flycheck--teardown ()
  "Tear down flycheck integration."
  (remove-hook 'flycheck-status-changed-functions #'wal-line--update-flycheck)
  (advice-remove
   #'wal-line--segment-buffer-name
   #'wal-line-flycheck--advise-buffer-name))

(add-hook 'wal-line-setup-hook #'wal-line-flycheck--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-flycheck--teardown)

(provide 'wal-line-flycheck)

;;; wal-line-flycheck.el ends here
