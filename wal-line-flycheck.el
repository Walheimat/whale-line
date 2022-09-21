;;; wal-line-flycheck.el --- Indicate issues with flycheck -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Flycheck integration.

;;; Code:

(declare-function flycheck-count-errors "ext:flycheck.el")
(declare-function wal-line-buffer-name--get-segment "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")

;;;; Functionality:

(defface wal-line-flycheck-running
  '((t (:underline (:style wave)
        :inherit (shadow)
        )))
  "Face used to indicate running state."
  :group 'wal-line)

(defvar flycheck-current-errors)
(defun wal-line-flycheck--get-face-for-status (status)
  "Get the face to use for STATUS."
  (pcase status
    ('running 'wal-line-flycheck-running)
    ('finished
     (if flycheck-current-errors
         (let-alist (flycheck-count-errors flycheck-current-errors)
           (cond
            (.error 'flycheck-error)
            (.warning 'flycheck-warning)
            (.info 'flycheck-info)))
       'wal-line-neutral))
    (_ 'wal-line-neutral)))

(defun wal-line-flycheck--underline-buffer-name (status)
  "Underline the buffer name depending on STATUS."
  (let ((face (wal-line-flycheck--get-face-for-status status))
        (segment (wal-line-buffer-name--get-segment)))
    (setq-local wal-line-buffer-name--segment (concat (wal-line--spacer) (propertize segment 'face face)))))

(defun wal-line-flycheck--setup ()
  "Set up flycheck integration."
  (add-hook 'flycheck-status-changed-functions #'wal-line-flycheck--underline-buffer-name))

(defun wal-line-flycheck--teardown ()
  "Tear down flycheck integration."
  (remove-hook 'flycheck-status-changed-functions #'wal-line-flycheck--underline-buffer-name))

(add-hook 'wal-line-setup-hook #'wal-line-flycheck--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-flycheck--teardown)

(provide 'wal-line-flycheck)

;;; wal-line-flycheck.el ends here
