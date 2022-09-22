;;; wal-line-flycheck.el --- Indicate issues with flycheck -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Flycheck integration.

;;; Code:

(require 'wal-line)

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

(wal-line-create-augment flycheck
  :action
  (let ((face (wal-line-flycheck--get-face-for-status (car args)))
        (segment (wal-line-buffer-name--get-segment)))
    (setq-local wal-line-buffer-name--segment (concat
                                               (wal-line--spacer)
                                               (propertize segment 'face face))))
  :hooks
  (flycheck-status-changed-function))

(provide 'wal-line-flycheck)

;;; wal-line-flycheck.el ends here
