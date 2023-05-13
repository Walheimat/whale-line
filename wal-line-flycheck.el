;;; wal-line-flycheck.el --- Indicate issues with flycheck -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Flycheck integration.

;;; Code:

(require 'wal-line)

(declare-function flycheck-count-errors "ext:flycheck.el")
(declare-function wal-line-buffer-name--get-segment "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")

;;;; Functionality:

(defface wlf-running
  '((t (:underline (:style wave)
        :inherit (shadow)
        )))
  "Face used to indicate running state."
  :group 'wal-line)

(defvar flycheck-current-errors)
(defun wlf--get-face-for-status (status)
  "Get the face to use for STATUS."
  (pcase status
    ('running 'wlf-running)
    ('finished
     (if flycheck-current-errors
         (let-alist (flycheck-count-errors flycheck-current-errors)
           (cond
            (.error 'flycheck-error)
            (.warning 'flycheck-warning)
            (.info 'flycheck-info)))
       'wal-line-neutral))
    (_ 'wal-line-neutral)))

(defun wlf--get-error-help (status)
  "Get the error count for STATUS.

Returns nil if not checking or if no errors were found."
  (pcase status
    ('running "Still checking")
    ('finished
     (when flycheck-current-errors
       (let-alist (flycheck-count-errors flycheck-current-errors)
         (format "Errors: %s, warnings: %s, infos: %s" (or .error 0) (or .warning 0) (or .info 0)))))
    (_ nil)))

(wal-line-create-augment flycheck
  :verify (lambda () (require 'flycheck nil t))
  :action
  (lambda (status &rest _r)
    (let ((face (wlf--get-face-for-status status))
          (text (wlf--get-error-help status))
          (segment (wal-line-buffer-name--get-segment)))

	  (setq-local wal-line-buffer-name--segment
				  (concat
                   (wal-line--spacer)
				   (if text
					   (propertize segment 'face face 'help-echo text)
					 (propertize segment 'face face))))))
  :hooks
  (flycheck-status-changed-functions))

(provide 'wal-line-flycheck)

;;; wal-line-flycheck.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wlf-" . "wal-line-flycheck-"))
;; End:
