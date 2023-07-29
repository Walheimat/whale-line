;;; whale-line-flycheck.el --- Indicate issues with flycheck -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Flycheck integration.

;;; Code:

(require 'whale-line)

(declare-function flycheck-count-errors "ext:flycheck.el")
(declare-function whale-line-buffer-name--get-segment "whale-line.el")
(declare-function whale-line--spacer "whale-line.el")

;;; -- Functionality

(defface wlf-running
  '((t (:underline (:style wave)
        :inherit (shadow)
        )))
  "Face used to indicate running state."
  :group 'whale-line)

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
       'whale-line-neutral))
    (_ 'whale-line-neutral)))

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

(whale-line-create-augment flycheck
  :verify (lambda () (require 'flycheck nil t))

  :action
  (lambda (status &rest _r)
    (let ((face (wlf--get-face-for-status status))
          (text (wlf--get-error-help status))
          (segment (whale-line-buffer-name--get-segment)))

	  (setq-local whale-line-buffer-name--segment
				  (concat
                   (whale-line--spacer)
				   (if text
					   (propertize segment 'face face 'help-echo text)
					 (propertize segment 'face face))))))

  :hooks
  (flycheck-status-changed-functions))

(provide 'whale-line-flycheck)

;;; whale-line-flycheck.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wlf-" . "whale-line-flycheck-"))
;; End:
