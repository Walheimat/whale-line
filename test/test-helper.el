;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'dinghy-rope)

;; Setup

(setq whale-line--testing t)

(defun propertized-string= (str arg)
  "Check if soon propertized ARG matches STR."
  (cond ((stringp arg) (string= arg str))
        ((and (listp arg)
              (eq :propertize (nth 0 (car arg))))

         (string= (nth 1 (car arg)) str))))

(dinghy-rope-setup-paths)
(dinghy-rope-setup-undercover (list "*.el"))
(dinghy-rope-setup-ert-runner)
(dinghy-rope-setup-ert :increase-print-depth t)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
