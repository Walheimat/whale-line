;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'ert-x)
(require 'compat nil t)
(require 'bydi nil t)

(declare-function bydi-path-setup "ext:bydi.el")
(declare-function bydi-ert-runner-setup "ext:bydi.el")
(declare-function bydi-undercover-setup "ext:bydi.el")

;; Setup

(bydi-path-setup)
(bydi-undercover-setup (list "*.el"))
(bydi-ert-runner-setup)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
