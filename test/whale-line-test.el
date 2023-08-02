;;; whale-line-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line)

(ert-deftest whale-line-mode--setup ()
  (let ((whale-line-features '(one two three))
        (mode-line-format 'format)
        (whale-line--default-mode-line nil))

    (bydi ((:mock require :return t)
           add-hook
           run-hooks)
      (whale-line-mode--setup)

      (bydi-was-called-with run-hooks (list 'whale-line-setup-hook))
      (bydi-was-called-with add-hook (list 'pre-redisplay-functions #'whale-line--set-selected-window))

      (eq 'format whale-line--default-mode-line))))

(ert-deftest whale-line-mode--teardown ()
  (let ((mode-line-format 'whale)
        (whale-line--default-mode-line 'other))

    (bydi (run-hooks remove-hook)
      (whale-line-mode--teardown)

      (bydi-was-called-with run-hooks (list 'whale-line-teardown-hook))
      (bydi-was-called-with remove-hook (list 'pre-redisplay-functions #'whale-line--set-selected-window))

      (should (eq 'other mode-line-format)))))

(ert-deftest whale-line-mode ()
  (bydi (whale-line-mode--setup whale-line-mode--teardown)
    (let ((whale-line-mode nil))
      (whale-line-mode)

      (bydi-was-called whale-line-mode--setup)

      (whale-line-mode -1)

      (bydi-was-called whale-line-mode--teardown))))

;;; whale-line-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
