;;; whale-line-lsp-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-lsp)

(ert-deftest wll--active-p ()
  (let ((feature nil))
    (bydi-with-mock ((featurep . (lambda (f) (eq f feature)))
                     lsp-workspaces)

      (should-not (whale-line-lsp--active-p))

      (bydi-clear-mocks)
      (setq feature 'lsp-mode)
      (whale-line-lsp--active-p)

      (bydi-was-called lsp-workspaces)

      (bydi-clear-mocks)
      (setq feature 'eglot)

      (with-temp-buffer
        (defvar eglot--managed-mode nil)
        (setq-local eglot--managed-mode t)
        (should (whale-line-lsp--active-p))))))

;;; whale-line-lsp-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
