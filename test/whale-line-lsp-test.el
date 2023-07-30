;;; whale-line-lsp-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-lsp)

(ert-deftest wll--active-p ()
  (let ((feature nil))
    (bydi ((:mock featurep :with (lambda (f) (eq f feature)))
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

(ert-deftest wll--indicate-session--icons ()
  (bydi ((:always whale-line--enabled-feature-p)
         (:mock whale-line-icons--get-segment :return (propertize "icon" 'face '(:inherit ert-test-result-expected)))
         (:sometimes whale-line-lsp--active-p))

    (with-temp-buffer
      (whale-line-lsp--indicate-session)

      (should (string= (propertize "icon" 'face 'whale-line-indicate) whale-line-icons--segment))

      (bydi-toggle-sometimes)

      (whale-line-lsp--indicate-session)
      (should (string= "icon" whale-line-icons--segment)))))

(ert-deftest wll--indicate-session--text ()
  (let ((name "test")
        (whale-line-lsp-delimiters '("-" "-"))
        (delim (propertize "-" 'face 'whale-line-indicate)))

    (bydi ((:ignore whale-line--enabled-feature-p)
           (:mock whale-line-buffer-name--get-segment :return name)
           (:sometimes whale-line-lsp--active-p)
           (:mock whale-line--spacer :return ""))

      (with-temp-buffer
        (whale-line-lsp--indicate-session)

        (should (string= (concat delim "test" delim) whale-line-buffer-name--segment))))))

;;; whale-line-lsp-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
