;;; whale-line-flycheck-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-flycheck)

(ert-deftest wlf--get-face-for-status ()
  (should (equal 'whale-line-neutral (whale-line-flycheck--get-face-for-status nil)))
  (should (equal 'whale-line-flycheck-running (whale-line-flycheck--get-face-for-status 'running)))

  (defvar flycheck-current-errors)
  (let ((flycheck-current-errors nil)
        (mock-errors nil))

    (should (equal 'whale-line-neutral (whale-line-flycheck--get-face-for-status 'finished)))

    (setq flycheck-current-errors 'errors)

    (bydi ((:mock flycheck-count-errors :return mock-errors))

      (setq mock-errors '((error . 1)))

      (should (equal 'flycheck-error (whale-line-flycheck--get-face-for-status 'finished)))

      (setq mock-errors '((warning . 1)))

      (should (equal 'flycheck-warning (whale-line-flycheck--get-face-for-status 'finished)))

      (setq mock-errors '((info . 1)))

      (should (equal 'flycheck-info (whale-line-flycheck--get-face-for-status 'finished))))))

(ert-deftest wlf--get-error-help ()
  (should (string= "Still checking" (whale-line-flycheck--get-error-help 'running)))

  (should-not (whale-line-flycheck--get-error-help nil))

  (defvar flycheck-current-errors)

  (let ((flycheck-current-errors '((error . 1) (warning . 2) (info . 3))))

    (bydi ((:mock flycheck-count-errors :with bydi-rf))
      (should (string= "Errors: 1, warnings: 2, infos: 3" (whale-line-flycheck--get-error-help 'finished))))))

(ert-deftest wlf--underline ()
  (let ((whale-line-buffer-name--segment nil)
        (segment "test")
        (text "testing"))

    (bydi ((:mock whale-line-buffer-name--get-segment :return segment)
           (:mock whale-line-flycheck--get-face-for-status :return 'success)
           (:mock whale-line-flycheck--get-error-help :return text))

      (with-temp-buffer
        (whale-line-flycheck--underline 'status)

        (should (string= (propertize " test" 'face 'success 'help-echo "testing") whale-line-buffer-name--segment))

        (setq text nil)

        (whale-line-flycheck--underline 'status)

        (should (string= (propertize " test" 'face 'success) whale-line-buffer-name--segment))))))

;;; whale-line-flycheck-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
