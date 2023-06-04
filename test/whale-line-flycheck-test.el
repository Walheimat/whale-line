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

    (bydi-with-mock ((flycheck-count-errors . (lambda (_) mock-errors)))

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

    (bydi-with-mock ((flycheck-count-errors . 'bydi-rf))
      (should (string= "Errors: 1, warnings: 2, infos: 3" (whale-line-flycheck--get-error-help 'finished))))))

;;; whale-line-flycheck-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
