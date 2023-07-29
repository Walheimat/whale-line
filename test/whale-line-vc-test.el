;;; whale-line-vc-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-vc)

(ert-deftest wlvc--update-state ()
  (bydi ((:mock whale-line-vc--get-state :with bydi-rt))
    (with-temp-buffer
      (should-not whale-line-vc--state)

      (whale-line-vc--update-state)

      (should (equal 'testing whale-line-vc--state)))))

(ert-deftest wlvc--get-state ()
  (bydi ((:mock vc-backend :with bydi-rt)
         vc-state
         (:mock file-local-name :return "/tmp/testing"))

    (whale-line-vc--get-state)

    (bydi-was-called-with vc-state (list "/tmp/testing" 'testing))))

(ert-deftest wlvc--face-for-state ()
  (with-temp-buffer
    (setq whale-line-vc--state 'needs-update)
    (should (eq (whale-line-vc--face-for-state) 'whale-line-contrast))

    (setq whale-line-vc--state 'edited)
    (should (eq (whale-line-vc--face-for-state) 'whale-line-indicate))

    (setq whale-line-vc--state 'conflict)
    (should (eq (whale-line-vc--face-for-state) 'whale-line-contrast))

    (setq whale-line-vc--state 'unknown)
    (should (eq (whale-line-vc--face-for-state) 'whale-line-neutral))))

(ert-deftest wlvc--update-info ()
  (with-temp-buffer
    (should-not whale-line-vc--info)

    (bydi ((:mock whale-line-vc--get-info :return "testing"))
      (whale-line-vc--update-info)
      (should (string= whale-line-vc--info "testing")))))

(ert-deftest wlvc--get-info--no-op-for-non-vc-files ()
  (with-temp-buffer
    (should-not (whale-line-vc--get-info))))

(ert-deftest wlvc--get-info ()
  (let ((vc-display-status t)
        (find-file-hook . nil))

    (bydi ((:mock vc-backend :return "none"))

      (ert-with-temp-file testing
        (with-current-buffer (find-file-noselect testing)
          (setq-local vc-mode " Git:feature/tests")
          (should (string= "tests" (whale-line-vc--get-info))))))))

;;; whale-line-vc-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
