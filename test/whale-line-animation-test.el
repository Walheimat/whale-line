;;; whale-line-animation-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-animation)

(ert-deftest wla--animate ()
  (bydi (force-mode-line-update)

    (should (string= " (__.- >{" (whale-line-animation--animate)))
    (bydi-was-called force-mode-line-update)

    (should (string= " (__.' >{" (whale-line-animation--animate)))))

(ert-deftest wla--start-timer ()
  (bydi (run-with-timer)
    (let ((whale-line-animation--timer nil))

      (whale-line-animation--start-timer)
      (bydi-was-called-with run-with-timer '(0 0.4 whale-line-animation--get-segment))

      (bydi-clear-mocks)

      (whale-line-animation--start-timer)
      (bydi-was-not-called run-with-timer))))

(ert-deftest wla--stop-timer ()
  (bydi cancel-timer
    (let ((whale-line-animation--timer nil))

      (whale-line-animation--stop-timer)
      (bydi-was-not-called cancel-timer)

      (bydi-clear-mocks)

      (setq whale-line-animation--timer 'timer)
      (whale-line-animation--stop-timer)
      (bydi-was-called cancel-timer)
      (should-not whale-line-animation--timer))))

;;; whale-line-animation-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
