;;; whale-line-tab-bar-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-tab-bar)

(ert-deftest wltb--get-explicit-name ()
  (bydi ((:mock tab-bar--current-tab :with (lambda () '((explicit-name . t) (name . "test-tab")))))
    (should (string= (whale-line-tab-bar--get-explicit-name) " test-tab "))))

;;; whale-line-tab-bar-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
