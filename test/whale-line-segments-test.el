;;; whale-line-segments-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-segments)

(ert-deftest whale-line-selection--get-columns ()
  (with-temp-buffer
    (insert-file-contents rectangle)
    (should (eq (whale-line-selection--get-columns 1 30) 9))))

;;; whale-line-segments-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
