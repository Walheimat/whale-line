;;; whale-line-cursors-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-cursors)

(ert-deftest whale-line-cursors--count ()
  (defvar mulitple-cursors-mode)
  (defvar iedit-mode)

  (let ((mulitple-cursors-mode nil)
        (iedit-mode nil))

    (should-not (whale-line-cursors--count))

    (bydi-with-mock ((mc/num-cursors . (lambda () 2)))

      (with-temp-buffer
        (setq-local multiple-cursors-mode t)

        (should (string= " 2 " (whale-line-cursors--count)))))

    (bydi-with-mock ((iedit-counter . (lambda () 3)))

      (with-temp-buffer
        (setq-local iedit-mode t)

        (should (string= " 3 " (whale-line-cursors--count)))))))

;;; whale-line-cursors-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
