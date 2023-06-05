;;; whale-line-org-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-org)

(ert-deftest wlo--maybe-truncate--truncates ()
  (let ((whale-line-org-max-heading-length 5))

    (should (string= "test…" (whale-line-org--maybe-truncate "testing")))))

(ert-deftest wlo--maybe-truncate--skips ()
  (let ((whale-line-org-max-heading-length 7))

    (should (string= "testing" (whale-line-org--maybe-truncate "testing")))))

(ert-deftest wlo--get-next-heading ()
  (let ((org-level-faces '(red green blue orange)))

    (bydi-with-mock (org-back-to-heading
                     (org-heading-components . (lambda () '(2 2 nil nil "Test Suite" nil)))
                     (org-display-format . #'bydi-rf))

      (should (string= (whale-line-org--get-next-heading) "Test Suite"))
      (bydi-was-called org-back-to-heading))))

(ert-deftest wlo--collect-headings ()
  (let ((headings '(a b c)))

    (bydi-with-mock ((org-up-heading-safe . (lambda () (pop headings)))
                     (whale-line-org--get-next-heading . (lambda () "Heading")))

      (should (equal '("Heading" "Heading" "Heading" "Heading") (whale-line-org--collect-headings))))))


(ert-deftest wlo--build-segment ()
  (defmacro org-with-wide-buffer (&rest body)
    "Mock implementation that just expands BODY."
    `(progn
       ,@body))

  (let ((first-heading t)
        (headings nil)
        (whale-line-org-include 'current-and-root))

    (bydi-with-mock ((org-before-first-heading-p . (lambda () first-heading))
                     (whale-line-org--collect-headings . (lambda () headings))
                     (whale-line-org--maybe-truncate . #'bydi-rf))
      (with-temp-buffer
        (should-not (whale-line-org--build-segment)))

      (setq first-heading nil)
      (with-temp-buffer
        (should (string= "" (whale-line-org--build-segment))))

      (setq headings (list (propertize "One" 'face 'shadow)
                           (propertize "Two" 'face 'shadow)))

      (with-temp-buffer
        (should (string= "Two One" (whale-line-org--build-segment))))

      (setq whale-line-org-include 'current)

      (with-temp-buffer
        (should (string= "One" (whale-line-org--build-segment))))

      (setq whale-line-org-include 'current-and-root
            headings (list (propertize "One" 'face 'shadow)))

      (with-temp-buffer
        (should (string= "One" (whale-line-org--build-segment)))))))

;;; whale-line-org-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
