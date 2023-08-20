;;; whale-line-iconify-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-iconify)

(defvar rectangle (ert-resource-file "rectangle.txt"))

(defvar test-specs '((test . (:name "test" :font test :face test-face :fallback "?"))
                     (no-def . (:name "test" :font test :face test-face :fallback "?" :no-defaults t))
                     (merge . (:name "test" :font test :face test-face :fallback "?" :height 0.5))))

(ert-deftest from-specs ()
  (let ((whale-line-iconify-specs test-specs)
        (whale-line-iconify--default-specs '(:height 0.8 :v-adjust -1.1)))

    (bydi (all-the-icons-test)

      (whale-line-iconify--from-specs (cdr-safe (assoc 'test whale-line-iconify-specs)))
      (bydi-was-called-with all-the-icons-test '("test" :face test-face :height 0.8 :v-adjust -1.1))

      (bydi-clear-mocks)
      (whale-line-iconify--from-specs (cdr-safe (assoc 'no-def whale-line-iconify-specs)))
      (bydi-was-called-with all-the-icons-test '("test" :face test-face))

      (bydi-clear-mocks)
      (whale-line-iconify--from-specs (cdr-safe (assoc 'merge whale-line-iconify-specs)))
      (bydi-was-called-with all-the-icons-test '("test" :face test-face :height 0.5 :v-adjust -1.1)))))

(ert-deftest use-icons-for-p ()
  (let ((whale-line-iconify-disabled nil))

    (bydi ((:sometimes whale-line-iconify--can-use-p))

      (should (whale-line-iconify--use-for-p 'test))

      (bydi-toggle-sometimes)

      (should-not (whale-line-iconify--use-for-p 'test))

      (bydi-toggle-sometimes)

      (setq whale-line-iconify-disabled 'all)

      (should-not (whale-line-iconify--use-for-p 'test))

      (setq whale-line-iconify-disabled '(test))

      (should-not (whale-line-iconify--use-for-p 'test)))))

(ert-deftest can-use-icons-p ()
  (bydi ((:sometimes require))

    (should (whale-line-iconify--can-use-p))

    (bydi-toggle-sometimes)

    (should-not (whale-line-iconify--can-use-p))))

(ert-deftest iconify ()
  (let ((whale-line-iconify-specs test-specs))

    (bydi (whale-line-iconify--from-specs
           (:sometimes whale-line-iconify--use-for-p))

      (whale-line-iconify 'test)

      (bydi-was-called-with whale-line-iconify--from-specs
        '((:name "test" :font test :face test-face :fallback "?")))

      (bydi-clear-mocks)
      (whale-line-iconify 'test 'other-face)

      (bydi-was-called-with whale-line-iconify--from-specs
        '((:name "test" :font test :face other-face :fallback "?")))

      (bydi-toggle-sometimes)

      (should (string= "?" (whale-line-iconify 'test)))

      (bydi-was-not-called whale-line-iconify--from-specs))))

;;; whale-line-iconify-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
