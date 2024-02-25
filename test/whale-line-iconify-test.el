;;; whale-line-iconify-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-iconify)

(defvar rectangle (ert-resource-file "rectangle.txt"))

(defvar test-specs '((test . (:name "test" :font test :face test-face))
                     (no-def . (:name "test" :font test :face test-face :no-defaults t))
                     (merge . (:name "test" :font test :face test-face :height 0.5))
                     (fun . (:function specs-fun))
                     (args . (:function specs-fun :pass-args t :height 0.5))))

(defun specs-fun (&rest _args)
  "Just a specs fun."
  nil)

(ert-deftest from-specs ()
  (let ((whale-line-iconify-specs test-specs)
        (whale-line-iconify--default-specs '(:height 0.8 :v-adjust -1.1)))

    (bydi (all-the-icons-test
           specs-fun)

      (whale-line-iconify--from-specs (cdr-safe (assoc 'test whale-line-iconify-specs)))
      (bydi-was-called-with all-the-icons-test '("test" :face test-face :height 0.8 :v-adjust -1.1))

      (bydi-clear-mocks)
      (whale-line-iconify--from-specs (cdr-safe (assoc 'no-def whale-line-iconify-specs)))
      (bydi-was-called-with all-the-icons-test '("test" :face test-face))

      (bydi-clear-mocks)
      (whale-line-iconify--from-specs (cdr-safe (assoc 'merge whale-line-iconify-specs)))
      (bydi-was-called-with all-the-icons-test '("test" :face test-face :height 0.5 :v-adjust -1.1))

      (whale-line-iconify--from-specs (cdr-safe (assoc 'fun whale-line-iconify-specs)))

      (bydi-was-called specs-fun t)

      (whale-line-iconify--from-specs (cdr-safe (assoc 'args whale-line-iconify-specs)))

      (bydi-was-called-with specs-fun '(:height 0.5)))))

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
  (let ((feature t)
        (display t))

    (bydi ((:mock display-graphic-p :return display)
           (:mock featurep :return feature))

      (should (whale-line-iconify--can-use-p))

      (setq display nil)

      (should-not (whale-line-iconify--can-use-p))

      (setq display t
            feature nil)

      (should-not (whale-line-iconify--can-use-p)))))

(ert-deftest iconify ()
  (let ((whale-line-iconify-specs test-specs))

    (bydi (whale-line-iconify--from-specs
           (:sometimes whale-line-iconify--use-for-p))

      (whale-line-iconify 'test)

      (bydi-was-called-with whale-line-iconify--from-specs
        '((:name "test" :font test :face test-face)))

      (bydi-clear-mocks)
      (whale-line-iconify 'test :face 'other-face)

      (bydi-was-called-with whale-line-iconify--from-specs
        '((:name "test" :font test :face other-face)))

      (bydi-toggle-sometimes)

      (should-not (whale-line-iconify 'test))

      (bydi-was-not-called whale-line-iconify--from-specs))))

(ert-deftest iconify-decorates-p ()
  :tags '(iconify)

  (bydi ((:sometimes whale-line-iconify--use-for-p))

    (should (whale-line-iconify-decorates-p 'test))

    (bydi-toggle-sometimes)

    (should-not (whale-line-iconify-decorates-p 'test))))

(ert-deftest whale-line-iconify-mode ()
  :tags '(iconify)

  (let ((whale-line-iconify-mode nil))

    (bydi ((:spy advice-add)
           (:spy advice-remove))

      (whale-line-iconify-mode)

      (bydi-was-called-last-with advice-add '(whale-line-segments--decorates-p :override whale-line-iconify-decorates-p))

      (whale-line-iconify-mode -1)
      (bydi-was-called-last-with advice-remove '(whale-line-segments--decorates-p  whale-line-iconify-decorates-p)))))

;;; whale-line-iconify-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
