;;; whale-line-icons-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-icons)

(ert-deftest wli--icon ()
  (bydi-with-mock ((all-the-icons-faicon . (lambda (&rest _) "I")))
    (let (
          (test-icon-with-face '(faicon . ("test" test)))
          (test-icon '(faicon . "test"))
          )

      (should (equal (whale-line-icons--icon test-icon-with-face :height 0.42) "I"))
      (bydi-was-called-with all-the-icons-faicon '("test" :face test :height 0.42))

      (bydi-clear-mocks)

      (should (equal (whale-line-icons--icon test-icon :height 0.42) "I"))
      (bydi-was-called-with all-the-icons-faicon '("test" :height 0.42)))))

(ert-deftest wli--prepend-icon-to-project-segment ()
  (let ((display nil))
    (bydi-with-mock ((whale-line-icons--icon . (lambda (&rest _) "I"))
                     (display-graphic-p . (lambda () display)))

      (should (string= (whale-line-icons--prepend-icon-to-project-segment "proj") "proj"))

      (setq display t)

      (should-not (whale-line-icons--prepend-icon-to-project-segment nil))

      (should (string= (whale-line-icons--prepend-icon-to-project-segment "proj") "I proj")))))

(ert-deftest wli--prepend-icon-to-vc-segment ()
  (let ((display nil)
        (name nil))
    (bydi-with-mock ((whale-line-icons--icon . (lambda (&rest _) "I"))
                     (display-graphic-p . (lambda () display))
                     (buffer-file-name . (lambda () name)))

      (should (string= (whale-line-icons--prepend-icon-to-vc-segment "vc") "vc"))

      (setq display t
            name "/tmp/test.el")

      (should-not (whale-line-icons--prepend-icon-to-vc-segment nil))

      (should (string= (whale-line-icons--prepend-icon-to-vc-segment "vc") "I vc")))))

(ert-deftest wli--advise-buffer-status-segment ()
  (let ((buffer-read-only nil)
        (name nil)
        (modified nil)
        (whale-line-icons-buffer-read-only-icon "read-only")
        (whale-line-icons-no-buffer-file-name-icon "no-file")
        (whale-line-icons-buffer-modified-icon "modified"))

    (with-temp-buffer
      (bydi-with-mock ((buffer-file-name . (lambda () name))
                       (buffer-modified-p . (lambda () modified))
                       (whale-line-icons--icon . (lambda (&rest _) "I")))

        (setq buffer-read-only t)

        (should (string= " I" (whale-line-icons--advise-buffer-status-segment)))
        (bydi-was-called-with whale-line-icons--icon '("read-only" :height 0.85 :v-adjust 0.0))

        (bydi-clear-mocks)
        (setq buffer-read-only nil)

        (should (string= " I" (whale-line-icons--advise-buffer-status-segment)))
        (bydi-was-called-with whale-line-icons--icon '("no-file" :height 0.85 :v-adjust 0.0))

        (bydi-clear-mocks)
        (setq name "/tmp/test.el"
              modified t)

        (should (string= " I" (whale-line-icons--advise-buffer-status-segment)))
        (bydi-was-called-with whale-line-icons--icon '("modified" :height 0.85 :v-adjust 0.0))
        (setq modified nil)

        (should (string= "" (whale-line-icons--advise-buffer-status-segment)))))))

(ert-deftest wli--advise-window-status-segment ()
  (let ((dedicated nil))

    (with-temp-buffer
      (bydi-with-mock ((window-dedicated-p . (lambda () dedicated))
                       (whale-line-icons--icon . (lambda (&rest _) "I")))
        (should (string= "" (whale-line-icons--advise-window-status-segment)))

        (bydi-clear-mocks)

        (setq dedicated t)

        (should (string= " I" (whale-line-icons--advise-window-status-segment)))))))

;;; whale-line-icons-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
