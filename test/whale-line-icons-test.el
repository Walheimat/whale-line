;;; whale-line-icons-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-icons)

(ert-deftest wli--icon ()
  (bydi ((:mock all-the-icons-faicon :return "I"))
    (let ((test-icon-with-face '(faicon . ("test" test)))
          (test-icon '(faicon . "test")))

      (should (equal (whale-line-icons--icon test-icon-with-face :height 0.42) "I"))
      (bydi-was-called-with all-the-icons-faicon '("test" :face test :height 0.42))

      (bydi-clear-mocks)

      (should (equal (whale-line-icons--icon test-icon :height 0.42) "I"))
      (bydi-was-called-with all-the-icons-faicon '("test" :height 0.42)))))

(ert-deftest wli--prepend-icon-to-project-segment ()
  (bydi ((:mock whale-line-icons--icon :return "I")
         (:sometimes display-graphic-p))

    (should-not (whale-line-icons--prepend-icon-to-project-segment nil))

    (should (string= (whale-line-icons--prepend-icon-to-project-segment "proj") "I proj"))

    (bydi-toggle-sometimes)
    (should (string= (whale-line-icons--prepend-icon-to-project-segment "proj") "proj"))))

(ert-deftest wli--prepend-icon-to-vc-segment ()
  (let ((name "/test/tmp"))

    (bydi ((:mock whale-line-icons--icon :return "I")
           (:sometimes display-graphic-p)
           (buffer-file-name . (lambda () name)))

      (should-not (whale-line-icons--prepend-icon-to-vc-segment nil))
      (should (string= (whale-line-icons--prepend-icon-to-vc-segment "vc") "I vc"))

      (bydi-toggle-sometimes)
      (setq name nil)

      (should (string= (whale-line-icons--prepend-icon-to-vc-segment "vc") "vc")))))

(ert-deftest wli--advise-buffer-status-segment ()
  (let ((buffer-read-only nil)
        (name nil)
        (modified nil)
        (whale-line-icons-buffer-read-only-icon "read-only")
        (whale-line-icons-no-buffer-file-name-icon "no-file")
        (whale-line-icons-buffer-modified-icon "modified"))

    (with-temp-buffer
      (bydi ((:mock buffer-file-name :return name)
             (:mock buffer-modified-p :return modified)
             (:mock whale-line-icons--icon :return "I"))

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
  (with-temp-buffer
    (bydi ((:sometimes window-dedicated-p)
           (:mock whale-line-icons--icon :return "I"))
      (should (string= " I" (whale-line-icons--advise-window-status-segment)))

      (bydi-toggle-sometimes)
      (should (string= "" (whale-line-icons--advise-window-status-segment))))))

;;; whale-line-icons-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
