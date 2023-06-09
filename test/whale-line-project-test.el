;;; whale-line-project-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-project)

(ert-deftest wlp--display-for-buffer-p--no-show-for-non-file-non-dired ()
  (with-temp-buffer
    (bydi ((:ignore derived-mode-p)
           (:ignore buffer-file-name))
      (should-not (whale-line-project--display-for-buffer-p)))))

(ert-deftest wlp--get--for-projectile ()
  (let ((whale-line-project-provider 'projectile))

    (bydi ((:always whale-line-project--display-for-buffer-p)
           (:mock projectile-project-root :return "/home/test/project/"))
      (should (string= "project" (whale-line-project--get))))))

(ert-deftest wlp--get--for-project ()
  (let ((whale-line-project-provider 'project))

    (bydi ((:always whale-line-project--display-for-buffer-p)
           (:always project-current)
           (:mock project-root :return "/home/test/project/")
           (:mock project-name :return "project"))
      (should (string= "project" (whale-line-project--get))))))

;;; whale-line-project-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
