;;; whale-line-edit-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-edit)

(ert-deftest whale-line-edit--edit ()
  :tags '(editing)

  (let* ((whale-line-segments '(a | b))
         (buffer (whale-line-edit--edit)))

    (with-current-buffer buffer
      (should (string= "a\n\nb" (buffer-string))))))

(ert-deftest whale-line-edit--apply ()
  :tags '(editing)

  (bydi ((:mock whale-line-edit--validate :return (list "Test error")))
    (should-error (whale-line-edit--apply) :type 'user-error))

  (let* ((whale-line-segments '(a | b))
         (buffer (whale-line-edit--edit)))

    (bydi ((:ignore whale-line-edit--validate)
           (:watch whale-line-edit--initial)
           (:watch whale-line-segments)
           whale-line--build-segments)

      (with-current-buffer buffer
        (erase-buffer)
        (insert "b\n\na"))

      (whale-line-edit--apply)

      (bydi-was-set-to whale-line-edit--initial '(a | b))
      (bydi-was-set-to whale-line-segments '(b | a))
      (bydi-was-called whale-line--build-segments))))

(ert-deftest whale-line-edit--abort ()
  :tags '(editing)

  (let ((buffer (whale-line-edit--edit)))

    (whale-line-edit--abort)

    (should-not (buffer-live-p buffer))))

(ert-deftest whale-line-edit--persist ()
  :tags '(editing)

  (bydi (whale-line-edit--apply
         customize-save-variable)

    (let ((whale-line-segments '(a | b)))
      (whale-line-edit--persist)

      (bydi-was-called whale-line-edit--apply)
      (bydi-was-called-with customize-save-variable '(whale-line-segments (a | b))))))

(ert-deftest whale-line-edit--revert ()
  :tags '(editing)

  (let ((whale-line-edit--initial nil)
        (whale-line-segments '(a | b)))

    (should-error (whale-line-edit--revert))

    (setq whale-line-edit--initial '(b | a))

    (bydi ((:watch whale-line-segments)
           (:watch whale-line-edit--initial)
           whale-line-edit--abort
           whale-line--build-segments)

      (whale-line-edit--revert)

      (bydi-was-set-to whale-line-segments '(b | a))
      (bydi-was-set-to whale-line-edit--inital nil)

      (bydi-was-called whale-line-edit--abort)
      (bydi-was-called whale-line--build-segments))))

(ert-deftest whale-line-edit--validate ()
  :tags '(editing)

  (let* ((whale-line-segments '(a | b))
         (whale-line--props '((a . (:type stateful))
                             (b . (:type stateless))))
         (buffer (whale-line-edit--edit)))

    (should-not (whale-line-edit--validate))))

(ert-deftest whale-line-edit--validate--invalid-segments ()
  :tags '(editing)

  (let* ((whale-line-segments '(a | b c))
         (whale-line--props '((a . (:type stateful))
                              (b . (:type stateless))
                              (c . (:type augment))))
         (buffer (whale-line-edit--edit)))

    (should (whale-line-edit--validate))))

(ert-deftest whale-line-edit--validate--empty-lines ()
  :tags '(editing)

  (let* ((whale-line-segments '(a | b | c))
         (whale-line--props '((a . (:type stateful))
                              (b . (:type stateless))
                              (c . (:type stateful))))
         (buffer (whale-line-edit--edit)))

    (should (whale-line-edit--validate))))

(ert-deftest whale-line-edit ()
  :tags '(user-facing editing)

  (bydi (whale-line-edit--edit)

    (whale-line-edit)
    (bydi-was-called whale-line-edit--edit)))

;;; whale-line-edit-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
