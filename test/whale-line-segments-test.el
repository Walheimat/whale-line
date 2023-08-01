;;; whale-line-segments-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-segments)

(defvar rectangle (ert-resource-file "rectangle.txt"))

(ert-deftest wls--buffer-name ()
  (let ((map (make-sparse-keymap))
        (mode-line-buffer-identification nil))

    (bydi ((:mock buffer-name :return "buffer"))

      (should (string= (propertize "%b"
                                   'help-echo nil
                                   'mouse-face 'whale-line-highlight
                                   'local-map nil)
                       (whale-line-segments--buffer-name)))

      (setq mode-line-buffer-identification `(((propertize
                                                "name"
                                                'help-echo "test"
                                                'local-map map))))

      (should (string= (propertize "%b"
                                   'help-echo "test"
                                   'mouse-face 'whale-line-highlight
                                   'local-map map)
                       (whale-line-segments--buffer-name))))))

(ert-deftest wls--buffer-status ()
  (with-temp-buffer
    (read-only-mode)

    (should (string= "@" (substring-no-properties (whale-line-segments--buffer-status))))

    (read-only-mode -1)

    (should (string= "&" (substring-no-properties (whale-line-segments--buffer-status)))))

  (ert-with-temp-file buffer-status :buffer current
    (with-current-buffer current
      (should (string= "" (substring-no-properties (whale-line-segments--buffer-status))))

      (insert "test")

      (should (string= "*" (substring-no-properties (whale-line-segments--buffer-status)))))))

(ert-deftest wls--window-status ()
  (should-not (whale-line-segments--window-status))

  (set-window-dedicated-p (selected-window) t)

  (should (string= "^" (substring-no-properties (whale-line-segments--window-status))))

  (set-window-dedicated-p (selected-window) nil))

(ert-deftest wls--position ()
  (bydi ((:mock image-mode-window-get :return 1)
         (:mock doc-view-last-page-number :return 2))

    (with-temp-buffer
      (setq major-mode 'doc-view-mode)

      (should (string= "1/2" (substring-no-properties (whale-line-segments--position))))))

  (with-temp-buffer
    (should (string= "%l:%c %p%" (substring-no-properties (whale-line-segments--position))))

    (defvar follow-mode)

    (setq follow-mode t)

    (should (string= "f: %l:%c %p%" (substring-no-properties (whale-line-segments--position))))))

(ert-deftest wls--global-mode-string ()
  (let ((global-mode-string '("a" "b")))

    (should (equal '(" " "b") (whale-line-segments--global-mode-string)))))

(ert-deftest wls--process ()
  (let ((mode-line-process '("a" "b")))
    (should (equal '(" " "b") (whale-line-segments--process)))

    (setq mode-line-process "c")

    (should (string= " c" (whale-line-segments--process)))

    (setq mode-line-process 'test)

    (should (string= "" (whale-line-segments--process)))))

(ert-deftest whale-line-selection--get-columns ()
  (with-temp-buffer
    (insert-file-contents rectangle)
    (should (eq (whale-line-selection--get-columns 1 30) 9))))

(ert-deftest whale-line-segments--selection--lines ()
  (with-temp-buffer
    (insert-file-contents rectangle)
    (goto-char (point-min))
    (push-mark)
    (goto-char (point-max))

    (should (string= (propertize " 3 " 'face 'region) (whale-line-segments--selection)))))

(ert-deftest whale-line-segments--selection--rectangle ()
  (with-temp-buffer
    (insert-file-contents rectangle)
    (goto-char (point-min))
    (rectangle-mark-mode)
    (goto-char (1- (point-max)))

    (should (string= (propertize " 3x9 " 'face 'region) (whale-line-segments--selection)))))

;;; whale-line-segments-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
