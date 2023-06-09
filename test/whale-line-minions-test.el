;;; whale-line-minions-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-minions)

(ert-deftest whale-line-minions--list ()
  (defvar minions-mode)
  (defvar minions-mode-line-lighter)
  (defvar minions-mode-line-minor-modes-map)

  (let ((minions-mode nil)
        (minor-mode-alist '(test rest))
        (minions-mode-line-lighter " min")
        (minions-mode-line-minor-modes-map nil))

    (should (equal '(test rest) (whale-line-minions--list)))

    (bydi ((:mock minions--prominent-modes :with (lambda () '((prominent " prm")))))
      (with-temp-buffer
        (setq-local minions-mode t)

        (should (equal '((:propertize ("" ((prominent " prm")))
                                      face whale-line-shadow)
                         " "
                         (:propertize " min"
                                      face whale-line-shadow
                                      local-map nil
                                      mouse-face whale-line-highlight))
                       (whale-line-minions--list)))))))

;;; whale-line-minions-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
