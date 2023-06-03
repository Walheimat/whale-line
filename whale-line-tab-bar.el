;;; whale-line-tab-bar.el --- Tab segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Show tab bar name if it was set explicitly.

;;; Code:


(require 'whale-line)

(defun wltb--get-explicit-name ()
  "Get the name of the tab if it was set explicitly."
  (when-let* ((tab (tab-bar--current-tab))
              ((alist-get 'explicit-name tab))
              (name (alist-get 'name tab)))

    (propertize (concat " " name " ") 'face 'whale-line-highlight)))

(whale-line-create-static-segment tab-bar
  :verify
  (lambda () (featurep 'tab-bar))

  :getter
  (wltb--get-explicit-name)

  :hooks
  (window-configuration-change-hook)

  :priority
  'current-low)

(provide 'whale-line-tab-bar)

;;; whale-line-tab-bar.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wltb-" . "whale-line-tab-bar-"))
;; End:
