;;; wal-line-tab-bar.el --- Tab segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Show tab bar name if it was set explicitly.

;;; Code:


(require 'wal-line)


(defun wal-tab-bar--get-explicit-name ()
  "Get the name of the tab if it was set explicitly."
  (when-let* ((tab (tab-bar--current-tab))
              ((alist-get 'explicit-name tab))
              (name (alist-get 'name tab)))

    (propertize (concat " " name " ") 'face 'wal-line-highlight)))

(wal-line-create-static-segment tab-bar
  :verify
  (lambda () (featurep 'tab-bar))

  :getter
  (wal-tab-bar--get-explicit-name)

  :hooks
  (window-configuration-change-hook)

  :priority
  'current-low)

(provide 'wal-line-tab-bar)

;;; wal-line-tab-bar.el ends here
