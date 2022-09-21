;;; wal-line-whale.el --- Animated whale -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; A simple whale animation.

;;; Code:

(require 'wal-line)

(declare-function wal-line-buffer-name--segment "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")
(declare-function wal-line--is-current-window-p "wal-line.el")

;;;; Customization:

(defcustom wal-line-whale-key-frames ["(__.- >{"
                                      "(__.' >{"
                                      "(__.- >{"
                                      "(__., >{"]
  "Animation key frames."
  :group 'wal-line
  :type '(vector string))

(defcustom wal-line-whale-animation-speed 0.4
  "Animation speed."
  :group 'wal-line
  :type 'float)

;;;; Functionality:

(defvar wal-line-whale--frame-index 0)
(defvar wal-line-whale--timer nil)

(wal-line-create-static-segment whale
  :getter
  (let* ((frame (aref wal-line-whale-key-frames wal-line-whale--frame-index))
         (colored (propertize frame 'face 'wal-line-emphasis)))
    (setq wal-line-whale--frame-index
          (mod
           (1+ wal-line-whale--frame-index)
           (length wal-line-whale-key-frames))
          wal-line-whale--segment (concat (wal-line--spacer) colored))
    (force-mode-line-update)
    wal-line-whale--segment)
  :setup
  (lambda ()
    (unless wal-line-whale--timer
      (setq wal-line-whale--timer (run-with-timer
                                   0
                                   wal-line-whale-animation-speed
                                   #'wal-line-whale--get-segment))))
  :teardown
  (lambda ()
    (when wal-line-whale--timer
      (cancel-timer wal-line-whale--timer)
      (setq wal-line-whale--timer nil)))
  :priority 'current)

(provide 'wal-line-whale)

;;; wal-line-whale.el ends here
