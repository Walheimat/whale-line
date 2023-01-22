;;; wal-line-animation.el --- Animation segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; A simple animation, a swimming whale by default.

;;; Code:

(require 'wal-line)

(declare-function wal-line-buffer-name--segment "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")
(declare-function wal-line--is-current-window-p "wal-line.el")

;;;; Customization:

(defcustom wal-line-animation-key-frames ["(__.- >{"
                                          "(__.' >{"
                                          "(__.- >{"
                                          "(__., >{"]
  "Animation key frames."
  :group 'wal-line
  :type '(vector string))

(defcustom wal-line-animation-speed 0.4
  "Animation speed."
  :group 'wal-line
  :type 'float)

;;;; Functionality:

(defvar wal-line-animation--frame-index 0)
(defvar wal-line-animation--timer nil)

(wal-line-create-static-segment animation
  :getter
  (let* ((frame (aref wal-line-animation-key-frames wal-line-animation--frame-index))
         (colored (propertize frame 'face 'wal-line-emphasis)))
    (setq wal-line-animation--frame-index
          (mod
           (1+ wal-line-animation--frame-index)
           (length wal-line-animation-key-frames))
          wal-line-animation--segment (concat (wal-line--spacer) colored))
    (force-mode-line-update)
    wal-line-animation--segment)
  :setup
  (lambda ()
    (unless wal-line-animation--timer
      (setq wal-line-animation--timer (run-with-timer
                                       0
                                       wal-line-animation-speed
                                       #'wal-line-animation--get-segment))))
  :teardown
  (lambda ()
    (when wal-line-animation--timer
      (cancel-timer wal-line-animation--timer)
      (setq wal-line-animation--timer nil)))
  :priority 'current-low)

(provide 'wal-line-animation)

;;; wal-line-animation.el ends here
