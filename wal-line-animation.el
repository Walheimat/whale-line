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

(defcustom wla-key-frames ["(__.- >{"
                           "(__.' >{"
                           "(__.- >{"
                           "(__., >{"]
  "Animation key frames."
  :group 'wal-line
  :type '(vector string))

(defcustom wla-speed 0.4
  "Animation speed."
  :group 'wal-line
  :type 'float)

;;;; Functionality:

(defvar wla--frame-index 0)
(defvar wla--timer nil)

(wal-line-create-static-segment animation
  :getter
  (let* ((frame (aref wla-key-frames wla--frame-index))
         (colored (propertize frame 'face 'wal-line-emphasis)))
    (setq wla--frame-index
          (mod
           (1+ wla--frame-index)
           (length wla-key-frames))
          wla--segment (concat (wal-line--spacer) colored))
    (force-mode-line-update)
    wla--segment)
  :setup
  (lambda ()
    (unless wla--timer
      (setq wla--timer (run-with-timer 0 wla-speed #'wla--get-segment))))
  :teardown
  (lambda ()
    (when wla--timer
      (cancel-timer wla--timer)
      (setq wla--timer nil)))
  :priority 'current-low)

(provide 'wal-line-animation)

;;; wal-line-animation.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wla-" . "wal-line-animation-"))
;; End:
