;;; whale-line-animation.el --- Animation segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; A simple animation, a swimming whale by default.

;;; Code:

(require 'whale-line)

(declare-function whale-line-buffer-name--segment "whale-line.el")
(declare-function whale-line--spacer "whale-line.el")
(declare-function whale-line--is-current-window-p "whale-line.el")

;;; -- Customization

(defcustom wla-key-frames ["(__.- >{"
                           "(__.' >{"
                           "(__.- >{"
                           "(__., >{"]
  "Animation key frames."
  :group 'whale-line
  :type '(vector string))

(defcustom wla-speed 0.4
  "Animation speed."
  :group 'whale-line
  :type 'float)

;;; -- Functionality

(defvar wla--frame-index 0)
(defvar wla--timer nil)
(defvar wla--segment)

(defun wla--animate ()
  "Animate.

Forces a mode-line update and returns the current frame."
  (let* ((frame (aref wla-key-frames wla--frame-index))
         (colored (propertize frame 'face 'whale-line-emphasis)))
    (setq wla--frame-index
          (mod
           (1+ wla--frame-index)
           (length wla-key-frames))
          wla--segment (concat (whale-line--spacer) colored))
    (force-mode-line-update)
    wla--segment))

(defun wla--start-timer ()
  "Set up the animation timer."
  (unless wla--timer
    (setq wla--timer (run-with-timer 0 wla-speed #'wla--get-segment))))

(defun wla--stop-timer ()
  "Stop the animation timer."
  (when wla--timer
    (cancel-timer wla--timer)
    (setq wla--timer nil)))

(whale-line-create-static-segment animation
  :getter wla--animate

  :setup wla--start-timer

  :teardown wla--stop-timer

  :priority current-low)

(provide 'whale-line-animation)

;;; whale-line-animation.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wla-" . "whale-line-animation-"))
;; End:
