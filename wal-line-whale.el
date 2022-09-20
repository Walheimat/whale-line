;;; wal-line-whale.el --- Animated whale -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
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

(defcustom wal-line-whale-key-frames
  [
   "(__.- >{"
   "(__.' >{"
   "(__.- >{"
   "(__., >{"
   ]
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
(defvar wal-line-whale--frame nil)

(defun wal-line-whale--animate ()
  "Animate the ASCII whale."
  (let* ((frame (aref wal-line-whale-key-frames wal-line-whale--frame-index))
         (colored (propertize frame 'face 'wal-line-emphasis)))
    (setq wal-line-whale--frame-index
          (mod
           (1+ wal-line-whale--frame-index)
           (length wal-line-whale-key-frames))
          wal-line-whale--frame colored)
    (force-mode-line-update)))

(defun wal-line-whale--segment ()
  "Show the current animation frame."
  (unless wal-line-whale--frame
    (wal-line-whale--animate))
  (if (wal-line--is-current-window-p)
      (concat (wal-line--spacer) wal-line-whale--frame)
    ""))

;;;; Entrypoint.

(defun wal-line-whale--setup ()
  "Set up the animated whale."
  (unless wal-line-whale--timer
    (setq wal-line-whale--timer (run-with-timer
                                 0
                                 wal-line-whale-animation-speed
                                 #'wal-line-whale--animate))))

(defun wal-line-whale--teardown ()
  "Clean up the animation."
  (when wal-line-whale--timer
    (cancel-timer wal-line-whale--timer)
    (setq wal-line-whale--timer nil)))

(add-hook 'wal-line-setup-hook #'wal-line-whale--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-whale--teardown)

(defvar wal-line--segments)
(wal-line-add-segment 'whale 'low)

(provide 'wal-line-whale)

;;; wal-line-whale.el ends here
