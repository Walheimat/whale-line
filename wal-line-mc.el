;;; wal-line-mc.el --- Indicate multiple cursors -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Integrate `multiple-cursors' segment.

;;; Code:

(require 'wal-line)

(declare-function mc/num-cursors "ext:multiple-cursors.el")

(defun wal-line-mc--segment ()
  "Show the number of multiple cursors."
  (if (and (wal-line--is-current-window-p)
           (bound-and-true-p multiple-cursors-mode))
      (let ((cursors (mc/num-cursors)))
        (concat (wal-line--spacer)
                (propertize (format " %d " cursors) 'face 'wal-line-highlight)))
    ""))

(defvar wal-line--segments)
(wal-line-add-segment 'mc)

(provide 'wal-line-mc)

;;; wal-line-mc.el ends here
