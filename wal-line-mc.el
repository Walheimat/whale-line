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

(wal-line-create-dynamic-segment mc
  :getter
  (let ((cursors (mc/num-cursors)))
    (propertize (format " %d " cursors) 'face 'wal-line-highlight))
  :condition
  (bound-and-true-p multiple-cursors-mode)
  :priority 'current)

(provide 'wal-line-mc)

;;; wal-line-mc.el ends here
