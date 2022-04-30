;;; wal-line-mc.el --- Mulitple cursors. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Integrate `multiple-cursors' segment.

;;; Code:

(require 'wal-line-utils)

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
(wal-line-add-segment mc)

(provide 'wal-line-mc)

;;; wal-line-mc.el ends here
