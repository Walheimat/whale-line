;;; whale-line-cursors.el --- Indicate multiple cursors -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.5.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Integrate segment for `multiple-cursors' and `iedit-mode'.

;;; Code:

(require 'whale-line)

(declare-function mc/num-cursors "ext:multiple-cursors.el")
(declare-function iedit-counter "ext:iedit.el")

(defun whale-line-cursors--count ()
  "Get the cursor count."
  (let* ((mc-cursors (when (bound-and-true-p multiple-cursors-mode)
                       (mc/num-cursors)))
         (iedit-cursors (when (bound-and-true-p iedit-mode)
                          (iedit-counter)))
         (cursors (or mc-cursors iedit-cursors)))

    (when cursors
      (propertize (format " %d " cursors) 'face 'whale-line-highlight))))

(whale-line-create-dynamic-segment cursors
  :getter whale-line-cursors--count

  :condition
  (or (bound-and-true-p multiple-cursors-mode)
      (bound-and-true-p iedit-mode))

  :priority current)

(provide 'whale-line-cursors)

;;; whale-line-cursors.el ends here
