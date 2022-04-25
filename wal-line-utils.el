;;; wal-line-utils.el --- Utility functions. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Utility functions.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defun wal-line--spacer ()
  "A space used for padding."
  " ")

(defun wal-line--pad (str pos)
  "Pad string STR based on its position POS."
  (pcase pos
    ('left (concat str (wal-line--spacer)))
    ('right (concat (wal-line--spacer) str))
    (_ str)))

(defun wal-line--format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display `((space :align-to (- right (- 0 right-margin) ,reserve))))
            right)))

(defun wal-line--concat (left right)
  "Concatenate LEFT and RIGHT with a divider."
  (concat left (wal-line--spacer) right))

(defun wal-line--intersperse-divider (list)
  "Intersperse divider in LIST."
  (reverse
   (cl-reduce
    (lambda (list el)
      (if list
          (cl-list* el (wal-line--spacer) list)
        (list el)))
    list :initial-value nil)))

(defvar wal-line--current-window nil)

(defun wal-line--get-current-window ()
  "Get the current window but should exclude the child windows."
  (if (and (fboundp 'frame-parent) (frame-parent))
      (frame-selected-window (frame-parent))
    (frame-selected-window)))

(defun wal-line--set-selected-window (&rest _)
  "Set selected window appropriately."
  (let ((win (wal-line--get-current-window)))
    (setq wal-line--current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defun wal-line--is-current-window-p ()
  "Check if the current window is the selected window."
  (and wal-line--current-window
       (eq (wal-line--get-current-window) wal-line--current-window)))

;;;; Macros:

(defvar wal-line--segments)
(defmacro wal-line-add-segment (segment)
  "Add SEGMENT to the list of segments."
  `(let ((left? (assoc ',segment (plist-get wal-line--segments :left)))
         (right? (assoc ',segment (plist-get wal-line--segments :right))))
     (cond
      (left?
       (setcdr left? t))
      (right?
       (setcdr right? t))
      (t (user-error "Unknown segment")))))

(provide 'wal-line-utils)

;;; wal-line-utils.el ends here
