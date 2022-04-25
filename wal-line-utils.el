;;; wal-line-utils.el --- Utility functions. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Keywords: mode-line
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Utility functions.

;;; Code:

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

(defmacro wal-line-add-segment (segment side &optional after)
  "Add SEGMENT for SIDE AFTER other segment.

Optionally, don't add a spacer if NO-SPACER is given."
  `(if-let* ((side (pcase ,side
                     ('left wal-line--left-side)
                     ('right wal-line--right-side)))
             (remainder (if ,after
                            (nthcdr (cl-position ,after side) side)
                          nil)))
       (when (not (memq ',segment side))
         (if remainder
             (setcdr remainder (cons ',segment (cdr remainder)))
           (setcar side (cons ',segment side))))
     nil))

(provide 'wal-line-utils)

;;; wal-line-utils.el ends here
