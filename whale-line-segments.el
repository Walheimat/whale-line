;;; whale-line-segments.el --- Default segments. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Definitions of the default mode-line segments.

;;; Code:

;; Segments:

(require 'whale-line)

(defvar whale-line-margin--segment (whale-line--spacer))

(whale-line-create-static-segment buffer-name
  :getter
  (let* ((identification (car-safe mode-line-buffer-identification))
         (help (get-text-property 0 'help-echo identification))
         (map (get-text-property 0 'local-map identification)))

    (propertize "%b" 'help-echo help 'mouse-face 'whale-line-highlight 'local-map map))

  :hooks
  (find-file-hook
   after-save-hook
   clone-indirect-buffer-hook)

  :advice
  (:after . (not-modified rename-buffer set-visited-file-name pop-to-buffer undo)))

(whale-line-create-dynamic-segment buffer-status
  :getter
  (cond
   (buffer-read-only
    (propertize "@" 'face 'whale-line-contrast))
   ((not (buffer-file-name))
    (propertize "&" 'face 'whale-line-shadow))
   ((buffer-modified-p)
    (propertize "*" 'face 'whale-line-emphasis))
   (t ""))

  :dense t)

(whale-line-create-dynamic-segment window-status
  :getter
  (when (window-dedicated-p)
    (propertize "^" 'face 'whale-line-shadow))

  :priority low)

(whale-line-create-dynamic-segment position
  :getter
  (let* ((following (bound-and-true-p follow-mode))
         (str (if following "f: %l:%c %p%" "%l:%c %p%")))
    (propertize str 'face 'whale-line-shadow))

  :priority current)

(whale-line-create-dynamic-segment global-mode-string
  :getter
  (cons (whale-line--spacer) (cdr global-mode-string))

  :dense t

  :priority current-low)

(whale-line-create-dynamic-segment minor-modes
  :getter
  (lambda () minor-mode-alist)

  :dense t

  :priority low)

(whale-line-create-dynamic-segment process
  :getter
  (let ((mlp mode-line-process))
    (cond
     ((listp mlp)
      (cons (whale-line--spacer) (cdr mlp)))
     ((stringp mlp)
      (propertize (concat (whale-line--spacer) mlp) 'face 'whale-line-shadow))
     (t "")))

  :condition mode-line-process

  :dense t

  :priority current)

(defun whale-line-selection--get-columns (beg end)
  "Get the columns from BEG to END for displaying `rectangle-mode'."
  (abs (- (save-excursion (goto-char end)
                          (current-column))
          (save-excursion (goto-char beg)
                          (current-column)))))

(whale-line-create-dynamic-segment selection
  :condition mark-active

  :priority current-low

  :getter
  (let* ((beg (region-beginning))
         (end (region-end))
         (lines (count-lines beg (min end (point-max)))))
    (propertize (if (bound-and-true-p rectangle-mark-mode)
                    (let ((columns (whale-line-selection--get-columns beg end)))
                      (format " %dx%d " lines columns))
                  (format " %d " lines))
                'face 'region)))

(provide 'whale-line-segments)

;;; whale-line-segments.el ends here
