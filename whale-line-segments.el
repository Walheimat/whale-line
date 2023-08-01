;;; whale-line-segments.el --- Default segments. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Definitions of the default mode-line segments.

;;; Code:

;;; -- Segments

(require 'whale-line)

(declare-function image-mode-window-get "ext:image-mode.el")
(declare-function doc-view-last-page-number "ext:doc-view.el")

(defvar whale-line-margin--segment (whale-line--spacer))

(defun wls--buffer-name ()
  "Get the buffer name."
  (let* ((identification (whale-line--car-safe-until
                          mode-line-buffer-identification
                          #'stringp
                          (buffer-name)))
         (help (get-text-property 0 'help-echo identification))
         (map (get-text-property 0 'local-map identification)))

    (propertize "%b" 'help-echo help 'mouse-face 'whale-line-highlight 'local-map map)))

(whale-line-create-static-segment buffer-name
  :getter wls--buffer-name
  :hooks (find-file-hook after-save-hook clone-indirect-buffer-hook)
  :advice (:after . (not-modified rename-buffer set-visited-file-name pop-to-buffer undo)))

(defun wls--buffer-status ()
  "Render buffer status segment."
  (cond
   (buffer-read-only
    (propertize "@" 'face 'whale-line-contrast))
   ((not (buffer-file-name))
    (propertize "&" 'face 'whale-line-shadow))
   ((buffer-modified-p)
    (propertize "*" 'face 'whale-line-emphasis))
   (t "")))

(whale-line-create-dynamic-segment buffer-status
  :getter wls--buffer-status
  :dense t)

(defun wls--window-status ()
  "Render window status segment."
  (when (window-dedicated-p)
    (propertize "^" 'face 'whale-line-shadow)))

(whale-line-create-dynamic-segment window-status
  :getter wls--window-status
  :priority low)

(defun wls--position ()
  "Render position segment."
  (let ((str (cond
              ((eq major-mode 'doc-view-mode)
               (concat
                (number-to-string (image-mode-window-get 'page))
                "/"
                (number-to-string (doc-view-last-page-number))))

              ((bound-and-true-p follow-mode)
               "f: %l:%c %p%")

              (t "%l:%c %p%"))))
    (propertize str 'face 'whale-line-shadow)))

(whale-line-create-dynamic-segment position
  :getter wls--position
  :priority current)

(defun wls--global-mode-string ()
  "Render `global-mode-string' segment."
  (cons (whale-line--spacer) (cdr global-mode-string)))

(whale-line-create-dynamic-segment global-mode-string
  :getter wls--global-mode-string
  :dense t
  :priority current-low)

(whale-line-create-dynamic-segment minor-modes
  :getter (lambda () minor-mode-alist)
  :dense t
  :priority low)

(defun wls--process ()
  "Get process segment."
  (let ((mlp mode-line-process))

    (cond
     ((listp mlp)
      (cons (whale-line--spacer) (cdr mlp)))
     ((stringp mlp)
      (propertize (concat (whale-line--spacer) mlp) 'face 'whale-line-shadow))
     (t ""))))

(whale-line-create-dynamic-segment process
  :getter wls--process
  :condition mode-line-process
  :dense t
  :priority current)

(defun whale-line-selection--get-columns (beg end)
  "Get the columns from BEG to END for displaying `rectangle-mode'."
  (abs (- (save-excursion (goto-char end)
                          (current-column))
          (save-excursion (goto-char beg)
                          (current-column)))))

(defun wls--selection ()
  "Show the selection."
  (let* ((beg (region-beginning))
         (end (region-end))
         (lines (count-lines beg (min end (point-max)))))

    (propertize (if (bound-and-true-p rectangle-mark-mode)
                    (let ((columns (whale-line-selection--get-columns beg end)))
                      (format " %dx%d " lines columns))
                  (format " %d " lines))
                'face 'region)))

(whale-line-create-dynamic-segment selection
  :condition mark-active
  :getter wls--selection
  :priority current-low)

(provide 'whale-line-segments)

;;; whale-line-segments.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wls-" . "whale-line-segments-"))
;; End:
