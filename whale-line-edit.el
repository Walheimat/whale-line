;;; whale-line-edit.el --- Edit mode-line segments -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.9.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Re-order, add and remove segments by editing a buffer.

;;; Code:

(require 'whale-line)

(defvar whale-line-edit--buffer "*whale-line-edit-segments*")
(defvar whale-line-edit--initial nil)

(defvar whale-line-edit-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-c" #'whale-line-edit--apply)
    (define-key map "\C-c\C-w" #'whale-line-edit--persist)
    (define-key map "\C-c\C-k" #'whale-line-edit--abort)
    (define-key map "\C-c\C-r" #'whale-line-edit--revert)
    map))

(define-minor-mode whale-line-edit-mode
  "Minor mode to edit segments."
  :lighter " wle"
  (setq-local header-line-format
              (substitute-command-keys
               "\\<whale-line-edit-mode-map>\
`\\[whale-line-edit--apply]' applies, \
`\\[whale-line-edit--persist]' persists, \
`\\[whale-line-edit--abort]' aborts, \
`\\[whale-line-edit--revert]' reverts.")))

(defun whale-line-edit--edit ()
  "Show the segment editor."
  (let ((buffer (get-buffer-create whale-line-edit--buffer))
        (count (length whale-line-segments)))

    (with-current-buffer buffer
      (erase-buffer)

      (seq-map-indexed
       (lambda (it i)
         (if (eq it '|)
             (insert "\n")
           (insert (symbol-name it))
           (unless (eq i (1- count))
             (insert "\n"))))
       whale-line-segments)

      (unless whale-line-edit-mode
        (whale-line-edit-mode)))

    (pop-to-buffer buffer nil t)))

(defun whale-line-edit--apply ()
  "Apply edited segments."
  (interactive)

  (when-let ((warnings (whale-line-edit--validate)))
    (user-error "Invalid edit, can't apply: %s" (string-join warnings ", ")))

  (let* ((buffer (get-buffer whale-line-edit--buffer))
         (raw (with-current-buffer buffer
                (buffer-string)))
         (win (get-buffer-window buffer))
         (edited nil))

    (seq-map
     (lambda (it)
       (if (string-empty-p it)
           (push '| edited)
         (push (intern it) edited)))
     (reverse (string-split raw "\n")))

    (quit-window t win)

    (unless whale-line-edit--initial
      (setq whale-line-edit--initial whale-line-segments))

    (setq whale-line-segments edited)
    (whale-line--build-segments)))

(defun whale-line-edit--persist ()
  "Persist the change."
  (interactive)

  (whale-line-edit--apply)
  (customize-save-variable 'whale-line-segments whale-line-segments))

(defun whale-line-edit--abort ()
  "Abort editing."
  (interactive)
  (quit-window t (get-buffer-window whale-line-edit--buffer)))

(defun whale-line-edit--revert ()
  "Revert all previous edits."
  (interactive)

  (unless whale-line-edit--initial
    (user-error "No edit has been made"))

  (whale-line-edit--abort)
  (setq whale-line-segments whale-line-edit--initial
        whale-line-edit--initial nil)

  (whale-line--build-segments))

(defun whale-line-edit--validate ()
  "Validate the current edit state.

Returns a list of warnings for every issue found."
  (let* ((buffer (get-buffer whale-line-edit--buffer))
         (raw (with-current-buffer buffer
                (buffer-string)))
         (lines (string-split raw "\n"))
         (warnings nil))

    (unless (eq (seq-count #'string-empty-p lines) 1)
      (push "More than one empty line" warnings))

    (unless (seq-every-p
             (lambda (it) (or (string-empty-p it)
                         (and-let* ((entry (assoc (intern it) whale-line--props))
                                    (props (cdr-safe entry))
                                    (type (plist-get props :type))
                                    ((memq type '(stateful stateless)))))))
             lines)
      (push "Invalid or unknown segment" warnings))

    warnings))

;;;; API

;;;###autoload
(defun whale-line-edit ()
  "Edit active segments."
  (interactive)

  (whale-line-edit--edit))

(provide 'whale-line-edit)

;;; whale-line-edit.el ends here
