;;; wal-line-icons.el --- Icons for the modeline -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Show icons in the modeline.

;;; Code:

(require 'all-the-icons)
(require 'wal-line)

(declare-function all-the-icons-icon-for-buffer "ext:all-the-icons.el")
(declare-function all-the-icons-faicon "ext:all-the-icons.el")
(declare-function wal-line--spacer "wal-line.el")
(declare-function wal-line-buffer-status--segment "wal-line.el")
(declare-function wal-line-project--get-info "wal-line-project.el")
(declare-function wal-line-vc--face-for-state "wal-line-vc.el")
(declare-function wal-line-vc--get-info "wal-line-vc.el")

;; Customization:

(defcustom wal-line-icons-prettify-buffer-status nil
  "Whether to use icons for the buffer status."
  :group 'wal-line
  :type 'boolean)

;; Additional icons:

(defun wal-line-icons--prepend-icon-to-project-segment (str)
  "Advise info getter to prepend an icon before STR."
  (if (and (stringp str)
           (display-graphic-p))
      (concat (all-the-icons-faicon "folder-open"
                                    :face 'wal-line-emphasis
                                    :height 0.85
                                    :v-adjust 0.0)
              (wal-line--spacer)
              str)
    str))

(defun wal-line-icons--prepend-icon-to-vc-segment (str)
  "Advise info getter to prepend an icon before STR."
  (if (and (stringp str)
           (display-graphic-p)
           (buffer-file-name))
      (concat
       (all-the-icons-faicon "code-fork"
                             :face (wal-line-vc--face-for-state)
                             :height 0.85
                             :v-adjust 0.0)
       (wal-line--spacer)
       str)
    str))

(defun wal-line-icons--advise-buffer-status-segment ()
  "Advise buffer line segment to use icons."
  (let* ((icon-and-face (cond
                         (buffer-read-only (cons "lock" 'wal-line-contrast))
                         ((not (buffer-file-name))
                          (cons "sticky-note-o" 'wal-line-shadow))
                         ((buffer-modified-p)
                          (cons "pencil" 'wal-line-emphasis))
                         (t (cons "" nil))))
         (icon (car icon-and-face))
         (face (cdr icon-and-face)))
    (if (string-empty-p icon)
        ""
      (concat
       (wal-line--spacer)
       (propertize (all-the-icons-faicon icon
                                         :face face
                                         :height 0.85
                                         :v-adjust 0.0))))))


;; Segment:

(wal-line-create-static-segment icons
  :dense t
  :getter
  (when (display-graphic-p)
    (let ((icon (all-the-icons-icon-for-buffer)))

      (propertize (if (or (null icon) (symbolp icon))
                      (all-the-icons-faicon
                       "question-circle"
                       :face 'wal-line-contrast)
                    icon)
                  'help-echo (format "%s" (format-mode-line mode-name))
                  'display '(raise -0.135))))
  :setup
  (lambda ()
    (advice-add
     #'wal-line-project--get-segment :filter-return
     #'wal-line-icons--prepend-icon-to-project-segment)
    (wal-line-project--set-segment)

    (advice-add
     #'wal-line-vc--get-segment :filter-return
     #'wal-line-icons--prepend-icon-to-vc-segment)
    (wal-line-vc--set-segment)

    (when wal-line-icons-prettify-buffer-status
      (advice-add
       #'wal-line-buffer-status--segment
       :override #'wal-line-icons--advise-buffer-status-segment))

    (add-hook 'find-file-hook #'wal-line-icons--set-segment)
    (add-hook 'after-change-major-mode-hook #'wal-line-icons--set-segment)
    (add-hook 'clone-indirect-buffer-hook #'wal-line-icons--set-segment))
  :teardown
  (lambda ()
    (advice-remove
     #'wal-line-project--get-segment
     #'wal-line-icons--prepend-icon-to-project-segment)
    (wal-line-project--set-segment)

    (advice-remove
     #'wal-line-vc--get-segment
     #'wal-line-icons--prepend-icon-to-vc-segment)
    (wal-line-vc--set-segment)

    (when wal-line-icons-prettify-buffer-status
      (advice-remove
       #'wal-line-buffer-status--segment
       #'wal-line-icons--advise-buffer-status-segment))

    (remove-hook 'find-file-hook #'wal-line-icons--set-segment)
    (remove-hook 'after-change-major-mode-hook #'wal-line-icons--set-segment)
    (remove-hook 'clone-indirect-buffer-hook #'wal-line-icons--set-segment)))

(wal-line-add-segment 'icons)

(provide 'wal-line-icons)

;;; wal-line-icons.el ends here
