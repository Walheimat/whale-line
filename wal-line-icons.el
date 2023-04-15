;;; wal-line-icons.el --- Icons for the modeline -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Show icons in the modeline.

;;; Code:

(require 'wal-line)

(declare-function all-the-icons-icon-for-buffer "ext:all-the-icons.el")
(declare-function all-the-icons-faicon "ext:all-the-icons.el")
(declare-function wal-line--spacer "wal-line.el")
(declare-function wal-line-buffer-status--segment "wal-line.el")
(declare-function wal-line-project--get-segment "wal-line-project.el")
(declare-function wal-line-project--set-segment "wal-line-project.el")
(declare-function wal-line-vc--face-for-state "wal-line-vc.el")
(declare-function wal-line-vc--get-segment "wal-line-vc.el")
(declare-function wal-line-vc--set-segment "wal-line-vc.el")

;; Customization:

(defcustom wal-line-icons-prettify-buffer-status nil
  "Whether to use icons for the buffer status."
  :group 'wal-line
  :type 'boolean)

(defconst wal-line-icon-type
  '(cons symbol
         (restricted-sexp
          :match-alternatives
          (stringp listp)))
  "Icon type as a cons cell of the icon library and the icons specs.
The specs are either a string of the icon name or a list of the
icon name and the face.")

(defcustom wal-line-icons-project-icon '(faicon . "folder-open")
  "Icon used for the project segment."
  :group 'wal-line
  :type wal-line-icon-type)

(defcustom wal-line-icons-vc-icon '(faicon . "code-fork")
  "Icon used for the VC segment."
  :group 'wal-line
  :type wal-line-icon-type)

(defcustom wal-line-icons-buffer-read-only-icon '(faicon . ("lock" wal-line-contrast))
  "Icon used to indicate buffer is read-only."
  :group 'wal-line
  :type wal-line-icon-type)

(defcustom wal-line-icons-no-buffer-file-name-icon '(faicon . ("sticky-note-o" wal-line-shadow))
  "Icon used to indicate buffer has no file name."
  :group 'wal-line
  :type wal-line-icon-type)

(defcustom wal-line-icons-buffer-modified-icon '(faicon . ("pencil" wal-line-emphasis))
  "Icon used to indicate buffer has been modified."
  :group 'wal-line
  :type wal-line-icon-type)

(defcustom wal-line-icons-window-dedicated-icon '(faicon . ("link" wal-line-shadow))
  "Icon used to indicate a window is dedicated to its buffer."
  :group 'wal-line
  :type wal-line-icon-type)

(defcustom wal-line-icons-buffer-fallback-icon '(faicon . ("question-circle" wal-line-contrast))
  "Icon used when a buffer has no associated icon."
  :group 'wal-line
  :type wal-line-icon-type)

;; Additional icons:

(defun wal-line-icons--icon (specs &rest plist)
  "Get icon in SPECS with PLIST properties."
  (declare (indent defun))
  (let ((fun (intern (concat "all-the-icons-" (symbol-name (car specs)))))
        (icon (if (listp (cdr specs))
                  (cadr specs)
                (cdr specs)))
        (face (when (listp (cdr specs))
                (caddr specs))))

    (if face
        (apply fun (append (list icon :face face) plist))
      (apply fun (append (list icon) plist)))))

(defun wal-line-icons--prepend-icon-to-project-segment (str)
  "Advise info getter to prepend an icon before STR."
  (if (and (stringp str)
           (display-graphic-p))
      (concat (wal-line-icons--icon wal-line-icons-project-icon
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
       (wal-line-icons--icon wal-line-icons-vc-icon
         :face (wal-line-vc--face-for-state)
         :height 0.85
         :v-adjust 0.0)
       (wal-line--spacer)
       str)
    str))

(defun wal-line-icons--advise-buffer-status-segment ()
  "Advise buffer line segment to use icons."
  (let ((icon (cond
               (buffer-read-only wal-line-icons-buffer-read-only-icon)
               ((not (buffer-file-name))
                wal-line-icons-no-buffer-file-name-icon)
               ((buffer-modified-p)
                wal-line-icons-buffer-modified-icon)
               (t nil))))

    (if icon
        (concat
         (wal-line--spacer)
         (wal-line-icons--icon icon
           :height 0.85
           :v-adjust 0.0))
      "")))

(defun wal-line-icons--advise-window-status-segment ()
  "Advise window status segment to use icons."
  (if (window-dedicated-p)
      (concat
       (wal-line--spacer)
       (wal-line-icons--icon wal-line-icons-window-dedicated-icon
         :height 0.85
         :v-adjust 0.0))
    ""))

;; Segment:

(wal-line-create-static-segment icons
  :dense t
  :verify (lambda () (require 'all-the-icons nil t))
  :getter
  (when (display-graphic-p)
    (let ((icon (all-the-icons-icon-for-buffer)))

      (propertize (if (or (null icon) (symbolp icon))
                      (wal-line-icons--icon wal-line-icons-buffer-fallback-icon)
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

    (advice-add
     #'wal-line-window-status--segment :override
     #'wal-line-icons--advise-window-status-segment)

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

(provide 'wal-line-icons)

;;; wal-line-icons.el ends here
