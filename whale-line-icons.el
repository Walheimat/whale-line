;;; whale-line-icons.el --- Icons for the modeline -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Show icons in the modeline.

;;; Code:

(require 'whale-line)

(declare-function all-the-icons-icon-for-buffer "ext:all-the-icons.el")
(declare-function all-the-icons-faicon "ext:all-the-icons.el")
(declare-function whale-line--spacer "whale-line.el")
(declare-function whale-line-buffer-status--segment "whale-line.el")
(declare-function whale-line-project--get-segment "whale-line-project.el")
(declare-function whale-line-project--action "whale-line-project.el")
(declare-function whale-line-vc--face-for-state "whale-line-vc.el")
(declare-function whale-line-vc--get-segment "whale-line-vc.el")
(declare-function whale-line-vc--action "whale-line-vc.el")

;; Customization:

(defcustom wli-prettify-buffer-status nil
  "Whether to use icons for the buffer status."
  :group 'whale-line
  :type 'boolean)

(defconst whale-line-icon-type
  '(cons symbol
         (restricted-sexp
          :match-alternatives
          (stringp listp)))
  "Icon type as a cons cell of the icon library and the icons specs.
The specs are either a string of the icon name or a list of the
icon name and the face.")

(defcustom wli-project-icon '(faicon . "folder-open")
  "Icon used for the project segment."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom wli-vc-icon '(faicon . "code-fork")
  "Icon used for the VC segment."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom wli-buffer-read-only-icon '(faicon . ("lock" whale-line-contrast))
  "Icon used to indicate buffer is read-only."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom wli-no-buffer-file-name-icon '(faicon . ("sticky-note-o" whale-line-shadow))
  "Icon used to indicate buffer has no file name."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom wli-buffer-modified-icon '(faicon . ("pencil" whale-line-emphasis))
  "Icon used to indicate buffer has been modified."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom wli-window-dedicated-icon '(faicon . ("link" whale-line-shadow))
  "Icon used to indicate a window is dedicated to its buffer."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom wli-buffer-fallback-icon '(faicon . ("question-circle" whale-line-contrast))
  "Icon used when a buffer has no associated icon."
  :group 'whale-line
  :type whale-line-icon-type)

;; Additional icons:

(defun wli--icon (specs &rest plist)
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

(defun wli--prepend-icon-to-project-segment (str)
  "Advise info getter to prepend an icon before STR."
  (if (and (stringp str)
           (display-graphic-p))
      (concat (wli--icon wli-project-icon
                :face 'whale-line-emphasis
                :height 0.85
                :v-adjust 0.0)
              (whale-line--spacer)
              str)
    str))

(defun wli--prepend-icon-to-vc-segment (str)
  "Advise info getter to prepend an icon before STR."
  (if (and (stringp str)
           (display-graphic-p)
           (buffer-file-name))
      (concat
       (wli--icon wli-vc-icon
         :face (whale-line-vc--face-for-state)
         :height 0.85
         :v-adjust 0.0)
       (whale-line--spacer)
       str)
    str))

(defun wli--advise-buffer-status-segment ()
  "Advise buffer line segment to use icons."
  (let ((icon (cond
               (buffer-read-only wli-buffer-read-only-icon)
               ((not (buffer-file-name))
                wli-no-buffer-file-name-icon)
               ((buffer-modified-p)
                wli-buffer-modified-icon)
               (t nil))))

    (if icon
        (concat
         (whale-line--spacer)
         (wli--icon icon
           :height 0.85
           :v-adjust 0.0))
      "")))

(defun wli--advise-window-status-segment ()
  "Advise window status segment to use icons."
  (if (window-dedicated-p)
      (concat
       (whale-line--spacer)
       (wli--icon wli-window-dedicated-icon
         :height 0.85
         :v-adjust 0.0))
    ""))

;; Segment:

(whale-line-create-static-segment icons
  :dense t

  :verify (lambda () (require 'all-the-icons nil t))

  :getter
  (when (display-graphic-p)
    (let ((icon (all-the-icons-icon-for-buffer)))

      (propertize (if (or (null icon) (symbolp icon))
                      (wli--icon wli-buffer-fallback-icon)
                    icon)
                  'help-echo (format "%s" (format-mode-line mode-name))
                  'display '(raise -0.135))))

  :hooks
  (find-file-hook after-change-major-mode-hook clone-indirect-buffer-hook)

  :setup
  (lambda ()
    (advice-add
     #'whale-line-project--get-segment :filter-return
     #'wli--prepend-icon-to-project-segment)
    (whale-line-project--action)

    (advice-add
     #'whale-line-vc--get-segment :filter-return
     #'wli--prepend-icon-to-vc-segment)
    (whale-line-vc--action)

    (advice-add
     #'whale-line-window-status--segment :override
     #'wli--advise-window-status-segment)

    (when wli-prettify-buffer-status
      (advice-add
       #'whale-line-buffer-status--segment
       :override #'wli--advise-buffer-status-segment)))

  :teardown
  (lambda ()
    (advice-remove
     #'whale-line-project--get-segment
     #'wli--prepend-icon-to-project-segment)
    (whale-line-project--action)

    (advice-remove
     #'whale-line-vc--get-segment
     #'wli--prepend-icon-to-vc-segment)
    (whale-line-vc--action)

    (when wli-prettify-buffer-status
      (advice-remove
       #'whale-line-buffer-status--segment
       #'wli--advise-buffer-status-segment))))

(provide 'whale-line-icons)

;;; whale-line-icons.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wli-" . "whale-line-icons-"))
;; End:
