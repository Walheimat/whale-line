;;; whale-line-iconify.el --- Decorate segments with icons -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.9.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Decorates `whale-line-segments' using `all-the-icons'.
;;
;; Provides `whale-line-iconify' and `whale-line-iconify-decorates-p'.
;; To use different icons you can customize
;; `whale-line-iconify-specs'. You can disable individual segments (or
;; sub-segments) by adding them to `whale-line-iconify-disabled'.

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup whale-line-iconify nil
  "Settings for icon usage."
  :group 'whale-line
  :tag "Segments")

(defcustom whale-line-iconify-disabled nil
  "Disabled icons.

This can either be a list of keys from
`whale-line-iconify-specs', the symbol `all' to disable all icons
or the nil to disable none.

This has no effect if icons cannot be enabled. See
`whale-line-iconify--can-use-icons-p'."
  :group 'whale-line-iconify
  :type '(restricted-sexp
          :match-alternatives
          (listp 'all 'nil)))

(defcustom whale-line-iconify-specs
  '((project . (:name "package" :font octicon :face whale-line-emphasis))
    (vc . (:name "code-fork" :face whale-line-contrast))
    (major-mode . (:function all-the-icons-icon-for-buffer))
    (buffer-read-only . (:name "lock" :face whale-line-contrast :parent buffer-status))
    (buffer-file-name . (:name "sticky-note-o" :face whale-line-shadow :parent buffer-status))
    (buffer-modified . (:name "pencil" :face whale-line-emphasis :parent buffer-status))
    (window-dedicated . (:name "link" :face whale-line-shadow :parent window-status))
    (window-no-other . (:name "low-vision" :face whale-line-shadow :parent window-status))
    (window-no-delete . (:name "deaf" :face whale-line-shadow :parent window-status))
    (buffer-fallback . (:name "question-circle" :face whale-line-contrast :no-defaults t))
    (lsp . (:name "plug" :face whale-line-contrast))
    (debug . (:name "bug" :face whale-line-urgent))
    (client . (:name "server" :face whale-line-shadow)))
  "Icon specifications.

This is an alist of (ICON-SYMBOL . SPECS). The ICON-SYMBOL is the
symbol passed to `whale-line-iconify' to retrieve the associated
specs. SPECS is a plist with the following keys.

FUNCTION is a function to use instead of constructing it using
NAME. If PASS-ARGS is t, all icon specs are passed to the
function.

NAME is the name of the icon in the font. The remaining keys are
optional.

FACE is the face to use for the icon.

FONT is a symbol, namely one of the possible suffixes of
`all-the-icons-insert-*'. It defaults to `faicon'.

PARENT is a symbol of another icon. This allows disabling an icon
through its parent.

NO-DEFAULTS is a boolean. If it is non-nil, no default icon specs
will be applied. See `whale-line-iconify-default-specs' for
automatically applied specs.

All remaining keys and their values will be passed to
`propertize' as-is."
  :group 'whale-line-iconify
  :type '(alist :key-type symbol :value-type plist))

;;;; Variables

(defvar whale-line-iconify--default-specs '(:height 0.85 :v-adjust 0.0))

;;;; Functionality

(defun whale-line-iconify--from-specs (specs)
  "Get icon from SPECS."
  (if-let* ((fun (plist-get specs :function)))
      (apply fun (when (plist-get specs :pass-args)
                   (whale-line-iconify--pure-specs specs)))
    (let* ((font (or (plist-get specs :font) 'faicon))
           (fun (intern (concat "all-the-icons-" (symbol-name font))))
           (icon (plist-get specs :name))
           (remainder (whale-line-iconify--pure-specs specs))
           (defaults (whale-line-iconify--default-specs specs remainder)))

      (apply fun (append (list icon) remainder defaults)))))

(defun whale-line-iconify--pure-specs (specs)
  "Return icon only specs from SPECS."
  (cl-loop for (key . val) in (cl--plist-to-alist specs)
           unless (memq key '(:name :function :font :no-defaults :parent :pass-args))
           nconc (list key val)))

(defun whale-line-iconify--default-specs (specs existing)
  "Return the default specs if SPECS wants to use them.

Only properties not in EXISTING are added."
  (unless (plist-get specs :no-defaults)
    (cl-loop for (key . val) in (cl--plist-to-alist whale-line-iconify--default-specs)
             unless (plist-get existing key)
             nconc (list key val))))

(defun whale-line-iconify--use-for-p (name)
  "Check whether icons should be used for NAME."
  (let ((disabled whale-line-iconify-disabled))

    (and (whale-line-iconify--can-use-p)
         (not (eq 'all disabled))
         (not (memq name disabled)))))

(defun whale-line-iconify--can-use-p ()
  "Check whether icons can be used."
  (and (display-graphic-p)
       (featurep 'all-the-icons)))

;;;; API

;;;###autoload
(cl-defun whale-line-iconify (name &key face &allow-other-keys)
  "Get the icon for NAME.

If optional argument FACE is passed, it will be used instead of
the value in the retrieved specs."
  (when-let* ((specs (copy-tree (cdr-safe (assoc name whale-line-iconify-specs))))
              (specs (if face (plist-put specs :face face) specs))
              (parent (or (plist-get specs :parent) name))
              ((whale-line-iconify--use-for-p parent)))

    (whale-line-iconify--from-specs specs)))

;;;###autoload
(defun whale-line-iconify-decorates-p (name)
  "Check if NAME is decorated."
  (whale-line-iconify--use-for-p name))

;;;###autoload
(define-minor-mode whale-line-iconify-mode
  "Mode that decorates segments using `all-the-icons'."
  :global t
  (if whale-line-iconify-mode
      (progn
        (advice-add
         'whale-line-segments--decorate :override
         #'whale-line-iconify)
        (advice-add
         'whale-line-segments--decorates-p :override
         #'whale-line-iconify-decorates-p))
    (advice-remove
     'whale-line-segments--decorate
     #'whale-line-iconify)
    (advice-remove
     'whale-line-segments--decorates-p
     #'whale-line-iconify-decorates-p)))

(provide 'whale-line-iconify)

;;; whale-line-iconify.el ends here
