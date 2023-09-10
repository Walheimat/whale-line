;;; whale-line-iconify.el -- Icon usage -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.7.1
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Use icons for segments.

;;; Code:

(require 'cl-lib)
(require 'whale-line-core)

;;; -- Customization

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
    (buffer-read-only . (:name "lock" :face whale-line-contrast :fallback "@" :parent buffer-status))
    (buffer-file-name . (:name "sticky-note-o" :face whale-line-shadow :fallback "&" :parent buffer-status))
    (buffer-modified . (:name "pencil" :face whale-line-emphasis :fallback "*" :parent buffer-status))
    (window-dedicated . (:name "link" :face whale-line-shadow :fallback "^" :parent window-status))
    (window-no-other . (:name "low-vision" :face whale-line-shadow :fallback "~" :parent window-status))
    (buffer-fallback . (:name "question-circle" :face whale-line-contrast :no-defaults t))
    (lsp . (:name "plug" :face whale-line-contrast :fallback "LSP"))
    (dap . (:name "bug" :face whale-line-urgent :fallback "DAP"))
    (partial-recall . (:name "tag" :face whale-line-contrast :fallback "PR")))
  "Icon specifications.

This is an alist of (ICON-SYMBOL . SPECS). The ICON-SYMBOL is the
symbol passed to `whale-line-iconify' to retrieve the associated
specs. SPECS is a plist with the following keys.

NAME is the name of the icon in the font. The remaining keys are
optional.

FACE is the face to use for the icon.

FONT is a symbol, namely one of the possible suffixes of
`all-the-icons-insert-*'. It defaults to `faicon'.

FALLBACK is a string to use when the icon can't be used or is
disabled.

PARENT is a symbol of another icon. This allows disabling an icon
through its parent.

NO-DEFAULTS is a boolean. If it is non-nil, no default icon specs
will be applied. See `whale-line-iconify-default-specs' for
automatically applied specs.

All remaining keys and their values will be passed to
`propertize' as-is."
  :group 'whale-line-iconify
  :type '(alist :key-type symbol :value-type plist))

;;; -- Variables

(defvar wli--default-specs '(:height 0.85 :v-adjust 0.0))

;;; -- Functionality

(defun wli--from-specs (specs)
  "Get icon from SPECS."
  (let* ((font (or (plist-get specs :font) 'faicon))
         (fun (intern (concat "all-the-icons-" (symbol-name font))))
         (icon (plist-get specs :name))
         (remainder (wli--pure-specs specs))
         (defaults (wli--default-specs specs remainder)))

    (apply fun (append (list icon) remainder defaults))))

(defun wli--pure-specs (specs)
  "Return icon only specs from SPECS."
  (cl-loop for (key . val) in (cl--plist-to-alist specs)
           unless (memq key '(:name :fallback :font :no-defaults :parent))
           nconc (list key val)))

(defun wli--default-specs (specs existing)
  "Return the default specs if SPECS wants to use them.

Only properties not in EXISTING are added."
  (unless (plist-get specs :no-defaults)
    (cl-loop for (key . val) in (cl--plist-to-alist wli--default-specs)
             unless (plist-get existing key)
             nconc (list key val))))

(defun wli--use-for-p (name)
  "Check whether icons should be used for NAME."
  (let ((disabled wli-disabled))

    (and (wli--can-use-p)
         (not (eq 'all disabled))
         (not (memq name disabled)))))

(defun wli--can-use-p ()
  "Check whether icons can be used."
  (require 'all-the-icons nil t))

;;; -- API

;;;###autoload
(defun whale-line-iconify (name &optional face)
  "Get the icon for NAME.

If optional argument FACE is passed, it will be used instead of
the value in the retrieved specs.

If icon can't or shouldn't be displayed, any existing fallback is
returned."
  (if-let* ((specs (cdr-safe (assoc name wli-specs)))
            (specs (if face (plist-put specs :face face) specs))
            (parent (or (plist-get specs :parent) name))
            ((wli--use-for-p parent)))

      (wli--from-specs specs)

    (when-let ((fallback (plist-get specs :fallback)))

      (propertize fallback 'face (plist-get specs :face)))))

(provide 'whale-line-iconify)

;;; whale-line-iconify.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wli-" . "whale-line-iconify-"))
;; End:
