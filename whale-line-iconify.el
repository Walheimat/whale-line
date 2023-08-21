;;; whale-line-iconify.el -- Icon usage -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.7.0
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
    (vc . (:name "code-fork"))
    (buffer-read-only . (:name "lock" :face whale-line-contrast :fallback "@" :parent buffer-status))
    (buffer-file-name . (:name "sticky-note-o" :face whale-line-shadow :fallback "&" :parent buffer-status))
    (buffer-modified . (:name "pencil" :face whale-line-emphasis :fallback "*" :parent buffer-status))
    (window-dedicated . (:name "link" :face whale-line-shadow :fallback "^"))
    (buffer-fallback . (:name "question-circle" :face whale-line-contrast :no-defaults t))
    (lsp . (:name "server" :face whale-line-contrast :fallback "LSP"))
    (partial-recall . (:name "tag" :face whale-line-contrast :fallback "PR")))
  "Named icon specifications.

Each specifications defines icon NAME and, optionally, FALLBACK
text, NO-DEFAULTS and PARENT. Icons using fonts other than
FontAwesome need to set FONT. Any remaining spec will be passed
as icon specs. See `whale-line-iconify-default-specs' for
automatically applied specs."
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

If icon can't or shouldn't be displayed return any existing
fallback."
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
