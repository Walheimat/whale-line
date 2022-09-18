;;; wal-line-lsp.el --- Show LSP-related information. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; LSP integration.

;;; Code:

(declare-function wal-line--enabled-feature-p "wal-line.el")
(declare-function wal-line--is-current-window-p "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")
(declare-function wal-line-buffer-name--segment "wal-line.el")
(declare-function wal-line-icons--get-icon "wal-line-icons.el")

;;;; Customization:

(defcustom wal-line-lsp-delimiters '("[" "]")
  "The delimiters to indicate LSP status for buffer names."
  :group 'wal-line
  :type '(repeat string))

;;;; Functionality:
(defun wal-line-lsp--active-p ()
  "Check if an LSP mode is active."
  (or (bound-and-true-p lsp-mode) (bound-and-true-p eglot--managed-mode)))

(defun wal-line-lsp--color-icon (icon)
  "Advise the ICON segment to indicate LSP status."
  (if (and icon (wal-line-lsp--active-p))
      (let* ((f-props (get-text-property 0 'face icon))
             (f-new (copy-tree f-props)))
        (plist-put f-new :inherit 'wal-line-indicate)
        (propertize icon 'face f-new))
    icon))

(defun wal-line-lsp--advise-buffer-name (str)
  "Advise buffer STR to indicate LSP status."
  (if (wal-line-lsp--active-p)
      (let ((left (nth 0 wal-line-lsp-delimiters))
            (right (nth 1 wal-line-lsp-delimiters)))
        (concat
         (wal-line--spacer)
         (propertize left 'face 'wal-line-indicate)
         (string-trim str)
         (propertize right 'face 'wal-line-indicate)))
    str))

(defun wal-line-lsp--setup ()
  "Set up LSP integration."
  (cond
   ((wal-line--enabled-feature-p 'icons)
    (advice-add
     'wal-line-icons--get-icon :filter-return
     #'wal-line-lsp--color-icon))
   (t
    (advice-add
     #'wal-line-buffer-name--segment
     :filter-return #'wal-line-lsp--advise-buffer-name))))

(defun wal-line-lsp--teardown ()
  "Tear down LSP integration."
  (advice-remove
   'wal-line-icons--get-icon
   #'wal-line-lsp--color-icon)
  (advice-remove
   #'wal-line-buffer-name--segment
   #'wal-line-lsp--advise-buffer-name))

(add-hook 'wal-line-setup-hook #'wal-line-lsp--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-lsp--teardown)

(provide 'wal-line-lsp)

;;; wal-line-lsp.el ends here
