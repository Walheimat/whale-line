;;; wal-line-lsp.el --- Show LSP-related information -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; LSP integration.

;;; Code:

(declare-function lsp-workspaces "ext:lsp-mode.el")
(declare-function wal-line--enabled-feature-p "wal-line.el")
(declare-function wal-line--is-current-window-p "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")
(declare-function wal-line-buffer-name--get-segment "wal-line.el")
(declare-function wal-line-icons--get-segment "wal-line-icons.el")
(declare-function wal-line-icons--set-segment "wal-line-icons.el")

;; Customization:

(defcustom wal-line-lsp-delimiters '("[" "]")
  "The delimiters to indicate LSP status for buffer names."
  :group 'wal-line
  :type '(repeat string))

;; Functionality:

(defun wal-line-lsp--active-p ()
  "Check if an LSP mode is active."
  (cond
   ((featurep 'lsp-mode)
    (lsp-workspaces))
   ((featurep 'eglot)
    (bound-and-true-p eglot--managed-mode))))

(defun wal-line-lsp--color-icon (&rest _)
  "Color the icon segment to indicate LSP status."
  (when-let* ((icon (wal-line-icons--get-segment))
              (f-props (get-text-property 0 'face icon))
              (f-new (copy-tree f-props)))
    (if (wal-line-lsp--active-p)
        (progn
          (plist-put f-new :inherit 'wal-line-indicate)
          (setq-local wal-line-icons--segment (propertize icon 'face f-new)))
      (setq-local wal-line-icons--segment icon))))

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
  (if (wal-line--enabled-feature-p 'icons)
      (progn
        (add-hook 'lsp-after-initialize-hook #'wal-line-lsp--color-icon)
        (add-hook 'lsp-after-uninitialized-functions #'wal-line-lsp--color-icon)
        (add-hook 'lsp-after-open-hook #'wal-line-lsp--color-icon)

        (add-hook 'eglot-server-initialized-hook #'wal-line-lsp--color-icon)
        (add-hook 'eglot-managed-mode-hook #'wal-line-lsp--color-icon))
    (advice-add
     #'wal-line-buffer-name--get-segment :filter-return
     #'wal-line-lsp--advise-buffer-name)))

(defun wal-line-lsp--teardown ()
  "Tear down LSP integration."
  (remove-hook 'lsp-after-initialize-hook #'wal-line-lsp--color-icon)
  (remove-hook 'lsp-after-uninitialized-functions #'wal-line-lsp--color-icon)
  (remove-hook 'lsp-after-open-hook #'wal-line-lsp--color-icon)

  (remove-hook 'eglot-managed-mode-hook #'wal-line-lsp--color-icon)
  (remove-hook 'eglot-server-initialized-hook #'wal-line-lsp--color-icon)

  (when (wal-line--enabled-feature-p 'icons)
    (wal-line-icons--set-segment))

  (advice-remove
   #'wal-line-buffer-name--get-segment
   #'wal-line-lsp--advise-buffer-name))

(add-hook 'wal-line-setup-hook #'wal-line-lsp--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-lsp--teardown)

(provide 'wal-line-lsp)

;;; wal-line-lsp.el ends here
