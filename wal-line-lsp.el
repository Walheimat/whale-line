;;; wal-line-lsp.el --- Show LSP-related information -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; LSP integration.

;;; Code:

(require 'wal-line)

(declare-function lsp-workspaces "ext:lsp-mode.el")
(declare-function wal-line--enabled-feature-p "wal-line.el")
(declare-function wal-line--is-current-window-p "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")
(declare-function wal-line-buffer-name--get-segment "wal-line.el")
(declare-function wal-line-icons--get-segment "wal-line-icons.el")
(declare-function wal-line-icons--set-segment "wal-line-icons.el")

;; Customization:

(defcustom wll-delimiters '("[" "]")
  "The delimiters to indicate LSP status for buffer names."
  :group 'wal-line
  :type '(repeat string))

;; Functionality:

(defun wll--active-p ()
  "Check if an LSP mode is active."
  (cond
   ((featurep 'lsp-mode)
    (lsp-workspaces))
   ((featurep 'eglot)
    (bound-and-true-p eglot--managed-mode))))

(wal-line-create-augment lsp
  :action
  (lambda (&rest _args)
    (if (wal-line--enabled-feature-p 'icons)
        (when-let* ((icon (wal-line-icons--get-segment))
                    (f-props (get-text-property 0 'face icon))
                    (f-new (copy-tree f-props)))
          (if (wll--active-p)
              (progn
                (plist-put f-new :inherit 'wal-line-indicate)
                (setq-local wal-line-icons--segment (propertize icon 'face f-new)))
            (setq-local wal-line-icons--segment icon)))
      (when (wll--active-p)
        (let ((left (nth 0 wll-delimiters))
              (right (nth 1 wll-delimiters))
              (str (or (wal-line-buffer-name--get-segment) "")))
          (setq wal-line-buffer-name--segment (concat
                                               (wal-line--spacer)
                                               (propertize left 'face 'wal-line-indicate)
                                               (string-trim str)
                                               (propertize right 'face 'wal-line-indicate)))))))
  :hooks
  (lsp-after-initialize-hook
   lsp-after-uninitialized-functions
   lsp-after-open-hook
   eglot-server-initialized-hook
   eglot-managed-mode-hook)
  :teardown
  (lambda ()
    (when (wal-line--enabled-feature-p 'icons)
      (wal-line-icons--set-segment))))

(provide 'wal-line-lsp)

;;; wal-line-lsp.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wll-" . "wal-line-lsp-"))
;; End:
