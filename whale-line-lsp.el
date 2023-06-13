;;; whale-line-lsp.el --- Show LSP-related information -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; LSP integration.

;;; Code:

(require 'whale-line)
(require 'whale-line-segments)

(declare-function lsp-workspaces "ext:lsp-mode.el")
(declare-function whale-line--enabled-feature-p "whale-line.el")
(declare-function whale-line--is-current-window-p "whale-line.el")
(declare-function whale-line--spacer "whale-line.el")
(declare-function whale-line-buffer-name--get-segment "whale-line.el")
(declare-function whale-line-icons--get-segment "whale-line-icons.el")
(declare-function whale-line-icons--action "whale-line-icons.el")

;; Customization:

(defcustom wll-delimiters '("[" "]")
  "The delimiters to indicate LSP status for buffer names."
  :group 'whale-line
  :type '(repeat string))

;; Functionality:

(defun wll--active-p ()
  "Check if an LSP mode is active."
  (cond
   ((featurep 'lsp-mode)
    (lsp-workspaces))
   ((featurep 'eglot)
    (bound-and-true-p eglot--managed-mode))))

(whale-line-create-augment lsp
  :action
  (lambda (&rest _args)
    (if (whale-line--enabled-feature-p 'icons)
        (when-let* ((icon (whale-line-icons--get-segment))
                    (f-props (get-text-property 0 'face icon))
                    (f-new (copy-tree f-props)))
          (if (wll--active-p)
              (progn
                (plist-put f-new :inherit 'whale-line-indicate)
                (setq-local whale-line-icons--segment (propertize icon 'face f-new)))
            (setq-local whale-line-icons--segment icon)))
      (when (wll--active-p)
        (let ((left (nth 0 wll-delimiters))
              (right (nth 1 wll-delimiters))
              (str (or (whale-line-buffer-name--get-segment) "")))
          (setq whale-line-buffer-name--segment (concat
                                               (whale-line--spacer)
                                               (propertize left 'face 'whale-line-indicate)
                                               (string-trim str)
                                               (propertize right 'face 'whale-line-indicate)))))))

  :hooks
  (lsp-after-initialize-hook
   lsp-after-uninitialized-functions
   lsp-after-open-hook
   eglot-server-initialized-hook
   eglot-managed-mode-hook)

  :teardown
  (lambda ()
    (when (whale-line--enabled-feature-p 'icons)
      (whale-line-icons--action))))

(provide 'whale-line-lsp)

;;; whale-line-lsp.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wll-" . "whale-line-lsp-"))
;; End:
