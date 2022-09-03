;;; wal-line-org.el --- Indicate position for Org files -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Org-mode integration.

;;; Code:

(eval-when-compile
  (require 'org))

(require 'wal-line)

(declare-function wal-line-buffer-name--segment "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")
(declare-function org-back-to-heading "ext:org.el")
(declare-function org-before-first-heading-p "ext:org.el")
(declare-function org-heading-components "ext:org.el")
(declare-function org-link-display-format "ext:org.el")

;;;; Customization:

(defcustom wal-line-org-delimiter "/"
  "The delimiter between file name and heading name."
  :group 'wal-line
  :type 'string)

;;;; Functionality:

(defun wal-line-org--get-heading ()
  "Get propertized heading."
  (org-with-wide-buffer
   (goto-char (window-start))
   (org-back-to-heading)
   (let* ((components (org-heading-components))
          (level (nth 0 components))
          (face (nth (- level 1) org-level-faces))
          (heading (org-link-display-format (nth 4 components))))
     (propertize heading 'face face))))

(defun wal-line-org--segment ()
  "Displays the current heading."
  (if (eq major-mode 'org-mode)
      (if (org-before-first-heading-p)
          ""
        (concat (wal-line--spacer)
                wal-line-org-delimiter
                (wal-line--spacer)
                (wal-line-org--get-heading)
                (wal-line--spacer)))
    ""))

(defvar wal-line--segments)
(wal-line-add-segment org)

(provide 'wal-line-org)

;;; wal-line-org.el ends here
