;;; wal-line-org.el --- Indicate position for Org files -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Org-mode integration.

;;; Code:

(eval-when-compile
  (require 'org))

(require 'wal-line)

(declare-function org-back-to-heading "ext:org.el")
(declare-function org-before-first-heading-p "ext:org.el")
(declare-function org-heading-components "ext:org.el")
(declare-function org-link-display-format "ext:org.el")
(declare-function wal-line-buffer-name--segment "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")

;; Customization:

(defcustom wal-line-org-delimiter "/"
  "The delimiter between file name and heading name."
  :group 'wal-line
  :type 'string)

(defcustom wal-line-org-ellipsis "…"
  "The string indicating truncation."
  :group 'wal-line
  :type 'string)

(defcustom wal-line-org-include 'current-and-root
  "The heading depth to show."
  :group 'wal-line
  :type '(choice (const current-and-root)
                 (const current)))

(defcustom wal-line-org-max-heading-length 12
  "The max length of a heading before truncation."
  :group 'wal-line
  :type 'integer)

;; Functionality:

(defun wal-line-org--maybe-truncate (heading)
  "Maybe truncate HEADING."
  (let ((max-len wal-line-org-max-heading-length)
        (len (string-width heading))
        (ellipsis-len (string-width wal-line-org-ellipsis)))
    (if (> len max-len)
        (concat (substring heading 0 (max (- max-len ellipsis-len) 1))
                wal-line-org-ellipsis)
      heading)))

(defun wal-line-org--get-next-heading ()
  "Get the next heading going backwards."
  (org-back-to-heading)
  (let* ((components (org-heading-components))
         (level (nth 0 components))
         (face (nth (- level 1) org-level-faces))
         (heading (org-link-display-format (nth 4 components))))
    (propertize heading 'face face)))

(defun wal-line-org--collect-headings ()
  "Collect headings until it's no longer safe."
  (save-excursion
    (cl-loop collect (wal-line-org--get-next-heading)
             while (org-up-heading-safe))))

(defun wal-line-org--build-segment ()
  "Build the segment from included segments."
  (org-with-wide-buffer
   (goto-char (window-start))
   (unless (org-before-first-heading-p)
     (let ((headings (wal-line-org--collect-headings)))
       (if (null headings)
           ""
         (pcase wal-line-org-include
           ('current (nth 0 headings))
           ('current-and-root
            (if (> (length headings) 1)
                (concat
                 (wal-line-org--maybe-truncate (car (last headings)))
                 (wal-line--spacer)
                 (nth 0 headings))
              (nth 0 headings)))))))))

;; Segment:

(wal-line-create-dynamic-segment org
  :getter
  (let ((segment (wal-line-org--build-segment)))
    (when segment
      (concat
       wal-line-org-delimiter
       (wal-line-org--build-segment))))
  :condition
  (eq major-mode 'org-mode)
  :dense t)

(provide 'wal-line-org)

;;; wal-line-org.el ends here
