;;; wal-line-org.el --- Indicate position for Org files -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
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

(defcustom wlo-delimiter "/"
  "The delimiter between file name and heading name."
  :group 'wal-line
  :type 'string)

(defcustom wlo-ellipsis "…"
  "The string indicating truncation."
  :group 'wal-line
  :type 'string)

(defcustom wlo-include 'current-and-root
  "The heading depth to show."
  :group 'wal-line
  :type '(choice (const current-and-root)
                 (const current)))

(defcustom wlo-max-heading-length 12
  "The max length of a heading before truncation."
  :group 'wal-line
  :type 'integer)

;; Functionality:

(defun wlo--maybe-truncate (heading)
  "Maybe truncate HEADING."
  (let ((max-len wlo-max-heading-length)
        (len (string-width heading))
        (ellipsis-len (string-width wlo-ellipsis)))
    (if (> len max-len)
        (concat (substring heading 0 (max (- max-len ellipsis-len) 1))
                wlo-ellipsis)
      heading)))

(defun wlo--get-next-heading ()
  "Get the next heading going backwards."
  (org-back-to-heading)
  (let* ((components (org-heading-components))
         (level (nth 0 components))
         (face (nth (- level 1) org-level-faces))
         (heading (org-link-display-format (nth 4 components))))
    (propertize heading 'face face)))

(defun wlo--collect-headings ()
  "Collect headings until it's no longer safe."
  (when (fboundp 'org-up-heading-safe)
    (save-excursion
      (cl-loop collect (wlo--get-next-heading)
               while (org-up-heading-safe)))))

(defun wlo--build-segment ()
  "Build the segment from included segments."
  (org-with-wide-buffer
   (goto-char (window-start))
   (unless (org-before-first-heading-p)
     (let ((headings (wlo--collect-headings)))
       (if (null headings)
           ""
         (pcase wlo-include
           ('current (nth 0 headings))
           ('current-and-root
            (if (> (length headings) 1)
                (concat
                 (wlo--maybe-truncate (car (last headings)))
                 (wal-line--spacer)
                 (nth 0 headings))
              (nth 0 headings)))))))))

;; Segment:

(wal-line-create-dynamic-segment org
  :getter
  (let ((segment (wlo--build-segment)))
    (when segment
      (concat
       wlo-delimiter
       (wlo--build-segment))))
  :condition
  (eq major-mode 'org-mode)
  :dense t)

(provide 'wal-line-org)

;;; wal-line-org.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wlo-" . "wal-line-org-"))
;; End:
