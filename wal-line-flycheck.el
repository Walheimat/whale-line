;;; wal-line-flycheck.el --- Indicate issues with flycheck -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Flycheck integration.

;;; Code:

(defvar-local wal-line-flycheck--face nil)

(declare-function flycheck-count-errors "ext:flycheck.el")
(declare-function wal-line-buffer-name--segment "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")

;;;; Functionality:

(defface wal-line-flycheck-running
  '((t (:underline (:style wave)
        :inherit (shadow)
        )))
  "Face used to indicate running state."
  :group 'wal-line)

(defvar flycheck-current-errors)
(defun wal-line-flycheck--update (&optional status)
  "Update face used for buffer name dependent on STATUS."
  (setq-local wal-line-flycheck--face
        (pcase status
          ('running 'wal-line-flycheck-running)
          ('finished
           (if flycheck-current-errors
               (let-alist (flycheck-count-errors flycheck-current-errors)
                 (cond
                  (.error 'flycheck-error)
                  (.warning 'flycheck-warning)
                  (.info 'flycheck-info)))
             nil))
          (_ nil))))

(defun wal-line-flycheck--advise-buffer-name (str)
  "Advise the buffer name STR."
  (concat (wal-line--spacer)
          (propertize (substring str 1) 'face (if wal-line-flycheck--face
                                                  wal-line-flycheck--face
                                                'wal-line-neutral))))

(defun wal-line-flycheck--setup ()
  "Set up flycheck integration."
  (add-hook 'flycheck-status-changed-functions #'wal-line-flycheck--update)
  (advice-add
   #'wal-line-buffer-name--segment
   :filter-return #'wal-line-flycheck--advise-buffer-name))

(defun wal-line-flycheck--teardown ()
  "Tear down flycheck integration."
  (remove-hook 'flycheck-status-changed-functions #'wal-line-flycheck--update)
  (advice-remove
   #'wal-line-buffer-name--segment
   #'wal-line-flycheck--advise-buffer-name))

(add-hook 'wal-line-setup-hook #'wal-line-flycheck--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-flycheck--teardown)

(provide 'wal-line-flycheck)

;;; wal-line-flycheck.el ends here
