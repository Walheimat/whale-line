;;; wal-line-minions.el --- Minimize with minions -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Integration for minions.

;;; Code:

(require 'wal-line)

(declare-function wal-line--set-selected-window "wal-line.el")
(declare-function wal-line--is-current-window-p "wal-line.el")
(declare-function wal-line--spacer "wal-line.el")
(declare-function wal-line-minor-modes--segment "wal-line.el")
(declare-function minions--prominent-modes "ext:minions.el")

;;;; Functionality:

(defvar minor-modes-alist)
(defvar minions-mode-line-minor-modes-map)
(defvar minions-mode-line-lighter)
(defun wal-line-minions--advise-minor-modes (&rest _r)
  "Advise minor modes to use minions if present."
  (if (bound-and-true-p minions-mode)
      `((:propertize ("" ,(minions--prominent-modes))
                    face wal-line-shadow)
        ,(wal-line--spacer)
        (:propertize ,minions-mode-line-lighter
                     face wal-line-shadow
                     local-map ,minions-mode-line-minor-modes-map
                     mouse-face wal-line-highlight))
    minor-mode-alist))

;;;; Setup/teardown:

(defun wal-line-minions--setup ()
  "Set up minions."
  (advice-add
   #'wal-line-minor-modes--segment
   :after-while #'wal-line-minions--advise-minor-modes))

(defun wal-line-minions--teardown ()
  "Set up minions."
  (advice-remove
   #'wal-line-minor-modes--segment
   #'wal-line-minions--advise-minor-modes))

(add-hook 'wal-line-setup-hook #'wal-line-minions--setup)
(add-hook 'wal-line-teardown-hook #'wal-line-minions--teardown)

(provide 'wal-line-minions)

;;; wal-line-minions.el ends here
