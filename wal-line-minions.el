;;; wal-line-minions.el --- Minimize with minions -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.4.0
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

(defvar minor-modes-alist)
(defvar minions-mode-line-minor-modes-map)
(defvar minions-mode-line-lighter)

(wal-line-create-augment minions
  :action
  (lambda (&rest _args)
    (if (bound-and-true-p minions-mode)
        `((:propertize ("" ,(minions--prominent-modes))
                       face wal-line-shadow)
          ,(wal-line--spacer)
          (:propertize ,minions-mode-line-lighter
                       face wal-line-shadow
                       local-map ,minions-mode-line-minor-modes-map
                       mouse-face wal-line-highlight))
      minor-mode-alist))
  :advice
  (:after-while . (wal-line-minor-modes--segment)))

(provide 'wal-line-minions)

;;; wal-line-minions.el ends here
