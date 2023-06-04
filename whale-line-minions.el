;;; whale-line-minions.el --- Minimize with minions -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Integration for minions.

;;; Code:

(require 'whale-line)

(declare-function whale-line--set-selected-window "whale-line.el")
(declare-function whale-line--is-current-window-p "whale-line.el")
(declare-function whale-line--spacer "whale-line.el")
(declare-function whale-line-minor-modes--segment "whale-line.el")
(declare-function minions--prominent-modes "ext:minions.el")

(defvar minor-modes-alist)
(defvar minions-mode-line-minor-modes-map)
(defvar minions-mode-line-lighter)

(defun whale-line-minions--list (&rest _args)
  "Get the appropriate mode list."
  (if (bound-and-true-p minions-mode)
      `((:propertize ("" ,(minions--prominent-modes))
                     face whale-line-shadow)
        ,(whale-line--spacer)
        (:propertize ,minions-mode-line-lighter
                     face whale-line-shadow
                     local-map ,minions-mode-line-minor-modes-map
                     mouse-face whale-line-highlight))
    minor-mode-alist))

(whale-line-create-augment minions
  :action whale-line-minions--list
  :advice
  (:after-while . (whale-line-minor-modes--segment)))

(provide 'whale-line-minions)

;;; whale-line-minions.el ends here
