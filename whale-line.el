;;; whale-line.el --- A whale-based mode-line -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.8.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; This package provides a highly modular mode-line. The `whale-line'
;; mode-line comprises a set of segments. Each of them can be freely
;; positioned or disabled by setting `whale-line-segments' (segments
;; belong either to the left or the right side). Many are
;; customizable.
;;
;; You can also create augments to change the behavior or look of an
;; existing segment.
;;
;; You may also create your own segments and augments using the
;; `whale-line-create-*' macros.
;;
;; This package takes inspiration from two other great custom
;; mode-lines: `mood-line' and `doom-modeline'.

;;; Code:

(require 'whale-line-core)
(require 'whale-line-segments)

(defvar whale-line--default-mode-line nil)

(declare-function whale-line--build-segments "whale-line-core.el")
(declare-function whale-line--set-selected-window "whale-line-core.el")

;;; -- Setup

(defun whale-line-mode--setup ()
  "Set up `whale-line-mode'."
  (unless (whale-line--build-segments)
    (user-error "Failed to build segments"))

  ;; Save a copy of the previous mode-line.
  (setq whale-line--default-mode-line mode-line-format)

  ;; Make setups do their thing.
  (run-hooks 'whale-line-setup-hook)
  (add-hook 'pre-redisplay-functions #'whale-line--set-selected-window)
  (add-hook 'window-configuration-change-hook #'whale-line--calculate-space)
  (add-hook 'buffer-list-update-hook #'whale-line--queue-refresh)

  ;; Set the new mode-line-format
  (setq-default mode-line-format '("%e" (:eval (whale-line--format)))))

(defun whale-line-mode--teardown ()
  "Tear down `whale-line-mode'."
  ;; Tear down everything.
  (run-hooks 'whale-line-teardown-hook)
  (remove-hook 'pre-redisplay-functions #'whale-line--set-selected-window)
  (remove-hook 'window-configuration-change-hook #'whale-line--calculate-space)
  (remove-hook 'buffer-list-update-hook #'whale-line--queue-refresh)

  ;; Restore the original mode-line format
  (setq-default mode-line-format whale-line--default-mode-line))

;;; -- API

(defun whale-line-rebuild ()
  "Rebuild the segments.

Only necessary if you have changed `whale-line-segments'."
  (interactive)

  (whale-line--build-segments))

(defun whale-line-trigger-augments (&optional arg)
  "Trigger augments.

Sets up augments (again). If ARG is t, tears them down instead."
  (interactive "P")

  (whale-line--trigger-augments arg))

(defun whale-line-pop-to-logs ()
  "Switch to the log buffer."
  (interactive)

  (let ((buffer (get-buffer whale-line--log-buffer-name)))

    (unless buffer
      (user-error "You need to set `harpoon-log' to t first"))

    (pop-to-buffer (get-buffer whale-line--log-buffer-name))))

;;;###autoload
(define-minor-mode whale-line-mode
  "Toggle `whale-line' on or off."
  :group 'whale-line
  :global t
  :lighter " wll"
  (if whale-line-mode
      (whale-line-mode--setup)
    (whale-line-mode--teardown)))

(provide 'whale-line)

;;; whale-line.el ends here
