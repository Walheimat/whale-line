;;; wal-line-buffer-name.el --- Buffer name segment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-line
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Show the buffer name.

;;; Code:

(require 'wal-line)

(wal-line-create-static-segment buffer-name
  :getter
  (buffer-name)
  :setup
  (lambda ()
    (add-hook 'find-file-hook #'wal-line-buffer-name--set-segment)
    (add-hook 'after-save-hook #'wal-line-buffer-name--set-segment)
    (add-hook 'clone-indirect-buffer-hook #'wal-line-buffer-name--set-segment)

    (advice-add #'not-modified :after #'wal-line-buffer-name--set-segment)
    (advice-add #'rename-buffer :after #'wal-line-buffer-name--set-segment)
    (advice-add #'set-visited-file-name :after #'wal-line-buffer-name--set-segment)
    (advice-add #'pop-to-buffer :after #'wal-line-buffer-name--set-segment)
    (advice-add #'undo :after #'wal-line-buffer-name--set-segment))
  :teardown
  (lambda ()
    (remove-hook 'find-file-hook #'wal-line-buffer-name--set-segment)
    (remove-hook 'after-save-hook #'wal-line-buffer-name--set-segment)
    (remove-hook 'clone-indirect-buffer-hook #'wal-line-buffer-name--set-segment)

    (advice-remove #'not-modified #'wal-line-buffer-name--set-segment)
    (advice-remove #'rename-buffer #'wal-line-buffer-name--set-segment)
    (advice-remove #'set-visited-file-name #'wal-line-buffer-name--set-segment)
    (advice-remove #'pop-to-buffer #'wal-line-buffer-name--set-segment)
    (advice-remove #'undo #'wal-line-buffer-name--set-segment)))

(wal-line-add-segment buffer-name)

(provide 'wal-line-buffer-name)

;;; wal-line-buffer-name.el ends here
