;;; whale-line-segments.el --- Built-in segments -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.8.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; This package includes the definitions of all built-in segments as
;; well as their private logic. Note that their positioning is
;; determined by `whale-line-segments' which you can freely customize.
;;
;; This package uses `whale-line-segments-decorator' and
;; `whale-line-segments-decorates' to determine how to decorate
;; certain segments. The default decorator is provided by package
;; `whale-line-iconify' which uses icons.

;;; Code:

(require 'whale-line)

(declare-function whale-line-iconify "whale-line-iconify.el")
(declare-function whale-line-iconify-decorates-p "whale-line-iconify.el")

;;; -- Customization

(defgroup whale-line-segments nil
  "Settings for individual segments."
  :group 'whale-line
  :tag "Segments")

(defcustom whale-line-segments-decorator #'whale-line-iconify
  "The function to use to decorate segments (or their parts).

This function should return a mode-line construct or string."
  :group 'whale-line-segments
  :type 'function)

(defcustom whale-line-segments-decorates #'whale-line-iconify-decorates-p
  "The function used to check whether a named segment is decorated."
  :group 'whale-line-segments
  :type 'function)

(defcustom whale-line-segments-animation-key-frames ["(__.- >{"
                                                     "(__.' >{"
                                                     "(__.- >{"
                                                     "(__., >{"]
  "Animation key frames."
  :group 'whale-line-segments
  :type '(vector string))

(defcustom whale-line-segments-animation-speed 1.0
  "Animation speed.

Note that this value also determines how often
`force-mode-line-update' will be called to display the next
frame."
  :group 'whale-line-segments
  :type 'float)

(defcustom whale-line-segments-org-separator ">"
  "The separator between headings."
  :group 'whale-line-segments
  :type 'string)

(defcustom whale-line-segments-org-ellipsis "…"
  "The string indicating truncation of a heading."
  :group 'whale-line-segments
  :type 'string)

(defcustom whale-line-segments-org-elision "*"
  "The string indicating elision of a heading."
  :group 'whale-line-segments
  :type 'string)

(defcustom whale-line-segments-org-max-count 2
  "The amount of headings to show.

If there are more headings, only the leading n will be shown,
others elided. This means the heading has depth 4 and this is set
to 2, only the 3rd level is elided."
  :group 'whale-line-segments
  :type 'integer)

;;; -- Utility

(defun whale-line-segments--decorate (symbol &rest args)
  "Get the decoration for SYMBOL passing ARGS.

See `whale-line-segments-decorator'."
  (when (fboundp whale-line-segments-decorator)
    (apply whale-line-segments-decorator (append (list symbol) args))))

(defun whale-line-segments--decorates-p (symbol)
  "Check if SYMBOL is decorated."
  (when (fboundp whale-line-segments-decorates)
    (funcall whale-line-segments-decorates symbol)))

;;; -- Segments

;;;; -- Buffer identification

(defvar whale-line-segments-buffer-identification-hook nil
  "Hook run to make `buffer-identification' segment react.")

(defvar-local whale-line-segments--buffer-identification--additional-face nil)
(defvar-local whale-line-segments--buffer-identification--additional-help nil)

(defun whale-line-segments--buffer-identification--set-additional (face help &rest _)
  "Set additional HELP and FACE."
  (setq whale-line-segments--buffer-identification--additional-face face
        whale-line-segments--buffer-identification--additional-help help)

  (run-hooks 'whale-line-segments-buffer-identification-hook))

(defun whale-line-segments--buffer-identification ()
  "Get the buffer identification."
  `((:propertize (:eval (propertized-buffer-identification "%b"))
                 face ,(list 'mode-line-buffer-id whale-line-segments--buffer-identification--additional-face)
                 ,@(and whale-line-segments--buffer-identification--additional-help
                        (list 'help-echo whale-line-segments--buffer-identification--additional-help)))))

(whale-line-create-stateful-segment buffer-identification
  :getter whale-line-segments--buffer-identification
  :hooks (find-file-hook after-save-hook clone-indirect-buffer-hook kill-buffer-hook whale-line-segments-buffer-identification-hook)
  :after (not-modified rename-buffer set-visited-file-name pop-to-buffer undo)
  :port whale-line-segments--buffer-identification--set-additional)

;;;; -- Buffer status

(defun whale-line-segments--buffer-status ()
  "Render buffer status."
  (if buffer-read-only
      (whale-line-segments--buffer-status--read-only)
    (whale-line-segments--buffer-status--writable)))

(defun whale-line-segments--buffer-status--writable ()
  "Buffer status for a writable buffer."
  (if (whale-line-segments--decorates-p 'buffer-status)
      (if (buffer-modified-p)
          (whale-line-segments--decorate 'buffer-modified (if buffer-file-name
                                                              'whale-line-emphasis
                                                            'whale-line-shadow))
        (unless buffer-file-name
          (whale-line-segments--decorate 'buffer-file-name)))
    (concat
     (unless buffer-file-name
       (propertize "&" 'face 'whale-line-shadow))
     (when (buffer-modified-p)
       (propertize "*" 'face 'whale-line-emphasis)))))

(defun whale-line-segments--buffer-status--read-only ()
  "Buffer status for a read-only buffer."
  (or (whale-line-segments--decorate 'buffer-read-only)
      (propertize "@" 'face 'whale-line-contrast)))

(whale-line-create-stateless-segment buffer-status
  :getter whale-line-segments--buffer-status)

;;;; -- Window status

(defun whale-line-segments--window-status ()
  "Render window status segment."
  (delq nil
        (list
         (and (window-parameter (selected-window) 'no-other-window)
              (or (whale-line-segments--decorate 'window-no-other)
                  (propertize "~" 'face 'whale-line-shadow)))
         (and (window-dedicated-p)
              (or (whale-line-segments--decorate 'window-dedicated)
                  (propertize "^" 'face 'whale-line-shadow))))))

(whale-line-create-stateless-segment window-status
  :getter whale-line-segments--window-status)

;;;; -- Position

(defvar whale-line-segments--position
  '((pdf-view--server-file-name
     ((:propertize (:eval (whale-line-segments--position--default))
                   face whale-line-shadow))
     (doc-view--buffer-file-name
      ((:propertize (:eval (whale-line-segments--position--default))
                    face whale-line-shadow))
      ((:propertize ("" mode-line-percent-position)
                    face whale-line-shadow)
       (:eval (whale-line--spacer))
       (:propertize (:eval (whale-line-segments--position--line-and-column)) face whale-line-shadow))))))

(defun whale-line-segments--position--line-and-column ()
  "Get the line and column."
  (when-let* ((format (car-safe mode-line-position-column-line-format)))
    (string-trim format)))

(defun whale-line-segments--position--default ()
  "Format the default `mode-line-position' construct."
  (string-trim
   (string-replace
    "%" "%%"
    (format-mode-line mode-line-position))))

(whale-line-create-stateless-segment position
  :var whale-line-segments--position
  :priority current)

;;;; -- Misc info

(whale-line-create-stateless-segment misc-info
  :var mode-line-misc-info
  :priority current-low)

;;;; -- Minor modes

(whale-line-create-stateless-segment minor-modes
  :var minor-mode-alist
  :padded left
  :priority current)

;;;; -- Process

(whale-line-create-stateless-segment process
  :var mode-line-process
  :priority low)

;;;; -- Client

(defvar whale-line-segments--client
  '((:eval (when (frame-parameter nil 'client)
             (if (whale-line-segments--decorates-p 'client)
                 (propertize (whale-line-segments--decorate 'client)'help-echo "Client frame")
               mode-line-client))))
  "Segment for client.")

(whale-line-create-stateless-segment client
  :var whale-line-segments--client
  :priority current-low)

;;;; -- Selection

(defvar whale-line-segments--selection
  '((mark-active
     (:propertize
      ((:eval (whale-line--spacer))
       (multiple-cursors-mode
        "mc"
        (rectangle-mark-mode
         ((:eval (whale-line-segments--selection--rows)) "×" (:eval (whale-line-segments--selection--columns)))
         (:eval (whale-line-segments--selection--rows))))
       (:eval (whale-line--spacer)))
      face region))))

(defun whale-line-segments--selection--columns ()
  "Get the columns for the region."
  (let ((beg (region-beginning))
        (end (region-end)))

    (number-to-string (abs (- (save-excursion (goto-char end)
                                              (current-column))
                              (save-excursion (goto-char beg)
                                              (current-column)))))))

(defun whale-line-segments--selection--rows ()
  "Get the rows for the region.."
  (let ((beg (region-beginning))
        (end (region-end)))

    (number-to-string (count-lines beg (min end (point-max))))))

(whale-line-create-stateless-segment selection
  :var whale-line-segments--selection
  :priority current
  :dense (lambda () (not mark-active)))

;;;; -- Animation

(defvar whale-line-segments--animation-frame-index 0)
(defvar whale-line-segments--animation-timer nil)
(defvar whale-line-segments--animation-frame nil)

(defvar whale-line-segments--animation
  `((whale-line-segments--animation-frame
     (:propertize whale-line-segments--animation-frame face whale-line-emphasis))))

(defun whale-line-segments--animation-animate ()
  "Animate.

This will increase the frame index and set the current frame.
Afterwards a mode-line update is forced to display the new frame."
  (let* ((frame (aref whale-line-segments-animation-key-frames whale-line-segments--animation-frame-index)))

    (setq whale-line-segments--animation-frame-index
          (mod
           (1+ whale-line-segments--animation-frame-index)
           (length whale-line-segments-animation-key-frames))
          whale-line-segments--animation-frame frame)

    (force-mode-line-update)))

(defun whale-line-segments--animation-start-timer ()
  "Set up the animation timer."
  (unless whale-line-segments--animation-timer
    (setq whale-line-segments--animation-timer (run-with-timer 0 whale-line-segments-animation-speed #'whale-line-segments--animation-animate))))

(defun whale-line-segments--animation-stop-timer ()
  "Stop the animation timer."
  (when whale-line-segments--animation-timer
    (cancel-timer whale-line-segments--animation-timer)
    (setq whale-line-segments--animation-timer nil)))

(whale-line-create-stateless-segment animation
  :var whale-line-segments--animation
  :setup whale-line-segments--animation-start-timer
  :teardown whale-line-segments--animation-stop-timer
  :priority current-low)

;;;; -- Flycheck

(declare-function flycheck-count-errors "ext:flycheck.el")

(defface whale-line-segments--syntax-checker-running
  '((t (:underline (:style wave)
                   :inherit (shadow))))
  "Face used to indicate running state.")

(defvar flycheck-current-errors)

(defun whale-line-segments--flycheck--face (status)
  "Get the face to use for STATUS."
  (pcase status
    ('not-checked nil)
    ('running 'whale-line-segments--syntax-checker-running)
    ('finished
     (when flycheck-current-errors
       (let-alist (flycheck-count-errors flycheck-current-errors)
         (cond
          (.error 'flycheck-error)
          (.warning 'flycheck-warning)
          (.info 'flycheck-info)))))
    (_ nil)))

(defvar whale-line-segments--flycheck--default-help "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer")

(defun whale-line-segments--flycheck--help (status)
  "Get the error count for STATUS.

Returns nil if not checking or if no errors were found."
  (concat
   whale-line-segments--flycheck--default-help
   (pcase status
     ('running "\n\nFlycheck: Still checking")
     ('finished
      (when flycheck-current-errors
        (let-alist (flycheck-count-errors flycheck-current-errors)
          (format "\n\nFlycheck: %d error(s), %d warning(s), %d info(s)" (or .error 0) (or .warning 0) (or .info 0)))))
     (_ nil))))

(defun whale-line-segments--flycheck (status)
  "Set face and help based on STATUS."
  (list (whale-line-segments--flycheck--face status) (whale-line-segments--flycheck--help status)))

(defun whale-line-segments--flycheck--can-use-flycheck-p ()
  "Verify that flycheck augment can be used."
  (require 'flycheck nil t))

(whale-line-create-augment flycheck
  :verify whale-line-segments--flycheck--can-use-flycheck-p
  :action whale-line-segments--flycheck
  :plugs-into buffer-identification
  :hooks (flycheck-status-changed-functions))

;;;; -- Flymake segment

(declare-function flymake--diag-type "ext:flymake.el")
(declare-function flymake-running-backends "ext:flymake.el")
(declare-function flymake-reporting-backends "ext:flymake.el")

(defvar flymake--state)

(defun whale-line-segments--flymake--count-types (diagnostics)
  "Count all types found in DIAGNOSTICS."
  (let ((errors 0)
        (warnings 0)
        (notes 0))

    (dolist (diag diagnostics)
      (pcase (flymake--diag-type diag)
        (:note
         (setq notes (1+ notes)))
        (:warning
         (setq warnings (1+ warnings)))
        (:error
         (setq errors (1+ errors)))))

    (list :errors errors :warnings warnings :notes notes)))

(defun whale-line-segments--flymake--face (counts)
  "Get the appropriate face for COUNTS."
  (cond
   ((> (plist-get counts :errors) 0)
    'flymake-error)
   ((> (plist-get counts :warnings) 0)
    'flymake-warning)
   ((> (plist-get counts :notes) 0)
    'flymake-note)
   (t nil)))

(defvar whale-line-segments--flymake--default-help "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer")

(defun whale-line-segments--flymake--help (counts)
  "Get the help text for COUNTS."
  (concat
   whale-line-segments--flymake--default-help
   (format "\n\nFlymake: %d error(s), %d warning(s), %d note(s)"
           (plist-get counts :errors)
           (plist-get counts :warnings)
           (plist-get counts :notes))))

(defun whale-line-segments--flymake (&rest _r)
  "Augment the buffer identification."
  (pcase-let* ((`(,face ,help)
                (cond
                 ((zerop (hash-table-count flymake--state))

                  `(nil ,(concat whale-line-segments--flymake--default-help
                                 "\n\nFlymake: No backends")))

                 ((cl-set-difference
                   (flymake-running-backends)
                   (flymake-reporting-backends))

                  `(whale-line-segments--syntax-checker-running ,(concat whale-line-segments--flymake--default-help
                                                                         "\n\nFlymake: Running")))

                 (t
                  (let* ((diagnostics (flymake-diagnostics))
                         (counts (whale-line-segments--flymake--count-types diagnostics)))

                    (list (whale-line-segments--flymake--face counts)
                          (whale-line-segments--flymake--help counts)))))))

    (list face help)))

(whale-line-create-augment flymake
  :action whale-line-segments--flymake
  :plugs-into buffer-identification
  :after flymake--mode-line-exception)

;;;; -- Major mode

(defun whale-line-segments--major-mode--decorated ()
  "Get the decoration for the `major-mode'."
  (and-let* ((decorated (whale-line-segments--decorate 'major-mode))
             (decorated (if (or (null decorated) (symbolp decorated))
                            '(:eval (whale-line-segments--decorate 'buffer-fallback))
                          decorated)))

    `((:propertize ,decorated
                   help-echo ,(format "%s" (format-mode-line mode-name))
                   display (raise -0.135)))))

(defun whale-line-segments--major-mode--text ()
  "Get the text for the `major-mode'."
  `((:propertize (" " mode-name " ") face whale-line-highlight)))

(defun whale-line-segments--major-mode ()
  "Get the `major-mode' segment."
  (or (whale-line-segments--major-mode--decorated) (whale-line-segments--major-mode--text)))

(whale-line-create-stateful-segment major-mode
  :hooks
  (find-file-hook after-change-major-mode-hook clone-indirect-buffer-hook)
  :getter whale-line-segments--major-mode)

;;; -- LSP

(declare-function lsp-workspaces "ext:lsp-mode.el")
(declare-function lsp--workspace-print "ext:lsp-mode.el")
(declare-function eglot-current-server "ext:eglot.el")
(declare-function eglot-project-nickname "ext:eglot.el")
(declare-function eglot--language-id "ext:eglot.el")

(defun whale-line-segments--lsp--uses-lsp-mode-p ()
  "Check if buffer is manged by `lsp-mode'."
  (and (featurep 'lsp-mode)
       (bound-and-true-p lsp-mode)
       (lsp-workspaces)))

(defun whale-line-segments--lsp--uses-eglot-p ()
  "Check if buffer is manged by `eglot'."
  (and (featurep 'eglot)
       (bound-and-true-p eglot--managed-mode)))

(defun whale-line-segments--lsp--active-p ()
  "Check if an LSP mode is active."
  (or (whale-line-segments--lsp--uses-lsp-mode-p)
      (whale-line-segments--lsp--uses-eglot-p)))

(defun whale-line-segments--lsp--help ()
  "Get the appropriate help text."
  (cond
   ((whale-line-segments--lsp--uses-lsp-mode-p)
    (concat
     "Connected to "
     (mapconcat #'lsp--workspace-print (lsp-workspaces) "|")))
   ((whale-line-segments--lsp--uses-eglot-p)
    (let ((server (eglot-current-server)))
      (concat
       "Connected to "
       (eglot--language-id server)
       "::"
       (eglot-project-nickname server))))))

(defun whale-line-segments--lsp--with-count ()
  "Get the count of connected servers."
  (if-let* ((decorated (or (whale-line-segments--decorate 'lsp)
                           "LSP"))
            ((whale-line-segments--lsp--uses-lsp-mode-p))
            (count (length (lsp-workspaces)))
            ((> count 1)))

      (list decorated
            (whale-line--spacer)
            (propertize (number-to-string count) 'face 'whale-line-shadow))
    decorated))

(defun whale-line-segments--lsp (&rest _args)
  "Indicate an active LSP session."
  (and-let* (((whale-line-segments--lsp--active-p))
             (help (whale-line-segments--lsp--help)))

    `((:propertize (:eval (whale-line-segments--lsp--with-count)) help-echo ,help))))

(whale-line-create-stateful-segment lsp
  :getter whale-line-segments--lsp
  :hooks
  (lsp-after-initialize-hook
   lsp-after-uninitialized-functions
   lsp-after-open-hook
   eglot-server-initialized-hook
   eglot-managed-mode-hook))

;;; -- Debugging

(declare-function dap--cur-session "ext:dap-mode.el")
(declare-function dap--debug-session-name "ext:dap-mode.el")
(declare-function dap--session-running "ext:dap-mode.el")

(defun whale-line-segments--debug--active-p ()
  "Check whether debugging is in process."
  (and-let* (((featurep 'dap-mode))
             ((bound-and-true-p dap-mode))
             (session (dap--cur-session))
             ((dap--session-running session)))))

(defun whale-line-segments--debug--name ()
  "Get the name of the session."
  (dap--debug-session-name (dap--cur-session)))

(defun whale-line-segments--debug ()
  "Indicate an active debugging session."
  (when-let* (((whale-line-segments--debug--active-p))
              (name (whale-line-segments--debug--name))
              (help (format "Debugging %s" name)))

    `((:propertize ,(or (whale-line-segments--decorate 'debug) "BUG") help-echo ,help))))

(whale-line-create-stateful-segment debug
  :getter whale-line-segments--debug
  :hooks
  (dap-session-created-hook
   dap-session-changed-hook
   dap-terminated-hook))

;;; -- Minions

(declare-function minions--prominent-modes "ext:minions.el")

(defvar minor-modes-alist)
(defvar minions-mode-line-minor-modes-map)
(defvar minions-mode-line-lighter)

(defun whale-line-segments--minions--list (&rest _args)
  "Get the appropriate mode list."
  (if (bound-and-true-p minions-mode)
      `((:eval (minions--prominent-modes))
        ,(whale-line--spacer)
        (:propertize (:eval minions-mode-line-lighter)
                     face whale-line-shadow
                     local-map ,minions-mode-line-minor-modes-map
                     mouse-face whale-line-highlight))
    minor-mode-alist))

(whale-line-create-augment minions
  :action whale-line-segments--minions--list
  :after-while whale-line-minor-modes--segment)

;;; -- Org

(eval-when-compile
  (require 'org))

(declare-function org-back-to-heading "ext:org.el")
(declare-function org-before-first-heading-p "ext:org.el")
(declare-function org-heading-components "ext:org.el")
(declare-function org-link-display-format "ext:org.el")
(declare-function org-up-heading-safe "ext:org.el")

(defvar whale-line-segments--org--min-length 4)
(defvar whale-line-segments--org--max-length 24)

(defun whale-line-segments--org--maybe-truncate (heading face max-len)
  "Maybe truncate HEADING depending on MAX-LEN.

Use FACE for the ellipsis glyph."
  (let ((len (string-width heading))
        (ellipsis-len (string-width whale-line-segments-org-ellipsis)))

    (if (> len max-len)
        (concat (substring heading 0 (max (- max-len ellipsis-len) 1))
                (propertize whale-line-segments-org-ellipsis 'face face))
      heading)))

(defun whale-line-segments--org--get-next-heading ()
  "Get the next heading going backwards."
  (org-back-to-heading)
  (let* ((components (org-heading-components))
         (level (nth 0 components))
         (face (nth (- level 1) org-level-faces))
         (heading (org-link-display-format (nth 4 components))))
    (propertize heading 'face face)))

(defun whale-line-segments--org--max-length ()
  "Check if we're in a low space environment."
  (let* ((space (max 1 (whale-line--space)))
         (width (/ space (window-font-width))))

    (if (> width (* whale-line-segments-org-max-count whale-line-segments--org--max-length))
        whale-line-segments--org--max-length
      (min whale-line-segments--org--max-length
           (max whale-line-segments--org--min-length (/ width whale-line-segments-org-max-count))))))

(defun whale-line-segments--org--collect-headings ()
  "Collect headings until it's no longer safe."
  (save-excursion
    (cl-loop collect (whale-line-segments--org--get-next-heading)
             while (org-up-heading-safe))))

(defun whale-line-segments--org ()
  "Build the segment from included segments."
  (org-with-wide-buffer

   (goto-char (window-start))

   (and-let* (((not (org-before-first-heading-p)))
              (headings (whale-line-segments--org--collect-headings))
              (count 0)
              (max-len (whale-line-segments--org--max-length)))

     (if (> (length (nth 0 headings)) (* max-len (length headings)))
         (nth 0 headings)
       (mapconcat
        #'identity
        (seq-map-indexed
         (lambda (it i)
           (if (eq (1- (length headings)) i)
               (progn
                 (setq count (1+ count))
                 it)
             (if (>= count whale-line-segments-org-max-count)
                 (propertize whale-line-segments-org-elision 'face (nth i org-level-faces))
               (setq count (1+ count))
               (whale-line-segments--org--maybe-truncate it (nth i org-level-faces) max-len))))
         (reverse headings))
        (propertize whale-line-segments-org-separator 'face 'whale-line-shadow))))))

(whale-line-create-stateless-segment org
  :getter whale-line-segments--org
  :condition (derived-mode-p 'org-mode)
  :priority current)

;;; -- Project

(declare-function project-name "ext:project.el")
(declare-function project-root "ext:project.el")

(defun whale-line-segments--project--display-for-buffer-p ()
  "Check if current buffer should show project information.

Only consider Dired buffers and file buffers."
  (with-current-buffer (current-buffer)
    (or (derived-mode-p 'dired-mode)
        (buffer-file-name))))

(defvar whale-line-segments--project--regexp ".+\\(\\/.+\\)\\/$")

(defvar whale-line-segments--project--map
  (let ((map (make-sparse-keymap)))

    (define-key map [mode-line mouse-1] 'project-dired)

    map))

(defun whale-line-segments--project--help ()
  "Get the help text for the project."
  (when-let* ((project (project-current))
              (root (project-root project)))

    (format "Project (%s)\nmouse-1: Open root" root)))

(defun whale-line-segments--project ()
  "Get the project segment."
  (when-let* ((candidate (whale-line-segments--project--display-for-buffer-p))
              (project (project-current))
              (name (project-name project))
              (help (whale-line-segments--project--help)))

    `(,@(when-let ((decorated (whale-line-segments--decorate 'project)))
          (list decorated (whale-line--spacer)))
      (:propertize ,name
                   face whale-line-emphasis
                   mouse-face whale-line-highlight
                   help-echo ,help
                   local-map ,whale-line-segments--project--map))))

(whale-line-create-stateful-segment project
  :getter whale-line-segments--project
  :hooks (find-file-hook)
  :priority low)

;;; --- Tab bar

(defun whale-line-segments--tab-bar--identifier ()
  "Get the identifier of the current tab.

This is either an explicit name or its index."
  (if-let* ((tab (tab-bar--current-tab))
            ((alist-get 'explicit-name tab))
            (name (alist-get 'name tab)))
      name
    (number-to-string (tab-bar--current-tab-index))))

(defun whale-line-segments--tab-bar ()
  "Get the name or number of the tab."
  (and-let* (((bound-and-true-p tab-bar-mode))
             (id (whale-line-segments--tab-bar--identifier)))

    `((:propertize ,(format " %s " id) face whale-line-highlight))))

(whale-line-create-stateful-segment tab-bar
  :getter whale-line-segments--tab-bar
  :hooks (window-configuration-change-hook)
  :priority current)

;;; -- VC

(declare-function vc-git-root "ext:vc-git.el")

(defvar whale-line-segments--vc--scope-regexp "\\(feature\\|\\(\\w+\\)?fix\\|improvement\\)\\/")

(defvar whale-line-segments--vc--state-specs '((up-to-date . (whale-line-shadow "."))
                                               (edited . (whale-line-contrast "*"))
                                               (needs-update . (whale-line-contrast "&"))
                                               (needs-merge . (whale-line-urgent "&"))
                                               (unlocked-changes . (whale-line-urgent "!"))
                                               (added . (whale-line-emphasis "+"))
                                               (removed . (whale-line-emphasis "-"))
                                               (conflict . (whale-line-urgent "!"))
                                               (missing . (whale-line-contrast "?"))
                                               (ignored . (whale-line-shadow "!"))
                                               (unregistered . (whale-line-shadow "?"))))

(defun whale-line-segments--vc--specs-for-state (state)
  "Get the correct specs for STATE."
  (alist-get state whale-line-segments--vc--state-specs '(whale-line-shadow ".")))

(defun whale-line-segments--vc ()
  "Get version control info."
  (and-let* (((buffer-file-name))
             (info (or (whale-line-segments--vc-registered--info) (whale-line-segments--vc-unregistered--info))))

    `(,@(when-let ((decorated (whale-line-segments--decorate 'vc)))
          (list decorated (whale-line--spacer)))
      ,info)))

;;;; -- Registered

(defun whale-line-segments--vc-registered--info ()
  "Get info for registered files."
  (and-let* (((and vc-mode buffer-file-name))
             (backend (vc-backend buffer-file-name))
             (status (and vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
             (state (vc-state buffer-file-name backend))
             (specs (whale-line-segments--vc--specs-for-state state)))

    `((:propertize ,(replace-regexp-in-string whale-line-segments--vc--scope-regexp "" status)
                   mouse-face whale-line-highlight)
      (:propertize ,(nth 1 specs) face ,(nth 0 specs)))))

;;;; -- Unregistered

(defun whale-line-segments--vc-unregistered--git-p (file)
  "Check whether the unregistered FILE is in a git repository."
  (require 'vc-git)
  (vc-git-root file))

(defun whale-line-segments--vc-unregistered--info ()
  "Get info for unregistered files."
  (when-let ((file (buffer-file-name)))

    (cond
     ((whale-line-segments--vc-unregistered--git-p file)
      (when-let* ((state (vc-state file 'Git))
                  (help (format "File state: %s" state))
                  (specs (whale-line-segments--vc--specs-for-state state)))

        `((:propertize "Git" help-echo ,help)
          (:propertize ,(nth 1 specs) face ,(nth 0 specs))))))))

(whale-line-create-stateful-segment vc
  :getter whale-line-segments--vc
  :hooks (find-file-hook after-save-hook)
  :after vc-refresh-state
  :priority current)

(provide 'whale-line-segments)

;;; whale-line-segments.el ends here
