;;; whale-line-segments.el -- Default segments. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.7.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Definitions of all mode-line segments.

;;; Code:

(require 'whale-line-core)
(require 'whale-line-iconify)

(declare-function all-the-icons-icon-for-buffer "ext:all-the-icons.el")
(declare-function whale-line-iconify "whale-line-iconify.el")

;;; -- Customization

(defgroup whale-line-segments nil
  "Settings for individual segments."
  :group 'whale-line
  :tag "Segments")

(defcustom whale-line-segments-animation-key-frames ["(__.- >{"
                                                     "(__.' >{"
                                                     "(__.- >{"
                                                     "(__., >{"]
  "Animation key frames."
  :group 'whale-line-segments
  :type '(vector string))

(defcustom whale-line-segments-animation-speed 1.0
  "Animation speed."
  :group 'whale-line-segments
  :type 'float)

(defcustom whale-line-segments-org-separator ">"
  "The separator between headings."
  :group 'whale-line-segments
  :type 'string)

(defcustom whale-line-segments-org-ellipsis "…"
  "The string indicating truncation."
  :group 'whale-line-segments
  :type 'string)

(defcustom whale-line-segments-org-elision "*"
  "The string indicating elision."
  :group 'whale-line-segments
  :type :string)

(defcustom whale-line-segments-org-max-count 2
  "The amount of headings to show.

If there are more headings, only the leading n will be shown,
others elided. This means the heading has depth 4 and this is set
to 2, only the 3rd level is elided."
  :group 'whale-line-segments
  :type 'integer)

(defcustom whale-line-segments-org-max-heading-length 12
  "The max length of a heading before truncation."
  :group 'whale-line-segments
  :type 'integer)

(defconst whale-line-icon-type
  '(cons symbol
         (restricted-sexp
          :match-alternatives
          (stringp listp)))
  "Icon type as a cons cell of the icon library and the icons specs.
The specs are either a string of the icon name or a list of the
icon name and the face.")

;;; -- Segments

(declare-function image-mode-window-get "ext:image-mode.el")
(declare-function doc-view-last-page-number "ext:doc-view.el")

;;;; -- Buffer identification

(defun wls--buffer-identification ()
  "Get the buffer name."
  '((:propertize (:eval (propertized-buffer-identification "%b"))
                 mouse-face whale-line-highlight
                 face whale-line-neutral)))

(whale-line-create-stateful-segment buffer-identification
  :getter wls--buffer-identification
  :hooks (find-file-hook after-save-hook clone-indirect-buffer-hook kill-buffer-hook)
  :advice (:after . (not-modified rename-buffer set-visited-file-name pop-to-buffer undo)))

;;;; -- Buffer status

(defun wls--buffer-status ()
  "Render buffer status segment."
  (let ((render (cond
                 (buffer-read-only
                  (whale-line-iconify 'buffer-read-only))
                 ((not (buffer-file-name))
                  (whale-line-iconify 'buffer-file-name))
                 ((buffer-modified-p)
                  (whale-line-iconify 'buffer-modified))
                 (t nil))))

    render))

(defun wls--buffer-status--dense-p ()
  "Check whether the segment should be dense."
  (not (whale-line-iconify--use-for-p 'buffer-status)))

(whale-line-create-stateless-segment buffer-status
  :getter wls--buffer-status
  :dense wls--buffer-status--dense-p)

;;;; -- Window status

(defun wls--window-status ()
  "Render window status segment."
  (when (window-dedicated-p)
    (whale-line-iconify 'window-dedicated)))

(whale-line-create-stateless-segment window-status
  :getter wls--window-status
  :priority low)

;;;; -- Position

(defun wls--position ()
  "Render position segment."
  (let ((str (cond
              ((eq major-mode 'doc-view-mode)
               (concat
                (number-to-string (image-mode-window-get 'page))
                "/"
                (number-to-string (doc-view-last-page-number))))

              ((bound-and-true-p follow-mode)
               "f: %l:%c %p%")

              (t "%l:%c %p%"))))
    `((:propertize ,str face whale-line-shadow))))

(whale-line-create-stateless-segment position
  :getter wls--position
  :priority current)

;;;; -- Misc info

(defun wls--misc-info ()
  "Render the misc info segment."
  mode-line-misc-info)

(whale-line-create-stateless-segment misc-info
  :condition mode-line-misc-info
  :getter wls--misc-info
  :priority current-low)

;;;; -- Minor modes

(whale-line-create-stateless-segment minor-modes
  :getter (lambda () minor-mode-alist)
  :priority low)

;;;; -- Process

(defun wls--process ()
  "Get process segment."
  mode-line-process)

(whale-line-create-stateless-segment process
  :getter wls--process
  :condition mode-line-process
  :priority current)

;;;; -- Selection

(defun whale-line-selection--get-columns (beg end)
  "Get the columns from BEG to END for displaying `rectangle-mode'."
  (abs (- (save-excursion (goto-char end)
                          (current-column))
          (save-excursion (goto-char beg)
                          (current-column)))))

(defun wls--selection ()
  "Show the selection."
  (let* ((beg (region-beginning))
         (end (region-end))
         (lines (count-lines beg (min end (point-max)))))

    `((:propertize ,(if (bound-and-true-p rectangle-mark-mode)
                        (let ((columns (whale-line-selection--get-columns beg end)))
                          (format " %dx%d " lines columns))
                      (format " %d " lines))
                   face region))))

(whale-line-create-stateless-segment selection
  :condition mark-active
  :getter wls--selection
  :priority current-low)

;;;; -- Animation

(defvar wls--animation-frame-index 0)
(defvar wls--animation-timer nil)
(defvar wls--animation-frame nil)

(defun wls--animation-animate ()
  "Animate.

This will increase the frame index and set the current frame.
Afterwards a mode-line update is forced to display the new frame."
  (let* ((frame (aref wls-animation-key-frames wls--animation-frame-index)))

    (setq wls--animation-frame-index
          (mod
           (1+ wls--animation-frame-index)
           (length wls-animation-key-frames))
          wls--animation-frame frame)

    (force-mode-line-update)))

(defun wls--animation-segment ()
  "Get the animation segment."
  (when-let ((frame wls--animation-frame))
    `((:propertize ,frame face whale-line-emphasis))))

(defun wls--animation-start-timer ()
  "Set up the animation timer."
  (unless wls--animation-timer
    (setq wls--animation-timer (run-with-timer 0 wls-animation-speed #'wls--animation-animate))))

(defun wls--animation-stop-timer ()
  "Stop the animation timer."
  (when wls--animation-timer
    (cancel-timer wls--animation-timer)
    (setq wls--animation-timer nil)))

(whale-line-create-stateless-segment animation
  :getter wls--animation-segment

  :setup wls--animation-start-timer

  :teardown wls--animation-stop-timer

  :priority current-low)

;;;; -- Cursors

(declare-function mc/num-cursors "ext:multiple-cursors.el")
(declare-function iedit-counter "ext:iedit.el")

(defun wls--cursors--count ()
  "Get the cursor count."
  (let* ((mc-cursors (when (bound-and-true-p multiple-cursors-mode)
                       (mc/num-cursors)))
         (iedit-cursors (when (bound-and-true-p iedit-mode)
                          (iedit-counter)))
         (cursors (or mc-cursors iedit-cursors)))

    (when cursors
      `((:propertize ,(format " %d " cursors) face whale-line-highlight)))))

(whale-line-create-stateless-segment cursors
  :getter wls--cursors--count

  :condition
  (or (bound-and-true-p multiple-cursors-mode)
      (bound-and-true-p iedit-mode))

  :priority current)

;;;; -- Flycheck

(declare-function flycheck-count-errors "ext:flycheck.el")

(defface wls--flycheck-running
  '((t (:underline (:style wave)
                   :inherit (shadow))))
  "Face used to indicate running state.")

(defvar flycheck-current-errors)
(defun wls--flycheck--get-face-for-status (status)
  "Get the face to use for STATUS."
  (pcase status
    ('running 'wls--flycheck-running)
    ('finished
     (if flycheck-current-errors
         (let-alist (flycheck-count-errors flycheck-current-errors)
           (cond
            (.error 'flycheck-error)
            (.warning 'flycheck-warning)
            (.info 'flycheck-info)))
       'whale-line-neutral))
    (_ 'whale-line-neutral)))

(defun wls--flycheck--get-error-help (status)
  "Get the error count for STATUS.

Returns nil if not checking or if no errors were found."
  (pcase status
    ('running "Still checking")
    ('finished
     (when flycheck-current-errors
       (let-alist (flycheck-count-errors flycheck-current-errors)
         (format "Errors: %s, warnings: %s, infos: %s" (or .error 0) (or .warning 0) (or .info 0)))))
    (_ nil)))

(defun wls--flycheck--underline (status &rest _r)
  "Underline buffer name based on STATUS."
  (let ((face (wls--flycheck--get-face-for-status status))
        (text (wls--flycheck--get-error-help status)))

	(setq-local whale-line-buffer-identification--segment
                (if text
					`((:propertize (:eval (propertized-buffer-identification "%b"))
                                   face ,face help-echo ,text))
				  `((:propertize (:eval (propertized-buffer-identification "%b"))
                                 face ,face))))))

(defun wls--flycheck--can-use-flycheck-p ()
  "Verify that flycheck augment can be used."
  (require 'flycheck nil t))

(whale-line-create-augment flycheck
  :verify wls--flycheck--can-use-flycheck-p

  :action wls--flycheck--underline

  :hooks
  (flycheck-status-changed-functions))

;;;; -- Buffer icon

(defun wls--buffer-icon ()
  "Get the buffer icon segment."
  (let ((icon (all-the-icons-icon-for-buffer)))

    `((:propertize ,(if (or (null icon) (symbolp icon))
                        '(:eval (whale-line-iconify 'buffer-fallback))
                      icon)
                   help-echo ,(format "%s" (format-mode-line mode-name))
                   display (raise -0.135)))))

(whale-line-create-stateful-segment buffer-icon
  :verify whale-line-iconify--can-use-p

  :hooks
  (find-file-hook after-change-major-mode-hook clone-indirect-buffer-hook)

  :getter wls--buffer-icon)

;;; -- LSP

(declare-function lsp-workspaces "ext:lsp-mode.el")
(declare-function lsp--workspace-print "ext:lsp-mode.el")
(declare-function eglot-current-server "ext:eglot.el")
(declare-function eglot-project-nickname "ext:eglot.el")
(declare-function eglot--language-id "ext:eglot.el")

(defun wls--lsp--uses-lsp-mode-p ()
  "Check if buffer is manged by `lsp-mode'."
  (and (featurep 'lsp-mode)
       (bound-and-true-p lsp-mode)
       (lsp-workspaces)))

(defun wls--lsp--uses-eglot-p ()
  "Check if buffer is manged by `eglot'."
  (and (featurep 'eglot)
       (bound-and-true-p eglot--managed-mode)))

(defun wls--lsp--active-p ()
  "Check if an LSP mode is active."
  (or (wls--lsp--uses-lsp-mode-p)
      (wls--lsp--uses-eglot-p)))

(defun wls--lsp--help ()
  "Get the appropriate help text."
  (cond
   ((wls--lsp--uses-lsp-mode-p)
    (concat
     "Connected to "
     (mapconcat #'lsp--workspace-print (lsp-workspaces) "|")))
   ((wls--lsp--uses-eglot-p)
    (let ((server (eglot-current-server)))
      (concat
       "Connected to "
       (eglot--language-id server)
       "::"
       (eglot-project-nickname server))))))

(defun wls--lsp--with-count ()
  "Get the count of connected servers."
  (if-let* ((icon (whale-line-iconify 'lsp))
            ((wls--lsp--uses-lsp-mode-p))
            (count (length (lsp-workspaces)))
            ((> count 1)))

      (list icon
            (whale-line--spacer)
            (propertize (number-to-string count) 'face 'whale-line-shadow))
    icon))

(defun wls--lsp--segment (&rest _args)
  "Indicate an active LSP session."
  (and-let* (((wls--lsp--active-p))
             (help (wls--lsp--help)))

    `((:propertize (:eval (wls--lsp--with-count)) help-echo ,help))))

(whale-line-create-stateful-segment lsp
  :getter wls--lsp--segment

  :hooks
  (lsp-after-initialize-hook
   lsp-after-uninitialized-functions
   lsp-after-open-hook
   eglot-server-initialized-hook
   eglot-managed-mode-hook))

;;; -- DAP

(declare-function dap--cur-session "ext:dap-mode.el")
(declare-function dap--debug-session-name "ext:dap-mode.el")
(declare-function dap--session-running "ext:dap-mode.el")

(defun wls--dap--active-p ()
  "Check whether debugging is in process."
  (and-let* (((featurep 'dap-mode))
             ((bound-and-true-p dap-mode))
             (session (dap--cur-session))
             ((dap--session-running session)))))

(defun wls--dap--segment ()
  "Indicate an active DAP session."
  (when-let* (((wls--dap--active-p))
              (name (dap--debug-session-name (dap--cur-session)))
              (help (format "Debugging %s" name)))

    `((:propertize ,(whale-line-iconify 'dap) help-echo ,help))))

(whale-line-create-stateful-segment dap
  :getter wls--dap--segment
  :hooks
  (dap-session-created-hook
   dap-session-changed-hook
   dap-terminated-hook))

;;; -- Minions

(declare-function minions--prominent-modes "ext:minions.el")

(defvar minor-modes-alist)
(defvar minions-mode-line-minor-modes-map)
(defvar minions-mode-line-lighter)

(defun whale-line-minions--list (&rest _args)
  "Get the appropriate mode list."
  (if (bound-and-true-p minions-mode)
      `((:propertize (:eval (minions--prominent-modes))
                     face whale-line-shadow)
        ,(whale-line--spacer)
        (:propertize (:eval minions-mode-line-lighter)
                     face whale-line-shadow
                     local-map ,minions-mode-line-minor-modes-map
                     mouse-face whale-line-highlight))
    minor-mode-alist))

(whale-line-create-augment minions
  :action whale-line-minions--list

  :advice
  (:after-while . (whale-line-minor-modes--segment)))

;;; -- Org

(eval-when-compile
  (require 'org))

(declare-function org-back-to-heading "ext:org.el")
(declare-function org-before-first-heading-p "ext:org.el")
(declare-function org-heading-components "ext:org.el")
(declare-function org-link-display-format "ext:org.el")
(declare-function org-up-heading-safe "ext:org.el")

(defun wls--org--maybe-truncate (heading face)
  "Maybe truncate HEADING.

Use FACE for the ellipsis glyph."
  (let ((max-len wls-org-max-heading-length)
        (len (string-width heading))
        (ellipsis-len (string-width wls-org-ellipsis)))
    (if (> len max-len)
        (concat (substring heading 0 (max (- max-len ellipsis-len) 1))
                (propertize wls-org-ellipsis 'face face))
      heading)))

(defun wls--org--get-next-heading ()
  "Get the next heading going backwards."
  (org-back-to-heading)
  (let* ((components (org-heading-components))
         (level (nth 0 components))
         (face (nth (- level 1) org-level-faces))
         (heading (org-link-display-format (nth 4 components))))
    (propertize heading 'face face)))

(defun wls--org--collect-headings ()
  "Collect headings until it's no longer safe."
  (save-excursion
    (cl-loop collect (wls--org--get-next-heading)
             while (org-up-heading-safe))))

(defun wls--org--build-segment ()
  "Build the segment from included segments."
  (org-with-wide-buffer

   (goto-char (window-start))

   (and-let* (((not (org-before-first-heading-p)))
              (headings (wls--org--collect-headings))
              (count 0))

     (mapconcat
      #'identity
      (seq-map-indexed
       (lambda (it i)
         (if (eq (1- (length headings)) i)
             (progn
               (setq count (1+ count))
               it)
           (if (>= count wls-org-max-count)
               (propertize wls-org-elision 'face (nth i org-level-faces))
             (setq count (1+ count))
             (wls--org--maybe-truncate it (nth i org-level-faces)))))
       (reverse headings))
      (propertize wls-org-separator 'face 'whale-line-shadow)))))

(defun wls--org--segment ()
  "Get the Org segment."
  (when (derived-mode-p 'org-mode)
    (wls--org--build-segment)))

(whale-line-create-stateless-segment org
  :getter wls--org--segment
  :condition
  (eq major-mode 'org-mode))

;;; -- Project

(declare-function projectile-project-root "ext:projectile.el")
(declare-function project-name "ext:project.el")
(declare-function project-root "ext:project.el")

(defun wls--project--display-for-buffer-p ()
  "Check if current buffer should show project information.

Only consider Dired buffers and file buffers."
  (with-current-buffer (current-buffer)
    (or (derived-mode-p 'dired-mode)
        (buffer-file-name))))

(defvar wls--project--regexp ".+\\(\\/.+\\)\\/$")

(defvar wls--project--map
  (let ((map (make-sparse-keymap)))

    (define-key map [mode-line mouse-1] 'project-dired)

    map))

(defun wls--project--help ()
  "Get the help text for the project."
  (when-let* ((project (project-current))
              (root (project-root project)))

    (format "Project (%s)\nmouse-1: Open root" root)))

(defun wls--project--segment ()
  "Get the project segment."
  (when-let* ((candidate (wls--project--display-for-buffer-p))
              (project (project-current))
              (name (project-name project))
              (help (wls--project--help)))

    `(,@(when-let ((icon (whale-line-iconify 'project)))
          (list icon (whale-line--spacer)))
      (:propertize ,name
                   face whale-line-emphasis
                   mouse-face whale-line-highlight
                   help-echo ,help
                   local-map ,wls--project--map))))

(whale-line-create-stateful-segment project
  :getter wls--project--segment
  :hooks (find-file-hook))

;;; --- Tab bar

(defun wls--tab-bar--get-identifier ()
  "Get the identifier of the current tab.

This is either an explicit name or its index."
  (if-let* ((tab (tab-bar--current-tab))
            ((alist-get 'explicit-name tab))
            (name (alist-get 'name tab)))
      name
    (number-to-string (tab-bar--current-tab-index))))

(defun wls--tab-bar--segment ()
  "Get the name or number of the tab."
  (and-let* (((bound-and-true-p tab-bar-mode))
             (id (wls--tab-bar--get-identifier)))

    `((:propertize ,(concat " " id " ") face whale-line-highlight))))

(whale-line-create-stateful-segment tab-bar
  :verify (lambda () (featurep 'tab-bar))
  :getter wls--tab-bar--segment
  :hooks (window-configuration-change-hook)
  :priority current-low)

;;; -- VC

(declare-function vc-git-root "ext:vc-git.el")

(defvar wls--vc--scope-regexp "\\(feature\\|\\(\\w+\\)?fix\\|improvement\\)\\/")

(defvar wls--vc--states '((up-to-date . whale-line-neutral)
                          (edited . whale-line-indicate)
                          (needs-update . whale-line-contrast)
                          (needs-merge . whale-line-urgent)
                          (unlocked-changes . whale-line-urgent)
                          (added . whale-line-emphasis)
                          (removed . whale-line-emphasis)
                          (conflict . whale-line-urgent)
                          (missing . whale-line-contrast)
                          (ignored . whale-line-shadow)
                          (unregistered . whale-line-shadow)))

(defun wls--vc--face-for-state (state)
  "Get the correct face for the STATE."
  (alist-get state wls--vc--states 'whale-line-neutral))

(defun wls--vc ()
  "Get version control info."
  (and-let* (((buffer-file-name))
             (info (or (wls--vc-registered--info) (wls--vc-unregistered--info))))

    `(,@(when-let ((icon (whale-line-iconify 'vc)))
          (list icon (whale-line--spacer)))
      ,info)))

;;;; -- Registered

(defun wls--vc-registered--info ()
  "Get info for registered files."
  (and-let* (((and vc-mode buffer-file-name))
             (backend (vc-backend buffer-file-name))
             (status (and vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
             (state (vc-state buffer-file-name backend)))

    `(:propertize ,(replace-regexp-in-string wls--vc--scope-regexp "" status)
                  mouse-face whale-line-highlight
                  face ,(wls--vc--face-for-state state))))

;;;; -- Unregistered

(defun wls--vc-unregistered--git-p (file)
  "Check whether the unregistered FILE is in a git repository."
  (require 'vc-git)
  (vc-git-root file))

(defun wls--vc-unregistered--info ()
  "Get info for unregistered files."
  (when-let ((file (buffer-file-name)))

    (cond
     ((wls--vc-unregistered--git-p file)
      (when-let* ((state (vc-state file 'Git))
                  (help (format "File state: %s" state)))

        `(:propertize "Git"
                      face ,(wls--vc--face-for-state state)
                      help-echo ,help))))))

(whale-line-create-stateful-segment vc
  :getter wls--vc

  :hooks
  (find-file-hook after-save-hook)

  :advice
  (:after . (vc-refresh-state)))

;;; -- Partial recall

(declare-function partial-recall-buffer-specs "ext:partial-recall-extensions.el")
(declare-function partial-recall-memory-specs "ext:partial-recall-extensions.el")
(declare-function partial-recall-implant "ext:partial-recall.el")

(defvar partial-recall-command-map)

(defun wls--can-use-partial-recall-p ()
  "Check whether `partial-recall' can be used."
  (and (require 'partial-recall nil t)
       (fboundp 'partial-recall-buffer-specs)))

(defun wls--partial-recall--toggle ()
  "Implant or excise the current buffer."
  (interactive)

  (let ((specs (partial-recall-buffer-specs)))

    (partial-recall-implant (current-buffer) (plist-get specs :implanted))))

(defun wls--partial-recall--menu ()
  "Show a menu for `partial-recall'."
  (interactive)

  (let* ((map (make-sparse-keymap))
         (rename (lambda (sym) (substring (symbol-name sym)
                                     (1+ (length "partial-recall")))))
         (bind (lambda (_event func)
                 (define-key-after map
                   (vector func)
                   (list 'menu-item (funcall rename func) func)))))

    (define-key-after map [--actions] (list 'menu-item "Partial Recall"))

    (map-keymap bind partial-recall-command-map)

    (condition-case nil
        (popup-menu map)
      (quit nil))))

(defvar wls--partial-recall-mode-line-map
  (let ((map (make-sparse-keymap)))

    (define-key map [mode-line mouse-1] 'wls--partial-recall--toggle)
    (define-key map [mode-line mouse-3] 'wls--partial-recall--menu)

    map))

(defun wls--partial-recall ()
  "Get the `partial-recall' segment.

The segment comprises two sub-segments. One to display the
current moment state (whether it is implanted or not) and another
to display the memory state.

The moment sub-segment binds implanting/excising as well as popup
menu for the library's command map."
  (when-let* ((b-specs (partial-recall-buffer-specs))
              (m-specs (partial-recall-memory-specs))
              ((plist-get b-specs :meaningful))

              (indicator (whale-line-iconify 'partial-recall))

              (size (plist-get m-specs :size))
              (cap (plist-get m-specs :capacity))
              (orig-cap (plist-get m-specs :original-capacity))
              (count (if (> size orig-cap)
                         (concat "+" (number-to-string (- size orig-cap)))
                       (number-to-string size)))
              (count-face (if (> cap orig-cap) 'whale-line-contrast 'whale-line-shadow)))

    `((:propertize ,indicator
                   face ,(if (plist-get b-specs :implanted) 'whale-line-contrast 'whale-line-shadow)
                   mouse-face whale-line-highlight
                   help-echo "Partial Recall\nmouse-1: Implant/Excise\nmouse-3: Menu"
                   local-map ,wls--partial-recall-mode-line-map)
      ,(whale-line--spacer)
      (:propertize ,count
                   face ,count-face
                   help-echo ,(format "Partial Recall Reality: %d/%d moments" size cap)))))

(whale-line-create-stateful-segment partial-recall
  :verify wls--can-use-partial-recall-p

  :getter wls--partial-recall

  :hooks (partial-recall-after-insert-hook
          partial-recall-probe-hook
          partial-recall-permanence-change-hook)

  :priority current-low)

(provide 'whale-line-segments)

;;; whale-line-segments.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wls-" . "whale-line-segments-"))
;; End:
