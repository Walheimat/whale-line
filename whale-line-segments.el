;;; whale-line-segments.el --- Default segments. -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/whale-line
;; Version: 0.6.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces mode-line

;;; Commentary:

;; Definitions of the default mode-line segments.

;;; Code:

(require 'whale-line-core)

;;; -- Customization

(defcustom whale-line-animation-key-frames ["(__.- >{"
                                            "(__.' >{"
                                            "(__.- >{"
                                            "(__., >{"]
  "Animation key frames."
  :group 'whale-line
  :type '(vector string))

(defcustom whale-line-animation-speed 1.0
  "Animation speed."
  :group 'whale-line
  :type 'float)

(defcustom whale-line-icons-prettify-buffer-status nil
  "Whether to use icons for the buffer status."
  :group 'whale-line
  :type 'boolean)

(defcustom whale-line-org-delimiter "/"
  "The delimiter between file name and heading name."
  :group 'whale-line
  :type 'string)

(defcustom whale-line-org-ellipsis "…"
  "The string indicating truncation."
  :group 'whale-line
  :type 'string)

(defcustom whale-line-org-include 'current-and-root
  "The heading depth to show."
  :group 'whale-line
  :type '(choice (const current-and-root)
                 (const current)))

(defcustom whale-line-org-max-heading-length 12
  "The max length of a heading before truncation."
  :group 'whale-line
  :type 'integer)

(defcustom whale-line-project-provider 'project
  "The project provider."
  :group 'whale-line
  :type '(choice (const project)
                 (const projectile)))

(defconst whale-line-icon-type
  '(cons symbol
         (restricted-sexp
          :match-alternatives
          (stringp listp)))
  "Icon type as a cons cell of the icon library and the icons specs.
The specs are either a string of the icon name or a list of the
icon name and the face.")

(defcustom whale-line-icons-project-icon '(faicon . "folder-open")
  "Icon used for the project segment."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom whale-line-icons-vc-icon '(faicon . "code-fork")
  "Icon used for the VC segment."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom whale-line-icons-buffer-read-only-icon '(faicon . ("lock" whale-line-contrast))
  "Icon used to indicate buffer is read-only."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom whale-line-icons-no-buffer-file-name-icon '(faicon . ("sticky-note-o" whale-line-shadow))
  "Icon used to indicate buffer has no file name."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom whale-line-icons-buffer-modified-icon '(faicon . ("pencil" whale-line-emphasis))
  "Icon used to indicate buffer has been modified."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom whale-line-icons-window-dedicated-icon '(faicon . ("link" whale-line-shadow))
  "Icon used to indicate a window is dedicated to its buffer."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom whale-line-icons-buffer-fallback-icon '(faicon . ("question-circle" whale-line-contrast))
  "Icon used when a buffer has no associated icon."
  :group 'whale-line
  :type whale-line-icon-type)

(defcustom whale-line-icons-lsp-icon '(faicon . ("server" whale-line-contrast))
  "Icon used to indicate active LSP session."
  :group 'whale-line
  :type whale-line-icon-type)

;;; -- Basic segments

(declare-function image-mode-window-get "ext:image-mode.el")
(declare-function doc-view-last-page-number "ext:doc-view.el")

;;;; -- Buffer identification

(defun wls--buffer-identification ()
  "Get the buffer name."
  '((:propertize (:eval (propertized-buffer-identification "%b"))
                 mouse-face whale-line-highlight
                 face whale-line-neutral)))

(whale-line-create-static-segment buffer-identification
  :getter wls--buffer-identification
  :hooks (find-file-hook after-save-hook clone-indirect-buffer-hook kill-buffer-hook)
  :advice (:after . (not-modified rename-buffer set-visited-file-name pop-to-buffer undo)))

(defun wls--buffer-status ()
  "Render buffer status segment."
  (let ((render (cond
                 (buffer-read-only
                  '((:propertize "@" face whale-line-contrast)))
                 ((not (buffer-file-name))
                  '((:propertize "&" face whale-line-shadow)))
                 ((buffer-modified-p)
                  '((:propertize "*" face whale-line-emphasis)))
                 (t ""))))

    render))

(whale-line-create-dynamic-segment buffer-status
  :getter wls--buffer-status)

(defun wls--window-status ()
  "Render window status segment."
  (when (window-dedicated-p)
    '((:propertize "^" face whale-line-shadow))))

(whale-line-create-dynamic-segment window-status
  :getter wls--window-status
  :priority low)

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

(whale-line-create-dynamic-segment position
  :getter wls--position
  :priority current)

(defun wls--misc-info ()
  "Render the misc info segment."
  mode-line-misc-info)

(whale-line-create-dynamic-segment misc-info
  :condition mode-line-misc-info
  :getter wls--misc-info
  :priority current-low)

(whale-line-create-dynamic-segment minor-modes
  :getter (lambda () minor-mode-alist)
  :priority low)

(defun wls--process ()
  "Get process segment."
  mode-line-process)

(whale-line-create-dynamic-segment process
  :getter wls--process
  :condition mode-line-process
  :priority current)

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

(whale-line-create-dynamic-segment selection
  :condition mark-active
  :getter wls--selection
  :priority current-low)

;;; -- Animation

(defvar wla--frame-index 0)
(defvar wla--timer nil)
(defvar wla--segment)

(defun wla--animate ()
  "Animate.

Forces a mode-line update and returns the current frame."
  (let* ((frame (aref wla-key-frames wla--frame-index)))
    (setq wla--frame-index
          (mod
           (1+ wla--frame-index)
           (length wla-key-frames))
          wla--segment `((:propertize ,frame face whale-line-emphasis)))
    (force-mode-line-update)
    wla--segment))

(defun wla--start-timer ()
  "Set up the animation timer."
  (unless wla--timer
    (setq wla--timer (run-with-timer 0 wla-speed #'wla--get-segment))))

(defun wla--stop-timer ()
  "Stop the animation timer."
  (when wla--timer
    (cancel-timer wla--timer)
    (setq wla--timer nil)))

(whale-line-create-static-segment animation
  :getter wla--animate

  :setup wla--start-timer

  :teardown wla--stop-timer

  :priority current-low)

;;; -- Cursors

(declare-function mc/num-cursors "ext:multiple-cursors.el")
(declare-function iedit-counter "ext:iedit.el")

(defun whale-line-cursors--count ()
  "Get the cursor count."
  (let* ((mc-cursors (when (bound-and-true-p multiple-cursors-mode)
                       (mc/num-cursors)))
         (iedit-cursors (when (bound-and-true-p iedit-mode)
                          (iedit-counter)))
         (cursors (or mc-cursors iedit-cursors)))

    (when cursors
      `((:propertize ,(format " %d " cursors) face whale-line-highlight)))))

(whale-line-create-dynamic-segment cursors
  :getter whale-line-cursors--count

  :condition
  (or (bound-and-true-p multiple-cursors-mode)
      (bound-and-true-p iedit-mode))

  :priority current)

;;; -- Flycheck

(declare-function flycheck-count-errors "ext:flycheck.el")
(declare-function whale-line--spacer "whale-line.el")

(defface wlf-running
  '((t (:underline (:style wave)
                   :inherit (shadow))))
  "Face used to indicate running state."
  :group 'whale-line)

(defvar flycheck-current-errors)
(defun wlf--get-face-for-status (status)
  "Get the face to use for STATUS."
  (pcase status
    ('running 'wlf-running)
    ('finished
     (if flycheck-current-errors
         (let-alist (flycheck-count-errors flycheck-current-errors)
           (cond
            (.error 'flycheck-error)
            (.warning 'flycheck-warning)
            (.info 'flycheck-info)))
       'whale-line-neutral))
    (_ 'whale-line-neutral)))

(defun wlf--get-error-help (status)
  "Get the error count for STATUS.

Returns nil if not checking or if no errors were found."
  (pcase status
    ('running "Still checking")
    ('finished
     (when flycheck-current-errors
       (let-alist (flycheck-count-errors flycheck-current-errors)
         (format "Errors: %s, warnings: %s, infos: %s" (or .error 0) (or .warning 0) (or .info 0)))))
    (_ nil)))

(defun wlf--underline (status &rest _r)
  "Underline buffer name based on STATUS."
  (let ((face (wlf--get-face-for-status status))
        (text (wlf--get-error-help status)))

	(setq-local whale-line-buffer-identification--segment
                (if text
					`((:propertize (:eval (propertized-buffer-identification "%b"))
                                   face ,face help-echo ,text))
				      `((:propertize (:eval (propertized-buffer-identification "%b"))
                                     face ,face))))))

(defun wlf--can-use-flycheck-p ()
  "Verify that flycheck augment can be used."
  (require 'flycheck nil t))

(whale-line-create-augment flycheck
  :verify wlf--can-use-flycheck-p

  :action wlf--underline

  :hooks
  (flycheck-status-changed-functions))

;;; -- Icons

(declare-function all-the-icons-icon-for-buffer "ext:all-the-icons.el")
(declare-function all-the-icons-faicon "ext:all-the-icons.el")

(defun wli--icon (specs &rest plist)
  "Get icon in SPECS with PLIST properties."
  (declare (indent defun))
  (let ((fun (intern (concat "all-the-icons-" (symbol-name (car specs)))))
        (icon (if (listp (cdr specs))
                  (cadr specs)
                (cdr specs)))
        (face (when (listp (cdr specs))
                (caddr specs))))

    (if face
        (apply fun (append (list icon :face face) plist))
      (apply fun (append (list icon) plist)))))

(defun wli--can-use-icons-p ()
  "Check whether icons can be used."
  (require 'all-the-icons nil t))

;;;; -- Icon for project

(defun wli--prepend-icon-to-project-segment (str)
  "Advise info getter to prepend an icon before STR."
  (if (stringp str)
      (list
       '(:eval (wli--icon wli-project-icon
                 :face 'whale-line-emphasis
                 :height 0.85
                 :v-adjust 0.0))
       (whale-line--spacer)
       str)
    str))

(whale-line-create-augment iconify-project
  :verify wli--can-use-icons-p

  :action wli--prepend-icon-to-project-segment

  :advice (:filter-return . (wlp--get-segment)))

;;;; -- Icon for VC

(defun wli--prepend-icon-to-vc-segment (str)
  "Advise info getter to prepend an icon before STR."
  (if (and (stringp str)
           (buffer-file-name))
      (list
       '(:eval (wli--icon wli-vc-icon
                 :face (whale-line-vc--face-for-state)
                 :height 0.85
                 :v-adjust 0.0))
       (whale-line--spacer)
       str)
    str))

(whale-line-create-augment iconify-vc
  :verify wli--can-use-icons-p

  :action wli--prepend-icon-to-vc-segment

  :advice (:filter-return . (wlvc--get-segment)))

;;;; -- Icon for buffer status

(defun wli--advise-buffer-status-segment ()
  "Advise buffer line segment to use icons."
  (let ((icon (cond
               (buffer-read-only 'wli-buffer-read-only-icon)
               ((not (buffer-file-name))
                'wli-no-buffer-file-name-icon)
               ((buffer-modified-p)
                'wli-buffer-modified-icon)
               (t nil))))

    (if icon
        `((:eval (wli--icon ,icon :height 0.85 :v-adjust 0.0)))
      "")))

(whale-line-create-augment iconify-buffer-status
  :verify wli--can-use-icons-p

  :action wli--advise-buffer-status-segment

  :advice (:override . (wls--buffer-status)))

;;;; -- Icon for window status

(defun wli--advise-window-status-segment ()
  "Advise window status segment to use icons."
  (if (window-dedicated-p)
      '((:eval (wli--icon wli-window-dedicated-icon :height 0.85 :v-adjust 0.0)))
    ""))

(whale-line-create-augment iconify-window-status
  :verify wli--can-use-icons-p

  :action wli--advise-window-status-segment

  :advice (:override . (wls--window-status)))


;;;; -- Buffer icon segment

(defun wli--buffer-icon ()
  "Get the buffer icon segment."
  (let ((icon (all-the-icons-icon-for-buffer)))

    `((:propertize ,(if (or (null icon) (symbolp icon))
                       '(:eval (wli--icon wli-buffer-fallback-icon))
                     icon)
                   help-echo ,(format "%s" (format-mode-line mode-name))
                   display (raise -0.135)))))

(whale-line-create-static-segment buffer-icon
  :verify wli--can-use-icons-p

  :hooks
  (find-file-hook after-change-major-mode-hook clone-indirect-buffer-hook)

  :getter wli--buffer-icon)

;;; -- LSP

(declare-function lsp-workspaces "ext:lsp-mode.el")

(defun wll--active-p ()
  "Check if an LSP mode is active."
  (cond
   ((featurep 'lsp-mode)
    (lsp-workspaces))
   ((featurep 'eglot)
    (bound-and-true-p eglot--managed-mode))))

(defun wll--segment (&rest _args)
  "Indicate an active LSP session."
  (if (memq 'icons whale-line-segments)
      (when (wll--active-p)
        '((:propertize (:eval (wli--icon wli-lsp-icon :height 0.85 :v-adjust 0.0))
                       help-echo "Connected to LSP server")))
    (when (wll--active-p)
      '((:propertize "LSP" face whale-line-indicate)))))

(whale-line-create-static-segment lsp
  :getter wll--segment

  :hooks
  (lsp-after-initialize-hook
   lsp-after-uninitialized-functions
   lsp-after-open-hook
   eglot-server-initialized-hook
   eglot-managed-mode-hook))


;;; -- Minions

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
(declare-function whale-line--spacer "whale-line.el")

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
  (save-excursion
    (cl-loop collect (wlo--get-next-heading)
             while (org-up-heading-safe))))

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
                 (whale-line--spacer)
                 (nth 0 headings))
              (nth 0 headings)))))))))

(whale-line-create-dynamic-segment org
  :getter
  (let ((segment (wlo--build-segment)))
    (when segment
      (concat
       wlo-delimiter
       (whale-line--spacer)
       (wlo--build-segment))))

  :condition
  (eq major-mode 'org-mode))

;;; -- Project

(declare-function whale-line--spacer "whale-line.el")
(declare-function projectile-project-root "ext:projectile.el")
(declare-function project-name "ext:project.el")
(declare-function project-root "ext:project.el")


(defun wlp--display-for-buffer-p ()
  "Check if current buffer should show project information.

Only consider Dired buffers and file buffers."
  (with-current-buffer (current-buffer)
    (or (derived-mode-p 'dired-mode)
        (buffer-file-name))))

(defvar wlp--regexp ".+\\(\\/.+\\)\\/$")

(defun wlp--get ()
  "Get the project segment."
  (when-let* ((candidate (wlp--display-for-buffer-p))
              (p-root (pcase wlp-provider
                        ('projectile
                         (projectile-project-root))
                        ('project
                         (when-let ((current (project-current)))
                           (project-root current)))
                        (_ nil)))
              (p-name (pcase wlp-provider
                        ('projectile
                         (string-match wlp--regexp p-root)
                         (substring (match-string 1 p-root) 1))
                        ('project
                         (project-name (project-current)))
                        (_ ""))))

    (propertize p-name 'face 'whale-line-emphasis 'help-echo p-root)))

(whale-line-create-static-segment project
  :getter wlp--get

  :hooks
  (find-file-hook))

;;; --- Tab bar

(defun wltb--get-explicit-name ()
  "Get the name of the tab if it was set explicitly."
  (when-let* ((tab (tab-bar--current-tab))
              ((alist-get 'explicit-name tab))
              (name (alist-get 'name tab)))

    (propertize (concat " " name " ") 'face 'whale-line-highlight)))

(whale-line-create-static-segment tab-bar
  :verify
  (lambda () (featurep 'tab-bar))

  :getter wltb--get-explicit-name

  :hooks
  (window-configuration-change-hook)

  :priority current-low)

;;; -- VC

(declare-function whale-line--is-current-window-p "whale-line.el")
(declare-function whale-line--spacer "whale-line.el")

(defvar-local wlvc--state nil)

(defun wlvc--update-state ()
  "Update the version control state."
  (when-let ((state (wlvc--get-state)))
    (setq-local wlvc--state state)))

(defun wlvc--get-state ()
  "Get the version control state."
  (when-let ((backend (vc-backend buffer-file-name)))
    (vc-state (file-local-name buffer-file-name) backend)))

(defun wlvc--face-for-state ()
  "Get the correct face for the state."
  (let ((state wlvc--state))
    (cond ((eq state 'needs-update)
           'whale-line-contrast)
          ((eq state 'edited)
           'whale-line-indicate)
          ((memq state '(removed conflict unregistered))
           'whale-line-contrast)
          (t 'whale-line-neutral))))

(defvar wlvc--scope-regexp "\\(feature\\|\\(\\w+\\)?fix\\|improvement\\)\\/")
(defvar-local wlvc--info nil)

(defun wlvc--update-info ()
  "Update version control info."
  (when-let ((info (wlvc--get-info)))
    (setq-local wlvc--info info)))

(defun wlvc--get-info ()
  "Get version control info."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (status (if vc-display-status
                       (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                     ""))
           (str (replace-regexp-in-string wlvc--scope-regexp "" status)))
      (propertize str
                  'mouse-face 'whale-line-highlight
                  'face (wlvc--face-for-state)))))

(whale-line-create-static-segment vc
  :getter
  (progn
    (wlvc--update-state)
    (wlvc--update-info)
    wlvc--info)

  :hooks
  (find-file-hook after-save-hook)

  :advice
  (:after . (vc-refresh-state)))

(provide 'whale-line-segments)

;;; whale-line-segments.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wls-" . "whale-line-segments-")
;;                          ("wla-" . "whale-line-animation-")
;;                          ("wlf-" . "whale-line-flycheck-")
;;                          ("wll-" . "whale-line-lsp-")
;;                          ("wlo-" . "whale-line-org-")
;;                          ("wlp-" . "whale-line-project-")
;;                          ("wltb-" . "whale-line-tab-bar-")
;;                          ("wlvc-" . "whale-line-vc-")
;;                          ("wli-" . "whale-line-icons-"))
;; End:
