;;; whale-line-segments-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-segments)

(defvar rectangle (ert-resource-file "rectangle.txt"))

(ert-deftest buffer-identification--set-additional ()
  (let ((whale-line-segments--buffer-identification--additional-face nil)
        (whale-line-segments--buffer-identification--additional-help nil)
        (whale-line-segments-buffer-identification-hook nil))

    (bydi ((:watch whale-line-segments--buffer-identification--additional-face)
           (:watch whale-line-segments--buffer-identification--additional-help)
           (:spy run-hooks))

      (whale-line-segments--buffer-identification--set-additional "face" "help")

      (bydi-was-set-to whale-line-segments--buffer-identification--additional-face "face")
      (bydi-was-set-to whale-line-segments--buffer-identification--additional-help "help")

      (bydi-was-called-with run-hooks 'whale-line-segments-buffer-identification-hook))))

(ert-deftest buffer-identification ()
  (let ((whale-line-segments--buffer-identification--path-segments-map nil))

    (bydi ((:mock buffer-file-name :return "/test/buffer.org"))
      (should (equal '((:propertize (:eval (whale-line-segments--buffer-identification--path-segments))
                                    face whale-line-shadow
                                    help-echo "/test/buffer.org"
                                    mouse-face whale-line-highlight
                                    local-map nil)
                       (:propertize (:eval (propertized-buffer-identification "%b"))
                                    face (mode-line-buffer-id nil)))
                     (whale-line-segments--buffer-identification))))

    (let ((whale-line-segments--buffer-identification--additional-help "help"))

      (should (equal '((:propertize (:eval (whale-line-segments--buffer-identification--path-segments))
                                    face whale-line-shadow
                                    help-echo nil
                                    mouse-face whale-line-highlight
                                    local-map nil)
                       (:propertize (:eval (propertized-buffer-identification "%b"))
                                    face (mode-line-buffer-id nil)
                                    help-echo "help"))
                     (whale-line-segments--buffer-identification))))))

(ert-deftest buffer-identification--path-segments ()
  :tags '(segments buffer)

  (bydi ((:mock buffer-file-name :return "/test/one/two/three.el")
         (:mock buffer-name :return "three.el"))

    (let ((whale-line-segments-buffer-identification-path-segments 1))

      (should (string= "two/" (whale-line-segments--buffer-identification--path-segments)))

      (setq whale-line-segments-buffer-identification-path-segments 12)

      (should (string= "/test/one/two/" (whale-line-segments--buffer-identification--path-segments)))

      (setq whale-line-segments-buffer-identification-path-segments 0)

      (should-not (whale-line-segments--buffer-identification--path-segments)))))

(ert-deftest buffer-status--using-icons ()
  (let ((modified nil)
        (buffer-file-name "some-name"))

    (bydi ((:always whale-line-segments--decorates-p)
           (:mock whale-line-segments--decorate :return "!")
           (:mock buffer-modified-p :return modified))

      (should-not (whale-line-segments--buffer-status--writable))

      (setq buffer-file-name nil)

      (should (string= "!" (whale-line-segments--buffer-status--writable)))

      (setq modified t)

      (should (string= "!" (whale-line-segments--buffer-status--writable)))
      (bydi-was-called-last-with whale-line-segments--decorate '(buffer-modified :face whale-line-shadow))

      (setq buffer-file-name "some-name")
      (should (string= "!" (whale-line-segments--buffer-status--writable)))
      (bydi-was-called-last-with whale-line-segments--decorate '(buffer-modified :face whale-line-emphasis)))))

(ert-deftest buffer-status--not-using-icons ()
  (let ((modified nil)
        (buffer-file-name "some-name"))

    (bydi ((:ignore whale-line-segments--decorate)
           (:mock buffer-modified-p :return modified))

      (should (string= "@" (whale-line-segments--buffer-status--read-only)))

      (should (string= "" (whale-line-segments--buffer-status--writable)))

      (setq buffer-file-name nil)

      (should (string= "&" (whale-line-segments--buffer-status--writable)))

      (setq modified t)

      (should (string= "&*" (whale-line-segments--buffer-status--writable))))))

(ert-deftest buffer-status--read-only ()
  (bydi (whale-line-segments--decorate)
    (whale-line-segments--buffer-status--read-only)

    (bydi-was-called-with whale-line-segments--decorate 'buffer-read-only)))

(ert-deftest whale-line-segments--buffer-status ()
  (bydi (whale-line-segments--buffer-status--read-only
         whale-line-segments--buffer-status--writable)

    (let ((buffer-read-only t))

      (whale-line-segments--buffer-status)

      (bydi-was-called whale-line-segments--buffer-status--read-only :clear t)

      (setq buffer-read-only nil)

      (whale-line-segments--buffer-status)

      (bydi-was-called whale-line-segments--buffer-status--writable))))

;;;; Window status

(ert-deftest window-status ()
  (should (string-empty-p (whale-line-segments--window-status)))

  (set-window-dedicated-p (selected-window) t)

  (should (string-equal "^" (whale-line-segments--window-status)))

  (set-window-parameter (selected-window) 'no-other-window t)

  (should (string-equal "~·^" (whale-line-segments--window-status)))

  (set-window-dedicated-p (selected-window) nil)

  (should (string-equal "~" (whale-line-segments--window-status)))

  (set-window-parameter (selected-window) 'no-other-window nil)

  (set-window-parameter (selected-window) 'no-delete-other-windows t)

  (should (string-equal "!" (whale-line-segments--window-status))))

;;; Position

(ert-deftest position--line-and-column ()
  (let ((mode-line-position-column-line-format '(" (%l, %c)")))

    (should (string= "(%l, %c)" (whale-line-segments--position--line-and-column)))))

(ert-deftest position--default ()
  (bydi ((:mock format-mode-line :return " 2% 5/6"))
    (should (string= "2%% 5/6" (whale-line-segments--position--default)))))

;;;; Selection

(ert-deftest selection--area ()
  (with-temp-buffer
    (insert-file-contents rectangle)
    (goto-char (point-min))
    (shut-up
      (push-mark))
    (goto-char (point-max))

    (should (string= "3" (whale-line-segments--selection--rows))))

  (with-temp-buffer
    (insert-file-contents rectangle)
    (goto-char (point-min))
    (shut-up
      (rectangle-mark-mode))
    (goto-char (1- (point-max)))

    (should (string= "9" (whale-line-segments--selection--columns)))))

(ert-deftest animation-animate ()
  (bydi (force-mode-line-update)

    (let ((whale-line-segments-animation-key-frames [" .." ".. "])
          (whale-line--frame-index 0))

      (whale-line-segments--animation-animate)
      (should (string=" .." whale-line-segments--animation-frame))
      (bydi-was-called force-mode-line-update)

      (whale-line-segments--animation-animate)
      (should (string= ".. " whale-line-segments--animation-frame)))))

(ert-deftest animation-start-timer ()
  (bydi (run-with-timer)
    (let ((whale-line-segments--animation-timer nil)
          (whale-line-segments-animation-speed 0.4))

      (whale-line-segments--animation-start-timer)
      (bydi-was-called-with run-with-timer '(0 0.4 whale-line-segments--animation-animate))

      (bydi-clear-mocks)

      (whale-line-segments--animation-start-timer)
      (bydi-was-not-called run-with-timer))))

(ert-deftest animation-stop-timer ()
  (bydi cancel-timer
    (let ((whale-line-segments--animation-timer nil))

      (whale-line-segments--animation-stop-timer)
      (bydi-was-not-called cancel-timer)

      (bydi-clear-mocks)

      (setq whale-line-segments--animation-timer 'timer)
      (whale-line-segments--animation-stop-timer)
      (bydi-was-called cancel-timer)
      (should-not whale-line-segments--animation-timer))))

(ert-deftest can-use-flycheck-p ()
  (bydi ((:sometimes require))

    (should (whale-line-segments--flycheck--can-use-flycheck-p))

    (bydi-toggle-sometimes)

    (should-not (whale-line-segments--flycheck--can-use-flycheck-p))))

(ert-deftest flycheck--face ()
  (should-not (whale-line-segments--flycheck--face nil))
  (should (equal 'whale-line-segments--syntax-checker-running (whale-line-segments--flycheck--face 'running)))

  (defvar flycheck-current-errors)
  (let ((flycheck-current-errors nil)
        (mock-errors nil))

    (should-not (whale-line-segments--flycheck--face 'finished))

    (setq flycheck-current-errors 'errors)

    (bydi ((:mock flycheck-count-errors :return mock-errors))

      (setq mock-errors '((error . 1)))

      (should (equal 'flycheck-error (whale-line-segments--flycheck--face 'finished)))

      (setq mock-errors '((warning . 1)))

      (should (equal 'flycheck-warning (whale-line-segments--flycheck--face 'finished)))

      (setq mock-errors '((info . 1)))

      (should (equal 'flycheck-info (whale-line-segments--flycheck--face 'finished))))))

(ert-deftest flycheck--help ()
  (let ((whale-line-segments--flycheck--default-help ""))

    (should (string= "\n\nFlycheck: Still checking" (whale-line-segments--flycheck--help 'running)))
    (should (string= "" (whale-line-segments--flycheck--help nil)))

    (defvar flycheck-current-errors)

    (let ((flycheck-current-errors '((error . 1) (warning . 2) (info . 3))))

      (bydi ((:mock flycheck-count-errors :with bydi-rf))
        (should (string= "\n\nFlycheck: 1 error(s), 2 warning(s), 3 info(s)" (whale-line-segments--flycheck--help 'finished)))))))

(ert-deftest flycheck ()
  (bydi ((:mock whale-line-segments--flycheck--face :return "face")
         (:mock whale-line-segments--flycheck--help :return "help"))

    (should (equal (list "face" "help")
                   (whale-line-segments--flycheck 'status)))))

;;;; Major mode

(ert-deftest major-mode--toggle-fundamental ()
  :tags '(segments major-mode)

  (bydi ((:always text-mode)
         (:watch whale-line-segments--major-mode--before-fundamental))

    (ert-with-test-buffer (:name "fundamental")

      (setq major-mode 'text-mode)

      (whale-line-segments--major-mode--toggle-fundamental)

      (bydi-was-set-to whale-line-segments--major-mode--before-fundamental 'text-mode)

      (whale-line-segments--major-mode--toggle-fundamental)

      (bydi-was-called text-mode)

      (bydi-was-set-to whale-line-segments--major-mode--before-fundamental nil))))

(ert-deftest major-mode--decorated ()
  :tags '(segments major-mode)

  (let ((whale-line-segments--major-mode--map nil))

    (bydi ((:mock whale-line-segments--decorate :return "?")
           (:mock format-mode-line :return "echo"))

      (should (equal '((:propertize "?"
                                    help-echo "echo"
                                    display (raise -0.135)
                                    mouse-face whale-line-highlight
                                    local-map nil))
                     (whale-line-segments--major-mode--decorated))))))

(ert-deftest major-mode--text ()
  :tags '(segments major-mode)

  (let ((whale-line-segments--major-mode--map nil))

    (should (equal '((:propertize (" " mode-name " ")
                                  face whale-line-highlight
                                  mouse-face whale-line-highlight
                                  local-map nil))
                   (whale-line-segments--major-mode--text)))))

(ert-deftest major-mode ()
  :tags '(segments major-mode)
  (bydi ((:sometimes whale-line-segments--major-mode--decorated)
         (:always whale-line-segments--major-mode--text))

    (should (whale-line-segments--major-mode))
    (bydi-was-called whale-line-segments--major-mode--decorated)
    (bydi-was-not-called whale-line-segments--major-mode--text)
    (bydi-toggle-sometimes)
    (whale-line-segments--major-mode)
    (bydi-was-called whale-line-segments--major-mode--text)))

;;;; Flymake

(require 'flymake)

(ert-deftest flymake--count-types ()
  (let ((diagnostics (list (flymake--diag-make :type :note)
                           (flymake--diag-make :type :error)
                           (flymake--diag-make :type :warning))))

    (should (equal '(:errors 1 :warnings 1 :notes 1)
                   (whale-line-segments--flymake--count-types diagnostics)))))

(ert-deftest flymake--face ()
  (let ((counts '(:errors 0 :warnings 0 :notes 0)))

    (should-not (whale-line-segments--flymake--face counts))

    (plist-put counts :notes 1)

    (should (equal 'flymake-note (whale-line-segments--flymake--face counts)))

    (plist-put counts :warnings 1)

    (should (equal 'flymake-warning (whale-line-segments--flymake--face counts)))

    (plist-put counts :errors 1)

    (should (equal 'flymake-error (whale-line-segments--flymake--face counts)))))



(ert-deftest flymake--help ()
  (let ((counts '(:errors 1 :warnings 2 :notes 3))
        (whale-line-segments--flymake--default-help ""))

    (should (string= "\n\nFlymake: 1 error(s), 2 warning(s), 3 note(s)"
                     (whale-line-segments--flymake--help counts)))))

(ert-deftest flymake ()
  (let ((flymake--state (make-hash-table))
        (count 0)
        (running nil)
        (reporting nil))

    (bydi (flymake-diagnostics
           (:mock hash-table-count :return count)
           (:mock flymake-running-backends :return running)
           (:mock flymake-reporting-backends :return reporting)

           whale-line-segments--flymake--count-types

           (:mock whale-line-segments--flymake--face :return "face")
           (:mock whale-line-segments--flymake--help :return "help"))

      ;; No running backends.
      (should (equal (list nil (concat whale-line-segments--flymake--default-help "\n\nFlymake: No backends"))
                     (whale-line-segments--flymake)))
      (bydi-was-not-called flymake-diagnostics)
      (bydi-was-not-called whale-line-segments--flymake--count-types)

      ;; Waiting on reports.
      (setq count 1
            running '(a b)
            reported '(a))

      (should (equal (list 'whale-line-segments--syntax-checker-running
                           (concat whale-line-segments--flymake--default-help "\n\nFlymake: Running"))
                     (whale-line-segments--flymake)))

      (bydi-was-not-called flymake-diagnostics)
      (bydi-was-not-called whale-line-segments--flymake--count-types)

      ;; Reports are in.
      (setq running nil
            reported nil)

      (should (equal (list "face" "help")
               (whale-line-segments--flymake)))

      (bydi-was-called flymake-diagnostics)
      (bydi-was-called whale-line-segments--flymake--count-types))))

;;;; LSP mode

(ert-deftest lsp--uses-lsp-mode-p ()
  (defvar lsp-mode)
  (let ((lsp-mode t))

    (bydi ((:always featurep)
           (:risky-mock fboundp :with always)
           (:always lsp-workspaces))

      (should (whale-line-segments--lsp--uses-lsp-mode-p)))))

(ert-deftest lsp--uses-eglot-p ()
  (defvar eglot--managed-mode)
  (let ((eglot--managed-mode t))

    (bydi ((:always featurep)
           (:risky-mock fboundp :with always))

      (should (whale-line-segments--lsp--uses-eglot-p)))))

(ert-deftest lsp--active-p ()
  (let ((lsp nil)
        (eglot nil))

    (bydi ((:mock whale-line-segments--lsp--uses-eglot-p :return eglot)
           (:mock whale-line-segments--lsp--uses-lsp-mode-p :return lsp))

      (should-not (whale-line-segments--lsp--active-p))

      (setq lsp t)

      (should (whale-line-segments--lsp--active-p))

      (setq lsp nil
            eglot t)

      (should (whale-line-segments--lsp--active-p)))))

(ert-deftest lsp--help ()
  (let ((lsp nil)
        (eglot nil))

    (bydi ((:mock whale-line-segments--lsp--uses-lsp-mode-p :return lsp)
           (:mock whale-line-segments--lsp--uses-eglot-p :return eglot)
           (:mock lsp--workspace-print :with (lambda (it) (concat "'" (symbol-name it))))
           (:mock lsp-workspaces :return '(a b))
           eglot-current-server
           (:mock eglot-project-nickname :return "testing")
           (:mock eglot--language-id :return "test"))

      (should-not (whale-line-segments--lsp--help))

      (setq lsp t)

      (should (string= "Connected to 'a|'b" (whale-line-segments--lsp--help)))

      (setq lsp nil
            eglot t)

      (should (string= "Connected to test::testing" (whale-line-segments--lsp--help))))))

(ert-deftest lsp--with-count ()
  (bydi ((:mock whale-line-segments--decorate :return "LSP")
         (:sometimes whale-line-segments--lsp--uses-lsp-mode-p)
         (:mock lsp-workspaces :return '(a b c)))

    (should (equal '("LSP" " " "3") (whale-line-segments--lsp--with-count)))

    (bydi-toggle-sometimes)

    (should (string= "LSP" (whale-line-segments--lsp--with-count)))))

(ert-deftest lsp-segment ()

  (bydi ((:sometimes whale-line-segments--lsp--active-p)
         (:mock whale-line-segments--lsp--help :return "help"))

    (should (equal '((:propertize (:eval (whale-line-segments--lsp--with-count)) help-echo "help"))
                   (whale-line-segments--lsp)))))

(ert-deftest debug--active-p ()
  (let ((session nil)
        (running nil))

    (bydi ((:sometimes featurep)
           (:mock dap--cur-session :return session)
           (:mock dap--session-running :return running))

      (with-temp-buffer
        (defvar dap-mode t)

        (should-not (whale-line-segments--debug--active-p))
        (setq session 'session)
        (should-not (whale-line-segments--debug--active-p))
        (setq running t)
        (should (whale-line-segments--debug--active-p))
        (setq-local dap-mode nil)
        (should-not (whale-line-segments--debug--active-p))

        (bydi-toggle-sometimes)
        (should-not (whale-line-segments--debug--active-p))))))

(ert-deftest dap-segment ()
  (bydi ((:mock whale-line-segments--decorate :return "*")
         (:always whale-line-segments--debug--active-p)
         dap--cur-session
         (:mock dap--debug-session-name :return "Test"))

    (should (equal '((:propertize "*" help-echo "Debugging Test"))
                   (whale-line-segments--debug)))))

(ert-deftest minions--list ()
  (defvar minions-mode)
  (defvar minions-mode-line-lighter)
  (defvar minions-mode-line-minor-modes-map)

  (let ((minions-mode nil)
        (minor-mode-alist '(test rest))
        (minions-mode-line-lighter " min")
        (minions-mode-line-minor-modes-map nil))

    (should (equal '(test rest) (whale-line-segments--minions--list)))

    (bydi ((:mock minions--prominent-modes :with (lambda () '((prominent " prm")))))
      (with-temp-buffer
        (setq-local minions-mode t)

        (should (equal '((:eval (minions--prominent-modes))
                         " "
                         (:propertize (:eval minions-mode-line-lighter)
                                      face whale-line-shadow
                                      local-map nil
                                      mouse-face whale-line-highlight))
                       (whale-line-segments--minions--list)))))))

(ert-deftest org--maybe-truncate--truncates ()
  (should (string= "test…" (whale-line-segments--org--maybe-truncate "testing" 'success 5))))

(ert-deftest org--maybe-truncate--skips ()
  (should (string= "testing" (whale-line-segments--org--maybe-truncate "testing" 'success 7))))

(ert-deftest org--max-length--obeys-min ()
  (bydi ((:mock whale-line--space :return -123)
         (:mock window-font-width :return 10))
    (should (eq whale-line-segments--org--min-length
                (whale-line-segments--org--max-length)))))

(ert-deftest org--max-length--obeys-max ()
  (let ((whale-line-segments-org-max-count 3)
        (whale-line-segments--org--max-length 12)
        (whale-line-segments--org--min-length 4))

    (bydi ((:mock whale-line--space :return 40)
           (:mock window-font-width :return 1))
      (should (eq 12 (whale-line-segments--org--max-length))))))

(ert-deftest org--max-length--reduces-max ()
  (let ((whale-line-segments-org-max-count 3)
        (whale-line-segments--org--max-length 12)
        (whale-line-segments--org--min-length 3))

    (bydi ((:mock whale-line--space :return 32)
           (:mock window-font-width :return 1))
      (should (eq 10 (whale-line-segments--org--max-length))))))

(ert-deftest org--get-next-heading ()
  (let ((org-level-faces '(red green blue orange)))

    (bydi (org-back-to-heading
           (:mock org-heading-components :with (lambda () '(2 2 nil nil "Test Suite" nil)))
           (:mock org-display-format :with bydi-rf))

      (should (string= (whale-line-segments--org--get-next-heading) "Test Suite"))
      (bydi-was-called org-back-to-heading))))

(ert-deftest org--collect-headings ()
  (let ((headings '(a b c)))

    (bydi ((:mock org-up-heading-safe :with (lambda () (pop headings)))
           (:mock whale-line-segments--org--get-next-heading :return "Heading"))

      (should (equal '("Heading" "Heading" "Heading" "Heading") (whale-line-segments--org--collect-headings))))))

(ert-deftest org ()
  :tags '(org)

  (defmacro org-with-wide-buffer (&rest body)
    "Mock implementation that just expands BODY."
    `(progn
       ,@body))

  (let ((first-heading t)
        (headings nil)
        (whale-line-segments-org-max-count 2)
        (whale-line-segments-org-elision "*")
        (whale-line-segments-org-separator " "))

    (bydi ((:mock org-before-first-heading-p :return first-heading)
           (:mock whale-line-segments--org--collect-headings :return headings)
           (:mock whale-line-segments--org--maybe-truncate :with bydi-rf)
           (:mock whale-line-segments--org--max-length :return 12))
      (with-temp-buffer
        (should-not (whale-line-segments--org)))

      (setq first-heading nil)
      (with-temp-buffer
        (should-not (whale-line-segments--org)))

      (setq headings (list (propertize "One" 'face 'shadow)
                           (propertize "Two" 'face 'shadow)
                           (propertize "Three" 'face 'shadow)))

      (with-temp-buffer
        (should (string= "Three Two One" (whale-line-segments--org))))

      (setq whale-line-segments-org-max-count 1)

      (with-temp-buffer
        (should (string= "Three * One" (whale-line-segments--org))))

      (setq whale-line-segments-org-max-count 2
            headings (list (propertize "One" 'face 'shadow)))

      (with-temp-buffer
        (should (string= "One" (whale-line-segments--org))))

      (setq headings (list (propertize "Five million and a few more" 'face 'shadow)
                           (propertize "One" 'face 'shadow)))

      (with-temp-buffer
        (should (string= "Five million and a few more" (whale-line-segments--org)))))))

(ert-deftest project--display-for-buffer-p--no-show-for-non-file-non-dired ()
  (with-temp-buffer
    (bydi ((:ignore derived-mode-p)
           (:ignore buffer-file-name))
      (should-not (whale-line-segments--project--display-for-buffer-p)))))

(ert-deftest project--help ()
  (bydi ((:always project-current)
         (:mock project-root :return "/home/test/project/"))

      (should (string= "Project (/home/test/project/)\nmouse-1: Open root" (whale-line-segments--project--help)))))

(ert-deftest project--get--for-project ()
  (let ((whale-line-segments--project--map nil))

    (bydi ((:always whale-line-segments--project--display-for-buffer-p)
           (:always project-current)
           (:mock whale-line-segments--project--help :return "help")
           (:mock whale-line-segments--decorate :return "*")
           (:mock project-root :return "/home/test/project/")
           (:mock project-name :return "project"))

      (should (equal '("*" " " (:propertize "project"
                                            face whale-line-emphasis
                                            mouse-face whale-line-highlight
                                            help-echo "help"
                                            local-map nil))
                     (whale-line-segments--project))))))

(ert-deftest tab-bar ()
  (let ((tab '((explicit-name . t) (name . "test-tab")))
        (tab-bar-mode t))

    (bydi ((:mock tab-bar--current-tab :return tab)
           (:mock tab-bar--current-tab-index :return 42)
           (:risky-mock fboundp :with always))

      (should (string= "test-tab" (whale-line-segments--tab-bar--identifier)))

      (setcdr (assoc 'explicit-name tab) nil)

      (should (string= "42" (whale-line-segments--tab-bar--identifier))))))

;;;; VC

(ert-deftest vc--face-for-state ()
  (let ((whale-line-segments--vc--state-specs '((test . (neutral))
                                                (live . (urgent)))))

    (should (eq 'neutral (car (whale-line-segments--vc--specs-for-state 'test))))
    (should (eq 'urgent (car (whale-line-segments--vc--specs-for-state 'live))))
    (should (eq 'whale-line-shadow (car (whale-line-segments--vc--specs-for-state 'unknown))))))

(ert-deftest vc ()
  (bydi ((:always buffer-file-name)
         (:ignore whale-line-segments--vc-registered--info)
         (:mock whale-line-segments--vc-unregistered--info :return "test")
         (:mock whale-line-segments--decorate :return "*"))

    (should (equal '("*" " " "test")
                   (whale-line-segments--vc)))))

(ert-deftest vc-registered--info ()
  (defvar vc-mode)
  (defvar buffer-file-name)

  (let ((vc-display-status t)
        (find-file-hook . nil)
        (buffer-file-name "/tmp/test")
        (vc-mode " Git:feature/tests"))

    (bydi ((:mock vc-backend :return "none")
           (:mock vc-state :return 'testing)
           (:mock whale-line-segments--decorate :return "*")
           (:mock whale-line-segments--vc--specs-for-state :return '(test-face "?")))

      (should (equal '((:propertize "tests" mouse-face whale-line-highlight)
                       (:propertize "?" face test-face))
                     (whale-line-segments--vc-registered--info))))))

(ert-deftest vc-unregistered--git-p ()
  (let ((file "/tmp/test"))

    (bydi (require
           (:sometimes vc-git-root))

      (should (whale-line-segments--vc-unregistered--git-p file))
      (bydi-toggle-sometimes)
      (should-not (whale-line-segments--vc-unregistered--git-p file))

      (bydi-was-called-last-with vc-git-root file))))

(ert-deftest vc-unregistered--info ()
  (let ((name nil))
    (bydi ((:mock buffer-file-name :return name)
           (:sometimes whale-line-segments--vc-unregistered--git-p)
           (:mock whale-line-segments--vc--specs-for-state :return '(test-face "?"))
           (:mock vc-state :return 'testing))

      (should-not (whale-line-segments--vc-unregistered--info))
      (setq name "/tmp/testing")
      (should (equal '((:propertize "Git" help-echo "File state: testing")
                       (:propertize "?" face test-face))
                     (whale-line-segments--vc-unregistered--info)))
      (bydi-toggle-sometimes)
      (should-not (whale-line-segments--vc-unregistered--info)))))

;;; whale-line-segments-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
