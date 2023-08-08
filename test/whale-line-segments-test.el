;;; whale-line-segments-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-segments)

(defvar rectangle (ert-resource-file "rectangle.txt"))

(ert-deftest wls--buffer-identification ()
  (should (equal '((:propertize (:eval (propertized-buffer-identification "%b"))
                                mouse-face whale-line-highlight
                                face whale-line-neutral))
                 (whale-line-segments--buffer-identification))))

(ert-deftest wls--buffer-status ()
  (with-temp-buffer
    (read-only-mode)

    (should (propertized-string= "@" (whale-line-segments--buffer-status)))

    (read-only-mode -1)

    (should (propertized-string= "&" (whale-line-segments--buffer-status))))

  (ert-with-temp-file buffer-status :buffer current
    (with-current-buffer current
      (should-not (whale-line-segments--buffer-status))

      (insert "test")

      (should (propertized-string= "*" (whale-line-segments--buffer-status))))))

(ert-deftest wls--window-status ()
  (should-not (whale-line-segments--window-status))

  (set-window-dedicated-p (selected-window) t)

  (should (propertized-string= "^" (whale-line-segments--window-status)))

  (set-window-dedicated-p (selected-window) nil))

(ert-deftest wls--position ()
  (bydi ((:mock image-mode-window-get :return 1)
         (:mock doc-view-last-page-number :return 2))

    (with-temp-buffer
      (setq major-mode 'doc-view-mode)

      (should (propertized-string= "1/2" (whale-line-segments--position)))))

  (with-temp-buffer
    (should (propertized-string= "%l:%c %p%" (whale-line-segments--position)))

    (defvar follow-mode)

    (setq follow-mode t)

    (should (propertized-string= "f: %l:%c %p%" (whale-line-segments--position)))))

(ert-deftest wls--misc-info ()
  (let ((mode-line-misc-info '("a" "b")))

    (should (equal '("a" "b") (whale-line-segments--misc-info)))))

(ert-deftest wls--process ()
  (let ((mode-line-process '("a" "b")))

    (should (equal '("a" "b") (whale-line-segments--process)))))

(ert-deftest whale-line-selection--get-columns ()
  (with-temp-buffer
    (insert-file-contents rectangle)
    (should (eq (whale-line-selection--get-columns 1 30) 9))))

(ert-deftest whale-line-segments--selection--lines ()
  (with-temp-buffer
    (insert-file-contents rectangle)
    (goto-char (point-min))
    (push-mark)
    (goto-char (point-max))

    (should (propertized-string= " 3 " (whale-line-segments--selection)))))

(ert-deftest whale-line-segments--selection--rectangle ()
  (with-temp-buffer
    (insert-file-contents rectangle)
    (goto-char (point-min))
    (rectangle-mark-mode)
    (goto-char (1- (point-max)))

    (should (propertized-string= " 3x9 " (whale-line-segments--selection)))))

(ert-deftest wla--animate ()
  (bydi (force-mode-line-update)

    (defvar whale-line--frame-index)
    (defvar whale-line-animation-key-frames)

    (let ((whale-line-animation-key-frames [" .." ".. "])
          (whale-line--frame-index 0))

      (should (equal '((:propertize " .." face whale-line-emphasis)) (whale-line-animation--animate)))
      (bydi-was-called force-mode-line-update)

      (should (equal '((:propertize ".. " face whale-line-emphasis)) (whale-line-animation--animate))))))

(ert-deftest wla--start-timer ()
  (bydi (run-with-timer)
    (let ((whale-line-animation--timer nil)
          (whale-line-animation-speed 0.4))

      (whale-line-animation--start-timer)
      (bydi-was-called-with run-with-timer '(0 0.4 whale-line-animation--get-segment))

      (bydi-clear-mocks)

      (whale-line-animation--start-timer)
      (bydi-was-not-called run-with-timer))))

(ert-deftest wla--stop-timer ()
  (bydi cancel-timer
    (let ((whale-line-animation--timer nil))

      (whale-line-animation--stop-timer)
      (bydi-was-not-called cancel-timer)

      (bydi-clear-mocks)

      (setq whale-line-animation--timer 'timer)
      (whale-line-animation--stop-timer)
      (bydi-was-called cancel-timer)
      (should-not whale-line-animation--timer))))

(ert-deftest whale-line-cursors--count ()
  (defvar mulitple-cursors-mode)
  (defvar iedit-mode)

  (let ((mulitple-cursors-mode nil)
        (iedit-mode nil))

    (should-not (whale-line-cursors--count))

    (bydi ((:mock mc/num-cursors :return 2))

      (with-temp-buffer
        (setq-local multiple-cursors-mode t)

        (should (propertized-string= " 2 " (whale-line-cursors--count)))))

    (bydi ((:mock iedit-counter :return 3))

      (with-temp-buffer
        (setq-local iedit-mode t)

        (should (propertized-string= " 3 " (whale-line-cursors--count)))))))

(ert-deftest wli--can-use-flycheck-p ()
  (bydi ((:sometimes require))

    (should (whale-line-flycheck--can-use-flycheck-p))

    (bydi-toggle-sometimes)

    (should-not (whale-line-flycheck--can-use-flycheck-p))))

(ert-deftest wlf--get-face-for-status ()
  (should (equal 'whale-line-neutral (whale-line-flycheck--get-face-for-status nil)))
  (should (equal 'whale-line-flycheck-running (whale-line-flycheck--get-face-for-status 'running)))

  (defvar flycheck-current-errors)
  (let ((flycheck-current-errors nil)
        (mock-errors nil))

    (should (equal 'whale-line-neutral (whale-line-flycheck--get-face-for-status 'finished)))

    (setq flycheck-current-errors 'errors)

    (bydi ((:mock flycheck-count-errors :return mock-errors))

      (setq mock-errors '((error . 1)))

      (should (equal 'flycheck-error (whale-line-flycheck--get-face-for-status 'finished)))

      (setq mock-errors '((warning . 1)))

      (should (equal 'flycheck-warning (whale-line-flycheck--get-face-for-status 'finished)))

      (setq mock-errors '((info . 1)))

      (should (equal 'flycheck-info (whale-line-flycheck--get-face-for-status 'finished))))))

(ert-deftest wlf--get-error-help ()
  (should (string= "Still checking" (whale-line-flycheck--get-error-help 'running)))

  (should-not (whale-line-flycheck--get-error-help nil))

  (defvar flycheck-current-errors)

  (let ((flycheck-current-errors '((error . 1) (warning . 2) (info . 3))))

    (bydi ((:mock flycheck-count-errors :with bydi-rf))
      (should (string= "Errors: 1, warnings: 2, infos: 3" (whale-line-flycheck--get-error-help 'finished))))))

(ert-deftest wlf--underline ()
  (let ((whale-line-buffer-identification--segment nil)
        (segment "test")
        (text "testing"))

    (bydi ((:mock whale-line-buffer-name--get-segment :return segment)
           (:mock whale-line-flycheck--get-face-for-status :return 'success)
           (:mock whale-line-flycheck--get-error-help :return text))

      (with-temp-buffer
        (whale-line-flycheck--underline 'status)

        (should (equal '((:propertize (:eval (propertized-buffer-identification "%b")) face success help-echo "testing")) whale-line-buffer-identification--segment))

        (setq text nil)

        (whale-line-flycheck--underline 'status)

        (should (equal '((:propertize (:eval (propertized-buffer-identification "%b")) face success)) whale-line-buffer-identification--segment))))))

(ert-deftest wli--icon ()
  (bydi ((:mock all-the-icons-faicon :return "I"))
    (let ((test-icon-with-face '(faicon . ("test" test)))
          (test-icon '(faicon . "test")))

      (should (equal (whale-line-icons--icon test-icon-with-face :height 0.42) "I"))
      (bydi-was-called-with all-the-icons-faicon '("test" :face test :height 0.42))

      (bydi-clear-mocks)

      (should (equal (whale-line-icons--icon test-icon :height 0.42) "I"))
      (bydi-was-called-with all-the-icons-faicon '("test" :height 0.42)))))

(ert-deftest wli--can-use-icons-p ()
  (bydi ((:sometimes require))

    (should (whale-line-icons--can-use-icons-p))

    (bydi-toggle-sometimes)

    (should-not (whale-line-icons--can-use-icons-p))))

(ert-deftest wli--prepend-icon-to-project-segment ()
  (should (equal '((:eval (whale-line-icons--icon whale-line-icons-project-icon :face 'whale-line-emphasis :height 0.85 :v-adjust 0.0)) (:eval (whale-line--spacer)) "proj")
                 (whale-line-icons--prepend-icon-to-project-segment '("proj")))))

(ert-deftest wli--prepend-icon-to-vc-segment ()
  (let ((name "/test/tmp"))

    (bydi ((:sometimes display-graphic-p)
           (:mock buffer-file-name :return name))

      (should-not (whale-line-icons--prepend-icon-to-vc-segment nil))
      (should (equal '((:eval (whale-line-icons--icon whale-line-icons-vc-icon :face (whale-line-vc--face-for-state) :height 0.85 :v-adjust 0.0))
                       (:eval (whale-line--spacer))
                       "vc")
                     (whale-line-icons--prepend-icon-to-vc-segment '("vc"))))

      (bydi-toggle-sometimes)
      (setq name nil)

      (should (string= (whale-line-icons--prepend-icon-to-vc-segment "vc") "vc")))))

(ert-deftest wli--prepend-icon-to-project-segment--returns-non-strings ()
  (should (string= "hi" (whale-line-icons--prepend-icon-to-project-segment "hi"))))

(ert-deftest wli--advise-buffer-status-segment ()
  (let ((buffer-read-only nil)
        (name nil)
        (modified nil)
        (whale-line-icons-buffer-read-only-icon "read-only")
        (whale-line-icons-no-buffer-file-name-icon "no-file")
        (whale-line-icons-buffer-modified-icon "modified"))

    (with-temp-buffer
      (bydi ((:mock buffer-file-name :return name)
             (:mock buffer-modified-p :return modified))

        (setq buffer-read-only t)

        (should (equal '((:eval (whale-line-icons--icon whale-line-icons-buffer-read-only-icon :height 0.85 :v-adjust 0.0))) (whale-line-icons--advise-buffer-status-segment)))

        (bydi-clear-mocks)
        (setq buffer-read-only nil)

        (should (equal '((:eval (whale-line-icons--icon whale-line-icons-no-buffer-file-name-icon :height 0.85 :v-adjust 0.0))) (whale-line-icons--advise-buffer-status-segment)))

        (bydi-clear-mocks)
        (setq name "/tmp/test.el"
              modified t)

        (should (equal '((:eval (whale-line-icons--icon whale-line-icons-buffer-modified-icon :height 0.85 :v-adjust 0.0))) (whale-line-icons--advise-buffer-status-segment)))

        (setq modified nil)

        (should-not (whale-line-icons--advise-buffer-status-segment))))))

(ert-deftest wli--advise-window-status-segment ()
  (with-temp-buffer
    (bydi ((:sometimes window-dedicated-p)
           (:mock whale-line-icons--icon :return "I"))
      (should (equal '((:eval (whale-line-icons--icon whale-line-icons-window-dedicated-icon :height 0.85 :v-adjust 0.0))) (whale-line-icons--advise-window-status-segment)))

      (bydi-toggle-sometimes)
      (should-not (whale-line-icons--advise-window-status-segment)))))

(ert-deftest wli--buffer-icon--fallback ()
  (bydi ((:ignore all-the-icons-icon-for-buffer)
         (:mock format-mode-line :return "echo"))

    (should (equal (whale-line-icons--buffer-icon)
                   '((:propertize (:eval (whale-line-icons--icon whale-line-icons-buffer-fallback-icon))
                                  help-echo "echo" display (raise -0.135)))))))

(ert-deftest wli--buffer-icon ()
  (bydi ((:mock all-the-icons-icon-for-buffer :return "?")
         (:mock format-mode-line :return "echo"))

    (should (equal (whale-line-icons--buffer-icon)
                   '((:propertize "?" help-echo "echo" display (raise -0.135)))))))

(ert-deftest wll--active-p ()
  (let ((feature nil))
    (bydi ((:mock featurep :with (lambda (f) (eq f feature)))
           lsp-workspaces)

      (should-not (whale-line-lsp--active-p))

      (bydi-clear-mocks)
      (setq feature 'lsp-mode)
      (whale-line-lsp--active-p)

      (bydi-was-called lsp-workspaces)

      (bydi-clear-mocks)
      (setq feature 'eglot)

      (with-temp-buffer
        (defvar eglot--managed-mode nil)
        (setq-local eglot--managed-mode t)
        (should (whale-line-lsp--active-p))))))

(ert-deftest wll--segment--icons ()
  (bydi ((:sometimes whale-line-lsp--active-p)
         (:always whale-line-icons--can-use-icons-p))

    (with-temp-buffer
      (should (equal '((:propertize (:eval (whale-line-icons--icon whale-line-icons-lsp-icon :height 0.85 :v-adjust 0.0))
                                    help-echo "Connected to LSP server"))
                     (whale-line-lsp--segment)))

      (bydi-toggle-sometimes)

      (should-not (whale-line-lsp--segment)))))

(ert-deftest wll--segment--text ()
  (let ((whale-line-segments '()))

    (bydi ((:sometimes whale-line-lsp--active-p)
           (:ignore whale-line-icons--can-use-icons-p))

      (with-temp-buffer
        (should (equal '((:propertize "LSP" face whale-line-indicate help-echo "Connected to LSP server"))
                       (whale-line-lsp--segment)))

        (bydi-toggle-sometimes)

        (should-not (whale-line-lsp--segment))))))

(ert-deftest whale-line-minions--list ()
  (defvar minions-mode)
  (defvar minions-mode-line-lighter)
  (defvar minions-mode-line-minor-modes-map)

  (let ((minions-mode nil)
        (minor-mode-alist '(test rest))
        (minions-mode-line-lighter " min")
        (minions-mode-line-minor-modes-map nil))

    (should (equal '(test rest) (whale-line-minions--list)))

    (bydi ((:mock minions--prominent-modes :with (lambda () '((prominent " prm")))))
      (with-temp-buffer
        (setq-local minions-mode t)

        (should (equal '((:propertize (:eval (minions--prominent-modes))
                                      face whale-line-shadow)
                         " "
                         (:propertize (:eval minions-mode-line-lighter)
                                      face whale-line-shadow
                                      local-map nil
                                      mouse-face whale-line-highlight))
                       (whale-line-minions--list)))))))

(ert-deftest wlo--maybe-truncate--truncates ()
  (let ((whale-line-org-max-heading-length 5))

    (should (string= "test…" (whale-line-org--maybe-truncate "testing")))))

(ert-deftest wlo--maybe-truncate--skips ()
  (let ((whale-line-org-max-heading-length 7))

    (should (string= "testing" (whale-line-org--maybe-truncate "testing")))))

(ert-deftest wlo--get-next-heading ()
  (let ((org-level-faces '(red green blue orange)))

    (bydi (org-back-to-heading
           (:mock org-heading-components :with (lambda () '(2 2 nil nil "Test Suite" nil)))
           (:mock org-display-format :with bydi-rf))

      (should (string= (whale-line-org--get-next-heading) "Test Suite"))
      (bydi-was-called org-back-to-heading))))

(ert-deftest wlo--collect-headings ()
  (let ((headings '(a b c)))

    (bydi ((:mock org-up-heading-safe :with (lambda () (pop headings)))
           (:mock whale-line-org--get-next-heading :return "Heading"))

      (should (equal '("Heading" "Heading" "Heading" "Heading") (whale-line-org--collect-headings))))))

(ert-deftest wlo--build-segment ()
  (defmacro org-with-wide-buffer (&rest body)
    "Mock implementation that just expands BODY."
    `(progn
       ,@body))

  (let ((first-heading t)
        (headings nil)
        (whale-line-org-include 'current-and-root))

    (bydi ((:mock org-before-first-heading-p :return first-heading)
           (:mock whale-line-org--collect-headings :return headings)
           (:mock whale-line-org--maybe-truncate :with bydi-rf))
      (with-temp-buffer
        (should-not (whale-line-org--build-segment)))

      (setq first-heading nil)
      (with-temp-buffer
        (should-not (whale-line-org--build-segment)))

      (setq headings (list (propertize "One" 'face 'shadow)
                           (propertize "Two" 'face 'shadow)))

      (with-temp-buffer
        (should (string= "Two One" (whale-line-org--build-segment))))

      (setq whale-line-org-include 'current)

      (with-temp-buffer
        (should (string= "One" (whale-line-org--build-segment))))

      (setq whale-line-org-include 'current-and-root
            headings (list (propertize "One" 'face 'shadow)))

      (with-temp-buffer
        (should (string= "One" (whale-line-org--build-segment)))))))

(ert-deftest wlp--display-for-buffer-p--no-show-for-non-file-non-dired ()
  (with-temp-buffer
    (bydi ((:ignore derived-mode-p)
           (:ignore buffer-file-name))
      (should-not (whale-line-project--display-for-buffer-p)))))

(ert-deftest wlp--get--for-projectile ()
  (let ((whale-line-project-provider 'projectile))

    (bydi ((:always whale-line-project--display-for-buffer-p)
           (:mock projectile-project-root :return "/home/test/project/"))
      (should (propertized-string= "project" (whale-line-project--segment))))))

(ert-deftest wlp--get--for-project ()
  (let ((whale-line-project-provider 'project))

    (bydi ((:always whale-line-project--display-for-buffer-p)
           (:always project-current)
           (:mock project-root :return "/home/test/project/")
           (:mock project-name :return "project"))
      (should (propertized-string= "project" (whale-line-project--segment))))))

(ert-deftest wltb--get-explicit-name ()
  (bydi ((:mock tab-bar--current-tab :with (lambda () '((explicit-name . t) (name . "test-tab")))))
    (should (propertized-string= " test-tab " (whale-line-tab-bar--get-explicit-name)))))

(ert-deftest wlvc--update-state ()
  (bydi ((:mock whale-line-vc--get-state :with bydi-rt))
    (with-temp-buffer
      (should-not whale-line-vc--state)

      (whale-line-vc--update-state)

      (should (equal 'testing whale-line-vc--state)))))

(ert-deftest wlvc--get-state ()
  (bydi ((:mock vc-backend :with bydi-rt)
         vc-state
         (:mock file-local-name :return "/tmp/testing"))

    (whale-line-vc--get-state)

    (bydi-was-called-with vc-state (list "/tmp/testing" 'testing))))

(ert-deftest wlvc--face-for-state ()
  (with-temp-buffer
    (setq whale-line-vc--state 'needs-update)
    (should (eq (whale-line-vc--face-for-state) 'whale-line-contrast))

    (setq whale-line-vc--state 'edited)
    (should (eq (whale-line-vc--face-for-state) 'whale-line-indicate))

    (setq whale-line-vc--state 'conflict)
    (should (eq (whale-line-vc--face-for-state) 'whale-line-contrast))

    (setq whale-line-vc--state 'unknown)
    (should (eq (whale-line-vc--face-for-state) 'whale-line-neutral))))

(ert-deftest wlvc--update-info ()
  (with-temp-buffer
    (should-not whale-line-vc--info)

    (bydi ((:mock whale-line-vc--get-info :return "testing"))
      (whale-line-vc--update-info)
      (should (string= whale-line-vc--info "testing")))))

(ert-deftest wlvc--get-info--no-op-for-non-vc-files ()
  (with-temp-buffer
    (should-not (whale-line-vc--get-info))))

(ert-deftest wlvc--get-info ()
  (let ((vc-display-status t)
        (find-file-hook . nil))

    (bydi ((:mock vc-backend :return "none"))

      (ert-with-temp-file testing
        (with-current-buffer (find-file-noselect testing)
          (setq-local vc-mode " Git:feature/tests")
          (should (propertized-string= "tests" (whale-line-vc--get-info))))))))

(ert-deftest wls--can-use-partial-recall ()
  (should-not (whale-line-segments--can-use-partial-recall-p))

  (bydi-with-mock ((:always require))

    (should-not (whale-line-segments--can-use-partial-recall-p))

    (defun partial-recall-implanted-p ()
      ""
      nil)

    (should (whale-line-segments--can-use-partial-recall-p))))

(ert-deftest wls--partial-recall ()
  (let ((whale-line-segments--partial-recall-mode-line-map nil))
    (bydi ((:sometimes partial-recall-implanted-p)
           (:sometimes whale-line-icons--can-use-icons-p))

      (should (equal '((:propertize (:eval (whale-line-icons--icon whale-line-icons-partial-recall-icon
                                             :height 0.85
                                             :v-adjust 0.0))
                                    face whale-line-contrast
                                    help-echo "Partial Recall\nmouse-1: Implant/Excise"
                                    local-map nil))
                     (whale-line-segments--partial-recall)))

      (bydi-toggle-sometimes)

      (should (equal '((:propertize "PR"
                                    face whale-line-shadow
                                    help-echo "Partial Recall\nmouse-1: Implant/Excise"
                                    local-map nil))
                     (whale-line-segments--partial-recall))))))
(ert-deftest wls--partial-recall--toggle ()

  (bydi (partial-recall-implant
         (:sometimes partial-recall-implanted-p))

    (whale-line-segments--partial-recall--toggle)

    (bydi-was-called-with partial-recall-implant '(... t))

    (bydi-toggle-sometimes)

    (whale-line-segments--partial-recall--toggle)

    (bydi-was-called-with partial-recall-implant '(... nil))))

;;; whale-line-segments-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
