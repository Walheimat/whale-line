;;; whale-line-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line)

(ert-deftest whale-line--spacer ()
  (should (string= " " (whale-line--spacer)))
  (should (string= "  " (whale-line--spacer t))))

(ert-deftest whale-line--format-side ()
  (bydi (format-mode-line
         (:mock whale-line--render :return "test"))
    (whale-line--format-side :left 'filter)

    (bydi-was-called-with whale-line--render '(:left filter))
    (bydi-was-called-with format-mode-line "test")))

(ert-deftest whale-line--enough-space ()
  (let ((left "left")
        (right "right")
        (width 10))

    (bydi ((:mock window-font-width :return 1)
           (:mock window-pixel-width :return  width)
           (:mock whale-line--format-side :with (lambda (side)
                                                  (pcase side
                                                    (:left left)
                                                    (:right right)))))
      (should (whale-line--enough-space-p))
      (setq width 8)
      (should-not (whale-line--enough-space-p)))))

(defmacro with-whale-line (&rest body)
  "Render BODY with main functions mocked."
  `(let ((space t))
     (bydi ((:mock whale-line--render :return '("rendered"))
            (:mock whale-line--format-side :return "formatted")
            (:mock whale-line--enough-space-p  :return space)
            (:mock whale-line--space-between :return "   "))
       ,@body)))

(ert-deftest whale-line--format-ignore ()
  (with-whale-line
   (should (equal (whale-line--format-ignore)
                  '("rendered" "   " "rendered")))
   (bydi-was-called-with whale-line--space-between 9)))

(ert-deftest whale-line--format-elide ()
  (with-whale-line
   (setq space nil)
   (should (equal (whale-line--format-elide)
                  '("rendered"
                    "   "
                    (:eval (propertize (concat (whale-line--spacer) "..." (whale-line--spacer))
                                       'face 'whale-line-shadow)))))
   (bydi-was-called-with whale-line--space-between 5)

   (bydi-clear-mocks)

   (setq space t)
   (should (equal (whale-line--format-elide)
                  `("rendered" "   " "rendered")))
   (bydi-was-called-with whale-line--space-between 9)))

(ert-deftest whale-line--format-prioritize ()
  (with-whale-line
   (bydi (whale-line--format-ignore)
     (whale-line--format-prioritize)
     (bydi-was-called whale-line--format-ignore))

   (setq space nil)
   (should (equal (whale-line--format-prioritize)
                  '("rendered" "   " "rendered")))
   (bydi-was-called-with whale-line--format-side '(:right t))))

(ert-deftest whale-line--space-between ()
  (should (equal (propertize
                  " "
                  'display
                  `((space :align-to (- right (- 0 right-margin) 10))))
                 (whale-line--space-between 10))))

(ert-deftest whale-line--format ()
  (let ((whale-line-segment-strategy 'ignore))

    (bydi (whale-line--format-ignore
           whale-line--format-elide
           whale-line--format-prioritize)

      (whale-line--format)
      (bydi-was-called whale-line--format-ignore)

      (bydi-clear-mocks)
      (setq whale-line-segment-strategy 'elide)
      (whale-line--format)
      (bydi-was-called whale-line--format-elide)

      (bydi-clear-mocks)
      (setq whale-line-segment-strategy 'prioritize)
      (whale-line--format)
      (bydi-was-called whale-line--format-prioritize))))

(ert-deftest whale-line--get-current-window ()
  (let ((parent nil)
        (selected nil))

    (bydi ((:mock frame-parent :return parent)
           (:mock frame-selected-window :return selected))
      (should-not (whale-line--get-current-window))

      (setq parent t)

      (should-not (whale-line--get-current-window))
      (bydi-was-called frame-parent))))

(ert-deftest whale-line--set-selected-window ()
  (let ((active nil)
        (whale-line--current-window nil))

    (bydi ((:mock whale-line--get-current-window :with bydi-rt)
           (:mock minibuffer-window-active-p :return active)
           (:mock minibuffer-selected-window :return 'selected))

      (whale-line--set-selected-window)

      (should (eq 'testing whale-line--current-window))

      (setq active t)

      (whale-line--set-selected-window)

      (should (eq 'selected whale-line--current-window)))))

(ert-deftest whale-line--is-current-window-p ()

  (let ((whale-line--current-window nil))

    (bydi ((:mock whale-line--get-current-window :with bydi-rt))
      (should-not (whale-line--is-current-window-p))

      (setq whale-line--current-window 'resting)

      (should-not (whale-line--is-current-window-p))

      (setq whale-line--current-window 'testing)

      (should (whale-line--is-current-window-p)))))

(ert-deftest whale-line--set-segment-priority ()
  (let ((whale-line--segments '(:left
                                ((one . nil))
                                :right
                                ((two . t)))))

    (whale-line--set-segment-priority 'one 'low)

    (should (equal whale-line--segments '(:left ((one . low)) :right ((two . t)))))

    (whale-line--set-segment-priority 'two nil)

    (should (equal whale-line--segments '(:left ((one . low)) :right ((two . nil)))))))

(ert-deftest whale-line--set-segment-priority--unknown-segment ()
  (should-error (whale-line--set-segment-priority 'testing 'current) :type 'user-error))

(ert-deftest whale-line-add-segment ()
  (let ((whale-line--segments '(:left
                                ((one . nil))
                                :right
                                ((two . t)))))

    (whale-line-add-segment 'one)

    (should (equal whale-line--segments '(:left ((one . t)) :right ((two . t)))))

    (whale-line-add-segment 'two 'low)

    (should (equal whale-line--segments '(:left ((one . t)) :right ((two . low)))))))

(defvar rectangle (ert-resource-file "rectangle.txt"))

(ert-deftest whale-line--filter ()
  (let ((current nil)
        (segments '((a . low) (b . current-low) (c . t) (d . current))))

    (bydi ((:mock whale-line--is-current-window-p :return current))

      (should (equal (whale-line--filter segments)
                     '((a . low) (c . t))))

      (should (equal (whale-line--filter segments t)
                     '((c . t))))

      (setq current t)

      (should (equal (whale-line--filter segments)
                     '((a . low) (b . current-low) (c . t) (d . current))))

      (should (equal (whale-line--filter segments t)
                     '((c . t) (d . current)))))))

(ert-deftest whale-line--render ()
  (let ((whale-line--segments '(:left ((one . t)) :right ((two . nil)))))

    (bydi ((:mock whale-line--filter :return '((one . t)))
           whale-line--render-segments)

      (whale-line--render :left)

      (bydi-was-called-with whale-line--render-segments (list '((one . t)))))))

(ert-deftest whale-line--render-segments ()
  (bydi ((:mock functionp :with (lambda (x) (eq 'whale-line-three--segment x))))
    (defvar whale-line-one--segment)
    (defvar whale-line-four--segment)
    (let ((segments '((one . t) (two . nil) (three . t) (four . initial)))
          (whale-line-one--segment "one")
          (whale-line-four--segment 'initial))

      (should (equal '((:eval whale-line-one--segment)
                       (:eval (whale-line-three--segment))
                       (:eval (whale-line-four--action)))
                     (whale-line--render-segments segments))))))

(ert-deftest whale-line--enabled-feature-p ()
  (let ((whale-line-features '(one three)))
    (should-not (whale-line--enabled-feature-p 'two))
    (should (whale-line--enabled-feature-p 'three))))

(ert-deftest whale-line--disabled-features ()
  (let ((whale-line--all-features '(one two three four five))
        (whale-line-features '(one four)))
    (should (equal '(two three five) (whale-line--disabled-features)))))

(ert-deftest whale-line--enable-or-disable-feature--enables-not-enabled ()
  (bydi ((:mock functionp :with (lambda (x) (eq 'whale-line-four--setup x)))
         whale-line-four--setup
         whale-line--set-segment-priority)
    (let ((whale-line-features '(one two three)))

      (whale-line--enable-or-disable-feature 'four t)

      (bydi-was-called-with whale-line--set-segment-priority (list 'four t))
      (bydi-was-called whale-line-four--setup)

      (should (equal whale-line-features '(one two three four))))))

(ert-deftest whale-line--enable-or-disable-feature--disables-enabled ()
  (bydi ((:mock functionp :with (lambda (x) (eq 'whale-line-three--teardown x)))
         whale-line-three--teardown
         whale-line--set-segment-priority)
    (let ((whale-line-features '(one two three)))

      (whale-line--enable-or-disable-feature "three" nil)

      (bydi-was-called-with whale-line--set-segment-priority (list 'three nil))
      (bydi-was-called whale-line-three--teardown)

      (should (equal whale-line-features '(one two))))))

(ert-deftest whale-line--enable-or-disable-feature--wrong-usage ()
  (let ((whale-line-features '(one two three)))

    (should-error (whale-line--enable-or-disable-feature 'one t))
    (should-error (whale-line--enable-or-disable-feature 'four nil))))

(ert-deftest whale-line-mode--setup ()
  (let ((whale-line-features '(one two three))
        (mode-line-format 'format)
        (whale-line--default-mode-line nil))

    (bydi ((:mock require :return t)
           add-hook
           run-hooks)
      (whale-line-mode--setup)

      (bydi-was-called-n-times require 4)
      (bydi-was-called-nth-with require '(whale-line-segments) 0)
      (bydi-was-called-with run-hooks (list 'whale-line-setup-hook))
      (bydi-was-called-with add-hook (list 'pre-redisplay-functions #'whale-line--set-selected-window))

      (eq 'format whale-line--default-mode-line))))

(ert-deftest whale-line-mode--teardown ()
  (let ((mode-line-format 'whale)
        (whale-line--default-mode-line 'other))

    (bydi (run-hooks remove-hook)
      (whale-line-mode--teardown)

      (bydi-was-called-with run-hooks (list 'whale-line-teardown-hook))
      (bydi-was-called-with remove-hook (list 'pre-redisplay-functions #'whale-line--set-selected-window))

      (should (eq 'other mode-line-format)))))

(ert-deftest whale-line-disable-feature ()
  (bydi ((:mock completing-read :return "test")
         whale-line--enable-or-disable-feature)
    (call-interactively 'whale-line-disable-feature)
    (bydi-was-called-with whale-line--enable-or-disable-feature (list "test" nil))))

(ert-deftest whale-line-enable-feature ()
  (bydi ((:mock completing-read :return "test")
         whale-line--enable-or-disable-feature)
    (call-interactively 'whale-line-enable-feature)
    (bydi-was-called-with whale-line--enable-or-disable-feature (list "test" t))))

(ert-deftest whale-line-mode ()
  (bydi (whale-line-mode--setup whale-line-mode--teardown)
    (let ((whale-line-mode nil))
      (whale-line-mode)

      (bydi-was-called whale-line-mode--setup)

      (whale-line-mode -1)

      (bydi-was-called whale-line-mode--teardown))))

;; Macros

(defmacro whale-line-do-expand (&rest body)
  "Expand BODY with testing set to nil."
  (declare (indent 0))
  `(progn
     (setq whale-line--testing nil)
     ,@body
     (setq whale-line--testing t)))

(ert-deftest whale-line-create-static-segment ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line-create-static-segment test
       :getter (lambda () t)
       :hooks (test-mode-hook)
       :teardown (lambda () t)
       :setup (lambda () t))
     '(progn
       (defvar whale-line-test--segment 'initial)
       (defun whale-line-test--action
           (&rest _)
         "Set test segment."
         (if-let
             ((str
               (whale-line-test--get-segment)))
             (setq-local whale-line-test--segment
                         (concat
                          (whale-line--spacer)
                          str))
           (setq-local whale-line-test--segment nil)))
       (whale-line--function whale-line-test--get-segment
         (lambda nil t)
         "Get the test segment.")
       (whale-line--setup test :setup
         (lambda nil t)
         :advice nil :hooks
         (test-mode-hook)
         :teardown
         (lambda nil t))
       (whale-line-add-segment 'test 't)))))

(ert-deftest whale-line-create-static-segment--simple ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line-create-static-segment test
       :getter (lambda () t))
     '(progn
       (defvar whale-line-test--segment 'initial)
       (defun whale-line-test--action
           (&rest _)
         "Set test segment."
         (if-let
             ((str
               (whale-line-test--get-segment)))
             (setq-local whale-line-test--segment
                         (concat
                          (whale-line--spacer)
                          str))
           (setq-local whale-line-test--segment nil)))
       (whale-line--function whale-line-test--get-segment
         (lambda nil t)
         "Get the test segment.")
       (whale-line--setup test :setup nil :advice nil :hooks nil :teardown nil)
       (whale-line-add-segment 'test 't)))))

(ert-deftest whale-line-create-static-segment--using-symbols ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line-create-static-segment test
       :getter ignore
       :advice (:before . (ancient old))
       :verify (lambda () t)
       :teardown ignore
       :setup ignore
       :dense t
       :priority low)
     '(progn
       (defvar whale-line-test--segment 'initial)
       (defun whale-line-test--action (&rest _)
         "Set test segment."
         (if-let ((str (whale-line-test--get-segment)))
             (setq-local whale-line-test--segment str)
           (setq-local whale-line-test--segment nil)))

       (whale-line--function whale-line-test--get-segment ignore "Get the test segment.")

       (whale-line--setup test
         :setup ignore
         :advice (:before ancient old)
         :hooks nil
         :teardown ignore)

       (whale-line-add-segment 'test 'low)))))

(ert-deftest whale-line-create-dynamic-segment ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line-create-dynamic-segment test
       :getter (lambda () t)
       :verify (lambda () t)
       :teardown (lambda () t)
       :setup (lambda () t))
     '(progn
       (defun whale-line-test--segment ()
         "Render `test' segment."
         (or
          (when t
            (concat
             (whale-line--spacer)
             (whale-line-test--get-segment)))
          ""))
       (whale-line--function whale-line-test--get-segment
         (lambda nil t)
         "Get the `test' segment.")
       (whale-line--setup test :setup
         (lambda nil t)
         :teardown
         (lambda nil t))
       (whale-line-add-segment 'test 't)))))

(ert-deftest whale-line-create-dynamic-segment--using-symbol ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line-create-dynamic-segment test
       :getter ignore
       :condition buffer-file-name
       :dense t)
     '(progn
       (defun whale-line-test--segment ()
         "Render `test' segment."
         (or
          (when buffer-file-name
            (whale-line-test--get-segment))
          ""))
       (whale-line--function whale-line-test--get-segment ignore "Get the `test' segment.")
       (whale-line--setup test :setup nil :teardown nil)
       (whale-line-add-segment 'test 't)))))

(ert-deftest whale-line-create-augment ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line-create-augment test
       :verify (lambda () t)
       :action ignore
       :setup (lambda () t)
       :teardown (lambda () t))

     '(progn
       (whale-line--function whale-line-test--action ignore
         "Augment function for `test'." t)
       (whale-line--setup test
         :hooks nil
         :advice nil
         :setup (lambda nil t)
         :teardown (lambda nil t))))))

(ert-deftest whale-line-create-augment--using-symbol ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line-create-augment test
       :action (lambda () t)
       :hooks (emacs-start-up)
       :advice (:after . (kill-line)))
     '(progn
       (whale-line--function whale-line-test--action
         (lambda nil t)
         "Augment function for `test'." t)
       (whale-line--setup test
         :hooks (emacs-start-up)
         :advice (:after kill-line)
         :setup nil
         :teardown nil)))))

(ert-deftest whale-line--function--lambda ()
  (bydi-match-expansion
   (whale-line--function test-fun (lambda () t) "Docs.")
   '(defun test-fun
       (&rest _args)
     "Docs."
     (lambda nil t))))

(ert-deftest whale-line--function--sym ()
  (bydi-match-expansion
   (whale-line--function test-fun other-test-fun "Docs.")
   '(defun test-fun (&rest _args)
     "Docs."
     (funcall 'other-test-fun))))

(ert-deftest whale-line--function--lambda-apply ()
  (bydi-match-expansion
   (whale-line--function test-fun (lambda () t) "Docs." t)
   '(defun test-fun (&rest args)
     "Docs."
     (apply (lambda () t) args))))

(ert-deftest whale-line--function--sym-apply ()
  (bydi-match-expansion
   (whale-line--function test-fun other-test-fun "Docs." t)
   '(defun test-fun (&rest args)
     "Docs."
     (apply 'other-test-fun args))))

(ert-deftest whale-line--setup ()
  (whale-line--setup test
    :setup (lambda () t)
    :teardown (lambda () t)
    :hooks (first-hook second-hook)
    :advice (:after . (one two)))
  '(progn
    (defun whale-line-test--setup (&rest _)
      "Set up test segment."
      (add-hook 'first-hook #'whale-line-test--action)
      (add-hook 'second-hook #'whale-line-test--action)
      (advice-add 'one :after #'whale-line-test--action)
      (advice-add 'two :after #'whale-line-test--action)
      (funcall (lambda nil t)))

    (add-hook 'whale-line-setup-hook #'whale-line-test--setup)

    (defun whale-line-test--teardown (&rest _)
      "Tear down test segment."
      (remove-hook 'first-hook #'whale-line-test--action)
      (remove-hook 'second-hook #'whale-line-test--action)
      (advice-remove 'one #'whale-line-test--action)
      (advice-remove 'two #'whale-line-test--action)
      (funcall (lambda nil t)))

    (add-hook 'whale-line-teardown-hook #'whale-line-test--teardown)))

(ert-deftest whale-line--setup--using-symbols ()
  (whale-line--setup test
    :setup one
    :teardown two)
  '(progn
    (defun whale-line-test--setup (&rest _)
      "Set up test segment."
      (funcall 'one))

    (add-hook 'whale-line-setup-hook #'whale-line-test--setup)

    (defun whale-line-test--teardown (&rest _)
      "Tear down test segment."

      (funcall 'two))
    (add-hook 'whale-line-teardown-hook #'whale-line-test--teardown)))

;;; whale-line-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
