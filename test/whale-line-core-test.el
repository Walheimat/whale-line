;;; whale-line-core-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line-core)

;; Macros

(defmacro whale-line-do-expand (&rest body)
  "Expand BODY with testing set to nil."
  (declare (indent 0))
  `(progn
     (setq whale-line--testing nil)
     ,@body
     (setq whale-line--testing t)))

(ert-deftest whale-line--create-stateful-segment ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateful-segment test
       :getter (lambda () t)
       :hooks (test-mode-hook)
       :teardown (lambda () t)
       :setup (lambda () t))
     '(progn
       (defvar-local whale-line-test--segment 'initial)
       (defun whale-line-test--action
           (&rest _)
         "Set test segment."
         (if-let
             ((str
               (whale-line-test--get-segment)))
             (setq whale-line-test--segment str)
           (setq whale-line-test--segment nil)))
       (whale-line--function whale-line-test--get-segment
         (lambda nil t)
         "Get the test segment.")
       (whale-line--setup test :setup
         (lambda nil t)
         :advice nil :hooks
         (test-mode-hook)
         :teardown
         (lambda nil t)
         :verify nil)
       nil
       (whale-line--add-segment 'test 'stateful 't 'nil 'nil)))))

(ert-deftest whale-line--create-stateful-segment--simple ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateful-segment test
       :getter (lambda () t))
     '(progn
       (defvar-local whale-line-test--segment 'initial)
       (defun whale-line-test--action
           (&rest _)
         "Set test segment."
         (if-let
             ((str
               (whale-line-test--get-segment)))
             (setq whale-line-test--segment str)
           (setq whale-line-test--segment nil)))
       (whale-line--function whale-line-test--get-segment
         (lambda nil t)
         "Get the test segment.")
       (whale-line--setup test :setup nil :advice nil :hooks nil :teardown nil :verify nil)
       nil
       (whale-line--add-segment 'test 'stateful 't 'nil 'nil)))))

(ert-deftest whale-line--create-stateful-segment--using-symbols ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateful-segment test
       :getter ignore
       :advice (:before . (ancient old))
       :verify (lambda () t)
       :teardown ignore
       :setup ignore
       :priority low)
     '(progn
       (defvar-local whale-line-test--segment 'initial)
       (defun whale-line-test--action (&rest _)
         "Set test segment."
         (if-let ((str (whale-line-test--get-segment)))
             (setq whale-line-test--segment str)
           (setq whale-line-test--segment nil)))

       (whale-line--function whale-line-test--get-segment ignore "Get the test segment.")

       (whale-line--setup test
         :setup ignore
         :advice (:before ancient old)
         :hooks nil
         :teardown ignore
         :verify t)

       (whale-line--function whale-line-test--verify (lambda () t) "Verify `test' segment." t)

       (whale-line--add-segment 'test 'stateful 'low 'nil 'nil)))))

(ert-deftest whale-line--create-stateless-segment ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateless-segment test
       :getter (lambda () t)
       :verify (lambda () t)
       :teardown (lambda () t)
       :setup (lambda () t))
     '(progn
       (defun whale-line-test--segment ()
         "Render `test' segment."
         (or
          (when t
            (whale-line-test--get-segment))
          ""))
       (whale-line--function whale-line-test--get-segment
         (lambda nil t)
         "Get the `test' segment.")
       (whale-line--setup test :setup (lambda nil t) :teardown (lambda nil t) :verify t)
       (whale-line--function whale-line-test--verify (lambda () t) "Verify `test' segment." t)
       (whale-line--add-segment 'test 'stateless 't 'nil 'nil)))))

(ert-deftest whale-line--create-stateless-segment--using-symbol ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateless-segment test
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
       (whale-line--setup test :setup nil :teardown nil :verify nil)
       nil
       (whale-line--add-segment 'test 'stateless 't 't 'nil)))))

(ert-deftest whale-line--create-augment ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-augment test
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
         :teardown (lambda nil t)
         :verify nil)
       nil
       (whale-line--add-segment 'test 'augment)))))

(ert-deftest whale-line--create-augment--using-symbol ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-augment test
       :action (lambda () t)
       :hooks (emacs-start-up)
       :advice (:after . (kill-line))
       :verify (lambda () t))
     '(progn
       (whale-line--function whale-line-test--action
         (lambda nil t)
         "Augment function for `test'." t)
       (whale-line--setup test
         :hooks (emacs-start-up)
         :advice (:after kill-line)
         :setup nil
         :teardown nil
         :verify t)
       (whale-line--function whale-line-test--verify
         (lambda nil t)
         "Verify `test' augment." t)
       (whale-line--add-segment 'test 'augment)))))

(ert-deftest whale-line--function--lambda ()
  (bydi-match-expansion
   (whale-line--function test-fun (lambda () t) "Docs.")
   '(defun test-fun
       (&rest _args)
     "Docs."
     (funcall (lambda nil t)))))

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

(ert-deftest whale-line--spacer ()
  (should (string= " " (whale-line--spacer)))
  (should (string= "" (whale-line--spacer t))))

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

(ert-deftest whale-line--enough-space--old-calculation ()
  (let ((left "left")
        (right "right")
        (width 10))

    (bydi ((:risky-mock fboundp :with ignore)
           (:mock window-font-width :return 1)
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
           (:mock minibuffer-selected-window :with (lambda () (if active 'selected nil)))
           whale-line--queue-refresh)

      (whale-line--set-selected-window)

      (should (eq 'testing whale-line--current-window))

      (setq active t)

      (whale-line--set-selected-window)

      (should (eq 'selected whale-line--current-window)))))

(ert-deftest whale-line--queue-refresh ()
  (let ((whale-line--stateful-timer nil)
        (timer (timer--create)))

    (setf (timer--triggered timer) nil)

    (bydi ((:mock run-with-idle-timer :return timer)
           cancel-timer)

      (whale-line--queue-refresh)
      (should whale-line--stateful-timer)

      (bydi-was-called-with run-with-idle-timer '(0.5 nil whale-line--refresh-stateful-segments))
      (bydi-was-not-called cancel-timer)

      (whale-line--queue-refresh)

      (bydi-was-called cancel-timer))

    (cancel-timer timer)))

(ert-deftest whale-line--refresh-stateful-segments ()
  (let ((whale-line--type '((a . stateful) (b . stateless) (c . stateful))))

    (defun whale-line-a--action () nil)
    (defun whale-line-b--action () nil)
    (defun whale-line-c--action () nil)

    (bydi ((:spy whale-line-a--action)
           (:spy whale-line-b--action)
           (:spy whale-line-c--action))

      (whale-line--refresh-stateful-segments)

      (bydi-was-called whale-line-a--action)
      (bydi-was-not-called whale-line-b--action)
      (bydi-was-called whale-line-c--action))))

(ert-deftest whale-line--is-current-window-p ()

  (let ((whale-line--current-window nil))

    (bydi ((:mock whale-line--get-current-window :with bydi-rt))
      (should-not (whale-line--is-current-window-p))

      (setq whale-line--current-window 'resting)

      (should-not (whale-line--is-current-window-p))

      (setq whale-line--current-window 'testing)

      (should (whale-line--is-current-window-p)))))

(ert-deftest whale-line--add-segment ()
  (let ((whale-line--priority '((one . nil) (two . t)))
        (whale-line--type '((one . stateful) (two . stateless)))
        (whale-line--dense '((one . nil))))

    (whale-line--add-segment 'one 'stateful)

    (should (equal whale-line--priority '((one . t) (two . t))))
    (should (equal whale-line--type '((one . stateful) (two . stateless))))

    (whale-line--add-segment 'two 'stateful 'low)

    (should (equal whale-line--priority '((one . t) (two . low))))
    (should (equal whale-line--type '((one . stateful) (two . stateful))))

    (whale-line--add-segment 'three 'stateful 'current-low)

    (should (equal whale-line--priority '((three . current-low) (one . t) (two . low))))

    (whale-line--add-segment 'one 'stateful nil t)

    (should (equal whale-line--dense '((three) (two) (one . t))))))

(ert-deftest whale-line--valid-segment-p ()
  (let ((verifies nil))
    (defun whale-line-test--verify ()
      "Does it verify?"
      verifies)

    (should-not (whale-line--valid-segment-p 'test))

    (setq verifies t)

    (should (whale-line--valid-segment-p 'test))))

(ert-deftest whale-line--pad-segment ()
  (let ((whale-line--segments '(:left ((one . t) (three . t) (five . t)) :right ((two . t) (four . t))))
        (whale-line--dense '((four . t) (five . always))))

    (should (equal '(" " "test") (whale-line--pad-segment 'one "test")))
    (should (equal '("test" " ") (whale-line--pad-segment 'two "test")))
    (should (equal '(" " "test") (whale-line--pad-segment 'three '("test"))))
    (should (equal '("test" "") (whale-line--pad-segment 'four "test")))
    (should (equal '("" "test") (whale-line--pad-segment 'five "test")))))

(ert-deftest pad-segment--pre-padded ()
  (let ((whale-line--segments '(:left ((one . t) (two . t) (three . t)) :right ((four . t) (five . t) (six . t))))
        (whale-line--padded '((two . all) (five . left))))

    (should (equal '(" " "one") (whale-line--pad-segment 'one "one")))
    (should (equal '("two") (whale-line--pad-segment 'two "two")))
    (should (equal '("three") (whale-line--pad-segment 'three "three")))
    (should (equal '("four") (whale-line--pad-segment 'four "four")))
    (should (equal '("five" " ") (whale-line--pad-segment 'five "five")))
    (should (equal '("six" " ") (whale-line--pad-segment 'six "six")))))

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

      (should (equal '((:eval (whale-line--pad-segment 'one whale-line-one--segment))
                       (:eval (whale-line--pad-segment 'three (whale-line-three--segment)))
                       (:eval (whale-line--pad-segment 'four (whale-line-four--action))))
                     (whale-line--render-segments segments))))))

(ert-deftest whale-line--setup ()
  (whale-line--setup test
    :setup (lambda () t)
    :teardown (lambda () t)
    :hooks (first-hook second-hook)
    :advice (:after . (one two)))
  '(progn
    (cl-defun whale-line-test--setup (&rest _)
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

(ert-deftest whale-line--setup--early-return ()
  (whale-line--setup test
    :setup (lambda () t)
    :teardown (lambda () t)
    :hooks (first-hook second-hook)
    :advice (:after . (one two))
    :verify t)
  '(progn
    (cl-defun whale-line-test--setup (&rest _)
      "Set up test segment."
      (unless (whale-line-test--verify)
        (cl-return-from whale-line-test--setup))
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

(ert-deftest whale-line--build-segments ()
  (let ((whale-line--priority '((one . low) (two . high)))
        (whale-line-segments '(one | two))
        (whale-line--segments nil))

    (bydi ((:always whale-line--valid-segment-p))
      (whale-line--build-segments)

      (should (equal '(:left ((one . low)) :right ((two . high)))
                     whale-line--segments)))))

;;; whale-line-core-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
