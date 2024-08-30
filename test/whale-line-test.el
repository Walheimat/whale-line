;;; whale-line-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'whale-line)

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
       (whale-line--set-props 'test 'stateful 't 'nil 'nil)
       (defvar-local whale-line-test--render 'initial)
       (defun whale-line-test--setter
           (&rest _)
         "Set `test' segment.\nTriggered by `test-mode-hook'."
         (if-let
             ((str
               (whale-line-test--getter)))
             (setq whale-line-test--render str)
           (setq whale-line-test--render nil)))
       (whale-line--function whale-line-test--getter
         (lambda nil t)
         "Get the test segment.")
       (whale-line--setup test :setup
         (lambda nil t)
         :advice nil :hooks
         (test-mode-hook)
         :teardown
         (lambda nil t)
         :verify nil)))))

(ert-deftest whale-line--create-stateful-segment--simple ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateful-segment test
       :getter (lambda () t))
     '(progn
       (whale-line--set-props 'test 'stateful 't 'nil 'nil)
       (defvar-local whale-line-test--render 'initial)
       (defun whale-line-test--setter
           (&rest _)
         "Set `test' segment."
         (if-let
             ((str
               (whale-line-test--getter)))
             (setq whale-line-test--render str)
           (setq whale-line-test--render nil)))
       (whale-line--function whale-line-test--getter
         (lambda nil t)
         "Get the test segment.")
       (whale-line--setup test :setup nil :advice nil :hooks nil :teardown nil :verify nil)))))

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
       (whale-line--set-props 'test 'stateful 'low 'nil 'nil)
       (defvar-local whale-line-test--render 'initial)
       (defun whale-line-test--setter (&rest _)
         "Set `test' segment."
         (if-let ((str (whale-line-test--getter)))
             (setq whale-line-test--render str)
           (setq whale-line-test--render nil)))

       (whale-line--function whale-line-test--getter ignore "Get the test segment.")

       (whale-line--setup test
         :setup ignore
         :advice (:before ancient old)
         :hooks nil
         :teardown ignore
         :verify t)

       (whale-line--function whale-line-test--verify (lambda () t) "Verify `test' segment." t)))))

(ert-deftest whale-line--create-stateful--with-port ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateful-segment test
       :port test-port)
     '(progn
       (whale-line--set-props 'test 'stateful 't 'nil 'nil)
       (defvar-local whale-line-test--render 'initial)
       (defun whale-line-test--setter
           (&rest _)
         "Set `test' segment."
         (if-let
             ((str
               (whale-line-test--getter)))
             (setq whale-line-test--render str)
           (setq whale-line-test--render nil)))
       (whale-line--function whale-line-test--getter nil "Get the test segment.")
       (whale-line--setup test :setup nil :advice nil :hooks nil :teardown nil :verify nil)
       (whale-line--function whale-line-test--port test-port "Plug into `test-port'" t)))))

(ert-deftest whale-line--create-with-single-item-hook ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateful-segment test
       :hooks change-major-mode-hook)
     '(progn
       (whale-line--set-props 'test 'stateful 't 'nil 'nil)
       (defvar-local whale-line-test--render 'initial)
       (defun whale-line-test--setter
           (&rest _)
         "Set `test' segment.\nTriggered by `change-major-mode-hook'."
         (if-let
             ((str
               (whale-line-test--getter)))
             (setq whale-line-test--render str)
           (setq whale-line-test--render nil)))
       (whale-line--function whale-line-test--getter nil "Get the test segment.")
       (whale-line--setup test :setup nil :advice nil :hooks
         (change-major-mode-hook)
         :teardown nil :verify nil)))))

(ert-deftest whale-line--create-stateful--advised ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateful-segment test
       :getter test-getter
       :after (some-fun))
     '(progn
       (whale-line--set-props 'test 'stateful 't 'nil 'nil)
       (defvar-local whale-line-test--render 'initial)
       (defun whale-line-test--setter
           (&rest _)
         "Set `test' segment.\nTriggered by `some-fun'."
         (if-let
             ((str
               (whale-line-test--getter)))
             (setq whale-line-test--render str)
           (setq whale-line-test--render nil)))
       (whale-line--function whale-line-test--getter test-getter "Get the test segment.")
       (whale-line--setup test :setup nil :advice
         (:after some-fun)
         :hooks nil :teardown nil :verify nil)))

    (bydi-match-expansion
     (whale-line--create-stateful-segment test
       :getter test-getter
       :advice (:after . (some-fun other-fun)))
     '(progn
       (whale-line--set-props 'test 'stateful 't 'nil 'nil)
       (defvar-local whale-line-test--render 'initial)
       (defun whale-line-test--setter
           (&rest _)
         "Set `test' segment."
         (if-let
             ((str
               (whale-line-test--getter)))
             (setq whale-line-test--render str)
           (setq whale-line-test--render nil)))
       (whale-line--function whale-line-test--getter test-getter "Get the test segment.")
       (whale-line--setup test :setup nil :advice
         (:after some-fun other-fun)
         :hooks nil :teardown nil :verify nil)))))

(ert-deftest whale-line--create-stateless-segment ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateless-segment test
       :getter (lambda () t)
       :verify (lambda () t)
       :teardown (lambda () t)
       :setup (lambda () t))
     '(progn
       (whale-line--set-props 'test 'stateless 't 'nil 'nil)
       (defun whale-line-test--render ()
         "Render `test' segment."
         (or
          (when t
            (whale-line-test--getter))
          ""))
       (whale-line--function whale-line-test--getter
         (lambda nil t)
         "Get the `test' segment.")
       (whale-line--setup test :setup (lambda nil t) :teardown (lambda nil t) :verify t)
       (whale-line--function whale-line-test--verify (lambda () t) "Verify `test' segment." t)))))

(ert-deftest whale-line--create-stateless-segment--with-variable ()
  (whale-line-do-expand
   (defvar test-variable-segment "test")
    (bydi-match-expansion
     (whale-line--create-stateless-segment test
       :var test-variable-segment)
     '(progn
       (whale-line--set-props 'test 'stateless 't 'nil 'nil)
       (defun whale-line-test--render ()
         "Render `test' segment."
         (or (when t test-variable-segment) ""))
       (whale-line--setup test :setup nil :teardown nil :verify nil)))))

(ert-deftest whale-line--create-stateless-segment--using-symbol ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateless-segment test
       :getter ignore
       :condition buffer-file-name
       :dense t)
     '(progn
       (whale-line--set-props 'test 'stateless 't 't 'nil)
       (defun whale-line-test--render ()
         "Render `test' segment."
         (or
          (when buffer-file-name
            (whale-line-test--getter))
          ""))
       (whale-line--function whale-line-test--getter ignore "Get the `test' segment.")
       (whale-line--setup test :setup nil :teardown nil :verify nil)))))

(ert-deftest whale-line--create-stateless-segment--with-port ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-stateless-segment test
       :port test-port)
     '(progn
       (whale-line--set-props 'test 'stateless 't 'nil 'nil)
       (defun whale-line-test--render nil "Render `test' segment."
              (or
               (when t
                 (whale-line-test--getter))
               ""))
       (whale-line--function whale-line-test--getter nil "Get the `test' segment.")
       (whale-line--setup test :setup nil :teardown nil :verify nil)
       (whale-line--function whale-line-test--port test-port "Plug into `test-port'." t)))))

(ert-deftest whale-line--create-augment ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-augment test
       :action ignore
       :setup (lambda () t)
       :teardown (lambda () t))

     '(progn
       (whale-line--set-props 'test 'augment)
       (whale-line--function whale-line-test--setter ignore
         "Augment function for `test'." t)
       (whale-line--setup test
         :hooks nil
         :advice nil
         :setup (lambda nil t)
         :teardown (lambda nil t)
         :verify t)
       (whale-line--function whale-line-test--verify always
         "Verify `test' augment." t)))))

(ert-deftest whale-line--create-augment--using-symbol ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-augment test
       :action (lambda () t)
       :hooks (emacs-startup-hook kill-emacs-hook)
       :verify (lambda () t))
     '(progn
       (whale-line--set-props 'test 'augment)
       (whale-line--function whale-line-test--setter
         (lambda nil t)
         "Augment function for `test'.\nTriggered by `emacs-startup-hook' and `kill-emacs-hook'." t)
       (whale-line--setup test
         :hooks (emacs-startup-hook kill-emacs-hook)
         :advice nil
         :setup nil
         :teardown nil
         :verify t)
       (whale-line--function whale-line-test--verify
         (lambda nil t)
         "Verify `test' augment." t)))

    (bydi-match-expansion
     (whale-line--create-augment test
       :action (lambda () t)
       :after kill-line)
     '(progn
       (whale-line--set-props 'test 'augment)
       (whale-line--function whale-line-test--setter
         (lambda nil t)
         "Augment function for `test'.\nTriggered by `kill-line'." t)
       (whale-line--setup test :hooks nil :advice
         (:after kill-line)
         :setup nil :teardown nil :verify t)
       (whale-line--function whale-line-test--verify always "Verify `test' augment." t)))

    (bydi-match-expansion
     (whale-line--create-augment test
       :action (lambda () t)
       :after-while (kill-line forward-line))
     '(progn
       (whale-line--set-props 'test 'augment)
       (whale-line--function whale-line-test--setter
         (lambda nil t)
         "Augment function for `test'.\nTriggered by `kill-line' and `forward-line'." t)
       (whale-line--setup test :hooks nil :advice
         (:after-while kill-line forward-line)
         :setup nil :teardown nil :verify t)
       (whale-line--function whale-line-test--verify always "Verify `test' augment." t)))))

(ert-deftest whale-line--create-augment--with-plug ()
  (whale-line-do-expand
    (bydi-match-expansion
     (whale-line--create-augment test
       :action test-action
       :plugs-into slot)
     '(progn
       (whale-line--set-props 'test 'augment)
       (whale-line--function whale-line-test--setter
         (lambda
           (&rest r)
           (apply 'whale-line-slot--port
                  (apply 'test-action r)))
         "Augment function for `test'.\nPlugs into `whale-line-slot--port'." t)
       (whale-line--setup test :hooks nil :advice nil :setup nil :teardown nil :verify t)
       (whale-line--function whale-line-test--verify always "Verify `test' augment." t)))))

;;;; Priorities

(ert-deftest whale-line--update-priorities ()
  (let ((whale-line--props '((one :priority nil) (two :priority nil) (three :priority nil))))

    (whale-line--update-priorities '(one three) 'current)

    (should (equal '((one :priority current) (two :priority nil) (three :priority current))
                   whale-line--props))))

(ert-deftest whale-line-with-priorities ()
  (bydi-match-expansion
   (whale-line-with-priorities
     major-mode
     buffer-status
     current
     project
     low
     lsp
     t)
   '(progn
      (whale-line--update-priorities
       '(lsp)
       't)
      (whale-line--update-priorities
       '(project)
       'low)
      (whale-line--update-priorities
       '(major-mode buffer-status)
       'current))))

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
  (should (string= "" (whale-line--spacer t)))

  (bydi ((:spy propertize))
    (let ((whale-line--debug-padding t))

      (should (string= " " (whale-line--spacer)))
      (bydi-was-called propertize))))

(ert-deftest whale-line--format-side ()
  (bydi (format-mode-line
         (:mock whale-line--render :return "test"))
    (whale-line--format-side :left 'filter)

    (bydi-was-called-with whale-line--render '(:left filter))
    (bydi-was-called-with format-mode-line "test")))

(ert-deftest calculate-space ()
  (bydi ((:always whale-line--enough-space-p))
    (let ((whale-line--space-cache (make-hash-table)))

      (whale-line--calculate-space)

      (should (length> (hash-table-keys whale-line--space-cache) 0)))))

(ert-deftest whale-line--enough-space ()
  (let ((left "left")
        (right "right")
        (width 10)
        (whale-line--space-cache (make-hash-table)))

    (bydi ((:mock window-font-width :return 1)
           (:mock window-pixel-width :return  width)
           (:mock whale-line--format-side :with (lambda (side &optional _)
                                                  (pcase side
                                                    (:left left)
                                                    (:right right)))))
      (should (whale-line--enough-space-p))
      (should (whale-line--enough-space-p))
      (setq width 8)
      (clrhash whale-line--space-cache)
      (should-not (whale-line--enough-space-p))
      (bydi-was-called-with whale-line--format-side '(... none)))))

(ert-deftest whale-line--enough-space--old-calculation ()
  (let ((left "left")
        (right "right")
        (width 10)
        (whale-line--space-cache (make-hash-table)))

    (bydi ((:risky-mock fboundp :with ignore)
           (:mock window-font-width :return 1)
           (:mock window-pixel-width :return  width)
           (:mock whale-line--format-side :with (lambda (side &optional _)
                                                  (pcase side
                                                    (:left left)
                                                    (:right right)))))
      (should (whale-line--enough-space-p))
      (setq width 8)
      (clrhash whale-line--space-cache)
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
  (let ((whale-line--props '((a :type stateful) (b :type stateless) (c :type stateful))))

    (defun whale-line-a--setter () nil)
    (defun whale-line-b--setter () nil)
    (defun whale-line-c--setter () nil)

    (bydi ((:spy whale-line-a--setter)
           (:spy whale-line-b--setter)
           (:spy whale-line-c--setter)
           whale-line-debug)

      (whale-line--refresh-stateful-segments)

      (bydi-was-called whale-line-debug)

      (bydi-was-called whale-line-a--setter)
      (bydi-was-not-called whale-line-b--setter)
      (bydi-was-called whale-line-c--setter))))

(ert-deftest whale-line--is-current-window-p ()

  (let ((whale-line--current-window nil))

    (bydi ((:mock whale-line--get-current-window :with bydi-rt))
      (should-not (whale-line--is-current-window-p))

      (setq whale-line--current-window 'resting)

      (should-not (whale-line--is-current-window-p))

      (setq whale-line--current-window 'testing)

      (should (whale-line--is-current-window-p)))))

(ert-deftest whale-line--set-props ()
  (let ((whale-line--props '((one :priority nil :type stateful :dense nil)
                             (two :priority t :type stateless))))

    (whale-line--set-props 'one 'stateful)

    (should (equal whale-line--props '((one :type stateful :priority t :dense nil :padded nil)
                                       (two :priority t :type stateless))))

    (whale-line--set-props 'two 'stateful 'low)

    (should (equal whale-line--props '((one :type stateful :priority t :dense nil :padded nil)
                                       (two :type stateful :priority low :dense nil :padded nil))))

    (whale-line--set-props 'three 'stateless nil t t)

    (should (equal whale-line--props '((three :type stateless :priority t :dense t :padded t)
                                       (one :type stateful :priority t :dense nil :padded nil)
                                       (two :type stateful :priority low :dense nil :padded nil))))))

(ert-deftest whale-line--valid-segment-p ()
  (let ((verifies nil))
    (defun whale-line-test--verify ()
      "Does it verify?"
      verifies)

    (should-not (whale-line--valid-segment-p 'test))

    (setq verifies t)

    (should (whale-line--valid-segment-p 'test))))

(ert-deftest whale-line--pad-segment ()
  (let ((whale-line--segments '(:left (one three five) :right (two four six)))
        (whale-line--props '((four :dense t) (five :dense always) (six :dense problematic))))

    (should (equal '(" " "test") (whale-line--pad-segment 'one "test")))
    (should (equal '("test" " ") (whale-line--pad-segment 'two "test")))
    (should (equal '(" " "test") (whale-line--pad-segment 'three '("test"))))
    (should (equal '("test" "") (whale-line--pad-segment 'four "test")))
    (should (equal '("" "test") (whale-line--pad-segment 'five "test")))

    (bydi ((:mock format-mode-line :return ""))
      (should (equal '("test" "") (whale-line--pad-segment 'six "test"))))))

(ert-deftest pad-segment--pre-padded ()
  (let ((whale-line--segments '(:left (one two three) :right (four five six)))
        (whale-line--props '((two :padded all) (five :padded left))))

    (should (equal '(" " "one") (whale-line--pad-segment 'one "one")))
    (should (equal '("two") (whale-line--pad-segment 'two "two")))
    (should (equal '("three") (whale-line--pad-segment 'three "three")))
    (should (equal '("four") (whale-line--pad-segment 'four "four")))
    (should (equal '("five" " ") (whale-line--pad-segment 'five "five")))
    (should (equal '("six" " ") (whale-line--pad-segment 'six "six")))))

(ert-deftest whale-line--filter ()
  (let ((current nil)
        (segments '(a b c d))
        (whale-line--props '((a :priority low) (b :priority current-low) (c :priority t) (d :priority current))))

    (bydi ((:mock whale-line--is-current-window-p :return current))

      (should (equal (whale-line--filter segments)
                     '(a c)))

      (should (equal (whale-line--filter segments t)
                     '(c)))

      (setq current t)

      (should (equal (whale-line--filter segments)
                     '(a b c d)))

      (should (equal (whale-line--filter segments t)
                     '(c d))))))

(ert-deftest whale-line--render ()
  (let ((whale-line--segments '(:left ((one . t)) :right ((two . nil)))))

    (bydi ((:mock whale-line--filter :return '((one . t)))
           whale-line--render-segments)

      (whale-line--render :left)

      (bydi-was-called-with whale-line--render-segments (list '((one . t))))
      (bydi-was-called whale-line--filter)

      (whale-line--render :left 'none)

      (bydi-clear-mocks)

      (bydi-was-not-called whale-line--filter))))

(ert-deftest whale-line--render-segments ()
  (bydi ((:spy functionp))

    (bydi-when functionp :called-with (list 'whale-line-three--render) :then-return t :once t)

    (defvar whale-line-one--render)
    (defvar whale-line-four--render)

    (let ((segments '(one three four))
          (whale-line-one--render "one")
          (whale-line-four--render 'initial))

      (should (equal '((:eval (whale-line--pad-segment 'one whale-line-one--render))
                       (:eval (whale-line--pad-segment 'three (whale-line-three--render)))
                       (:eval (whale-line--pad-segment 'four (whale-line-four--setter))))
                     (whale-line--render-segments segments))))))

(ert-deftest whale-line--setup ()
  (let ((whale-line--props '((test :type stateful))))
    (bydi-match-expansion
     (whale-line--setup test
       :setup (lambda () t)
       :teardown (lambda () t)
       :hooks (first-hook second-hook)
       :advice (:after . (one two)))
     '(progn
       (cl-defun whale-line-test--setup (&rest _)
         "Set up test segment."
         (unless (memq 'test whale-line-segments)
           (cl-return-from whale-line-test--setup))
         (whale-line-log "Setting up `test' (%s)" (whale-line--prop 'test :type))
         (add-hook 'first-hook #'whale-line-test--setter)
         (add-hook 'second-hook #'whale-line-test--setter)
         (advice-add 'one :after #'whale-line-test--setter)
         (advice-add 'two :after #'whale-line-test--setter)
         (funcall (lambda nil t)))

       (add-hook 'whale-line-setup-hook #'whale-line-test--setup)

       (cl-defun whale-line-test--teardown (&rest _)
         "Tear down test segment."
         (unless (memq 'test whale-line-segments)
           (cl-return-from whale-line-test--teardown))
         (whale-line-log "Tearing down `test' (%s)" (whale-line--prop 'test :type))
         (remove-hook 'first-hook #'whale-line-test--setter)
         (remove-hook 'second-hook #'whale-line-test--setter)
         (advice-remove 'one #'whale-line-test--setter)
         (advice-remove 'two #'whale-line-test--setter)
         (funcall (lambda nil t)))

       (add-hook 'whale-line-teardown-hook #'whale-line-test--teardown)))))

(ert-deftest whale-line--setup--empty-setup ()
  (bydi-match-expansion
   (whale-line--setup test)
   '(progn
      (cl-defun whale-line-test--setup
          (&rest _)
        "Set up test segment."
        (unless
            (memq 'test whale-line-segments)
          (cl-return-from whale-line-test--setup))
        (whale-line-log "Segment `test' (%s) requires no setup" (whale-line--prop 'test :type)))
      (add-hook 'whale-line-setup-hook #'whale-line-test--setup)
      (cl-defun whale-line-test--teardown
          (&rest _)
        "Tear down test segment."
        (unless
            (memq 'test whale-line-segments)
          (cl-return-from whale-line-test--teardown))
        (whale-line-log "Segment `test' (%s) requires no teardown" (whale-line--prop 'test :type)))
      (add-hook 'whale-line-teardown-hook #'whale-line-test--teardown))))

(ert-deftest whale-line--setup--early-return ()
  (let ((whale-line--props '((test :type stateless))))
    (bydi-match-expansion
     (whale-line--setup test
       :setup (lambda () t)
       :teardown (lambda () t)
       :hooks (first-hook second-hook)
       :advice (:after . (one two))
       :verify t)
     '(progn
       (cl-defun whale-line-test--setup (&rest _)
         "Set up test segment."
         (unless (and (not whale-line--rebuilding) (whale-line-test--verify))
           (cl-return-from whale-line-test--setup))
         (whale-line-log "Setting up `test' (%s)" (whale-line--prop 'test :type))
         (add-hook 'first-hook #'whale-line-test--setter)
         (add-hook 'second-hook #'whale-line-test--setter)
         (advice-add 'one :after #'whale-line-test--setter)
         (advice-add 'two :after #'whale-line-test--setter)
         (funcall (lambda nil t)))

       (add-hook 'whale-line-setup-hook #'whale-line-test--setup)

       (cl-defun whale-line-test--teardown (&rest _)
         "Tear down test segment."
         (unless (and (not whale-line--rebuilding) (whale-line-test--verify))
           (cl-return-from whale-line-test--teardown))
         (whale-line-log "Tearing down `test' (%s)" (whale-line--prop 'test :type))
         (remove-hook 'first-hook #'whale-line-test--setter)
         (remove-hook 'second-hook #'whale-line-test--setter)
         (advice-remove 'one #'whale-line-test--setter)
         (advice-remove 'two #'whale-line-test--setter)
         (funcall (lambda nil t)))

       (add-hook 'whale-line-teardown-hook #'whale-line-test--teardown)))))

(ert-deftest whale-line--setup--using-symbols ()
  (let ((whale-line--props '((test :type augment))))
    (bydi-match-expansion
     (whale-line--setup test
       :setup one
       :teardown two)
     '(progn
       (cl-defun whale-line-test--setup (&rest _)
         "Set up test segment."
         (unless (memq 'test whale-line-segments)
           (cl-return-from whale-line-test--setup))
         (whale-line-log "Setting up `test' (%s)" (whale-line--prop 'test :type))
         (funcall 'one))

       (add-hook 'whale-line-setup-hook #'whale-line-test--setup)

       (cl-defun whale-line-test--teardown (&rest _)
         "Tear down test segment."
         (unless (memq 'test whale-line-segments)
           (cl-return-from whale-line-test--teardown))
         (whale-line-log "Tearing down `test' (%s)" (whale-line--prop 'test :type))
         (funcall 'two))
       (add-hook 'whale-line-teardown-hook #'whale-line-test--teardown)))))

(ert-deftest whale-line--build-segments ()
  (let ((whale-line--props '((one :priority low) (two :priority high)))
        (whale-line-segments '(one | two))
        (whale-line--segments nil))

    (bydi ((:always whale-line--valid-segment-p))
      (whale-line--build-segments)

      (should (equal '(:left (one) :right (two))
                     whale-line--segments)))))

(ert-deftest whale-line--trigger-augments ()
  (let ((whale-line--props '((one :type stateful) (two :type augment) (three :type augment))))

    (bydi ((:watch whale-line-segments)
           run-hooks)

      (whale-line--trigger-augments)

      (bydi-was-set-to whale-line-segments '(two three))
      (bydi-was-called-with run-hooks 'whale-line-setup-hook :clear t)

      (whale-line--trigger-augments t)

      (bydi-was-called-with run-hooks 'whale-line-teardown-hook))))

(ert-deftest whale-line-log--formats ()
  (let ((whale-line-log nil))

    (whale-line-log "This is a %s" "test")

    (should-not (get-buffer whale-line-log--buffer-name))

    (setq whale-line-log 0)

    (whale-line-log "This is the %s message" "first")
    (whale-line-debug "This %s the %s message" "will be" "second")

    (with-current-buffer (get-buffer whale-line-log--buffer-name)
      (should (string= (buffer-string)
                       "This is the first message\nThis will be the second message\n")))))

(ert-deftest whale-line--handle-build-difference ()
  (let ((whale-line-segments '(one two three))
        (whale-line--last-build nil))

    (shut-up
      (bydi (whale-line-log)

        (ert-with-message-capture messages

          (whale-line--handle-build-difference)

          (bydi-was-not-called whale-line-log)

          (whale-line--handle-build-difference)

          (bydi-was-not-called whale-line-log)

          (setq whale-line-segments '(two four))

          (whale-line--handle-build-difference)

          (bydi-was-called-nth-with whale-line-log '("Added segment(s) %s since last build" (four)) 0)
          (bydi-was-called-nth-with whale-line-log '("Removed segment(s) %s since last build" (one three)) 1))))))

(ert-deftest whale-line--normalize-list ()
  (should (equal (whale-line--normalize-list '(test))
                 (whale-line--normalize-list 'test))))

(ert-deftest whale-line-mode--setup ()
  (let ((mode-line-format 'format)
        (whale-line--default-mode-line nil))

    (bydi ((:mock require :return t)
           (:sometimes whale-line--build-segments)
           add-hook
           run-hooks
           (:watch mode-line-format)
           (:watch whale-line--default-mode-line))
      (whale-line-mode--setup)

      (bydi-was-called-with run-hooks (list 'whale-line-setup-hook))
      (bydi-was-called-nth-with add-hook (list 'pre-redisplay-functions #'whale-line--set-selected-window) 0)
      (bydi-was-called-nth-with add-hook (list 'window-configuration-change-hook #'whale-line--calculate-space) 1)
      (bydi-was-called-nth-with add-hook (list 'buffer-list-update-hook #'whale-line--queue-refresh) 2)

      (bydi-was-set-to mode-line-format whale-line-mode-line)
      (bydi-was-set-to whale-line--default-mode-line 'format)

      (bydi-toggle-sometimes)

      (should-error (whale-line-mode--setup)))))

(ert-deftest whale-line-mode--teardown ()
  (let ((mode-line-format 'whale)
        (whale-line--default-mode-line 'other))

    (bydi (run-hooks
           remove-hook
           (:watch mode-line-format))
      (whale-line-mode--teardown)

      (bydi-was-called-with run-hooks (list 'whale-line-teardown-hook))
      (bydi-was-called-nth-with remove-hook (list 'pre-redisplay-functions #'whale-line--set-selected-window) 0)
      (bydi-was-called-nth-with remove-hook (list 'window-configuration-change-hook #'whale-line--calculate-space) 1)
      (bydi-was-called-nth-with remove-hook (list 'buffer-list-update-hook #'whale-line--queue-refresh) 2)
      (bydi-was-set-to mode-line-format 'other))))

(ert-deftest whale-line-mode ()
  (bydi (whale-line-mode--setup whale-line-mode--teardown)
    (let ((whale-line-mode nil))
      (whale-line-mode)

      (bydi-was-called whale-line-mode--setup)

      (whale-line-mode -1)

      (bydi-was-called whale-line-mode--teardown))))

(ert-deftest whale-line-rebuild ()
  (bydi (whale-line--build-segments)

    (whale-line-rebuild)
    (bydi-was-called whale-line--build-segments)))

(ert-deftest whale-line-trigger-augments ()
  (bydi (whale-line--trigger-augments)

    (whale-line-trigger-augments)
    (bydi-was-called whale-line--trigger-augments)))

(ert-deftest whale-line--pop-to-logs ()
  (let ((whale-line-log 0))
    (bydi (pop-to-buffer)
      (whale-line-log "Make sure it exists")

      (whale-line-pop-to-logs)
      (bydi-was-called pop-to-buffer)

      (kill-buffer whale-line-log--buffer-name)

      (should-error (whale-line-pop-to-logs)))))

(ert-deftest whale-line--symbol-for-type--errors-for-unknown ()

  (should-error (whale-line--symbol-for-type 'segment 'something)))

;;; whale-line-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
