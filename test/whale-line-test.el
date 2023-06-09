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

(ert-deftest whale-line-selection--get-columns ()
  (with-temp-buffer
    (insert-file-contents rectangle)
    (should (eq (whale-line-selection--get-columns 1 30) 9))))

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

;;; whale-line-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
