;; -*- lexical-binding: t -*-

(require 'pdf-roll)
(require 'ert)

;; Tests for pdf-roll.el - continuous scroll functionality.
;; Many pdf-roll functions require window context, so these tests focus on
;; utility functions and basic mode setup that can be tested in batch mode.

;;; Utility function tests

(ert-deftest pdf-roll-page-to-pos-basic ()
  "Test pdf-roll-page-to-pos returns correct buffer positions."
  ;; Page 1 should be at position 1
  (should (= (pdf-roll-page-to-pos 1) 1))
  ;; Page 2 should be at position 5
  (should (= (pdf-roll-page-to-pos 2) 5))
  ;; Page 3 should be at position 9
  (should (= (pdf-roll-page-to-pos 3) 9))
  ;; Page 10 should be at position 37
  (should (= (pdf-roll-page-to-pos 10) 37)))

(ert-deftest pdf-roll-page-at-current-pos-basic ()
  "Test pdf-roll-page-at-current-pos returns correct page numbers."
  (with-temp-buffer
    ;; Position 1 (page 1)
    (goto-char 1)
    (insert " ")  ; Need content at position
    (goto-char 1)
    (should (= (pdf-roll-page-at-current-pos) 1)))
  (with-temp-buffer
    ;; Position 5 (page 2)
    (insert "    X")  ; 5 chars, point at 5 is page 2
    (goto-char 5)
    (should (= (pdf-roll-page-at-current-pos) 2)))
  (with-temp-buffer
    ;; Position 9 (page 3)
    (insert "        X")  ; 9 chars
    (goto-char 9)
    (should (= (pdf-roll-page-at-current-pos) 3))))

(ert-deftest pdf-roll-page-at-current-pos-error-on-even ()
  "Test pdf-roll-page-at-current-pos errors on even positions."
  (with-temp-buffer
    (insert "  ")
    (goto-char 2)
    (should-error (pdf-roll-page-at-current-pos))))

;;; Customization tests

(ert-deftest pdf-roll-vertical-margin-default ()
  "Test pdf-roll-vertical-margin has correct default value."
  (should (= pdf-roll-vertical-margin 2)))

(ert-deftest pdf-roll-margin-color-default ()
  "Test pdf-roll-margin-color has correct default value."
  (should (equal pdf-roll-margin-color "gray")))

;;; Symbol property tests

(ert-deftest pdf-roll-symbol-properties ()
  "Test that pdf-roll symbol has correct properties set."
  ;; Display property for placeholder
  (should (equal (get 'pdf-roll 'display) '(space :width 25 :height 1000)))
  ;; Evaporate property
  (should (get 'pdf-roll 'evaporate))
  (should (get 'pdf-roll-margin 'evaporate)))

;;; Face tests

(ert-deftest pdf-roll-default-face-exists ()
  "Test that pdf-roll-default face is defined."
  (should (facep 'pdf-roll-default)))

;;; Minor mode keymap tests

(ert-deftest pdf-roll-minor-mode-keymap-exists ()
  "Test that pdf-view-roll-minor-mode-map is defined with remappings."
  (should (keymapp pdf-view-roll-minor-mode-map))
  ;; Check that scroll commands are remapped
  (should (lookup-key pdf-view-roll-minor-mode-map
                      [remap pdf-view-previous-line-or-previous-page]))
  (should (lookup-key pdf-view-roll-minor-mode-map
                      [remap pdf-view-next-line-or-next-page])))

(defun pdf-roll-test--page-overlay (page window)
  "Return a new page overlay for PAGE in WINDOW in the current buffer."
  (let* ((pos (pdf-roll-page-to-pos page))
         (ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'category 'pdf-roll)
    (overlay-put ov 'window window)
    ov))

(ert-deftest pdf-roll-undisplay-pages-skips-missing-overlay ()
  "Test that undisplaying a page without an overlay is a no-op.
The `displayed-pages' a window remembers outlive its overlays, which
`pdf-roll-initialize' recreates whenever the buffer is reverted, so the
list can name a page the buffer no longer holds an overlay for -- one
inside the buffer as well as one past its end."
  (with-temp-buffer
    (insert " \n \n \n ")                 ; two pages
    (let* ((window (selected-window))
           (first (pdf-roll-test--page-overlay 1 window)))
      (should-not (pdf-roll-page-overlay 2 window))
      (should-not (pdf-roll-page-overlay 3 window))
      (pdf-roll-undisplay-pages '(1 2 3) window)
      (should (equal (overlay-get first 'display) (get 'pdf-roll 'display))))))

(ert-deftest pdf-roll-initialize-postpones-during-a-render ()
  "Test that a revert arriving during a render leaves the buffer alone.
`pdf-roll-initialize' is what a revert runs.  While `pdf-roll--delay-revert'
is set a page render is waiting on the server, and erasing the buffer would
take away the overlay that render is about to write to, so the work has to
be queued instead."
  (with-temp-buffer
    (insert " \n \n")
    (let ((overlay (make-overlay 1 2))
          (timers (length timer-list))
          (queued nil))
      (unwind-protect
          (let ((pdf-roll--delay-revert t))
            (pdf-roll-initialize)
            (setq queued (- (length timer-list) timers))
            ;; The buffer is untouched.
            (should (= (point-max) 5))
            (should (overlay-buffer overlay))
            ;; And the work was put on a timer.
            (should (= queued 1)))
        (dotimes (_ queued) (cancel-timer (car (last timer-list))))))))

;;; Provide

(provide 'pdf-roll-test)

;;; pdf-roll-test.el ends here
