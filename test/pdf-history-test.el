;; -*- lexical-binding: t -*-

(require 'pdf-history)
(require 'cl-lib)
(require 'ert)

;; Tests for pdf-history.el.
;; Recording and restoring a position needs a live window and a running
;; server, so these tests cover the stack itself, with the current page and
;; the current origin stubbed out.

(defmacro pdf-history-test-with-stack (page origin &rest body)
  "Run BODY with the history stack of a buffer on PAGE at ORIGIN.
PAGE and ORIGIN are places, so BODY can move the window along."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (setq-local pdf-history-minor-mode t)
     (cl-letf (((symbol-function 'image-mode-window-get)
                (lambda (prop &optional _window)
                  (when (eq prop 'page) ,page)))
               ((symbol-function 'pdf-history-current-origin)
                (lambda (&optional _window) ,origin)))
       (pdf-history-clear)
       ,@body)))

(ert-deftest pdf-history-create-item-carries-the-origin ()
  "An item is the page and the position on it."
  (pdf-history-test-with-stack 3 '(0.25 . 0.5)
    (should (equal (pdf-history-create-item) '(3 (0.25 . 0.5))))))

(ert-deftest pdf-history-push-compares-the-page ()
  "A second push from the same page adds nothing, whatever the position."
  (let ((page 3) (origin '(0.0 . 0.0)))
    (pdf-history-test-with-stack page origin
      (should (= (length pdf-history-stack) 1))
      (setq origin '(0.0 . 0.75))
      (pdf-history-push)
      (should (= (length pdf-history-stack) 1))
      (setq page 4)
      (pdf-history-push)
      (should (= (length pdf-history-stack) 2)))))

(ert-deftest pdf-history-record-origin-updates-the-current-item ()
  "Leaving a page stores where it was left."
  (let ((page 3) (origin '(0.0 . 0.0)))
    (pdf-history-test-with-stack page origin
      (setq origin '(0.0 . 0.75))
      (pdf-history-record-origin)
      (should (equal (nth pdf-history-index pdf-history-stack)
                     '(3 (0.0 . 0.75)))))))

(ert-deftest pdf-history-record-origin-inhibited ()
  "A jump in progress leaves the item it goes to alone."
  (let ((page 3) (origin '(0.0 . 0.0)))
    (pdf-history-test-with-stack page origin
      (setq origin '(0.0 . 0.75))
      (let ((pdf-history--inhibit-record t))
        (pdf-history-record-origin))
      (should (equal (nth pdf-history-index pdf-history-stack)
                     '(3 (0.0 . 0.0)))))))

(ert-deftest pdf-history-record-origin-other-page ()
  "An item for another page keeps the position it was left with."
  (let ((page 3) (origin '(0.0 . 0.25)))
    (pdf-history-test-with-stack page origin
      (setq page 4 origin '(0.0 . 0.75))
      (pdf-history-record-origin)
      (should (equal (nth pdf-history-index pdf-history-stack)
                     '(3 (0.0 . 0.25)))))))

(ert-deftest pdf-history-push-needs-a-page ()
  "A buffer whose window has no page yet puts nothing on the stack."
  (pdf-history-test-with-stack nil '(0.0 . 0.0)
    (should (null pdf-history-stack))))

(ert-deftest pdf-history-backward-on-an-empty-stack ()
  "Going back with nothing on the stack says so."
  (pdf-history-test-with-stack nil '(0.0 . 0.0)
    (should (equal (should-error (pdf-history-backward 1))
                   '(error "The history is empty")))))

(ert-deftest pdf-history-jump-within-a-page ()
  "A jump that stays on the page records where it came from and where it went."
  (let ((page 3) (origin '(0.0 . 0.5)))
    (pdf-history-test-with-stack page origin
      (pdf-history-before-jump)
      (setq origin '(0.0 . 0.1))
      (pdf-history-after-jump)
      (should (equal pdf-history-stack
                     '((3 (0.0 . 0.1)) (3 (0.0 . 0.5))))))))

(ert-deftest pdf-history-jump-that-does-not-move ()
  "A jump that lands where the window already was adds nothing."
  (let ((page 3) (origin '(0.0 . 0.5)))
    (pdf-history-test-with-stack page origin
      (pdf-history-before-jump)
      (pdf-history-after-jump)
      (should (equal pdf-history-stack '((3 (0.0 . 0.5))))))))

(ert-deftest pdf-history-jump-to-another-page ()
  "A jump to another page gives its item the position the jump reached."
  (let ((page 3) (origin '(0.0 . 0.5)))
    (pdf-history-test-with-stack page origin
      (pdf-history-before-jump)
      ;; What `pdf-view-goto-page' does: change the page and push, with the
      ;; vscroll back at 0, before the jump scrolls to its target.
      (setq page 4 origin '(0.0 . 0.0))
      (pdf-history-push)
      (setq origin '(0.0 . 0.3))
      (pdf-history-after-jump)
      (should (equal pdf-history-stack
                     '((4 (0.0 . 0.3)) (3 (0.0 . 0.5))))))))

(ert-deftest pdf-history-jump-while-browsing ()
  "A list buffer showing what the cursor is on records nothing."
  (let ((page 3) (origin '(0.0 . 0.5)))
    (pdf-history-test-with-stack page origin
      (let ((pdf-history-inhibit-jump t))
        (pdf-history-before-jump)
        (setq page 4 origin '(0.0 . 0.1))
        (pdf-history-after-jump))
      (should (equal pdf-history-stack '((3 (0.0 . 0.5))))))))

(provide 'pdf-history-test)
