;; -*- lexical-binding: t; -*-

;; * ================================================================== *
;; * Tests for pdf-cache.el
;; * ================================================================== *

(require 'pdf-cache)
(require 'ert)

(ert-deftest pdf-cache-get-image ()
  (let (pdf-cache--image-cache)
    (should-not (pdf-cache-get-image 1 1))
    (setq pdf-cache--image-cache
          (list
           (pdf-cache--make-image 1 1 "1" nil)
           (pdf-cache--make-image 2 1 "2" nil)
           (pdf-cache--make-image 3 1 "3" nil)))
    (should (equal (pdf-cache-get-image 1 1) "1"))
    (should (equal pdf-cache--image-cache
                   (list
                    (pdf-cache--make-image 1 1 "1" nil)
                    (pdf-cache--make-image 2 1 "2" nil)
                    (pdf-cache--make-image 3 1 "3" nil))))
    (should (equal (pdf-cache-get-image 2 1) "2"))
    (should (equal pdf-cache--image-cache
                   (list
                    (pdf-cache--make-image 2 1 "2" nil)
                    (pdf-cache--make-image 1 1 "1" nil)
                    (pdf-cache--make-image 3 1 "3" nil))))
    (should-not (pdf-cache-get-image 4 1))))

(require 'cl-lib)

(defmacro pdf-cache-test-with-prefetch-buffer (&rest body)
  "Run BODY with real prefetch timers in an isolated buffer."
  (declare (indent 0) (debug t))
  `(let ((buffer (generate-new-buffer " *pdf-cache-prefetch-test*")) timers)
     (unwind-protect
         (cl-letf (((symbol-function 'pdf-util-assert-pdf-buffer) #'ignore))
           (with-current-buffer buffer ,@body))
       (dolist (timer timers) (cancel-timer timer))
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest pdf-cache-prefetch-kill-buffer-cancels-timer ()
  (pdf-cache-test-with-prefetch-buffer
    (pdf-cache-prefetch-minor-mode 1)
    (push pdf-cache--prefetch-timer timers)
    (should (memq (car timers) timer-idle-list))
    (kill-buffer buffer)
    (should-not (memq (car timers) timer-idle-list))))

(ert-deftest pdf-cache-prefetch-major-mode-change-cancels-timer ()
  (pdf-cache-test-with-prefetch-buffer
    (pdf-cache-prefetch-minor-mode 1)
    (push pdf-cache--prefetch-timer timers)
    (fundamental-mode)
    (should-not (memq (car timers) timer-idle-list))))

(ert-deftest pdf-cache-prefetch-repeat-enable-and-disable ()
  (pdf-cache-test-with-prefetch-buffer
    (dotimes (_ 3)
      (pdf-cache-prefetch-minor-mode 1)
      (push pdf-cache--prefetch-timer timers)
      (should (memq (car timers) timer-idle-list))
      (dolist (previous (cdr timers))
        (should-not (memq previous timer-idle-list))))
    (should (= 1 (cl-count #'pdf-cache--prefetch-cancel kill-buffer-hook)))
    (should (= 1 (cl-count #'pdf-cache--prefetch-cancel change-major-mode-hook)))
    (pdf-cache-prefetch-minor-mode -1)
    (should-not (memq (car timers) timer-idle-list))
    (should-not (memq #'pdf-cache--prefetch-cancel kill-buffer-hook))
    (should-not (memq #'pdf-cache--prefetch-cancel change-major-mode-hook))))

(ert-deftest pdf-cache-prefetch-buffer-timers-are-isolated ()
  (pdf-cache-test-with-prefetch-buffer
    (pdf-cache-prefetch-minor-mode 1)
    (push pdf-cache--prefetch-timer timers)
    (let ((first-timer (car timers)))
      (pdf-cache-test-with-prefetch-buffer
        (pdf-cache-prefetch-minor-mode 1)
        (push pdf-cache--prefetch-timer timers)
        (kill-buffer buffer)
        (should-not (memq (car timers) timer-idle-list))
        (should (memq first-timer timer-idle-list))))))
