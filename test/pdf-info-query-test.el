;;; pdf-info-query-test.el --- Query completion tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'pdf-info)

(defmacro pdf-info-test-with-query-process (&rest body)
  "Run BODY with a private native transaction queue answering every query.

Sending a query answers it, through the queue's own filter.  The response
may not be delivered from a timer: a synchronous `pdf-info-query' waits
with an integer JUST-THIS-ONE, which reads only its process and runs no
timers, because that wait is reachable from a mode-line construct and
would otherwise run arbitrary Lisp in the middle of a redisplay.  A timer
here would never fire, and neither would the `with-timeout' meant to bound
the test, so the test would hang rather than fail."
  (declare (indent 0))
  `(let* ((process (make-process :name "pdf-info-query-test"
                                 :command '("cat") :noquery t))
          (pdf-info--queue (tq-create process))
          (pdf-info-log nil))
     (unwind-protect
         (cl-letf (((symbol-function 'process-send-string)
                    (lambda (&rest _)
                      (tq-filter pdf-info--queue "OK\n.\n"))))
           ,@body)
       (tq-close pdf-info--queue))))

(ert-deftest pdf-info-query-preprocessing-error-completes-synchronous-request ()
  "Parser, transformation and response logging errors terminate the waiter."
  (dolist (stage '(parse transform log))
    (pdf-info-test-with-query-process
      (let* ((function (pcase stage
                         ('parse 'pdf-info-query--parse-response)
                         ('transform 'pdf-info-query--transform-response)
                         ('log 'pdf-info-query--log)))
             expired failure)
        (cl-letf (((symbol-function function)
                   (lambda (&rest args)
                     (unless (and (eq stage 'log) (cadr args))
                       (error "Synthetic response failure")))))
          (with-timeout (0.2 (setq expired t))
            (setq failure (should-error (pdf-info-query 'open "fixture.pdf")))))
        (should-not expired)
        (should (string-match-p "Synthetic response failure" (error-message-string failure)))
        (should (tq-queue-empty pdf-info--queue))))))

(ert-deftest pdf-info-query-preprocessing-error-completes-asynchronous-request ()
  "An asynchronous request receives its parser failure exactly once."
  (pdf-info-test-with-query-process
    (let ((calls 0) result expired)
      (cl-letf (((symbol-function 'pdf-info-query--parse-response)
                 (lambda (&rest _) (error "Synthetic response failure"))))
        (let ((pdf-info-asynchronous
               (lambda (status response)
                 (cl-incf calls)
                 (setq result (list status response)))))
          (pdf-info-query 'open "fixture.pdf"))
        (with-timeout (0.2 (setq expired t))
          (while (zerop calls) (accept-process-output process 0.01))))
      (should-not expired)
      (should (= calls 1))
      (should (equal result '(error "Synthetic response failure"))))))

(ert-deftest pdf-info-query-consumer-error-does-not-invoke-consumer-twice ()
  "A consumer exception is not reclassified as a parser error and redelivered."
  (pdf-info-test-with-query-process
    (let ((calls 0) expired)
      (let ((pdf-info-asynchronous
             (lambda (&rest _)
               (cl-incf calls)
               (error "Synthetic consumer failure"))))
        (pdf-info-query 'open "fixture.pdf"))
      (with-timeout (0.2 (setq expired t))
        (while (zerop calls) (accept-process-output process 0.01)))
      (should-not expired)
      (should (= calls 1))
      (should (tq-queue-empty pdf-info--queue)))))

(ert-deftest pdf-info-query-success-remains-successful ()
  "A normal synchronous response keeps its existing return value."
  (pdf-info-test-with-query-process
    (let (expired)
      (with-timeout (0.2 (setq expired t))
        (should-not (pdf-info-query 'open "fixture.pdf")))
      (should-not expired))))

;;; pdf-info-query-test.el ends here
