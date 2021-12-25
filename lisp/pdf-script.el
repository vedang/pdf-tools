(defun pdf-script-send-region (start end)
  (interactive (list (region-beginning) (region-end)))
  (print (tq-enqueue
          pdf-info--queue  (concat "try: " (buffer-substring start end) ";print('.')\nexcept: print(traceback.format_exc());print('.')\n\n") "^\\.\n" nil (lambda (closure response) (print response)))))


(defun pdf-script-send-line ()
  "Send the current line to shell"
  (interactive)
  (let ((start (point-at-bol))
        (end (point-at-eol)))
    (pdf-script-send-region start end)))
