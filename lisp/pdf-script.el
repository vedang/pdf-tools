;;; pdf-script.el --- Script support for vimura server.  -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Daniel Nicolai

;; Author: Daniel Nicolai <dalanicolai@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

(defun pdf-script-send-region (start end)
  (interactive (list (region-beginning) (region-end)))
  (print (tq-enqueue pdf-info--queue
                     (concat "try: "
                             (buffer-substring-no-properties start end)
                             ";print('.')\nexcept: print(traceback.format_exc());print('.')\n\n")
                     "^\\.\n"
                     nil
                     (lambda (closure response) (print response)))))


(defun pdf-script-send-line ()
  "Send the current line to shell"
  (interactive)
  (let ((start (point-at-bol))
        (end (point-at-eol)))
    (pdf-script-send-region start end)))

(provide 'pdf-script)
;;; pdf-script.el ends here
