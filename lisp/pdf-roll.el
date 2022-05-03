;;; pdf-roll.el --- Add continuous scroll. -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Daniel Nicolai <dalanicolai@gmail.com>
;; Keywords: files, multimedia

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

(require 'image-roll)

(eval-when-compile
  (require 'pdf-view))


(defun pdf-roll-page-sizes ()
  (let (s)
    (dotimes (i (pdf-info-number-of-pages) (nreverse s))
      (push (pdf-view-desired-image-size (1+ i)) s))))

(defun pdf-roll-set-redisplay-flag-function ()
  (setf (pdf-view-window-needs-redisplay) t))

(define-minor-mode pdf-view-roll-minor-mode
  "If enabled display document on a virtual scroll providing
continuous scrolling."
  :lighter " Continuous"
  :keymap `((,(kbd "S-<next>") . image-roll-scroll-screen-forward)
            (,(kbd "S-<prior>") . image-roll-scroll-screen-backward))
  :version 28.1

  (cond (pdf-view-roll-minor-mode
         (setq-local image-roll-last-page (pdf-cache-number-of-pages)
                     image-roll-display-page-function 'pdf-view-display-page
                     image-roll-page-sizes-function 'pdf-roll-page-sizes
                     image-roll-set-redisplay-flag-function 'pdf-roll-set-redisplay-flag-function

                     image-roll-center t)

         (add-hook 'window-configuration-change-hook 'image-roll-redisplay nil t)

         (remove-hook 'image-mode-new-window-functions
	                    #'pdf-view-new-window-function t)
         (add-hook 'image-mode-new-window-functions 'image-roll-new-window-function nil t)

         (let ((inhibit-read-only t))
           (erase-buffer)
           (image-roll-new-window-function (list (selected-window))))
         (pdf-view-redisplay))
        (t
         (remove-hook 'window-configuration-change-hook 'image-roll-redisplay t)

         (remove-hook 'image-mode-new-window-functions 'image-roll-new-window-function t)
         (add-hook 'image-mode-new-window-functions
	                    #'pdf-view-new-window-function nil t)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert-file-contents-literally (buffer-file-name))
           (pdf-view-new-window-function (list (selected-window)))
           (set-buffer-modified-p nil)))))

(provide 'pdf-roll)

;;; pdf-roll.el ends here
