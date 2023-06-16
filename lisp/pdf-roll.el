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
(require 'pdf-view)

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
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap pdf-view-previous-line-or-previous-page] 'image-roll-scroll-backward)
            (define-key map [remap pdf-view-next-line-or-next-page] 'image-roll-scroll-forward)
            (define-key map "<wheel-down>" 'image-roll-scroll-mouse-wheel)
            (define-key map "<wheel-up>" 'image-roll-scroll-mouse-wheel)
            (define-key map (kbd "S-<next>") 'image-roll-scroll-screen-forward)
            (define-key map (kbd "S-<prior>") 'image-roll-scroll-screen-backward)
            map)
  :version 28.1

  (cond (pdf-view-roll-minor-mode
         (setq-local image-roll-last-page (pdf-cache-number-of-pages)
                     image-roll-display-page-function 'pdf-view-display-page
                     image-roll-page-sizes-function 'pdf-roll-page-sizes
                     image-roll-set-redisplay-flag-function 'pdf-roll-set-redisplay-flag-function
                     image-roll-center t
                     mwheel-scroll-up-function #'image-roll-scroll-forward
                     mwheel-scroll-down-function #'image-roll-scroll-backward)

         (add-hook 'window-size-change-functions 'image-roll-redisplay nil t)

         (remove-hook 'image-mode-new-window-functions
                      #'pdf-view-new-window-function t)
         (add-hook 'image-mode-new-window-functions 'image-roll-new-window-function nil t)

         (add-hook 'image-roll-after-change-page-hook 'pdf-history-before-change-page-hook nil t)

         (let ((inhibit-read-only t))
           (erase-buffer)
           (remove-overlays)
           (image-roll-new-window-function (list (selected-window))))
         (pdf-view-redisplay)

         (define-key pdf-view-mode-map (kbd "<mouse-5>") 'pdf-view-next-line-or-next-page)
         (define-key pdf-view-mode-map (kbd "<mouse-4>") 'pdf-view-previous-line-or-previous-page))
        (t
         (setq-local mwheel-scroll-up-function #'pdf-view-scroll-up-or-next-page
                     mwheel-scroll-down-function #'pdf-view-scroll-down-or-previous-page)
         (remove-hook 'window-size-change-functions 'image-roll-redisplay t)

         (remove-hook 'image-mode-new-window-functions 'image-roll-new-window-function t)
         (add-hook 'image-mode-new-window-functions
                   #'pdf-view-new-window-function nil t)

         (remove-hook 'pdf-view-after-change-page-hook
                      #'pdf-history-before-change-page-hook t)

         (let ((inhibit-read-only t))
           (erase-buffer)
           (remove-overlays)
           (insert-file-contents-literally (buffer-file-name))
           (pdf-view-new-window-function (list (selected-window)))
           (set-buffer-modified-p nil)))))

(provide 'pdf-roll)

;;; pdf-roll.el ends here
