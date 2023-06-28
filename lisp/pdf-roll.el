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

;;; Code:
(require 'image-roll)
(require 'pdf-view)


(defun pdf-roll-maybe-slice-image (image &optional window inhibit-slice-p)
  "Return a sliced IMAGE if `pdf-view-current-slice' in WINDOW is non-nil.
If INHIBIT-SLICE-P is non-nil, disregard `pdf-view-current-slice'."
  (if-let ((slice (pdf-view-current-slice window))
           ((not inhibit-slice-p)))
      (list (cons 'slice
                  (pdf-util-scale slice (image-size image t) 'round))
            image)
    image))

(defun pdf-roll-display-image (image page &optional window inhibit-slice-p)
  "Display IMAGE for PAGE in WINDOW.
If INHIBIT-SLICE-P is non-nil, disregard `pdf-view-current-slice'."
  (let ((image (pdf-roll-maybe-slice-image image window inhibit-slice-p)))
    (image-roll-display-image image page window)))

(defun pdf-roll-page-image (page window)
  "Function to retrieve image of the PAGE in WINDOW."
  (pdf-roll-maybe-slice-image (pdf-view-create-page page window) window))

;;;###autoload
(define-minor-mode pdf-view-roll-minor-mode
  "If enabled display document on a virtual scroll providing continuous scrolling."
  :lighter " Continuous"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap pdf-view-previous-line-or-previous-page] 'image-roll-scroll-backward)
            (define-key map [remap pdf-view-next-line-or-next-page] 'image-roll-scroll-forward)
            (define-key map [remap mouse-set-point] 'ignore)
            (define-key map (kbd "<wheel-down>") 'image-roll-scroll-mouse-wheel)
            (define-key map (kbd "<wheel-up>") 'image-roll-scroll-mouse-wheel)
            (define-key map (kbd "S-<next>") 'image-roll-scroll-screen-forward)
            (define-key map (kbd "S-<prior>") 'image-roll-scroll-screen-backward)
            map)
  :version 28.1

  (cond (pdf-view-roll-minor-mode
         (setq-local image-roll-last-page (pdf-cache-number-of-pages)
                     image-roll-page-image-function 'pdf-roll-page-image
                     image-roll-center t
                     face-remapping-alist '((default . image-roll-default))
                     mwheel-scroll-up-function #'image-roll-scroll-forward
                     mwheel-scroll-down-function #'image-roll-scroll-backward)

         (remove-hook 'window-configuration-change-hook 'image-mode-reapply-winprops t)
         (remove-hook 'window-configuration-change-hook 'pdf-view-redisplay-some-windows t)
         (remove-hook 'image-mode-new-window-functions#'pdf-view-new-window-function t)

         (add-hook 'pre-redisplay-functions 'image-roll-pre-redisplay nil t)
         (add-hook 'image-roll-after-change-page-hook 'pdf-history-before-change-page-hook nil t)

         (let ((inhibit-read-only t))
           (erase-buffer)
           (remove-overlays))
         (image-roll-new-window-function))
        (t
         (setq-local mwheel-scroll-up-function #'pdf-view-scroll-up-or-next-page
                     mwheel-scroll-down-function #'pdf-view-scroll-down-or-previous-page)

         (add-hook 'window-configuration-change-hook 'image-mode-reapply-winprops nil t)
         (add-hook 'window-configuration-change-hook 'pdf-view-redisplay-some-windows nil t)
         (add-hook 'image-mode-new-window-functions #'pdf-view-new-window-function nil t)

         (remove-hook 'pre-redisplay-functions 'image-roll-pre-redisplay t)
         (remove-hook 'image-roll-after-change-page-hook 'pdf-history-before-change-page-hook t)

         (let ((inhibit-read-only t))
           (erase-buffer)
           (remove-overlays)
           (insert-file-contents-literally (buffer-file-name))
           (pdf-view-new-window-function (list (selected-window)))
           (set-buffer-modified-p nil)))))

(provide 'pdf-roll)

;;; pdf-roll.el ends here
