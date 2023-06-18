;;; image-roll-mode.el --- Demo for image roll. -*- lexical-binding: t; -*-

;;; Commentary:
;; Demo for image roll. Contains a major mode for displaying images.

;;; Code:

(require 'svg)
(require 'image-roll)

(defun image-roll-demo-display-page (page)
  "Return demo image of PAGE.
This function is used for the `image-roll-demo'."
  (let* ((o (image-roll-page-overlay page))
         (s (cdr (overlay-get o 'display)))
         (w (car (plist-get s :width)))
         (h (car (plist-get s :height)))
         (svg (svg-create w h)))
    (unless w (print "NO W" #'external-debugging-output))
    (svg-rectangle svg 0 0 w h :fill-color "white")
    (svg-text svg
              (number-to-string page)
              :font-size "40"
              :fill "black"
              :x 20
              :y 50)
    (when image-roll-center
      (overlay-put o 'before-string
                   (when (> (window-pixel-width) w)
                     (propertize " " 'display
                                 `(space :align-to
                                         (,(floor (/ (- (window-pixel-width) w) 2))))))))
    (overlay-put o 'display (svg-image svg :margin `(0 . ,image-roll-vertical-margin)))))

(define-derived-mode image-roll-mode special-mode "Image Roll"
  ;; we don't use `(image-mode-setup-winprops)' because it would additionally
  ;; add `image-mode-reapply-winprops' to the
  ;; `window-configuration-change-hook', but `image-roll-redisplay' already
  ;; reapplies the vscroll, so we simply initialize the
  ;; `image-mode-winprops-alist' here, and add lines from
  ;; `image-mode-reapply-winprops' at the start of `image-roll-redisplay'.
  (add-hook 'window-size-change-functions 'image-roll-window-size-change-function nil t)
  (add-hook 'image-mode-new-window-functions 'image-roll-new-window-function nil t)
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil)))
;; (add-hook 'window-configuration-change-hook
;;           #'image-mode-reapply-winprops nil t))
;; (image-mode-setup-winprops))

(setq image-roll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-n") 'image-roll-scroll-forward)
        (define-key map (kbd "<down>") 'image-roll-scroll-forward)
        (define-key map (kbd "C-p") 'image-roll-scroll-backward)
        (define-key map (kbd "<up>") 'image-roll-scroll-backward)
        (define-key map (kbd "<wheel-up>") 'image-roll-scroll-mouse-wheel)
        (define-key map (kbd "<wheel-down>") 'image-roll-scroll-mouse-wheel)
        (define-key map (kbd "<mouse-5>") 'image-roll-scroll-forward)
        (define-key map (kbd "<mouse-4>") 'image-roll-scroll-backward)
        (define-key map "n" 'image-roll-next-page)
        (define-key map (kbd "<next>") 'image-roll-next-page)
        (define-key map "p" 'image-roll-previous-page)
        (define-key map (kbd "<prior>") 'image-roll-previous-page)
        (define-key map (kbd "S-<next>") 'image-roll-scroll-screen-forward)
        (define-key map (kbd "S-<prior>") 'image-roll-scroll-screen-backward)
        (define-key map [remap goto-line] 'image-roll-goto-page)
        map))

(defun image-roll-demo (&optional page-size pages)
  "Display a demo buffer displaying some images.
PAGE-SIZE is the size of each image and PAGES is the number of images."
  (interactive)
  (let ((buf-name "*image-roll-demo*"))
    ;;   (if (get-buffer buf-name)
    ;;       (switch-to-buffer (current-buffer))
    (with-current-buffer (get-buffer-create buf-name)
      (erase-buffer)
      (image-roll-mode)
      (setq cursor-type nil)
      (setq image-roll-step-size 50)
      (setq-local image-roll-last-page             (or pages 3)
                  image-roll-display-page-function 'image-roll-demo-display-page
                  image-roll-demo-page-size        (or page-size
                                                       (lambda ()
                                                         (let ((w (window-pixel-width)))
                                                           (cons w (* 1.4 w))))))

      (setq image-roll-center t)
      (switch-to-buffer (current-buffer)))))
(provide 'image-roll-mode)

;;; image-roll-mode.el ends here
