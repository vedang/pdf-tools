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
(require 'pdf-view)

(put 'pdf-roll 'display '(space :width 25 :height 1000))
(put 'pdf-roll 'evaporate t)
(put 'pdf-roll-margin 'evaporate t)

;;; Custom Variables
(defgroup pdf-roll nil
  "Image roll configurations."
  :group 'pdf-view)

(defface pdf-roll-default `((t :font ,(font-spec :family "monospace" :size 1)))
  "Default face for image roll documents.")

(defcustom pdf-roll-vertical-margin 2
  "Vertical margin between images in pixels, i.e. page separation height."
  :type 'integer)

(defcustom pdf-roll-margin-color "gray"
  "Background color of overlay, i.e. page separation color."
  :type 'color
  :set (lambda (sym color)
         (set-default sym color)
         (put 'pdf-roll-margin 'face `(:background ,color))))

;;; Variables
(defvar pdf-roll--state nil
  "Local variable that tracks window, point and vscroll to handle changes.")

;;; Utility Macros and functions
(defsubst pdf-roll-page-to-pos (page)
  "Get the buffer position displaing PAGE."
  (- (* 4 page) 3))

(defun pdf-roll--pos-overlay (pos window)
  "Return an overlay for WINDOW at POS."
  (cl-find window (overlays-at pos) :key (lambda (ov) (overlay-get ov 'window))))

(defun pdf-roll-page-overlay (&optional page window)
  "Return overlay displaying PAGE in WINDOW."
  (pdf-roll--pos-overlay
   (pdf-roll-page-to-pos (or page (pdf-roll-page-at-current-pos)))
   (or window (selected-window))))

(defun pdf-roll-page-at-current-pos ()
  "Page at point."
  (if (cl-oddp (point))
      (/ (+ (point) 3) 4)
    (error "No page is displayed at current position (%s)" (point))))

(defun pdf-roll-set-vscroll (vscroll win)
  "Set vscroll to VSCROLL in window WIN."
  (image-mode-winprops win t)
  (image-mode-window-put 'vscroll vscroll win)
  (set-window-vscroll win vscroll t))

;;; Displaying/Undisplaying pages
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
  (let* ((image (pdf-roll-maybe-slice-image image window inhibit-slice-p))
         (size (image-display-size image t))
         (overlay (pdf-roll-page-overlay page window))
         (margin-pos (+ (pdf-roll-page-to-pos page) 2))
         (margin-overlay (pdf-roll--pos-overlay margin-pos window))
         (offset (when (> (window-width window t) (car size))
                   `(space :width (,(/ (- (window-width window t) (car size)) 2))))))
    (overlay-put overlay 'display image)
    (overlay-put overlay 'line-prefix offset)
    (overlay-put margin-overlay 'display `(space :width (,(car size)) :height (,pdf-roll-vertical-margin)))
    (overlay-put margin-overlay 'line-prefix offset)
    (cdr size)))

(defun pdf-roll-display-page (page window &optional force)
  "Display PAGE in WINDOW.
With FORCE non-nil display fetch page again even if it is already displayed."
  (let ((display (overlay-get (pdf-roll-page-overlay page window) 'display)))
    (if (or force (not display) (eq (car display) 'space))
        (pdf-roll-display-image (pdf-view-create-page page window) page window)
      (cdr (image-display-size display t)))))

(defun pdf-roll-display-pages (page &optional window force pscrolling)
  "Display pages to fill the WINDOW starting from PAGE.
If FORCE is non-nill redisplay a page even if it is already displayed."
  (let (displayed
        (available-height (window-pixel-height window)))
    (when (and pscrolling (> page 1))
      (pdf-roll-display-page (1- page) window force)
      (push (1- page) displayed))
    (let ((vscroll (image-mode-window-get 'vscroll window))
          (im-height (pdf-roll-display-page page window force)))
      (pdf-roll-set-vscroll (min vscroll (1- im-height)) window)
      (cl-callf - available-height (- im-height (window-vscroll window t))))
    (push page displayed)
    (while (and (> available-height 0) (< page (pdf-cache-number-of-pages)))
      (cl-callf - available-height (pdf-roll-display-page (cl-incf page) window force))
      (push page displayed))
    (when (and pscrolling (< page (pdf-cache-number-of-pages)))
      (pdf-roll-display-page (cl-incf page) window force)
      (push page displayed))
    ;; store displayed images for determining which images to update when update
    ;; is triggered
    (cl-callf cl-union (image-mode-window-get 'displayed-pages window) displayed)
    displayed))

(defun pdf-roll-undisplay-pages (pages &optional window)
  "Undisplay PAGES from WINDOW.
Replaces the display property of the overlay holding a page with a space."
  (dolist (page pages)
    (overlay-put (pdf-roll-page-overlay page window)
                 'display (get 'pdf-roll 'display))))

;;; State Management
(defun pdf-roll-new-window-function (&optional win)
  "Setup image roll in a new window WIN.
If the buffer is newly created, then it does not contain any
overlay and this function erases the buffer contents, after which
it inserts empty spaces that each hold a overlay. If the buffer
already has overlays (i.e. a second or subsequent window is
created), the function simply copies the overlays and adds the
new window as window overlay-property to each overlay.

This function should be added to pdf-roll (continuous scroll)
minor mode commands, after erasing the buffer to create the
overlays."
  (setq win (or (and (windowp win) win) (selected-window)))
  (if (not (overlays-at 1))
      (let ((pages (pdf-cache-number-of-pages))
            (inhibit-read-only t))
        (erase-buffer)
        (setq pdf-roll--state (list t))
        (dotimes (i (* 2 pages))
          (insert " ")
          (let ((o (make-overlay (1- (point)) (point))))
            (overlay-put o 'category (if (eq 0 (mod i 2)) 'pdf-roll 'pdf-roll-margin))
            (overlay-put o 'window win))
          (insert "\n"))
        (delete-char -1)
        (set-buffer-modified-p nil))
    (unless (pdf-roll-page-overlay 1 win)
      (dotimes (i (/ (point-max) 2))
        (overlay-put (copy-overlay (car (overlays-at (1+ (* 2 i)))))
                     'window win))
      (dolist (win-st pdf-roll--state)
        (when-let ((win-old (car-safe win-st))
                   ((not (window-live-p win-old))))
          (remove-overlays (point-min) (point-max) 'window win-old)))
      (cl-callf2 cl-delete-if-not #'window-live-p pdf-roll--state :key #'car-safe)))
  ;; initial `pdf-roll-redisplay' needs to know which page(s) to display
  (cl-callf or (pdf-view-current-page win) 1)
  (cl-callf or (image-mode-window-get 'vscroll win) 0))

(defun pdf-roll-redisplay (&optional window)
  "Analogue of `pdf-view-redisplay' for WINDOW."
  (setq window (if (windowp window) window (selected-window)))
  (when (pdf-roll-page-overlay 1 window)
    (setf (alist-get window pdf-roll--state) nil)
    (force-window-update window)))

(defun pdf-roll-pre-redisplay (win)
  "Handle modifications to the state in window WIN.
It should be added to `pre-redisplay-functions' buffer locally."
  (with-demoted-errors "Error in image roll pre-redisplay: %S"
    (unless (pdf-roll-page-overlay 1 win)
      (pdf-roll-new-window-function win))
    (let* ((state (alist-get win pdf-roll--state))
           (pscrolling (memq last-command
                             '(pixel-scroll-precision pixel-scroll-start-momentum
                               pixel-scroll-interpolate-up pixel-scroll-interpolate-down)))
           (page (progn (when pscrolling
                          (setf (pdf-view-current-page win)
                                (/ (min (+ (window-start win) 5) (point-max)) 4)))
                        (pdf-view-current-page win)))
           (height (window-pixel-height win))
           (vscroll (image-mode-window-get 'vscroll win))
           (size-changed (not (and (eq height (nth 1 state))
                                   (eq (window-pixel-width win) (nth 2 state)))))
           (page-changed (not (eq page (nth 0 state))))
           (vscroll-changed (not (eq vscroll (nth 3 state))))
           (start (pdf-roll-page-to-pos page)))
      (if (and pscrolling
               (or (not (eq start (- (point-max) 3)))
                   (let ((visible-pixels (nth 4 (pos-visible-in-window-p start win t))))
                     (and visible-pixels (> visible-pixels (/ (window-text-height win t) 2))))
                   (prog1 nil (message "End of buffer"))))
          (progn (image-mode-window-put 'vscroll (window-vscroll win t) win)
                 (image-mode-window-put 'hscroll (window-hscroll win)) win)
        (set-window-vscroll win vscroll t)
        (set-window-hscroll win (or (image-mode-window-get 'hscroll win) 0))
        (set-window-start win start t))
      (setq disable-point-adjustment t)
      (when (or size-changed page-changed vscroll-changed)
        (let ((old (image-mode-window-get 'displayed-pages win))
              (new (pdf-roll-display-pages page win size-changed pscrolling)))
          ;; If images/pages are small enough (or after jumps), there
          ;; might be multiple image that need to get updated
          (pdf-roll-undisplay-pages (cl-set-difference old new) win)
          (image-mode-window-put 'displayed-pages new win)
          (set-window-point win (+ start
                                   (if (pos-visible-in-window-p (+ 2 start) win) 2 0))))
        (setf (alist-get win pdf-roll--state)
              `(,page ,height ,(window-pixel-width win) ,vscroll nil))
        (when page-changed (run-hooks 'pdf-view-after-change-page-hook))))))

;;; Page navigation commands
(defun pdf-roll-goto-page-start ()
  "Go to the start of the first displayed page."
  (interactive)
  (pdf-roll-set-vscroll 0 nil))

(defun pdf-roll-goto-page (page &optional window)
  "Go to PAGE in WINDOW."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  (unless (and (>= page 1)
               (<= page (pdf-cache-number-of-pages)))
    (error "No such page: %d" page))
  (setf (pdf-view-current-page window) page)
  (pdf-roll-set-vscroll 0 window))

(defun pdf-roll-next-page (&optional n)
  "Go to next page or next Nth page."
  (interactive "p")
  (pdf-roll-goto-page (+ (pdf-roll-page-at-current-pos) n)))

(defun pdf-roll-previous-page (&optional n)
  "Go to previous page or previous Nth page."
  (interactive "p")
  (pdf-roll-next-page (- n)))

;;; Scrolling Commands
(defun pdf-roll-scroll-forward (&optional n window pixels)
  "Scroll image N lines forward in WINDOW.
Line height is determined by `frame-char-height'. When N is negative
scroll backward instead. With a prefix arg N is its numeric value.

If PIXELS is non-nil N is number of pixels instead of lines."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (setq n (* (or n 1) (if pixels 1 (frame-char-height))))
  (setq window (or window (selected-window)))
  (when (> 0 n) (pdf-roll-scroll-backward (- n) window))
  (let ((pos (goto-char (window-start window))))
    (while (let* ((data (pos-visible-in-window-p (point) window t))
                  (occupied-pixels (cond ((nth 2 data) (nth 4 data))
                                         (data (line-pixel-height))
                                         (t (pdf-roll-display-page
                                             (pdf-roll-page-at-current-pos) window)))))
             (if (eq (point) (- (point-max) 3))
                 (prog1 nil
                   (setq n (min n (max 0 (- occupied-pixels (/ (window-text-height window t) 2)))))
                   (message "End of buffer"))
               (when (>= n occupied-pixels)
                 (cl-decf n occupied-pixels))))
      (forward-char 4))
    (setf (pdf-view-current-page window) (pdf-roll-page-at-current-pos))
    (pdf-roll-set-vscroll (+ (if (eq pos (point)) (window-vscroll window t) 0) n)
                          window)))

(defun pdf-roll-scroll-backward (&optional n window pixels)
  "Scroll image N lines backwards in WINDOW.
Line height is determined by `frame-char-height'. When N is negative
scroll forward instead. With a prefix arg N is its numeric value.

If PIXELS is non-nil N is number of pixels instead of lines."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (setq n (* (or n 1) (if pixels 1 (frame-char-height))))
  (setq window (or window (selected-window)))
  (when (> 0 n) (pdf-roll-scroll-backward (- n) window))
  (goto-char (window-start window))
  (let* ((data (pos-visible-in-window-p (point) window t))
         (pixels-top (if (nth 2 data) (nth 2 data) 0)))
    (if (< n pixels-top)
        (pdf-roll-set-vscroll (- (window-vscroll window t) n)
                                window)
      (cl-decf n pixels-top)
      (while (and (if (bobp)
                      (prog1 nil (message "Beginning of buffer."))
                    t)
                  (progn (forward-char -4)
                         (pdf-roll-display-page
                          (pdf-roll-page-at-current-pos) window)
                         (cl-decf n (line-pixel-height)))
                  (> n 0)))
      (pdf-roll-set-vscroll (- n) window)))
  (setf (pdf-view-current-page window) (pdf-roll-page-at-current-pos)))

(defun pdf-roll-scroll-screen-forward (&optional arg)
  "Scroll forward by (almost) ARG many full screens."
  (interactive "p")
  (pdf-roll-scroll-forward
   (- (* (window-text-height nil t) arg) (* next-screen-context-lines (frame-char-height)))
   nil t))

(defun pdf-roll-scroll-screen-backward (&optional arg)
  "Scroll backward by (almost) ARG many full screens."
  (interactive "p")
  (pdf-roll-scroll-backward
   (- (* (window-text-height nil t) arg) (* next-screen-context-lines (frame-char-height)))
   nil t))

;;; Minor mode
(defun pdf-roll-initialize (&rest _args)
  "Fun to initialize `pdf-view-roll-minor-mode'.
It is also added to `revert-buffer-function'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays))
  (image-mode-window-put 'displayed-pages nil)
  (pdf-roll-new-window-function))

;;;###autoload
(define-minor-mode pdf-view-roll-minor-mode
  "If enabled display document on a virtual scroll providing continuous scrolling."
  :lighter " Continuous"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap pdf-view-previous-line-or-previous-page] 'pdf-roll-scroll-backward)
            (define-key map [remap pdf-view-next-line-or-next-page] 'pdf-roll-scroll-forward)
            (define-key map [remap pdf-view-scroll-down-or-previous-page] 'pdf-roll-scroll-backward)
            (define-key map [remap pdf-view-scroll-up-or-next-page] 'pdf-roll-scroll-forward)
            (define-key map [remap mouse-set-point] 'ignore)
            (define-key map (kbd "S-<next>") 'pdf-roll-scroll-screen-forward)
            (define-key map (kbd "S-<prior>") 'pdf-roll-scroll-screen-backward)
            map)
  :version 28.1

  (cond (pdf-view-roll-minor-mode
         (setq-local face-remapping-alist '((default . pdf-roll-default))
                     mwheel-scroll-up-function #'pdf-roll-scroll-forward
                     mwheel-scroll-down-function #'pdf-roll-scroll-backward)

         (remove-hook 'window-configuration-change-hook 'image-mode-reapply-winprops t)
         (remove-hook 'window-configuration-change-hook 'pdf-view-redisplay-some-windows t)
         (remove-hook 'image-mode-new-window-functions#'pdf-view-new-window-function t)

         (add-hook 'pre-redisplay-functions 'pdf-roll-pre-redisplay nil t)
         (add-hook 'pdf-roll-after-change-page-hook 'pdf-history-before-change-page-hook nil t)

         (add-function :after (local 'revert-buffer-function) #'pdf-roll-initialize)

         (make-local-variable 'pdf-roll--state)

         (when (local-variable-p 'pixel-scroll-precision-mode)
           (kill-local-variable 'pixel-scroll-precision-mode)
           (kill-local-variable 'mwheel-coalesce-scroll-events))

         (pdf-roll-initialize))
        (t
         (setq-local mwheel-scroll-up-function #'pdf-view-scroll-up-or-next-page
                     mwheel-scroll-down-function #'pdf-view-scroll-down-or-previous-page)

         (add-hook 'window-configuration-change-hook 'image-mode-reapply-winprops nil t)
         (add-hook 'window-configuration-change-hook 'pdf-view-redisplay-some-windows nil t)
         (add-hook 'image-mode-new-window-functions #'pdf-view-new-window-function nil t)

         (remove-function (local 'revert-buffer-function) #'pdf-roll-initialize)

         (remove-hook 'pre-redisplay-functions 'pdf-roll-pre-redisplay t)
         (remove-hook 'pdf-roll-after-change-page-hook 'pdf-history-before-change-page-hook t)

         (kill-local-variable 'pdf-roll--state)

         (when (bound-and-true-p pixel-scroll-precision-mode)
             (setq-local pixel-scroll-precision-mode nil)
             (setq-local mwheel-coalesce-scroll-events t))

         (let ((inhibit-read-only t))
           (remove-overlays)
           (image-mode-window-put 'displayed-pages nil)
           (pdf-view-new-window-function (list (selected-window)))
           (set-buffer-modified-p nil)))))

(defun pdf-roll--get-display-property ()
  "`:before-until' advice for `image-get-display-property'.
`image-get-display-property' looks at the `point-min'. This function instead
returns the display property for the current page if `pdf-view-roll-minor-mode'
is non-nil."
  (when pdf-view-roll-minor-mode
    (get-char-property (pdf-roll-page-to-pos (pdf-view-current-page))
                       'display
                       (if (eq (window-buffer) (current-buffer))
                           (selected-window)))))

(advice-add 'image-get-display-property :before-until #'pdf-roll--get-display-property)

(provide 'pdf-roll)

;;; pdf-roll.el ends here
