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

(put 'pdf-roll 'display '(space :width 25 :height 5000))
(put 'pdf-roll 'evaporate t)
(put 'pdf-roll-margin 'evaporate t)
(put 'pdf-roll-margin 'cursor t)

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
         (set-default-toplevel-value sym color)
         (put 'pdf-roll-margin 'face `(:background ,color))))

;;; Variables
(defvar pdf-roll--state nil
  "Local variable that tracks window, point and vscroll to handle changes.")

;;; Utility Macros and functions
(defsubst pdf-roll-page-to-pos (page)
  "Get the buffer position displaying PAGE."
  (- (* 4 page) 3))

(defun pdf-roll-posn-page (pos)
  "Return the page number at POS."
  (/ (+ 3 (posn-point pos)) 4))

(defun pdf-roll--pos-overlay (pos window category)
  "Return an overlay for WINDOW with CATEGORY at POS."
  (cl-find-if (lambda (ov) (and (eq (overlay-get ov 'window) window)
                                (eq (overlay-get ov 'category) category)))
              (overlays-at pos)))

(defun pdf-roll--window-end-posn (&optional win)
  "Return a position object for the end of visible region of window WIN."
  (posn-at-x-y (/ (window-text-width win t) 2) (1- (window-text-height win t)) win))

(defun pdf-roll-page-overlay (&optional page window)
  "Return overlay displaying PAGE in WINDOW."
  (pdf-roll--pos-overlay
   (pdf-roll-page-to-pos (or page (pdf-view-current-page)))
   (or window (selected-window)) 'pdf-roll))

(defun pdf-roll-page-image (page window)
  "Return the image for PAGE that is being displayed in WINDOW."
  (overlay-get (pdf-roll-page-overlay page window) 'display))

(defun pdf-roll-displayed-pages (&optional window)
  "Return the pages being displayed in WINDOW."
  (number-sequence
   (pdf-view-current-page window)
   (min (pdf-view-posn-page (pdf-roll--window-end-posn window))
        (pdf-cache-number-of-pages))))

(defun pdf-roll-page-at-current-pos ()
  "Page at point."
  (if (cl-oddp (point))
      (/ (+ (point) 3) 4)
    (error "No page is displayed at current position (%s)" (point))))

(defun pdf-roll-set-vscroll (vscroll win)
  "Set vscroll to VSCROLL in window WIN."
  (image-mode-window-put 'vscroll vscroll win)
  (set-window-vscroll win vscroll t))

(defun pdf-roll-scroll-to-edges (edges eager-p)
  "See `pdf-util-scroll-to-edges' for EDGES and EAGER-P.
`pdf-roll' ignores EAGER-P for vertical motion and always scrolls so that the
top of EDGE is `next-screen-context-lines' down from the top the window."
  (let* ((vscroll (max 0 (- (nth 1 edges)
                            (* next-screen-context-lines (frame-char-height)))))
         (hscroll (pdf-util-required-hscroll edges eager-p)))
    (when vscroll (image-set-window-vscroll vscroll))
    (when hscroll (image-set-window-hscroll hscroll))
    `(,(- (nth 0 edges) (or hscroll 0))
      . ,(- (nth 1 edges) (or vscroll 0)))))

(defun pdf-roll-drag-region (pos region)
  "Scroll if POS and REGION have moved too close to the edge of the window."
  (when-let ((window (posn-window pos))
             ((windowp window)))
    (let* ((window (posn-window pos))
           (margin (* next-screen-context-lines (frame-char-height)))
           (posspec (pos-visible-in-window-p (posn-point pos) window t))
           (y (cdr (posn-x-y pos)))
           (top (or (nth 2 posspec) 0))
           (dy (- (+ y top) (nth 1 region))))
      (cond
       ((and (> dy 0) (< (- (window-text-height window t) y top) margin))
        (pdf-roll-scroll-forward (min margin (or (nth 3 posspec) 0)) nil t))
       ((and (< dy 0) (< y  margin))
        (pdf-roll-scroll-backward (min margin top) nil t))))))

;;; Displaying/Undisplaying pages
(defun pdf-roll--flush-spec (spec)
  "Flush the possibly sliced image in SPEC."
  (when (and (consp spec) (not (eq 'image (car spec))))
    (setq spec (nth 1 spec)))
  (when (imagep spec)
    (image-flush spec)))

(defun pdf-roll-display-image (image page &optional window inhibit-slice-p size)
  "Display IMAGE for PAGE of SIZE in WINDOW.
If INHIBIT-SLICE-P is non-nil, disregard `pdf-view-current-slice'."
  (let* ((window (or window (selected-window)))
         (slice (and (not inhibit-slice-p ) (pdf-view-get-slice window page)))
         (size (or size (image-size image t)))
         (slice (and slice (pdf-util-scale slice size 'round)))
         (image (if slice
                    `((slice . ,slice) ,image)
                  image))
         (size (if slice
                   `(,(nth 2 slice) . ,(nth 3 slice))
                 size))
         (overlay (pdf-roll-page-overlay page window))
         (margin-pos (+ (pdf-roll-page-to-pos page) 2))
         (margin-overlay (pdf-roll--pos-overlay margin-pos window 'pdf-roll-margin))
         (offset (when (> (window-width window t) (car size))
                   `(space :width (,(/ (- (window-width window t) (car size)) 2))))))
    (pdf-roll--flush-spec (overlay-get overlay 'display))
    (overlay-put overlay 'display image)
    (overlay-put overlay 'line-prefix offset)
    (overlay-put overlay 'image-height (cdr size))
    (overlay-put margin-overlay 'display `(space :width (,(car size))
                                                 :height (pdf-roll-vertical-margin)))
    (overlay-put margin-overlay 'line-prefix offset)
    (cdr size)))

(defun pdf-roll-display-page (page window &optional force)
  "Display PAGE in WINDOW.
With FORCE non-nil display fetch page again even if it is already displayed."
  (let* ((overlay (pdf-roll-page-overlay page window))
         (display (overlay-get overlay 'display)))
    (if (or force (not display) (eq (car display) 'space))
        (let ((size (pdf-view-desired-image-size page window)))
          (pdf-roll-display-image (pdf-view-create-page page window size)
                                  page window nil size))
      (or (overlay-get overlay 'image-height)
          (cdr (image-display-size display t))))))

(defun pdf-roll-display-pages (page &optional window force)
  "Display pages to fill the WINDOW starting from PAGE.
If FORCE is non-nill redisplay a page even if it is already displayed."
  (let (displayed
        (available-height (window-text-height window t)))
    (let ((vscroll (image-mode-window-get 'vscroll window))
          (im-height (pdf-roll-display-page page window force)))
      (pdf-roll-set-vscroll (min vscroll (1- im-height)) window)
      (cl-callf - available-height (- im-height (window-vscroll window t))))
    (push (pdf-roll-page-overlay page window) displayed)
    (set-window-point window (+ (pdf-roll-page-to-pos page)
                                (if (>= available-height pdf-roll-vertical-margin) 2 0)))
    (while (and (> available-height 0) (< page (pdf-cache-number-of-pages)))
      (cl-callf - available-height (pdf-roll-display-page (cl-incf page) window force))
      (push (pdf-roll-page-overlay page window) displayed))
    ;; store displayed images for determining which images to update when update
    ;; is triggered
    (cl-callf cl-union (window-parameter window 'pdf-roll--displayed-overlays) displayed)
    displayed))

(defun pdf-roll-undisplay-pages (overlays)
  "Undisplay pages displayed in OVERLAYS and flush the corresponding images.
Replaces the display property of the overlay holding a page with a space."
  (dolist (ov overlays)
    (pdf-roll--flush-spec (overlay-get ov 'display))
    (overlay-put ov 'display (get 'pdf-roll 'display))))

;;; State Management
(defun pdf-roll-new-window-function (&optional win)
  "Make overlays to hold pages and set WIN as window property to each overlay."
  (setq win (or (and (windowp win) win) (selected-window)))
  (unless (pdf-roll-page-overlay 1 win)
    (dotimes (i (* 2 (+ (pdf-cache-number-of-pages) 1)))
      (let ((o (make-overlay (+ 1 (* 2 i)) (+ 2 (* 2 i)))))
        (overlay-put o 'category (if (eq 0 (mod i 2)) 'pdf-roll 'pdf-roll-margin))
        (overlay-put o 'window win)))
    (image-mode-winprops win t)
    (dolist (win-st pdf-roll--state)
      (when-let ((win-old (car-safe win-st))
                 ((not (window-live-p win-old))))
        (remove-overlays (point-min) (point-max) 'window win-old))))
  ;; initial `pdf-roll-redisplay' needs to know which page(s) to display
  (cl-callf or (pdf-view-current-page win) 1)
  (cl-callf or (image-mode-window-get 'vscroll win) 0)
  (pdf-view--restore-origin))

(defun pdf-roll-redisplay (&optional window)
  "Analogue of `pdf-view-redisplay' for WINDOW."
  (unless window (setq pdf-roll--state nil))
  (dolist (window (if (windowp window)
                      `(,window)
                    (get-buffer-window-list nil nil t)))
    (setf (alist-get window pdf-roll--state) nil)
    (pdf-roll-pre-redisplay window)))

(defun pdf-roll-pre-redisplay (win)
  "Handle modifications to the state in window WIN.
It should be added to `pre-redisplay-functions' buffer locally."
  (with-demoted-errors "Error in image roll pre-redisplay: %S"
    (unless (pdf-roll-page-overlay 1 win)
      (pdf-roll-new-window-function win))
    (let* ((state (alist-get win pdf-roll--state))
           (pscrolling (memq last-command
                             '( pixel-scroll-precision pixel-scroll-start-momentum
                                pixel-scroll-interpolate-up pixel-scroll-interpolate-down
                                ultra-scroll ultra-scroll-up ultra-scroll-down)))
           (page (progn (when pscrolling
                          (setf (pdf-view-current-page win)
                                (/ (min (+ (window-start win) 5) (- (point-max) 4)) 4)))
                        (pdf-view-current-page win)))
           (height (window-pixel-height win))
           (vscroll (image-mode-window-get 'vscroll win))
           (size-changed (not (and (eq height (nth 1 state))
                                   (eq (window-pixel-width win) (nth 2 state)))))
           (page-changed (not (eq page (nth 0 state))))
           (vscroll-changed (not (eq vscroll (nth 3 state))))
           (buffer-changed (not (eq (current-buffer)
                                    (window-parameter win 'pdf-roll--buffer)))))
      ;; When using pixel scroll precision mode we accept its values for vscroll and hscroll.
      (if (and pscrolling
               (let ((pos (pdf-roll--window-end-posn win)))
                 ;; Except on window is visible beyonf the last page.
                 (or (< (posn-point pos) (- (point-max) 4))
                     ;; Then we try to keep at least half the window occupied by pdf pages.
                     ;; So we stop scrolling if it blanks too large a part of the window.
                     (< (cdr (posn-object-x-y pos)) (/ (window-text-height win t) 2))
                     ;; However if vscroll has been set by other means scrolling should be
                     ;; able to descrease it.
                     (> vscroll (window-vscroll win t))
                     (prog1 nil (message "End of buffer")))))
          (progn (image-mode-window-put 'vscroll (window-vscroll win t) win)
                 (image-mode-window-put 'hscroll (window-hscroll win)) win)
        (set-window-vscroll win vscroll t)
        (set-window-hscroll win (or (image-mode-window-get 'hscroll win) 0))
        (set-window-start win (pdf-roll-page-to-pos page) t))
      (set-window-point win (or (nth 4 state) (window-point win)))
      (setq disable-point-adjustment t)
      (when (or size-changed page-changed vscroll-changed buffer-changed)
        ;; If images/pages are small enough (or after jumps), there
        ;; might be multiple image that need to get updated
        (let ((old (window-parameter win 'pdf-roll--displayed-overlays))
              new)
          ;; If we don't keep this page displayed, pixel-scroll-precision-mode
          ;; causes a memory leak when scrolling up.
          (when (> page 1)
            (pdf-roll-display-page (1- page) win size-changed)
            (let ((ov (pdf-roll-page-overlay (1- page) win)))
              (push ov (window-parameter win 'pdf-roll--displayed-overlays))
              (setq new `(,ov))))
          (cl-callf2 append (pdf-roll-display-pages page win size-changed) new)
          (pdf-roll-undisplay-pages (cl-set-difference old new))
          (set-window-parameter win 'pdf-roll--displayed-overlays new))
        (setf (alist-get win pdf-roll--state)
              `(,page ,height ,(window-pixel-width win) ,vscroll ,(window-point win)))
        (set-window-parameter win 'pdf-roll--buffer (current-buffer))
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
  (pdf-roll-goto-page (+ (pdf-view-current-page) n)))

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
  (if (> 0 n)
      (pdf-roll-scroll-backward (- n) window pixels)
    (setq n (* (or n 1) (if pixels 1 (frame-char-height))))
    (setq window (or window (selected-window)))
    (cl-callf + n (window-vscroll window t))
    (goto-char (window-start window))
    (while (let ((occupied-pixels (pdf-roll-display-page
                                   (pdf-roll-page-at-current-pos) window)))
             (if (eq (point) (- (point-max) 7))
                 (let ((m (- occupied-pixels
                             (/ (window-text-height window t) 2))))
                   (prog1 nil
                     (when (<= m n)
                       (message "End of buffer"))
                     (setq n (min n (max 0 m)))))
               (when (>= n occupied-pixels)
                 (cl-decf n occupied-pixels))))
      (forward-char 4))
    (setf (pdf-view-current-page window) (pdf-roll-page-at-current-pos))
    (pdf-roll-set-vscroll n window)))

(defun pdf-roll-scroll-backward (&optional n window pixels)
  "Scroll image N lines backwards in WINDOW.
Line height is determined by `frame-char-height'. When N is negative
scroll forward instead. With a prefix arg N is its numeric value.

If PIXELS is non-nil N is number of pixels instead of lines."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (if (> 0 n)
      (pdf-roll-scroll-forward (- n) window pixels)
    (setq n (* (or n 1) (if pixels 1 (frame-char-height))))
    (setq window (or window (selected-window)))
    (cl-callf + n (- (cdr (pdf-view-image-size t window))
                     (window-vscroll nil t)))
    (goto-char (window-start window))
    (while (and (progn (cl-decf n (pdf-roll-display-page
                                   (pdf-roll-page-at-current-pos) window))
                       (> n 0))
                (if (bobp)
                    (prog1 nil (message "Beginning of buffer"))
                  t))
      (forward-char -4))
    (setf (pdf-view-current-page window) (pdf-roll-page-at-current-pos))
    (pdf-roll-set-vscroll (max 0 (- n)) window)))

(defun pdf-roll-scroll-screen-forward (&optional arg)
  "Scroll forward by (almost) ARG many full screens."
  (interactive "p")
  (pdf-roll-scroll-forward
   (- (* (window-text-height nil t) arg)
      (* next-screen-context-lines (frame-char-height)))
   nil t))

(defun pdf-roll-scroll-screen-backward (&optional arg)
  "Scroll backward by (almost) ARG many full screens."
  (interactive "p")
  (pdf-roll-scroll-backward
   (- (* (window-text-height nil t) arg)
      (* next-screen-context-lines (frame-char-height)))
   nil t))

(defun pdf-roll-scroll-to-end-of-page ()
  "Scroll forward till the end of last displayed page is visible."
  (interactive)
  (let ((pos (pdf-roll--window-end-posn)))
    (if (not (posn-object pos))
        (pdf-roll-scroll-backward (cdr (posn-object-x-y pos)) nil t)
      (pdf-roll-scroll-forward
       (or (nth 3 (pos-visible-in-window-p (posn-point pos) nil t)) 0) nil t))))

(defun pdf-roll-scroll-to-mouse-pos (event)
  "Scroll to the position of mouse EVENT."
  (interactive "e")
  (let ((event (event-start event)))
    (pdf-roll-scroll-forward
     (- (cdr (posn-x-y event)) (* next-screen-context-lines (frame-char-height)))
     (posn-window event) t)))

;;; Minor mode
(defun pdf-roll-initialize (&rest _args)
  "Function to initialize `pdf-view-roll-minor-mode'.
It is also added to `revert-buffer-function'.

It erases the buffer and adds one line containing a space for each page."
  (dolist (winal pdf-roll--state)
    (let ((win (car winal)))
      (when (eq (window-buffer win) (current-buffer))
        (pdf-roll-undisplay-pages
         (window-parameter win 'pdf-roll--displayed-overlays))
        (set-window-parameter win 'pdf-roll--displayed-overlays nil))))
  (setq pdf-roll--state nil)
  (remove-overlays)
  (let ((pages (pdf-cache-number-of-pages))
        (inhibit-read-only t))
    (erase-buffer)
    (dotimes (_i (* 2 (+ pages 1)))
      (insert " \n"))
    (delete-char -1)
    (set-buffer-modified-p nil)))

(defvar pdf-view-roll-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap pdf-view-previous-line-or-previous-page] 'pdf-roll-scroll-backward)
    (define-key map [remap pdf-view-next-line-or-next-page] 'pdf-roll-scroll-forward)
    (define-key map [remap pdf-view-scroll-down-or-previous-page] 'pdf-roll-scroll-screen-backward)
    (define-key map [remap pdf-view-scroll-up-or-next-page] 'pdf-roll-scroll-screen-forward)
    (define-key map [remap mouse-set-point] 'ignore)
    (define-key map (kbd "S-<next>") 'pdf-roll-scroll-screen-forward)
    (define-key map (kbd "S-<prior>") 'pdf-roll-scroll-screen-backward)
    map))

;;;###autoload
(define-minor-mode pdf-view-roll-minor-mode
  "If enabled display document on a virtual scroll providing continuous scrolling."
  :lighter " Continuous"

  (cond (pdf-view-roll-minor-mode
         (setq-local mwheel-scroll-up-function #'pdf-roll-scroll-forward
                     mwheel-scroll-down-function #'pdf-roll-scroll-backward)

         (setq-local pdf-view-posn-page-function #'pdf-roll-posn-page
                     pdf-view-page-image-function #'pdf-roll-page-image
                     pdf-view-displayed-pages-function #'pdf-roll-displayed-pages
                     pdf-view-redisplay-function #'pdf-roll-redisplay
                     pdf-view-display-image-function #'pdf-roll-display-image
                     pdf-view-drag-region-function #'pdf-roll-drag-region
                     pdf-util-scroll-to-edges-function #'pdf-roll-scroll-to-edges)

         (face-remap-set-base 'default 'pdf-roll-default)
         (face-remap-set-base 'isearch 'default)

         (remove-hook 'window-configuration-change-hook 'image-mode-reapply-winprops t)
         (remove-hook 'window-configuration-change-hook 'pdf-view-redisplay-some-windows t)
         (remove-hook 'image-mode-new-window-functions #'pdf-view-new-window-function t)

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

         (mapc #'kill-local-variable
               '( pdf-view-posn-page-function pdf-view-page-image-function
                  pdf-view-redisplay-function pdf-view-display-image-function
                  pdf-view-displayed-pages-function pdf-view-drag-region-function
                  pdf-util-scroll-to-edges-function))

         (let ((inhibit-read-only t))
           (remove-overlays)
           (dolist (winprops image-mode-winprops-alist)
             (when-let ((win (car winprops))
                        ((window-live-p win)))
               (pdf-roll-set-vscroll 0 win)
               (image-mode-window-put 'displayed-pages nil win)
               (pdf-view-new-window-function winprops)
               (pdf-view-redisplay win)))
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
