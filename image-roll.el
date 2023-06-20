;;; image-roll.el --- Virtual scroll display engine  -*- lexical-binding:t -*-

;; Copyright (C) 202 Free Software Foundation, Inc.

;; Author: D. L. Nicolai <dalanicolai@gmail.com>
;; Version: 1.0
;; Keywords: files, pdf
;; URL: https://github.com/dalanicolai/image-roll.el
;; Package-Requires: ((emacs "26.1"))


;;; Commentary:

;; This package provides a virtual scroll engine for displaying books/documents.
;; The main purpose of the package is to provide a continuous scrolling when
;; viewing documents.

;; The package is written in a way that it supports images/pages of different
;; sizes on the same roll (see comment above `image-roll-scroll-forward'). Also
;; there is no minumum or maximum on the range of the sizes, and finally, it is
;; written to support being displayed in (any number of) multiple windows.

;; The core functionality, i.e. the 'scroll' is provided by the
;; `image-roll-new-window-function' and `image-roll-redisplay' functions. The
;; function `image-roll-new-window-function' should be added to the
;; `image-mode-new-window-functions' while the `image-roll-redisplay' should be
;; added to the `window-configuration-change-hook' both as buffer local hook
;; functions (i.e. by passing a non-nil LOCAL argument to `add-hook'). For the
;; `image-mode-new-window-functions' to have effect, the `image-mode-winprops'
;; should be initialized by either using `image-mode-setup-winprops' (like in
;; the body of `pdf-view-mode') or by initializing the
;; `image-mode-winprops-alist' explicitly (by setting its value to nil, like in
;; the `image-roll-mode' example).

;; The package is meant to be used in combination with some other package that
;; provides features to extract and manage the data from the document. An
;; example of such a file is the file `pdf-scroll.el' at URL:
;; https://github.com/dalanicolai/pdf-tools/blob/image-roll-version/lisp/pdf-scroll.el
;; The file `pdf-scroll.el' provides the configurations for pdf-tools to work
;; with `image-roll.el'

;; However, for development purposes, the package provides an `image-roll-demo'
;; function. Also, as an example of its usage it includes a function
;; `image-roll-directory' which can be used to view all images in some directory
;; using the roll.

;; This file provides four buffer local variables that should be set to the
;; values of the functions that correctly 'retrieve' the required data from the
;; document. See their docstrings and the `image-roll-directory' function (or
;; `pdf-scroll.el') for more info.


;;; Issues

;; No real issues are known when using this package in a clean way, i.e. by
;; starting from `emacs -Q', then loading this package and using it.

;; However, I have experienced some errors in redisplay when using this package
;; in combination with vertico, marginalia and savehist. In that case sometimes
;; Emacs its `redisplay' can get a little 'confused/messed up', so that a page
;; (although never the first page but only later pages) will not show directly
;; when creating a new (second, third, ...) window. In that case `redisplay' can
;; be forced by 'activating' the minibuffer (e.g. by pressing `M-x') and hiding
;; it again. It is a weird bug, because it only happens when installing those
;; packages via `use-package', but not when the packages are installed via
;; `package-install'. Also it seems to occur mostly when these three packages
;; are combined. Additionally, it might be might 'due to' using multiple Emacs
;; versions (you can try if the issue occurs on the other Emacs version also,
;; probably not). See
;; https://lists.gnu.org/archive/html/emacs-devel/2022-04/msg00829.html Anyway,
;; I don't know what causes the bug, but this is what I have noticed from
;; inspecting it.

;;; Code:
(require 'image-mode)

(put 'image-roll 'display '(space :width 25 :height 1000))
(put 'image-roll 'evaporate t)

;;; Custom Variables
(defgroup image-roll nil
  "Image roll configurations."
  :group 'applications
  :version "28.1")

(defcustom image-roll-vertical-margin 2
  "Vertical margin between images in pixels, i.e. page separation height."
  :type 'integer)

(defcustom image-roll-overlay-face-bg-color "gray"
  "Background color of overlay, i.e. page separation color."
  :type 'color
  :set (lambda (_ color) (put 'image-roll 'face `(:background ,color))))

(defcustom image-roll-step-size 50
  "Scroll step size in pixels units."
  :type 'integer)

(defcustom image-roll-center nil
  "When non-nil, center the roll horizontally in the window."
  :type 'boolean)

(defcustom image-roll-after-change-page-hook nil
  "Hook run after changing to and displaying another page."
  :type 'hook)

;;; Local variables to be set by implementations
(defvar-local image-roll-last-page 0)

(defvar-local image-roll-display-page-function nil
  "Function that sets the overlay's display property.
The function receives the page number as a single
argument (PAGE). The function should use `(image-roll-page-overlay
PAGE)' to add the image of the page as the overlay's
display-property.")

;;; Other Variables
(defvar-local image-roll--last-state nil
  "Local variable that tracks window, point and vscroll to handle changes.")

;;; Utility Macros and inline functions
(defmacro image-roll-current-page (&optional window)
  "Return the page number of the currently displayed page in WINDOW.
The current page is the page that overlaps with the window
start (this choice was made in order to simplify the scrolling
logic)"
  `(image-mode-window-get 'page ,window))

(defmacro image-roll-displayed-pages (&optional window)
  "Return list of pages currently displayed in WINDOW."
  `(image-mode-window-get 'displayed-pages ,window))

(defsubst image-roll-page-to-pos (page)
  "Get the buffer position displaing PAGE."
  (1- (* 2 page)))

(defun image-roll-page-overlay (&optional page window)
  "Return overlay displaying PAGE in WINDOW."
  (cl-find (or window (selected-window))
           (overlays-at (image-roll-page-to-pos
                         (or page (image-roll-page-at-current-pos))))
           :key (lambda (ov) (overlay-get ov 'window))))

(defun image-roll-page-at-current-pos ()
  "Page at point."
  (if (cl-oddp (point))
      (/ (1+ (point)) 2)
    (error "No page is displayed at current position (%s)" (point))))

;;; Helper functions
(defun image-roll-undisplay-pages (pages &optional window)
  "Undisplay PAGES from WINDOW.
Replaces the display property of the overlay holding a page with a space."
  (dolist (page pages)
   (overlay-put (image-roll-page-overlay page window)
               'display (get 'image-roll 'display))))

(defun image-roll-new-window-function (&optional winprops)
  "Setup image roll in a new window.
If the buffer is newly created, then it does not contain any
overlay and this function erases the buffer contents, after which
it inserts empty spaces that each hold a overlay. If the buffer
already has overlays (i.e. a second or subsequent window is
created), the function simply copies the overlays and adds the
new window as window overlay-property to each overlay.

WINPROPS are the initial window properties.

This function should be added to image-roll (continuous scroll)
minor mode commands, after erasing the buffer to create the
overlays."
  (let ((win (or (and (windowp winprops) winprops) (selected-window))))
    (if (not (overlays-at 1))
        (let ((pages image-roll-last-page)
              (inhibit-read-only t))

          (erase-buffer)
          (setq image-roll--last-state (list t))
          ;; here we only add the 'page' and 'window' overlay-properties, we add
          ;; more properties/information as soon as it becomes available in the
          ;; 'image-roll-redisplay' function
          (dotimes (_ pages)
            (insert " ")
            (let ((o (make-overlay (1- (point)) (point))))
              (overlay-put o 'category 'image-roll)
              (overlay-put o 'window win))
            (insert "\n"))
          (delete-char -1)
          (set-buffer-modified-p nil))
      (unless (image-roll-page-overlay 1 win)
        (dotimes (i (/ (point-max) 2))
          (overlay-put (copy-overlay (car (overlays-at (1+ (* 2 i)))))
                       'window win))
        (dolist (win-st image-roll--last-state)
          (when-let ((win-old (car-safe win-st))
                     ((not (window-live-p win-old))))
            (remove-overlays (point-min) (point-max) 'window win-old)))
        (cl-callf2 cl-delete-if-not #'window-live-p image-roll--last-state :key #'car-safe)))
    ;; initial `image-roll-redisplay' needs to know which page(s) to display
    (cl-callf or (image-roll-current-page win) 1)
    (cl-callf or (image-mode-window-get 'vscroll win) 0)
    (goto-char (image-roll-page-to-pos (image-roll-current-page win)))))

(defun image-roll-set-vscroll (vscroll win)
  "Set vscroll to VSCROLL in window WIN."
  (image-mode-window-put 'vscroll vscroll win)
  (set-window-vscroll win vscroll t t))

(defun image-roll-set-hscroll (hscroll win)
  "Set hscroll to HSCROLL in window WIN."
  (image-mode-window-put 'hscroll hscroll win)
  (set-window-hscroll win hscroll))

(defun image-roll-display-pages (&optional window force)
  "Display pages to fill the WINDOW.
If FORCE is non-nill redisplay a page even if it is already displayed."
  (let (displayed
        (page (image-roll-page-at-current-pos)))
    (while (pos-visible-in-window-p (image-roll-page-to-pos page) window t)
      (when (or force (eq (car (overlay-get (image-roll-page-overlay page window) 'display))
                          'space))
        (funcall image-roll-display-page-function page window))
      (push page displayed)
      (cl-incf page))
    ;; store displayed images for determining which images to update when update
    ;; is triggered
    (cl-callf cl-union (image-mode-window-get 'displayed-pages window) displayed)
    displayed))

(defun image-roll-redisplay (&optional window)
  "Analogue of `pdf-view-redisplay' for WINDOW."
  (when (image-roll-page-overlay 1 window)
    (setq window (if (windowp window) window (selected-window)))
    (goto-char (image-roll-page-to-pos (image-roll-current-page window)))
    (image-roll-update-displayed-pages window t)))

(defun image-roll-pre-redisplay (win)
  "Handle modifications to the state in window WIN.
It should be added to `pre-redisplay-functions' buffer locally."
  (with-demoted-errors "Error in image roll pre-display: %S"
    (when (eq win (get-buffer-window))
      (image-roll-set-vscroll (image-mode-window-get 'vscroll win) win)
      (let* ((state (alist-get win image-roll--last-state))
             (size-changed (not (and (eq (window-pixel-height win) (nth 1 state))
                                     (eq (window-pixel-width win) (nth 2 state)))))
             (point-changed (not (eq (point) (nth 0 state))))
             (vscroll-changed (not (eq (window-vscroll nil t) (nth 3 state)))))
        (setq disable-point-adjustment t)
        (unless (image-roll-page-overlay 1 win)
          (image-roll-new-window-function win))
        (when (or size-changed point-changed vscroll-changed)
          (setf (alist-get win image-roll--last-state)
                `(,(point) ,(window-pixel-height win) ,(window-pixel-width win)
                  ,(window-vscroll nil t)))
          (set-window-start win (point) t)
          (image-roll-update-displayed-pages win size-changed)
          (when point-changed (run-hooks 'image-roll-after-change-page-hook)))))))

(defun image-roll-update-displayed-pages (&optional window force)
  "Update the pages displayed in WINDOW.
When FORCE is non-nil redisplay even the already displayed pages."
  (let ((old (image-mode-window-get 'displayed-pages window))
        (new (image-roll-display-pages window force)))
    ;; If images/pages are small enough (or after jumps), there
    ;; might be multiple image that need to get updated
    (image-roll-undisplay-pages (cl-set-difference old new) window)
    (setf (image-roll-current-page window) (image-roll-page-at-current-pos))
    (image-mode-window-put 'displayed-pages new window)
    (cl-set-difference new old)))

;;; Page navigation commands
(defun image-roll-goto-page-start ()
  "Go to the start of the first displayed page."
  (interactive)
  (image-roll-set-vscroll 0 nil))

;; NOTE code based on (taken from) `pdf-view-goto-page'.
(defun image-roll-goto-page (page &optional window)
  "Go to PAGE in WINDOW."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  (unless (and (>= page 1)
               (<= page image-roll-last-page))
    (error "No such page: %d" page))
  (goto-char (image-roll-page-to-pos page))
  (image-roll-set-vscroll 0 window))

(defun image-roll-next-page (&optional n)
  "Go to next page or next Nth page."
  (interactive "p")
  (set-window-start nil (+ (point) (* 2 n)) t)
  (image-roll-goto-page (+ (image-roll-page-at-current-pos) n)))

(defun image-roll-previous-page (&optional n)
  "Go to previous page or previous Nth page."
  (interactive "p")
  (image-roll-next-page (- n)))

;;; Scrolling Commands
(defun image-roll-scroll-forward (&optional pixels window)
  "Scroll image PIXELS forward in WINDOW.
By default PIXELS is `image-roll-step-size'. When PIXELS is negative scroll
backward instead.

With a prefix arg PIXELS is the numeric value times `image-roll-step-size'."
  (interactive (list (* (prefix-numeric-value current-prefix-arg) image-roll-step-size)))
  (setq pixels (or pixels image-roll-step-size))
  (when (> 0 pixels) (image-roll-scroll-backward (- pixels) window))
  (let ((pos (point)))
    (while (let* ((data (pos-visible-in-window-p (point) window t))
                  (occupied-pixels (cond ((nth 2 data) (nth 4 data))
                                         (data (line-pixel-height))
                                         (t (funcall image-roll-display-page-function
                                                     (image-roll-page-at-current-pos) window)
                                            (line-pixel-height)))))
             (and (> pixels occupied-pixels)
                  (if (eq (point) (1- (point-max)))
                      (prog1 nil
                        (setq pixels (- occupied-pixels 10))
                        (message "End of buffer"))
                    (cl-decf pixels occupied-pixels))))
      (forward-char 2))
    (image-roll-set-vscroll (+ (if (eq pos (point)) (window-vscroll window t) 0) pixels)
                            window)))

(defun image-roll-scroll-backward (&optional pixels window)
  "Scroll image PIXELS backwards in WINDOW.
By default PIXELS is `image-roll-step-size'. When PIXELS is negative scroll
forward instead.

With a prefix arg PIXELS is the numeric value times `image-roll-step-size'."
  (interactive (list (* (prefix-numeric-value current-prefix-arg) image-roll-step-size)))
  (setq pixels (or pixels image-roll-step-size))
  (when (> 0 pixels) (image-roll-scroll-backward (- pixels) window))
  (let* ((data (pos-visible-in-window-p (point) window t))
         (pixels-top (if (nth 2 data) (nth 2 data) 0)))
    (if (< pixels pixels-top)
        (image-roll-set-vscroll (- (window-vscroll window t) pixels)
                                window)
      (cl-decf pixels pixels-top)
      (while (and (if (bobp)
                      (prog1 nil (message "Beginning of buffer."))
                    t)
                  (progn (forward-char -2)
                         (funcall image-roll-display-page-function
                                  (image-roll-page-at-current-pos) window)
                         (cl-decf pixels (line-pixel-height)))
                  (> pixels 0)))
      (image-roll-set-vscroll (- pixels) window))))

(defun image-roll-scroll-screen-forward (&optional arg)
  "Scroll forward by (almost) ARG many full screens."
  (interactive "p")
  (image-roll-scroll-forward
   (* (window-text-height nil t) (- arg 0.1))))

(defun image-roll-scroll-screen-backward (&optional arg)
  "Scroll backward by (almost) ARG many full screens."
  (interactive "p")
  (image-roll-scroll-backward
   (* (window-text-height nil t) (- arg 0.1))))

(defun image-roll-scroll-mouse-wheel (event)
  "Scroll according to mouse wheel EVENT."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (pcase (event-basic-type event)
      ('wheel-down (image-roll-scroll-forward))
      ('wheel-up (image-roll-scroll-backward))
      (_ (error "Event must be wheel down or wheel up event")))))

(provide 'image-roll)

;;; image-roll.el ends here
