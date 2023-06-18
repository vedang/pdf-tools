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

(defmacro image-roll-current-page (&optional window)
  "Return the page number of the currently displayed page in WINDOW.
The current page is the page that overlaps with the window
start (this choice was made in order to simplify the scrolling
logic)"
  `(image-mode-window-get 'page ,window))

;;; Utility Macros and inline functions
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

(defsubst image-roll-overlay-height (page &optional window)
  "Get the height of overlay displaying PAGE on WINDOW."
  (when-let (ov (image-roll-page-overlay page window))
    (+ (cdr (overlay-get ov 'overlay-size)) image-roll-vertical-margin)))

;;; Helper functions
(defun image-roll-undisplay-page (page &optional window)
  "Undisplay PAGE from WINDOW.
Replaces the image display property of the overlay holding PAGE
with a space. It size is determined from the image its
`image-size'."
  (let* ((o (image-roll-page-overlay page window))
         (d (overlay-get o 'display))
         (im (if (imagep d)
                 d
               (nth 1 d))))
    (when (imagep im)
      (overlay-put o 'display nil))))

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
  (let ((win (or (and (windowp (car winprops)) (car winprops)) (selected-window))))
    (if (not (overlays-at 1))
        (let ((pages image-roll-last-page)
              (inhibit-read-only t))

          (erase-buffer)
          (remove-overlays)

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
          (goto-char 1)
          (set-buffer-modified-p nil))
      (dotimes (i (/ (point-max) 2))
        (overlay-put (copy-overlay (car (overlays-at (1+ (* 2 i)))))
                     'window (car winprops))))
    ;; initial `image-roll-redisplay' needs to know which page(s) to display
    (cl-callf or (image-roll-current-page win) 1)))

(defun image-roll-set-vscroll (vscroll win)
  "Set vscroll to VSCROLL in window WIN."
  (image-mode-window-put 'vscroll vscroll win)
  (set-window-vscroll win vscroll t t))

(defun image-roll-set-hscroll (hscroll win)
  "Set hscroll to HSCROLL in window WIN."
  (image-mode-window-put 'hscroll hscroll win)
  (set-window-hscroll win hscroll))

(defun image-roll-window-configuration-change-hook (&optional win)
  "Handle state change for window WIN.
It should be added to `window-configuration-change-hook' buffer locally."
  (when (memq 'image-roll-new-window-function image-mode-new-window-functions)
    (image-roll-set-vscroll (or (image-mode-window-get 'vscroll win) 0) win)
    (image-roll-set-hscroll (or (image-mode-window-get 'hscroll) 0) win)))

(defun image-roll-display-pages (&optional window force)
  "Display pages to fill the WINDOW.
If FORCE is non-nill redisplay a page even if it is already displayed."
  (let (displayed
        (page (image-roll-page-at-current-pos)))
    (while (pos-visible-in-window-p (image-roll-page-to-pos page) window t)
      (when (or force (not (overlay-get (image-roll-page-overlay page window) 'display)))
          (funcall image-roll-display-page-function page window))
      (push page displayed)
      (cl-incf page))
    ;; store displayed images for determining which images to update when update
    ;; is triggered
    (cl-callf cl-union (image-mode-window-get 'displayed-pages window) displayed)
    ;; store the height of the visible pages for determining when to update
    ;; images, namely when some part of the roll outside this range becomes
    ;; visible
    displayed))

(defun image-roll-window-size-change-function (&optional window _no-relative-vscroll)
  "Redisplay the scroll in WINDOW.
Besides that this function can be called directly, it should also
be added to the `window-size-change-functions'. It is a substitute for the
`pdf-view-redisplay' function."

  ;; Beware: this call to image-mode-winprops can't be optimized away, because
  ;; it not only gets the winprops data but sets it up if needed (e.g. it's used
  ;; by doc-view to display the image in a new window).
  (setq window (if (window-live-p window) window (selected-window)))
  (when (and (memq 'image-roll-new-window-function image-mode-new-window-functions)
             (eq (current-buffer) (window-buffer window)))
    (if (and (> (point-max) 1) (equal (buffer-substring (point-min) (1+ (point-min))) "%"))
        (image-roll-new-window-function `(,window))
      (image-mode-winprops window t))
    ;; Determine to display pages and display. Undisplay pages is not necessary as
    ;; this is taken care off by `image-roll-update-displayed-pages'.
    (image-roll-display-pages window t)
    ;; we only need to jump to the right page, the vscroll is conserved and if
    ;; required can be set to 0 before the redisplay
    (image-roll-window-configuration-change-hook window)))

(defun image-roll-update-displayed-pages (&optional window)
  "Update the pages displayed in WINDOW."
  (let ((old (image-mode-window-get 'displayed-pages window))
        (new (image-roll-display-pages window)))
    ;; dolist because if images/pages are small enough (or after jumps), there
    ;; might be multiple image that need to get updated
    (dolist (p (cl-set-difference old new))
      (image-roll-undisplay-page p window))
    ;; setting/appending new below
    (setf (image-roll-current-page window) (image-roll-page-at-current-pos))
    (image-mode-window-put 'displayed-pages new window)
    new))

(defun image-roll--goto-point (vscroll &optional window redisplay)
  "Make page at point the current page with VSCROLL in WINDOW.
If REDISPLAY is non-nil perform redisplay for `set-window-start' to take effect."
  (set-window-start window (point) t)
  (image-roll-set-vscroll vscroll window)
  (image-roll-update-displayed-pages window)
  (when redisplay
    (redisplay)
    (message "%S" (window-start)))
  (run-hooks 'image-roll-after-change-page-hook))

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
  (if (eq page (image-roll-page-at-current-pos))
      (image-roll-set-vscroll 0 window)
    (goto-char (image-roll-page-to-pos page))
    (image-roll--goto-point 0 window t)))

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
    (while (when-let ((data (pos-visible-in-window-p (point) window t))
                      (occupied-pixels (if (nth 2 data)
                                           (nth 4 data)
                                         (line-pixel-height))))
             (and (> pixels occupied-pixels)
                  (cl-decf pixels occupied-pixels)))
      (forward-char 2))
    (unless  (pos-visible-in-window-p (point) window t)
      (while (and (if (eobp)
                      (prog1 nil (message "End of buffer."))
                    t)
                  (progn (funcall image-roll-display-page-function
                                  (image-roll-page-at-current-pos) window)
                         (let ((height (line-pixel-height)))
                           (when (> pixels height)
                             (cl-decf pixels height)))))
        (forward-char 2)))
    (if (eq pos (point))
        (progn (image-roll-set-vscroll (+ (window-vscroll window t) pixels)
                                       window)
               (image-roll-update-displayed-pages window))
      (image-roll--goto-point pixels window))))

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
        (progn (image-roll-set-vscroll (- (window-vscroll window t) pixels)
                                window)
               (image-roll-update-displayed-pages window))
      (cl-decf pixels pixels-top)
      (while (and (if (bobp)
                      (prog1 nil (message "Beginning of buffer."))
                    t)
                  (progn (forward-char -2)
                         (funcall image-roll-display-page-function
                                  (image-roll-page-at-current-pos) window)
                         (cl-decf pixels (line-pixel-height)))
                  (> pixels 0)))
      (image-roll--goto-point (- pixels) window))))

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
