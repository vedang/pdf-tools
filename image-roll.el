;;; image-roll.el --- Virtual scroll display engine  -*- lexical-binding:t -*-

;; Copyright (C) 202 Free Software Foundation, Inc.

;; Author: D. L. Nicolai <dalanicolai@gmail.com>
;; Version: 1.0
;; Keywords: files, pdf
;; URL: https://github.com/dalanicolai/image-roll.el


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


(require 'image-mode)
(require 'svg)

(defgroup image-roll nil
  "Image roll configurations."
  :group 'applications
  :version "28.1")

(defcustom image-roll-vertical-margin 2
  "Vertical margin around image (pixels), i.e. page separation height.
Because the margin is added to both sides of each page, the page
separation height is twice this value."
  :type 'integer)

(defcustom image-roll-overlay-face-bg-color "gray"
  "Background color of overlay, i.e. page separation color."
  :type 'color)

;; (defcustom image-roll-step-size
;;   ;; (lambda ()
;;   ;;   (let* ((o (image-roll-page-overlay))
;;   ;;          (s (overlay-get o 'display))
;;   ;;          (w (image-property s :width)))
;;   ;;     (if w
;;   ;;         (* 50
;;   ;;            (/  (float (if (consp w) (car w) w))
;;   ;;                (window-pixel-height)))
;;   ;;       50)))
;;   "Scroll step size.
;; The value can be either a number or a function that takes no
;; arguments and returns a positive number. If the number is equal
;; to or larger than 1, it represents pixel units. Otherwise, if the
;; value is between 0 and 1, it represents a fraction of the current
;; page height."
;;   :type '(choice function integer float))

(defcustom image-roll-step-size 50
  "Scroll step size in pixels units."
  :type '(choice function integer float))

(defcustom image-roll-center nil
  "When non-nil, center the roll horizontally in the window."
  :type 'boolean)

;; (defvar-local image-roll-number-of-pages-function nil
;;   "Function that should return the total number of pages.
;; The function should return an integer with the total number of
;; pages in the document.")
(defvar-local image-roll-last-page 0)

(defvar-local image-roll-page-sizes-function nil
  "Function that should return page-sizes of document.
The function should return a list of conses of the form (WIDTH .
HEIGHT), both numbers.")

(defvar-local image-roll-set-redisplay-flag-function nil
  "Function that sets the `needs-redisplay' window property.
Setting this is required for the
`pdf-view-redisplay-some-windows' to work correctly. The same
mechanism should probably get ported to image-roll.el also, to
make redisplay work correctly on multiple windows. However, this
is an advanced feature that is not required for basic image-roll
usage.")

(defvar-local image-roll-display-page-function nil
  "Function that sets the overlay's display property.
The function receives the page number as a single
argument (PAGE). The function should use `(image-roll-page-overlay
PAGE)' to add the image of the page as the overlay's
display-property.")


(defmacro image-roll-debug (object)
  `(progn (print (format "%s = %s" ,object (eval ,object))
                 #'external-debugging-output)
          (eval ,object)))

;; define as macro's for setf-ability

;; TODO update docstring
(defmacro image-roll-overlays (&optional window)
  "List of overlays that make up a scroll."
  `(image-mode-window-get 'overlays ,window))

(defmacro image-roll-page-overlay (&optional page)
  "Return the overlay that hold page number PAGE.
Implemented as macro to make it setf'able."
  `(nth (1- (or ,page (image-roll-current-page)))
        (image-roll-overlays)))

(defmacro image-roll-page-overlay-get (page prop)
  "Get overlay-property PROP of overlay holding page number PAGE.
Implemented as macro to make it setf'able."
  `(overlay-get (nth (1- ,page) (image-roll-overlays))
                ,prop))

(defmacro image-roll-current-page (&optional window)
  "Return the page number of the currently displayed page.
The current page is the page that overlaps with the window
start (this choice was made in order to simplify the scrolling
logic)"
  `(image-mode-window-get 'page ,window))

(defmacro image-roll-displayed-pages (&optional window)
  "Return list of currently displayed pages."
  `(image-mode-window-get 'displayed-pages ,window))

(defsubst image-roll-overlay-height (page)
  (+ (cdr (image-roll-page-overlay-get page 'overlay-size))
     (* 2 image-roll-vertical-margin)))

(defsubst image-roll-page-to-pos (page)
  (1- (* 2 page)))

(defun image-roll-visible-overlays ()
  "Page numbers of currently visible overlays.
The numbers are returned in a list. Overlays that are only
partially visible are included."
  (let* (visible
         (page (image-roll-current-page))
         (available-height (window-pixel-height))
         (last-page image-roll-last-page))

    (push page visible)
    (cl-decf available-height (- (image-roll-overlay-height page)
                                 (window-vscroll nil t)))
    (cl-incf page)

    (unless (> page image-roll-last-page)
      (while (> available-height 0)
        (push page visible)
        (if (= page last-page)
            (setq available-height 0)
          (cl-decf available-height (image-roll-overlay-height page))
          (cl-incf page))))
    visible))

(defun image-roll-undisplay-page (page)
  "Undisplay PAGE.
Replaces the image display property of the overlay holding PAGE
with a space. It size is determined from the image its
`image-size'."
  (display-warning '(image-roll) (format "undisplay %s" page)
                   :debug "*image-roll-debug-log*")
  (let* ((o (image-roll-page-overlay page))
         (im (overlay-get o 'display))
         (s (image-size im t))
         (w (car s))
         (h (cdr s)))
    (overlay-put o 'display `(space . (:width (,w) :height (,h))))
    (overlay-put o 'face `(:background "gray"))))

(defun image-roll-new-window-function (winprops)
  "Function called first after displaying buffer in a new window.
If the buffer is newly created, then it does not contain any
overlay and this function erases the buffer contents, after which
it inserts empty spaces that each hold a overlay. If the buffer
already has overlays (i.e. a second or subsequent window is
created), the function simply copies the overlays and adds the
new window as window overlay-property to each overlay.

This function should be added to image-roll (continuous scroll)
minor mode commands, after erasing the buffer to create the
overlays."
  ;; (if (= (buffer-size) 0)
  (if (not (overlays-at 1))
      (let (overlays
            (pages image-roll-last-page)
            (win (car winprops))
            (inhibit-read-only t))

        (erase-buffer)

        ;; here we only add the 'page' and 'window' overlay-properties, we add
        ;; more properties/information as soon as it becomes available in the
        ;; 'image-roll-redisplay' function
        (dotimes (i pages)
          (let ((i (1+ i)))
            (insert " ")
            (let ((o (make-overlay (1- (point)) (point))))
              (overlay-put o 'page  i)
              (overlay-put o 'window win)
              (push o overlays))
            (insert "\n")))
        (delete-char -1)
        (image-mode-window-put 'overlays (nreverse overlays))
        (set-buffer-modified-p nil)

        ;; required to make `pdf-view-redisplay-some-windows' call `pdf-view-redisplay'
        (when-let (fun image-roll-set-redisplay-flag-function)
          (funcall fun)))
    (let ((ols (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (image-roll-overlays))))
      (image-mode-window-put 'overlays ols winprops)))

  ;; initial `image-roll-redisplay' needs to know which page(s) to display
  (setf (image-roll-current-page (car winprops))
        (or (image-roll-current-page (car winprops)) 1)))

(defun image-roll-redisplay (&optional window no-relative-vscroll)
  "Redisplay the scroll.
Besides that this function can be called directly, it should also
be added to the `window-configuration-change-hook'.

The argument WINDOW is not used in the body, but it exists to
make the function compatible with `pdf-tools' (in which case it
is a substitute for the `pdf-view-redisplay' function)."
  (display-warning '(image-roll)
                   (format "redisplay %s" (car (image-mode-winprops)))
                   :debug "*image-roll-debug-log*")

  ;; NOTE the `(when (listp image-mode-winprops-alist)' from
  ;; `image-mode-reapply-winprops' was removed here (but in the end might turn
  ;; out to be required)

  ;; Beware: this call to image-mode-winprops can't be optimized away, because
  ;; it not only gets the winprops data but sets it up if needed (e.g. it's used
  ;; by doc-view to display the image in a new window).
  (image-mode-winprops nil t)

  (let* ((pages image-roll-last-page)
         (page-sizes (if image-roll-page-sizes-function
                         (funcall image-roll-page-sizes-function)
                       (make-list pages (if (functionp image-roll-demo-page-size)
                                            (funcall image-roll-demo-page-size)
                                          image-roll-demo-page-size))))
         (n 0))

    (dolist (page-size page-sizes)
      (let* ((page-width (car page-size))
             (overley-heigth (+ (cdr page-size) (* 2 image-roll-vertical-margin)))
             (o (nth n (image-roll-overlays))))
        (when image-roll-center
          (overlay-put o 'before-string
                       (when (> (window-pixel-width) page-width)
                         (propertize " " 'display
                                     `(space :align-to
                                             (,(floor (/ (- (window-pixel-width) page-width) 2))))))))
        (overlay-put o 'display `(space . (:width (,page-width) :height (,overley-heigth))))
        (overlay-put o 'face `(:background ,image-roll-overlay-face-bg-color))
        (overlay-put o 'overlay-size page-size)
        (setq n (1+ n)))))
  ;; Determine to display pages and display. Undisplay pages is not necessary as
  ;; this is taken care off by `image-roll-update-displayed-pages'.
  (let (displayed)
    (dolist (p (image-roll-visible-overlays))
      (apply image-roll-display-page-function
             p
             (when (eq image-roll-display-page-function
                       'doc-view-insert-image)
               `(:width ,doc-view-image-width)))
      (push p displayed))
    ;; store displayed images for determining which images to update when update
    ;; is triggered
  (image-mode-window-put 'displayed-pages (reverse displayed))
  ;; store the height of the visible pages for determining when to update
  ;; images, namely when some part of the roll outside this range becomes
  ;; visible
  (image-mode-window-put 'visible-pages-vscroll-limit
                         (- (apply #'+ (mapcar #'image-roll-overlay-height displayed))
                            (window-text-height nil t))))
  ;; we only need to jump to the right page, the vscroll is conserved and if
  ;; required can be set to 0 before the redisplay
  (when-let (p (image-roll-current-page))
    (goto-char (1- (* 2 p)))
    ;; (redisplay)
    ;; TODO implement in redisplay, `fractional vscroll' (in page units)
    (image-set-window-vscroll (or (image-mode-window-get 'vscroll)
                                  image-roll-vertical-margin))))

(defun image-roll-update-displayed-pages ()
  (let ((old (print (image-mode-window-get 'displayed-pages) #'external-debugging-output))
        (new (print (image-roll-visible-overlays) #'external-debugging-output)))
    ;; dolist because if images/pages are small enough (or after jumps), there
    ;; might be multiple image that need to get updated
    (dolist (p (cl-set-difference old new))
      (image-roll-undisplay-page p)
      (image-mode-window-put 'displayed-pages
                             (setq old (delete p old)))) ; important to update/setq old before
    ;; setting/appending new below
    (dolist (p (cl-set-difference new old))
      (apply image-roll-display-page-function
             p
             (when (eq image-roll-display-page-function
                       'doc-view-insert-image)
               `(:width ,doc-view-image-width)))
      (image-mode-window-put 'displayed-pages (setq old (append old (list p)))))
    ;; update also visible-range
    (image-mode-window-put 'visible-pages-vscroll-limit
                           (- (apply #'+ (mapcar #'image-roll-overlay-height new))
                              (window-text-height nil t)))))

(defun image-roll-goto-page-start ()
  (interactive)
  (image-set-window-vscroll 0))

;; NOTE code based on (taken from) `pdf-view-goto-page'.
(defun image-roll-goto-page (page &optional window)
  "Go to PAGE in document."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  (unless (and (>= page 1)
               (<= page (count-lines (point-min) (point-max))))
    (error "No such page: %d" page))
  (unless window
    (setq window
          ;; (if (pdf-util-pdf-window-p)
              (selected-window)
            ;; t)))
              ))
  (save-selected-window
    ;; Select the window for the hooks below.
    (when (window-live-p window)
      (select-window window 'norecord))
    (let ((changing-p
           (not (eq page (image-roll-current-page window)))))
      (when changing-p
        ;; (run-hooks 'pdf-view-before-change-page-hook)
        (setf (image-roll-current-page window) page)
        ;; (run-hooks 'pdf-view-change-page-hook))
        (when (window-live-p window)
          (goto-char (image-roll-page-to-pos page))
          (image-roll-update-displayed-pages))
      ;; (when changing-p
      ;;   (pdf-view-deactivate-region)
      ;;   (force-mode-line-update)
      ;;   (run-hooks 'pdf-view-after-change-page-hook))))
  nil))))

(defun image-roll-next-page (&optional n)
  (interactive "p")
  (set-window-start nil (+ (point) 2) t)
  (image-roll-goto-page (+ (image-roll-current-page) n)))

(defun image-roll-previous-page (&optional n)
  (interactive "p")
  (image-roll-next-page (- n)))


(defun image-roll-scroll-forward (&optional backward screen)
  (interactive)
  (let* ((current-page (image-roll-current-page))
         (new-page current-page)
         (current-overlay-height (image-roll-overlay-height current-page))
         (visible-pages-vscroll-limit (image-mode-window-get 'visible-pages-vscroll-limit))
         (step-size (if screen
                        (window-text-height nil t)
                      image-roll-step-size))

         ;; determine number of pages to forward/backward (e.g. when scrolling a
         ;; full screen the images/pages are small). We subtract, from the step
         ;; size, the remaining visible part of the current-page, and then
         ;; continue subtracting page-overlay heights until no available height
         ;; is left. To also set the scroll correctly after jumping the number
         ;; of pages, we store the remaining height, i.e. the remaining height
         ;; before the last step where the available height becomes negative
         ;; (for the edge case when available height is 0, there is no new
         ;; overlay visible yet and the `=' is not included in the `while'
         ;; condition)
         (available-height step-size)
         (remaining-height available-height)
         new-vscroll)
    (cond (backward
           (cl-decf available-height (window-vscroll nil t))
           (while (and (> new-page 0) (> available-height 0))
             (setq remaining-height available-height)
             (setq new-page (1- new-page)) ; allow new-page become < 1
             (cl-decf available-height (image-roll-overlay-height new-page))))
          (t
           ;; (cl-decf available-height (- (image-roll-overlay-height current-page)
           ;;                              (window-vscroll nil t)))
           (cl-decf available-height (- (image-roll-overlay-height current-page)
                                                         (window-vscroll nil t)))
           (while (and (< new-page (1+ image-roll-last-page)) (> available-height 0))
             (setq remaining-height available-height)
             (setq new-page (1+ new-page)) ; allow new-page > last-page
             (cl-decf available-height (image-roll-overlay-height new-page)))))

    (when backward
      (setq step-size (- step-size)))

    (when (= new-page current-page)
      (setq new-vscroll (+ (window-vscroll nil t) step-size)))


    (if (cond ((< new-page current-page)
               (goto-char (image-roll-page-to-pos new-page))
               (setf (image-roll-current-page) (max new-page 1))
               (image-set-window-vscroll
                (if (< new-page 1)
                    (user-error "Beginning of document")

                  (- (image-roll-overlay-height (image-roll-current-page))
                     remaining-height))))
              ((> new-page current-page)
               ;; (print "hier")
               (goto-char (image-roll-page-to-pos new-page))
               (setf (image-roll-current-page) (min new-page image-roll-last-page))
               (if (> new-page image-roll-last-page)
                   (user-error "End of document")
                 (image-set-window-vscroll
                  remaining-height)))
               ((> new-vscroll
                   visible-pages-vscroll-limit)
                (image-set-window-vscroll (if (< new-page image-roll-last-page)
                                              new-vscroll
                                            (user-error "End of document")))))
                                            ;; (- visible-pages-vscroll-limit
                                            ;;    image-roll-vertical-margin)
        (image-roll-update-displayed-pages)
      (image-set-window-vscroll new-vscroll))))

(defun image-roll-scroll-backward ()
  (interactive)
  (image-roll-scroll-forward t))

(defun image-roll-scroll-screen-forward ()
  (interactive)
  (image-roll-scroll-forward nil t))

(defun image-roll-scroll-screen-backward ()
  (interactive)
  (image-roll-scroll-forward t t))

(defun image-roll-quick-scroll ()
  (interactive)
  (dotimes (i 200)
    (image-roll-scroll-forward)
    (sit-for 0.0001)))

(defun image-roll-demo-display-page (page)
  "Return demo image of page.
This function is used for the image-roll-demo."
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
  (add-hook 'window-configuration-change-hook 'image-roll-redisplay nil t)
  (add-hook 'image-mode-new-window-functions 'image-roll-new-window-function nil t)
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil)))
;; (add-hook 'window-configuration-change-hook
;;           #'image-mode-reapply-winprops nil t))
;; (image-mode-setup-winprops))

(setq image-roll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<down>") 'image-roll-scroll-forward)
        (define-key map (kbd "<up>") 'image-roll-scroll-backward)
        (define-key map (kbd "<next>") 'image-roll-next-page)
        (define-key map (kbd "<prior>") 'image-roll-previous-page)
        (define-key map (kbd "S-<next>") 'image-roll-scroll-screen-forward)
        (define-key map (kbd "S-<prior>") 'image-roll-scroll-screen-backward)
        (define-key map [remap goto-line] 'image-roll-goto-page)
        map))

(defun image-roll-demo (&optional page-size pages)
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

(provide 'image-roll)

;;; image-roll.el ends here
