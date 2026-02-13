;;; pdf-links.el --- Handle PDF links. -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
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

(require 'pdf-info)
(require 'pdf-util)
(require 'pdf-misc)
(require 'pdf-cache)
(require 'pdf-isearch)
(require 'let-alist)
(require 'org)



;;; Code:



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-links nil
  "Following links in PDF documents."
  :group 'pdf-tools)

(defface pdf-links-read-link
  '((((background dark)) (:background "red" :foreground "yellow"))
    (((background light)) (:background "red" :foreground "yellow")))
  "Face used to determine the colors when reading links."
  ;; :group 'pdf-links
  :group 'pdf-tools-faces)

(defcustom pdf-links-read-link-convert-commands
  '(;;"-font" "FreeMono"
    "-pointsize" "%P"
    "-undercolor" "%f"
    "-fill" "%b"
    "-draw" "text %X,%Y '%c'")

  "The commands for the convert program, when decorating links for reading.
See `pdf-util-convert' for an explanation of the format.

Aside from the description there, two additional escape chars are
available.

%P -- The scaled font pointsize, i.e. IMAGE-WIDTH * SCALE (See
 `pdf-links-convert-pointsize-scale').
%c -- String describing the current link key (e.g. AA, AB,
 etc.)."
  :group 'pdf-links
  :type '(repeat string)
  :link '(variable-link pdf-isearch-convert-commands)
  :link '(url-link "http://www.imagemagick.org/script/convert.php"))

(defcustom pdf-links-convert-pointsize-scale 0.01
  "The scale factor for the -pointsize convert command.

This determines the relative size of the font, when interactively
reading links."
  :group 'pdf-links
  :type '(restricted-sexp :match-alternatives
                          ((lambda (x) (and (numberp x)
                                            (<= x 1)
                                            (>= x 0))))))

(defcustom pdf-links-browse-uri-function
  'pdf-links-browse-uri-default
  "The function for handling uri links.

This function should accept one argument, the URI to follow, and
do something with it."
  :group 'pdf-links
  :type 'function)

(defvar pdf-links--child-frame nil)
(defcustom pdf-links-child-frame-parameters
  '((name . "PDF links preview")
    (height . 0.25)
    (child-frame-border-width . 2)
    (minibuffer . nil)
    (unsplittable . t)
    (no-other-frame . t)
    (auto-hide-function . pdf-links-hide-childframe)
    ;; Setting fringes to zero causes Emacs to reserve space for truncation
    ;; and continuation glyphs which end up taking more space.
    (left-fringe . 1)
    (right-fringe . 1)
    (undecorated . t))
  "Frame parameters for the childframe used by `pdf-links-preview-in-childframe'."
  :group 'pdf-links
  :type '(alist :key-type symbol :value-type sexp)
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (when pdf-links--child-frame
           (modify-frame-parameters pdf-links--child-frame val))))

(defcustom pdf-links-child-frame-size '(0.6 . 0.3)
  "The size of the child frame relative to the pdf window.
The car is the width of the child frame and should be a number between 0 and 1.
The cdr is the height should be between 0 and 0.5"
  :group 'pdf-links
  :type '(cons float float))


;; * ================================================================== *
(defcustom pdf-links-child-frame-auto-preview-wait nil
  "Duration in seconds to wait before the preview when the cursor is on a link.
Value of nil disables the preview."
  :group 'pdf-links
  :type 'float)

(defvar pdf-links-child-frame-vscroll nil
  "Determines how far to scroll initially during child frame preview.
It can be a floating point number between 0 and 1. In this case the preview
will be started with this fraction of page scrolled out of view. This is useful
for documents missing precise location information. The default value of nil
is adequate for documents with accurate location for links.

It can also be function accepting two arguments. The first argument is
the fractional location of the top of the link target given in the
link. The second argument is the fractional size of the top margin on
the page. It should return a floating point number between 0 and 1
which gives the amount to scroll.")

;; * Minor Mode
;; * ================================================================== *

(defvar pdf-links-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "f") 'pdf-links-isearch-link)
    (define-key kmap (kbd "F") 'pdf-links-action-perform)
    kmap))

(defvar pdf-links--auto-preview-state nil)

;;;###autoload
(define-minor-mode pdf-links-minor-mode
  "Handle links in PDF documents.

If this mode is enabled, most links in the document may be
activated by clicking on them or by pressing \\[pdf-links-action-perform] and selecting
one of the displayed keys, or by using isearch limited to
links via \\[pdf-links-isearch-link].

\\{pdf-links-minor-mode-map}"
  :group 'pdf-links
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-links-minor-mode
    (pdf-view-add-hotspot-function 'pdf-links-hotspots-function 0))
   (t
    (pdf-view-remove-hotspot-function 'pdf-links-hotspots-function)))
  (pdf-view-redisplay t))

(defun pdf-links--make-help-echo (id link)
  "Return a lambda returning help-echo for LINK and ID."
  (lambda (win buf _pos)
    (when (and pdf-links-child-frame-auto-preview-wait
               (not (eq id (car pdf-links--auto-preview-state)))
               (eq (alist-get 'type link) 'goto-dest))
      (unless (not pdf-links--auto-preview-state)
        (cancel-timer (nth 1 pdf-links--auto-preview-state)))
      (with-current-buffer buf
        (setq pdf-links--auto-preview-state
              (list
               id
               (run-with-timer
                pdf-links-child-frame-auto-preview-wait
                pdf-links-child-frame-auto-preview-wait
                #'pdf-links--timer link (pdf-view-current-page win)
                buf win)
               nil))))
    (unless (and tooltip-mode pdf-links--auto-preview-state)
     (pdf-links-action-to-string link))))

(defun pdf-links-hotspots-function (page size)
  "Create hotspots for links on PAGE using SIZE."
  (let ((links (pdf-cache-pagelinks page))
        (id-fmt "link-%d-%d")
        (i 0)
        (pointer 'hand)
        hotspots)
    (dolist (l links)
      (push `(link-page . ,page) l)
      (let ((e (pdf-util-scale
                (cdr (assq 'edges l)) size 'round))
            (id (intern (format id-fmt page
                                (cl-incf i)))))
        (push `((rect . ((,(nth 0 e) . ,(nth 1 e))
                         . (,(nth 2 e) . ,(nth 3 e))))
                ,id
                (pointer ,pointer
                 help-echo ,(pdf-links--make-help-echo id l)))
              hotspots)
        (local-set-key
         (vector id 'mouse-1)
         (lambda nil
           (interactive "@")
           (pdf-links-action-perform l)))
        (local-set-key
         (vector id t)
         'pdf-util-image-map-mouse-event-proxy)))
    (nreverse hotspots)))

(defun pdf-links-action-to-string (link)
  "Return a string representation of action for LINK."
  (let-alist link
    (concat
     (cl-case .type
       (goto-dest
        (if (> .page 0)
            (format "Goto page %d" .page)
          "Destination not found"))
       (goto-remote
        (if (and .filename (file-exists-p .filename))
            (format "Goto %sfile '%s'"
                    (if (> .page 0)
                        (format "p.%d of " .page)
                      "")
                    .filename)
          (format "Link to nonexistent file '%s'" .filename)))
       (uri
        (if (> (length .uri) 0)
            (format "Link to uri '%s'" .uri)
          (format "Link to empty uri")))
       (t (format "Unrecognized link type: %s" .type)))
     (if (> (length .title) 0)
         (format " (%s)" .title)))))

;;;###autoload
(defun pdf-links-action-perform (link)
  "Follow LINK, depending on its type.

This may turn to another page, switch to another PDF buffer or
invoke `pdf-links-browse-uri-function'.

Interactively, link is read via `pdf-links-read-link-action'.
This function displays characters around the links in the current
page and starts reading characters (ignoring case).  After a
sufficient number of characters have been read, the corresponding
link's link is invoked.  Additionally, SPC may be used to
scroll the current page."
  (interactive
   (list (or (pdf-links-read-link-action "Activate link (SPC scrolls): ")
             (error "No link selected"))))
  (let-alist link
    (cl-case .type
      ((goto-dest goto-remote)
       (let ((window (selected-window)))
         (cl-case .type
           (goto-dest
            (unless (> .page 0)
              (error "Link points to nowhere")))
           (goto-remote
            (unless (and .filename (file-exists-p .filename))
              (error "Link points to nonexistent file %s" .filename))
            (setq window (display-buffer
                          (or (find-buffer-visiting .filename)
                              (find-file-noselect .filename))))))
         (with-selected-window window
           (when (derived-mode-p 'pdf-view-mode)
             (when (> .page 0)
               (pdf-view-goto-page .page window))
             (when .top
               (when (derived-mode-p 'pdf-view-mode)
                 (pdf-util-tooltip-arrow .top)))))))
      (uri
       (funcall pdf-links-browse-uri-function .uri))
      (t
       (error "Unrecognized link type: %s" .type)))
    nil))

(defvar pdf-links-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'pdf-links-preview-follow-link)
    (define-key map (kbd "q") #'pdf-links-hide-child-frame)
    (define-key map (kbd "<down>") #'pdf-links-scroll-up-child-frame)
    (define-key map (kbd "<up>") #'pdf-links-scroll-down-child-frame)
    map)
  "Keymap for `pdf-links-preview-mode'.")

(define-minor-mode pdf-links-preview-mode
  "Minor mode that is active when previewing pdf links in a child frame.
It is turned on and off automatically and shouldn't be used manually except
for binding keys on `pdf-links-preview-mode-map'. These bindings will be
active only when the preview is active. By default `q' can be used to dismiss
the preview and up and down arrow keys can be used to scroll it.

The mode is non-interactive and can only be enabled/disabled by calling it
from Lisp with a suitable ARG."
  :global t
  :interactive nil
  (if pdf-links-preview-mode
      (push  `((pdf-links-preview-mode . ,pdf-links-preview-mode-map))
             emulation-mode-map-alists)
    (cl-callf2 cl-delete 'pdf-links-preview-mode emulation-mode-map-alists
      :key (lambda (x) (if (consp x) (caar x) x)))))

(defun pdf-links--position-child-frame (link width height use-mouse-pos)
  "Return top and left of child frame of HEIGHT and WIDTH for LINK.
If USE-MOUSE-POS is non-nil consider mouse position to be link position."
  (let* ((edges (if use-mouse-pos
                    (let ((mouse (cdr (mouse-pixel-position)))
                          (offset (frame-char-height)))
                      (list (- (car mouse) offset)
                            (- (cdr mouse) offset)
                            (+ (car mouse) offset)
                            (+ (cdr mouse) offset)))
                    (pdf-links--frame-edges link)))
         (win-edges (window-edges nil t nil t))
         (top-pos (- (nth 1 edges) height))
         (offset (frame-char-width))
         (top-pos-adjusted (- top-pos offset))
         (mid (/ (+ (nth 0 edges) (nth 2 edges)) 2))
         (left-pos (- mid (/ width 2)))
         (right-lim (- (nth 2 win-edges) mid)))
    (cons (if (> top-pos (nth 1 win-edges))
              (if (> top-pos-adjusted (nth 1 win-edges))
                  top-pos-adjusted
                (nth 1 win-edges))
            (if (> (- (+ (nth 3 win-edges) offset) (nth 3 edges)) height)
                (+ (nth 3 edges) offset)
              (nth 3 edges)))
          (if (> left-pos (nth 0 win-edges))
              (- left-pos (if (> right-lim (/ width 2))
                              0
                            (- (/ width 2) right-lim)))
            (nth 0 win-edges)))))

(defun pdf-links--timer (link page buf window)
  "Function to manage preview for LINK on PAGE of BUF displayed in WINDOW."
  (let ((stop nil))
    (with-current-buffer buf
      (if (and (eq buf (window-buffer window))
               (eq page (pdf-view-current-page window)))
          (with-selected-window window
            (cl-destructuring-bind (x1 y1 x2 y2) (pdf-links--frame-edges link)
              (cl-destructuring-bind (_frame x . y) (mouse-pixel-position)
                (let ((insidep (and (< x1 x) (< y1 y) (< x x2) (< y y2)))
                      (status (nth 2 pdf-links--auto-preview-state)))
                  (cond
                   ((and insidep (not status))
                    (pdf-links-preview-in-child-frame link)
                    (setf (nth 2 pdf-links--auto-preview-state) t))
                   ((and (not insidep) status)
                    (let ((child-pos (frame-position pdf-links--child-frame))
                          (parent-pos (frame-position)))
                      (setq x1 (- (car child-pos) (car parent-pos)))
                      (setq y1 (- (cdr child-pos) (car parent-pos)))
                      (setq x2 (+ x1 (frame-native-width pdf-links--child-frame)))
                      (setq y2 (+ y1 (frame-native-height pdf-links--child-frame)))
                      (unless (and (< x1 x) (< y1 y) (< x x2) (< y y2))
                        (setq stop t))))
                   ((not status) (setq stop t)))))))
        (setq stop t)))
    (when stop
      (select-window (frame-selected-window (window-frame window)))
      (pdf-links-hide-child-frame))))

(defun pdf-links-preview-vscroll (top bb)
  "Return vscroll fraction according to TOP and BB."
  (cond ((floatp pdf-links-child-frame-vscroll) pdf-links-child-frame-vscroll)
        ((functionp pdf-links-child-frame-vscroll)
         (funcall pdf-links-child-frame-vscroll top bb))
        (t (- top bb))))

(defun pdf-links-preview-in-child-frame (link &optional use-mouse-pos)
  "Preview the LINK if it points to a destination in the same pdf.
Otherwise follow it using `pdf-links-action-perform'. If USE-MOUSE-POS
is non-nil assume link to be at mouse position."
  (interactive
   (list (or (pdf-links-read-link-action "Activate link: ")
             (error "No link selected"))))
  (let-alist link
    (if (and (eq .type 'goto-dest) (> .page 0))
        (let* ((width (floor (* (car pdf-links-child-frame-size)
                                (window-pixel-width))))
               (bb (pdf-cache-boundingbox .page))
               (slice (pdf-view-bounding-box-to-slice bb))
               (page-width (floor (/ width (nth 2 slice))))
               (height (floor (* (cdr pdf-links-child-frame-size)
                                 (window-pixel-height))))
               (page (create-image (pdf-cache-renderpage
                                    .page page-width page-width)
                                   (pdf-view-image-type) t
                                   :width page-width))
               (buffer (get-buffer-create " *pdf-link-preview*" t))
               (top-left (pdf-links--position-child-frame
                          link width height use-mouse-pos))
               (window nil))
          (unless (frame-live-p pdf-links--child-frame)
            (setq pdf-links--child-frame
                  (make-frame `((parent-frame . ,(selected-frame))
                                . ,pdf-links-child-frame-parameters))))
          (setq window (frame-selected-window pdf-links--child-frame))
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (erase-buffer)
            (setq-local image-scaling-factor 1)
            (insert-image page nil nil
                          (pdf-util-scale slice (image-size page t) 'round))
            (setq mode-line-format nil
                  cursor-type nil)
            (goto-char (point-max))
            (insert "\n"
                    ;; This makes scrolling to the end of page much better with
                    ;; pixel scroll precision mode.
                    (propertize " " 'display '(space :width 25 :height 1000)))
            (image-mode-setup-winprops))
          (set-window-point window (point-min))
          (set-window-vscroll
           window (max 0 (* (pdf-links-preview-vscroll .top (nth 1 bb))
                            (cdr (image-size page t))))
           t)
          (modify-frame-parameters pdf-links--child-frame
                                   `((visibility . t)
                                     (parent-frame . ,(selected-frame))
                                     (top . ,(car top-left))
                                     (left . ,(cdr top-left))
                                     (width text-pixels . ,width)
                                     (height text-pixels . ,height)
                                     (link . ,link)))
          (pdf-links-preview-mode))
      (pdf-links-action-perform link))))

(defun pdf-links-preview-toggle-mouse (link)
  "Toggle the preview for LINK."
  (if (and (frame-live-p pdf-links--child-frame)
           (frame-visible-p pdf-links--child-frame)
           (eq (aref (this-command-keys-vector) 0)
               (frame-parameter pdf-links--child-frame 'link-id)))
      (pdf-links-hide-child-frame)
    (pdf-links-preview-in-child-frame link t)
    (set-frame-parameter pdf-links--child-frame
                         'link-id (aref (this-command-keys-vector) 0))))

(defun pdf-links-preview-follow-link ()
  "Follow the link being previewed."
  (interactive)
  (pdf-links-hide-child-frame)
  (pdf-links-action-perform (frame-parameter pdf-links--child-frame 'link)))

(defun pdf-links-hide-child-frame (&optional frame)
  "Function to hide the childframe FRAME."
  (interactive (list pdf-links--child-frame))
  (when-let ((frame (or frame pdf-links--child-frame))
             ((frame-live-p frame)))
    (make-frame-invisible frame)
    (pdf-links-preview-mode -1))
  (when pdf-links--auto-preview-state
    (cancel-timer (nth 1 pdf-links--auto-preview-state))
    (setq pdf-links--auto-preview-state nil)))

(defun pdf-links-scroll-up-child-frame (&optional n)
  "Scroll up the preview in child frame by N lines."
  (interactive "p")
  (when-let ((frame (and (frame-live-p pdf-links--child-frame)
                         pdf-links--child-frame)))
    (with-selected-window (frame-selected-window frame)
      (image-next-line n))))

(defun pdf-links-scroll-down-child-frame (&optional n)
  "Scroll down the preview in child frame by N lines."
  (interactive "p")
  (when-let ((frame (and (frame-live-p pdf-links--child-frame)
                         pdf-links--child-frame)))
    (with-selected-window (frame-selected-window frame)
      (image-previous-line n))))

(defun pdf-links-redirect-event-to-child-frame (event)
  "Redirect EVENT to child frame by changing the window in it."
  (interactive "e")
  (unless (eq (window-frame (nth 0 (nth 1 event))) pdf-links--child-frame)
    (setf (nth 0 (nth 1 event)) (frame-selected-window pdf-links--child-frame))
    (push event unread-command-events)))

(defun pdf-links-preview-command-was-redirected-p (cmd)
  "Return CMD unless `last-command' was `pdf-links-redirect-event-to-child-frame'."
  (unless (eq last-command #'pdf-links-redirect-event-to-child-frame)
    cmd))

(defun pdf-links-preview-bind-redirection (key)
  "Bind KEY to be redirected using `pdf-links-redirect-event-to-child-frame'."
  (define-key pdf-links-preview-mode-map (kbd key)
              '(menu-item "" pdf-links-redirect-event-to-child-frame
                          :filter pdf-links-preview-command-was-redirected-p)))

(pdf-links-preview-bind-redirection "<wheel-up>")
(pdf-links-preview-bind-redirection "<wheel-down>")
(pdf-links-preview-bind-redirection "<touch-end>")

(defun pdf-links--frame-edges (link)
  "Return the position of the LINK edges on the frame coordinate system."
  (let-alist link
    (let* ((win-edges (window-edges nil t nil t))
           (win-width (- (nth 2 win-edges) (nth 0 win-edges)))
           (image (pdf-view-page-image .link-page))
           (image-width (car (image-display-size image t)))
           (image (or (assoc 'image image) image))
           (prefix (if (> win-width image-width)
                       (/ (- win-width image-width) 2)
                     0))
           (edges (pdf-util-scale .edges
                                  (image-size image t) 'round))
           (image-offset (pdf-view-image-offset nil .link-page))
           (y-offset (+ (nth 1 win-edges)
                        (if (eq (pdf-view-current-page) .link-page)
                            (- (window-vscroll nil t))
                          ;; `pos-visible-in-window-p' seems to be unreliable
                          ;; for large images. So we check the visibility of
                          ;; margin line and add its height to get the start
                          ;; of page in window.
                          ;; TO DO: Reproduce in `emacs -Q' and file a bug report.
                          (+ (nth 1 (pos-visible-in-window-p
                                     (- (* 4 .link-page) 5) nil t))
                             (line-pixel-height)))
                        (- (cdr image-offset))))
           (x-offset (+ (nth 0 win-edges)
                        (- (* (frame-char-height) (window-hscroll)))
                        prefix
                        (- (car image-offset)))))
      (list (+ (nth 0 edges) x-offset)
            (+ (nth 1 edges) y-offset)
            (+ (nth 2 edges) x-offset)
            (+ (nth 3 edges) y-offset)))))

(defun pdf-links--links-for-pages (pages)
  "Return a list of list for links on PAGES."
  (let (links)
    (dolist (page pages)
      (let (pagelinks)
        (dolist (link (pdf-cache-pagelinks page))
          (push `(link-page . ,page) link)
          (push link pagelinks))
        (push (nreverse pagelinks) links)))
    (nreverse links)))

(defun pdf-links-read-link-action (prompt)
  "Using PROMPT, interactively read a link-action.

See `pdf-links-action-perform' for the interface."

  (pdf-util-assert-pdf-window)
  (let* ((win (selected-window))
         (pages (pdf-view-displayed-pages win))
         (links (pdf-links--links-for-pages pages))
         (keys (pdf-links-read-link-action--create-keys
                (apply #'+ (mapcar #'length links))))
         (alist (cl-mapcar 'cons keys (apply #'append links)))
         (colors (pdf-util-face-colors
                  'pdf-links-read-link pdf-view-dark-minor-mode)))
    (if (not links)
        (error "No links on displayed pages")
      (unwind-protect
          (progn
            (dolist (page pages)
              (let* ((image (pdf-view-page-image page))
                     (image (or (assoc 'image image) image))
                     (height (cdr (image-size image t)))
                     (orig-image (create-image (plist-get (cdr image) :data)
                                               (pdf-view-image-type) t)))
                (pdf-view-display-image
                 (create-image (pdf-util-convert-image
                                orig-image
                                :foreground (car colors)
                                :background (cdr colors)
                                :formats
                                `((?c . ,(lambda (_edges) (apply #'string (pop keys))))
                                  (?P . ,(number-to-string
                                          (max 1 (* height
                                                    pdf-links-convert-pointsize-scale)))))
                                :commands pdf-links-read-link-convert-commands
                                :apply (pdf-util-scale
                                        (mapcar (lambda (l) (cdr (assq 'edges l)))
                                                (pop links))
                                        (image-size orig-image t)))
                               (pdf-view-image-type) t
                               :height height)
                 page win)))
            (pdf-links-read-link-action--read-chars prompt alist))
        (pdf-view-redisplay)))))

(defun pdf-links-read-link-action--read-chars (prompt alist)
  (catch 'done
    (let (key)
      (while t
        (let* ((chars (append (mapcar 'caar alist)
                              (mapcar 'downcase (mapcar 'caar alist))
                              (list ?\s)))
               (ch (read-char-choice prompt chars)))
          (setq ch (upcase ch))
          (cond
           ((= ch ?\s)
            (when (= (window-vscroll) (image-scroll-up))
              (image-scroll-down (window-vscroll))))
           (t
            (setq alist (delq nil (mapcar (lambda (elt)
                                            (and (eq ch (caar elt))
                                                 (cons (cdar elt)
                                                       (cdr elt))))
                                          alist))
                  key (append key (list ch))
                  prompt (concat prompt (list ch)))
            (when (= (length alist) 1)
              (message nil)
              (throw 'done (cdar alist))))))))))

(defun pdf-links-read-link-action--create-keys (n)
  (when (> n 0)
    (let ((len (1+ (floor (log n 26))))
          keys)
      (dotimes (i n)
        (let (key)
          (dotimes (_x len)
            (push (+ (% i 26) ?A) key)
            (setq i (/ i 26)))
          (push key keys)))
      (nreverse keys))))

(defun pdf-links-isearch-link (&optional action)
  "Use isearch to find a link.
If non-nil ACTION should be function of one argument: the selected link."
  (interactive)
  (let* (quit-p
         (isearch-mode-end-hook
          (cons (lambda nil
                  (setq quit-p isearch-mode-end-hook-quit))
                isearch-mode-end-hook))
         (pdf-isearch-filter-matches-function
          'pdf-links-isearch-link-filter-matches)
         (pdf-isearch-narrow-to-page t)
         (isearch-message-prefix-add "(Links)")
         pdf-isearch-batch-mode)
    (isearch-forward)
    (unless (or quit-p (null pdf-isearch-current-match))
      (let* ((page (when (memq pdf-isearch-current-page
                               (pdf-view-displayed-pages))
                     pdf-isearch-current-page))
             (match (car pdf-isearch-current-match))
             (size (pdf-view-image-size))
             (links (sort (cl-remove-if
                           (lambda (e)
                             (= 0 (pdf-util-edges-intersection-area (car e) match)))
                           (mapcar (lambda (l)
                                     (cons (pdf-util-scale (alist-get 'edges l) size)
                                           l))
                                   (pdf-cache-pagelinks page)))
                          (lambda (e1 e2)
                            (> (pdf-util-edges-intersection-area
                                (alist-get 'edges e1) match)
                               (pdf-util-edges-intersection-area
                                (alist-get 'edges e2) match))))))
        (unless links
          (error "No link found at this position"))
        (funcall (or action 'pdf-links-action-perform)
                 (push `(link-page . ,page) (car links)))))))

(defun pdf-links-isearch-preview-in-child-frame ()
  "Use isearch to select a link and preview it in a child frame."
  (interactive)
  (pdf-links-isearch-link 'pdf-links-preview-in-child-frame))

(defun pdf-links-isearch-link-filter-matches (matches)
  (let ((links (pdf-util-scale
                (mapcar (apply-partially 'alist-get 'edges)
                        (pdf-cache-pagelinks
                         (pdf-view-current-page)))
                (pdf-view-image-size))))
    (cl-remove-if-not
     (lambda (m)
       (cl-some
        (lambda (edges)
          (cl-some (lambda (link)
                     (pdf-util-with-edges (link edges)
                       (let ((area (min (* link-width link-height)
                                        (* edges-width edges-height))))
                         (>  (/  (pdf-util-edges-intersection-area edges link)
                                 (float area)) 0.5))))
                   links))
        m))
     matches)))

(defun pdf-links-browse-uri-default (uri)
  "Open the string URI using Org.

Wraps the URI in \[\[ ... \]\] and calls `org-open-link-from-string'
on the resulting string."
  (cl-check-type uri string)
  (message "Opening `%s' with Org" uri)
  (cond
   ((fboundp 'org-link-open-from-string)
    (org-link-open-from-string (format "[[%s]]" uri)))
   ;; For Org 9.2 and older
   ((fboundp 'org-open-link-from-string)
    (org-open-link-from-string (format "[[%s]]" uri)))))

(provide 'pdf-links)

;;; pdf-links.el ends here
