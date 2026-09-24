;;; pdf-history.el --- A simple stack-based history in PDF buffers. -*- lexical-binding: t -*-

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

(require 'pdf-view)
(require 'pdf-util)

;;; Code:

(defgroup pdf-history nil
  "A simple stack-based history."
  :group 'pdf-tools)

(defvar-local pdf-history-stack nil
  "The stack of history items.")

(defvar-local pdf-history-index nil
  "The current index into the `pdf-history-stack'.")

(defvar pdf-history-inhibit-jump nil
  "Non-nil when a jump should leave the history alone.

A list buffer that shows whatever the cursor is on moves the document as
you look around: `pdf-outline-follow-mode', `pdf-annot-list-follow-minor-mode'
and `next-error-follow-minor-mode' in an occur buffer all do.  That is
browsing rather than going somewhere, as stepping through an isearch is,
and `pdf-history-before-change-page-hook' declines to record for the same
reason.")

(defvar pdf-history--jump-page nil
  "The page a jump started on.
Set by `pdf-history-before-jump' and read by `pdf-history-after-jump'.")

(defvar pdf-history--inhibit-record nil
  "Non-nil while `pdf-history-goto' changes the page.
The item it goes to already carries the position to restore, so
`pdf-history-record-origin' has to leave it alone.")

(defvar pdf-history-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "B") #'pdf-history-backward)
    (define-key kmap (kbd "N") #'pdf-history-forward)
    (define-key kmap (kbd "l") #'pdf-history-backward)
    (define-key kmap (kbd "r") #'pdf-history-forward)
    kmap)
  "Keymap used in `pdf-history-minor-mode'.")

;;;###autoload
(define-minor-mode pdf-history-minor-mode
  "Keep a history of previously visited pages.

This is a simple stack-based history.  Turning the page or
following a link pushes the left-behind page on the stack, which
may be navigated with the following keys.

\\{pdf-history-minor-mode-map}"
  :group 'pdf-history
  (pdf-util-assert-pdf-buffer)
  (pdf-history-clear)
  (cond
   (pdf-history-minor-mode
    (pdf-history-push)
    (add-hook 'pdf-view-before-change-page-hook
              #'pdf-history-record-origin nil t)
    (add-hook 'pdf-view-after-change-page-hook
              #'pdf-history-before-change-page-hook nil t))
   (t
    (remove-hook 'pdf-view-before-change-page-hook
                 #'pdf-history-record-origin t)
    (remove-hook 'pdf-view-after-change-page-hook
                 #'pdf-history-before-change-page-hook t))))

(defun pdf-history-before-change-page-hook ()
  "Push a history item, before leaving this page."
  (when (and pdf-history-minor-mode
             (not (bound-and-true-p pdf-isearch-active-mode))
             (pdf-view-current-page))
    (pdf-history-push)))

(defun pdf-history-push ()
  "Push the current page on the stack.

This function does nothing, if current stack item already
represents the current page, or if there is no current page yet.
`pdf-history-minor-mode' calls `pdf-history-clear' before the
window it is turned on in has been given one."
  (interactive)
  (let ((item (pdf-history-create-item)))
    (unless (or (null (car item))
                (and pdf-history-stack
                     (eq (car (nth pdf-history-index pdf-history-stack))
                         (car item))))
      (pdf-history--push-item item))))

(defun pdf-history-push-position ()
  "Push the current page and the position on it on the stack.

Unlike `pdf-history-push' this adds an item for a page that is already
on top of the stack, which is what a jump that stays on one page needs:
the place it came from and the place it went to are both on that page."
  (let ((item (pdf-history-create-item)))
    (when (car item)
      (pdf-history--push-item item))))

(defun pdf-history--push-item (item)
  "Put ITEM on the stack, dropping whatever was ahead of the index."
  (setq pdf-history-stack
        (last pdf-history-stack
              (- (length pdf-history-stack)
                 pdf-history-index))
        pdf-history-index 0)
  (push item pdf-history-stack))

(defun pdf-history-clear ()
  "Remove all history items."
  (interactive)
  (setq pdf-history-stack nil
        pdf-history-index 0)
  (pdf-history-push))

(defun pdf-history-create-item ()
  "Create a history item representing the current page.

An item is a list of the page number and the position on that
page, as `pdf-history-current-origin' returns it."
  (list
   (pdf-view-current-page)
   (pdf-history-current-origin)))

(defun pdf-history-current-origin (&optional window)
  "Return where WINDOW is scrolled to on the page it displays.

The value is the upper left corner of the visible region of the
page image, relative to the size of that image, which is what
`pdf-view-bookmark-make-record' stores as the bookmark\='s origin.
It is nil if WINDOW displays no PDF, and nil while WINDOW has no
image to measure: `pdf-view-new-window-function' calls
`pdf-view-goto-page' for a window that has not drawn a page yet,
and `pdf-view-image-size' signals rather than measuring nothing.
That call happens inside redisplay, where a signal is reported
and the rest of the frame is dropped."
  (when (pdf-util-pdf-window-p window)
    (let ((edges (ignore-errors (pdf-util-image-displayed-edges window t))))
      (when edges
        (pdf-util-scale-pixel-to-relative
         (cons (car edges) (cadr edges)) nil t window)))))

(defun pdf-history-restore-origin (origin)
  "Scroll the selected window to ORIGIN on the page it displays.

ORIGIN is a position created by `pdf-history-current-origin'.  A
nil ORIGIN leaves the window at the top of the page, which is
where `pdf-view-goto-page' puts it."
  (when (and origin (pdf-util-pdf-window-p))
    (let ((size (pdf-view-image-size t)))
      (image-set-window-hscroll
       (round (/ (* (car origin) (car size))
                 (frame-char-width))))
      (image-set-window-vscroll
       (round (/ (* (cdr origin) (cdr size))
                 (if pdf-view-have-image-mode-pixel-vscroll
                     1
                   (frame-char-height))))))))

(defun pdf-history-record-origin ()
  "Record in the current item where its page is being left.

This runs from `pdf-view-before-change-page-hook'.  The item
cannot carry the position it is pushed with: `pdf-view-goto-page'
sets the vscroll to 0 before it runs
`pdf-view-after-change-page-hook', where the push happens, and
scrolling within a page runs neither hook."
  (when (and pdf-history-minor-mode
             pdf-history-stack
             (not pdf-history--inhibit-record))
    (let ((item (nth pdf-history-index pdf-history-stack))
          (origin (pdf-history-current-origin)))
      ;; Keep the position the item has if there is nothing to measure.
      (when (and origin (eq (car item) (pdf-view-current-page)))
        (setf (nth 1 item) origin)))))

(defun pdf-history-before-jump ()
  "Note where the window is, before a command jumps it somewhere else.

A command that follows a reference calls this and
`pdf-history-after-jump' around the jump.  Both are needed because a
jump that stays on the page changes no page, so neither
`pdf-view-before-change-page-hook' nor `pdf-view-after-change-page-hook'
runs and the history would not hear about it."
  (when (and pdf-history-minor-mode
             (not pdf-history-inhibit-jump))
    (setq pdf-history--jump-page (pdf-view-current-page))
    (pdf-history-record-origin)))

(defun pdf-history-after-jump ()
  "Record where a jump arrived.  See `pdf-history-before-jump'."
  (when (and pdf-history-minor-mode
             (not pdf-history-inhibit-jump))
    (let ((origin (pdf-history-current-origin)))
      (if (eq (pdf-view-current-page) pdf-history--jump-page)
          ;; Nothing was pushed, because the page did not change.  The
          ;; position the jump reached needs an item of its own, or there is
          ;; nothing to come back from.  A jump that landed where the window
          ;; already was needs none.
          (unless (equal origin
                         (nth 1 (nth pdf-history-index pdf-history-stack)))
            (pdf-history-push-position))
        ;; `pdf-history-push' ran from `pdf-view-after-change-page-hook',
        ;; while `pdf-view-goto-page' had just set the vscroll to 0 and
        ;; before the jump scrolled to its target.  Give the item the
        ;; position the window actually reached.
        (pdf-history-record-origin)))
    (setq pdf-history--jump-page nil)))

(defun pdf-history-beginning-of-history-p ()
  "Return t, if at the beginning of the history."
  (= pdf-history-index 0))

(defun pdf-history-end-of-history-p ()
  "Return t, if at the end of the history."
  (= pdf-history-index
     (1- (length pdf-history-stack))))

(defun pdf-history-backward (n)
  "Go N times backward in the history."
  (interactive "p")
  (cond
   ((and (> n 0)
         (pdf-history-end-of-history-p))
    (error "End of history"))
   ((and (< n 0)
         (pdf-history-beginning-of-history-p))
    (error "Beginning of history"))
   ((/= n 0)
    (let ((i (min (max 0 (+ pdf-history-index n))
                  (1- (length pdf-history-stack)))))
      (prog1
          (- (+ pdf-history-index n) i)
        (pdf-history-goto i))))
   (t 0)))

(defun pdf-history-forward (n)
  "Go N times forward in the history."
  (interactive "p")
  (pdf-history-backward (- n)))

(defun pdf-history-goto (n)
  "Go to item N in the history."
  (interactive "p")
  (when (null pdf-history-stack)
    (error "The history is empty"))
  (cond
   ((>= n (length pdf-history-stack))
    (error "End of history"))
   ((< n 0)
    (error "Beginning of history"))
   (t
    ;; Record where the page on screen is left while the index still
    ;; names its item.
    (pdf-history-record-origin)
    (setq pdf-history-index n)
    (let ((item (nth n pdf-history-stack))
          (pdf-history--inhibit-record t))
      (pdf-view-goto-page (car item))
      (pdf-history-restore-origin (nth 1 item))))))

(defun pdf-history-debug ()
  "Visualize the history in the header-line."
  (interactive)
  (setq header-line-format
        '(:eval
          (let ((pages (mapcar 'car pdf-history-stack))
                (index pdf-history-index)
                header)
            (dotimes (i (length pages))
              (push (propertize
                     (format "%s" (nth i pages))
                     'face
                     (and (= i index) 'match))
                    header))
            (concat
             "(" (format "%d" index) ")  "
             (mapconcat 'identity (nreverse header) " | "))))))

(provide 'pdf-history)

;;; pdf-history.el ends here
