;;; pdf-misc.el --- Miscellaneous commands for PDF buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Andreas Politz
;; Copyright (C) 2025  Benjamin Slade

;; Author: Andreas Politz <politza@fh-trier.de>
;; Maintainer: Vedang Manerikar <vedang.manerikar@gmail.com>
;; URL: https://github.com/vedang/pdf-tools/
;; Package-Requires: ((emacs "24.4") (org "9.4"))
;; Package-Version: 0.11
;; Version: 0.11
;; Keywords: files, multimedia

;; This file is NOT part of GNU Emacs.

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; miscellaneous commands for PDF buffers: metadata, printing, &c.

;;; Code:

(require 'pdf-view)
(require 'pdf-util)
(require 'imenu)
(require 'pdf-tools) ; for `pdf-tools-pdf-buffer-p'
(require 'org)


;;; Misc. group and defcustom definitions

(defgroup pdf-misc nil
  "Miscellaneous options for PDF documents."
  :group 'pdf-tools)

(defcustom pdf-misc-print-program-executable nil
  "Executable for printing."
  :group 'pdf-tools
  :type 'string)

;;; Keybindings

(defvar pdf-misc-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `I' for `Info', = pdf metadata:
    (define-key map (kbd "I") #'pdf-misc-display-metadata)
    ;; `O' for `Org-style' Info, = pdf metadata in orgish display:
    (define-key map (kbd "O") #'pdf-misc-display-metadata-org-style)
    ;; `T' for `Typeface', i.e., Font info [since `F' is already taken]:
    (define-key map (kbd "T") #'pdf-misc-display-font-information)
    ;; `U' for `Unified' info, i.e., both Metadata and Font info:
    (define-key map (kbd "U") #'pdf-misc-display-combined-metadata-and-font-info)
    ;; `c'ommand to `p'rint:
    (define-key map (kbd "C-c C-p") #'pdf-misc-print-document)
    map)
  "Keymap used in `pdf-misc-minor-mode'.")


;;;###autoload
(define-minor-mode pdf-misc-minor-mode
  "FIXME:  Not documented."
  :group 'pdf-misc)

;;;###autoload
(define-minor-mode pdf-misc-size-indication-minor-mode
  "Provide a working size indication in the mode-line."
  :group 'pdf-misc
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-misc-size-indication-minor-mode
    (unless (assq 'pdf-misc-size-indication-minor-mode
                  mode-line-position)
      (setq mode-line-position
            `((pdf-misc-size-indication-minor-mode
               (:eval (pdf-misc-size-indication)))
              ,@mode-line-position))))
   (t
    (setq mode-line-position
          (cl-remove 'pdf-misc-size-indication-minor-mode
                     mode-line-position :key 'car-safe)))))

(defun pdf-misc-size-indication ()
  "Return size indication string for the mode-line."
  (let ((top (= (window-vscroll nil t) 0))
        (bot (>= (+ (- (nth 3 (window-inside-pixel-edges))
                       (nth 1 (window-inside-pixel-edges)))
                    (window-vscroll nil t))
                 (cdr (pdf-view-image-size t)))))
    (cond
     ((and top bot) " All")
     (top " Top")
     (bot " Bot")
     (t (format
         " %d%%%%"
         (ceiling
          (* 100 (/ (float (window-vscroll nil t))
                    (cdr (pdf-view-image-size t))))))))))

(defvar pdf-misc-menu-bar-minor-mode-map (make-sparse-keymap)
  "The keymap used in `pdf-misc-menu-bar-minor-mode'.")

(easy-menu-define nil pdf-misc-menu-bar-minor-mode-map
  "Menu for PDF Tools."
  `("PDF Tools"
    ["Go Backward" pdf-history-backward
     :visible (bound-and-true-p pdf-history-minor-mode)
     :active (and (bound-and-true-p pdf-history-minor-mode)
                  (not (pdf-history-end-of-history-p)))]
    ["Go Forward" pdf-history-forward
     :visible (bound-and-true-p pdf-history-minor-mode)
     :active (not (pdf-history-end-of-history-p))]
    ["--" nil
     :visible (derived-mode-p 'pdf-virtual-view-mode)]
    ["Next file" pdf-virtual-buffer-forward-file
     :visible  (derived-mode-p 'pdf-virtual-view-mode)
     :active (pdf-virtual-document-next-file
              (pdf-view-current-page))]
    ["Previous file" pdf-virtual-buffer-backward-file
     :visible (derived-mode-p 'pdf-virtual-view-mode)
     :active (not (eq 1 (pdf-view-current-page)))]
    ["--" nil
     :visible (bound-and-true-p pdf-history-minor-mode)]
    ["Add text annotation" pdf-annot-mouse-add-text-annotation
     :visible (bound-and-true-p pdf-annot-minor-mode)
     :keys "\\[pdf-annot-add-text-annotation]"]
    ("Add markup annotation"
     :active (pdf-view-active-region-p)
     :visible (and (bound-and-true-p pdf-annot-minor-mode)
                   (pdf-info-markup-annotations-p))
     ["highlight" pdf-annot-add-highlight-markup-annotation]
     ["squiggly" pdf-annot-add-squiggly-markup-annotation]
     ["underline" pdf-annot-add-underline-markup-annotation]
     ["strikeout" pdf-annot-add-strikeout-markup-annotation])
    ["--" nil :visible (bound-and-true-p pdf-annot-minor-mode)]
    ["Display Annotations" pdf-annot-list-annotations
     :help "List all annotations"
     :visible (bound-and-true-p pdf-annot-minor-mode)]
    ["Display Attachments" pdf-annot-attachment-dired
     :help "Display attachments in a dired buffer"
     :visible (featurep 'pdf-annot)]
    ["Display Metadata" pdf-misc-display-metadata
     :help "Display information about the document"
     :visible (featurep 'pdf-misc)]
    ["Display Outline" pdf-outline
     :help "Display documents outline"
     :visible (featurep 'pdf-outline)]
    "--"
    ("Render Options"
     ["Printed Mode" (lambda ()
                       (interactive)
                       (pdf-view-printer-minor-mode 'toggle))
      :style toggle
      :selected pdf-view-printer-minor-mode
      :help "Display the PDF as it would be printed."]
     ["Midnight Mode" (lambda ()
                        (interactive)
                        (pdf-view-midnight-minor-mode 'toggle))
      :style toggle
      :selected pdf-view-midnight-minor-mode
      :help "Apply a color-filter appropriate for past midnight reading."])
    "--"
    ["Copy region" pdf-view-kill-ring-save
     :keys "\\[kill-ring-save]"
     :active (pdf-view-active-region-p)]
    ("Selection style"
     ["Glyph" (pdf-view-set-selection-style 'glyph)
      :style radio
      :selected (eq pdf-view-selection-style 'glyph)
      :help "When dragging the mouse, select individual characters."]
     ["Word" (pdf-view-set-selection-style 'word)
      :style radio
      :selected (eq pdf-view-selection-style 'word)
      :help "When dragging the mouse, select entire words."]
     ["Line" (pdf-view-set-selection-style 'line)
      :style radio
      :selected (eq pdf-view-selection-style 'line)
      :help "When dragging the mouse, select entire lines."])
    "--"
    ["Isearch document" isearch-forward
     :visible (bound-and-true-p pdf-isearch-minor-mode)]
    ["Occur document" pdf-occur
     :visible (featurep 'pdf-occur)]
    "--"
    ["Locate TeX source" pdf-sync-backward-search-mouse
     :visible (and (featurep 'pdf-sync)
                   (equal last-command-event
                          last-nonmenu-event))]
    ["--" nil :visible (and (featurep 'pdf-sync)
                            (equal last-command-event
                                   last-nonmenu-event))]
    ["Print" pdf-misc-print-document
     :active (and (pdf-view-buffer-file-name)
                  (file-readable-p (pdf-view-buffer-file-name)))]
    ["Create image" pdf-view-extract-region-image
     :help "Create an image of the page or the selected region(s)."]
    ["Create virtual PDF" pdf-virtual-buffer-create
     :help "Create a PDF containing all documents in this directory."
     :visible (bound-and-true-p pdf-virtual-global-minor-mode)]
    "--"
    ["Revert buffer" pdf-view-revert-buffer
     :visible (pdf-info-writable-annotations-p)]
    "--"
    ["Customize" pdf-tools-customize]))

;;;###autoload
(define-minor-mode pdf-misc-menu-bar-minor-mode
  "Display a PDF Tools menu in the menu-bar."
  :group 'pdf-misc
  (pdf-util-assert-pdf-buffer))

(defvar pdf-misc-context-menu-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [down-mouse-3] 'pdf-misc-popup-context-menu)
    kmap))

;;;###autoload
(define-minor-mode pdf-misc-context-menu-minor-mode
  "Provide a right-click context menu in PDF buffers.

\\{pdf-misc-context-menu-minor-mode-map}"
  :group 'pdf-misc
  (pdf-util-assert-pdf-buffer))

(defun pdf-misc-popup-context-menu (_event)
  "Popup a context menu at position."
  (interactive "@e")
  (popup-menu
   (cons 'keymap
         (cddr (or (lookup-key pdf-misc-menu-bar-minor-mode-map
                               [menu-bar PDF\ Tools])
                   (lookup-key pdf-misc-menu-bar-minor-mode-map
                               [menu-bar pdf\ tools]))))))

;;; misc helper functions
(defun pdf-misc--merge-cons-to-string (lst)
  "Merge a list `LST' into a white-space separated string."
  ;; start with first item to avoid leading space
  (let ((unified-field-string (car lst))
        (remain (cdr lst)))
    ;; then process remainder, adding a " " between
    (dolist (w remain)
      (setq unified-field-string
            (concat
             unified-field-string
             " "
             w)))
    unified-field-string))

(defun pdf-misc--flatten-tree (tree)
  "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.

\(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7).
[Taken from subr.el to avoid requiring Emacs 27.1]"
  (declare (side-effect-free error-free))
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

;;; metadata functions

;;;; PDF metadata
(defun pdf-misc-display-metadata ()
  "Display all available metadata in a separate buffer."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (let* ((buffer (current-buffer))
         (md (pdf-info-metadata)))
    (with-current-buffer (get-buffer-create "*PDF-Metadata*")
      (let* ((inhibit-read-only t)
             (pad (apply' max (mapcar (lambda (d)
                                        (length (symbol-name (car d))))
                                      md)))
             (fmt (format "%%%ds:%%s\n" pad)))
        (erase-buffer)
        (setq header-line-format (buffer-name buffer)
              buffer-read-only t)
        (font-lock-mode 1)
        (font-lock-add-keywords nil
          '(("^ *\\(\\(?:\\w\\|-\\)+\\):"
             (1 font-lock-keyword-face))))
        (dolist (d md)
          (let ((key (car d))
                (val (cdr d)))
            (cl-case key
              (keywords
               (setq val (mapconcat 'identity val ", "))))
            (let ((beg (+ (length (symbol-name key)) (point) 1))
                  (fill-prefix
                   (make-string (1+ pad) ?\s)))
              (insert (format fmt key val))
              (fill-region beg (point) )))))
      (goto-char 1)
      (display-buffer (current-buffer)))
    md))

(defun pdf-misc-display-metadata-org-style (doc &optional combined)
  "Display all available metadata in a separate buffer in Org-mode style.
Argument `DOC' defaults to current buffer if it contains a PDF file;
otherwise queries for a PDF file.  The optional argument `COMBINED' is
used when combined with `pdf-misc-display-font-information'."
  (interactive
   (if (pdf-tools-pdf-buffer-p)
       (list (buffer-file-name))
     (list (read-file-name "Choose PDF file:"))))
  (let ((raw-pim (pdf-info-metadata doc))
        (temp-buff-name (if combined
                            "*PDF metadata and font info*"
                            "*PDF metadata*")))
    (get-buffer-create temp-buff-name)
    (with-current-buffer temp-buff-name
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "* PDF metadata for file \"=%s=\":\n"
                      (file-name-nondirectory doc)))
      (dolist (item raw-pim)
        (let ((fname (car item))
              (fval (cdr item)))
          ;; keyword field needs special handling
          (if (and (listp fval)
                   (> (length fval) 1)
                   (equal fname 'keywords))
              (let ((kword1 (cadr fval))
                    (kwords (cddr fval)))
                (insert (format "- =%s=: " fname))
                (insert (format "~%s~"
                                (string-trim kword1)))
                (dolist (kword kwords)
                  (insert
                   (format ", ~%s~"
                           (string-trim kword))))
                (insert "\n"))
            (insert (format "- =%s=: " fname))
            ;; don't format empty strings or values
            (if (or (null fval) (string-empty-p fval))
                (insert "\n")
              (insert (format "~%s~\n"
                              (string-trim fval)))))))
      (org-mode)
      (org-fold-show-all)
      (read-only-mode 1)
      (when (null combined)
        (switch-to-buffer-other-window temp-buff-name))
      (goto-char (point-min)))))

;;;; PDF Font information
(defvar pdf-misc-pdffonts-man-help
  "** Key to the above font information:
*** The following information is listed for each font:
  - =name=: the font name, exactly as given in the PDF file (potentially including
    a subset prefix)
  - =type=: the font type -- see below for details
  - =emb=: \"yes\" if the font is embedded in the PDF file
  - =sub=: \"yes\" if the font is a subset
  - =uni=: \"yes\" if there is an explicit ~ToUnicode~ map in the PDF file (the
    absence of a ~ToUnicode~ map doesn't necessarily mean that the text can't be
    converted to Unicode)
  - =object ID=: the font dictionary object ID (number and generation; given here
    in format ~Number.Generation~)

*** PDF files can contain the following types of fonts:
   - ~Type 1~
   - ~Type 1C~ [= Compact Font Format (CFF)]
   - ~Type 3~
   - ~TrueType~
   - ~CID Type 0~ [= 16-bit font with no specified type]
   - ~CID Type 0C~ [= 16-bit PostScript CFF font]
   - ~CID TrueType~ [= 16-bit TrueType font]

[ adapted from ~man pdffonts~ ]"
  "Key to font data information.
Information about the PDF font information displayed by
`pdf-misc-display-font-information'.")

(defun pdf-misc--extract-pdffonts-info (doc)
  "Non-interactive function to parse the output of `pdffonts'.
Extracts information from calling `pdffonts' utility on PDF document
`DOC'.  Called by `pdf-misc-display-font-information'."
  (unless (executable-find "pdffonts")
    (error "System package `pdffonts' must be installed"))
  (let* ((raw
          (remove ""
                 (split-string
                  (shell-command-to-string
                   (concat "pdffonts " doc))
                  "\n")))
         (body (cddr raw))
         (pdffont-values nil))
    (dolist (line body)
      (let ((raw-font-info (nreverse
                            (split-string line))))
        (let* ((object-id (concat
                           (nth 1 raw-font-info)
                           "."
                           (nth 0 raw-font-info)))
               (uni (nth 2 raw-font-info))
               (sub (nth 3 raw-font-info))
               (emb (nth 4 raw-font-info))
               (encoding (nth 5 raw-font-info))
               (remainder (cdddr (cdddr raw-font-info)))
               (flipback (nreverse remainder))
               (font-name (car flipback))
               (type
                (pdf-misc--merge-cons-to-string
                 (pdf-misc--flatten-tree
                       (cdr flipback)))))
          (setq pdffont-values
                (cons
                 (list
                  font-name
                  type
                  encoding
                  emb
                  sub
                  uni
                  object-id)
                 pdffont-values)))))
    pdffont-values))

(defun pdf-misc-display-font-information (doc &optional combined)
  "Parse the output of `pdffonts' for PDF file `DOC'.
Information is display in an Org-mode table in a temporary buffer.
Includes explanatory information if called with prefix argument.
\(I.e., if command is preceded by `C-u'.\) Optional `COMBINED' argument
alters behaviour for use with
`pdf-misc-display-combined-metadata-and-font-info'."
  (interactive
   (if (pdf-tools-pdf-buffer-p)
       (list (buffer-file-name))
     (list (read-file-name "Choose PDF file:"))))
  (unless (executable-find "pdffonts")
    (error "System package `pdffonts' must be installed"))
  (let ((pdffont-values (pdf-misc--extract-pdffonts-info doc))
        (temp-buff-name (if combined
                            "*PDF metadata and font info*"
                          "*PDF fonts*")))
    (when (null combined)
      (get-buffer-create temp-buff-name))
    (with-current-buffer temp-buff-name
      (read-only-mode -1)
      (if combined
          (progn
            (goto-char (point-max))
            (insert "\n"))
        (erase-buffer))
      (insert (format "* PDF font information for file \"=%s=\":\n"
                      (file-name-nondirectory doc)))
      (insert "|-\n")
      (let ((header '("=name=" "=type=" "=encoding=" "=emb=" "=sub=" "=uni=" "=object ID=")))
          (dolist (field header)
            (insert " | ")
            (insert field)))
      (insert "|\n")
      (insert "|--")
      (insert "\n")
      (goto-char (point-max))
      (dolist (item pdffont-values)
        (let ((ffield t))
          (dolist (field item)
            (insert " | ")
            (if ffield
                (progn
                  (insert (format "~%s~" field))
                  (setq ffield nil))
              (insert (format "%s" field)))))
        (insert "\n"))
      (insert "|-")
      (org-mode)
      (org-fold-show-all)
      (org-table-align)
      (org-table-align) ; needs to be twice to get formatting right
      (goto-char (point-max))
      (when current-prefix-arg
        (insert "\n")
        (insert pdf-misc-pdffonts-man-help))
      (read-only-mode 1)
      (switch-to-buffer-other-window temp-buff-name)
      ;; visual-fill-column-mode will be too narrow
      ;; disable if on:
      (when (and (boundp 'visual-fill-column-mode)
             visual-fill-column-mode)
        (visual-fill-column-mode -1))
      (goto-char (point-min)))))

;;;; Combined PDF metadata and font information display

(defun pdf-misc-display-combined-metadata-and-font-info (doc)
  "Show combined PDF metadata and font information.
Operates on PDF document `DOC', either current buffer, or passed
manually, or user is queried to supply one.  \(Prefixed argument
triggers showing explanatory information for font metadata.\)"
  (interactive
   (if (pdf-tools-pdf-buffer-p)
       (list (buffer-file-name))
     (list (read-file-name "Choose PDF file:"))))
  (pdf-misc-display-metadata-org-style doc t)
  (pdf-misc-display-font-information doc t))


;;; Misc. things, printing
(defgroup pdf-misc nil
  "Miscellaneous options for PDF documents."
  :group 'pdf-tools)

(define-obsolete-variable-alias 'pdf-misc-print-programm
  'pdf-misc-print-program-executable "1.0")
(defcustom pdf-misc-print-program-executable nil
  "The program used for printing.

It is called with one argument, the PDF file."
  :group 'pdf-misc
  :type 'file)

(define-obsolete-variable-alias 'pdf-misc-print-programm-args
  'pdf-misc-print-program-args "1.0")
(defcustom pdf-misc-print-program-args nil
  "List of additional arguments passed to `pdf-misc-print-program'."
  :group 'pdf-misc
  :type '(repeat string))

(define-obsolete-function-alias 'pdf-misc-print-programm
  'pdf-misc-print-program "1.0")
(defun pdf-misc-print-program (&optional interactive-p)
  "Return the program used to print PDFs (if the executable is installed).

If INTERACTIVE-P is non-nil, ask the user for which program to
use when printing the PDF. Optionally, save the choice"
  (or (and pdf-misc-print-program-executable
           (executable-find pdf-misc-print-program-executable))
      (when interactive-p
        (let* ((default (car (delq nil (mapcar
                                        'executable-find
                                        '("gtklp" "xpp" "gpr")))))
               buffer-file-name
               (program
                (expand-file-name
                 (read-file-name
                  "Print with: " default nil t nil 'file-executable-p))))
          (when (and program
                     (executable-find program))
            (when (y-or-n-p "Save choice using customize? ")
              (customize-save-variable
               'pdf-misc-print-program-executable program))
            (setq pdf-misc-print-program-executable program))))))

(defun pdf-misc-print-document (filename &optional interactive-p)
  "Print the PDF doc FILENAME.

`pdf-misc-print-program' handles the print program, which see for
definition of INTERACTIVE-P."
  (interactive
   (list (pdf-view-buffer-file-name) t))
  (cl-check-type filename (and string (satisfies file-readable-p)))
  (let ((program (pdf-misc-print-program interactive-p))
        (args (append pdf-misc-print-program-args (list filename))))
    (unless program
      (error "No print program available"))
    (apply #'start-process "printing" nil program args)
    (message "Print job started: %s %s"
             program (mapconcat #'identity args " "))))


(provide 'pdf-misc)

;;; pdf-misc.el ends here
