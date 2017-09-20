;;; org-pdfview.el --- Support for links to pdf-view-mode buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: Oleg Pykhalov <go.wigust at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to open files in pdf-view-mode.
;; Org mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;; The links take the form
;;
;;    pdfview:<file path>::<page number>
;;
;; for example: [[pdfview:~/.elisp/org/doc/org.pdf::1][Org-Mode Manual]]
;;
;; Autocompletion for inserting links is supported; you will be
;; prompted for a file and a page number.
;;
;; If you use org-store-link in a pdf-view mode buffer, the stored
;; link will point to the current page.

;;; Code:


(require 'org)

(declare-function pdf-view-goto-page "pdf-view" (page))

(org-link-set-parameters "pdfview"
			 :follow #'org-pdfview-open
			 :export #'org-pdfview-export
			 :store #'org-pdfview-store-link)

(defun org-pdfview-export (link description format)
  "Export a pdfview link from Org files."
  (let* ((path (if (string-match "\\(.+\\)::.+" link) (match-string 1 link)
		 link))
         (desc (or description link)))
    (when (stringp path)
      (setq path (org-link-escape (expand-file-name path)))
      (cond
       ((eq format 'html) (format "<a href=\"%s\">%s</a>" path desc))
       ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
       ((eq format 'ascii) (format "%s (%s)" desc path))
       (t path)))))

(defun org-pdfview-open (link)
  (string-match "\\(.*?\\)\\(?:::\\([0-9]+\\)\\)?$" link)
  (let ((path (match-string 1 link))
	(page (and (match-beginning 2)
		   (string-to-number (match-string 2 link)))))
    ;; Let Org mode open the file (in-emacs = 1) to ensure
    ;; org-link-frame-setup is respected.
    (org-open-file path 1)
    (when page (pdf-view-goto-page page))))

(defun org-pdfview-store-link ()
  "Store a link to a pdfview buffer."
  (when (eq major-mode 'pdf-view-mode)
    ;; This buffer is in pdf-view-mode
    (let* ((path buffer-file-name)
	   (page (pdf-view-current-page))
	   (link (concat "pdfview:" path "::" (number-to-string page))))
      (org-store-link-props
       :type "pdf-view"
       :link link
       :description path))))

(defun org-pdfview-complete-link ()
  "Use the existing file name completion for file.
Links to get the file name, then ask the user for the page number
and append it."
  (concat (replace-regexp-in-string "^file:" "pdfview:" (org-file-complete-link))
	  "::"
	  (read-from-minibuffer "Page:" "1")))


(provide 'org-pdfview)

;;; org-pdfview.el ends here
