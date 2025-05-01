;;; edraw-org-export-latex.el --- Export edraw link As LaTeX in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Graphics, Drawing, SVG, Editor, Orgmode

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

;; 

;;; Code:

(require 'ox-latex)
(require 'edraw-org)

;;;; Export

;; First we setup varaibles for export command and options,
;; then we try to automatically set them if possible.

(defvar edraw-org-svg-to-pdf-command nil
  "Executable to convert svg to pdf for latex export (e.g., inkscape).")
(defvar edraw-org-svg-to-pdf-command-options nil
  "Options for edraw-org-svg-to-pdf-command for latex export (e.g., -A).")

(if (and (not edraw-org-svg-to-pdf-command) (executable-find "inkscape"))
    (progn (message "Using inkscape for edraw svg to pdf on latex export")
	   (setq edraw-org-svg-to-pdf-command "inkscape")
	   (setq edraw-org-svg-to-pdf-command-options "-A")))

(if (and (not edraw-org-svg-to-pdf-command) (executable-find "rsvg-convert"))
    (progn (message "Using rsvg-convert for edraw svg to pdf on latex export")
	   (setq edraw-org-svg-to-pdf-command "rsvg-convert")
	   (setq edraw-org-svg-to-pdf-command-options "-f pdf -o ")))

(if (not edraw-org-svg-to-pdf-command)
    (display-warning :warning
		     "edraw-org-svg-to-pdf-command not set for latex export"))


;; The following will get called on latex export and will
;; try converting svg files to pdf and include them in latex.
(defun edraw-org-export-latex-link (path description backend info link)
  "Export edraw links for LaTeX backend.
PATH is the link path, DESCRIPTION is the link description,
BACKEND is the export backend, INFO is the export info, and LINK is the link object."
  (when (eq backend 'latex)
    (if (string-match "file=\\(.*\\.svg\\)" path)
	(let* ((svg-file (match-string 1 path))
               (pdf-file (concat (file-name-sans-extension svg-file) ".pdf"))
               ;; Keep the full path for the LaTeX \includegraphics command
               (full-pdf-path pdf-file))
          
          ;; Convert SVG to PDF if needed
          (when (and (file-exists-p svg-file)
                     (or (not (file-exists-p pdf-file))
			 (file-newer-than-file-p svg-file pdf-file)))
            (message "Converting %s to %s for LaTeX export" svg-file pdf-file)
            (call-process edraw-org-svg-to-pdf-command nil nil nil
			  svg-file
			  edraw-org-svg-to-pdf-command-options
			  pdf-file))
          
          ;; Return the LaTeX \includegraphics command with the full path
          (format "\\includegraphics{%s}" full-pdf-path))
      ;; If not a file-type link then cannot convert it
      (concat "\\begin{verbatim}\n"
	      "cannot export edraw link " path
	      "\n\\end{verbatim}")
      )
    )
  )

(provide 'edraw-org-export-latex)
;;; edraw-org-export-latex.el ends here
