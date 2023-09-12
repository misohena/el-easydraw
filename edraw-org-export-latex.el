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
(require 'edraw-org-edit)

;;;; Customize


;;;; Export

(defun edraw-org-export-latex-link (path _description _back-end info link)
  ;; path is unescaped : \[ \] => [ ]
  ;; description is not unescaped : \[ \] => \[ \]
  (require 'edraw)
  (if-let ((link-props (edraw-org-link-props-parse path nil t)))
      (if-let ((data (edraw-org-link-prop-data link-props)))
          ;; Export data=base64.
          ;; Create temporary file.
          (edraw-org-export-latex-link-file
           (edraw-org-export-latex-create-temp-data-file data)
           info link)

        (if-let ((file (edraw-org-link-prop-file link-props)))
            ;; Export file=.
            (edraw-org-export-latex-link-file file info link)
          ""))
    ""))

(defun edraw-org-export-latex-create-temp-data-file (data)
  (let* ((svg-str (edraw-decode-string data t))
         (hash (sha1 svg-str))
         (file (format "link-data-%s.edraw.svg" hash))) ;;@todo customize
    (with-temp-file file
      (insert svg-str)
      (set-buffer-file-coding-system 'utf-8))
    file))

(defun edraw-org-export-latex-link-file (file info link)
  ;; (concat "\\includesvg" "{" file "}\n")
  (let* ((element (org-element-copy link))
         (element (org-element-put-property element :path file))
         (parent (org-element-property :parent link))
         (element (org-element-put-property element :parent parent)))
    ;; Delegate to ox-latex.
    ;; To support caption, attributes, etc.
    (org-latex--inline-image element info)))

(provide 'edraw-org-export-latex)
;;; edraw-org-export-latex.el ends here
