;;; edraw-generator.el --- Shape generators  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords:

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

;; Define shape generators to be used from `edraw-shape-generator' and
;; `edraw-editor-tool-generator'.
;;
;; This file defines the following generator types.
;; - latex
;; - grid
;;
;; See `edraw-shape-generator-alist' for a list of generators.
;;

;;; Code:

;;;; Declarations

(require 'edraw)
(require 'edraw-dom-svg)

(defvar edraw-import-warning-suppress-types)
(autoload 'edraw-import-warning-suppress-types "edraw-import")
(autoload 'edraw-import-svg-string "edraw-import")

;;;; LaTeX

(defgroup edraw-gen-latex nil
  "Generate shapes by LaTeX."
  :tag "LaTeX Shape Generator"
  :prefix "edraw-gen-latex-"
  :group 'edraw)

;;;;; LaTeX Compilation

(defcustom edraw-gen-latex-command-latex
  "latex -interaction nonstopmode -output-directory ./ %s"
  "Command to convert tex file to dvi file."
  :group 'edraw-gen-latex
  :type 'string)

(defcustom edraw-gen-latex-command-dvisgm
  ;; https://dvisvgm.de/Manpage/
  ;; --no-fonts=1 means to use only `path' elements (no `use' elements).
  ;; --scale=1.6 is not necessary as it will be scaled on the Emacs side.
  "dvisvgm %s --no-fonts=1 --exact-bbox --output=%s"
  "Command to convert dvi file to svg file."
  :group 'edraw-gen-latex
  :type 'string)

(defun edraw-gen-latex-compile (tex-src)
  (let* ((tex-file-abs (expand-file-name
                        (make-temp-file "edraw-gen-latex-" nil ".tex" tex-src)))
         (tex-file (file-name-nondirectory tex-file-abs))
         (tex-dir (file-name-directory tex-file-abs))
         (tex-base (file-name-base tex-file))
         (dvi-file (concat tex-base ".dvi"))
         (svg-file (concat tex-base ".svg"))
         (aux-file (concat tex-base ".aux"))
         (log-file (concat tex-base ".log"))
         (default-directory tex-dir))
    (unwind-protect
        (let ((latex-cmd
               (format edraw-gen-latex-command-latex tex-file))
              (dvisvgm-cmd
               (format edraw-gen-latex-command-dvisgm dvi-file svg-file))
              (outbuf (get-buffer-create "*edraw-gen-latex*")))
          (with-current-buffer outbuf
            (erase-buffer))
          (unless (eq (call-process-shell-command latex-cmd nil outbuf) 0)
            (pop-to-buffer outbuf)
            (error "Error: %s" latex-cmd))
          (unless (eq (call-process-shell-command dvisvgm-cmd nil outbuf) 0)
            (pop-to-buffer outbuf)
            (error "Error: %s" dvisvgm-cmd))
          (with-temp-buffer
            (insert-file-contents svg-file)
            (buffer-string)))
      (delete-file svg-file)
      (delete-file aux-file)
      (delete-file log-file)
      (delete-file dvi-file)
      (delete-file tex-file))))

;; EXAMPLE: (edraw-gen-latex-compile "\\documentclass{article}\n\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\pagestyle{empty}\n\\begin{document}\n$a=-\\sqrt{2}$\n\\end{document}")

;;;;; LaTeX Assembling TeX Source

(defcustom edraw-gen-latex-tex-preamble-first
  '("\\documentclass[fleqn]{article}")
  "The beginning of the TeX source."
  :group 'edraw-gen-latex
  :type '(choice (string)
                 (repeat (string))))

(defcustom edraw-gen-latex-tex-packages
  '("\\usepackage[usenames]{color}"
    "\\usepackage[normalem]{ulem}"
    "\\usepackage{amsmath}"
    "\\usepackage{amssymb}")
  "Package part of TeX source."
  :group 'edraw-gen-latex
  :type '(choice (string)
                 (repeat (string))))

(defcustom edraw-gen-latex-tex-style
  ;; \\setlength{\\parindent}{0cm}
  ;; \\setlength{\\footheight}{0cm}
  '("\\pagestyle{empty}
\\setlength{\\textwidth}{\\paperwidth}
\\setlength{\\oddsidemargin}{0pt}
\\setlength{\\evensidemargin}{0pt}
\\setlength{\\textheight}{\\paperheight}
\\setlength{\\topmargin}{0pt}
\\setlength{\\headheight}{0pt}
\\setlength{\\headsep}{0pt}
\\setlength{\\topskip}{0pt}
\\setlength{\\footskip}{0pt}
\\setlength{\\mathindent}{0pt}")
  "Style declaration part of TeX source."
  :group 'edraw-gen-latex
  :type '(choice (string)
                 (repeat (string))))

(defvar edraw-gen-latex-tex-preamble-options nil)

(defcustom edraw-gen-latex-tex-preamble-last nil
  "The end of the preamble part of the TeX source."
  :group 'edraw-gen-latex
  :type '(choice (string)
                 (repeat (string))))

(defcustom edraw-gen-latex-tex-document-first
  '("\\begin{document}")
  "The beginning of the documentation part of the TeX source."
  :group 'edraw-gen-latex
  :type '(choice (string)
                 (repeat (string))))

(defvar edraw-gen-latex-tex-document-body nil)

(defcustom edraw-gen-latex-tex-document-last
  '("\\end{document}")
  "The end of the documentation part of the TeX source."
  :group 'edraw-gen-latex
  :type '(choice (string)
                 (repeat (string))))

(defconst edraw-gen-latex-tex-format
  '(edraw-gen-latex-tex-preamble-first
    edraw-gen-latex-tex-packages
    edraw-gen-latex-tex-style
    edraw-gen-latex-tex-preamble-options
    edraw-gen-latex-tex-preamble-last
    edraw-gen-latex-tex-document-first
    edraw-gen-latex-tex-document-body
    edraw-gen-latex-tex-document-last))

(defun edraw-gen-latex-format (fmt)
  (cond
   ((stringp fmt)
    fmt)
   ((null fmt)
    nil)
   ((listp fmt)
    (mapconcat #'identity
               (delq nil (mapcar #'edraw-gen-latex-format fmt)) "\n"))
   ((symbolp fmt)
    (when (boundp fmt)
      (edraw-gen-latex-format (symbol-value fmt))))))

(defun edraw-gen-latex-assemble (src-code options)
  (unless src-code
    (error "No latex src-code"))
  (let* (;; Indentation
         (parindent (alist-get 'parindent options))
         (edraw-gen-latex-tex-preamble-options
          (when (numberp parindent)
            (concat "\\setlength{\\parindent}{"
                    (edraw-to-string parindent)
                    ;;@todo pt?
                    "pt}")))
         ;; Document body
         (edraw-gen-latex-tex-document-body src-code))
    (edraw-gen-latex-format edraw-gen-latex-tex-format)))

;; EXAMPLE: (edraw-gen-latex-assemble "$x=\\sqrt{2}$")

;;;;; LaTeX Generator Functions

(defun edraw-gen-latex (src &rest plist)
  (unless (string-empty-p src)
    (let* ((options (plist-get plist :options))
           ;; Suppress warnings
           ;; @todo Problems can occur when ungrouped
           (edraw-import-warning-suppress-types
            (edraw-import-warning-suppress-types 'path-multiple-subpaths))
           (body (edraw-dom-get-by-id
                  (edraw-import-svg-string
                   (edraw-gen-latex-compile
                    (edraw-gen-latex-assemble
                     src options)))
                  "edraw-body"))
           ;; Discard unnecessary group element.
           (result-element (if (cdr (edraw-dom-children body))
                               body
                             (car (edraw-dom-children body)))))
      ;; Remove id attributes
      (edraw-dom-remove-attr-from-tree result-element 'id)

      ;; Scaling
      (let ((scale (alist-get 'scale options)))
        (when (numberp scale)
          (edraw-svg-element-transform-multiply
           result-element
           (edraw-matrix-scale scale scale 1))))

      result-element)))

;; EXAMPLE: (edraw-gen-latex-compile (edraw-gen-latex-assemble "$x=\\sqrt{2}$"))

(defun edraw-gen-latex-options-info ()
  (list
   (edraw-svg-prop-info 'scale nil 'number nil)
   (edraw-svg-prop-info 'parindent nil 'number nil)))

(defun edraw-gen-latex-defaults ()
  `((fill . ,edraw-package-default-stroke)
    (gen-options
     . ,(concat
         ;; The coordinate system output by latex and dvisvgm is in pt units.
         ;; The default font size is 10pt. Multiply this by 1.6 to 16px.
         "scale:1.6"
         ";parindent:0"))))


;;;; Grid

(defconst edraw-gen-grid-max-lines 1000)

(defun edraw-gen-grid-safety ()
  'immediately-applicable)

(defun edraw-gen-grid (_src &rest plist)
  (let* ((options (plist-get plist :options))
         (x-interval (alist-get 'x-interval options))
         (x-min (alist-get 'x-min options))
         (x-max (alist-get 'x-max options))
         (y-interval (alist-get 'y-interval options))
         (y-min (alist-get 'y-min options))
         (y-max (alist-get 'y-max options)))
    (when (and (numberp x-min)
               (numberp x-max)
               (numberp y-min)
               (numberp y-max))
      (let ((children
             (nconc
              (edraw-gen-grid-make-lines x-min x-max x-interval
                                         y-min y-max nil)
              (edraw-gen-grid-make-lines y-min y-max y-interval
                                         x-min x-max t))))
        (when children
          (edraw-svg-group :children children))))))

(defun edraw-gen-grid-make-lines (x-min x-max x-interval y-min y-max transpose)
  (when (and (numberp x-interval)
             (> x-interval 0)
             (> x-max x-min)
             (< (/ (- x-max x-min) x-interval) edraw-gen-grid-max-lines))
    (cl-loop for x from x-min to x-max by x-interval
             collect
             (edraw-svg-path
              (concat
               "M"
               (mapconcat #'edraw-to-string
                          (if transpose
                              (list y-min x y-max x)
                            (list x y-min x y-max))
                          " "))))))

(defun edraw-gen-grid-options-info ()
  (list
   (edraw-svg-prop-info 'x-interval nil 'number nil)
   (edraw-svg-prop-info 'x-min nil 'number nil)
   (edraw-svg-prop-info 'x-max nil 'number nil)
   (edraw-svg-prop-info 'y-interval nil 'number nil)
   (edraw-svg-prop-info 'y-min nil 'number nil)
   (edraw-svg-prop-info 'y-max nil 'number nil)))

(defun edraw-gen-grid-defaults ()
  `((stroke . "#808080")
    (stroke-width . "1")
    (gen-options
     . "x-interval:20;x-min:0;x-max:100;y-interval:20;y-min:0;y-max:100")))

(defun edraw-gen-grid-interactive (editor rect)
  (unless rect
    (setq rect (edraw-editor-read-rectangle-interactively
                (edraw-msg "Drag the grid creation range")
                editor))) ;;@todo This code is not used.

  (let* ((click-p (edraw-rect-empty-p rect)) ;;@todo Check the minimum amount of movement that is considered a click
         (options
          `((x-min . ,(if click-p (read-number (edraw-msg "X Minimum: ") 0) 0))
            (y-min . ,(if click-p (read-number (edraw-msg "Y Minimum: ") 0) 0))
            (x-max . ,(if click-p (read-number (edraw-msg "X Maximum: ") 100)
                        (edraw-rect-width rect)))
            (y-max . ,(if click-p (read-number (edraw-msg "Y Maximum: ") 100)
                        (edraw-rect-height rect)))
            (x-interval . ,(read-number (edraw-msg "X Interval: ") 20))
            (y-interval . ,(read-number (edraw-msg "Y Interval: ") 20)))))
    `((gen-src . nil)
      (gen-options . ,(mapconcat (lambda (cell)
                                   (format "%s:%s"
                                           (car cell)
                                           (edraw-to-string (cdr cell))))
                                 options
                                 ";"))
      (transform . ,(edraw-svg-transform-from-matrix
                     (edraw-matrix-translate-xy (edraw-rect-lt rect)))))))


(provide 'edraw-generator)
;;; edraw-generator.el ends here
