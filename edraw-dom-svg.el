;;; edraw-dom-svg.el --- DOM/SVG Utility             -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Graphics,Drawing,SVG

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
(require 'svg)
(require 'seq)
(require 'subr-x)
(require 'edraw-math)
(require 'edraw-path)
(require 'edraw-util)

;;;; DOM Utility

(defun edraw-dom-element-p (node)
  (and node
       (listp node)
       (not (null (car node)))
       (symbolp (car node))))

(defun edraw-dom-get-by-id (parent id)
  (car (dom-by-id parent (concat "\\`" (regexp-quote id) "\\'"))))

(defun edraw-dom-get-or-create (parent tag id)
  (or
   (edraw-dom-get-by-id parent id)
   (svg-node parent tag :id id)))

(defun edraw-dom-remove-all-children (node)
  (when node
    ;; depends on dom.el node structure
    (setcdr (cdr node) nil))
  node)

(defun edraw-dom-remove-by-id (dom id)
  (when-let ((node (edraw-dom-get-by-id dom id)))
    (dom-remove-node dom node)))

(defun edraw-dom-remove-attr (node attr)
  (dom-set-attributes node (assq-delete-all attr (dom-attributes node))))

(defun edraw-dom-first-node-p (dom node)
  (if-let ((parent (dom-parent dom node)))
      (eq (car (dom-children parent)) node)
    t))

(defun edraw-dom-last-node-p (dom node)
  (if-let ((parent (dom-parent dom node)))
      (eq (car (last (dom-children parent))) node)
    t))

(defun edraw-dom-reorder-prev (dom node)
  (when-let ((parent (dom-parent dom node)))
    (let ((index (seq-position (dom-children parent) node #'eq)))
      (when (> index 0)
        (let* ((prev-cell (nthcdr (1- index) (dom-children parent)))
               (prev-node (car prev-cell)))
          ;; depends on dom.el node structure
          (setcar prev-cell node)
          (setcar (cdr prev-cell) prev-node))
        t))))

(defun edraw-dom-reorder-next (dom node)
  (when-let ((parent (dom-parent dom node)))
    (let* ((index (seq-position (dom-children parent) node #'eq))
           (curr-cell (nthcdr index (dom-children parent)))
           (next-cell (cdr curr-cell))
           (next-node (car next-cell)))
      (when next-cell
        ;; depends on dom.el node structure
        (setcar next-cell node)
        (setcar curr-cell next-node)
        t))))

(defun edraw-dom-reorder-first (dom node)
  (when-let ((parent (dom-parent dom node)))
    (when (not (eq (car (dom-children parent)) node))
      (dom-remove-node parent node)
      (dom-add-child-before parent node (car (dom-children parent)))
      t)))

(defun edraw-dom-reorder-last (dom node)
  (when-let ((parent (dom-parent dom node)))
    (when (not (eq (car (last (dom-children parent))) node))
      (dom-remove-node parent node)
      (dom-append-child parent node)
      t)))

(defun edraw-dom-insert-first (node child)
  (dom-add-child-before node child))

(defun edraw-dom-first-child (node)
  (car (dom-children node)))

(defun edraw-dom-last-child (node)
  (car (last (dom-children node))))

(defun edraw-dom-next-sibling (dom node)
  (when-let ((parent (dom-parent dom node)))
    (let ((siblings (dom-children parent)))
      (while (and siblings
                  (not (eq (car siblings) node)))
        (setq siblings (cdr siblings)))
      (cadr siblings))))

(defun edraw-dom-previous-sibling (dom node)
  (when-let ((parent (dom-parent dom node)))
    (let ((siblings (dom-children parent)))
      (if (eq (car siblings) node)
          nil
        (while (and (cadr siblings)
                    (not (eq (cadr siblings) node)))
          (setq siblings (cdr siblings)))
        (car siblings)))))

(defun edraw-dom-insert-nth (node child index)
  (setq node (dom-ensure-node node))
  ;; depends on dom.el node structure
  (if (<= index 0)
      (setcdr (cdr node) (cons child (cddr node)))
    (let ((cell (or (nthcdr (1- index) (cddr node))
                    (last (cddr node)))))
      (setcdr cell (cons child (cdr cell)))))
  child)


;;;; SVG Print


(defun edraw-svg-to-image (svg &rest props)
  (apply
   #'create-image
   (edraw-svg-to-string svg nil #'edraw-svg-print-attr-filter)
   'svg t
   props))

(defun edraw-svg-to-string (dom node-filter attr-filter &optional indent no-indent)
  (with-temp-buffer
    (edraw-svg-print dom node-filter attr-filter indent no-indent)
    (buffer-string)))

(defun edraw-svg-print (dom node-filter attr-filter &optional indent no-indent)
  ;; Derived from svg-print in svg.el
  (when (or (null node-filter) (funcall node-filter dom))
    (if (stringp dom)
        (insert (edraw-svg-escape-chars dom))
      (let ((tag (car dom))
            (attrs (cadr dom))
            (children (cddr dom)))
        (when (and (integerp indent) (not no-indent)) (insert (make-string indent ? )))
        (insert (format "<%s" tag))
        (dolist (attr attrs)
          (when (or (null attr-filter) (funcall attr-filter attr))
            (insert (format " %s=\"%s\""
                            (car attr)
                            ;;@todo add true attribute filter and add number format option on export
                            (edraw-svg-escape-chars
                             (edraw-svg-ensure-string-attr (cdr attr)))))))
        (if (null children)
            ;;children is empty
            (insert " />")
          ;; output children
          (insert ">")
          (let ((no-indent (or (not (integerp indent))
                               no-indent
                               (memq tag '(text tspan))
                               (seq-find 'stringp children))))
            (dolist (elem children)
              (unless no-indent (insert "\n"))
              (edraw-svg-print elem node-filter attr-filter (unless no-indent (+ indent 2)) no-indent))
            (unless no-indent (insert "\n" (make-string indent ? )))
            (insert (format "</%s>" (car dom)))))))))

(defun edraw-svg-print-attr-filter (attr)
  (/= (aref (edraw-svg-symbol-name (car attr)) 0) ?:))

(defun edraw-svg-symbol-name (symbol-or-str)
  (format "%s" symbol-or-str))

(defun edraw-svg-escape-chars (str)
  (replace-regexp-in-string
   "\\([\"&<]\\)"
   (lambda (str)
     (pcase (elt str 0)
       (?\" "&quot;")
       (?& "&amp;")
       (?< "&lt;")))
   str
   t t))


;;;; SVG Encode / Decode


(defun edraw-svg-decode (data base64-p)
  (with-temp-buffer
    (insert data)
    (edraw-decode-buffer base64-p)
    (libxml-parse-xml-region (point-min) (point-max))))

(defun edraw-svg-encode (svg base64-p gzip-p)
  (with-temp-buffer
    (edraw-svg-print
     svg
     nil
     'edraw-svg-print-attr-filter)
    (edraw-encode-buffer base64-p gzip-p)
    (buffer-string)))


;;;; SVG File I/O


(defun edraw-svg-make-file-writer (path gzip-p)
  (lambda (svg)
    (edraw-svg-write-to-file svg path gzip-p)))

(defun edraw-svg-write-to-file (svg path gzip-p)
  (with-temp-file path
    (insert (edraw-svg-encode svg nil gzip-p))
    (set-buffer-file-coding-system 'utf-8)))

(defun edraw-svg-read-from-file (path)
  (edraw-svg-decode
   (with-temp-buffer
     (insert-file-contents path)
     (buffer-substring-no-properties (point-min) (point-max)))
   nil))


;;;; SVG Attributes

;;;;; Conversion

(defun edraw-svg-attr-length-to-number (value)
  "Convert length attribute value to number."
  ;; <length> ::=  number ("em"|"ex"|"px"|"in"|"cm"|"mm"|"pt"|"pc"|"%")?
  (cond
   ((null value)
    value)
   ((stringp value)
    ;;@todo support unit? (px,em,ex,in,cm,mm,pt,pc,%) error?
    (string-to-number value)) ;;@todo invalid format
   ((numberp value)
    value)
   ;; symbol?
   (t
    value)))

(defun edraw-svg-attr-number-to-number (value)
  "Convert number attribute value to number."
  (cond
   ((null value)
    value)
   ((stringp value)
    (string-to-number value)) ;;@todo invalid format
   ((numberp value)
    value)
   ;; symbol?
   (t
    value)))

(defun edraw-svg-ensure-string-attr (value)
  "Convert attribute value to string."
  (cond
   ((null value) "")
   ((numberp value) (edraw-to-string value))
   (t (format "%s" value))))

;;;;; Get Attribute

(defun edraw-svg-attr-number (element attr)
  "Return the number attribute ATTR from ELEMENT."
  (edraw-svg-attr-number-to-number (dom-attr element attr)))

(defun edraw-svg-attr-coord (element attr)
  "Return the coordinate attribute ATTR from ELEMENT."
  ;; <coordinate> ::= <length>
  (edraw-svg-attr-length element attr))

(defun edraw-svg-attr-length (element attr)
  "Return the length attribute ATTR from ELEMENT."
  (edraw-svg-attr-length-to-number (dom-attr element attr)))

;;;;; Set Attribute

(defun edraw-svg-set-attr-string (element attribute value)
  "Set ATTRIBUTE in ELEMENT to string VALUE.
VALUE is converted to a string for sure."
  (dom-set-attribute element attribute (edraw-svg-ensure-string-attr value)))

(defun edraw-svg-set-attr-number (element attribute value)
  "Set ATTRIBUTE in ELEMENT to number VALUE.
To avoid numerical errors, VALUE is not converted to
anything. Numeric values are set as numeric values and strings
are set as strings."
  (dom-set-attribute element attribute value))


;;;; SVG Transform Attribute

(defconst edraw-svg-transform-number
  "\\(?:[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\)")
(defconst edraw-svg-transform-unit "\\(?:[a-z]+\\|%\\)")
(defconst edraw-svg-transform-number-unit
  (concat edraw-svg-transform-number edraw-svg-transform-unit "?"))
(defconst edraw-svg-transform-wsp "\\(?:[ \t\n\f\r]+\\)")
(defconst edraw-svg-transform-wsp-opt "[ \t\n\f\r]*")
(defconst edraw-svg-transform-comma-wsp "\\(?:[ \t\n\f\r]+,?[ \t\n\f\r]*\\|,[ \t\n\f\r]*\\)")
(defconst edraw-svg-transform-function
  (concat
   edraw-svg-transform-wsp-opt
   ;; (1) function name
   "\\([A-Za-z0-9_]+\\)"
   edraw-svg-transform-wsp-opt
   "("
   edraw-svg-transform-wsp-opt
   ;;(2) command arguments
   "\\(" edraw-svg-transform-number-unit
   "\\(?:" edraw-svg-transform-comma-wsp edraw-svg-transform-number-unit "\\)*\\)?"
   edraw-svg-transform-wsp-opt ")" edraw-svg-transform-wsp-opt))

(defun edraw-svg-transform-parse-numbers (numbers-str)
  (when numbers-str
    (mapcar
     (lambda (ns)
       (when (string-match
              (concat "\\(" edraw-svg-transform-number "\\)"
                      "\\(" edraw-svg-transform-unit "\\)?")
              ns)
         (let ((num (string-to-number (match-string 1 ns)))
               (unit (match-string 2 ns)))
           (pcase unit
             ((or 'nil "") num)
             ;; angle to degrees
             ("deg" num)
             ("rad" (radians-to-degrees num))
             ("grad" (/ (* num 180) 200.0))
             ("turn" (* 360 num))
             ;; length to px
             ;;@todo relative
             ("cm" (/ (* num 96) 2.54))
             ("mm" (/ (* num 96) 25.4))
             ("Q" (/ (* num 96) 2.54 40))
             ("in" (* num 96))
             ("pc" (/ (* num 96) 6))
             ("pt" (/ (* num 96) 72))
             ("px" num)
             (_ (cons num unit))))))
     (split-string numbers-str
                   edraw-svg-transform-comma-wsp))))

(defun edraw-svg-transform-parse (str)
  (let ((pos 0)
        functions)
    (while (and (string-match edraw-svg-transform-function str pos)
                (= (match-beginning 0) pos))
      (setq pos (match-end 0))
      (let* ((fname (match-string 1 str))
             (numbers-str (match-string 2 str))
             (numbers (edraw-svg-transform-parse-numbers numbers-str)))
        (push (cons fname numbers) functions)))
    (when (/= pos (length str))
      (error "transform value parsing error at %s" (substring str pos)))
    (nreverse functions)))

;;TEST: (edraw-svg-transform-parse "") => nil
;;TEST: (edraw-svg-transform-parse "translate(10 20)") => (("translate" 10 20))
;;TEST: (edraw-svg-transform-parse "rotate(180deg)") => (("rotate" 180))
;;TEST: (edraw-svg-transform-parse "scale(2) rotate(0.125turn)") => (("scale" 2) ("rotate" 45.0))

(defun edraw-svg-transform-apply (fname-args)
  (let ((fname (car fname-args))
        (args (cdr fname-args)))
    (apply (intern (concat "edraw-svg-transform--" fname)) args)))

(defun edraw-svg-transform--matrix (a b c d e f)
  (edraw-matrix (vector a b c d e f)))

(defun edraw-svg-transform--translate (tx &optional ty)
  (edraw-matrix-translate tx (or ty 0) 0))

(defun edraw-svg-transform--translateX (tx)
  (edraw-matrix-translate tx 0 0))

(defun edraw-svg-transform--translateY (ty)
  (edraw-matrix-translate 0 ty 0))

(defun edraw-svg-transform--scale (sx &optional sy)
  (edraw-matrix-scale sx (or sy sx) 1))

(defun edraw-svg-transform--scaleX (sx)
  (edraw-matrix-scale sx 1 1))

(defun edraw-svg-transform--scaleY (sy)
  (edraw-matrix-scale 1 sy 1))

(defun edraw-svg-transform--rotate (angle-deg &optional cx cy)
  (if (or cx cy)
      (edraw-matrix-mul-mat-mat
       (edraw-matrix-translate (or cx 0) (or cy 0) 0)
       (edraw-matrix-mul-mat-mat
        (edraw-matrix-rotate angle-deg)
        (edraw-matrix-translate (- (or cx 0)) (- (or cy 0)) 0)))
    (edraw-matrix-rotate angle-deg)))

(defun edraw-svg-transform--skew (ax-deg &optional ay-deg)
  (edraw-matrix-skew ax-deg (or ay-deg 0)))

(defun edraw-svg-transform--skewX (ax-deg)
  (edraw-matrix-skew ax-deg 0))

(defun edraw-svg-transform--skewY (ay-deg)
  (edraw-matrix-skew 0 ay-deg))

(defun edraw-svg-transform-to-matrix (str)
  (seq-reduce #'edraw-matrix-mul-mat-mat
              (mapcar #'edraw-svg-transform-apply
                      (edraw-svg-transform-parse str))
              (edraw-matrix)))

;;TEST: (edraw-svg-transform-to-matrix "translate(10 20)") => [1 0 0 0 0 1 0 0 0 0 1 0 10 20 0 1]
;;TEST: (edraw-svg-transform-to-matrix "scale(2) translate(10 20)") => [2 0 0 0 0 2 0 0 0 0 1 0 20 40 0 1]
;;TEST: (edraw-svg-transform-to-matrix "rotate(45deg)") => [0.7071067811865476 0.7071067811865475 0.0 0.0 -0.7071067811865475 0.7071067811865476 0.0 0.0 0 0 1 0 0 0 0 1]
;;TEST: (edraw-svg-transform-to-matrix "rotate(45deg 10 10)") => [0.7071067811865476 0.7071067811865475 0.0 0.0 -0.7071067811865475 0.7071067811865476 0.0 0.0 0.0 0.0 1.0 0.0 10.0 -4.142135623730951 0.0 1.0]

(defun edraw-svg-transform-from-matrix (mat)
  (when mat
    (format "matrix(%s,%s,%s,%s,%s,%s)"
            (edraw-to-string (edraw-matrix-at mat 0))
            (edraw-to-string (edraw-matrix-at mat 1))
            (edraw-to-string (edraw-matrix-at mat 4))
            (edraw-to-string (edraw-matrix-at mat 5))
            (edraw-to-string (edraw-matrix-at mat 12))
            (edraw-to-string (edraw-matrix-at mat 13)))))

(defun edraw-svg-element-transform-get (element &optional matrix)
  (edraw-matrix-mul-mat-mat
   ;;nil means identity matrix
   matrix
   (when-let ((transform-str (dom-attr element 'transform)))
     (ignore-errors
       (edraw-svg-transform-to-matrix transform-str)))))

(defun edraw-svg-element-transform-set (element mat)
  (if (edraw-matrix-identity-p mat)
      (edraw-dom-remove-attr element 'transform)
    (edraw-svg-set-attr-string element 'transform (edraw-svg-transform-from-matrix mat))))

(defun edraw-svg-element-transform-multiply (element mat)
  (unless (edraw-matrix-identity-p mat)
    (edraw-svg-element-transform-set
     element
     (edraw-svg-element-transform-get element mat))))

(defun edraw-svg-element-transform-translate (element xy)
  (when (and xy (not (edraw-xy-zero-p xy)))
    (let ((transform (or
                      (edraw-svg-element-transform-get element)
                      (edraw-matrix))))
      (edraw-matrix-translate-add transform (car xy) (cdr xy))
      (edraw-svg-element-transform-set element transform))))



;;;; SVG Shape Creation


;; (defun edraw-svg-rect (parent xy0 xy1 &rest args)
;;   (let ((element (dom-node 'rect
;;                            `((wwidth . ,(abs (- (car xy0) (car xy1))))
;;                              (height . ,(abs (- (cdr xy0) (cdr xy1))))
;;                              (x . ,(min (car xy0) (car xy1)))
;;                              (y . ,(min (cdr xy0) (cdr xy1)))
;;                              ,@(svg--arguments parent args)))))
;;     (svg--append parent element) ;;Avoid duplicate ids
;;     element)) ;;Return element

;; (defun edraw-svg-ellipse (parent xy0 xy1 &rest args)
;;   (let ((element (dom-node 'ellipse
;;                            `((cx . ,(* 0.5 (+ (car xy0) (car xy1))))
;;                              (cy . ,(* 0.5 (+ (cdr xy0) (cdr xy1))))
;;                              (rx . ,(* 0.5 (abs (- (car xy0) (car xy1)))))
;;                              (ry . ,(* 0.5 (abs (- (cdr xy0) (cdr xy1)))))
;;                              ,@(svg--arguments parent args)))))
;;     (svg--append parent element) ;;Avoid duplicate ids
;;     element)) ;;Return element

;; (defun edraw-svg-path (parent d &rest args)
;;   (let ((element (dom-node 'path
;;                            `((d . ,d)
;;                              ,@(svg--arguments parent args)))))
;;     (svg--append parent element) ;;Avoid duplicate ids
;;     element)) ;;Return element

;; (defun edraw-svg-text (parent text xy &rest args)
;;   (let ((element (dom-node 'text
;;                            `((x . ,(car xy))
;;                              (y . ,(cdr xy))
;;                              ,@(svg--arguments parent args))
;;                            text)))
;;     (svg--append parent element) ;;Avoid duplicate ids
;;     element)) ;;Return element


;;;; SVG Shape Rectangular Range Setting


(defun edraw-svg-rect-set-range (element xy0 xy1)
  (edraw-svg-set-attr-number element 'x (min (car xy0) (car xy1)))
  (edraw-svg-set-attr-number element 'y (min (cdr xy0) (cdr xy1)))
  (edraw-svg-set-attr-number element 'width (abs (- (car xy0) (car xy1))))
  (edraw-svg-set-attr-number element 'height (abs (- (cdr xy0) (cdr xy1)))))

(defun edraw-svg-ellipse-set-range (element xy0 xy1)
  (edraw-svg-set-attr-number element 'cx (* 0.5 (+ (car xy0) (car xy1))))
  (edraw-svg-set-attr-number element 'cy (* 0.5 (+ (cdr xy0) (cdr xy1))))
  (edraw-svg-set-attr-number element 'rx (* 0.5 (abs (- (car xy0) (car xy1)))))
  (edraw-svg-set-attr-number element 'ry (* 0.5 (abs (- (cdr xy0) (cdr xy1))))))

(defun edraw-svg-image-set-range (element xy0 xy1)
  (edraw-svg-set-attr-number element 'x (min (car xy0) (car xy1)))
  (edraw-svg-set-attr-number element 'y (min (cdr xy0) (cdr xy1)))
  (edraw-svg-set-attr-number element 'width (abs (- (car xy0) (car xy1))))
  (edraw-svg-set-attr-number element 'height (abs (- (cdr xy0) (cdr xy1)))))


;;;; SVG Shape Summary


(defun edraw-svg-element-summary (element)
  (pcase (dom-tag element)
    ('path (edraw-svg-path-summary element))
    ('rect (edraw-svg-rect-summary element))
    ('ellipse (edraw-svg-ellipse-summary element))
    ('circle (edraw-svg-circle-summary element))
    ('text (edraw-svg-text-summary element))
    ('image (edraw-svg-image-summary element))
    ('g (edraw-svg-group-summary element))))

(defun edraw-svg-path-summary (element)
  (format "path (%s)"
          (truncate-string-to-width
           (or (dom-attr element 'd) "") 20 nil nil "...")))

(defun edraw-svg-rect-summary (element)
  (format "rect (%s,%s,%s,%s)"
          (dom-attr element 'x)
          (dom-attr element 'y)
          (dom-attr element 'width)
          (dom-attr element 'height)))

(defun edraw-svg-ellipse-summary (element)
  (format "ellipse (%s,%s,%s,%s)"
          (dom-attr element 'cx)
          (dom-attr element 'cy)
          (dom-attr element 'rx)
          (dom-attr element 'ry)))

(defun edraw-svg-circle-summary (element)
  (format "circle (%s,%s,%s)"
          (dom-attr element 'cx)
          (dom-attr element 'cy)
          (dom-attr element 'r)))

(defun edraw-svg-text-summary (element)
  (format "text (%s)"
          (truncate-string-to-width (dom-text element) 20 nil nil "...")))

(defun edraw-svg-image-summary (element)
  (format "image (%s,%s,%s,%s,%s)"
          (dom-attr element 'x)
          (dom-attr element 'y)
          (dom-attr element 'width)
          (dom-attr element 'height)
          (truncate-string-to-width
           (or (dom-attr element 'href) "")
           20 nil nil "...")))

(defun edraw-svg-group-summary (element)
  (format "group (%s children)" ;;@todo edraw-msg (require 'edraw-util)
          (length (dom-children element))))


;;;; SVG Shape Properties

(defconst edraw-svg-elem-prop-number-types
  '(number opacity length coordinate))

(defconst edraw-svg-element-properties-common
  ;;name source type required
  '((opacity attr opacity nil)
    (fill attr-fill-stroke paint nil)
    (fill-opacity attr opacity nil)
    (stroke attr-fill-stroke paint nil)
    (stroke-opacity attr opacity nil)
    (stroke-width attr length nil)
    (stroke-dasharray attr string nil)
    (stroke-dashoffset attr length nil)
    (style attr string nil)
    (transform attr string nil)))
(defconst edraw-svg-element-properties-path-common
  '((fill-rule attr (or "nonzero" "evenodd") nil)
    (stroke-linecap attr (or "butt" "round" "square") nil)
    (stroke-linejoin attr (or "miter" "round" "bevel") nil)
    (stroke-miterlimit attr number nil)))
(defconst edraw-svg-element-properties
  `((rect
     (x attr coordinate t)
     (y attr coordinate t)
     (width attr length t)
     (height attr length t)
     (rx attr length nil)
     (ry attr length nil)
     ,@edraw-svg-element-properties-common)
    (circle
     (cx attr coordinate t)
     (cy attr coordinate t)
     (r attr length t)
     ,@edraw-svg-element-properties-common)
    (ellipse
     (cx attr coordinate t)
     (cy attr coordinate t)
     (rx attr length t)
     (ry attr length t)
     ,@edraw-svg-element-properties-common)
    (path
     (d attr string t :internal t)
     ,@edraw-svg-element-properties-common
     ,@edraw-svg-element-properties-path-common
     (marker-start attr-marker (or "arrow" "circle") nil)
     (marker-mid attr-marker (or "arrow" "circle") nil)
     (marker-end attr-marker (or "arrow" "circle") nil))
    (text
     (text inner-text string t)
     ;; librsvg does not support list-of-coordinates
     ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/183
     (x attr coordinate t)
     (y attr coordinate t)
     (dx attr coordinate nil)
     (dy attr coordinate nil)
     ;; librsvg does not support?
     ;;(rotate attr string nil)
     ;; librsvg does not support textLength
     ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/88
     ;;(textLength attr number nil)
     ;;(lengthAdjust attr (or "spacing" "spacingAndGlyphs") nil)
     (font-family attr font-family nil)
     (font-size attr number nil)
     (font-weight attr (or "normal" "bold" "bolder" "lighter") nil)
     (font-style attr (or "normal" "italic" "oblique") nil)
     (text-decoration attr (or "underline" "overline" "line-through") nil)
     (text-anchor attr (or "start" "middle" "end") nil)
     ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/129
     ;;(baseline-shift attr number nil)
     ,@edraw-svg-element-properties-common)
    (image
     (x attr coordinate t)
     (y attr coordinate t)
     (width attr length t)
     (height attr length t)
     (href attr string t)
     (preserveAspectRatio attr string nil)
     (opacity attr opacity nil)
     (style attr string nil)
     (transform attr string nil))
    (g
     ,@edraw-svg-element-properties-common
     ,@edraw-svg-element-properties-path-common)))
(defun edraw-svg-elem-prop-name (prop-def) (nth 0 prop-def))
(defun edraw-svg-elem-prop-source (prop-def) (nth 1 prop-def))
(defun edraw-svg-elem-prop-type (prop-def) (nth 2 prop-def))
(defun edraw-svg-elem-prop-required (prop-def) (nth 3 prop-def))
(defun edraw-svg-elem-prop-attrs (prop-def) (nthcdr 4 prop-def))

(defun edraw-svg-elem-prop-number-p (prop-def)
  (memq (edraw-svg-elem-prop-type prop-def) edraw-svg-elem-prop-number-types))

(defun edraw-svg-element-get-property-info-list (element)
  (edraw-svg-element-get-property-info-list-by-tag (dom-tag element)))

(defun edraw-svg-element-get-property-info-list-by-tag (tag)
  (when-let ((prop-def-list (alist-get tag edraw-svg-element-properties)))
    (cl-loop for prop-def in prop-def-list
             for prop-type = (edraw-svg-elem-prop-type prop-def)
             collect
             (append
              (list :name (edraw-svg-elem-prop-name prop-def)
                    :type prop-type
                    :required (edraw-svg-elem-prop-required prop-def)
                    :to-string #'edraw-svg-ensure-string-attr
                    :from-string #'identity
                    :number-p (edraw-svg-elem-prop-number-p prop-def)
                    :to-number (pcase prop-type
                                 ('coordinate #'edraw-svg-attr-length-to-number)
                                 ('length #'edraw-svg-attr-length-to-number)
                                 ('number #'edraw-svg-attr-number-to-number)
                                 ('opacity #'edraw-svg-attr-number-to-number)
                                 (_ nil))
                    )
              (edraw-svg-elem-prop-attrs prop-def)))))
;; TEST: (edraw-svg-element-get-property-info-list-by-tag 'rect)

(defun edraw-svg-element-can-have-property-p (element prop-name)
  (edraw-svg-tag-can-have-property-p (dom-tag element) prop-name))

(defun edraw-svg-tag-can-have-property-p (tag prop-name)
  (when-let ((prop-def-list (alist-get tag edraw-svg-element-properties)))
    (seq-some (lambda (prop-def) (eq (edraw-svg-elem-prop-name prop-def)
                                     prop-name))
              prop-def-list)))

(defun edraw-svg-element-get-property (element prop-name defrefs)
  (when-let ((prop-def-list (alist-get (dom-tag element) edraw-svg-element-properties))
             (prop-def (assq prop-name prop-def-list)))
    (let* ((source (edraw-svg-elem-prop-source prop-def))
           (getter (intern
                    (concat "edraw-svg-element-get-" (symbol-name source)))))
      (funcall getter element prop-name defrefs))))

(defun edraw-svg-element-set-property (element prop-name value defrefs)
  (when-let ((prop-def-list (alist-get (dom-tag element) edraw-svg-element-properties))
             (prop-def (assq prop-name prop-def-list)))
    (let* ((source (edraw-svg-elem-prop-source prop-def))
           (setter (intern
                    (concat "edraw-svg-element-set-" (symbol-name source)))))
      (funcall setter element prop-name value defrefs))))

(defun edraw-svg-element-has-property-p (element prop-name defrefs)
  (not (null (edraw-svg-element-get-property element prop-name defrefs))))

;; Property Source

(defun edraw-svg-element-get-attr (element prop-name _defrefs)
  ;; nil means no property.
  ;; Return nil, string, or other stored types like a number.
  (dom-attr element prop-name))

(defun edraw-svg-element-set-attr (element prop-name value _defrefs)
  (cond
   ;; nil means no property.
   ((null value)
    (edraw-dom-remove-attr element prop-name))
   ;; x of text must by changed along with inner tspans.
   ((and (eq (dom-tag element) 'text)
         (eq prop-name 'x))
    (edraw-svg-text-set-x element value))
   ;; Store as is. Avoid numerical errors.
   ((numberp value)
    (edraw-svg-set-attr-number element prop-name value))
   ((stringp value)
    (edraw-svg-set-attr-string element prop-name value))
   (t
    (dom-set-attribute element prop-name value))))

(defun edraw-svg-element-get-inner-text (element _prop-name _defrefs)
  ;;(dom-text element)
  (edraw-svg-text-get-text element))

(defun edraw-svg-element-set-inner-text (element _prop-name value _defrefs)
  ;; (when (stringp value)
  ;;   (edraw-dom-remove-all-children element)
  ;;   (dom-append-child element value)))
  (edraw-svg-text-set-text element value))

(defun edraw-svg-element-get-attr-marker (element prop-name defrefs)
  (edraw-svg-get-marker-property element prop-name defrefs))

(defun edraw-svg-element-set-attr-marker (element prop-name value defrefs)
  (edraw-svg-set-marker-property element prop-name value defrefs))

(defun edraw-svg-element-get-attr-fill-stroke (element prop-name defrefs)
  (edraw-svg-element-get-attr element prop-name defrefs))

(defun edraw-svg-element-set-attr-fill-stroke (element prop-name value defrefs)
  (edraw-svg-element-set-attr element prop-name value defrefs)
  (edraw-svg-update-marker-properties element defrefs))


;;;; SVG Text Layout


(defun edraw-svg-text-set-text (element text)
  (edraw-dom-remove-all-children element)

  (when (stringp text)
    (let ((lines (split-string text "\n")))
      (if (null (cdr lines))
          ;; single line
          (dom-append-child element (car lines))
        ;; multi-line
        (let ((x (or (edraw-svg-attr-coord element 'x) 0))
              (first-line-p t))
          (dolist (line lines)
            (dom-append-child
             element
             (dom-node 'tspan
                       (append (list (cons 'class "text-line")
                                     (cons 'x x))
                               (when (not first-line-p)
                                 (list (cons 'dy "1em"))))
                       line))
            (setq first-line-p nil)))))))

(defun edraw-svg-text-get-text (element)
  (if (stringp (car (dom-children element)))
      (car (dom-children element))
    (let ((tspans (dom-by-class element "\\`text-line\\'")))
      (mapconcat (lambda (tspan) (dom-text tspan)) tspans "\n"))))

(defun edraw-svg-text-set-x (element x)
  (edraw-svg-set-attr-number element 'x x)
  (let ((tspans (dom-by-class element "\\`text-line\\'")))
    (dolist (tspan tspans)
      (edraw-svg-set-attr-number tspan 'x x))))

(defun edraw-svg-text-set-xy (element xy)
  (edraw-svg-text-set-x element (car xy))
  (edraw-svg-set-attr-number element 'y (cdr xy)))


;;;; SVG Defs


(defun edraw-svg-defs-as-defrefs (id)
  (edraw-svg-defrefs
   (dom-node 'defs
             (list (cons 'id id)))))


;;;;; Definition and Referrers Pair


(defun edraw-svg-defref (def idnum)
  "Create a definition-referrers pair."
  (list def idnum))
(defun edraw-svg-defref-def (defref) (car defref))
(defun edraw-svg-defref-idnum (defref) (cadr defref))
(defun edraw-svg-defref-elements (defref) (cddr defref))
(defun edraw-svg-defref-elements-head (defref) (cdr defref))
(defun edraw-svg-defref-add-element (defref element)
  (setcdr (edraw-svg-defref-elements-head defref)
          (cons element (edraw-svg-defref-elements defref))))
(defun edraw-svg-defref-remove-element (defref element)
  ;; remove first one
  (let ((cell (edraw-svg-defref-elements-head defref)))
    (while (and (cdr cell)
                (not (eq (cadr cell) element)))
      (setq cell (cdr cell)))
    (when (cdr cell)
      (setcdr cell (cddr cell)))))
(defun edraw-svg-defref-empty-p (defref)
  (null (edraw-svg-defref-elements defref)))

(defun edraw-svg-def-equal-p (a b)
  ;; equal except id
  (and
   (eq (dom-tag a) (dom-tag b))
   (seq-set-equal-p
    (seq-remove (lambda (atr) (eq (car atr) 'id)) (dom-attributes a))
    (seq-remove (lambda (atr) (eq (car atr) 'id)) (dom-attributes b)))
   (equal (dom-children a) (dom-children b))))


;;;;; Definition and Referrers Table


(defun edraw-svg-defrefs (defs-element)
  "Create a definition-referrers table."
  (list defs-element))
(defun edraw-svg-defrefs-defs (defrefs) (car defrefs))
(defun edraw-svg-defrefs-defrefs (defrefs) (cdr defrefs))
(defun edraw-svg-defrefs-head (defrefs) defrefs)
(defun edraw-svg-defrefs-insert-to-empty-idnum (defrefs def)
  (let ((idnum 0)
        (cell (edraw-svg-defrefs-head defrefs)))
    (while (and (cdr cell)
                (= idnum (edraw-svg-defref-idnum (cadr cell))))
      (setq cell (cdr cell))
      (setq idnum (1+ idnum)))
    (let ((defref (edraw-svg-defref def idnum)))
      (setcdr cell (cons defref (cdr cell)))
      defref)))
(defun edraw-svg-defrefs-add-ref (defrefs def element prop-value)
  (if-let ((defref (assoc def (edraw-svg-defrefs-defrefs defrefs) 'edraw-svg-def-equal-p)))
      (progn
        (edraw-svg-defref-add-element defref element)
        (format "url(#edraw-def-%s-%s)"
                (edraw-svg-defref-idnum defref)
                prop-value))
    (let* ((defref (edraw-svg-defrefs-insert-to-empty-idnum defrefs def))
           (idnum (edraw-svg-defref-idnum defref)))
      ;; add a new definition element
      (edraw-svg-defref-add-element defref element)
      (edraw-svg-set-attr-string def 'id (format "edraw-def-%s-%s" idnum prop-value))
      (dom-append-child (edraw-svg-defrefs-defs defrefs) def)
      (format "url(#edraw-def-%s-%s)" idnum prop-value))))
(defun edraw-svg-defrefs-remove-ref-by-idnum (defrefs idnum element)
  (let ((cell (edraw-svg-defrefs-head defrefs)))
    (while (and (cdr cell)
                (not (= (edraw-svg-defref-idnum (cadr cell)) idnum)))
      (setq cell (cdr cell)))
    (when (cdr cell)
      (let ((defref (cadr cell)))
        (edraw-svg-defref-remove-element defref element)
        ;; when no referrer
        (when (edraw-svg-defref-empty-p defref)
          ;; remove definition element
          (dom-remove-node
           (edraw-svg-defrefs-defs defrefs)
           (edraw-svg-defref-def defref))
          ;; remove defref pair
          (setcdr cell (cddr cell)))))))
(defun edraw-svg-defrefs-get-by-idnum (defrefs idnum)
  (seq-find (lambda (defref) (= (edraw-svg-defref-idnum defref) idnum))
            (edraw-svg-defrefs-defrefs defrefs)))
(defun edraw-svg-defrefs-add-ref-by-idnum (defrefs idnum element)
  (when-let ((defref (edraw-svg-defrefs-get-by-idnum defrefs idnum)))
    (edraw-svg-defref-add-element defref element)))
(defun edraw-svg-defref-id-attr-to-idnum (id-attr)
  (and (stringp id-attr)
       (string-match "\\`edraw-def-\\([0-9]+\\)-\\([^)]+\\)\\'" id-attr)
       (string-to-number (match-string 1 id-attr))))
(defun edraw-svg-defref-url-to-idnum (url)
  (and (stringp url)
       (string-match "\\`url(#edraw-def-\\([0-9]+\\)-\\([^)]+\\))\\'" url)
       (string-to-number (match-string 1 url))))
(defun edraw-svg-defref-url-to-prop-value (url)
  (and (stringp url)
       (string-match "\\`url(#edraw-def-\\([0-9]+\\)-\\([^)]+\\))\\'" url)
       (match-string-no-properties 2 url)))
(defun edraw-svg-defrefs-remove-ref-by-url (defrefs url element)
  (when-let ((idnum (edraw-svg-defref-url-to-idnum url)))
    (edraw-svg-defrefs-remove-ref-by-idnum defrefs idnum element)))
(defun edraw-svg-defrefs-get-defref-by-url (defrefs url)
  (when-let ((idnum (edraw-svg-defref-url-to-idnum url)))
    (edraw-svg-defrefs-get-by-idnum defrefs idnum)))

(defun edraw-svg-defrefs-from-dom (defs-node body-node)
  (let ((defrefs (edraw-svg-defrefs defs-node))
        defref-list)
    ;; Collect definitions
    (dolist (def (dom-children defs-node))
      (when-let ((idnum (edraw-svg-defref-id-attr-to-idnum (dom-attr def 'id))))
        (push (edraw-svg-defref def idnum) defref-list)))
    ;; Sort and assign
    (setcdr (edraw-svg-defrefs-head defrefs)
            (sort defref-list (lambda (defref1 defref2)
                                (< (edraw-svg-defref-idnum defref1)
                                   (edraw-svg-defref-idnum defref2)))))
    ;; Collect references
    (when body-node
      (dolist (element (dom-children body-node))
        (dolist (attr (dom-attributes element))
          (when (member (car attr) '(marker-start marker-mid marker-end))
            (when-let ((idnum (edraw-svg-defref-url-to-idnum (cdr attr))))
              (edraw-svg-defrefs-add-ref-by-idnum defrefs idnum element))))))
    ;; Remove unreferenced definitions
    (dolist (defref (edraw-svg-defrefs-defrefs defrefs))
      (when (edraw-svg-defref-empty-p defref)
        (dom-remove-node defs-node (edraw-svg-defref-def defref))))
    (setcdr (edraw-svg-defrefs-head defrefs)
            (seq-remove 'edraw-svg-defref-empty-p
                        (edraw-svg-defrefs-defrefs defrefs)))

    defrefs))


;;;; SVG Marker

(defconst edraw-svg-marker-arrow-overhang
  (/ (*
      6 ;;markerWidth
      4) ;;arrow tip position
     20.0)) ;;viewBox width

(defun edraw-svg-create-marker (type prop-name element)
  (pcase type
    ("arrow"
     (dom-node 'marker
               `((markerWidth . "6")
                 (markerHeight . "6")
                 (viewBox . "-10 -10 20 20")
                 (refX . "0")
                 (refY . "0")
                 (orient . "auto")
                 (stroke . "none")
                 (fill .
                       ;; @todo I want to use context-stroke and remove edraw-svg-update-marker-properties
                       ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/618
                       ,(let ((stroke (dom-attr element 'stroke)))
                          (if (or (null stroke) (equal stroke "none"))
                              "none" ;;stroke may change later
                            stroke))))
               (dom-node 'path
                         ;; @todo I want to use auto-start-reverse
                         ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/484
                         (if (eq prop-name 'marker-start)
                             `((d . "M10,-7 10,7 -4,0Z")) ;; <|
                           `((d . "M-10,-7 -10,7 4,0Z")))))) ;; |>
    ("circle"
     (dom-node 'marker
               `((markerWidth . "4")
                 (markerHeight . "4")
                 (viewBox . "0 0 10 10")
                 (refX . "5")
                 (refY . "5")
                 (stroke . "none")
                 (fill .
                       ;; @todo I want to use context-stroke
                       ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/618
                       ,(let ((stroke (dom-attr element 'stroke)))
                          (if (or (null stroke) (equal stroke "none"))
                              "none" ;;stroke may change later
                            stroke))))
               (dom-node 'circle
                         `((cx . "5") (cy . "5") (r . "4")))))))

(defun edraw-svg-set-marker-property (element prop-name value defrefs)
  ;; Remove reference to current marker
  (edraw-svg-defrefs-remove-ref-by-url
   defrefs
   (dom-attr element prop-name) ;;url(#...) or "none" or nil
   element)
  ;; Add reference to value
  (let ((marker (edraw-svg-create-marker value prop-name element)))
    (if marker
        (edraw-svg-set-attr-string element
                                   prop-name
                                   (edraw-svg-defrefs-add-ref
                                    defrefs marker element value))
      (edraw-dom-remove-attr element
                             prop-name))))

(defun edraw-svg-get-marker-property (element prop-name _defrefs)
  (edraw-svg-defref-url-to-prop-value
   (dom-attr element prop-name)))

(defun edraw-svg-update-marker-properties (element defrefs)
  (edraw-svg-update-marker-property element 'marker-start defrefs)
  (edraw-svg-update-marker-property element 'marker-mid defrefs)
  (edraw-svg-update-marker-property element 'marker-end defrefs))

(defun edraw-svg-update-marker-property (element prop-name defrefs)
  (let ((value (dom-attr element prop-name)))
    (when (and value
               (stringp value)
               (not (string= value "none")))
      (edraw-svg-set-marker-property
       element
       prop-name
       (edraw-svg-defref-url-to-prop-value value)
       defrefs))))



;;;; SVG Shape Bounding Box

;; (Depends on edraw-math.el)

(defun edraw-svg-shape-aabb (element &optional matrix local-p)
  (let ((edraw-path-cmdlist-to-seglist--include-empty-p t)) ;;Enumerate zero-length segments
    (edraw-path-seglist-aabb
     (edraw-svg-element-to-seglist element matrix local-p))))

(defvar edraw-svg-text-contents-aabb--remove-last-descent nil)

(defun edraw-svg-text-contents-aabb (element)
  "Return the axis-aligned bounding box of the text ELEMENT.

This function does not consider the effect of the transform attribute."
  ;; https://www.w3.org/TR/SVG11/text.html#TextElement
  ;; @todo support inherit attribute from ancestor
  (let* ((x (or (dom-attr element 'x) ""))
         (y (or (dom-attr element 'y) ""))
         (separator ;;comma-wsp
          "\\(?:[ \t\n\f\r]+,?[ \t\n\f\r]*\\|,[ \t\n\f\r]*\\)")
         (xs
          (if (stringp x)
              (or (mapcar #'string-to-number (split-string x separator t))
                  (list 0))
            (list x)))
         (ys
          (if (stringp y)
              (or (mapcar #'string-to-number (split-string y separator t))
                  (list 0))
            (list y)))
         ;;@todo support dx, dy
         (text (edraw-svg-text-get-text element));;@todo analyze decendant nodes
         (lines (split-string text "\n"))
         (max-width (cl-loop for line in lines
                             maximize (string-width line)))
         (text-anchor (or (dom-attr element 'text-anchor) "start"))
         (font-size (or (edraw-svg-attr-number element 'font-size) 12)) ;;@todo default font size
         (font-ascent (/ (* font-size 80) 100)) ;;@todo default font ascent
         )
    ;;@todo direction=rtl, writing-mode
    ;;@todo support baseline spec. (but librsvg does not support baseline spec https://gitlab.gnome.org/GNOME/librsvg/-/issues/414 )
    ;;@todo support list-of-coordinates x=, y=, dx=, dy= (librsvg does not support https://gitlab.gnome.org/GNOME/librsvg/-/issues/183 )
    ;;@todo support rotate (librsvg does not suppor ?)
    ;;@todo support textLength (librsvg does not support https://gitlab.gnome.org/GNOME/librsvg/-/issues/88 )

    (let* ((text-w (* 0.5 font-size max-width))
           (text-x (+ (car xs)
                      (* text-w (pcase text-anchor
                                  ("middle" -0.5) ("end" -1) (_ 0)))))
           (text-y (- (car ys) font-ascent))
           (text-h (* font-size (length lines))))
      (when edraw-svg-text-contents-aabb--remove-last-descent
        (setq text-h (max 0 (- text-h (- font-size font-ascent)))))
      ;;(message "x=%s y=%s text-x=%s text-y=%s text-w=%s text-h=%s text-anchor=%s" (car xs) (car ys) text-x text-y text-w text-h text-anchor)
      (edraw-rect-xywh text-x text-y text-w text-h))))


;;;; SVG Shape Translation

;;
;;

(defun edraw-svg-element-translate (element xy)
  (let ((transform (edraw-svg-element-transform-get element)))
    (pcase (dom-tag element)
      ((or 'path 'rect 'ellipse 'circle 'text 'image)
       (if transform ;;(not (edraw-matrix-translation-only-p transform)) ?
           (progn
             (edraw-matrix-translate-add transform (car xy) (cdr xy))
             (edraw-svg-element-transform-set element transform))
         (edraw-svg-shape-translate-contents element xy)))
      ('g
       (if transform
           (progn
             (edraw-matrix-translate-add transform (car xy) (cdr xy))
             (edraw-svg-element-transform-set element transform))
         (edraw-svg-element-transform-set
          element
          (edraw-matrix-translate (car xy) (cdr xy) 0)))))))

(defun edraw-svg-shape-translate-contents (element xy)
  (pcase (dom-tag element)
    ('rect (edraw-svg-rect-translate-contents element xy))
    ('ellipse (edraw-svg-ellipse-translate-contents element xy))
    ('circle (edraw-svg-circle-translate-contents element xy))
    ('text (edraw-svg-text-translate-contents element xy))
    ('image (edraw-svg-image-translate-contents element xy))
    ('path (edraw-svg-path-translate-contents element xy))
    ('g (edraw-svg-group-translate-contents element xy)))
  element)

(defun edraw-svg-rect-translate-contents (element xy)
  (edraw-svg-set-attr-number element 'x
                             (+ (or (edraw-svg-attr-coord element 'x) 0)
                                (car xy)))
  (edraw-svg-set-attr-number element 'y
                             (+ (or (edraw-svg-attr-coord element 'y) 0)
                                (cdr xy))))

(defun edraw-svg-ellipse-translate-contents (element xy)
  (edraw-svg-set-attr-number element 'cx
                             (+ (or (edraw-svg-attr-coord element 'cx) 0)
                                (car xy)))
  (edraw-svg-set-attr-number element 'cy
                             (+ (or (edraw-svg-attr-coord element 'cy) 0)
                                (cdr xy))))

(defun edraw-svg-circle-translate-contents (element xy)
  (edraw-svg-set-attr-number element 'cx
                             (+ (or (edraw-svg-attr-coord element 'cx) 0)
                                (car xy)))
  (edraw-svg-set-attr-number element 'cy
                             (+ (or (edraw-svg-attr-coord element 'cy) 0)
                                (cdr xy))))

(defun edraw-svg-text-translate-contents (element xy)
  ;;@todo support list-of-coordinates
  (edraw-svg-text-set-x element (+ (or (edraw-svg-attr-coord element 'x) 0)
                                   (car xy)))
  (edraw-svg-set-attr-number element 'y
                             (+ (or (edraw-svg-attr-coord element 'y) 0)
                                (cdr xy))))

(defun edraw-svg-image-translate-contents (element xy)
  (edraw-svg-set-attr-number element 'x
                             (+ (or (edraw-svg-attr-coord element 'x) 0)
                                (car xy)))
  (edraw-svg-set-attr-number element 'y
                             (+ (or (edraw-svg-attr-coord element 'y) 0)
                                (cdr xy))))

(defun edraw-svg-path-translate-contents (element xy)
  (when-let ((d (dom-attr element 'd)))
    (edraw-svg-set-attr-string element 'd (edraw-path-d-translate d xy))))

(defun edraw-svg-group-translate-contents (element xy)
  ;;@todo Should I change the transform attribute instead?
  ;; Transformation of children is inefficient and causes numerical error.
  ;; But easy to ungroup.
  (dolist (child (dom-children element))
    (when (edraw-dom-element-p child)
      (edraw-svg-element-translate child xy))))



;;;; SVG Shapes to edraw-path-cmdlist

(defconst edraw-bezier-circle-point 0.552284749831) ;;https://stackoverflow.com/questions/1734745/how-to-create-circle-with-b%C3%A9zier-curves

;; (Depends on edraw-path.el)

(defun edraw-svg-element-to-path-cmdlist (element &optional matrix transformed)
  (edraw-svg-element-contents-to-path-cmdlist
   element
   (when transformed
     (edraw-svg-element-transform-get element matrix))))

(defun edraw-svg-element-contents-to-path-cmdlist (element &optional matrix)
  (when (edraw-dom-element-p element)
    (pcase (dom-tag element)
      ((or 'path 'rect 'ellipse 'circle 'text 'image)
       (let ((cmdlist (edraw-svg-shape-contents-to-path-cmdlist element)))
         (unless (edraw-matrix-identity-p matrix)
           (edraw-path-cmdlist-transform cmdlist matrix))
         cmdlist))
      ('g
       (edraw-svg-group-contents-to-path-cmdlist element matrix)))))

(defun edraw-svg-shape-contents-to-path-cmdlist (element)
  (when (edraw-dom-element-p element)
    (pcase (dom-tag element)
      ('path (edraw-svg-path-contents-to-path-cmdlist element))
      ('rect (edraw-svg-rect-contents-to-path-cmdlist element))
      ('ellipse (edraw-svg-ellipse-contents-to-path-cmdlist element))
      ('circle (edraw-svg-circle-contents-to-path-cmdlist element))
      ('text (edraw-svg-text-contents-to-path-cmdlist element))
      ('image (edraw-svg-image-contents-to-path-cmdlist element)))))

(defun edraw-svg-path-contents-to-path-cmdlist (element)
  (let ((fill (dom-attr element 'fill))
        (d (dom-attr element 'd)))
    (when d
      (let ((cmdlist (edraw-path-cmdlist-from-d d))
            (needs-closed-p (not (equal fill "none"))))
        (when needs-closed-p
          (edraw-path-cmdlist-close-path cmdlist t))
        cmdlist))))

(defun edraw-svg-rect-contents-to-path-cmdlist (element)
  ;; https://www.w3.org/TR/SVG11/shapes.html#RectElement
  (let* ((x0 (or (edraw-svg-attr-coord element 'x) 0))
         (y0 (or (edraw-svg-attr-coord element 'y) 0))
         (width (or (edraw-svg-attr-coord element 'width) 0))
         (height (or (edraw-svg-attr-coord element 'height) 0))
         (x3 (+ x0 width))
         (y3 (+ y0 height))
         (rx-spec (edraw-svg-attr-length element 'rx))
         (ry-spec (edraw-svg-attr-length element 'ry))
         (rx (edraw-clamp (if (numberp rx-spec) rx-spec
                            (if (numberp ry-spec) ry-spec 0))
                          0 (/ width 2.0)))
         (ry (edraw-clamp (if (numberp ry-spec) ry-spec
                            (if (numberp rx-spec) rx-spec 0))
                          0 (/ height 2.0)))
         (c edraw-bezier-circle-point)
         (crx (* c rx))
         (cry (* c ry))
         (x1 (+ x0 rx))
         (y1 (+ y0 ry))
         (x2 (max x1 (- x3 rx)))
         (y2 (max y1 (- y3 ry)))
         (cmdlist (edraw-path-cmdlist)))

    (cond
     ((or (= rx 0) (= ry 0))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'M (cons x0 y0)))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'L (cons x3 y0)))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'L (cons x3 y3)))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'L (cons x0 y3)))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'L (cons x0 y0)))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'Z)))

     (t
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'M (cons x1 y0)))
      (unless (= x1 x2)
        (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                               'L (cons x2 y0))))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'C
                                             (cons (+ x2 crx) y0)
                                             (cons x3 (- y1 cry))
                                             (cons x3 y1)))
      (unless (= y1 y2)
        (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                               'L (cons x3 y2))))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'C
                                             (cons x3 (+ y2 cry))
                                             (cons (+ x2 crx) y3)
                                             (cons x2 y3)))
      (unless (= x1 x2)
        (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                               'L (cons x1 y3))))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'C
                                             (cons (- x1 crx) y3)
                                             (cons x0 (+ y2 cry))
                                             (cons x0 y2)))
      (unless (= y1 y2)
        (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                               'L (cons x0 y1))))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                             'C
                                             (cons x0 (- y1 cry))
                                             (cons (- x1 crx) y0)
                                             (cons x1 y0)))
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'Z))))
    cmdlist))

(defun edraw-svg-ellipse-contents-to-path-cmdlist (element)
  ;; https://www.w3.org/TR/SVG11/shapes.html#EllipseElement
  (let* ((cx (or (edraw-svg-attr-coord element 'cx) 0))
         (cy (or (edraw-svg-attr-coord element 'cy) 0))
         (rx (or (edraw-svg-attr-coord element 'rx) 0))
         (ry (or (edraw-svg-attr-coord element 'ry) 0))
         (left   (- cx rx))
         (top    (- cy ry))
         (right  (+ cx rx))
         (bottom (+ cy ry))
         (c edraw-bezier-circle-point)
         (crx (* c rx))
         (cry (* c ry))
         (cmdlist (edraw-path-cmdlist)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'M (cons right cy)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'C
                                           (cons right (+ cy cry))
                                           (cons (+ cx crx) bottom)
                                           (cons cx bottom)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'C
                                           (cons (- cx crx) bottom)
                                           (cons left (+ cy cry))
                                           (cons left cy)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'C
                                           (cons left (- cy cry))
                                           (cons (- cx crx) top)
                                           (cons cx top)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'C
                                           (cons (+ cx crx) top)
                                           (cons right (- cy cry))
                                           (cons right cy)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'Z))
    cmdlist))

(defun edraw-svg-circle-contents-to-path-cmdlist (element)
  ;; https://www.w3.org/TR/SVG11/shapes.html#CircleElement
  (let* ((cx (or (edraw-svg-attr-coord element 'cx) 0))
         (cy (or (edraw-svg-attr-coord element 'cy) 0))
         (r (or (edraw-svg-attr-coord element 'r) 0))
         (left   (- cx r))
         (top    (- cy r))
         (right  (+ cx r))
         (bottom (+ cy r))
         (c edraw-bezier-circle-point)
         (cr (* c r))
         (cmdlist (edraw-path-cmdlist)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'M (cons right cy)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'C
                                           (cons right (+ cy cr))
                                           (cons (+ cx cr) bottom)
                                           (cons cx bottom)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'C
                                           (cons (- cx cr) bottom)
                                           (cons left (+ cy cr))
                                           (cons left cy)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'C
                                           (cons left (- cy cr))
                                           (cons (- cx cr) top)
                                           (cons cx top)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd
                                           'C
                                           (cons (+ cx cr) top)
                                           (cons right (- cy cr))
                                           (cons right cy)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'Z))
    cmdlist))

(defun edraw-svg-text-contents-to-path-cmdlist (element)
  ;; Exact calculation is difficult, so use AABB instead
  (let* ((rect (edraw-svg-text-contents-aabb element))
         (left   (caar rect))
         (top    (cdar rect))
         (right  (cadr rect))
         (bottom (cddr rect))
         (cmdlist (edraw-path-cmdlist)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'M (cons left top)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'L (cons right top)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'L (cons right bottom)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'L (cons left bottom)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'L (cons left top)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'Z))
    cmdlist))

(defun edraw-svg-image-contents-to-path-cmdlist (element)
  ;; https://www.w3.org/TR/SVG11/struct.html#ImageElement
  (let* ((left   (or (edraw-svg-attr-coord element 'x) 0))
         (top    (or (edraw-svg-attr-coord element 'y) 0))
         (width  (or (edraw-svg-attr-coord element 'width) 0))
         (height (or (edraw-svg-attr-coord element 'height) 0))
         (right  (+ left width))
         (bottom (+ top height))
         (cmdlist (edraw-path-cmdlist)))
    ;;@todo support overflow? clip?
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'M (cons left top)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'L (cons right top)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'L (cons right bottom)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'L (cons left bottom)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'L (cons left top)))
    (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'Z))
    cmdlist))

(defun edraw-svg-group-contents-to-path-cmdlist (element &optional matrix)
  (let (cmdlist)
    (dolist (child (dom-children element))
      (when (edraw-dom-element-p child)
        (let ((child-cmdlist (edraw-svg-element-to-path-cmdlist element matrix)))
          (when (and child-cmdlist
                     (not (edraw-path-cmdlist-empty-p child-cmdlist)))
            (when cmdlist
              (edraw-path-cmdlist-insert-cmdlist-front child-cmdlist cmdlist))
            (setq cmdlist child-cmdlist)))))
    cmdlist))



;;;; SVG Shapes to Segment List

;; (Depends on edraw-path.el)

(defun edraw-svg-element-to-seglist (element &optional matrix local-p)
  (edraw-svg-element-contents-to-seglist
   element
   (if local-p
       matrix
     ;; Apply the transform= attribute if not local-p
     (edraw-svg-element-transform-get element matrix))))

(defun edraw-svg-element-contents-to-seglist (element &optional matrix)
  (when (edraw-dom-element-p element)
    (pcase (dom-tag element)
      ((or 'path 'rect 'ellipse 'circle 'text 'image)
       (let ((segments (edraw-svg-shape-contents-to-seglist element)))
         (unless (edraw-matrix-identity-p matrix)
           (edraw-path-seglist-transform segments matrix))
         segments))
      ('g
       (edraw-svg-group-contents-to-seglist element matrix)))))

(defun edraw-svg-shape-contents-to-seglist (element)
  (when (edraw-dom-element-p element)
    (pcase (dom-tag element)
      ('path (edraw-svg-path-contents-to-seglist element))
      ('rect (edraw-svg-rect-contents-to-seglist element))
      ('ellipse (edraw-svg-ellipse-contents-to-seglist element))
      ('circle (edraw-svg-circle-contents-to-seglist element))
      ('text (edraw-svg-text-contents-to-seglist element))
      ('image (edraw-svg-image-contents-to-seglist element)))))

(defun edraw-svg-path-contents-to-seglist (element)
  (let ((fill (dom-attr element 'fill))
        (d (dom-attr element 'd)))
    (when d
      (edraw-path-cmdlist-to-seglist
       (edraw-path-cmdlist-from-d d)
       (not (equal fill "none"))))))

(defun edraw-svg-rect-contents-to-seglist (element)
  ;; https://www.w3.org/TR/SVG11/shapes.html#RectElement
  (let* ((x0 (or (edraw-svg-attr-coord element 'x) 0))
         (y0 (or (edraw-svg-attr-coord element 'y) 0))
         (width (or (edraw-svg-attr-coord element 'width) 0))
         (height (or (edraw-svg-attr-coord element 'height) 0))
         (x3 (+ x0 width))
         (y3 (+ y0 height))
         (rx-spec (edraw-svg-attr-length element 'rx))
         (ry-spec (edraw-svg-attr-length element 'ry))
         (rx (edraw-clamp (if (numberp rx-spec) rx-spec
                            (if (numberp ry-spec) ry-spec 0))
                          0 (/ width 2.0)))
         (ry (edraw-clamp (if (numberp ry-spec) ry-spec
                            (if (numberp rx-spec) rx-spec 0))
                          0 (/ height 2.0)))
         (c edraw-bezier-circle-point)
         (crx (* c rx))
         (cry (* c ry))
         (x1 (+ x0 rx))
         (y1 (+ y0 ry))
         (x2 (max x1 (- x3 rx)))
         (y2 (max y1 (- y3 ry)))
         (segments
          (cond
           ((or (= rx 0) (= ry 0))
            (list (vector (cons x0 y0) (cons x3 y0))
                  (vector (cons x3 y0) (cons x3 y3))
                  (vector (cons x3 y3) (cons x0 y3))
                  (vector (cons x0 y3) (cons x0 y0))))
           (t
            (delq
             nil
             (list
              (unless (= x1 x2)
                (vector (cons x1 y0) (cons x2 y0)))
              (vector (cons x2 y0) (cons (+ x2 crx) y0)
                      (cons x3 (- y1 cry)) (cons x3 y1))
              (unless (= y1 y2)
                (vector (cons x3 y1) (cons x3 y2)))
              (vector (cons x3 y2) (cons x3 (+ y2 cry))
                      (cons (+ x2 crx) y3) (cons x2 y3))
              (unless (= x1 x2)
                (vector (cons x2 y3) (cons x1 y3)))
              (vector (cons x1 y3) (cons (- x1 crx) y3)
                      (cons x0 (+ y2 cry)) (cons x0 y2))
              (unless (= y1 y2)
                (vector (cons x0 y2) (cons x0 y1)))

              (vector (cons x0 y1) (cons x0 (- y1 cry))
                      (cons (- x1 crx) y0) (cons x1 y0))))))))
    segments))

(defun edraw-svg-ellipse-contents-to-seglist (element)
  ;; https://www.w3.org/TR/SVG11/shapes.html#EllipseElement
  (let* ((cx (or (edraw-svg-attr-coord element 'cx) 0))
         (cy (or (edraw-svg-attr-coord element 'cy) 0))
         (rx (or (edraw-svg-attr-coord element 'rx) 0))
         (ry (or (edraw-svg-attr-coord element 'ry) 0))
         (left   (- cx rx))
         (top    (- cy ry))
         (right  (+ cx rx))
         (bottom (+ cy ry))
         (c edraw-bezier-circle-point)
         (crx (* c rx))
         (cry (* c ry))
         (segments
          (list
           (vector (cons right cy) (cons right (+ cy cry))
                   (cons (+ cx crx) bottom) (cons cx bottom))
           (vector (cons cx bottom) (cons (- cx crx) bottom)
                   (cons left (+ cy cry)) (cons left cy))
           (vector (cons left cy) (cons left (- cy cry))
                   (cons (- cx crx) top) (cons cx top))
           (vector (cons cx top) (cons (+ cx crx) top)
                   (cons right (- cy cry)) (cons right cy)))))
         segments))

(defun edraw-svg-circle-contents-to-seglist (element)
  ;; https://www.w3.org/TR/SVG11/shapes.html#CircleElement
  (let* ((cx (or (edraw-svg-attr-coord element 'cx) 0))
         (cy (or (edraw-svg-attr-coord element 'cy) 0))
         (r (or (edraw-svg-attr-coord element 'r) 0))
         (left   (- cx r))
         (top    (- cy r))
         (right  (+ cx r))
         (bottom (+ cy r))
         (c edraw-bezier-circle-point)
         (cr (* c r))
         (segments
          (list
           (vector (cons right cy) (cons right (+ cy cr))
                   (cons (+ cx cr) bottom) (cons cx bottom))
           (vector (cons cx bottom) (cons (- cx cr) bottom)
                   (cons left (+ cy cr)) (cons left cy))
           (vector (cons left cy) (cons left (- cy cr))
                   (cons (- cx cr) top) (cons cx top))
           (vector (cons cx top) (cons (+ cx cr) top)
                   (cons right (- cy cr)) (cons right cy)))))
    segments))

(defun edraw-svg-text-contents-to-seglist (element)
  ;; Exact calculation is difficult, so use AABB instead
  (let* ((rect (edraw-svg-text-contents-aabb element))
         (left   (caar rect))
         (top    (cdar rect))
         (right  (cadr rect))
         (bottom (cddr rect))
         (segments (list (vector (cons left  top   ) (cons right top   ))
                         (vector (cons right top   ) (cons right bottom))
                         (vector (cons right bottom) (cons left  bottom))
                         (vector (cons left  bottom) (cons left  top)))))
    segments))

(defun edraw-svg-image-contents-to-seglist (element)
  ;; https://www.w3.org/TR/SVG11/struct.html#ImageElement
  (let* ((left   (or (edraw-svg-attr-coord element 'x) 0))
         (top    (or (edraw-svg-attr-coord element 'y) 0))
         (width  (or (edraw-svg-attr-coord element 'width) 0))
         (height (or (edraw-svg-attr-coord element 'height) 0))
         (right  (+ left width))
         (bottom (+ top height))
         ;;@todo support overflow? clip?
         (segments (list (vector (cons left  top   ) (cons right top   ))
                         (vector (cons right top   ) (cons right bottom))
                         (vector (cons right bottom) (cons left  bottom))
                         (vector (cons left  bottom) (cons left  top)))))
    segments))

(defun edraw-svg-group-contents-to-seglist (element &optional matrix)
  (let (segments)
    (dolist (child (dom-children element))
      (when (edraw-dom-element-p child)
        (let ((child-segments (edraw-svg-element-to-seglist child matrix)))
          (setq segments (nconc segments child-segments)))))
    segments))



;;;; Point in SVG Shapes Test

;; (Depends on edraw-path.el)

(defconst edraw-pick-point-radius 2)

(defun edraw-svg-element-contains-point-p (element xy)
  (let ((transform (edraw-svg-element-transform-get element)))
    (unless (edraw-matrix-identity-p transform)
      (when-let ((inv (edraw-matrix-inverse transform)))
        (setq xy (edraw-matrix-mul-mat-xy inv xy)))))

  (when (edraw-dom-element-p element)
    (pcase (dom-tag element)
      ((or 'path 'rect 'ellipse 'circle 'text 'image)
       (edraw-svg-shape-contains-point-p element xy))
      ('g
       (edraw-svg-group-contains-point-p element xy)))))

(defun edraw-svg-shape-contains-point-p (element xy)
  (let* ((fill (dom-attr element 'fill))
         (fill-p (not (equal fill "none"))) ;;default black
         (fill-rule (dom-attr element 'fill-rule))
         (stroke (dom-attr element 'stroke))
         (stroke-p (and stroke ;;default none
                        (not (equal stroke ""))
                        (not (equal stroke "none"))))
         (stroke-width (if stroke-p
                           (or (edraw-svg-attr-number element 'stroke-width) 1)
                         0))
         (stroke-square-r (/ stroke-width (* 2 (sqrt 2))))
         (segments (edraw-svg-shape-contents-to-seglist element)
                   ;;or (edraw-svg-element-contents-to-seglist element)
                   )
         (text-bb-p (eq (dom-tag element) 'text)))

    (when segments
      (or (and stroke-p
               (not text-bb-p)
               (edraw-path-seglist-intersects-rect-p
                segments
                (edraw-square xy (+ edraw-pick-point-radius stroke-square-r))))
          (and (or fill-p
                   text-bb-p)
               (edraw-path-seglist-contains-point-p
                segments
                xy
                (equal fill-rule "evenodd")))))))

(defun edraw-svg-group-contains-point-p (element xy)
  (seq-some
   (lambda (child)
     (and (edraw-dom-element-p child)
          (edraw-svg-element-contains-point-p child xy)))
   (dom-children element)))


;;;; SVG Shapes and Rectangle Intersection Test

(defun edraw-svg-element-intersects-rect-p (element rect &optional matrix)
  (when (edraw-dom-element-p element)
    (pcase (dom-tag element)
      ((or 'path 'rect 'ellipse 'circle 'text 'image)
       (edraw-svg-shape-intersects-rect-p element rect matrix))
      ('g
       (edraw-svg-group-intersects-rect-p element rect matrix)))))

(defun edraw-svg-shape-intersects-rect-p (element rect &optional matrix)
  (when (and element
             rect
             (not (edraw-rect-empty-p rect)))
    (let* ((fill (dom-attr element 'fill))
           (fill-p (not (equal fill "none"))) ;;default black
           (fill-rule (dom-attr element 'fill-rule))
           (stroke (dom-attr element 'stroke))
           (stroke-width (if (and stroke
                                  (not (equal stroke ""))
                                  (not (equal stroke "none")))
                             (or (edraw-svg-attr-length element 'stroke-width) 1)
                           0))
           (stroke-r (/ stroke-width (* 2 (sqrt 2))))
           (enlarged-rect (edraw-rect
                           (- (caar rect) stroke-r)
                           (- (cdar rect) stroke-r)
                           (+ (cadr rect) stroke-r)
                           (+ (cddr rect) stroke-r)))
           (segments (edraw-svg-element-to-seglist element matrix))
           (text-aabb-p (eq (dom-tag element) 'text)))
      (when segments
        (or (edraw-path-seglist-intersects-rect-p segments enlarged-rect)
            ;; Case where rect is completely inside the shape
            (and (or fill-p
                     text-aabb-p)
                 (edraw-path-seglist-contains-point-p
                  segments
                  (edraw-xy (caar enlarged-rect) (cdar enlarged-rect))
                  (equal fill-rule "evenodd"))))))))

(defun edraw-svg-group-intersects-rect-p (element rect &optional matrix)
  (let ((sub-matrix (edraw-svg-element-transform-get element matrix)))
    (seq-some
     (lambda (child)
       (and (edraw-dom-element-p child)
            (edraw-svg-element-intersects-rect-p child rect sub-matrix)))
     (dom-children element))))

;;;; Intersection Coordinates of SVG Shape and Line

(defun edraw-svg-element-and-line-intersections (element pt dir &optional matrix local-p)
  (setq dir (edraw-xy-normalize dir))
  (let* ((segments (edraw-svg-element-to-seglist element matrix local-p))
         (invdir (edraw-xy (edraw-x dir) (- (edraw-y dir))))
         (invdir90 (edraw-xy-rot90 invdir))
         (invpt-y (+ (* (edraw-x pt) (edraw-y invdir))
                     (* (edraw-y pt) (edraw-y invdir90))))
         (invpt-y-dir90 (edraw-xy-nmul invpt-y (edraw-xy-rot90 dir))))
    (edraw-path-seglist-transform-mat22
     segments
     (cons invdir invdir90))

    (mapcar
     (lambda (x) (edraw-xy-add (edraw-xy-nmul x dir) invpt-y-dir90))
     (sort
      (edraw-path-seglist-and-horizontal-line-intersections
       segments invpt-y)
      #'<))))
;; (edraw-svg-element-and-line-intersections (dom-node 'rect '((x . "100") (y . "50") (width . 300) (height . 200))) (edraw-xy 100 100) (edraw-xy 10 10))

;;;; SVG Shape Thumbnail

(defun edraw-svg-shape-thumbnail-cover (svg svg-width svg-height spec
                                            pl pt cw ch id)
  (let ((bl 0)
        (bt 0)
        (bw svg-width)
        (bh svg-height)
        (attrs '((fill . "#ffffff"))))
    ;; '(content (symbol . value)...)
    ;; '(full (symbol . value)...)
    (pcase spec
      (`(content . ,alist)
       (setq bl pl bt pt bw cw bh ch attrs alist))
      (`(full . ,alist)
       (setq attrs alist)))
    (dom-append-child
     svg
     (dom-node 'rect
               `((id . ,id)
                 (x . ,bl)
                 (y . ,bt)
                 (width . ,bw)
                 (height . ,bh)
                 ,@attrs)))))

(defun edraw-svg-shape-thumbnail (shape svg-width svg-height
                                        &optional
                                        padding background foreground
                                        svg-max-width svg-max-height)
  (let ((aabb (edraw-svg-shape-aabb shape)))
    (unless (edraw-rect-empty-p aabb)
      (setq padding
            (pcase padding
              (`(,pl ,pt ,pr ,pb) (list pl pt pr pb))
              (`(,plr ,ptb) (list plr ptb plr ptb))
              ('nil (list 0 0 0 0))
              (n (list n n n n))))
      (unless (seq-every-p #'numberp padding)
        (error "Wrong padding spec %s" padding))

      (let* (;;bounding box
             (bl (edraw-rect-left aabb))
             (bt (edraw-rect-top aabb))
             (bw (edraw-rect-width aabb))
             (bh (edraw-rect-height aabb))
             ;;padding
             (pl (nth 0 padding))
             (pt (nth 1 padding))
             (pr (nth 2 padding))
             (pb (nth 3 padding))
             ;;content (without padding)
             (cw (max 0
                      (- svg-width pl pr)
                      (if svg-max-width (min (- svg-max-width pl pr) bw) 0)))
             (ch (max 0
                      (- svg-height pt pb)
                      (if svg-max-height (min (- svg-max-height pt pb) bh) 0)))
             ;;scale
             (sx (/ (float cw) bw))
             (sy (/ (float ch) bh))
             (scale (min sx sy 1.0)))

        (setq svg-width (+ pl cw pr)
              svg-height (+ pt ch pb))

        (let ((svg (svg-create svg-width svg-height)))
          (when background
            (edraw-svg-shape-thumbnail-cover
             svg svg-width svg-height
             background pl pt cw ch "background"))

          ;; Body
          (dom-append-child
           svg
           (dom-node 'g
                     `((id . "body")
                       (transform
                        .
                        ,(concat
                          (format "translate(%s %s)"
                                  (+ pl (/ (- cw (* bw scale)) 2))
                                  (+ pt (/ (- ch (* bh scale)) 2)))
                          " "
                          (format "scale(%s)" scale)
                          " "
                          (format "translate(%s %s)" (- bl) (- bt)))))
                     shape))

          (when foreground
            (edraw-svg-shape-thumbnail-cover
             svg svg-width svg-height
             foreground pl pt cw ch "foreground"))

          svg)))))



(provide 'edraw-dom-svg)
;;; edraw-dom-svg.el ends here
