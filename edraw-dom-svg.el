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
(require 'edraw-math)


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


;;;; SVG Print


(defun edraw-svg-to-image (svg &rest props)
  (apply
   #'create-image
   (edraw-svg-to-string svg nil #'edraw-svg-print-attr-filter)
   'svg t
   props))

(defun edraw-svg-to-string (dom node-filter attr-filter)
  (with-temp-buffer
    (edraw-svg-print dom node-filter attr-filter)
    (buffer-string)))

(defun edraw-svg-print (dom node-filter attr-filter &optional indent no-indent)
  ;; Derived from svg-print in svg.el
  (when (or (null node-filter) (funcall node-filter dom))
    (if (stringp dom)
        ;;@todo escape text
        (insert dom)
      (let ((tag (car dom))
            (attrs (cadr dom))
            (children (cddr dom)))
        (when (and (integerp indent) (not no-indent)) (insert (make-string indent ? )))
        (insert (format "<%s" tag))
        (dolist (attr attrs)
          (when (or (null attr-filter) (funcall attr-filter attr))
            ;;@todo escape attribute values (What is the state after libxml parses?)
            (insert (format " %s=\"%s\"" (car attr) (cdr attr)))))
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


;;;; SVG Attributes


(defun edraw-svg-attr-number (element attr)
  (edraw-svg-number-string-to-number (dom-attr element attr)))

(defun edraw-svg-attr-coord (element attr)
  ;; <coordinate> ::= <length>
  (edraw-svg-attr-length element attr))

(defun edraw-svg-attr-length (element attr)
  (edraw-svg-length-string-to-number (dom-attr element attr)))

(defun edraw-svg-length-string-to-number (value)
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

(defun edraw-svg-number-string-to-number (value)
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

(defun edraw-svg-rect-set-range (element xy0 xy1)
  (dom-set-attribute element 'x (min (car xy0) (car xy1)))
  (dom-set-attribute element 'y (min (cdr xy0) (cdr xy1)))
  (dom-set-attribute element 'width (abs (- (car xy0) (car xy1))))
  (dom-set-attribute element 'height (abs (- (cdr xy0) (cdr xy1)))))

;; (defun edraw-svg-ellipse (parent xy0 xy1 &rest args)
;;   (let ((element (dom-node 'ellipse
;;                            `((cx . ,(* 0.5 (+ (car xy0) (car xy1))))
;;                              (cy . ,(* 0.5 (+ (cdr xy0) (cdr xy1))))
;;                              (rx . ,(* 0.5 (abs (- (car xy0) (car xy1)))))
;;                              (ry . ,(* 0.5 (abs (- (cdr xy0) (cdr xy1)))))
;;                              ,@(svg--arguments parent args)))))
;;     (svg--append parent element) ;;Avoid duplicate ids
;;     element)) ;;Return element

(defun edraw-svg-ellipse-set-range (element xy0 xy1)
  (dom-set-attribute element 'cx (* 0.5 (+ (car xy0) (car xy1))))
  (dom-set-attribute element 'cy (* 0.5 (+ (cdr xy0) (cdr xy1))))
  (dom-set-attribute element 'rx (* 0.5 (abs (- (car xy0) (car xy1)))))
  (dom-set-attribute element 'ry (* 0.5 (abs (- (cdr xy0) (cdr xy1))))))

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


;;;; SVG Shape Summary


(defun edraw-svg-element-summary (element)
  (pcase (dom-tag element)
    ('path (edraw-svg-path-summary element))
    ('rect (edraw-svg-rect-summary element))
    ('ellipse (edraw-svg-ellipse-summary element))
    ('circle (edraw-svg-circle-summary element))
    ('text (edraw-svg-text-summary element))))

(defun edraw-svg-path-summary (element)
  (format "path (%s)"
          (truncate-string-to-width
           (or (dom-attr element 'd) "") 20 nil nil "...")))

(defun edraw-svg-rect-summary (element)
  (format "rect (%s,%s,%s,%s)"
          (edraw-svg-attr-coord element 'x)
          (edraw-svg-attr-coord element 'y)
          (edraw-svg-attr-length element 'width)
          (edraw-svg-attr-length element 'height)))

(defun edraw-svg-ellipse-summary (element)
  (format "ellipse (%s,%s,%s,%s)"
          (edraw-svg-attr-coord element 'cx)
          (edraw-svg-attr-coord element 'cy)
          (edraw-svg-attr-length element 'rx)
          (edraw-svg-attr-length element 'ry)))

(defun edraw-svg-circle-summary (element)
  (format "circle (%s,%s,%s)"
          (edraw-svg-attr-coord element 'cx)
          (edraw-svg-attr-coord element 'cy)
          (edraw-svg-attr-length element 'r)))

(defun edraw-svg-text-summary (element)
  (format "text (%s)"
          (truncate-string-to-width (dom-text element) 20 nil nil "...")))


;;;; SVG Shape Properties


(defconst edraw-svg-element-properties-common
  ;;name source type required
  '((fill attr-fill-stroke paint nil)
    (fill-opacity attr opacity nil)
    (stroke attr-fill-stroke paint nil)
    (stroke-opacity attr opacity nil)
    (stroke-width attr length nil)
    (stroke-dasharray attr string nil)
    (stroke-dashoffset attr length nil)
    (style attr string nil)))
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
     (fill-rule attr (or "nonzero" "evenodd") nil)
     (stroke-linecap attr (or "butt" "round" "square") nil)
     (stroke-linejoin attr (or "miter" "round" "bevel") nil)
     (stroke-miterlimit attr number nil)
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
     ,@edraw-svg-element-properties-common)))
(defun edraw-svg-elem-prop-name (prop-def) (nth 0 prop-def))
(defun edraw-svg-elem-prop-source (prop-def) (nth 1 prop-def))
(defun edraw-svg-elem-prop-type (prop-def) (nth 2 prop-def))
(defun edraw-svg-elem-prop-required (prop-def) (nth 3 prop-def))
(defun edraw-svg-elem-prop-attrs (prop-def) (nthcdr 4 prop-def))

(defun edraw-svg-element-get-property-info-list (element)
  (edraw-svg-element-get-property-info-list-by-tag (dom-tag element)))

(defun edraw-svg-element-get-property-info-list-by-tag (tag)
  (when-let ((prop-def-list (alist-get tag edraw-svg-element-properties)))
    (cl-loop for prop-def in prop-def-list
             collect
             (append
              (list (edraw-svg-elem-prop-name prop-def)
                    :type (edraw-svg-elem-prop-type prop-def)
                    :required (edraw-svg-elem-prop-required prop-def))
              (edraw-svg-elem-prop-attrs prop-def)))))
;; TEST: (edraw-svg-element-get-property-info-list-by-tag 'rect)

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

;; Property Source

(defun edraw-svg-element-get-attr (element prop-name _defrefs)
  (let ((value (dom-attr element prop-name)))
    ;; Always return as a string or nil
    (if (null value)
        nil
      (format "%s" value))))

(defun edraw-svg-element-set-attr (element prop-name value _defrefs)
  ;; Always store as a string or nil
  (if (null value)
      (edraw-dom-remove-attr element prop-name)
    (if (and (eq (dom-tag element) 'text)
             (eq prop-name 'x))
        (edraw-svg-text-set-x element (format "%s" value))
      (dom-set-attribute element prop-name (format "%s" value)))))

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
  (when (stringp text)
    (edraw-dom-remove-all-children element)

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
  (dom-set-attribute element 'x x)
  (let ((tspans (dom-by-class element "\\`text-line\\'")))
    (dolist (tspan tspans)
      (dom-set-attribute tspan 'x x))))

(defun edraw-svg-text-set-xy (element xy)
  (edraw-svg-text-set-x element (car xy))
  (dom-set-attribute element 'y (cdr xy)))


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
      (dom-set-attribute def 'id (format "edraw-def-%s-%s" idnum prop-value))
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


(defun edraw-svg-create-marker (type prop-name element)
  (pcase type
    ("arrow"
     (dom-node 'marker
               `((markerWidth . "6")
                 (markerHeight . "6")
                 (viewBox . "0 0 10 10")
                 (refX . "5")
                 (refY . "5")
                 (orient . "auto")
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
                             `((d . "M10,1.5 10,8.5 2.5,5"))
                           `((d . "M0,1.5 0,8.5 7.0,5"))))))
    ("circle"
     (dom-node 'marker
               `((markerWidth . "4")
                 (markerHeight . "4")
                 (viewBox . "0 0 10 10")
                 (refX . "5")
                 (refY . "5")
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
        (dom-set-attribute element
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


(defun edraw-svg-text-aabb (element)
  "Return the axis-aligned bounding box of the text ELEMENT."
  ;; https://www.w3.org/TR/SVG11/text.html#TextElement
  ;; @todo support inherit attribute from ancestor
  ;; @todo support transform
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
      ;;(message "x=%s y=%s text-x=%s text-y=%s text-w=%s text-h=%s text-anchor=%s" (car xs) (car ys) text-x text-y text-w text-h text-anchor)
      (edraw-rect-xywh text-x text-y text-w text-h))))


;;;; SVG Path Data Parser

;; Path Data Syntax
;; https://www.w3.org/TR/SVG11/paths.html#PathDataBNF

(defconst edraw-svg-path-d-number
  "\\(?:[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\)")
(defconst edraw-svg-path-d-wsp "\\(?:[ \t\n\f\r]+\\)")
(defconst edraw-svg-path-d-wsp-opt "[ \t\n\f\r]*")
(defconst edraw-svg-path-d-comma-wsp "\\(?:[ \t\n\f\r]+,?[ \t\n\f\r]*\\|,[ \t\n\f\r]*\\)")
(defconst edraw-svg-path-d-command
  (concat
   edraw-svg-path-d-wsp-opt
   "\\([A-Z]\\)" ;; (1) command type
   "\\(?:" edraw-svg-path-d-wsp-opt
   "\\(" edraw-svg-path-d-number ;;(2) command arguments
   "\\(?:" edraw-svg-path-d-comma-wsp edraw-svg-path-d-number "\\)*\\)" "\\)?"
   edraw-svg-path-d-wsp "?"))

(defun edraw-svg-path-d-parse (d)
  "Return (type . list of number)"
  (let ((pos 0)
        commands)
    (while (string-match edraw-svg-path-d-command d pos)
      (when (/= (match-beginning 0) pos)
        (error "path data parsing error at %s" (substring d pos)))
      (setq pos (match-end 0))
      (let* ((type (intern (match-string 1 d)))
             (numbers-str (match-string 2 d))
             (numbers (if numbers-str
                          (mapcar #'string-to-number
                                  (split-string numbers-str
                                                edraw-svg-path-d-comma-wsp)))))
        (push (cons type numbers) commands)))
    (nreverse commands)))
;; TEST: (edraw-svg-path-d-parse "Z M 10 20.1 L .1 2e+1 20e1 -5e-1") => '((Z) (M 10 20.1) (L 0.1 20.0 200.0 -0.5))
;; TEST: (edraw-svg-path-d-parse "ZM10 20.1L.1 2e+1 20e1 -5e-1") => '((Z) (M 10 20.1) (L 0.1 20.0 200.0 -0.5))

(defun edraw-svg-path-d-from-command-list (command-list)
  (mapconcat (lambda (command)
               (mapconcat (lambda (arg) (format "%s" arg)) command " "))
             command-list
             " "))
;; TEST: (edraw-svg-path-d-from-command-list '((Z) (M 10 20.1) (L 0.1 20.0 200.0 -0.5)))

(defun edraw-svg-path-d-translate (d xy)
  (let ((x (car xy))
        (y (cdr xy)))

    (edraw-svg-path-d-from-command-list
     (cl-loop for cmd in (edraw-svg-path-d-parse d)
              collect (let ((type (car cmd))
                            (args (cdr cmd)))
                        (cons
                         type
                         (pcase type
                           ((or 'M 'L 'C 'S 'Q 'T)
                            (seq-map-indexed (lambda (n idx)
                                               (+ n (if (= (% idx 2) 0) x y)))
                                             args))
                           ;;(('H))
                           ;;(('V))
                           ;;(('A))
                           (_ args))))))))
;; TEST: (edraw-svg-path-d-translate '"M 10 20 L 30 40 50 60" '(100 . 200))


;;;; SVG Shapes Translation

;;
;;

(defun edraw-svg-element-translate (element xy)
  (pcase (dom-tag element)
    ('rect (edraw-svg-rect-translate element xy))
    ('ellipse (edraw-svg-ellipse-translate element xy))
    ('circle (edraw-svg-circle-translate element xy))
    ('text (edraw-svg-text-translate element xy))
    ('path (edraw-svg-path-translate element xy)))
  element)

(defun edraw-svg-rect-translate (element xy)
  (dom-set-attribute element 'x (+ (or (edraw-svg-attr-coord element 'x) 0)
                                   (car xy)))
  (dom-set-attribute element 'y (+ (or (edraw-svg-attr-coord element 'y) 0)
                                   (cdr xy))))

(defun edraw-svg-ellipse-translate (element xy)
  (dom-set-attribute element 'cx (+ (or (edraw-svg-attr-coord element 'cx) 0)
                                   (car xy)))
  (dom-set-attribute element 'cy (+ (or (edraw-svg-attr-coord element 'cy) 0)
                                   (cdr xy))))

(defun edraw-svg-circle-translate (element xy)
  (dom-set-attribute element 'cx (+ (or (edraw-svg-attr-coord element 'cx) 0)
                                   (car xy)))
  (dom-set-attribute element 'cy (+ (or (edraw-svg-attr-coord element 'cy) 0)
                                   (cdr xy))))

(defun edraw-svg-text-translate (element xy)
  ;;@todo support list-of-coordinates
  (edraw-svg-text-set-x element (+ (or (edraw-svg-attr-coord element 'x) 0)
                                   (car xy)))
  (dom-set-attribute element 'y (+ (or (edraw-svg-attr-coord element 'y) 0)
                                   (cdr xy))))

(defun edraw-svg-path-translate (element xy)
  (when-let ((d (dom-attr element 'd)))
    (dom-set-attribute element 'd (edraw-svg-path-d-translate d xy))))



;;;; Point in SVG Shapes Test

;;
;; Depends on edraw-path.el (edraw-path-*, edraw-bezier)
;;

(defconst edraw-pick-point-radius 2)

(defun edraw-svg-element-contains-point-p (element xy)
  (when (edraw-dom-element-p element)
    (pcase (dom-tag element)
      ('path (edraw-svg-path-contains-point-p element xy))
      ('rect (edraw-svg-rect-contains-point-p element xy))
      ('ellipse (edraw-svg-ellipse-contains-point-p element xy))
      ('circle (edraw-svg-circle-contains-point-p element xy))
      ('text (edraw-svg-text-contains-point-p element xy)))))

(defun edraw-svg-path-contains-point-p (element xy)
  (let* ((fill (dom-attr element 'fill))
         (stroke-width (or (edraw-svg-attr-number element 'stroke-width) 1))
         (stroke-square-r (/ stroke-width (* 2 (sqrt 2))))
         (d (dom-attr element 'd))
         (fill-rule (dom-attr element 'fill-rule)))

    (when d
      (if (and fill (equal fill "none"))
          (edraw-path-cmdlist-intersects-rect-p
           (edraw-path-cmdlist-from-d d)
           (edraw-square xy
                         (+ edraw-pick-point-radius stroke-square-r)))
        (edraw-path-cmdlist-contains-point-p
         (edraw-path-cmdlist-from-d d)
         xy
         (equal fill-rule "evenodd"))))))

(defun edraw-svg-rect-contains-point-p (element xy)
  ;; https://www.w3.org/TR/SVG11/shapes.html#RectElement
  (let* ((fill (dom-attr element 'fill))
         (left   (or (edraw-svg-attr-coord element 'x) 0))
         (top    (or (edraw-svg-attr-coord element 'y) 0))
         (width  (or (edraw-svg-attr-coord element 'width) 0))
         (height (or (edraw-svg-attr-coord element 'height) 0)))
    (let* ((right  (+ left width))
           (bottom (+ top height))
           ;;@todo support rx, ry
           ;;@todo create edraw-bezier-segments-from-rect function
           (segments (list (vector (cons left  top   ) (cons right top   ))
                           (vector (cons right top   ) (cons right bottom))
                           (vector (cons right bottom) (cons left  bottom))
                           (vector (cons left  bottom) (cons left  top)))))
      (if (and fill (equal fill "none"))
          (edraw-bezier-segments-intersects-rect-p
           segments
           (edraw-square xy edraw-pick-point-radius))
        (edraw-bezier-segments-contains-point-p
         segments
         xy)))))

(defun edraw-svg-ellipse-contains-point-p (element xy)
  ;; https://www.w3.org/TR/SVG11/shapes.html#EllipseElement
  (let* ((fill (dom-attr element 'fill))
         (cx (or (edraw-svg-attr-coord element 'cx) 0))
         (cy (or (edraw-svg-attr-coord element 'cy) 0))
         (rx (or (edraw-svg-attr-coord element 'rx) 0))
         (ry (or (edraw-svg-attr-coord element 'ry) 0)))
    (let* ((left   (- cx rx))
           (top    (- cy ry))
           (right  (+ cx rx))
           (bottom (+ cy ry))
           (c 0.551915024494) ;;https://spencermortensen.com/articles/bezier-circle/
           (crx (* c rx))
           (cry (* c ry))
           ;;@todo create edraw-bezier-segments-from-ellipse function
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
      (if (and fill (equal fill "none"))
          (edraw-bezier-segments-intersects-rect-p
           segments
           (edraw-square xy edraw-pick-point-radius))
        (edraw-bezier-segments-contains-point-p
         segments
         xy)))))

(defun edraw-svg-circle-contains-point-p (element xy)
  ;; https://www.w3.org/TR/SVG11/shapes.html#CircleElement
  (let* ((fill (dom-attr element 'fill))
         (cx (or (edraw-svg-attr-coord element 'cx) 0))
         (cy (or (edraw-svg-attr-coord element 'cy) 0))
         (r (or (edraw-svg-attr-coord element 'r) 0)))
    (let* ((left   (- cx r))
           (top    (- cy r))
           (right  (+ cx r))
           (bottom (+ cy r))
           (c 0.551915024494) ;;https://spencermortensen.com/articles/bezier-circle/
           (cr (* c r))
           ;;@todo create edraw-bezier-segments-from-circle function
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
      (if (and fill (equal fill "none"))
          (edraw-bezier-segments-intersects-rect-p
           segments
           (edraw-square xy edraw-pick-point-radius))
        (edraw-bezier-segments-contains-point-p
         segments
         xy)))))

(defun edraw-svg-text-contains-point-p (element xy)
  (edraw-rect-contains-point-p (edraw-svg-text-aabb element) xy))



(provide 'edraw-dom-svg)
;;; edraw-dom-svg.el ends here
