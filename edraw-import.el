;;; edraw-import.el --- Convert to Edraw SVG  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

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

;; 

;;; Code:

(require 'edraw-dom-svg)

;;;; Common

;;;###autoload
(defun edraw-convert-buffer-to-edraw-svg-xml (buffer output)
  (interactive
   (list (current-buffer) (current-buffer)))

  ;; @todo Select the importer that matches the buffer mode or file extension.

  (when (and
         (called-interactively-p 'interactive)
         (not (y-or-n-p
               (edraw-msg "If you import diagrams generated with other software into Edraw, they may not be displayed correctly or the editing operation may become unstable. The original information is lost in the converted data. Do you want to convert to a format for Edraw?"))))
    (keyboard-quit))

  (when-let ((svg (edraw-import-from-svg-buffer buffer)))
    (with-current-buffer output
      (erase-buffer)
      (edraw-svg-print svg nil nil))))

(defun edraw-import-error (string &rest args)
  (apply #'error string args))

(defun edraw-import-warn (string &rest args)
  (apply #'warn string args))

;;;; Import From General SVG

(defconst edraw-import-svg-level 'strict) ;; 'loose

;;;###autoload
(defun edraw-import-from-svg-buffer (buffer)
  (edraw-import-from-svg-dom
   (with-current-buffer buffer
     (libxml-parse-xml-region (point-min) (point-max)))))

;;;###autoload
(defun edraw-import-from-svg-dom (dom)
  (let* ((svg-comments (edraw-dom-split-top-nodes dom))
         (svg (car svg-comments)))

    (unless (eq (edraw-dom-tag svg) 'svg)
      (edraw-import-error (edraw-msg "Not SVG data")))

    (if (edraw-dom-get-by-id svg "edraw-body")
        (progn
          (edraw-import-warn (edraw-msg "Already an SVG for Edraw"))
          dom)

      ;; `edraw-svg-attr-length' requires ability to get parent from
      ;; child element only.
      (edraw-dom-update-parent-links svg)

      (let* ((edraw-dom-inhibit-parent-links t) ;; Do not create parent links(?)
             (context '(nil))
             (converted (edraw-import-svg-convert-children svg context))
             (definitions (plist-get (car context) :definitions)))
        (edraw-dom-element
         'svg
         :attributes
         (edraw-import-svg-convert-element-attributes svg context)
         ;; Children
         (when definitions
           (edraw-dom-element 'g
                              :id "edraw-imported-definitions"
                              :children definitions))
         (edraw-dom-element 'g
                            :id "edraw-body"
                            :children converted))))))

;;;;; Convert Children

(defun edraw-import-svg-convert-children (parent context)
  (let (new-children)
    (dolist (node (dom-children parent))
      (if (edraw-dom-element-p node)
          (if-let ((new-node (edraw-import-svg-convert-element node context)))
              (if (and (consp new-node)
                       (consp (car new-node)))
                  ;; List of node
                  (dolist (node new-node)
                    (push node new-children))
                ;; Single node (element or text)
                (push new-node new-children))
            ;; Discard element
            nil)
        (push node new-children)))
    (nreverse new-children)))

;;;;; Convert Element

(defvar edraw-import-svg-convert-element-alist
  '((defs . edraw-import-svg-convert-definition)
    (g . edraw-import-svg-convert-group)
    (rect . edraw-import-svg-convert-shape)
    (ellipse . edraw-import-svg-convert-shape)
    (path . edraw-import-svg-convert-shape)
    (image . edraw-import-svg-convert-shape)
    (circle . edraw-import-svg-convert-circle)
    (line . edraw-import-svg-convert-line)
    (polyline . edraw-import-svg-convert-poly-shape)
    (polygon . edraw-import-svg-convert-poly-shape)
    (text . edraw-import-svg-convert-text)
    (comment . edraw-import-svg-convert-comment)
    (a . edraw-import-svg-convert-a)
    ;; Not supported:
    ;; animate
    ;; animateMotion
    ;; animateTransform
    ;; clipPath
    ;; desc
    ;; fe*
    ;; filter
    ;; foreignObject
    ;; linearGradient
    ;; marker
    ;; mask
    ;; metadata
    ;; mpath
    ;; pattern
    ;; radialGradient
    ;; script
    ;; set
    ;; stop
    ;; style
    ;; svg
    ;; switch
    ;; symbol
    ;; textPath
    ;; use
    ;; view
    ))

(defun edraw-import-svg-convert-element (elem context)
  (if-let ((fun (alist-get (edraw-dom-tag elem)
                           edraw-import-svg-convert-element-alist)))
      (funcall fun elem context)
    (if (eq edraw-import-svg-level 'loose)
        (progn
          (edraw-import-warn (edraw-msg "Keep unsupported element: %s")
                             (edraw-dom-tag elem))
          (edraw-import-svg-convert-shape elem context))
      (edraw-import-warn (edraw-msg "Discard unsupported element: %s")
                         (edraw-dom-tag elem))
      nil)))

(defun edraw-import-svg-convert-comment (_elem _context)
  ;; Discard comment
  nil)

(defun edraw-import-svg-convert-a (elem context)
  ;; Expose contents
  ;; @todo check style?
  (edraw-import-svg-convert-children elem context))

(defun edraw-import-svg-convert-definition (elem context)
  ;; @todo Clone ELEM?
  (push elem (plist-get (car context) :definitions))
  nil)

(defun edraw-import-svg-convert-group (elem context)
  (edraw-dom-element
   (edraw-dom-tag elem)
   :attributes (edraw-import-svg-convert-element-attributes elem context)
   :children (edraw-import-svg-convert-children elem context)))

(defun edraw-import-svg-convert-shape (elem context)
  (edraw-dom-element
   (edraw-dom-tag elem)
   :attributes (edraw-import-svg-convert-element-attributes elem context)))

(defun edraw-import-svg-convert-circle (elem context)
  (let ((r (dom-attr elem 'r)))
    (when r
      (edraw-dom-element
       'ellipse
       :rx r
       :ry r
       :attributes (edraw-import-svg-convert-element-attributes
                    elem context '(r))))))

(defun edraw-import-svg-convert-line (elem context)
  (let ((x1 (edraw-svg-attr-length elem 'x1))
        (y1 (edraw-svg-attr-length elem 'y1))
        (x2 (edraw-svg-attr-length elem 'x2))
        (y2 (edraw-svg-attr-length elem 'y2)))
    (when (and x1 y1 x2 y2)
      (edraw-dom-element
       'path
       :d (edraw-path-d-from-command-list `((M ,x1 ,y1 ,x2 ,y2)))
       :attributes (edraw-import-svg-convert-element-attributes
                    elem context '(x1 y1 x2 y2))))))

(defun edraw-import-svg-convert-poly-shape (elem context) ;; polyline or polygon
  (let* ((closepath (eq (edraw-dom-tag elem) 'polygon))
         (points-attr (dom-attr elem 'points))
         (points (edraw-svg-parse-points (or points-attr ""))))
    (cond
     ((null points-attr)
      (edraw-import-warn (edraw-msg "No `points' attribute: %s")
                         (edraw-dom-tag elem))
      nil)
     ((null points)
      (edraw-import-warn (edraw-msg "Empty `points' attribute: %s")
                         (edraw-dom-tag elem))
      nil)
     (t
      (edraw-dom-element
       'path
       :d (edraw-path-d-from-command-list
           (nconc
            (list (cons 'M (cl-loop for (x . y) in points
                                    collect x collect y)))
            (when closepath (list (list 'Z)))))
       :attributes (edraw-import-svg-convert-element-attributes
                    elem context '(points)))))))

(defun edraw-import-svg-convert-text (elem context)
  ;;@todo impl
  (edraw-dom-element
   (edraw-dom-tag elem)
   :attributes (edraw-import-svg-convert-element-attributes elem context)
   :children (edraw-import-svg-convert-children elem context)))


;;;;; Convert Attribute

(defun edraw-import-svg-convert-element-attributes (elem context &optional
                                                         exclude-attrs)
  (edraw-import-svg-convert-attributes
   (if exclude-attrs
       (cl-loop for kv in (edraw-dom-attributes elem)
                unless (memq (car kv) exclude-attrs)
                collect kv)
     (edraw-dom-attributes elem))
   elem
   context))

(defun edraw-import-svg-convert-attributes (attributes elem context)
  (cl-loop for (k . v) in attributes
           for new-attr = (edraw-import-svg-convert-attribute k v elem context)
           when new-attr
           collect new-attr))

(defvar edraw-import-svg-convert-attributes-alist
  ;; https://developer.mozilla.org/ja/docs/Web/SVG/Attribute
  '((class . edraw-import-svg-convert-attr-keep)
    (id . edraw-import-svg-convert-attr-keep)
    (opacity . edraw-import-svg-convert-attr-keep)
    (fill . edraw-import-svg-convert-attr-keep)
    (fill-opacity . edraw-import-svg-convert-attr-keep)
    (fill-rule . edraw-import-svg-convert-attr-keep)
    (stroke . edraw-import-svg-convert-attr-keep)
    (stroke-opacity . edraw-import-svg-convert-attr-keep)
    (stroke-width . edraw-import-svg-convert-attr-keep)
    (stroke-dasharray . edraw-import-svg-convert-attr-keep)
    (stroke-dashoffset . edraw-import-svg-convert-attr-keep)
    (stroke-linecap . edraw-import-svg-convert-attr-keep)
    (stroke-linejoin . edraw-import-svg-convert-attr-keep)
    (stroke-miterlimit . edraw-import-svg-convert-attr-keep)

    (style . edraw-import-svg-convert-attr-keep)
    (transform . edraw-import-svg-convert-attr-keep)
    ;; geometry
    (x . edraw-import-svg-convert-attr-keep) ;;@todo check text's x
    (y . edraw-import-svg-convert-attr-keep) ;;@todo check text's y
    (cx . edraw-import-svg-convert-attr-keep)
    (cy . edraw-import-svg-convert-attr-keep)
    (width . edraw-import-svg-convert-attr-keep)
    (height . edraw-import-svg-convert-attr-keep)
    (r . edraw-import-svg-convert-attr-keep)
    (rx . edraw-import-svg-convert-attr-keep)
    (ry . edraw-import-svg-convert-attr-keep)
    ;; path
    (d . edraw-import-svg-convert-attr-d)
    (marker-start . edraw-import-svg-convert-attr-keep) ;;@todo check
    (marker-mid . edraw-import-svg-convert-attr-keep) ;;@todo check
    (marker-end . edraw-import-svg-convert-attr-keep) ;;@todo check
    ;; text
    (dx . edraw-import-svg-convert-attr-keep)
    (dy . edraw-import-svg-convert-attr-keep)
    (font-family . edraw-import-svg-convert-attr-keep)
    (font-size . edraw-import-svg-convert-attr-keep)
    (font-weight . edraw-import-svg-convert-attr-keep)
    (font-style . edraw-import-svg-convert-attr-keep)
    (text-decoration . edraw-import-svg-convert-attr-keep)
    (text-anchor . edraw-import-svg-convert-attr-keep)
    (writing-mode . edraw-import-svg-convert-attr-keep)
    ;; image
    (xlink:href . edraw-import-svg-convert-attr-keep)
    (href . edraw-import-svg-convert-attr-keep)
    (preserveAspectRatio . edraw-import-svg-convert-attr-keep) ;; svg or image
    ;; svg
    (viewBox . edraw-import-svg-convert-attr-keep)
    (version . edraw-import-svg-convert-attr-keep)
    (xmlns . edraw-import-svg-convert-attr-keep)
    (xmlns:xlink . edraw-import-svg-convert-attr-keep)))

(defun edraw-import-svg-convert-attribute (attr-name value elem context)
  (let ((fun (alist-get attr-name edraw-import-svg-convert-attributes-alist)))
    (cond
     (fun
      (funcall fun attr-name value elem context))
     ;; Ignore internal attribute
     ((edraw-dom-attr-internal-p attr-name)
      nil)
     ;; Keep data attribute
     ((string-prefix-p "data-" (symbol-name attr-name))
      (cons attr-name value))
     ;; Unsupported attribute
     (t
      (if (eq edraw-import-svg-level 'loose)
          (progn
            (edraw-import-warn (edraw-msg "Keep unsupported attribute: %s")
                               attr-name)
            (cons attr-name value))
        (edraw-import-warn (edraw-msg "Discard unsupported attribute: %s")
                           attr-name)
        nil)))))

(defun edraw-import-svg-convert-attr-keep (attr-name value _elem _context)
  (cons attr-name value))


(defun edraw-import-svg-convert-attr-d (attr-name value _elem _context)
  ;;@todo impl
  (cons attr-name value))

(provide 'edraw-import)
;;; edraw-import.el ends here
