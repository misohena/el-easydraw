;;; edraw-org-export-html.el --- Export edraw link As HTML in Org  -*- lexical-binding: t; -*-

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

(require 'ox-html)
(require 'edraw-org-edit)

;;;; Customize

(define-obsolete-variable-alias 'edraw-org-link-export-data-tag
  'edraw-org-export-html-data-tag "2022-09-22")
(defcustom edraw-org-export-html-data-tag 'svg
  "HTML tag used to export data links.

img = Data URI scheme (<img src=\"data:image/svg+xml;base64,.....\">)
svg = Embed SVG element (<svg>...</svg>)
"
  :group 'edraw-org
  :type '(choice (const :tag "<img>" img)
                 (const :tag "<svg>" svg)
                 (function)))

(define-obsolete-variable-alias 'edraw-org-link-export-file-tag
  'edraw-org-export-html-file-tag "2022-09-22")
(defcustom edraw-org-export-html-file-tag 'img
  "HTML tag used to export file links.

img = Simple file link (<img src=\"<path-to-file>\">)
svg = Embed SVG element (<svg>...</svg>)
"
  :group 'edraw-org
  :type '(choice (const :tag "<img>" img)
                 (const :tag "<svg>" svg)
                 (function)))

(defcustom edraw-org-export-html-use-viewbox t
  "Add viewBox= attribute to svg root elements when SVG export."
  :group 'edraw-org
  :type '(boolean))

;;;; Export

(defun edraw-org-export-html-setup ()
  (with-eval-after-load 'ox-html
    (setf (alist-get edraw-org-link-type
                     org-html-inline-image-rules nil nil #'equal)
          ".*")))

(defun edraw-org-export-html-link (path _description _back-end info link)
  ;; path is unescaped : \[ \] => [ ]
  ;; description is not unescaped : \[ \] => \[ \]
  (require 'edraw)
  (if-let ((link-props (edraw-org-link-props-parse path nil t)))
      (let ((html-tag (edraw-org-link-prop-html-tag link-props)))
        (if-let ((data (edraw-org-link-prop-data link-props)))
            (pcase (or html-tag edraw-org-export-html-data-tag)
              ('svg (edraw-org-link-html-link-to-svg link-props link info))
              ('img (edraw-org-link-data-to-img data link info))
              ((and (pred functionp)
                    func)
               (funcall func data))
              (_ (edraw-org-link-html-link-to-svg link-props link info)))
          (if-let ((file (edraw-org-link-prop-file link-props)))
              (pcase (or html-tag edraw-org-export-html-file-tag)
                ('svg (edraw-org-link-html-link-to-svg link-props link info))
                ('img (edraw-org-link-file-to-img file link info))
                ((and (pred functionp)
                      func)
                 (funcall func file))
                (_ (edraw-org-link-file-to-img file link info)))
            "")))
    ""))

(defun edraw-org-link-data-to-data-uri (data)
  (with-temp-buffer
    (insert data)
    (base64-decode-region (point-min) (point-max))
    ;; Web browsers don't support svgz
    (when (edraw-buffer-gzip-p)
      (edraw-gunzip-buffer)
      (encode-coding-region (point-min) (point-max) 'utf-8))
    (base64-encode-region (point-min) (point-max) t)
    (goto-char (point-min))
    (insert "data:image/svg+xml;base64,")
    (buffer-substring-no-properties (point-min) (point-max))))

(defun edraw-org-link-data-to-img (data link info)
  (edraw-org-link-html-img (edraw-org-link-data-to-data-uri data) link info))

(defun edraw-org-link-file-to-img (file link info)
  (edraw-org-link-html-img file link info))

(defun edraw-org-link-html-img (src link info)
  (org-html-close-tag
   "img"
   (org-html--make-attribute-string
    (org-combine-plists
     (list :src src) ;;@todo alt
     (edraw-org-link-html-attributes-plist link info)))
   info))

(defun edraw-org-link-html-link-to-svg (link-props link info)
  (let ((svg (edraw-org-link-load-svg link-props))
        (attributes (edraw-org-link-html-attributes-plist link info))
        (link-ref (org-export-get-reference link info)))
    (unless svg
      (message "Failed to load SVG %s" (prin1-to-string link-props)))

    ;; Set svg attributes, replace ids and return as string
    (edraw-encode-svg
     (edraw-org-link-html-convert-svg-for-embed-in-html svg attributes
                                                        link-ref)
     nil nil)))

(defun edraw-org-link-html-attributes-plist (link info)
  "Return attributes specified by #+ATTR_HTML as a plist."
  (when link
    ;; NOTE: The code below is a copy from org-html-link function.
    (org-combine-plists
     ;; Extract attributes from parent's paragraph.  HACK: Only
     ;; do this for the first link in parent (inner image link
     ;; for inline images).  This is needed as long as
     ;; attributes cannot be set on a per link basis.
     (let* ((parent (org-export-get-parent-element link))
            (link (let ((container (org-export-get-parent link)))
                    (if (and (eq 'link (org-element-type container))
                             (org-html-inline-image-p link info))
                        container
                      link))))
       (and (eq link (org-element-map parent 'link #'identity info t))
            (org-export-read-attribute :attr_html parent)))
     ;; Also add attributes from link itself.  Currently, those
     ;; need to be added programmatically before `org-html-link'
     ;; is invoked, for example, by backends building upon HTML
     ;; export.
     (org-export-read-attribute :attr_html link))))

(defun edraw-org-link-html-convert-svg-for-embed-in-html (svg
                                                          attributes
                                                          link-ref)
  "Convert SVG into a form that can be embedded in HTML.

Currently this function does three things:

If edraw-org-export-html-use-viewbox is non-nil, add a viewBox
attribute to the svg root element.

Sets the attribute specified by #+ATTR_HTML to the svg root element.

Guarantees the uniqueness of ids defined by the SVG in the
exported HTML. Add a random string to id."
  ;; Add viewBox= attribute
  (when edraw-org-export-html-use-viewbox
    (let ((width (dom-attr svg 'width))
          (height (dom-attr svg 'height)))
      (dom-set-attribute svg 'viewBox (format "%s %s %s %s" 0 0 width height))))

  ;; Apply attributes specified by #+ATTR_HTML to the root svg element
  (cl-loop for (key value) on attributes by #'cddr
           do (dom-set-attribute
               svg
               (cond
                ((keywordp key) (intern (substring (symbol-name key) 1)))
                ((stringp key) (intern key))
                (t key))
               value))

  ;; Replace all ids (Make ids unique in the HTML)
  ;; e.g.
  ;;  #edraw-body => #edraw-orgc4e2460-body
  ;;  #edraw-defs => #edraw-orgc4e2460-defs
  ;;  #edraw-def-0-arrow => #edraw-orgc4e2460-def-0-arrow

  (let* ((image-id link-ref)
         (id-converter (lambda (id)
                         (format "edraw-%s-%s"
                                 image-id
                                 (string-remove-prefix "edraw-" id)))))
    (edraw-org-link-html-replace-ids svg id-converter))
  svg)

(defun edraw-org-link-html-replace-ids (svg id-converter)
  (let (;; Replace id in definitions
        ;; and create id conversion table.
        (id-map
         (delq nil
               (mapcar
                (lambda (element)
                  (when-let ((old-id (dom-attr element 'id))
                             (new-id (funcall id-converter old-id)))
                    (dom-set-attribute element 'id new-id)
                    (cons old-id new-id)))
                ;; The target elements are:
                (append
                 (list
                  (edraw-dom-get-by-id svg "edraw-background")
                  (edraw-dom-get-by-id svg "edraw-body")
                  (edraw-dom-get-by-id svg "edraw-defs"))
                 (dom-children (edraw-dom-get-by-id svg "edraw-defs")))))))
    ;; Replace all references
    (edraw-org-link-html-replace-id-in-url-references svg id-map)))

(defun edraw-org-link-html-replace-id-in-url-references (element id-map)
  (when (edraw-dom-element-p element)
    (dolist (attr (dom-attributes element))
      (let ((key (car attr))
            (value (cdr attr)))
        ;;@todo should be limited to url data type attributes such as marker-start, marker-mid, marker-end
        (when (and (stringp value)
                   (string-match "\\` *url *( *#\\([^ )]+\\) *) *\\'" value))
          (when-let ((old-id (match-string 1 value))
                     (new-id (alist-get old-id id-map nil nil #'equal)))
            (dom-set-attribute element
                               key
                               (format "url(#%s)" new-id))))))
    ;; Children
    (dolist (child (dom-children element))
      (edraw-org-link-html-replace-id-in-url-references child id-map))))

(provide 'edraw-org-export-html)
;;; edraw-org-export-html.el ends here
