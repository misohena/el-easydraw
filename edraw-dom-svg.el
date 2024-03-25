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
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'edraw-math)
(require 'edraw-path)
(require 'edraw-util)

(defvar edraw-svg-version "1.1")

(defvar edraw-dom-inhibit-parent-links nil)

;;;; DOM Element Creation

(defun edraw-dom-element (tag &rest attr-plist-and-children)
  "Return a new DOM element with TAG and ATTR-PLIST-AND-CHILDREN.

ATTR-PLIST-AND-CHILDREN specifies the attributes and children of
the new element. For example:

  (edraw-dom-element
    \\='div
    :class \"some-div\"
    (edraw-dom-element \\='p \"Paragraph 1.\")
    (edraw-dom-element \\='p \"Paragraph 2.\"))

Attributes are specified in a property list starting at the
beginning of ATTR-PLIST-AND-CHILDREN. A property list key must be
a symbol (and non-nil). If the symbol is a keyword, the leading
colon is ignored (i.e. :x and \\='x are the same).

If a non-symbol (or nil) appears at the position where the key
symbol of the property list should appear, the subsequent
elements are treated as children. Children that are nil are
automatically removed.

The following special properties can be specified.

:parent      Parent DOM element. Can be specified only once.
:children    A list of child DOM nodes. Can be specified multiple.
:attributes  A plist or alist of additional attributes. Can be specified
            multiple.

Commonly used SVG elements have their own creation functions:

- `edraw-svg-create'
- `edraw-svg-rect'
- `edraw-svg-circle'
- `edraw-svg-ellipse'
- `edraw-svg-line'
- `edraw-svg-path'
- `edraw-svg-polygon'
- `edraw-svg-polyline'
- `edraw-svg-group'

These functions can specify the same arguments as this function
in the rest argument."
  (let ((rest attr-plist-and-children)
        parent
        children
        attr-alist)
    ;; Split ATTR-PLIST-AND-CHILDREN into ATTR-ALIST and CHILDREN.
    (while (and rest
                (car rest) ;; Nil means invalid child
                (symbolp (car rest)))
      (let* ((key (car rest))
             (value (cadr rest))
             (attr-name (edraw-dom-element--strip-colon key)))
        (pcase attr-name
          ;; Support :parent <parent> notation.
          ('parent
           (setq parent value))
          ;; Support :children (<child> ...) notation.
          ('children
           (setq children (nconc children (delq nil (copy-sequence value)))))
          ;; Support :attributes (<key> <value> ...) or ((<key> . <value>) ...)
          ('attributes
           (when (consp value)
             (cond
              ((consp (car value)) ;; alist
               (cl-loop for (k . v) in value
                        when (and k (symbolp k))
                        do (push (cons (edraw-dom-element--strip-colon k) v)
                                 attr-alist)))
              ((plistp value) ;; plist
               (cl-loop for (k v) on value by #'cddr
                        when (and k (symbolp k))
                        do (push (cons (edraw-dom-element--strip-colon k) v)
                                 attr-alist))))))
          (_
           (push (cons attr-name value) attr-alist)))
        (setq rest (cddr rest))))

    (setq attr-alist (nreverse attr-alist))
    (setq children (nconc children (delq nil (copy-sequence rest))))

    ;; Create an element
    (let ((element (apply #'dom-node tag attr-alist children)))
      ;; Set ELEMENT as parent for children
      (unless edraw-dom-inhibit-parent-links
        (dolist (child children)
          (edraw-dom-set-parent-auto child element)))
      ;; Append the element to parent
      (when parent
        (edraw-dom-append-child parent element))
      element)))
;; TEST: (edraw-dom-element 'rect :x 1 :y 2 :width 3 :height 4) => (rect ((x . 1) (y . 2) (width . 3) (height . 4)))
;; TEST: (edraw-dom-element 'rect :x 1 :attributes '(:y 2 :width 3) :height 4) => (rect ((x . 1) (y . 2) (width . 3) (height . 4)))
;; TEST: (edraw-dom-element 'rect :x 1 :attributes '(:y 2 :width 3 nil 10) :attributes '((:height . 4) (nil . 11)) nil) => (rect ((x . 1) (y . 2) (width . 3) (height . 4)))
;; TEST: (let ((edraw-dom-inhibit-parent-links t)) (edraw-dom-element 'g :stroke "red" :children (list (edraw-dom-element 'rect :x 11 :y 22 :width 33 :height 44) nil) (edraw-dom-element 'rect :x 111 :y 222 :width 333 :height 444) nil)) => (g ((stroke . "red")) (rect ((x . 11) (y . 22) (width . 33) (height . 44))) (rect ((x . 111) (y . 222) (width . 333) (height . 444))))
;; TEST: (let ((edraw-dom-inhibit-parent-links t) (g (dom-node 'g))) (edraw-dom-element 'rect :parent g :x 11 :y 22 :width 33 :height 44) g) => (g nil (rect ((x . 11) (y . 22) (width . 33) (height . 44))))

(defun edraw-dom-element--strip-colon (key)
  (cond
   ((keywordp key) (intern (substring (symbol-name key) 1)))
   ((symbolp key) key)))

(defun edraw-dom-copy-tree (node)
  "Duplicate the DOM tree NODE.

Attribute keys and values, and text node strings are shared
before and after copying.

Each element in the cloned tree has no link to its parent
element. Call `edraw-dom-update-parent-links' explicitly if necessary.

Attributes for internal use are not duplicated.
Whether it is for internal use is determined by `edraw-dom-attr-internal-p'."
  (if (and (consp node)
           (symbolp (car node)))
      (let* ((tag (dom-tag node))
             (attributes (cl-loop for (key . value) in (dom-attributes node)
                                  unless (edraw-dom-attr-internal-p key)
                                  collect (cons key value)))
             (children (cl-loop for child in (dom-children node)
                                collect (edraw-dom-copy-tree child))))
        (apply #'dom-node tag attributes children)
        ;; Do not call `edraw-dom-set-parent' and
        ;; (edraw-dom-element tag :attributes attributes :children children)
        )
    node))

;;;; DOM Element Accessors

(defun edraw-dom-element-p (node)
  (and node
       (listp node)
       (not (null (car node)))
       (symbolp (car node))))

(defmacro edraw-dom-tag (node)
  "Return the NODE tag.
Unlike `dom-tag', this function doesn't consider NODE if is's a
list of nodes.
Since this is a macro, setf can be used."
  ;; depends on dom.el node structure
  `(car-safe ,node))

(defmacro edraw-dom-attributes (node)
  "Return the NODE attribute list.
Unlike `dom-attributes', this function doesn't consider NODE if
is's a list of nodes.
Since this is a macro, setf can be used."
  ;; depends on dom.el node structure
  `(cadr ,node))

(defmacro edraw-dom-children (node)
  "Return the NODE child list.
Unlike `dom-children', this function doesn't consider NODE if
is's a list of nodes.
Since this is a macro, setf can be used."
  ;; depends on dom.el node structure
  `(cddr ,node))

(defun edraw-dom-tag-eq (node tag)
  (eq (edraw-dom-tag node) tag))

;;;; DOM Search

(defun edraw-dom-get-by-id (parent id)
  (car (dom-by-id parent (concat "\\`" (regexp-quote id) "\\'"))))

(defun edraw-dom-get-or-create (parent tag id)
  (or
   (edraw-dom-get-by-id parent id)
   (edraw-dom-element tag :id id :parent parent)))

;;;; DOM Comparison

;; Note: It is inappropriate to use `equal' to compare DOM nodes. In
;; particular, if there is an internal attribute such as a link to a
;; parent node, `equal' cannot be used to compare correctly. Also,
;; even though the contents of the attributes are the same, it is
;; possible that only the order of the attributes is different.

(defun edraw-dom-equal (node1 node2 &optional
                              attrs-to-exclude-for-top-nodes
                              attrs-to-exclude-for-children
                              without-children)
  (if (edraw-dom-element-p node1)
      (if (edraw-dom-element-p node2)
          ;; Element
          (and (eq (edraw-dom-tag node1) (edraw-dom-tag node2))
               (edraw-dom-equal-attributes (edraw-dom-attributes node1)
                                           (edraw-dom-attributes node2)
                                           attrs-to-exclude-for-top-nodes)
               (or without-children
                   (edraw-dom-equal-children node1 node2
                                             attrs-to-exclude-for-children)))
        nil)
    (if (edraw-dom-element-p node2)
        nil
      ;; Text node
      (equal node1 node2))))

(defun edraw-dom-equal-children (node1 node2 &optional attrs-to-exclude)
  (when (and (edraw-dom-element-p node1) (edraw-dom-element-p node2))
    (edraw-dom-equal-node-list (edraw-dom-children node1)
                               (edraw-dom-children node2)
                               attrs-to-exclude)))

(defun edraw-dom-equal-node-list (nodes1 nodes2 &optional attrs-to-exclude)
  (when (= (length nodes1) (length nodes2))
    (while (and nodes1
                nodes2
                (edraw-dom-equal (car nodes1) (car nodes2) attrs-to-exclude))
      (setq nodes1 (cdr nodes1)
            nodes2 (cdr nodes2)))
    (and (null nodes1)
         (null nodes2))))

(defun edraw-dom-equal-attributes (attrs1 attrs2 &optional attrs-to-exclude)
  (seq-set-equal-p
   (seq-remove (lambda (attr) (or (edraw-dom-attr-internal-p (car attr))
                                  (memq (car attr) attrs-to-exclude)))
               attrs1)
   (seq-remove (lambda (attr) (or (edraw-dom-attr-internal-p (car attr))
                                  (memq (car attr) attrs-to-exclude)))
               attrs2)
   #'equal))

;;;; DOM Parent Tracking

(defun edraw-dom-set-parent-auto (node parent)
  (unless edraw-dom-inhibit-parent-links
    (edraw-dom-set-parent node parent))
  node)

(defun edraw-dom-set-parent (node parent)
  (when (edraw-dom-element-p node)
    ;; :-edraw-dom-parent is an attribute for internal use.
    ;; (See: `edraw-dom-attr-internal-p')
    (dom-set-attribute node :-edraw-dom-parent parent))
  node)

(defun edraw-dom-get-parent (node)
  (when (edraw-dom-element-p node)
    (dom-attr node :-edraw-dom-parent)))

(defun edraw-dom-reset-parent (node)
  (when (edraw-dom-element-p node)
    (edraw-dom-remove-attr node :-edraw-dom-parent)))

(defun edraw-dom-update-parent-links (tree)
  "Make it possible to retrieve parents of all elements in TREE."
  (when (edraw-dom-element-p tree)
    (dolist (child (dom-children tree))
      (edraw-dom-set-parent child tree)
      (edraw-dom-update-parent-links child))))

(defun edraw-dom-remove-parent-links (tree)
  "Remove links to parent from all nodes in TREE."
  (edraw-dom-reset-parent tree)
  (when (edraw-dom-element-p tree)
    (dolist (child (dom-children tree))
      (edraw-dom-remove-parent-links child))))

(defun edraw-dom-get-root (node)
  (let (parent)
    (while (setq parent (edraw-dom-get-parent node))
      (setq node parent))
    node))

(defun edraw-dom-get-ancestor-by-tag (node tag)
  (let (parent)
    (while (and (setq parent (edraw-dom-get-parent node))
                (not (eq (dom-tag parent) tag)))
      (setq node parent))
    parent))

(defun edraw-dom-parent (dom node)
  "Return the parent of NODE in DOM.

Same as `dom-parent', but if NODE has the parent node information
set by `dom-set-parent', this function will skip searching from
the DOM and quickly identify the parent."
  (let ((parent (edraw-dom-get-parent node)))
    (if (and parent (memq node (edraw-dom-children parent)))
        parent
      (dom-parent dom node))))

;;;; DOM Removing

(defun edraw-dom-remove-node (dom node)
  (prog1 (dom-remove-node dom node)
    ;; @todo Should check to see if it has really been removed.
    (edraw-dom-reset-parent node)))

(defun edraw-dom-remove-all-children (node)
  (when (consp node)
    (dolist (child (dom-children node))
      (edraw-dom-reset-parent child))
    (setf (edraw-dom-children node) nil))
  node)

(defun edraw-dom-remove-by-id (dom id)
  (when-let ((node (edraw-dom-get-by-id dom id)))
    (edraw-dom-remove-node dom node)))

(defun edraw-dom-remove-attr (node attr)
  (dom-set-attributes node (assq-delete-all attr (dom-attributes node))))

(defun edraw-dom-remove-attr-if (node pred)
  (dom-set-attributes node (cl-delete-if pred (dom-attributes node))))

;;;; DOM Insertion

(defun edraw-dom-add-child-before (node child &optional before)
  (prog1 (dom-add-child-before node child before)
    (edraw-dom-set-parent-auto child node)))

(defun edraw-dom-append-child (node child)
  (prog1 (dom-append-child node child)
    (edraw-dom-set-parent-auto child node)))

(defun edraw-dom-insert-first (node child)
  (prog1 (dom-add-child-before node child)
    (edraw-dom-set-parent-auto child node)))

(defun edraw-dom-insert-nth (node child index)
  (setq node (dom-ensure-node node))
  ;; depends on dom.el node structure
  (if (<= index 0)
      (setcdr (cdr node) (cons child (cddr node)))
    (let ((cell (or (nthcdr (1- index) (cddr node))
                    (last (cddr node)))))
      (setcdr cell (cons child (cdr cell)))))
  (edraw-dom-set-parent-auto child node)
  child)

;;;; DOM Retrieve Siblings

(defun edraw-dom-first-child (node)
  (car (dom-children node)))

(defun edraw-dom-last-child (node)
  (car (last (dom-children node))))

(defun edraw-dom-next-sibling (dom node)
  (when-let ((parent (edraw-dom-parent dom node)))
    (let ((siblings (dom-children parent)))
      (while (and siblings
                  (not (eq (car siblings) node)))
        (setq siblings (cdr siblings)))
      (cadr siblings))))

(defun edraw-dom-previous-sibling (dom node)
  (when-let ((parent (edraw-dom-parent dom node)))
    (let ((siblings (dom-children parent)))
      (if (eq (car siblings) node)
          nil
        (while (and (cadr siblings)
                    (not (eq (cadr siblings) node)))
          (setq siblings (cdr siblings)))
        (car siblings)))))

;;;; DOM Ordering

(defun edraw-dom-first-node-p (dom node)
  (if-let ((parent (edraw-dom-parent dom node)))
      (eq (car (dom-children parent)) node)
    t))

(defun edraw-dom-last-node-p (dom node)
  (if-let ((parent (edraw-dom-parent dom node)))
      (eq (car (last (dom-children parent))) node)
    t))

(defun edraw-dom-reorder-prev (dom node)
  (when-let ((parent (edraw-dom-parent dom node)))
    (let ((index (seq-position (dom-children parent) node #'eq)))
      (when (> index 0)
        (let* ((prev-cell (nthcdr (1- index) (dom-children parent)))
               (prev-node (car prev-cell)))
          ;; depends on dom.el node structure
          (setcar prev-cell node)
          (setcar (cdr prev-cell) prev-node))
        t))))

(defun edraw-dom-reorder-next (dom node)
  (when-let ((parent (edraw-dom-parent dom node)))
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
  (when-let ((parent (edraw-dom-parent dom node)))
    (when (not (eq (car (dom-children parent)) node))
      ;; The parent of NODE does not change.
      (dom-remove-node parent node)
      (dom-add-child-before parent node (car (dom-children parent)))
      t)))

(defun edraw-dom-reorder-last (dom node)
  (when-let ((parent (edraw-dom-parent dom node)))
    (when (not (eq (car (last (dom-children parent))) node))
      ;; The parent of NODE does not change.
      (dom-remove-node parent node)
      (dom-append-child parent node)
      t)))

;;;; DOM Attributes

(defun edraw-dom-attr-internal-p (attr-name)
  "Return non-nil if the attribute's name ATTR-NAME is for internal use.

ATTR-NAME is a symbol or string.

Attribute names starting with a colon are for internal use."
  (cond
   ((symbolp attr-name) (keywordp attr-name))
   ((stringp attr-name) (and (not (string-empty-p attr-name))
                             (eq (aref attr-name 0) ?:)))))

(defun edraw-dom-remove-internal-attributes (node)
  (when (edraw-dom-element-p node)
    (edraw-dom-remove-attr-if node
                              (lambda (attr)
                                (edraw-dom-attr-internal-p (car attr)))))
  node)

(defun edraw-dom-remove-internal-attributes-from-tree (node)
  (edraw-dom-do
   node
   (lambda (node _ancestors)
     (edraw-dom-remove-internal-attributes node)))
  node)

(defun edraw-dom-set-attribute-name (node old-name new-name)
  "Rename OLD-NAME attribute in NODE to NEW-NAME if it exists.
If the attribute named OLD-NAME does not exist, do nothing.
Attribute value is preserved."
  (setq node (dom-ensure-node node))
  (let* ((attributes (cadr node))
         (old-cell (assoc old-name attributes)))
    (when old-cell
      (setcar old-cell new-name))))

;;;; DOM Mapping

(defun edraw-dom-do (node function &optional ancestors)
  (funcall function node ancestors)
  (when (edraw-dom-element-p node)
    (let ((ancestors (cons node ancestors))
          (children (dom-children node)))
      (cond
       ((listp children)
        (dolist (child-node children)
          (edraw-dom-do child-node function ancestors)))
       ;; ;; Comment Node (comment nil "comment text")
       ;; ;; @todo Isn't it unnecessary?
       ;; ((stringp children)
       ;;  (funcall function children ancestors))
       ))))

;;;; DOM Top Level Handling

(defun edraw-dom-split-top-nodes (dom)
  "Split DOM into pre comment nodes, top-level element, and post
comment nodes.

Return (ROOT-ELEMENT . (PRE-COMMENTS . POST-COMMENTS)).

`libxml-parse-xml-region' returns an element with the tag top if
there are comments before or after root element. This function
splits the DOM into pre comment nodes, root element, and post
comment nodes."
  (if (edraw-dom-tag-eq dom 'top)
      ;; DOM contains comments directly below
      (let* ((top-nodes (dom-children dom))
             (p top-nodes)
             (pre-comments nil))
        (while (and p (edraw-dom-tag-eq (car p) 'comment))
          (push (car p) pre-comments)
          (setq p (cdr p)))

        (if p
            ;; (ROOT-ELEMENT . (PRE-COMMENTS . POST-COMMENTS))
            (cons (car p) (cons (nreverse pre-comments) (cdr p)))
          ;; No elements!
          (cons nil (cons top-nodes nil))))
    (cons dom nil)))

(defun edraw-dom-merge-top-nodes (root-element pre-comments post-comments)
  "Reverse operation of `edraw-dom-split-top-nodes'."
  ;;@todo If (edraw-dom-tag-eq root-element 'top)?
  (if (or pre-comments post-comments)
      (apply #'dom-node 'top nil
             (append pre-comments (list root-element) post-comments))
    root-element))


;;;; CSS

;;;;; Regexp

;; https://www.w3.org/TR/css-syntax-3/#token-diagrams
;; https://www.w3.org/TR/CSS21/grammar.html
(defconst edraw-css-re-comment "\\(?:/\\*.*?\\*/\\)") ;; non-greedy
(defconst edraw-css-re-newline "\\(?:\r\n\\|[\n\r\f]\\)")
(defconst edraw-css-re-ws "\\(?:\r\n\\|[\n\r\f \t]\\)")
(defconst edraw-css-re-ws? (concat edraw-css-re-ws "?"))
(defconst edraw-css-re-ws* (concat edraw-css-re-ws "*"))
(defconst edraw-css-re-escape (concat
                               "\\(?:" "\\\\"
                               "\\(?:" "[^\n\r\f[:xdigit:]]" "\\|"
                               "[[:xdigit:]]\\{1,6\\}" edraw-css-re-ws?
                               "\\)" "\\)"))
(defconst edraw-css-re-nmstart (concat
                                "\\(?:[_a-zA-Z]\\|[[:nonascii:]]\\|"
                                edraw-css-re-escape "\\)"))
(defconst edraw-css-re-nmchar (concat
                               "\\(?:[-_a-zA-Z0-9]\\|[[:nonascii:]]\\|"
                               edraw-css-re-escape "\\)"))
(defconst edraw-css-re-ident (concat
                              "\\(?:--\\|-?" edraw-css-re-nmstart "\\)"
                              edraw-css-re-nmchar "*"))
(defconst edraw-css-re-function (concat edraw-css-re-ident "("))
(defconst edraw-css-re-at-keyword (concat "@" edraw-css-re-ident))
(defconst edraw-css-re-hash (concat "#" edraw-css-re-nmchar "+"))
(defconst edraw-css-re-string-escape (concat
                                      "\\(?:"
                                      edraw-css-re-escape "\\|"
                                      "\\\\" edraw-css-re-newline
                                      "\\)"))
(defconst edraw-css-re-string1 (concat
                                "\"" "\\(?:[^\n\r\f\"\\\\]\\|"
                                edraw-css-re-string-escape "\\)*" "\""))
(defconst edraw-css-re-string2 (concat
                                "'" "\\(?:[^\n\r\f'\\\\]\\|"
                                edraw-css-re-string-escape "\\)*" "'"))
(defconst edraw-css-re-string (concat "\\(?:" edraw-css-re-string1 "\\|"
                                      edraw-css-re-string2 "\\)"))
(defconst edraw-css-re-url-arg (concat
                                "\\(?:"
                                "[!#$%&*-~]\\|[[:nonascii:]]\\|"
                                edraw-css-re-escape
                                "\\)*"))
(defconst edraw-css-re-url-rest (concat
                                 edraw-css-re-ws*
                                 "\\("
                                 edraw-css-re-url-arg
                                 "\\)"
                                 edraw-css-re-ws*
                                 ")"))
(defconst edraw-css-re-number ;;edraw-svg-re-number
  (concat "\\(?:"
          "[-+]?"
          ;; Valid: 12  12.34  .34  Invalid: 12.
          "\\(?:[0-9]+\\(?:\\.[0-9]+\\)?\\|\\.[0-9]+\\)"
          "\\(?:[eE][-+]?[0-9]+\\)?"
          "\\)"))
(defconst edraw-css-re-dimension (concat
                                  edraw-css-re-number
                                  edraw-css-re-ident))
(defconst edraw-css-re-percentage (concat edraw-css-re-number "%"))

;; @todo Support bad-*, unicode-range token
;; Remaining tokens:
;; bad-string
;; bad-url
;; delim
;; unicode-range
;; CDO, CDC
;; colon
;; semicolon
;; comma
;; [, ]
;; (, )
;; {, }

;;;;; Unescape

(defun edraw-css-unescape (string)
  (replace-regexp-in-string
   edraw-css-re-string-escape
   (lambda (text)
     (let ((ch (aref text 1)))
       (cond
        ((or (<= ?0 ch ?9) (<= ?a ch ?f) (<= ?A ch ?F))
         (let ((cp (string-to-number (substring text 1) 16)))
           ;;@todo check range
           (char-to-string cp)))
        ((memq ch '(?\r ?\n ?\f)) "")
        (t (substring text 1)))))
   string t t))
;; TEST: (edraw-css-unescape "u\\rl(") => "url("
;; TEST: (edraw-css-unescape "u\\72 l(") => "url("
;; TEST: (edraw-css-unescape "line1\\\r\nline1\\\nline1") => "line1line1line1"
;; TEST: (edraw-css-unescape "\\26 B") => "&B"
;; TEST: (edraw-css-unescape "\\000026B") => "&B"

;;;;; Tokenize

(defconst edraw-css-re-token
  (concat
   edraw-css-re-comment "*"
   "\\(?:\\(" edraw-css-re-ws "\\)"
   "\\|\\(" edraw-css-re-string "\\)"  ;; " '
   "\\|\\(" edraw-css-re-hash "\\)"  ;; #
   "\\|\\(" edraw-css-re-at-keyword "\\)"  ;; @
   "\\|\\(" edraw-css-re-dimension "\\)"
   "\\|\\(" edraw-css-re-percentage "\\)"
   "\\|\\(" edraw-css-re-number "\\)"
   "\\|\\(" edraw-css-re-function "\\)"
   "\\|\\(" edraw-css-re-ident "\\)"
   "\\|\\(" "[]({}),:;[]" "\\)"
   "\\|\\(" "." "\\)" ;; ( { [ ] } ) , : ; delim
   "\\|\\(" "\\'" "\\)"
   "\\)"))

(defun edraw-css-match (regexp str ppos &optional noerror)
  (let ((pos (car ppos)))
    (if (equal (string-match regexp str pos) pos)
        (setcar ppos (match-end 0))
      (unless noerror
        (error "CSS Syntax Error: %s `%s'" pos str)))))

(defun edraw-css-token (str ppos)
  (edraw-css-match edraw-css-re-token str ppos)
  (let* ((index (cl-loop for index from 1
                         ;; match-data is 100x slower than match-beginning
                         when (match-beginning index)
                         return index))
         (range (cons (match-beginning index) (match-end index))))
    (pcase index
      (1 (cons 'ws range))
      (2 (cons 'string range))
      (3 (cons 'hash range))
      (4 (cons 'at-keyword range))
      (5 (cons 'dimension range))
      (6 (cons 'percentage range))
      (7 (cons 'number range))
      (8
       ;; URL or Function
       (let ((fname (edraw-css-unescape
                     (substring str (car range) (1- (cdr range))))))
         (if (string= (downcase fname) "url")
             (progn
               (edraw-css-match edraw-css-re-url-rest str ppos)
               (let ((url-end (match-end 0)))
                 (cons 'url (cons (car range) url-end))))
           (cons 'function range))))
      (9 (cons 'ident range))
      (10
       ;; ( { [ ] } ) , : ;
       (cons (intern (substring str (car range) (cdr range))) range))
      (11 (cons 'delim range))
      (12 (cons 'EOF range)))))

(defun edraw-css-token-value (str token)
  (let ((type (car token))
        (beg (cadr token))
        (end (cddr token)))
    (pcase type
      ('ws (substring str beg end))
      ('string (edraw-css-unescape (substring str (1+ beg) (1- end))))
      ('hash (edraw-css-unescape (substring str (1+ beg) end)))
      ('at-keyword (edraw-css-unescape (substring str (1+ beg) end)))
      ('dimension (substring str beg end)) ;;@todo to number and unit?
      ('percentage (substring str beg end)) ;;@todo to number and unit?
      ('number (substring str beg end)) ;;@todo to number?
      ('url
       (string-match (concat "(" edraw-css-re-url-rest) str beg)
       (match-string 1 str))
      ('function (edraw-css-unescape (substring str beg (1- end))))
      ('ident (edraw-css-unescape (substring str beg end)))
      ('delim (aref str beg))
      ('EOF nil)
      ;; ( { [ ] } ) , : ;
      (_ type))))

(defun edraw-css-token-test (str pos)
  (let* ((ppos (list pos))
         (result (edraw-css-token str ppos)))
    (list (car result)
          (cdr result)
          (car ppos)
          (edraw-css-token-value str result))))
;; TEST: (edraw-css-token-test "  hoge" 0) => (ws (0 . 1) 1 " ")
;; TEST: (edraw-css-token-test "/* hoge */hoge" 0) => (ident (10 . 14) 14 "hoge")
;; TEST: (edraw-css-token-test "'hoge\"ho\\ge\"'" 0) => (string (0 . 13) 13 "hoge\"hoge\"")
;; TEST: (edraw-css-token-test "@hoge" 0) => (at-keyword (0 . 5) 5 "hoge")
;; TEST: (edraw-css-token-test " #hoge" 1) => (hash (1 . 6) 6 "hoge")
;; TEST: (edraw-css-token-test " 100px" 1) => (dimension (1 . 6) 6 "100px")
;; TEST: (edraw-css-token-test " 100%" 1) => (percentage (1 . 5) 5 "100%")
;; TEST: (edraw-css-token-test " 100 " 1) => (number (1 . 4) 4 "100")
;; TEST: (edraw-css-token-test " 100. " 1) => (number (1 . 4) 4 "100")
;; TEST: (edraw-css-token-test " fun(hoge) " 1) => (function (1 . 5) 5 "fun")
;; TEST: (edraw-css-token-test " u\\rl( https://misohena.jp/?q=hoge ) " 1) => (url (1 . 36) 36 "https://misohena.jp/?q=hoge")
;; TEST: (edraw-css-token-test " url( https://misohena.jp/" 1) => error
;; TEST: (edraw-css-token-test "hoge" 0) => (ident (0 . 4) 4 "hoge")
;; TEST: (edraw-css-token-test "  { " 2) => ({ (2 . 3) 3 {)
;; TEST: (edraw-css-token-test "  ! " 2) => (delim (2 . 3) 3 33)


;;;;; Parse

(defun edraw-css-skip-ws* (str ppos)
  (edraw-css-match edraw-css-re-ws* str ppos))

(defun edraw-css-expect (str ppos type)
  (let ((beg (car ppos))
        (token (edraw-css-token str ppos)))
    (unless (eq (car token) type)
      (error "Unexpected token: `%s' %s `%s'" (car token) beg str))
    (cdr token)))

(defun edraw-css-skip-simple-block (str ppos start-token)
  ;; https://www.w3.org/TR/css-syntax-3/#consume-simple-block
  (let ((ending-token (pcase (car start-token)
                        ('\[ '\])
                        ('\{ '\})
                        ;; '\( or 'function
                        (_ '\)))))
    (while (let ((cvtt (edraw-css-skip-component-value str ppos)))
             (when (eq cvtt 'EOF) (error "Unexpected token: `%s'" cvtt))
             (not (eq cvtt ending-token))))
    'simple-block))

(defun edraw-css-skip-component-value (str ppos &optional token)
  ;; https://www.w3.org/TR/css-syntax-3/#component-value-diagram
  ;; https://www.w3.org/TR/css-syntax-3/#parse-component-value
  ;; https://www.w3.org/TR/css-syntax-3/#consume-a-component-value
  (let ((token (or token
                   (progn
                     (edraw-css-skip-ws* str ppos)
                     (edraw-css-token str ppos)))))
    (pcase (car token)
      ((or '\( '\{ '\[ 'function)
       (edraw-css-skip-simple-block str ppos token)
       ;; Return 'simple-block
       )
      (type type))))

(defun edraw-css-skip-at-rule (str ppos)
  ;; https://www.w3.org/TR/css-syntax-3/#consume-at-rule
  (while (progn
           (edraw-css-skip-ws* str ppos)
           (let ((token (edraw-css-token str ppos)))
             (pcase (car token)
               ('ws t) ;; This is not used because skip-ws* is called, but just in case
               ('\; nil)
               ('EOF
                (error "Unexpected %s in at-rule `%s'" token str)
                nil)
               ('\{
                (edraw-css-skip-simple-block str ppos token)
                nil)
               (_
                (edraw-css-skip-component-value str ppos token)
                t))))))

(defun edraw-css-split-decl-list (str ppos)
  ;; https://www.w3.org/TR/css-syntax-3/#consume-list-of-declarations
  (let (decls)
    (while (progn
             (edraw-css-skip-ws* str ppos)
             (let ((token-beg (car ppos))
                   (token (edraw-css-token str ppos)))
               (pcase (car token)
                 ('ws t) ;; This is not used because skip-ws* is called, but just in case
                 (': t)
                 ('EOF nil)
                 ('at-keyword
                  (edraw-css-skip-at-rule str ppos)
                  t)
                 ('ident
                  (edraw-css-skip-ws* str ppos)
                  (edraw-css-expect str ppos ':)
                  (edraw-css-skip-ws* str ppos)
                  (let ((bov (car ppos)) ;;@todo include comments?
                        eov)
                    (while (progn
                             (pcase (edraw-css-skip-component-value str ppos)
                               ('\; (setq eov (1- (car ppos))) nil)
                               ('EOF (setq eov (car ppos)) nil)
                               (_ t))))
                    (push (cons token (cons bov eov)) decls))
                  t)
                 (_ (error "Unexpected token: `%s' %s `%s'" token str token-beg) nil)))))
    (nreverse decls)))
;; TEST: (edraw-css-split-decl-list "margin :0 auto  " (list 0)) => (((ident 0 . 6) 8 . 16))
;; TEST: (edraw-css-split-decl-list "margin" (list 0)) => error
;; TEST: (edraw-css-split-decl-list "prop1: fun(hoge" (list 0)) => error
;; TEST: (edraw-css-split-decl-list "123: 456" (list 0)) => error
;; TEST: (edraw-css-split-decl-list "@unsupported { splines: reticulating } color: green" (list 0)) => (((ident 39 . 44) 46 . 51))  Example From: https://drafts.csswg.org/css-style-attr/#style-attribute:~:text=%3Cspan-,style%3D%22%40unsupported,-%7B%20splines%3A%20reticulating%20%7D%20color
;; TEST: (edraw-css-split-decl-list "@unsupported opt1 \"hoge\" 123; color: green" (list 0)) => (((ident 30 . 35) 37 . 42))

(defun edraw-css-split-decl-list-as-strings (str ppos)
  (mapcar (lambda (prop)
            (cons (edraw-css-token-value str (car prop))
                  (substring str (cadr prop) (cddr prop))))
          (edraw-css-split-decl-list str ppos)))

;; TEST: (edraw-css-split-decl-list-as-strings "margin :0 auto  " (list 0)) => (("margin" . "0 auto  "))
;; TEST: (edraw-css-split-decl-list-as-strings "font-size:14px;fill:red;" (list 0)) => (("font-size" . "14px") ("fill" . "red"))
;; TEST: (edraw-css-split-decl-list-as-strings "font-size:14px;font-family  :  \"Helvetica Neue\", \"Arial\", sans-serif;" (list 0)) => (("font-size" . "14px") ("font-family" . "\"Helvetica Neue\", \"Arial\", sans-serif"))
;; TEST: (edraw-css-split-decl-list-as-strings "prop1: func( a b ; c d ); prop2: { aa bb ; cc dd}" (list 0)) => (("prop1" . "func( a b ; c d )") ("prop2" . "{ aa bb ; cc dd}"))
;; TEST: (edraw-css-split-decl-list-as-strings "str\\oke: u\\72 l(https://misohena.jp/blog/?q=;)" (list 0)) => (("stroke" . "u\\72 l(https://misohena.jp/blog/?q=;)"))


;;;; SVG Print


(defun edraw-svg-to-image (svg &rest props)
  (apply
   #'create-image
   (edraw-svg-to-string svg nil nil)
   'svg t
   props))

(defun edraw-svg-to-string (dom node-filter attr-filter &optional indent no-indent)
  (with-temp-buffer
    (edraw-svg-print dom node-filter attr-filter indent no-indent)
    (buffer-string)))

(defun edraw-svg-print (dom node-filter attr-filter &optional indent no-indent)
  ;; Derived from svg-print in svg.el
  (when (or (null node-filter) (funcall node-filter dom))
    (cond
     ;; Text Node
     ((stringp dom)
      (edraw-svg-print--text-node dom))
     ;; Comment
     ((eq (car-safe dom) 'comment)
      (edraw-svg-print--comment dom))
     ;; Top-Level Comments
     ;;@todo `top' should only be processed if dom is expected to be the root element.
     ((eq (car-safe dom) 'top)
      (edraw-svg-print--top-level dom node-filter attr-filter indent no-indent))
     ;; Element
     (t
      (edraw-svg-print--element dom node-filter attr-filter indent no-indent))
     )))

(defun edraw-svg-print--text-node (dom)
  (insert (edraw-svg-escape-chars dom)))

(defun edraw-svg-print--comment (dom)
  (insert "<!--" (caddr dom) "-->"))

(defun edraw-svg-print--top-level (dom node-filter attr-filter indent no-indent)
  (let ((children (cddr dom)))
    (dolist (node children)
      (if (edraw-dom-tag-eq node 'comment)
          (progn
            ;; Insert a line break after root element for footer comments.
            (unless (bolp)
              (insert "\n"))
            (edraw-svg-print--comment node)
            ;; Insert a line break after each top-level comment.
            ;; It expects to put a line like
            ;; <!-- -*- mode: edraw -*- --> at the top of the file.
            (insert "\n"))
        (edraw-svg-print node node-filter attr-filter indent no-indent)))))

(defun edraw-svg-print--element (dom node-filter attr-filter indent no-indent)
  (let ((tag (car dom))
        (attrs (cadr dom))
        (children (cddr dom)))
    (when (and (integerp indent) (not no-indent))
      (insert (make-string indent ? )))
    (insert (format "<%s" tag))
    (dolist (attr attrs)
      (when (and (or (null attr-filter) (funcall attr-filter attr))
                 (not (edraw-dom-attr-internal-p (car attr))))
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
      (edraw-svg-print--children-and-end-tag
       tag children node-filter attr-filter indent no-indent))))

(defun edraw-svg-print--children-and-end-tag (tag
                                              children node-filter attr-filter
                                              indent no-indent)
  (let ((no-indent (or no-indent
                       (not (integerp indent))
                       (memq tag '(text tspan))
                       (seq-find 'stringp children))))
    (dolist (elem children)
      (unless no-indent (insert "\n"))
      (edraw-svg-print elem node-filter attr-filter (unless no-indent (+ indent 2)) no-indent))
    (unless no-indent (insert "\n" (make-string indent ? )))
    (insert (format "</%s>" tag))))

(make-obsolete 'edraw-svg-print-attr-filter nil "2024-05-08")
(defun edraw-svg-print-attr-filter (_attr)
  "Always return t."
  t)

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
    (let ((dom (libxml-parse-xml-region (point-min) (point-max))))
      ;; libxml-parse-xml-region drops the xmlns= attribute and
      ;; replaces xlink:href= with href=.
      ;; Fix xmlns:xlink and xlink:href
      (edraw-svg-compatibility-fix dom)
      dom)))

(defun edraw-svg-decode-svg (data base64-p
                                  &optional accepts-top-level-comments-p)
  (let* ((dom (edraw-svg-decode data base64-p))
         (root-svg (car (edraw-dom-split-top-nodes dom))))
    ;; Recover missing xmlns on root-svg.
    ;; libxml-parse-xml-region drops the xmlns attribute.
    (when (edraw-dom-tag-eq root-svg 'svg)
      (unless (dom-attr root-svg 'xmlns)
        (dom-set-attribute root-svg 'xmlns "http://www.w3.org/2000/svg")))

    ;; Strip `top' root element generated by libxml-parse-xml-region.
    (if accepts-top-level-comments-p
        dom
      root-svg)))

(defun edraw-svg-encode (svg base64-p gzip-p)
  (with-temp-buffer
    (edraw-svg-print svg nil nil)
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

(defun edraw-svg-read-from-file (path &optional accepts-top-level-comments-p)
  (edraw-svg-decode-svg
   (with-temp-buffer
     (insert-file-contents path)
     (buffer-substring-no-properties (point-min) (point-max)))
   nil
   accepts-top-level-comments-p))


;;;; SVG Attributes

;;;;; White Spaces

(defconst edraw-svg-re-wsp "\\(?:[ \t\n\f\r]+\\)")
(defconst edraw-svg-re-wsp-opt "[ \t\n\f\r]*")
(defconst edraw-svg-re-comma-wsp
  "\\(?:[ \t\n\f\r]+,?[ \t\n\f\r]*\\|,[ \t\n\f\r]*\\)")

;;;;; Number

(defconst edraw-svg-re-number
  (concat "\\(?:"
          "[-+]?"
          ;; Valid: 12  12.34  .34  Invalid: 12.
          "\\(?:[0-9]+\\(?:\\.[0-9]+\\)?\\|\\.[0-9]+\\)"
          "\\(?:[eE][-+]?[0-9]+\\)?"
          "\\)")
  "A regular expression that matches CSS numbers.

URL `https://www.w3.org/TR/SVG11/types.html#DataTypeNumber'
URL `https://www.w3.org/TR/css-syntax-3/#number-token-diagram'

NOTE: This is different from the number in d attribute of path.")

(defconst edraw-svg-re-abs-number-with-dot
  (concat "\\(?:"
          ;; Valid: 12  12.34  .34  12.
          "\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)"
          "\\(?:[eE][-+]?[0-9]+\\)?"
          "\\)")
  "A regular expression for numbers without sign that can end with a dot.

Matches the number syntax in the transform, d, and points
attributes of the SVG1.1 specification.
However, SVG2's transform attribute does not allow trailing dots.")

(defconst edraw-svg-re-number-with-dot
  (concat "\\(?:"
          "[-+]?"
          edraw-svg-re-abs-number-with-dot
          "\\)")
  "A regular expression for numbers that can end with a dot.

Matches the number syntax in the transform, d, and points
attributes of the SVG1.1 specification.
However, SVG2's transform attribute does not allow trailing dots.")

;;;;; Length

(defconst edraw-svg-re-length-unit
  "\\(?:em\\|ex\\|px\\|in\\|cm\\|mm\\|pt\\|pc\\|\\%\\)?"
  "URL `https://www.w3.org/TR/SVG11/types.html#DataTypeLength'")

(defconst edraw-svg-re-length
  (concat "\\(" edraw-svg-re-number "\\)"
          "\\(" edraw-svg-re-length-unit "\\)"))

(defconst edraw-svg-re-attr-length
  (concat "\\`" edraw-svg-re-wsp-opt edraw-svg-re-length edraw-svg-re-wsp-opt
          "\\'"))

(defun edraw-svg-attr-length-match (value)
  (when (string-match edraw-svg-re-attr-length value)
    (cons (string-to-number (match-string 1 value))
          (match-string 2 value))))

;;@todo default values
(defconst edraw-svg-attr-default-font-size 16)
(defconst edraw-svg-attr-default-dpi 96)

(defun edraw-svg-attr-length-dpi ()
  edraw-svg-attr-default-dpi)

(defun edraw-svg-attr-length-em (element)
  ;; @todo Is there a way to get the exact em?
  ;; @todo Style should be considered.
  (or (edraw-svg-attr-length-or-inherited element 'font-size)
      edraw-svg-attr-default-font-size))

(defun edraw-svg-attr-length-ex (element)
  ;; @todo Is there a way to get the exact ex?
  ;; @todo Style should be considered.
  (/ (edraw-svg-attr-length-em element) 2.0))

(defun edraw-svg-attr-length-viewport-size (element)
  (if-let ((svg (if (eq (dom-tag element) 'svg)
                    element
                  (edraw-dom-get-ancestor-by-tag element 'svg))))
      (if-let ((vbox (dom-attr svg 'viewBox)))
          ;; viewBox="<min-x> <min-y> <width> <height>"
          (let* ((vbox-vals
                  (save-match-data
                    (split-string vbox
                                  "\\([ \t\n\r]*,[ \t\n\r]*\\|[ \t\n\r]+\\)")))
                 (width (string-to-number (or (nth 2 vbox-vals) "")))
                 (height (string-to-number (or (nth 3 vbox-vals) ""))))
            (cons
             (max 0 width)
             (max 0 height)))
        ;; width= height=
        (cons
         (or (edraw-svg-attr-length element 'width) 0)
         (or (edraw-svg-attr-length element 'height) 0)))
    (cons 0 0)))

(defun edraw-svg-attr-length-percentage (element attr)
  (pcase attr
    ('font-size
     (or (edraw-svg-attr-length-or-inherited (edraw-dom-get-parent element)
                                             'font-size)
         edraw-svg-attr-default-font-size))
    ((or 'x 'rx 'cx 'x1 'x2 'dx 'width)
     (car (edraw-svg-attr-length-viewport-size element)))
    ((or 'y 'ry 'cy 'y1 'y2 'dy 'height)
     (cdr (edraw-svg-attr-length-viewport-size element)))
    (_
     ;; https://www.w3.org/TR/SVG11/coords.html#Units_viewport_percentage
     (let* ((vsize (edraw-svg-attr-length-viewport-size element))
            (vw (car vsize))
            (vh (cdr vsize)))
       (/ (sqrt (+ (* vw vw) (* vh vh))) (sqrt 2))))))

(defun edraw-svg-attr-length-match-to-number (num-unit element attr)
  ;; <length> ::=  number ("em"|"ex"|"px"|"in"|"cm"|"mm"|"pt"|"pc"|"%")?
  (if num-unit
      (let ((num (car num-unit))
            (unit (cdr num-unit)))
        (pcase unit
          ('nil num)
          ("" num)
          ("px" num)
          ("in" (* num (edraw-svg-attr-length-dpi)))
          ("cm" (/ (* num (edraw-svg-attr-length-dpi)) 2.54))
          ("mm" (/ (* num (edraw-svg-attr-length-dpi)) 25.4))
          ("pt" (/ (* num (edraw-svg-attr-length-dpi)) 72.0))
          ("pc" (/ (* num (edraw-svg-attr-length-dpi)) 6.0))
          ("em" (* (edraw-svg-attr-length-em element) num))
          ("ex" (* (edraw-svg-attr-length-ex element) num))
          ("%" (/ (* (edraw-svg-attr-length-percentage element attr) num)
                  100.0))
          (_ 0)))
    0))

;;;;; List

(defun edraw-svg-attr-split-list-string (string)
  "Split STRING with comma-wsp.

Can only be used if there cannot be commas or spaces within the
parts being split."
  (when (stringp string)
    (let ((trimmed (string-trim string edraw-svg-re-wsp)))
      (unless (string-empty-p trimmed)
        (split-string trimmed edraw-svg-re-comma-wsp)))))
;; TEST: (edraw-svg-attr-split-list-string nil) => nil
;; TEST: (edraw-svg-attr-split-list-string "") => nil
;; TEST: (edraw-svg-attr-split-list-string "    ") => nil
;; TEST: (edraw-svg-attr-split-list-string "11") => ("11")
;; TEST: (edraw-svg-attr-split-list-string "11,22") => ("11" "22")
;; TEST: (edraw-svg-attr-split-list-string "11 22 33 44 55") => ("11" "22" "33" "44" "55")
;; TEST: (edraw-svg-attr-split-list-string "  11   22 , 33, 44 ,55  ") => ("11" "22" "33" "44" "55")
;; TEST: (edraw-svg-attr-split-list-string "  11   22 , 33, 44 ,55  ,  ") => ("11" "22" "33" "44" "55" "")
;; TEST: (edraw-svg-attr-split-list-string "  ,  ") => ("" "")

;;;;; Conversion

(defun edraw-svg-attr-length-to-number (value &optional element attr)
  "Convert length attribute value to number."
  (cond
   ((null value)
    value)
   ((stringp value)
    (edraw-svg-attr-length-match-to-number (edraw-svg-attr-length-match value)
                                           element
                                           attr))
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

(defun edraw-svg-attr-length-list-to-number-list (value &optional element attr)
  "Convert length list attribute value to a list of numbers."
  (cond
   ((stringp value)
    (cl-loop for str in (edraw-svg-attr-split-list-string value)
             for num-unit = (edraw-svg-attr-length-match str)
             while num-unit
             collect (edraw-svg-attr-length-match-to-number num-unit element attr)))
   ((numberp value)
    (list value))
   ((null value)
    nil)
   ((listp value) ;; Not nil
    (when (seq-every-p #'numberp value)
      value))
   ;; symbol?
   (t
    nil)))

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
  (edraw-svg-attr-length-to-number (dom-attr element attr)
                                   element
                                   attr))

(defun edraw-svg-attr-length-or-inherited (element attr)
  (when element
    (if (dom-attr element attr)
        (edraw-svg-attr-length element attr)
      (edraw-svg-attr-length-or-inherited (edraw-dom-get-parent element)
                                          attr))))

(defun edraw-svg-attr-length-list (element attr)
  (when element
    (edraw-svg-attr-length-list-to-number-list (dom-attr element attr)
                                               element
                                               attr)))

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


;;;; SVG Transform Syntax (CSS Properties and SVG Attributes)

(defconst edraw-svg-transform-number
  ;;edraw-svg-re-number-with-dot
  ;; Use CSS <number-token> even if it is an attribute rather than a
  ;; CSS property.
  ;; Both browsers and librsvg treat numbers like "123." as errors.
  edraw-svg-re-number)

(defconst edraw-svg-transform-unit "\\(?:[a-z]+\\|%\\)")
(defconst edraw-svg-transform-number-unit
  (concat edraw-svg-transform-number edraw-svg-transform-unit "?"))
(defconst edraw-svg-transform-function
  (concat
   edraw-svg-re-wsp-opt
   ;; (1) function name
   "\\([A-Za-z0-9_]+\\)"
   edraw-svg-re-wsp-opt
   "("
   edraw-svg-re-wsp-opt
   ;;(2) command arguments
   "\\(" edraw-svg-transform-number-unit
   "\\(?:" edraw-svg-re-comma-wsp edraw-svg-transform-number-unit "\\)*\\)?"
   edraw-svg-re-wsp-opt ")" edraw-svg-re-wsp-opt))

(defvar edraw-svg-css-transform-functions
  ;; https://www.w3.org/TR/css-transforms-1/#transform-functions
  '(("matrix" edraw-svg-transform--matrix
     (number number number number number number) (6))
    ("translate" edraw-svg-transform--translate (length length) (1 2))
    ("translateX" edraw-svg-transform--translateX (length) (1))
    ("translateY" edraw-svg-transform--translateY (length) (1))
    ("scale" edraw-svg-transform--scale (number number) (1 2))
    ("scaleX" edraw-svg-transform--scaleX (number) (1))
    ("scaleY" edraw-svg-transform--scaleY (number) (1))
    ("rotate" edraw-svg-transform--rotate (angle) (1))
    ("skew" edraw-svg-transform--skew (angle angle) (1 2))
    ("skewX" edraw-svg-transform--skewX (angle) (1))
    ("skewY" edraw-svg-transform--skewY (angle) (1))))

(defvar edraw-svg-attr-transform-functions
  ;; https://www.w3.org/TR/css-transforms-1/#svg-transform
  ;; For backwards compatibility reasons, the syntax of the transform,
  ;; patternTransform, gradientTransform attributes differ from the
  ;; syntax of the transform CSS property.
  ;; See: https://www.w3.org/TR/css-transforms-1/#svg-syntax
  '(("matrix" edraw-svg-transform--matrix
     (number number number number number number) (6))
    ("translate" edraw-svg-transform--translate (number number) (1 2))
    ("scale" edraw-svg-transform--scale (number number) (1 2))
    ("rotate" edraw-svg-transform--rotate (number number number ) (1 3))
    ("skewX" edraw-svg-transform--skewX (number) (1))
    ("skewY" edraw-svg-transform--skewY (number) (1))))

(defun edraw-svg-transform-parse-numbers (numbers-str
                                          transform-functions-alist
                                          element fname)
  (let* ((f-info (assoc fname transform-functions-alist))
         (arg-types (nth 2 f-info))
         (arg-counts (nth 3 f-info))
         (args (split-string numbers-str edraw-svg-re-comma-wsp)))
    (unless f-info
      (error "Unknown transform function `%s'" fname))
    (when (and arg-counts (not (memq (length args) arg-counts)))
      (error "Wrong number of arguments for transform function `%s' %s"
             fname (length args)))

    (cl-loop for ns in args
             for index from 0
             collect
             (when (string-match
                    (concat "\\(" edraw-svg-transform-number "\\)"
                            "\\(" edraw-svg-transform-unit "\\)?")
                    ns)
               (let ((num (string-to-number (match-string 1 ns)))
                     (unit (match-string 2 ns))
                     (arg-type (nth index arg-types)))
                 ;; @todo Support more length units (See: https://www.w3.org/TR/css-values-4/#lengths)
                 ;; Reject invalid units for FNAME's arguments
                 (unless (pcase arg-type
                           ('number (member unit '(nil "")))
                           ('length (or (member unit '("px" "in" "cm" "mm"
                                                       "pt" "pc"
                                                       "em" "ex" "%"))
                                        (= num 0)))
                           ('angle (or (member unit '("deg" "rad" "grad"
                                                      "turn"))
                                       (= num 0))))
                   (error "Invalid unit `%s' (arg:%s fun:%s)" unit index fname))

                 (pcase unit
                   ((or 'nil "") num)
                   ;; angle to degrees
                   ("deg" num)
                   ("rad" (radians-to-degrees num))
                   ("grad" (/ (* num 180) 200.0))
                   ("turn" (* 360 num))
                   ;; length to px
                   (_
                    ;; @todo Support transform-box
                    (edraw-svg-attr-length-match-to-number
                     (cons num unit)
                     element
                     (pcase fname
                       ("translateX" 'x)
                       ("translateY" 'y)
                       ("translate" (pcase index (0 'x) (1 'y))))))))))))

(defun edraw-svg-transform-parse (str transform-functions-alist element)
  (let ((pos 0)
        functions)
    (while (and (string-match edraw-svg-transform-function str pos)
                (= (match-beginning 0) pos))
      (setq pos (match-end 0))
      (let* ((fname (match-string 1 str))
             (numbers-str (match-string 2 str))
             (numbers (edraw-svg-transform-parse-numbers
                       numbers-str transform-functions-alist element fname)))
        (push (cons fname numbers) functions)))
    (when (/= pos (length str))
      (error "transform value parsing error at %s" (substring str pos)))
    (nreverse functions)))

;; [CSS]
;;TEST: (edraw-svg-transform-parse "" edraw-svg-css-transform-functions nil) => nil
;;TEST: (edraw-svg-transform-parse "translate(10px 20px)" edraw-svg-css-transform-functions nil) => (("translate" 10 20))
;;TEST: (edraw-svg-transform-parse "rotate(180deg)" edraw-svg-css-transform-functions nil) => (("rotate" 180))
;;TEST: (edraw-svg-transform-parse "scale(2) rotate(0.125turn)" edraw-svg-css-transform-functions nil) => (("scale" 2) ("rotate" 45.0))
;;TEST: (edraw-svg-transform-parse "translate(20% 70%)" edraw-svg-css-transform-functions nil) => (("translate" 0.0 0.0))
;;TEST: (edraw-svg-transform-parse "translate(20% 70%)" edraw-svg-css-transform-functions (edraw-svg-create 200 100)) => (("translate" 40.0 70.0))

;; [ATTR]
;;TEST: (edraw-svg-transform-parse "" edraw-svg-attr-transform-functions nil) => nil
;;TEST: (edraw-svg-transform-parse "translate(10)" edraw-svg-attr-transform-functions nil) => (("translate" 10))
;;TEST: (edraw-svg-transform-parse "translate(10 20)" edraw-svg-attr-transform-functions nil) => (("translate" 10 20))
;;TEST: (edraw-svg-transform-parse "translateX(10)" edraw-svg-attr-transform-functions nil) => error
;;TEST: (edraw-svg-transform-parse "rotate(180deg)" edraw-svg-attr-transform-functions nil) => error
;;TEST: (edraw-svg-transform-parse "rotate(180)" edraw-svg-attr-transform-functions nil) => (("rotate" 180))
;;TEST: (edraw-svg-transform-parse "rotate(180 320 240)" edraw-svg-attr-transform-functions nil) => (("rotate" 180 320 240))
;;TEST: (edraw-svg-transform-parse "scale(2.5)" edraw-svg-attr-transform-functions nil) => (("scale" 2.5))
;;TEST: (edraw-svg-transform-parse "scale(2.5 -1)" edraw-svg-attr-transform-functions nil) => (("scale" 2.5 -1))
;;TEST: (edraw-svg-transform-parse "skewX(30.5)" edraw-svg-attr-transform-functions nil) => (("skewX" 30.5))
;;TEST: (edraw-svg-transform-parse "skewY(-.34)" edraw-svg-attr-transform-functions nil) => (("skewY" -0.34))
;;TEST: (edraw-svg-transform-parse "scale(2) rotate(45.0)" edraw-svg-attr-transform-functions nil) => (("scale" 2) ("rotate" 45.0))
;;TEST: (edraw-svg-transform-parse "scale(2) rotate(0.125turn)" edraw-svg-attr-transform-functions nil) => error

(defun edraw-svg-css-transform-to-matrix (str element)
  (edraw-svg-transform-to-matrix str
                                 edraw-svg-css-transform-functions
                                 element))

(defun edraw-svg-transform-to-matrix (str &optional
                                          transform-functions-alist
                                          element)
  (let ((transform-functions-alist (or transform-functions-alist
                                       edraw-svg-attr-transform-functions)))
    (seq-reduce
     #'edraw-matrix-mul-mat-mat
     (mapcar (lambda (fname-args)
               (edraw-svg-transform-apply fname-args transform-functions-alist))
             (edraw-svg-transform-parse str
                                        transform-functions-alist
                                        element))
     (edraw-matrix))))

;;TEST: (edraw-svg-transform-to-matrix "translate(10 20)") => [1 0 0 0 0 1 0 0 0 0 1 0 10 20 0 1]
;;TEST: (edraw-svg-transform-to-matrix "translate(10)") => [1 0 0 0 0 1 0 0 0 0 1 0 10 0 0 1]
;;TEST: (edraw-svg-transform-to-matrix "scale(2) translate(10 20)") => [2 0 0 0 0 2 0 0 0 0 1 0 20 40 0 1]
;;TEST: (edraw-svg-transform-to-matrix "scale(2 -4) translate(10 20)") => [2 0 0 0 0 -4 0 0 0 0 1 0 20 -80 0 1]
;;TEST: (edraw-svg-transform-to-matrix "rotate(45)") => [0.7071067811865476 0.7071067811865475 0.0 0.0 -0.7071067811865475 0.7071067811865476 0.0 0.0 0 0 1 0 0 0 0 1]
;;TEST: (edraw-svg-transform-to-matrix "rotate(45 10 10)") => [0.7071067811865476 0.7071067811865475 0.0 0.0 -0.7071067811865475 0.7071067811865476 0.0 0.0 0.0 0.0 1.0 0.0 10.0 -4.142135623730951 0.0 1.0]

;;;; SVG Transform Functions

(defun edraw-svg-transform-apply (fname-args transform-functions-alist)
  (let* ((fname (car fname-args))
         (args (cdr fname-args))
         (f-info (assoc fname transform-functions-alist))
         (fun (nth 1 f-info)))
    (apply fun args)))

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

;;;; SVG Transform Attribute

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


;;;; SVG Points Attribute


;; https://www.w3.org/TR/SVG11/shapes.html#PointsBNF
(defconst edraw-svg-re-coordinate-pair
  (concat edraw-svg-re-wsp-opt
          "\\(" edraw-svg-re-number-with-dot "\\)" ;;(1)
          "\\(?:"
          edraw-svg-re-comma-wsp "\\(" edraw-svg-re-number-with-dot "\\)" ;;(2)
          "\\|"
          "\\(" "-" edraw-svg-re-abs-number-with-dot "\\)" ;;(3)
          "\\)"))

(defun edraw-svg-parse-points (points-str)
  (let ((pos 0) result)
    (while (and
            ;; comma-wsp
            (or (= pos 0)
                (when (eq (string-match
                           edraw-svg-re-comma-wsp points-str pos)
                          pos)
                  (setq pos (match-end 0))
                  t))
            ;; coordinate-pair
            (when (eq (string-match
                       edraw-svg-re-coordinate-pair points-str pos)
                      pos)
              (setq pos (match-end 0))
              t))

      (push (cons (string-to-number (match-string 1 points-str))
                  (string-to-number (or (match-string 2 points-str)
                                        (match-string 3 points-str))))
            result))
    ;; wsp*
    (when (eq (string-match edraw-svg-re-wsp-opt points-str pos) pos)
      (setq pos (match-end 0)))
    (when (/= pos (length points-str))
      (error "Failed to parse list-of-points at %s"
             (substring points-str pos)))
    (nreverse result)))
;; TEST: (edraw-svg-parse-points "") => nil
;; TEST: (edraw-svg-parse-points "1") => error
;; TEST: (edraw-svg-parse-points "1 2") => ((1 . 2))
;; TEST: (edraw-svg-parse-points "11.-2e-2") => ((11 . -0.02))
;; TEST: (edraw-svg-parse-points "11-22,33 44") => ((11 . -22) (33 . 44))
;; TEST: (edraw-svg-parse-points "11-22 33 44") => ((11 . -22) (33 . 44))
;; TEST: (edraw-svg-parse-points " 11-22 33 44 ") => ((11 . -22) (33 . 44))
;; TEST: (edraw-svg-parse-points " 11-22 33 44 hh") => error
;; TEST: (edraw-svg-parse-points " 11,-22 33,44") => ((11 . -22) (33 . 44))


;;;; SVG Compatibility

(defun edraw-svg-compatibility-fix (svg)
  (let ((ver.1.1-p (version<= edraw-svg-version "1.1")))
    (edraw-dom-do
     svg
     (lambda (node _ancestors)
       (when (edraw-dom-element-p node)
         ;; xmlns:xlink= and version=
         (when (edraw-dom-tag-eq node 'svg)
           (if ver.1.1-p
               (progn
                 (dom-set-attribute node 'xmlns:xlink "http://www.w3.org/1999/xlink")
                 (dom-set-attribute node 'version edraw-svg-version))
             (edraw-dom-remove-attr node 'xmlns:xlink)
             (edraw-dom-remove-attr node 'version)))

         ;; xlink:href
         (if ver.1.1-p
             ;; Use xlink:href
             (edraw-dom-set-attribute-name node 'href 'xlink:href)

           ;; Use href
           (edraw-dom-set-attribute-name node 'xlink:href 'href)))))))

(defun edraw-svg-href-symbol ()
  (if (version<= edraw-svg-version "1.1")
      'xlink:href
    'href))

;;;; SVG View Box

(defconst edraw-svg-re-attr-viewbox
  (concat "\\`" edraw-svg-re-wsp-opt
          "\\(" edraw-svg-re-number "\\)" edraw-svg-re-comma-wsp
          "\\(" edraw-svg-re-number "\\)" edraw-svg-re-comma-wsp
          "\\(" edraw-svg-re-number "\\)" edraw-svg-re-comma-wsp
          "\\(" edraw-svg-re-number "\\)" edraw-svg-re-wsp-opt "\\'"))

(defun edraw-svg-parse-viewbox-string (viewbox)
  (when (and (stringp viewbox)
             (string-match edraw-svg-re-attr-viewbox viewbox))
    (list (match-string 1 viewbox)
          (match-string 2 viewbox)
          (match-string 3 viewbox)
          (match-string 4 viewbox))))
;; TEST: (edraw-svg-parse-viewbox-string "  11 , 22 , 33 , 44  ") => ("11" "22" "33" "44")
;; TEST: (edraw-svg-parse-viewbox-string "11 22 33 44") => ("11" "22" "33" "44")
;; TEST: (edraw-svg-parse-viewbox-string "  11 , 22 , 33 , 44, 55  ") => nil
;; TEST: (edraw-svg-parse-viewbox-string "") => nil
;; TEST: (edraw-svg-parse-viewbox-string nil) => nil

(defun edraw-svg-parse-viewbox (viewbox)
  (mapcar #'string-to-number (edraw-svg-parse-viewbox-string viewbox)))

;;;; SVG Element Creation

(defun edraw-svg-create (width height &rest attr-plist-and-children)
  (apply #'edraw-dom-element
         'svg
         `(width
           ,width
           height ,height
           xmlns "http://www.w3.org/2000/svg"
           ,@(when (version<= edraw-svg-version "1.1")
               (list
                'version edraw-svg-version
                'xmlns:xlink "http://www.w3.org/1999/xlink"))
           ,@attr-plist-and-children)))

(defun edraw-svg-rect (x y width height &rest attr-plist-and-children)
  "Create a `rect' element.
Attributes are specified by X, Y, WIDTH, HEIGHT, and ATTR-PLIST-AND-CHILDREN.

See `edraw-dom-element' for more information about ATTR-PLIST-AND-CHILDREN."
  (apply #'edraw-dom-element
         'rect
         `(x ,x y ,y width ,width height ,height ,@attr-plist-and-children)))

(defun edraw-svg-circle (cx cy r &rest attr-plist-and-children)
  "Create a `circle' element.
Attributes are specified by CX, CY, R, and ATTR-PLIST-AND-CHILDREN.

See `edraw-dom-element' for more information about ATTR-PLIST-AND-CHILDREN."
  (apply #'edraw-dom-element
         'circle
         `(cx ,cx cy ,cy r ,r ,@attr-plist-and-children)))

(defun edraw-svg-ellipse (cx cy rx ry &rest attr-plist-and-children)
  "Create an `ellipse' element.
Attributes are specified by CX, CY, RX, RY, and ATTR-PLIST-AND-CHILDREN.

See `edraw-dom-element' for more information about ATTR-PLIST-AND-CHILDREN."
  (apply #'edraw-dom-element
         'ellipse
         `(cx ,cx cy ,cy rx ,rx ry ,ry ,@attr-plist-and-children)))

(defun edraw-svg-line (x1 y1 x2 y2 &rest attr-plist-and-children)
  "Create a `line' element.
Attributes are specified by X1, Y1, X2, Y2, and ATTR-PLIST-AND-CHILDREN.

See `edraw-dom-element' for more information about ATTR-PLIST-AND-CHILDREN."
  (apply #'edraw-dom-element
         'line
         `(x1 ,x1 y1 ,y1 x2 ,x2 y2 ,y2 ,@attr-plist-and-children)))

(defun edraw-svg-path (d &rest attr-plist-and-children)
  "Create a `path' element.
Attributes are specified by D, and ATTR-PLIST-AND-CHILDREN.

See `edraw-dom-element' for more information about ATTR-PLIST-AND-CHILDREN."
  (apply #'edraw-dom-element
         'path
         `(d ,d ,@attr-plist-and-children)))

(defun edraw-svg-polygon (points &rest attr-plist-and-children)
  "Create a `polygon' element.
Attributes are specified by POINTS, and ATTR-PLIST-AND-CHILDREN.

POINTS is a string or a list of cons cell representing coordinates.

See `edraw-dom-element' for more information about ATTR-PLIST-AND-CHILDREN."
  (apply #'edraw-dom-element
         'polygon
         `(points
           ,(if (stringp points)
                points
              (mapconcat (lambda (xy) (format "%s %s" (car xy) (cdr xy)))
                         points " "))
           ,@attr-plist-and-children)))

(defun edraw-svg-polyline (points &rest attr-plist-and-children)
  "Create a `polyline' element.
Attributes are specified by POINTS, and ATTR-PLIST-AND-CHILDREN.

POINTS is a string or a list of cons cell representing coordinates.

See `edraw-dom-element' for more information about ATTR-PLIST-AND-CHILDREN."
  (apply #'edraw-dom-element
         'polyline
         `(points
           ,(if (stringp points)
                points
              (mapconcat (lambda (xy) (format "%s %s" (car xy) (cdr xy)))
                         points " "))
           ,@attr-plist-and-children)))

(defun edraw-svg-group (&rest attr-plist-and-children)
  "Create a `g' element.
Attributes and children are specified by ATTR-PLIST-AND-CHILDREN.

For example:
  (edraw-svg-group
  :class \"red-cross\"
  :stroke \"red\"
  :stroke-width 10
  (edraw-svg-line 0 -100 0 100)
  (edraw-svg-line -100 0 100 0))

See `edraw-dom-element' for more information about ATTR-PLIST-AND-CHILDREN."
  (apply #'edraw-dom-element 'g attr-plist-and-children))


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
           (or (dom-attr element (edraw-svg-href-symbol)) "")
           20 nil nil "...")))

(defun edraw-svg-group-summary (element)
  (format "group (%s children)" ;;@todo edraw-msg (require 'edraw-util)
          (length (dom-children element))))


;;;; SVG Shape Properties

;; Source:
;; - attr
;; - attr-fill-stroke
;; - attr-marker
;; - attr-update-text
;; - inner-text

;; Type:
;; - <number>
;;   - number
;;   - opacity
;;   - length
;;   - coordinate
;; - paint
;; - string
;; - (or <choice>...)
;; - marker
;; - text
;; - font-family

;; Flags
;; - required
;; - geometry

(defconst edraw-svg-elem-prop-number-types
  '(number opacity length coordinate))

(defconst edraw-svg-element-properties-common
  ;;Name Source Type Flags
  '((opacity attr opacity nil)
    (fill attr-fill-stroke paint nil)
    (fill-opacity attr opacity nil)
    (stroke attr-fill-stroke paint nil)
    (stroke-opacity attr opacity nil)
    (stroke-width attr length nil)
    (stroke-dasharray attr string nil)
    (stroke-dashoffset attr length nil)
    (style attr string nil)
    (transform attr string (geometry))))

(defconst edraw-svg-element-properties-path-common
  '((fill-rule attr (or "nonzero" "evenodd") nil)
    (stroke-linecap attr (or "butt" "round" "square") nil)
    (stroke-linejoin attr (or "miter" "round" "bevel") nil)
    (stroke-miterlimit attr number nil)))

(defconst edraw-svg-element-properties
  `((rect
     (x attr coordinate (required geometry))
     (y attr coordinate (required geometry))
     (width attr length (required geometry))
     (height attr length (required geometry))
     (rx attr length (geometry))
     (ry attr length (geometry))
     ,@edraw-svg-element-properties-common)
    (circle
     (cx attr coordinate (required geometry))
     (cy attr coordinate (required geometry))
     (r attr length (required geometry))
     ,@edraw-svg-element-properties-common)
    (ellipse
     (cx attr coordinate (required geometry))
     (cy attr coordinate (required geometry))
     (rx attr length (required geometry))
     (ry attr length (required geometry))
     ,@edraw-svg-element-properties-common)
    (path
     (d attr string (required geometry internal))
     ,@edraw-svg-element-properties-common
     ,@edraw-svg-element-properties-path-common
     (marker-start attr-marker marker nil)
     (marker-mid attr-marker marker nil)
     (marker-end attr-marker marker nil))
    (text
     (text inner-text text (required geometry))
     ;; librsvg does not support list-of-coordinates
     ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/183
     (x attr coordinate (required geometry))
     (y attr coordinate (required geometry))
     (dx attr coordinate (geometry))
     (dy attr coordinate (geometry))
     ;; librsvg does not support?
     ;;(rotate attr string nil)
     ;; librsvg does not support textLength
     ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/88
     ;;(textLength attr number nil)
     ;;(lengthAdjust attr (or "spacing" "spacingAndGlyphs") nil)
     (font-family attr font-family nil)
     (font-size attr number (geometry))
     (font-weight attr (or "normal" "bold" "bolder" "lighter") nil)
     (font-style attr (or "normal" "italic" "oblique") nil)
     (text-decoration attr (or "underline" "overline" "line-through") nil)
     (text-anchor attr (or "start" "middle" "end") (geometry))
     (writing-mode attr-update-text
                   (or "horizontal-tb" "vertical-rl" "vertical-lr") (geometry))
     ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/129
     ;;(baseline-shift attr number nil)
     (data-edraw-text-leading attr-update-text number (geometry))
     ,@edraw-svg-element-properties-common)
    (image
     (x attr coordinate (required geometry))
     (y attr coordinate (required geometry))
     (width attr length (required geometry))
     (height attr length (required geometry))
     ;;@todo should change dynamically depending on edraw-svg-version
     (xlink:href attr string ,(when (eq (edraw-svg-href-symbol) 'xlink:href) '(required)))
     (href attr string  ,(when (eq (edraw-svg-href-symbol) 'href) '(required)))
     (preserveAspectRatio attr string nil)
     (opacity attr opacity nil)
     (style attr string nil)
     (transform attr string (geometry)))
    (g
     ,@edraw-svg-element-properties-common
     ,@edraw-svg-element-properties-path-common)))

(defun edraw-svg-elem-prop-name (prop-def) (nth 0 prop-def))
(defun edraw-svg-elem-prop-source (prop-def) (nth 1 prop-def))
(defun edraw-svg-elem-prop-type (prop-def) (nth 2 prop-def))
(defun edraw-svg-elem-prop-flags (prop-def) (nth 3 prop-def))
(defun edraw-svg-elem-prop-required (prop-def)
  (when (memq 'required (edraw-svg-elem-prop-flags prop-def)) t))

(defun edraw-svg-elem-prop-number-p (prop-def)
  (memq (edraw-svg-elem-prop-type prop-def) edraw-svg-elem-prop-number-types))

(defun edraw-svg-element-get-property-info-list (element)
  (edraw-svg-element-get-property-info-list-by-tag (dom-tag element)))

(defun edraw-svg-element-get-property-info-list-by-tag (tag)
  (when-let ((prop-def-list (alist-get tag edraw-svg-element-properties)))
    (cl-loop for prop-def in prop-def-list
             for prop-type = (edraw-svg-elem-prop-type prop-def)
             collect
             (list :name (edraw-svg-elem-prop-name prop-def)
                   :type prop-type
                   :required (edraw-svg-elem-prop-required prop-def)
                   :flags (edraw-svg-elem-prop-flags prop-def)
                   :to-string #'edraw-svg-ensure-string-attr
                   :from-string #'identity
                   :number-p (edraw-svg-elem-prop-number-p prop-def)
                   :to-number (pcase prop-type
                                ('coordinate #'edraw-svg-attr-length-to-number)
                                ('length #'edraw-svg-attr-length-to-number)
                                ('number #'edraw-svg-attr-number-to-number)
                                ('opacity #'edraw-svg-attr-number-to-number)
                                (_ nil))
                   ))))
;; EXAMPLE: (edraw-svg-element-get-property-info-list-by-tag 'rect)

(defun edraw-svg-element-can-have-property-p (element prop-name)
  (edraw-svg-tag-can-have-property-p (dom-tag element) prop-name))

(defun edraw-svg-tag-can-have-property-p (tag prop-name)
  (when-let ((prop-def-list (alist-get tag edraw-svg-element-properties)))
    (seq-some (lambda (prop-def) (eq (edraw-svg-elem-prop-name prop-def)
                                     prop-name))
              prop-def-list)))

(defun edraw-svg-element-get-property (element prop-name deftbl)
  (when-let ((prop-def-list (alist-get (dom-tag element) edraw-svg-element-properties))
             (prop-def (assq prop-name prop-def-list)))
    (let* ((source (edraw-svg-elem-prop-source prop-def))
           (getter (intern
                    (concat "edraw-svg-element-get-" (symbol-name source)))))
      (funcall getter element prop-name deftbl))))

(defun edraw-svg-element-set-property (element prop-name value deftbl)
  (when-let ((prop-def-list (alist-get (dom-tag element) edraw-svg-element-properties))
             (prop-def (assq prop-name prop-def-list)))
    (let* ((source (edraw-svg-elem-prop-source prop-def))
           (setter (intern
                    (concat "edraw-svg-element-set-" (symbol-name source)))))
      (funcall setter element prop-name value deftbl))))

(defun edraw-svg-element-has-property-p (element prop-name deftbl)
  (not (null (edraw-svg-element-get-property element prop-name deftbl))))

;; Property Source

(defun edraw-svg-element-get-attr (element prop-name _deftbl)
  ;; nil means no property.
  ;; Return nil, string, or other stored types like a number.
  (dom-attr element prop-name))

(defun edraw-svg-element-set-attr (element prop-name value _deftbl)
  (cond
   ;; nil means no property.
   ((null value)
    (edraw-dom-remove-attr element prop-name))
   ;; x of text must by changed along with inner tspans.
   ((and (eq (dom-tag element) 'text)
         (eq prop-name 'x))
    (edraw-svg-text-set-x element value))
   ;; y of text must by changed along with inner tspans if vertical writing.
   ((and (eq (dom-tag element) 'text)
         (eq prop-name 'y))
    (edraw-svg-text-set-y element value))
   ;; Store as is. Avoid numerical errors.
   ((numberp value)
    (edraw-svg-set-attr-number element prop-name value))
   ((stringp value)
    (edraw-svg-set-attr-string element prop-name value))
   (t
    (dom-set-attribute element prop-name value))))

(defun edraw-svg-element-get-inner-text (element _prop-name _deftbl)
  (edraw-svg-text-get-text element))

(defun edraw-svg-element-set-inner-text (element _prop-name value _deftbl)
  (edraw-svg-text-set-text element value))

(defun edraw-svg-element-get-attr-update-text (element prop-name deftbl)
  (edraw-svg-element-get-attr element prop-name deftbl))

(defun edraw-svg-element-set-attr-update-text (element prop-name value deftbl)
  (edraw-svg-element-set-attr element prop-name value deftbl)
  (edraw-svg-text-update-text element))

(defun edraw-svg-element-get-attr-marker (element prop-name deftbl)
  (edraw-svg-get-marker-property element prop-name deftbl))

(defun edraw-svg-element-set-attr-marker (element prop-name value deftbl)
  (edraw-svg-set-marker-property element prop-name value deftbl))

(defun edraw-svg-element-get-attr-fill-stroke (element prop-name deftbl)
  (edraw-svg-element-get-attr element prop-name deftbl))

(defun edraw-svg-element-set-attr-fill-stroke (element prop-name value deftbl)
  (edraw-svg-element-set-attr element prop-name value deftbl)
  (edraw-svg-update-marker-properties element deftbl))


;;;; SVG Text Layout

(defun edraw-svg-text-update-text (element)
  (edraw-svg-text-set-text element (edraw-svg-text-get-text element)))

(defun edraw-svg-text-set-text (element text)
  (edraw-dom-remove-all-children element)

  (when (stringp text)
    (let ((lines (split-string text "\n")))
      (if (null (cdr lines))
          ;; single line
          (edraw-dom-append-child element (car lines)) ;; string
        ;; multi-line
        (edraw-svg-text--set-text-multiline element lines)))))

(defconst edraw-svg-text--line-class-name
  "edraw-text-line")

(defconst edraw-svg-text--line-class-name-re
  ;; `text-line' was used until 2024-03-10
  "\\`\\(?:edraw-text-line\\|text-line\\)\\'")

(defun edraw-svg-text--set-text-multiline (element lines)
  (let* ((vertical-p (edraw-svg-text-vertical-writing-p element))
         (negative-dir-p (eq (edraw-svg-text-writing-mode element)
                             'vertical-rl))
         (attr-col (if vertical-p 'y 'x))
         (col (or (car (edraw-svg-attr-length-list element attr-col)) 0))
         (attr-line-delta (if vertical-p 'dx 'dy))
         (leading (edraw-svg-attr-length element 'data-edraw-text-leading))
         (line-delta-unit (if leading "" "em"))
         (line-delta-step-abs (or leading 1))
         (line-delta-step (if negative-dir-p
                              (- line-delta-step-abs)
                            line-delta-step-abs))
         (line-delta 0))
    (dolist (line lines)
      (edraw-dom-element 'tspan
                         :parent element
                         :class edraw-svg-text--line-class-name
                         attr-col col
                         :attributes
                         (when (and (/= line-delta 0)
                                    (not (string-empty-p line)))
                           (list attr-line-delta
                                 (format "%s%s"
                                         line-delta
                                         line-delta-unit)))
                         ;; string
                         line)
      (unless (string-empty-p line)
        (setq line-delta 0))
      (cl-incf line-delta line-delta-step))))

(defun edraw-svg-text--element-content-to-string (element first-p)
  (cl-loop for node in (edraw-dom-children element)
           for curr-first = first-p
           ;; https://www.w3.org/TR/SVG11/text.html#TextElement
           concat
           (cond
            ((edraw-dom-element-p node)
             (pcase (edraw-dom-tag node)
               ('a
                (setq first-p nil)
                (edraw-svg-text--element-content-to-string node curr-first))
               ('tspan
                (setq first-p nil)
                (concat
                 (when (and (string-match-p edraw-svg-text--line-class-name-re
                                            (or (dom-attr node 'class) ""))
                            (not curr-first))
                   "\n")
                 ;; @todo Save NODE attributes and styles as text properties
                 (edraw-svg-text--element-content-to-string node curr-first)))
               ;; Ignore altGlyph, textPath, tref, animation elements and
               ;; descriptive elements
               ))
            ((stringp node)
             (setq first-p nil)
             node)
            ;; Ignore malformed node
            )))

(defun edraw-svg-text-get-text (element)
  (edraw-svg-text--element-content-to-string element t))

(defun edraw-svg-text-set-xy (element xy)
  (edraw-svg-text-set-x element (car xy))
  (edraw-svg-text-set-y element (cdr xy)))

(defun edraw-svg-text-set-x (element x)
  (edraw-svg-text-set-coord element 'x x))

(defun edraw-svg-text-set-y (element y)
  (edraw-svg-text-set-coord element 'y y))

(defun edraw-svg-text-set-coord (element attr new-x)
  (let* ((old-xs (or (edraw-svg-attr-length-list element attr) (list 0)))
         (old-x0 (car old-xs))
         (_ (if (stringp new-x)
                (edraw-svg-set-attr-string element attr new-x)
              (edraw-svg-set-attr-number element attr new-x)))
         (new-xs (or (edraw-svg-attr-length-list element attr) (list 0)))
         (new-x0 (car new-xs)))

    (dolist (child (edraw-dom-children element))
      (edraw-svg-text-set-coord--tspan child attr old-x0 new-x0))))

(defun edraw-svg-text-set-coord--tspan (element attr old-x0 new-x0)
  (when (and (edraw-dom-element-p element)
             (eq (edraw-dom-tag element) 'tspan))
    (let* ((old-xs (edraw-svg-attr-length-list element attr))
           (new-xs (mapcar (lambda (ox) (if (= ox old-x0)
                                            new-x0
                                          (+ (- ox old-x0) new-x0)))
                           old-xs)))
      (cond
       ((cdr new-xs)
        (edraw-svg-set-attr-string element attr
                                   (mapconcat #'edraw-to-string new-xs " ")))
       (new-xs
        (edraw-svg-set-attr-number element attr (car new-xs))))

      (dolist (child (edraw-dom-children element))
        (edraw-svg-text-set-coord--tspan child attr old-x0 new-x0)))))


;;;; SVG Defs

;; defs(1)<--(0..1)deftbl(1)-->(0..*)defref
;;                                  (0..1)`-->(1)def-element (e.g.,marker)
;;                                  (0..*)`-->(0..*)referrer-element (e.g.,path)


(defun edraw-svg-defs-as-deftbl (id)
  (edraw-svg-deftbl
   (edraw-dom-element 'defs :id id)))


;;;;; Definition and Referrers Pair


(defun edraw-svg-defref (def-element idnum)
  "Create a definition-referrers pair.

DEF-ELEMENT is an element under the defs element. For example,
one of the elements that is reused from others, such as <marker>
or <linearGradient>.

IDNUM is the identification number of DEF-ELEMENT."
  (list def-element idnum))

;; (<def-element> <idnum> . <referrers>)
(defmacro edraw-svg-defref-def-element (defref) `(car ,defref))
(defmacro edraw-svg-defref-idnum (defref) `(cadr ,defref))
(defmacro edraw-svg-defref-referrers (defref) `(cddr ,defref))

(defun edraw-svg-defref-add-referrer (defref referrer-element)
  "Add REFERRER-ELEMENT that references the definition element of DEFREF."
  (push referrer-element (edraw-svg-defref-referrers defref)))

(defun edraw-svg-defref-remove-referrer (defref referrer-element)
  "Remove REFERRER-ELEMENT that references the definition element of DEFREF.

The same ELEMENT may exist multiple times in the list, in which
case only the first one is removed."
  (cl-callf2 cl-delete referrer-element
             (edraw-svg-defref-referrers defref) :count 1))

(defun edraw-svg-defref-unreferenced-p (defref)
  (null (edraw-svg-defref-referrers defref)))

(defun edraw-svg-def-element-equal-p (a b)
  (edraw-dom-equal a b '(id) nil nil))

(defun edraw-svg-def-element-url (defref additional-info)
  (format "url(#edraw-def-%s-%s)"
          (edraw-svg-defref-idnum defref)
          additional-info))

(defun edraw-svg-def-element-id (defref additional-info)
  (format "edraw-def-%s-%s" (edraw-svg-defref-idnum defref) additional-info))

(defun edraw-svg-def-element-id-to-idnum (id-attr)
  (and (stringp id-attr)
       (string-match "\\`edraw-def-\\([0-9]+\\)-\\([^)]+\\)\\'" id-attr)
       (string-to-number (match-string 1 id-attr))))

(defun edraw-svg-def-element-url-to-idnum (url)
  (and (stringp url)
       (string-match "\\`url(#edraw-def-\\([0-9]+\\)-\\([^)]+\\))\\'" url)
       (string-to-number (match-string 1 url))))

(defun edraw-svg-def-element-url-to-additional-info (url)
  (and (stringp url)
       (string-match "\\`url(#edraw-def-\\([0-9]+\\)-\\([^)]+\\))\\'" url)
       (match-string-no-properties 2 url)))


;;;;; Definition and Referrers Table


(defun edraw-svg-deftbl (defs-element)
  "Create a definition-referrers table.

DEFS-ELEMENT is a <defs> element for storing definitions."
  (list defs-element))

;; (<defs-element> . <defref-list>)
(defmacro edraw-svg-deftbl-defs-element (deftbl) `(car ,deftbl))
(defmacro edraw-svg-deftbl-defrefs (deftbl) `(cdr ,deftbl))
(defun edraw-svg-deftbl-defrefs--head (deftbl) deftbl)

(defun edraw-svg-deftbl-insert-with-unused-idnum (deftbl def-element)
  "Insert DEF-ELEMENT into DEFTBL.

Return an `edraw-svg-defref' object assigned an unused ID number.

This function does not check whether DEF-ELEMENT already exists
in DEFTBL, so check beforehand if you need it."
  (cl-loop for prev-cell on (edraw-svg-deftbl-defrefs--head deftbl)
           for idnum from 0
           when (or (null (cdr prev-cell))
                    (/= (edraw-svg-defref-idnum (cadr prev-cell)) idnum))
           ;; Keep ID number order
           return (let ((defref (edraw-svg-defref def-element idnum)))
                    (push defref (cdr prev-cell))
                    defref)))

(defun edraw-svg-deftbl-add-ref (deftbl def-element referrer-element
                                        additional-info)
  "Add a reference to the definition element DEF-ELEMENT.

If a definition identical to DEF-ELEMENT already exists in
DEFTBL, return a reference to it. If not, add DEF-ELEMENT to the
defs element targeted by DEFTBL and return a reference to it.

DEFTBL records that REFERRER-ELEMENT refers to DEF-ELEMENT.

The string converted from ADDITIONAL-INFO is concatenated to the
end of the id attribute of DEF-ELEMENT.
See `edraw-svg-def-element-id' and `edraw-svg-def-element-url'."
  (if-let ((defref (assoc def-element
                          (edraw-svg-deftbl-defrefs deftbl)
                          #'edraw-svg-def-element-equal-p)))
      (progn
        (edraw-svg-defref-add-referrer defref referrer-element)
        (edraw-svg-def-element-url defref additional-info))
    (let ((defref (edraw-svg-deftbl-insert-with-unused-idnum deftbl
                                                             def-element)))
      ;; Add a new definition element
      (edraw-svg-defref-add-referrer defref referrer-element)
      (edraw-svg-set-attr-string def-element 'id
                                 (edraw-svg-def-element-id defref
                                                           additional-info))
      (edraw-dom-append-child (edraw-svg-deftbl-defs-element deftbl)
                              def-element)
      (edraw-svg-def-element-url defref additional-info))))

(defun edraw-svg-deftbl-remove-ref-by-idnum (deftbl idnum referrer-element)
  (let ((cell (edraw-svg-deftbl-defrefs--head deftbl)))
    (while (and (cdr cell)
                (not (= (edraw-svg-defref-idnum (cadr cell)) idnum)))
      (setq cell (cdr cell)))
    (when (cdr cell)
      (let ((defref (cadr cell)))
        (edraw-svg-defref-remove-referrer defref referrer-element)
        ;; when no referrer
        (when (edraw-svg-defref-unreferenced-p defref)
          ;; remove definition element
          (edraw-dom-remove-node
           (edraw-svg-deftbl-defs-element deftbl)
           (edraw-svg-defref-def-element defref))
          ;; remove defref pair
          (setcdr cell (cddr cell)))))))

(defun edraw-svg-deftbl-get-defref-by-idnum (deftbl idnum)
  (seq-find (lambda (defref) (= (edraw-svg-defref-idnum defref) idnum))
            (edraw-svg-deftbl-defrefs deftbl)))

(defun edraw-svg-deftbl-add-ref-by-idnum (deftbl idnum referrer-element)
  (when-let ((defref (edraw-svg-deftbl-get-defref-by-idnum deftbl idnum)))
    (edraw-svg-defref-add-referrer defref referrer-element)))

(defun edraw-svg-deftbl-remove-ref-by-url (deftbl url element)
  (when-let ((idnum (edraw-svg-def-element-url-to-idnum url)))
    (edraw-svg-deftbl-remove-ref-by-idnum deftbl idnum element)))

(defun edraw-svg-deftbl-get-defref-by-url (deftbl url)
  (when-let ((idnum (edraw-svg-def-element-url-to-idnum url)))
    (edraw-svg-deftbl-get-defref-by-idnum deftbl idnum)))

(defun edraw-svg-deftbl-from-dom (defs-element body-node)
  "Create deftbl from an existing SVG DOM.

DEFS-ELEMENT is a defs element that has elements referenced from
elements in BODY-NODE. Its child elements should have an id
attribute as generated by `edraw-svg-def-element-id'.

BODY-NODE is a element that contains graphic elements and
contains references to the definition elements in DEFS-ELEMENT.

Currently, the only attributes that detect references are those
specified by the `edraw-svg-deftbl-target-attributes' constant."
  (let ((deftbl (edraw-svg-deftbl defs-element))
        defref-list)
    ;; Collect definitions
    (dolist (def (dom-children defs-element))
      (when-let ((idnum (edraw-svg-def-element-id-to-idnum (dom-attr def 'id))))
        (push (edraw-svg-defref def idnum) defref-list)))
    ;; Sort and assign
    (setcdr (edraw-svg-deftbl-defrefs--head deftbl)
            (sort defref-list (lambda (defref1 defref2)
                                (< (edraw-svg-defref-idnum defref1)
                                   (edraw-svg-defref-idnum defref2)))))
    ;; Collect references
    (edraw-svg-deftbl-from-dom--collect-references deftbl body-node)
    ;; Remove unreferenced definitions
    (edraw-svg-deftbl-remove-unreferenced-definitions deftbl)

    deftbl))

(defconst edraw-svg-deftbl-target-attributes
  '(marker-start marker-mid marker-end)
  "list of attributes that can have references to definitions.

Note: In the future, this value may include a wide variety of
attributes that are not limited to markers.")

(defun edraw-svg-deftbl-from-dom--collect-references (deftbl dom)
  (when (edraw-dom-element-p dom) ;;exclude text nodes
    ;; Collect from attributes
    (dolist (attr (dom-attributes dom))
      (when (member (car attr) edraw-svg-deftbl-target-attributes)
        (when-let ((idnum (edraw-svg-def-element-url-to-idnum (cdr attr))))
          (edraw-svg-deftbl-add-ref-by-idnum deftbl idnum dom))))
    ;; Collect from children
    (dolist (child (dom-children dom))
      (edraw-svg-deftbl-from-dom--collect-references deftbl child))))

(defun edraw-svg-deftbl-remove-unreferenced-definitions (deftbl)
  ;; Remove unreferenced DEF-ELEMENTs from DEFS-ELEMENT
  (let ((defs-element (edraw-svg-deftbl-defs-element deftbl)))
    (dolist (defref (edraw-svg-deftbl-defrefs deftbl))
      (when (edraw-svg-defref-unreferenced-p defref)
        (edraw-dom-remove-node defs-element
                               (edraw-svg-defref-def-element defref)))))
  ;; Remove unreferenced DEFREFs from DEFTBL
  (cl-callf2 cl-delete-if #'edraw-svg-defref-unreferenced-p
             (edraw-svg-deftbl-defrefs deftbl)))


(defun edraw-svg-deftbl-update-referrer-property (element prop-name deftbl
                                                          &optional src-deftbl)
  "Update reference to definition element in the property PROP-NAME
of ELEMENT.

Get the value of the property PROP-NAME that can be a reference to a
definition element (under SRC-DEFTBL) and set it again (under DEFTBL).

If SRC-DEFTBL is specified, the referenced defs element (deftbl)
can be changed."
  (when-let ((value (edraw-svg-element-get-property element
                                                    prop-name
                                                    (or src-deftbl deftbl))))
    ;; Remove reference to SRC-DEFTBL
    (when (and src-deftbl (not (eq src-deftbl deftbl)))
      (edraw-svg-element-set-property element prop-name nil src-deftbl))
    ;; Add reference to DEFTBL
    (edraw-svg-element-set-property element prop-name value deftbl)))

(defun edraw-svg-deftbl-update-referrer-element (element deftbl
                                                         &optional src-deftbl)
  "Update the ELEMENT's properties that can be references to the
defining element.

Properties that can be references to definition elements are
determined by the `edraw-svg-deftbl-target-attributes' variable.

Apply the function `edraw-svg-deftbl-update-referrer-property' to
the properties."
  (dolist (prop-name edraw-svg-deftbl-target-attributes)
    (edraw-svg-deftbl-update-referrer-property element prop-name
                                               deftbl src-deftbl)))

(defun edraw-svg-deftbl-update-referrers-in-dom (dom deftbl
                                                     &optional src-deftbl)
  (when (edraw-dom-element-p dom)
    (edraw-svg-deftbl-update-referrer-element dom deftbl src-deftbl)
    (dolist (child (edraw-dom-children dom))
      (edraw-svg-deftbl-update-referrers-in-dom child deftbl src-deftbl))))

;;;; SVG Marker

(defconst edraw-svg-marker-arrow-overhang
  (/ (*
      6 ;;markerWidth
      4) ;;arrow tip position
     20.0)) ;;viewBox width

(defun edraw-svg-marker-arrow-overhang (marker stroke-width)
  (/ (*
      stroke-width
      (edraw-svg-marker-prop-number marker 'markerWidth 6)
      4) ;;arrow tip position
     20.0)) ;;viewBox width

(defun edraw-svg-marker-arrow-props (marker-attrs)
  (list
   (cons 'markerWidth (alist-get 'markerWidth marker-attrs "6"))
   (cons 'markerHeight (alist-get 'markerHeight marker-attrs "6"))
   (cons 'refX (alist-get 'refX marker-attrs "0"))))

(defun edraw-svg-marker-arrow-create (prop-name element marker)
  (edraw-dom-element
   'marker
   :markerWidth (edraw-svg-marker-prop-str marker 'markerWidth "6")
   :markerHeight (edraw-svg-marker-prop-str marker 'markerHeight "6")
   :preserveAspectRatio "none"
   :viewBox "-10 -10 20 20"
   :refX (edraw-svg-marker-prop-str marker 'refX "0")
   :refY "0"
   :orient "auto"
   :stroke "none"
   :fill
   ;; @todo I want to use context-stroke and remove edraw-svg-update-marker-properties
   ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/618
   (let ((stroke (dom-attr element 'stroke)))
     (if (or (null stroke) (equal stroke "none"))
         "none" ;;stroke may change later
       stroke))
   ;; Children
   (edraw-svg-path
    ;; @todo I want to use auto-start-reverse
    ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/484
    (if (eq prop-name 'marker-start)
        "M10,-7 10,7 -4,0Z" ;; <|
      "M-10,-7 -10,7 4,0Z")))) ;; |>

(defun edraw-svg-marker-circle-props (marker-attrs)
  (list
   (cons 'markerWidth (alist-get 'markerWidth marker-attrs "4"))
   (cons 'markerHeight (alist-get 'markerHeight marker-attrs "4"))
   (cons 'refX (alist-get 'refX marker-attrs "0"))))

(defun edraw-svg-marker-circle-create (_prop-name element marker)
  (edraw-dom-element
   'marker
   :markerWidth (edraw-svg-marker-prop-str marker 'markerWidth "4")
   :markerHeight (edraw-svg-marker-prop-str marker 'markerHeight "4")
   :preserveAspectRatio "none"
   :viewBox "-5 -5 10 10"
   :refX (edraw-svg-marker-prop-str marker 'refX "0")
   :refY "0"
   :orient "auto"
   :stroke "none"
   :fill
   ;; @todo I want to use context-stroke
   ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/618
   (let ((stroke (dom-attr element 'stroke)))
     (if (or (null stroke) (equal stroke "none"))
         "none" ;;stroke may change later
       stroke))
   ;; Children
   (edraw-svg-circle "0" "0" "4")))

(defconst edraw-svg-marker-types
  `(("arrow"
     :overhang edraw-svg-marker-arrow-overhang
     :creator edraw-svg-marker-arrow-create
     :get-props edraw-svg-marker-arrow-props
     :prop-info-list
     ((:name markerWidth :type number :required nil :flags nil :to-string edraw-svg-ensure-string-attr :from-string identity :number-p t :to-number edraw-svg-attr-number-to-number)
      (:name markerHeight :type number :required nil :flags nil :to-string edraw-svg-ensure-string-attr :from-string identity :number-p t :to-number edraw-svg-attr-number-to-number)
      (:name refX :type number :required nil :flags nil :to-string edraw-svg-ensure-string-attr :from-string identity :number-p t :to-number edraw-svg-attr-number-to-number)))
    ("circle"
     :creator edraw-svg-marker-circle-create
     :get-props edraw-svg-marker-circle-props
     :prop-info-list
     ((:name markerWidth :type number :required nil :flags nil :to-string edraw-svg-ensure-string-attr :from-string identity :number-p t :to-number edraw-svg-attr-number-to-number)
      (:name markerHeight :type number :required nil :flags nil :to-string edraw-svg-ensure-string-attr :from-string identity :number-p t :to-number edraw-svg-attr-number-to-number)
      (:name refX :type number :required nil :flags nil :to-string edraw-svg-ensure-string-attr :from-string identity :number-p t :to-number edraw-svg-attr-number-to-number)))
    ;; Ignore "" or "none"
    ))

(defun edraw-svg-marker-type-all ()
  (mapcar #'car edraw-svg-marker-types))

(defun edraw-svg-marker-type-next (type)
  (if (null type)
      (caar edraw-svg-marker-types)
    (cl-loop for x on edraw-svg-marker-types
             when (equal (caar x) type)
             return (caadr x))))
;; TEST: (edraw-svg-marker-type-next nil) => "arrow"
;; TEST: (edraw-svg-marker-type-next "arrow") => "circle"
;; TEST: (edraw-svg-marker-type-next "circle") => nil

(defun edraw-svg-marker-prop-info-list (type)
  (when-let ((props (alist-get type edraw-svg-marker-types nil nil #'equal)))
    (plist-get props :prop-info-list)))

(defun edraw-svg-marker-type-funcall (type key &rest args)
  (when-let ((props (alist-get type edraw-svg-marker-types nil nil #'equal)))
    (when-let ((fun (plist-get props key)))
      (apply fun args))))

(defun edraw-svg-marker-create-element (marker prop-name referrer-element)
  (edraw-svg-marker-type-funcall (edraw-svg-marker-type marker) :creator
                                 prop-name referrer-element
                                 marker))

(defun edraw-svg-marker-from-element (element prop-name deftbl)
  "Create a marker descriptor from the attribute PROP-NAME of the ELEMENT."
  (let ((value (dom-attr element prop-name)))
    (when (and value
               (stringp value)
               (not (string= value "none"))
               (not (string= value "")))
      (let ((marker-type (edraw-svg-def-element-url-to-additional-info value))
            (marker-element
             (edraw-svg-defref-def-element
              (edraw-svg-deftbl-get-defref-by-url deftbl value))))
        (when marker-type
          (edraw-svg-marker
           marker-type
           (when marker-element
             (edraw-svg-marker-type-funcall
              marker-type :get-props (dom-attributes marker-element)))))))))

(defun edraw-svg-marker-overhang (element prop-name deftbl)
  (when-let ((marker (edraw-svg-marker-from-element element prop-name deftbl)))
    (edraw-svg-marker-type-funcall (edraw-svg-marker-type marker) :overhang
                                   marker
                                   ;;@todo support group stroke-width
                                   (or (edraw-svg-attr-length element 'stroke-width) 1)
                                   )))


(defun edraw-svg-marker (marker-type props)
  "Create a marker descriptor.

MARKER-TYPE is a type name in `edraw-svg-marker-types'.

PROPS is an alist of properties defined by the MARKER-TYPE."
  (nconc (list 'marker marker-type) props))

(defun edraw-svg-marker-p (object)
  (eq (car-safe object) 'marker))

(defun edraw-svg-marker-type (marker)
  "Return marker type."
  (when (edraw-svg-marker-p marker)
    (cadr marker)))

(defun edraw-svg-marker-props (marker)
  "Return alist of marker property."
  (when (edraw-svg-marker-p marker)
    (cddr marker)))

(defun edraw-svg-marker-props-head (marker)
  (when (edraw-svg-marker-p marker)
    (cdr marker)))

(defun edraw-svg-marker-prop-str (marker key default)
  (edraw-svg-ensure-string-attr
   (alist-get key (edraw-svg-marker-props marker) default)))

(defun edraw-svg-marker-prop-number (marker key default)
  (let ((value (alist-get key (edraw-svg-marker-props marker))))
    (if (and (stringp value)
             (string-match-p "\\`-?\\([0-9]\\|\\.[0-9]\\)" value))
        (string-to-number value)
      default)))

(defun edraw-svg-set-marker-property (element prop-name marker deftbl)
  "Set the property PROP-NAME of the SVG ELEMENT to MARKER."
  ;; String to marker descriptor
  (when (stringp marker)
    (setq marker (edraw-svg-marker marker nil))) ;; Including "" or "none"

  ;; Remove reference to current marker
  (edraw-svg-deftbl-remove-ref-by-url
   deftbl
   (dom-attr element prop-name) ;;url(#...) or "none" or nil
   element)
  ;; Add reference to marker
  (let ((marker-element
         (edraw-svg-marker-create-element marker prop-name element)))
    (if marker-element
        (edraw-svg-set-attr-string element
                                   prop-name
                                   (edraw-svg-deftbl-add-ref
                                    deftbl marker-element element
                                    (edraw-svg-marker-type marker)))
      (edraw-dom-remove-attr element
                             prop-name))))

(defun edraw-svg-get-marker-property (element prop-name deftbl)
  "Return marker descriptor set in the property PROP-NAME of the SVG ELEMENT"
  ;; Return marker descriptor
  (edraw-svg-marker-from-element element prop-name deftbl)
  ;; Return only marker type (old behavior)
  ;;(edraw-svg-def-element-url-to-additional-info (dom-attr element prop-name))
  )

(defun edraw-svg-update-marker-property (element prop-name deftbl
                                                 &optional src-deftbl)
  (when-let ((marker (edraw-svg-marker-from-element element prop-name
                                                    (or src-deftbl deftbl))))
    (edraw-svg-set-marker-property element prop-name marker deftbl)))

(defun edraw-svg-update-marker-properties (element deftbl
                                                   &optional src-deftbl)
  (edraw-svg-update-marker-property element 'marker-start deftbl src-deftbl)
  (edraw-svg-update-marker-property element 'marker-mid deftbl src-deftbl)
  (edraw-svg-update-marker-property element 'marker-end deftbl src-deftbl))



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
  (let* ((xs (or (edraw-svg-attr-length-list element 'x) (list 0)))
         (ys (or (edraw-svg-attr-length-list element 'y) (list 0)))
         (anchor-x (car xs))
         (anchor-y (car ys))
         ;;@todo support dx, dy
         (text (edraw-svg-text-get-text element));;@todo analyze decendant nodes
         (lines (split-string text "\n"))
         (max-width (cl-loop for line in lines
                             maximize (string-width line)))
         (text-anchor (or (dom-attr element 'text-anchor) "start"))
         (font-size (or (edraw-svg-attr-length element 'font-size)
                        edraw-svg-attr-default-font-size)) ;;@todo default font size
         (font-ascent (/ (* font-size 80) 100)) ;;@todo default font ascent
         (writing-mode (edraw-svg-text-writing-mode element))
         (vertical-p (edraw-svg-text-vertical-writing-p element))
         (vertical-rl-p (eq writing-mode 'vertical-rl)))
    ;;@todo direction=rtl
    ;;@todo support style
    ;;@todo support baseline spec. (but librsvg does not support baseline spec https://gitlab.gnome.org/GNOME/librsvg/-/issues/414 )
    ;;@todo support list-of-coordinates x=, y=, dx=, dy= (librsvg does not support https://gitlab.gnome.org/GNOME/librsvg/-/issues/183 )
    ;;@todo support rotate (librsvg does not suppor ?)
    ;;@todo support textLength (librsvg does not support https://gitlab.gnome.org/GNOME/librsvg/-/issues/88 )

    (let* ((anchor-col (if vertical-p anchor-y anchor-x))
           (anchor-line (if vertical-p anchor-x anchor-y))
           (num-lines (length lines))
           (leading (or (edraw-svg-attr-length element 'data-edraw-text-leading)
                        font-size)) ;; NOTE: Can be negative
           (leading-total (if (= num-lines 0) 0 (* (1- num-lines) leading)))
           (leading-total-abs (abs leading-total))
           (leading-total-neg (- (min leading-total 0)))
           (text-w (* 0.5 font-size max-width))
           (text-h
            (if (= num-lines 0)
                0
              (max 0
                   (+ font-size
                      leading-total-abs
                      (if edraw-svg-text-contents-aabb--remove-last-descent
                          (- (- font-size font-ascent)) 0)))))
           (text-col (- anchor-col
                        (* text-w (pcase text-anchor
                                    ("middle" 0.5) ("end" 1) (_ 0)))))
           (text-line (if vertical-p
                          (if vertical-rl-p
                              (+ (- anchor-line text-h) (* 0.5 font-size)
                                 leading-total-neg)
                            ;; vertical-lr
                            (- anchor-line (* 0.5 font-size) leading-total-neg))
                        (- anchor-line font-ascent leading-total-neg))))
      (if vertical-p
          (edraw-rect-xywh text-line text-col text-h text-w)
        (edraw-rect-xywh text-col text-line text-w text-h)))))

(defun edraw-svg-text-writing-mode (element)
  ;;@todo support style attribute
  ;;@todo support inherit
  ;; https://www.w3.org/TR/css-writing-modes-3/#svg-writing-mode
  (pcase (dom-attr element 'writing-mode)
    ((or "horizontal-tb" "lr" "lr-tb" "rl" "rl-tb") 'horizontal-tb)
    ((or "vertical-rl" "tb-rl" "tb") 'vertical-rl)
    ("vertical-lr" 'vertical-lr)
    (_ 'horizontal-tb)))

(defun edraw-svg-text-vertical-writing-p (element)
  (memq (edraw-svg-text-writing-mode element) '(vertical-rl vertical-lr)))


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
  (edraw-svg-text-set-x element
                        (+ (or (car (edraw-svg-attr-length-list element 'x)) 0)
                           (car xy)))
  (edraw-svg-text-set-y element
                        (+ (or (car (edraw-svg-attr-length-list element 'y)) 0)
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
                           (or (edraw-svg-attr-length element 'stroke-width) 1)
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
    (edraw-svg-rect bl bt bw bh
                    :parent svg
                    :id id
                    :attributes attrs)))

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

        (let ((svg (edraw-svg-create svg-width svg-height)))
          (when background
            (edraw-svg-shape-thumbnail-cover
             svg svg-width svg-height
             background pl pt cw ch "background"))

          ;; Body
          (edraw-svg-group :parent svg
                           :id "body"
                           :transform
                           (concat
                            (format "translate(%s %s)"
                                    (+ pl (/ (- cw (* bw scale)) 2))
                                    (+ pt (/ (- ch (* bh scale)) 2)))
                            " "
                            (format "scale(%s)" scale)
                            " "
                            (format "translate(%s %s)" (- bl) (- bt)))
                           ;; Children
                           shape)

          (when foreground
            (edraw-svg-shape-thumbnail-cover
             svg svg-width svg-height
             foreground pl pt cw ch "foreground"))

          svg)))))



(provide 'edraw-dom-svg)
;;; edraw-dom-svg.el ends here
