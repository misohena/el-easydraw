;;; edraw-org.el --- Embed Drawing Editor for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

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

;; To use this package, add this to your init.el:
;; (with-eval-after-load 'org
;;   (require 'edraw-org)
;;   (edraw-org-setup-default))

;;; Code:

(require 'org)
(require 'org-element)
(require 'edraw-util)


;;;; Autoload

(autoload 'edraw-org-edit-link "edraw-org-edit" "" t)
(autoload 'edraw-org-link-copy-contents-at-point "edraw-org-edit" "" t)
(autoload 'edraw-org-export-html-link "edraw-org-export-html" "" nil)
(autoload 'edraw-org-export-latex-link "edraw-org-export-latex" "" nil)
(autoload 'edraw-org-export-odt-link "edraw-org-export-odt" "" nil)

;;;; Setup

(defun edraw-org-setup-default ()
  (edraw-org-setup-exporter)
  (edraw-org-setup-inline-images)
  (edraw-org-setup-text-conversion))

(defun edraw-org-setup-exporter ()
  (edraw-org-link-setup-exporter))

(defun edraw-org-setup-inline-images ()
  (add-hook 'org-mode-hook 'edraw-org-link-image-mode))



;;;; Customize

(defgroup edraw-org nil
  "Embeds drawing editors in Org documents."
  :tag "Edraw Org"
  :prefix "edraw-org-"
  :group 'edraw
  :group 'org)

(defcustom edraw-org-link-compress-data-p t
  "Non-nil means compress SVG on save to data links."
  :group 'edraw-org
  :type '(boolean))

(defcustom edraw-org-link-compress-file-p nil
  "Non-nil means compress SVG on save to file.

NOTE: Web browsers do not support compressed SVG files."
  :group 'edraw-org
  :type '(boolean))

(defcustom edraw-org-link-image-gunzip-p t
  "Non-nil means that gzip compression is decompressed when displaying
inline images.

Emacs supports svgz, but it may not be displayed unless gzip data
is expanded. Since the cause is not clear, it is expanded by default."
  :group 'edraw-org
  :type '(boolean))

(defcustom edraw-org-link-image-max-size '(0.92 . 0.92)
  "Maximum size of inline images."
  :group 'edraw-org
  :type
  '(choice
    (const :tag "No limit" nil)
    (integer :tag "Number of pixels")
    (float :tag "Ratio to frame size")
    (cons :tag "Width and Height"
          (choice :tag "Width"
                  (const :tag "No limit" nil)
                  (integer :tag "Number of pixels")
                  (float :tag "Ratio to frame width")
                  )
          (choice :tag "Height"
                  (const :tag "No limit" nil)
                  (integer :tag "Number of pixels")
                  (float :tag "Ratio to frame height")))))

(defcustom edraw-org-link-image-link-formats '(bracket)
  "A list of link formats to display inline.

The link formats that can be specified are the types of links
recognized by org-element: `bracket', `angle', or `plain'.

Displaying `angle' and `plain' links inline will get in the way
when editing the contents of bracket links, so by default only
`bracket' links are displayed."
  :group 'edraw-org
  :type
  '(set :tag "Link formats"
        (const bracket)
        (const angle)
        (const plain)))


;;;; Link Type

;; <link> ::= "[[edraw:" <link-props> "]]" | "[["..."][edraw:" <link-props> "]]"
;; <link-props> ::= <empty> | <property> ( ";" <property> )*
;; <property> ::= <prop-name> "=" <prop-value>
;; <prop-name> ::= [-_0-9a-zA-Z]+
;; <prop-value> ::= (<escaped-char> | [^];])*
;; <escape-seq> ::= "\\;" | "\\\\" | <org-link-escape-seq>
;; <org-link-escape-seq> ::= "\\[" | "\\]"

;; Examples:
;; [[edraw:data=H4sICEYpKWEAA2Euc3ZnADWOTQ7CIBCF9ybeYTL7FrS6MYx3aQGBFG0zEGlvLwZdfW/emz+V3g62Z3wlQp/zehOilNKXoV/YibOUUtQOBG+D85nwJCVCCSb7pu/HA4ByEAyhNTyWblrM3uwa6MA6WtAb4bUO6r2RCS8VKfMyW8Ipjnr+l91v+4DwCDESsjUIoh0SrlJ9X6r8AGxkeye8AAAA]] (gzipped SVG data)
;; [[edraw:some-option=value;file=./example.edraw]]
;; [[*Inbox][edraw:file=./example.edraw]]
;;

(defconst edraw-org-link-type "edraw")

(defun edraw-org-link-register-type ()
  "Register the link type for edraw in org-mode."
  (org-link-set-parameters
   edraw-org-link-type
   :follow 'edraw-org-edit-link
   :export 'edraw-org-link-export
   :help-echo 'edraw-org-link-help-echo
   :activate-func 'edraw-org-link-activate-func))

(with-eval-after-load 'org
  ;; Registering the link type is necessary for org-element-context to
  ;; return the correct link type.
  (edraw-org-link-register-type))

(defun edraw-org-link-help-echo (_window object pos)
  (when (bufferp object)
    (with-current-buffer object
      (save-excursion
        (goto-char pos)
        (when-let* ((link-object (edraw-org-link-at-point))
                    (link-props (car (edraw-org-link-object-link-properties
                                      link-object t))))
          (if-let* ((file (edraw-org-link-prop-file link-props)))
              (format "edraw:file=%s" file)
            (if-let* ((data (edraw-org-link-prop-data link-props)))
                (format "edraw:data=(%s chars)" (length data)))))))))

(defun edraw-org-link-activate-func (link-beg link-end _path _has-brackets-p
                                              &rest _future-add)
  (edraw-org-link-update-mouse-face link-beg link-end))

;;;;; Link Properties

(defun edraw-org-link-unescape (value &optional replace-brackets-p)
  (replace-regexp-in-string
   (if replace-brackets-p "\\\\\\([][;\\\\]\\)" "\\\\\\([;\\\\]\\)")
   "\\1" value t nil))

(defun edraw-org-link-escape (value &optional replace-brackets-p)
  (replace-regexp-in-string
   (if replace-brackets-p "\\([][;\\\\]\\)" "\\([;\\\\]\\)")
   "\\\\\\1" value t nil))

(defun edraw-org-link-props-parse (path &optional in-description-p noerror)
  (when path
    (save-match-data
      ;; Remove "edraw:" at the beginning of PATH just in case.
      ;; If the link type is not registered, edraw: may remain at the beginning.
      (setq path (string-remove-prefix (format "%s:" edraw-org-link-type) path))

      ;; Convert "A=B;C=D;..." to ((A . B) (C . D) ...)
      (delq
       nil
       (mapcar
        (lambda (prop)
          (if (string-match "\\`\\([^=]*\\)=\\(.*\\)\\'" prop)
              (let ((name (string-trim (match-string 1 prop)))
                    (value (edraw-org-link-unescape
                            (string-trim (match-string 2 prop))
                            ;; If PATH is in the description part,
                            ;; brackets have not yet been unescaped
                            in-description-p)))
                (if (string= name (edraw-org-link-path-terminator-pname))
                    ;; Remove dummy property
                    nil
                  (cons name value)))
            (if noerror
                nil
              (error "Invalid link format: %s" path))))
        (split-string path ";" t "[ \t\n\r]+"))))))

(defun edraw-org-link-props-to-string (link-props)
  ;; Convert ((A . B) (C . D) ...) to "A=B;C=D;..."
  (mapconcat
   (lambda (prop)
     (concat
      (car prop)
      "="
      (edraw-org-link-escape (cdr prop) t)))
   link-props
   ";"))

(defun edraw-org-link-prop-get (key-str link-props)
  (alist-get key-str link-props nil nil #'string=))

(defun edraw-org-link-prop-html-tag (link-props)
  (when-let* ((str (edraw-org-link-prop-get "html-tag" link-props)))
    (intern str)))

(defun edraw-org-link-prop-data (link-props)
  (edraw-org-link-prop-get "data" link-props))

(defun edraw-org-link-prop-file (link-props)
  (when-let* ((file-spec (edraw-org-link-prop-get "file" link-props)))
    (edraw-org-link-filter-file-path file-spec)))

(defun edraw-org-link-filter-file-path (path)
  path)

(defun edraw-org-link-props-image-data-or-file (link-props)
  "Create arguments to pass to `create-image' from LINK-PROPS.

Return a cons cell of the form (FILE-OR-DATA . DATA-P)."
  (if-let* ((data (edraw-org-link-prop-data link-props)))
      (ignore-errors (cons
                      ;; see: https://github.com/misohena/el-easydraw/issues/5
                      (if edraw-org-link-image-gunzip-p
                          (edraw-decode-string data t)
                        (base64-decode-string data))
                      t))
    (if-let* ((file (edraw-org-link-prop-file link-props)))
        (if (file-exists-p file) (cons (expand-file-name file) nil)))))

;;;;; Link Object

(defun edraw-org-link-at (pos)
  "Return the link object at POS.
See `edraw-org-link-at-point'."
  (save-excursion
    (goto-char pos)
    (edraw-org-link-at-point)))

(defun edraw-org-link-at-point ()
  "Return the link object adjacent to point.

If a link object exists immediately after point, return it.
If a non-link org-mode element or object exists immediately after
point, return nil.
If there is nothing immediately after point but a link object
exists immediately before it, return that link object.

Note that the returned link is not limited to the \"edraw:\" type
(`edraw-org-link-type'). edraw-org must also handle cases where an
\"edraw:\" link is written in the description section or where an svg
file is specified in the path of a \"file:\" link.
(e.g. [[file:example.org][edraw:...]], [[file:example.edraw.svg]],
[[file:example1.edraw.svg][file:example2.edraw.svg]])"
  (when-let* ((link-object (org-element-lineage
                            (save-match-data (org-element-context))
                            '(link) t))
              (end (save-excursion
                     (goto-char
                      (org-element-property :end link-object))
                     (skip-chars-backward " \t")
                     (point))))
    (org-element-put-property link-object :end end)))

(defconst edraw-org-link-path-terminator-pname "eop"
  "Property name to append to the end of path to match `org-link-plain-re'.

For example, in the path \"edraw:data=abcdef==\", the \"=\" part
at the end does not match `org-link-plain-re', but by adding a
dummy property to the end of the path,
\"edraw:data=abcdef==;eop=1\" matches `org-link-plain-re'.")

(defun edraw-org-link-path-terminator-pname ()
  edraw-org-link-path-terminator-pname)

(defun edraw-org-link-path-terminator-required-p (path)
  (memq (elt path (1- (length path))) '(?= ?+)))

(defun edraw-org-link-path-terminator-add (path)
  (concat path ";" edraw-org-link-path-terminator-pname "=1"))

(defun edraw-org-link-replace-object (link-object new-text part)
  "Replace a specific PART of LINK-OBJECT with NEW-TEXT.

After calling this function, the :end property of LINK-OBJECT
becomes invalid, as the end position of the link changes due to
the text replacement.

NEW-TEXT is the text to replace the given PART with.

PART determines which part of the link to replace and should be
one of the following symbols:

  - `path' : Replace the path part of the link.
  - `description' : Replace the description part of the link.
  - `link' : Replace the entire link.

If PART is nil, it defaults to `link'."
  (when link-object
    (save-excursion
      (goto-char (org-element-property :begin link-object))
      (edraw-org-link-replace-beginning-at-point new-text part))))

(defun edraw-org-link-replace-beginning-at-point (new-text part)
  "Replace a specific PART of an Org mode link beginning at point
with NEW-TEXT.

This function replaces a specific PART of the link starting at
the current point.

NEW-TEXT is the text to replace the given PART with.

PART determines which part of the link to replace and should be
one of the following symbols:

  - `path' : Replace the path part of the link.
  - `description' : Replace the description part of the link.
  - `link' : Replace the entire link.

If PART is nil, it defaults to `link'."
  (cond
   ;; bracket link
   ((looking-at org-link-bracket-re)
    (replace-match
     (cond
      ((string-empty-p new-text)
       new-text)
      ;; [[   ][<edraw:data=aaaaaaaaa==>]]
      ((and (eq part 'description)
            (eq (char-after (match-beginning 2)) ?<)
            (eq (char-before (match-end 2)) ?>))
       (concat "<" new-text ">"))
      ;; [[   ][edraw:data=aaaaaaaaa==;eop=1]]
      ((and (eq part 'description)
            (edraw-org-link-path-terminator-required-p new-text))
       (edraw-org-link-path-terminator-add new-text))
      ;; [[   ][edraw:data=aaaaaaaaaAA]] or [[   ][edraw:file=hoge.edraw.svg]]
      (t
       new-text))
     t t nil
     (pcase part
       ;;('link 0)
       ('path 1)
       ('description 2)))
    t)
   ;; angle link (Check before plain link)
   ((looking-at org-link-angle-re)
    (replace-match (concat "<" new-text ">") t t)
    t)
   ;; plain link
   ((looking-at org-link-plain-re)
    (replace-match
     (cond
      ((string-empty-p new-text)
       new-text)
      ;; edraw:data=aaaaaaaaa==;eop=1
      ((edraw-org-link-path-terminator-required-p new-text)
       (edraw-org-link-path-terminator-add new-text))
      ;; edraw:data=aaaaaaaaaAA or edraw:file=hoge.edraw.svg
      (t
       new-text))
     t t)
    t)))

(defun edraw-org-link-object-path-in-description (link-object
                                                  &optional link-type)
  "Return the string after \"<LINK-TYPE>:\" in the description of LINK-OBJECT.

When LINK-TYPE is nil, use `edraw-org-link-type'."
  (when-let* ((c-begin (org-element-property :contents-begin link-object))
              (c-end (org-element-property :contents-end link-object)))
    (let ((desc (buffer-substring-no-properties c-begin c-end)))
      (save-match-data
        (cond
         ;; angle link
         ((string-match (format "\\`<%s:\\(.*\\)>\\'"
                                (or link-type edraw-org-link-type))
                        desc)
          (match-string 1 desc))
         ;; plain link
         ((string-match (format "\\`%s:\\(.*\\)\\'"
                                (or link-type edraw-org-link-type))
                        desc)
          (match-string 1 desc)))))))

(defun edraw-org-link-object-link-properties (link-object noerror &optional use-normal-file-link-p)
  "Return the property alist of LINK-OBJECT.

Return a list (LINK-PROPS IN-DESCRIPTION-P LINK-TYPE).

If NOERROR is t, ignore invalid property components. For example,
when [[edraw: A=B;C;D=E]], return ((\"A\" . \"B\") (\"D\" . \"E\").
If NOERROR is nil, signals an error."
  ;; edraw: link type
  (cond
   ;; from description part
   ((when-let* ((desc (edraw-org-link-object-path-in-description link-object)))
      (list (edraw-org-link-props-parse desc t noerror)
            t
            edraw-org-link-type)))
   ;; from path part
   ((equal (org-element-property :type link-object) edraw-org-link-type)
    (let ((path (org-element-property :path link-object)))
      (list (edraw-org-link-props-parse path nil noerror)
            nil
            edraw-org-link-type)))
   ;; file: link type
   (use-normal-file-link-p
    (cond
     ;; from description part
     ((when-let* ((desc (edraw-org-link-object-path-in-description
                         link-object "file")))
        (list (list (cons "file" desc))
              t
              "file")))
     ;; from path part
     ((equal (org-element-property :type link-object) "file")
      (let ((path (org-element-property :path link-object)))
        (list (list (cons "file" path))
              nil
              edraw-org-link-type)))))))

;;;;; mouse-face Control

;; The following functions are for removing and restoring the
;; mouse-face property set on the link part as needed. If the
;; mouse-face property is present where the drawing editor is
;; displayed, the mouse cursor (pointer) will be hand-shaped. Even if
;; the pointer property of the overlay is set to arrow, the arrow will
;; be displayed after the hand is displayed for a moment each time the
;; screen is updated. I don't know the cause, but it may be an Emacs
;; bug. It's annoying because the mouse cursor looks so
;; flickering. This is a workaround to suppress it.
;; (Confirmed the phenomenon with Emacs 29.1 for MS-Windows)

;;@todo Support the case of org-fold-core-style is 'overlays ?

(defun edraw-org-link-remove-mouse-face (beg end)
  "Remove mouse-face property while editing.

This is to suppress the flickering of the mouse cursor (pointer).

If the mouse-face text property is set for the link, the hand
shape will be displayed for a moment even if the arrow is
specified for the overlay property pointer. This may be an Emacs
bug. I started to worry about flickering after Emacs 29 (on
Windows)"
  (when-let* ((mouse-face (get-text-property beg 'mouse-face)))
    (with-silent-modifications
      (remove-text-properties beg end '(mouse-face nil))
      (put-text-property beg end 'edraw-org-mouse-face-backup mouse-face))))

(defun edraw-org-link-recover-mouse-face (beg end)
  "Recover mouse-face property."
  (when-let* ((mouse-face (get-text-property beg 'edraw-org-mouse-face-backup)))
    (with-silent-modifications
      (put-text-property beg end 'mouse-face mouse-face)
      (remove-text-properties beg end '(edraw-org-mouse-face-backup nil)))))

(defun edraw-org-link-update-mouse-face (beg end)
  "Keeps a state where the mouse-face property is not present
between the time `edraw-org-link-remove-mouse-face' is called and
the time `edraw-org-link-recover-mouse-face' is called."
  (when-let* ((mouse-face (get-text-property beg 'edraw-org-mouse-face-backup)))
    (with-silent-modifications
      (remove-text-properties beg end '(mouse-face nil)))))

;;;; Inline Link Image

;; Key Map

(defconst edraw-org-link-image-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km org-mouse-map)
    (when (version<= "28" emacs-version)
      (define-key km [remap context-menu-open] 'edraw-org-link-image-menu-open))
    (define-key km (kbd "<apps>") 'edraw-org-link-image-menu-open)
    (define-key km (kbd "<menu>") 'edraw-org-link-image-menu-open)
    (define-key km (kbd "S-<f10>") 'edraw-org-link-image-menu-open)
    (define-key km [down-mouse-3] 'ignore) ;; Disable context-menu-mode
    (define-key km [mouse-3] 'edraw-org-link-image-menu-at-mouse)
    (define-key km (kbd "C-c C-o") 'edraw-org-link-image-open-at-point)
    km))

;; Menu

(defun edraw-org-link-image-menu-at-mouse (ev)
  (interactive "e")
  (mouse-set-point ev)
  (edraw-org-link-image-menu-open))

(defun edraw-org-link-image-menu-open ()
  (interactive)
  (edraw-popup-menu
   "Edraw Link Menu"
   `(((edraw-msg "Edit") edraw-org-edit-link)
     ((edraw-msg "Find File") edraw-org-link-image-find-file-at-mouse
      :visible ,(edraw-org-link-at-description-link-p))
     ((edraw-msg "Copy Contents") edraw-org-link-copy-contents-at-point)
     ((edraw-msg "Show SVG") edraw-org-link-show-svg-at-point)
     ((edraw-msg "Export SVG") edraw-org-link-export-svg-at-point)
     ((edraw-msg "Convert To [[edraw:file=]]") edraw-org-link-convert-to-edraw-file-link-at-point)
     ((edraw-msg "Convert To [[edraw:data=]]") edraw-org-link-convert-to-edraw-data-link-at-point)
     ((edraw-msg "Convert To [[file:]]") edraw-org-link-convert-to-normal-file-link-at-point)
     )))

(defun edraw-org-link-at-description-link-p ()
  (let ((link-object (edraw-org-link-at-point)))
    (and link-object
         (not (null (edraw-org-link-object-path-in-description link-object))))))

;; Tools

(defun edraw-org-link-image-open-at-point (&optional arg)
  (interactive)
  (if (and (edraw-org-link-at-description-link-p)
           (edraw-y-or-n-p "Edit edraw link? (y:edit link, n:open link)"))
      (edraw-org-edit-link)
    (org-open-at-point arg)))

(defun edraw-org-link-image-find-file-at-mouse ()
  (org-open-at-point 'in-emacs))

(defun edraw-org-link-load-svg-text-at-point (src-buffer dst-buffer)
  (with-current-buffer src-buffer
    (let* ((link-object (or (edraw-org-link-at-point)
                             (error (edraw-msg "No link at point"))))
           (props (car (edraw-org-link-object-link-properties link-object nil t)))
           (data (edraw-org-link-prop-data props))
           (file (edraw-org-link-prop-file props)))
      (unless (or data file)
        (error (edraw-msg "Link at point does not contain valid data")))
      (when (and file (not (file-exists-p file)))
        (error (edraw-msg "File does not exist")))

      (set-buffer dst-buffer)
      (set-buffer-file-coding-system 'utf-8)
      (erase-buffer)
      (cond
       (data
        (insert data)
        (edraw-decode-buffer t))
       (file
        (edraw-insert-xml-file-contents file))))))

(defun edraw-org-link-show-svg-at-point ()
  "Display the SVG data of the link at point."
  (interactive)
  (let ((src-buffer (current-buffer)))
    (with-temp-buffer
      (edraw-org-link-load-svg-text-at-point src-buffer (current-buffer))
      (let ((buffer (current-buffer)))
        (pop-to-buffer "*Easy Draw SVG*")
        (set-buffer-file-coding-system 'utf-8)
        (erase-buffer)
        (insert-buffer-substring buffer)
        (xml-mode)))))

(defun edraw-org-link-export-svg-at-point ()
  "Writes the SVG data of the link at point to a file."
  (interactive)
  (let ((src-buffer (current-buffer)))
    (with-temp-buffer
      (edraw-org-link-load-svg-text-at-point src-buffer (current-buffer))
      (let ((buffer (current-buffer))
            (output-file (read-file-name "Export File(.edraw.svg): ")))
        (when (and (file-exists-p output-file)
                   (not (edraw-y-or-n-p (edraw-msg "Overwrite?"))))
          (signal 'quit nil))

        (with-temp-file output-file
          (set-buffer-file-coding-system 'utf-8)
          (insert-buffer-substring buffer))))))

(defun edraw-org-link-convert-to-edraw-data-link-at-point ()
  "Converts the link at point to an edraw:data= format link."
  (interactive)
  (edraw-org-link-convert-at-point 'edraw-data))

(defun edraw-org-link-convert-to-edraw-file-link-at-point ()
  "Converts the link at point to an edraw:file= format link."
  (interactive)
  (edraw-org-link-convert-at-point 'edraw-file))

(defun edraw-org-link-convert-to-normal-file-link-at-point ()
  "Converts the link at point to an file: format link."
  (interactive)
  (edraw-org-link-convert-at-point 'file))

(defun edraw-org-link-convert-at-point (target-type)
  "Converts the link at point to TARGET-TYPE.

Allowed values for TARGET-TYPE are:

  edraw-data: [[edraw:data=]]
  edraw-file: [[edraw:file=]]
        file: [[file:]]"
  (let* ((link-object (or (edraw-org-link-at-point)
                           (error (edraw-msg "No link at point"))))
         (props-place-type
          (or (edraw-org-link-object-link-properties link-object t t)
              (error "Invalid link type")))
         (props (nth 0 props-place-type))
         (in-description-p (nth 1 props-place-type))
         (link-type (nth 2 props-place-type))
         (data (edraw-org-link-prop-data props))
         (file (edraw-org-link-prop-file props)))
    (unless (or data file)
      (error (edraw-msg "Link at point does not contain valid data")))
    (when (and file (not (file-exists-p file)))
      (error (edraw-msg "File does not exist")))

    ;; Prepare data, file, properties
    (pcase target-type
      ;; To Data
      ('edraw-data
       (when data
         (error (edraw-msg "No need to convert")))
       (let ((new-data
              (with-temp-buffer
                (edraw-insert-xml-file-contents file)
                (edraw-encode-buffer t edraw-org-link-compress-data-p)
                (buffer-string))))
         (setf (alist-get "file" props nil t #'string=) nil)
         (setf (alist-get "data" props nil nil #'string=) new-data)))
      ;; To file
      ((or 'edraw-file 'file)
       (when (or (and (eq target-type 'edraw-file)
                      (equal link-type edraw-org-link-type)
                      file)
                 (and (eq target-type 'file)
                      (equal link-type "file")))
         (error (edraw-msg "No need to convert")))
       (when data
         (let ((output-file (read-file-name "Export File(.edraw.svg): ")))
           (when (and (file-exists-p output-file)
                      (not (edraw-y-or-n-p (edraw-msg "Overwrite?"))))
             (signal 'quit nil))
           (with-temp-file output-file
             (set-buffer-file-coding-system 'utf-8)
             (insert data)
             (edraw-decode-buffer t)
             (when edraw-org-link-compress-file-p
               (edraw-encode-buffer nil t)))
           (setq file (file-relative-name output-file))
           (setf (alist-get "data" props nil t #'string=) nil)
           (setf (alist-get "file" props nil nil #'string=) file))))
      (_
       (error "Unknown target type %s" target-type)))

    ;; Replace Link
    (unless (edraw-org-link-replace-object
             link-object ;; LINK-OBJECT is invalid after the call
             (if (eq target-type 'file)
                 (concat "file:" file)
               (concat edraw-org-link-type ":"
                       (edraw-org-link-props-to-string props)))
             (if in-description-p 'description 'path))
      (error "Failed to replace edraw link"))))



;;
;; Display edraw link image inline
;;

(define-minor-mode edraw-org-link-image-mode
  "Show edraw link images inline."
  :init-value nil
  (progn
    (if edraw-org-link-image-mode
        (progn
          (edraw-org-link-image-activate)
          (font-lock-flush))
      (edraw-org-link-image-remove-all))))

(defun edraw-org-link-image-activate ()
  (advice-add 'org-activate-links :around 'edraw-org-advice-activate-links))

(defun edraw-org-link-image-deactivate ()
  (advice-remove 'org-activate-links 'edraw-org-advice-activate-links))

(defun edraw-org-advice-activate-links (old-func limit)
  ":around advice for `org-activate-links'"
  (let* ((beg (point))
         (result (funcall old-func limit))
         (end (if result (point) limit)))

    (when edraw-org-link-image-mode
      (edraw-org-link-image-update-region beg end))

    result))

(defun edraw-org-link-image-make-search-regexp ()
  (mapconcat
   #'identity
   (delq nil
         (list
          ;; Note: The first parentheses must refer to the `edraw' part.
          ;; (bracket link) [[edraw:? or ][edraw:? or ][<edraw:?
          (when (memq 'bracket edraw-org-link-image-link-formats)
            (format "\\(?:[][]\\[\\|]\\[<\\)\\(?1:%s\\):[^]\n]"
                    edraw-org-link-type))
          ;; (angle link) <edraw:?
          (when (memq 'angle edraw-org-link-image-link-formats)
            (format "<\\(?1:%s\\):[^>]" edraw-org-link-type))
          ;; (plain link) edraw:
          (when (memq 'plain edraw-org-link-image-link-formats)
            ;; NG:
            ;;   [edraw:?
            ;;   <edraw:?
            ;; OK:
            ;;   \`edraw:?
            ;;   \=edraw:?  <= There may be [ or < before it, but it is allowed.
            (format "\\(?:\\(?:[^[<]\\|\\`\\|\\=\\)\\<\\(?1:%s\\):[^ \t\n]\\)"
                    edraw-org-link-type))))
   "\\|"))

(defun edraw-org-link-image-update-region (beg end)
  (save-match-data
    (save-excursion
      (goto-char beg)
      (let ((last-end beg)
            (regexp (edraw-org-link-image-make-search-regexp)))

        ;;(message "Fontify beg:%s end:%s" beg end)
        ;; When start from the middle of a link
        (when-let* ((link-object (edraw-org-link-at-point)))
          (let ((link-begin (org-element-property :begin link-object))
                (link-end (org-element-property :end link-object)))
            ;;(message "Link at start %s-%s format:%s" link-begin link-end (org-element-property :format link-object))
            (when (< link-begin beg link-end)
              ;;(message "Fontify from the middle of a link!")
              (when (and (memq (org-element-property :format link-object)
                               edraw-org-link-image-link-formats)
                         (edraw-org-link-image-update link-begin link-end
                                                      link-object))
                (setq last-end link-end))
              (goto-char link-end))))

        ;; All links up to the END.
        ;;(message "Start search from %s" (point))
        (while (re-search-forward regexp end t)
          (let ((match-beg0 (match-beginning 0)))
            ;; Move to beginning of edraw: part
            (goto-char (match-beginning 1))
            ;;(message "Found candidate at %s" (point))
            (if-let* ((link-object (edraw-org-link-at-point)))
                (let ((link-begin (org-element-property :begin link-object))
                      (link-end (org-element-property :end link-object)))
                  (when (and (memq (org-element-property :format link-object)
                                   edraw-org-link-image-link-formats)
                             (edraw-org-link-image-update link-begin link-end
                                                          link-object))
                    ;; Remove overlays before image
                    (edraw-org-link-image-remove-region last-end link-begin)
                    (setq last-end link-end))
                  (goto-char link-end))
              (goto-char (match-end 0)))
            ;; Prevent infinite loops.
            (when (<= (point) match-beg0)
              (forward-char))))
        ;; Remove overlays after last image
        (when (< last-end end)
          (edraw-org-link-image-remove-region last-end end))))))

(defun edraw-org-link-image-remove-region (beg end)
  ;;(message "Remove %s-%s" beg end)
  ;;(remove-overlays beg end 'edraw-org-link-image-p t) ;;move the endpoints? split?
  (mapc #'delete-overlay (edraw-org-link-image-overlays-in beg end)))

(defun edraw-org-link-image-update (link-begin link-end link-object)
  (when-let* ((link-props
               (car (edraw-org-link-object-link-properties link-object t))))
    (let* ((ovs (edraw-org-link-image-overlays-in link-begin link-end))
           (ov (progn
                 ;; Remove redundant overlays
                 (mapc #'delete-overlay (cdr ovs))
                 (car ovs)))
           (visible (if ov
                        (overlay-get ov 'edraw-org-link-image-visible)
                      (null (and (fboundp 'edraw-editor-overlays-in)
                                 (edraw-editor-overlays-in
                                  link-begin link-end)))))
           (image (edraw-org-link-image-create link-props)))

      ;; Add or Remove a link image
      (if image
          (progn
            (if ov
                (progn
                  (move-overlay ov link-begin link-end)
                  (overlay-put ov 'display (if visible image))
                  (overlay-put ov 'edraw-org-link-image image))
              (setq ov (make-overlay link-begin link-end nil t nil))
              (overlay-put ov 'display (if visible image))
              (overlay-put ov 'keymap (if visible edraw-org-link-image-map))
              (overlay-put ov 'face 'default)
              (overlay-put ov 'edraw-org-link-image-p t)
              (overlay-put ov 'edraw-org-link-image image)
              (overlay-put ov 'edraw-org-link-image-visible t)
              (overlay-put ov 'evaporate t))
            ;; Remove `invisible' text property of the imaged link to
            ;; be able to place point between adjacent images.
            (edraw-org-link-image-remove-invisible-text-property
             link-begin link-end))
        (if ov
            (delete-overlay ov))))
    ;; Processed
    t))

(defun edraw-org-link-image-overlays-in (beg end)
  (seq-filter
   (lambda (ov) (overlay-get ov 'edraw-org-link-image-p))
   (overlays-in beg end)))

(defun edraw-org-link-image-overlay-at (&optional pos)
  (if (null pos) (setq pos (point)))
  (car (edraw-org-link-image-overlays-in pos (1+ pos))))

(defun edraw-org-link-image-create (link-props)
  (let ((file-or-data (edraw-org-link-props-image-data-or-file link-props)))
    (when file-or-data
      (let (image-props)
        (when-let* ((max-width (edraw-org-link-image-max-size t)))
          (setq image-props `(:max-width ,max-width ,@image-props)))
        (when-let* ((max-height (edraw-org-link-image-max-size nil)))
          (setq image-props `(:max-height ,max-height ,@image-props)))
        (apply #'create-image
               (car file-or-data) ;; FILE-OR-DATA
               'svg ;;TYPE
               (cdr file-or-data) ;;DATA-P
               image-props))))) ;;PROPS

(defun edraw-org-link-image-max-size (width-p)
  "Return the maximum size of the image in pixels.
If WIDTH-P is non-nil, return width, otherwise return height."
  (let ((max-size
         (if (consp edraw-org-link-image-max-size)
             (if width-p
                 (car edraw-org-link-image-max-size)
               (cdr edraw-org-link-image-max-size))
           edraw-org-link-image-max-size)))
    (if (floatp max-size)
        (ceiling (* max-size
                    (if width-p (frame-text-width) (frame-text-height))))
      max-size)))

(defun edraw-org-link-image-remove-all ()
  ;;(edraw-org-link-image-remove-region (point-min) (point-max))
  (remove-overlays nil nil 'edraw-org-link-image-p t))

(defun edraw-org-link-image-set-visible (ov visible-p)
  (when ov
    (overlay-put ov 'edraw-org-link-image-visible visible-p)

    (overlay-put ov 'display
                 (if visible-p (overlay-get ov 'edraw-org-link-image)))
    (overlay-put ov 'keymap
                 (if visible-p edraw-org-link-image-map))
    (overlay-put ov 'face
                 (if visible-p 'default))))

(defun edraw-org-link-image-remove-invisible-text-property (link-begin link-end)
  "Remove the `invisible' text property between LINK-BEGIN and LINK-END.

By removing this property, the cursor can be placed between two adjacent
links.  For example, in the text [[edraw:...]][[edraw:...]], the region
]][[ normally has the `invisible' property set to `org-link', making it
invisible. As a result, the cursor cannot be placed between the two
links (between ]] and [[). This is an issue with Org mode's link
display, but for inline image links, it can be avoided by setting the
`invisible' property to nil.
This adjustment allows precise keyboard navigation of individual images."
  (remove-text-properties link-begin link-end '(invisible nil)))


;;;; Text Conversion Control

(defvar-local edraw-org-text-conversion-style-on-editor nil)
(defvar-local edraw-org-text-conversion-style-original nil)

(defvar text-conversion-style) ;; Emacs 30

(define-minor-mode edraw-org-text-conversion-control-mode
  "A minor mode to work around the `text-conversion-style' issue.

When `text-conversion-style' is non-nil, the IME inputs alphabets
directly into the buffer. As a result, the keymap property of the
`edraw-editor' overlay is ignored, and convenient shortcut keys for
single alphabetic characters cannot be used. A similar problem occurs
with `org-speed-command' and `edebug-mode'.

To avoid this problem, use a `post-command-hook' to track the movement
of point and switch the `text-conversion-style' depending on the
location."
  :init-value nil
  (cond
   (edraw-org-text-conversion-control-mode
    (if (not (boundp 'text-conversion-style))
        ;; Emacs 29 or earlier
        ;; @todo Should signal error?
        (setq edraw-org-text-conversion-control-mode nil)
      (setq edraw-org-text-conversion-style-on-editor nil)
      (add-hook 'post-command-hook
                #'edraw-org-text-conversion-style-update nil t)))
   (t
    (edraw-org-text-conversion-style--leave)
    (remove-hook 'post-command-hook
                 #'edraw-org-text-conversion-style-update t))))

(defun edraw-org-text-conversion-style-update ()
  (when (fboundp 'edraw-editor-at)
    (if edraw-org-text-conversion-style-on-editor
        (unless (edraw-editor-at nil t)
          (edraw-org-text-conversion-style--leave))
      (if text-conversion-style
          (when (edraw-editor-at nil t)
            (edraw-org-text-conversion-style--enter))
        ;; No needs to control `text-conversion-style'.
        ;; `text-conversion-style' is originally nil.
        nil))))

(defun edraw-org-text-conversion-style--enter ()
  (when (not edraw-org-text-conversion-style-on-editor)
    (when (fboundp 'set-text-conversion-style)
      (set-text-conversion-style nil))
    (setq edraw-org-text-conversion-style-original text-conversion-style
          edraw-org-text-conversion-style-on-editor t)))

(defun edraw-org-text-conversion-style--leave ()
  (when edraw-org-text-conversion-style-on-editor
    (when (fboundp 'set-text-conversion-style)
      (set-text-conversion-style edraw-org-text-conversion-style-on-editor))
    (setq edraw-org-text-conversion-style-on-editor nil)))

(defun edraw-org-setup-text-conversion ()
  "Work around the issue where shortcut keys for edraw-editor don't work
because of `text-conversion-style'.

For now, this workaround only applies to the Android version of Emacs."
  (when (eq system-type 'android) ;; @todo Is it the same for other platforms?
    (add-hook 'org-mode-hook 'edraw-org-text-conversion-control-mode)))

;;;; Export

(defconst edraw-org-export-backends
  ;; backend ox-library export-fun inline-image-rules-var
  '((html
     ox-html edraw-org-export-html-link org-html-inline-image-rules)
    (latex
     ox-latex edraw-org-export-latex-link org-latex-inline-image-rules)
    (odt
     ox-odt edraw-org-export-odt-link org-odt-inline-image-rules)))
(defsubst edraw-org-export-backend-name (info) (nth 0 info))
(defsubst edraw-org-export-backend-library (info) (nth 1 info))
(defsubst edraw-org-export-backend-export-fun (info) (nth 2 info))
(defsubst edraw-org-export-backend-image-rules-var (info) (nth 3 info))

(defun edraw-org-link-setup-exporter ()
  ;; A hack for referencing link objects from export functions.
  (with-eval-after-load 'ox
    (advice-add 'org-export-custom-protocol-maybe :around
                'edraw-org-export-ad-export-custom-protocol-maybe))
  ;; Register the edraw link type in org-<backend>-inline-image-rules
  ;; variables to ensure inline images.
  (dolist (backend edraw-org-export-backends)
    ;; (with-eval-after-load <library>
    ;;   (setf (alist-get edraw-org-link-type
    ;;                    org-<backend>-inline-image-rules nil nil #'equal)
    ;;         ".*"))
    (eval-after-load (edraw-org-export-backend-library backend)
      (when-let* ((rules-var
                   (edraw-org-export-backend-image-rules-var backend)))
        (lambda ()
          (edraw-org-export-add-link-type-to-inline-image-rules rules-var))))))

(defun edraw-org-export-add-link-type-to-inline-image-rules (rules-var)
  "Register edraw link type in the inline image rules defined by the backend.

RULES-VAR is the symbol of the variable to register.

This process is necessary to correctly export the edraw link written in
the description part of the links.
(e.g.[[file:test.html][edraw:data=...]])"
  (setf (alist-get edraw-org-link-type
                   (symbol-value rules-var) nil nil #'equal)
        ".*"))

(defvar edraw-org-export-current-link nil)

(defun edraw-org-export-ad-export-custom-protocol-maybe
    (old-func link &rest args)
  "A hack to reference the link object being exported from a function set
in the :export property of `org-link-parameters'."
  (let ((edraw-org-export-current-link link))
    (apply old-func link args)))

(autoload 'org-export-derived-backend-p "ox")

(defun edraw-org-link-export (path description back-end info)
  "Export an edraw link.
Use this by setting the :export property of `org-link-parameters'."
  (when-let* ((fun
               ;; Find the export function for BACK-END
               (cl-loop for backend in edraw-org-export-backends
                        when (org-export-derived-backend-p
                              back-end
                              (edraw-org-export-backend-name backend))
                        return (edraw-org-export-backend-export-fun backend))))
    (funcall fun path description back-end info edraw-org-export-current-link)))

;;;;; Utilities for Implementing Backends

(defun edraw-org-export-get-file-from-edraw-path (edraw-path)
  "Get the file name to export from the edraw link path (EDRAW-PATH).

If EDRAW-PATH is of data format, create a temporary file and return its
file name."
  (when-let* ((link-props (edraw-org-link-props-parse edraw-path nil t)))
    (or
     ;; Export data=base64.
     ;; Create temporary file.
     (when-let* ((data (edraw-org-link-prop-data link-props)))
       (edraw-org-export-create-temp-data-file data))
     ;; Export file=.
     (edraw-org-link-prop-file link-props)
     ;; Others?
     )))

(defun edraw-org-export-create-temp-data-file (data)
  "Create a temporary file for exporting SVG DATA."
  (let* ((svg-str (edraw-decode-string data t))
         (hash (sha1 svg-str))
         (file (format "link-data-%s.edraw.svg" hash))) ;;@todo customize
    (with-temp-file file
      (insert svg-str)
      (set-buffer-file-coding-system 'utf-8))
    file))

(defun edraw-org-export-link-as-file (link info file export-fun)
  "Temporarily rewrite LINK object to a file type link and export it.

FILE is the path to the file.

EXPORT-FUN is a function for exporting LINK. It takes LINK and INFO as
arguments. Such functions include `org-latex--inline-image' and
`org-odt-link--inline-image'."
  (let ((old-type (org-element-property :type link))
        (old-path (org-element-property :path link))
        (old-raw-link (org-element-property :raw-link link)))
    (unwind-protect
        (progn
          (org-element-put-property link :type "file")
          (org-element-put-property link :path file)
          (org-element-put-property link :raw-link (concat "file:" file))
          ;; export-fun is one of the following:
          ;; - `org-latex--inline-image'
          ;; - `org-odt-link--inline-image'
          (funcall export-fun link info))
      (org-element-put-property link :type old-type)
      (org-element-put-property link :path old-path)
      (org-element-put-property link :raw-link old-raw-link))))


;;;; Search

(defun edraw-org-link-re-search (regexp &optional bound noerror count backward)
  "Search for a data link whose content matches REGEXP.

Return a list containing information about the found link or nil.

Link information list format:
  ( LINK-OBJECT DECODED-TEXT LINK-PROPS IN-DESCRIPTION-P LINK-TYPE)

Point is moved to the end of the found link.

Note: The resulting match data is set for the entire link. SUBEXP 0
represents the range of the link, and SUBEXP 1 and beyond are not
guaranteed.

Currently this function doesn't search inside edraw:file= or
file:???.svg. Use grep to search for those."
  (unless count (setq count 1))
  (unless (fixnump count)
    (signal 'wrong-type-argument (list #'fixnump count)))
  (when (< count 0)
    (setq backward (not backward)
          count (- count)))

  (let ((search-fun (if backward #'re-search-backward #'re-search-forward))
        last-found-link-data)
    (while (and
            (> count 0)
            (funcall search-fun org-link-any-re bound t))
      (when-let* ((link-object (edraw-org-link-at (match-beginning 0)))
                  (link-props-place-type
                   (edraw-org-link-object-link-properties
                    link-object
                    t
                    ;; Exclude file: type link
                    nil))
                  (link-props (car link-props-place-type))
                  (data (edraw-org-link-prop-data link-props))
                  (decoded-text (ignore-errors (edraw-decode-string data t))))
        (when (string-match-p regexp decoded-text)
          (setq last-found-link-data (nconc
                                      (list link-object decoded-text)
                                      link-props-place-type)
                count (1- count)))))
    (if (> count 0)
        (if noerror nil (signal 'search-failed (list regexp)))
      last-found-link-data)))

(defun edraw-org-link-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for a data link whose content matches REGEXP."
  (interactive "sRE search in edraw link: ")
  (when (edraw-org-link-re-search regexp bound noerror count nil)
    (point)))

(defun edraw-org-link-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for a data link whose content matches REGEXP."
  (interactive "sRE search in edraw link: ")
  (when (edraw-org-link-re-search regexp bound noerror count t)
    (point)))

(defun edraw-org-link-isearch-fun ()
  ;; See: `isearch-search-fun-default'
  (lambda (string &optional bound noerror count)
    (let (;; Beginning of copy from isearch.el
          ;; ---------------------------------------------------------
          ;; Evaluate this before binding `search-spaces-regexp' which
          ;; can break all sorts of regexp searches.  In particular,
          ;; calling `isearch-regexp-function' can trigger autoloading
          ;; (Bug#35802).
          (regexp
           (cond (isearch-regexp-function
                  (let ((lax (and (not bound) ; not lazy-highlight
                                  (isearch--lax-regexp-function-p))))
                    (when lax
                      (setq isearch-adjusted 'lax))
                    (if (functionp isearch-regexp-function)
                        (funcall isearch-regexp-function string lax)
                      (word-search-regexp string lax))))
                 (isearch-regexp string)
                 (t (regexp-quote string))))
          ;; Use lax versions to not fail at the end of the word while
          ;; the user adds and removes characters in the search string
          ;; (or when using nonincremental word isearch)
          (search-spaces-regexp (when (if isearch-regexp
                                          isearch-regexp-lax-whitespace
                                        isearch-lax-whitespace)
                                  search-whitespace-regexp))
          ;; ---------------------------------------------------------
          ;; End of copy from isearch.el
          )
      (when (edraw-org-link-re-search regexp bound noerror count
                                      (not isearch-forward))
        (point)))))

(defun edraw-org-link-isearch-forward (&optional regexp-p)
  "Incrementally search forward for data links."
  (interactive "P")
  (let ((isearch-search-fun-function #'edraw-org-link-isearch-fun))
    (isearch-mode t (not (null regexp-p)) nil t)))

(defun edraw-org-link-isearch-backward (&optional regexp-p)
  "Incrementally search backward for data links."
  (interactive "P")
  (let ((isearch-search-fun-function #'edraw-org-link-isearch-fun))
    (isearch-mode nil (not (null regexp-p)) nil t)))

;;;; Replace

;; @todo Use replace.el?
;; @todo Add query-replace

(defun edraw-org-link-replace-regexp (regexp to-string)
  (interactive "sReplace regexp: \nsReplace to: ")

  (let ((link-count 0) (text-count 0))
    (while
        (when-let* ((link-info (edraw-org-link-re-search regexp nil t)))
          (let* ((link-object (nth 0 link-info))
                 (decoded-text (nth 1 link-info))
                 (props (nth 2 link-info))
                 (in-description-p (nth 3 link-info))
                 (new-data
                  (with-temp-buffer
                    (insert decoded-text)
                    (goto-char (point-min))
                    (while (re-search-forward regexp nil t)
                      (replace-match to-string)
                      (cl-incf text-count))
                    (edraw-encode-buffer t edraw-org-link-compress-data-p)
                    (buffer-string))))
            (setf (alist-get "data" props nil nil #'string=) new-data)

            ;; Replace Link
            (unless (edraw-org-link-replace-object
                     link-object ;; LINK-OBJECT is invalid after the call
                     (concat edraw-org-link-type ":"
                             (edraw-org-link-props-to-string props))
                     (if in-description-p 'description 'path))
              (error "Failed to replace edraw link"))
            (cl-incf link-count))
          t))

    (message "Replaced %d text in %s links" text-count link-count)))

(defun edraw-org-link-replace-string (from-string to-string)
  (interactive "sReplace string: \nsReplace to: ")
  (edraw-org-link-replace-regexp (regexp-quote from-string) to-string))

(provide 'edraw-org)
;;; edraw-org.el ends here
