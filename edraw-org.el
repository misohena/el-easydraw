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

;;;; Setup

(defun edraw-org-setup-default ()
  (edraw-org-setup-exporter)
  (edraw-org-setup-inline-images))

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
        (when-let ((link-element (edraw-org-link-at-point))
                   (link-props (car (edraw-org-link-element-link-properties
                                     link-element t))))
          (if-let ((file (edraw-org-link-prop-file link-props)))
              (format "edraw:file=%s" file)
            (if-let ((data (edraw-org-link-prop-data link-props)))
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
    ;; Remove "edraw:" at the beginning of PATH just in case.
    ;; If the link type is not registered, edraw: may remain at the beginning.
    (setq path (string-remove-prefix (format "%s:" edraw-org-link-type) path))

    ;; Convert "A=B;C=D;..." to ((A . B) (C . D) ...)
    (delq
     nil
     (mapcar
      (lambda (prop)
        (if (string-match "\\`\\([^=]*\\)=\\(.*\\)\\'" prop)
            (cons
             (string-trim (match-string 1 prop))
             (edraw-org-link-unescape
              (string-trim (match-string 2 prop))
              ;; If PATH is in the description part, brackets have not
              ;; yet been unescaped
              in-description-p))
          (if noerror
              nil
            (error "Invalid link format: %s" path))))
      (split-string path ";" t "[ \t\n\r]+")))))

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
  (when-let ((str (edraw-org-link-prop-get "html-tag" link-props)))
    (intern str)))

(defun edraw-org-link-prop-data (link-props)
  (edraw-org-link-prop-get "data" link-props))

(defun edraw-org-link-prop-file (link-props)
  (when-let ((file-spec (edraw-org-link-prop-get "file" link-props)))
    (edraw-org-link-filter-file-path file-spec)))

(defun edraw-org-link-filter-file-path (path)
  path)

(defun edraw-org-link-props-image-data-or-file (link-props)
  "Create arguments to pass to `create-image' from LINK-PROPS.

Return a cons cell of the form (FILE-OR-DATA . DATA-P)."
  (if-let ((data (edraw-org-link-prop-data link-props)))
      (ignore-errors (cons
                      ;; see: https://github.com/misohena/el-easydraw/issues/5
                      (if edraw-org-link-image-gunzip-p
                          (edraw-decode-string data t)
                        (base64-decode-string data))
                      t))
    (if-let ((file (edraw-org-link-prop-file link-props)))
        (if (file-exists-p file) (cons (expand-file-name file) nil)))))

;;;;; Link Element

(defun edraw-org-link-at-point ()
  (when-let ((element (org-element-lineage
                       (save-match-data (org-element-context))
                       '(link) t))
             (end (save-excursion
                    (goto-char
                     (org-element-property :end element))
                    (skip-chars-backward " \t")
                    (point))))
    (org-element-put-property element :end end)))

(defun edraw-org-link-replace-at-point (text part)
  "Replace a PART of the link at the current point with TEXT.

This function will replace the link even if it is in the comment,
so check the org element before using this function."
  (when (org-in-regexp org-link-bracket-re 1)
    (replace-match text t t nil
                   (pcase part
                     ('link 0)
                     ('path 1)
                     ('description 2)))
    t))

(defun edraw-org-link-element-path-in-description (link-element
                                                   &optional link-type)
  "Return the string after \"<LINK-TYPE>:\" in the description of LINK-ELEMENT.

When LINK-TYPE is nil, use `edraw-org-link-type'."
  (when-let ((c-begin (org-element-property :contents-begin link-element))
             (c-end (org-element-property :contents-end link-element)))
    (let ((desc (buffer-substring-no-properties c-begin c-end)))
      (when (string-match (format "\\`%s:\\(.*\\)\\'"
                                  (or link-type edraw-org-link-type))
                          desc)
        (match-string 1 desc)))))

(defun edraw-org-link-element-link-properties (link-element noerror &optional use-normal-file-link-p)
  "Return the property alist of LINK-ELEMENT.

Return a list (LINK-PROPS IN-DESCRIPTION-P LINK-TYPE).

If NOERROR is t, ignore invalid property components. For example,
when [[edraw: A=B;C;D=E]], return ((\"A\" . \"B\") (\"D\" . \"E\").
If NOERROR is nil, signals an error."
  ;; edraw: link type
  (cond
   ;; from description part
   ((when-let ((desc (edraw-org-link-element-path-in-description link-element)))
      (list (edraw-org-link-props-parse desc t noerror)
            t
            edraw-org-link-type)))
   ;; from path part
   ((equal (org-element-property :type link-element) edraw-org-link-type)
    (let ((path (org-element-property :path link-element)))
      (list (edraw-org-link-props-parse path nil noerror)
            nil
            edraw-org-link-type)))
   ;; file: link type
   (use-normal-file-link-p
    (cond
     ;; from description part
     ((when-let ((desc (edraw-org-link-element-path-in-description
                        link-element "file")))
        (list (list (cons "file" desc))
              t
              "file")))
     ;; from path part
     ((equal (org-element-property :type link-element) "file")
      (let ((path (org-element-property :path link-element)))
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
  (when-let ((mouse-face (get-text-property beg 'mouse-face)))
    (with-silent-modifications
      (remove-text-properties beg end '(mouse-face nil))
      (put-text-property beg end 'edraw-org-mouse-face-backup mouse-face))))

(defun edraw-org-link-recover-mouse-face (beg end)
  "Recover mouse-face property."
  (when-let ((mouse-face (get-text-property beg 'edraw-org-mouse-face-backup)))
    (with-silent-modifications
      (put-text-property beg end 'mouse-face mouse-face)
      (remove-text-properties beg end '(edraw-org-mouse-face-backup nil)))))

(defun edraw-org-link-update-mouse-face (beg end)
  "Keeps a state where the mouse-face property is not present
between the time `edraw-org-link-remove-mouse-face' is called and
the time `edraw-org-link-recover-mouse-face' is called."
  (when-let ((mouse-face (get-text-property beg 'edraw-org-mouse-face-backup)))
    (with-silent-modifications
      (remove-text-properties beg end '(mouse-face nil)))))

;;;; Inline Link Image

;; Key Map

(defconst edraw-org-link-image-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km org-mouse-map)
    (define-key km [mouse-3] 'edraw-org-link-image-menu-at-mouse)
    (define-key km (kbd "C-c C-o") 'edraw-org-link-image-open-at-point)
    km))

;; Menu

(defun edraw-org-link-image-menu-at-mouse (ev)
  (interactive "e")

  (mouse-set-point ev)
  (edraw-popup-menu
   "Edraw Link Menu"
   `(((edraw-msg "Edit") edraw-org-edit-link)
     ((edraw-msg "Find File") edraw-org-link-image-find-file-at-mouse
      :visible (edraw-org-link-at-description-link-p))
     ((edraw-msg "Copy Contents") edraw-org-link-copy-contents-at-point)
     ((edraw-msg "Show SVG") edraw-org-link-show-svg-at-point)
     ((edraw-msg "Export SVG") edraw-org-link-export-svg-at-point)
     ((edraw-msg "Convert To [[edraw:file=]]") edraw-org-link-convert-to-edraw-file-link-at-point)
     ((edraw-msg "Convert To [[edraw:data=]]") edraw-org-link-convert-to-edraw-data-link-at-point)
     ((edraw-msg "Convert To [[file:]]") edraw-org-link-convert-to-normal-file-link-at-point)
     )))

(defun edraw-org-link-at-description-link-p ()
  (let ((link-element (edraw-org-link-at-point)))
    (and link-element
         (not (null (edraw-org-link-element-path-in-description link-element))))))

;; Tools

(defun edraw-org-link-image-open-at-point (&optional arg)
  (interactive)
  (if (and (edraw-org-link-at-description-link-p)
           (y-or-n-p "Edit edraw link? (y:edit link, n:open link)"))
      (edraw-org-edit-link)
    (org-open-at-point arg)))

(defun edraw-org-link-image-find-file-at-mouse ()
  (org-open-at-point 'in-emacs))

(defun edraw-org-link-load-svg-text-at-point (src-buffer dst-buffer)
  (with-current-buffer src-buffer
    (let* ((link-element (or (edraw-org-link-at-point)
                             (error (edraw-msg "No link at point"))))
           (props (car (edraw-org-link-element-link-properties link-element nil t)))
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
        (insert-file-contents file))))))

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
                   (not (y-or-n-p (edraw-msg "Overwrite?"))))
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
  (let* ((link-element (or (edraw-org-link-at-point)
                           (error (edraw-msg "No link at point"))))
         (props-place-type
          (or (edraw-org-link-element-link-properties link-element t t)
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
                (insert-file-contents file)
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
                      (not (y-or-n-p (edraw-msg "Overwrite?"))))
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
    (unless (edraw-org-link-replace-at-point
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

(defun edraw-org-link-image-update-region (beg end)
  (save-match-data
    (save-excursion
      (goto-char beg)
      (let ((last-end beg))
	(while (re-search-forward
		;; [[edraw:...]] or ][edraw:...]]
		(format "[][]\\[%s:.*?\\]\\]" edraw-org-link-type) end t)
          (goto-char (match-beginning 0))
          (if-let ((link-element (edraw-org-link-at-point)))
              (let ((link-begin (org-element-property :begin link-element))
                    (link-end (org-element-property :end link-element)))
		(when (edraw-org-link-image-update link-begin link-end link-element)
                  ;; Remove overlays before image
                  (edraw-org-link-image-remove-region last-end link-begin)
                  (setq last-end link-end))
		(goto-char link-end))
            (goto-char (match-end 0))))
	;; Remove overlays after last image
	(when (< last-end end)
          (edraw-org-link-image-remove-region last-end end))))))

(defun edraw-org-link-image-remove-region (beg end)
  ;;(remove-overlays beg end 'edraw-org-link-image-p t) ;;move the endpoints? split?
  (mapc #'delete-overlay (edraw-org-link-image-overlays-in beg end)))

(defun edraw-org-link-image-update (link-begin link-end link-element)
  (when-let (link-props (car
                         (edraw-org-link-element-link-properties link-element t)))
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
        (when-let ((max-width (edraw-org-link-image-max-size t)))
          (setq image-props `(:max-width ,max-width ,@image-props)))
        (when-let ((max-height (edraw-org-link-image-max-size nil)))
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


;;;; Export

(defun edraw-org-link-setup-exporter ()
  (with-eval-after-load 'ox
    (advice-add 'org-export-custom-protocol-maybe :around
                'edraw-org-export-ad-export-custom-protocol-maybe))
  (with-eval-after-load 'ox-html
    (edraw-org-export-html-setup))
  (with-eval-after-load 'ox-latex
    (edraw-org-export-latex-setup)))

(defun edraw-org-export-html-setup ()
  (defvar org-html-inline-image-rules) ;;ox-html.el
  (with-eval-after-load 'ox-html
    (setf (alist-get edraw-org-link-type
                     org-html-inline-image-rules nil nil #'equal)
          ".*")))

(defun edraw-org-export-latex-setup ()
  (defvar org-latex-inline-image-rules) ;;ox-latex.el
  (with-eval-after-load 'ox-latex
    (setf (alist-get edraw-org-link-type
                     org-latex-inline-image-rules nil nil #'equal)
          ".*")))

(defvar edraw-org-export-current-link nil)

(defun edraw-org-export-ad-export-custom-protocol-maybe
    (old-func link &rest args)
  (let ((edraw-org-export-current-link link))
    (apply old-func link args)))

(autoload 'org-export-derived-backend-p "ox")

(defun edraw-org-link-export (path description back-end info)
  (let ((link edraw-org-export-current-link))
    (cond
     ((org-export-derived-backend-p back-end 'html)
      (edraw-org-export-html-link path description back-end info link))
     ((org-export-derived-backend-p back-end 'latex)
      (edraw-org-export-latex-link path description back-end info link)))))

(provide 'edraw-org)
;;; edraw-org.el ends here
