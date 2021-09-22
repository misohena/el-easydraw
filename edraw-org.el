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
(require 'edraw-util)

(eval-when-compile
  (require 'ox-html)
  (require 'edraw))



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
  "Embed Drawing Editor for Org-mode"
  :prefix "edraw-org-"
  :group 'multimedia
  :group 'org)

(defcustom edraw-org-link-export-data-tag 'svg
  "HTML tag used to export data links.

img = Data URI scheme (<img src=\"data:image/svg+xml;base64,.....\">)
svg = Embed SVG element (<svg>...</svg>)
"
  :group 'edraw-org
  :type '(choice (const :tag "<img>" img)
                 (const :tag "<svg>" svg)
                 (function)))

(defcustom edraw-org-link-export-file-tag 'img
  "HTML tag used to export file links.

img = Simple file link (<img src=\"<path-to-file>\">)
svg = Embed SVG element (<svg>...</svg>)
"
  :group 'edraw-org
  :type '(choice (const :tag "<img>" img)
                 (const :tag "<svg>" svg)
                 (function)))

(defcustom edraw-org-link-compress-data-p t
  "Non-nil means compress SVG on save to data links."
  :group 'edraw-org
  :type '(boolean))

(defcustom edraw-org-link-compress-file-p nil
  "Non-nil means compress SVG on save to file.

NOTE: Web browsers do not support compressed SVG files."
  :group 'edraw-org
  :type '(boolean))


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
   :help-echo 'edraw-org-link-help-echo))

(with-eval-after-load 'org
  ;; Registering the link type is necessary for org-element-context to
  ;; return the correct link type.
  (edraw-org-link-register-type))

(defun edraw-org-link-help-echo (_window object pos)
  (when (bufferp object)
    (with-current-buffer object
      (goto-char pos)
      (when-let ((link-element (edraw-org-link-at-point))
                 (link-props (car (edraw-org-link-element-link-properties
                                   link-element))))
        (if-let ((file (edraw-org-link-prop-file link-props)))
            (format "edraw:file=%s" file)
          (if-let ((data (edraw-org-link-prop-data link-props)))
              (format "edraw:data=(%s chars)" (length data))))))))

;;;;; Link Properties

(defun edraw-org-link-unescape (value &optional replace-brackets-p)
  (replace-regexp-in-string
   (if replace-brackets-p "\\\\\\([][;\\\\]\\)" "\\\\\\([;\\\\]\\)")
   "\\1" value t nil))

(defun edraw-org-link-escape (value &optional replace-brackets-p)
  (replace-regexp-in-string
   (if replace-brackets-p "\\([][;\\\\]\\)" "\\([;\\\\]\\)")
   "\\\\\\1" value t nil))

(defun edraw-org-link-props-parse (path &optional in-description-p)
  (when path
    ;; Remove "edraw:" at the beginning of PATH just in case.
    ;; If the link type is not registered, edraw: may remain at the beginning.
    (setq path (string-remove-prefix (format "%s:" edraw-org-link-type) path))

    ;; Convert "A=B;C=D;..." to ((A . B) (C . D) ...)
    (mapcar
     (lambda (prop)
       (when (string-match "\\`\\([^=]*\\)=\\(.*\\)\\'" prop)
         (cons
          (string-trim (match-string 1 prop))
          (edraw-org-link-unescape
           (string-trim (match-string 2 prop))
           ;; If PATH is in the description part, brackets have not
           ;; yet been unescaped
           in-description-p))))
     (split-string path ";" t "[ \t\n\r]+"))))

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
      (ignore-errors (cons (base64-decode-string data) t))
    (if-let ((file (edraw-org-link-prop-file link-props)))
        (if (file-exists-p file) (cons file nil)))))

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

(defun edraw-org-link-element-path-in-description (link-element)
  "Return the string after \"edraw:\" in the description of LINK-ELEMENT."
  (when-let ((c-begin (org-element-property :contents-begin link-element))
             (c-end (org-element-property :contents-end link-element)))
    (let ((desc (buffer-substring-no-properties c-begin c-end)))
      (when (string-match (format "\\`%s:\\(.*\\)\\'" edraw-org-link-type) desc)
        (match-string 1 desc)))))

(defun edraw-org-link-element-link-properties (link-element)
  "Return the property alist of LINK-ELEMENT.

Return a cons cell (LINK-PROPS . IN-DESCRIPTION-P)."
  (if-let ((desc (edraw-org-link-element-path-in-description link-element)))
      ;; from description part
      (cons (edraw-org-link-props-parse desc t)
            t)
    ;; from path part
    (if (equal (org-element-property :type link-element)
               edraw-org-link-type)
        (let ((path (org-element-property :path link-element)))
          (cons (edraw-org-link-props-parse path nil)
                nil)))))



;;;; Inline Link Image

;;
;; Display edraw link image inline
;;

(define-minor-mode edraw-org-link-image-mode ()
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
        (edraw-org-link-image-remove-region last-end end)))))

(defun edraw-org-link-image-remove-region (beg end)
  ;;(remove-overlays beg end 'edraw-org-link-image-p t) ;;move the endpoints? split?
  (mapc #'delete-overlay (edraw-org-link-image-overlays-in beg end)))

(defun edraw-org-link-image-update (link-begin link-end link-element)
  (when-let (link-props (car
                         (edraw-org-link-element-link-properties link-element)))
    (let* ((ovs (edraw-org-link-image-overlays-in link-begin link-end))
           (ov (progn
                 ;; Remove redundant overlays
                 (mapc #'delete-overlay (cdr ovs))
                 (car ovs)))
           (visible (if ov
                        (overlay-get ov 'edraw-org-link-image-visible)
                      (null (edraw-org-link-editor-overlays-in
                             link-begin link-end))))
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
      (create-image (car file-or-data) 'svg (cdr file-or-data)))))

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

;;

(defconst edraw-org-link-image-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km org-mouse-map)
    (define-key km [mouse-3] 'edraw-org-link-image-menu-at-mouse)
    (define-key km (kbd "C-c C-o") 'edraw-org-link-image-open-at-point)
    km))

(defun edraw-org-link-image-menu-at-mouse (ev)
  (interactive "e")

  (mouse-set-point ev)
  (edraw-popup-menu
   "Edraw Link Menu"
   `(((edraw-msg "Edit") edraw-org-edit-link)
     ((edraw-msg "Find File") edraw-org-link-image-find-file-at-mouse
      :visible (edraw-org-link-at-description-link-p)))))

(defun edraw-org-link-at-description-link-p ()
  (let ((link-element (edraw-org-link-at-point)))
    (and link-element
         (not (null (edraw-org-link-element-path-in-description link-element))))))

(defun edraw-org-link-image-open-at-point (&optional arg)
  (interactive)
  (if (and (edraw-org-link-at-description-link-p)
           (y-or-n-p "Edit edraw link? (y:edit link, n:open link)"))
      (edraw-org-edit-link)
    (org-open-at-point arg)))

(defun edraw-org-link-image-find-file-at-mouse ()
  (org-open-at-point 'in-emacs))


;;;; Inline Link Edit

;;
;; Edit edraw link image inline
;;

(defvar edraw-org-enable-modification nil)

(defun edraw-org-link-editor-overlays-in (beg end)
  (seq-filter
   (lambda (ov) (overlay-get ov 'edraw-editor))
   (overlays-in beg end)))

(defun edraw-org-edit-link (&optional _path _arg)
  (interactive)

  (require 'edraw)

  ;; Get link element & link properties
  (when-let ((link-element (edraw-org-link-at-point))
             (link-props-and-place (edraw-org-link-element-link-properties
                                    link-element)))

    (let* ((link-begin (org-element-property :begin link-element))
           (link-end (org-element-property :end link-element))
           (link-props (car link-props-and-place)))
      ;; Make sure the editor overlay doesn't exist yet
      (when (edraw-org-link-editor-overlays-in link-begin link-end)
        (error "Editor already exists"))

      ;; Hide inline link image if it exists
      (when-let ((image-overlay (edraw-org-link-image-overlay-at link-begin)))
        (edraw-org-link-image-set-visible image-overlay nil))

      ;; Create editor
      (let* ((editor-overlay (make-overlay link-begin link-end nil t nil))
             (editor (edraw-editor
                      :overlay editor-overlay
                      :svg (edraw-org-link-load-svg link-props)
                      :document-writer (edraw-org-link-make-writer
                                        editor-overlay
                                        edraw-org-link-compress-data-p
                                        edraw-org-link-compress-file-p)
                      :menu-filter #'edraw-org-link-editor-menu-filter
                      )))
        (edraw-initialize editor)
        ;;(overlay-put editor-overlay 'evaporate t)
        (overlay-put editor-overlay 'modification-hooks
                     (list (lambda (_ov _after-p _beg _end &optional _len)
                             (unless edraw-org-enable-modification
                               (error "There is an edraw-editor within modification range. Please close the editor")))))
        ;; Add key bindings
        (overlay-put editor-overlay 'keymap
                     (edraw-org-link-editor-make-keymap
                      (or (overlay-get editor-overlay 'keymap)
                          edraw-editor-map))))
      ;; Hook kill buffer
      (edraw-org-link-hook-kill-buffer-query)

      (message "%s" (substitute-command-keys "\\[edraw-org-link-finish-edit]:Finish Edit, \\[edraw-org-link-cancel-edit]:Cancel Edit")))))

(defun edraw-org-link-load-svg (link-props)
  (if-let ((data (edraw-org-link-prop-data link-props)))
      (edraw-decode-svg data t)
    (if-let ((file (edraw-org-link-prop-file link-props)))
        (if (file-exists-p file)
            (edraw-read-svg-from-file file)))))

(defun edraw-org-link-make-writer (editor-overlay data-gzip-p file-gzip-p)
  (lambda (svg)
    (edraw-org-link-save-svg editor-overlay svg data-gzip-p file-gzip-p)))

(defun edraw-org-link-save-svg (editor-overlay svg data-gzip-p file-gzip-p)
  ;; Move to beginning of editing link
  (let ((buffer (overlay-buffer editor-overlay)))
    (unless buffer
      (error "The editor's overlay has been removed"))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (overlay-start editor-overlay))
        ;; Get link element & parse properties
        (let* ((link-element
                (or (edraw-org-link-at-point)
                    (error "The edraw link currently being edited has been lost")))
               (link-props-and-place
                (or (edraw-org-link-element-link-properties link-element)
                    (error "The type of the editing link is not `edraw:'")))
               (link-props (car link-props-and-place))
               (in-description-p (cdr link-props-and-place)))
          (if-let ((file-path (edraw-org-link-prop-file link-props)))
              ;; file
              (progn
                (edraw-write-svg-to-file svg file-path file-gzip-p) ;;signal an error
                (image-refresh (edraw-org-link-image-create link-props))
                t)
            ;;data
            (setf (alist-get "data" link-props nil nil #'string=)
                  (edraw-encode-svg svg t data-gzip-p))
            (let ((edraw-org-enable-modification t)) ;; call modification hooks(inhibit-modification-hooks=nil) but allow modification. If inhibit-modification-hooks is t, inline images not updated.
              (unless (edraw-org-link-replace-at-point
                       (concat edraw-org-link-type ":"
                               (edraw-org-link-props-to-string link-props))
                       (if in-description-p 'description 'path))
                (error "Failed to replace edraw link")))
            t))))))

(defun edraw-org-link-editor-menu-filter (menu-type items)
  (pcase menu-type
    ('main-menu
     (append
      items
      '(((edraw-msg "Finish Edit") edraw-org-link-finish-edit)
        ((edraw-msg "Cancel Edit") edraw-org-link-cancel-edit))))
    (_ items)))

(defun edraw-org-link-editor-make-keymap (original-keymap)
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km original-keymap)
    (define-key km (kbd "C-c C-c") 'edraw-org-link-finish-edit)
    (define-key km (kbd "C-c C-k") 'edraw-org-link-cancel-edit)
    km))

(defun edraw-org-link-finish-edit (&optional editor)
  (interactive)
  (when-let ((editor (or editor (edraw-editor-at-input last-input-event))))
    (when (or (not (edraw-modified-p editor))
              (condition-case err
                  (edraw-save editor)
                (error
                 (yes-or-no-p
                  (format
                   (edraw-msg "Failed to save. %s. Discard changes?")
                   (error-message-string err))))))
      (edraw-org-link-close-editor editor))))

(defun edraw-org-link-cancel-edit (&optional editor)
  (interactive)
  (when-let ((editor (or editor (edraw-editor-at-input last-input-event))))
    (when (or (null (edraw-modified-p editor))
              (yes-or-no-p (edraw-msg "Discard changes?")))
      (edraw-org-link-close-editor editor))))

(defun edraw-org-link-close-editor (editor)
  ;; show link image
  (when-let ((editor-overlay (oref editor overlay))
             (buffer (overlay-buffer editor-overlay)))
    (with-current-buffer buffer
      (save-excursion
        (when-let ((image-overlay (edraw-org-link-image-overlay-at
                                   (overlay-start editor-overlay))))
          (edraw-org-link-image-set-visible image-overlay t)))))
  ;; delete editor overlay
  (edraw-close editor))

(defun edraw-org-link-hook-kill-buffer-query ()
  (add-hook 'kill-buffer-query-functions
            'edraw-org-link-kill-buffer-query nil t))

(defun edraw-org-link-kill-buffer-query ()
  (save-excursion
    (save-restriction
      (cl-loop for ov in (overlays-in (point-min) (point-max))
               do (when-let ((editor (overlay-get ov 'edraw-editor)))
                    (when (edraw-modified-p editor)
                      (goto-char (overlay-start ov))
                      (when (y-or-n-p
                             "Edraw editor has unsaved changes. Save it ?")
                        (edraw-save editor)))))))
  t)



;;;; Export

;;
;;

(defun edraw-org-link-setup-exporter ()
  (with-eval-after-load 'ox-html
    (setf (alist-get edraw-org-link-type
                     org-html-inline-image-rules nil nil #'equal)
          ".*")
    (advice-add 'org-export-custom-protocol-maybe :around 'edraw-org-advice-export-custom-protocol-maybe)))

(defvar edraw-org-current-link nil)

(defun edraw-org-advice-export-custom-protocol-maybe (old-func link &rest args)
  (let ((edraw-org-current-link link))
    (apply old-func link args)))

(defun edraw-org-link-export (path _description back-end info)
  ;; path is unescaped : \[ \] => [ ]
  ;; description is not unescaped : \[ \] => \[ \]
  (require 'edraw)
  (let ((link edraw-org-current-link))

    (pcase back-end
      ('html
       (if-let ((link-props (edraw-org-link-props-parse path)))
           (if-let ((data (edraw-org-link-prop-data link-props)))
               (pcase edraw-org-link-export-data-tag
                 ('svg (edraw-org-link-html-link-to-svg link-props link info))
                 ('img (edraw-org-link-data-to-img data link info))
                 ((and (pred functionp)
                       func)
                  (funcall func data))
                 (_ (edraw-org-link-html-link-to-svg link-props link info)))
             (if-let ((file (edraw-org-link-prop-file link-props)))
                 (pcase edraw-org-link-export-file-tag
                   ('svg (edraw-org-link-html-link-to-svg link-props link info))
                   ('img (edraw-org-link-file-to-img file link info))
                   ((and (pred functionp)
                         func)
                    (funcall func file))
                   (_ (edraw-org-link-file-to-img file link info)))
               ""))
         "")))))

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

Currently this function does two things:

Sets the attribute specified by #+ATTR_HTML to the svg root element.

Guarantees the uniqueness of ids defined by the SVG in the exported HTML. Add a random string to id."
  ;; Apply attributes specified by #+ATTR_HTML to the root svg element
  (cl-loop for (key value) on attributes by #'cddr
           do (dom-set-attribute svg key value))

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
                ;; The target elements are #edraw-body, #edraw-defs,
                ;; elements under #edraw-defs.
                (append
                 (list
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

(provide 'edraw-org)
;;; edraw-org.el ends here
