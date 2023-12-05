;;; edraw-org-edit.el --- Edit edraw link in Org-mode -*- lexical-binding: t; -*-

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

(require 'edraw)
(require 'edraw-org)

(declare-function image-flush "image.c")

;;;; Edit edraw link inline

(defvar edraw-org-enable-modification nil)

;;@todo Use edraw-edit-svg?

;;;###autoload
(defun edraw-org-edit-link (&optional _path _arg)
  "Edit the `edraw:' link at point.

_PATH and _ARG are not used. These are passed in when called from
org-link, but since these pieces of information aren't quite
enough, this function parses the link at the current position on
its own."
  (interactive)

  (require 'edraw)

  ;; Get link element & link properties
  (when-let ((link-element (edraw-org-link-at-point))
             (link-props-place-type (edraw-org-link-element-link-properties
                                     link-element nil)))

    (let* ((link-begin (org-element-property :begin link-element))
           (link-end (org-element-property :end link-element))
           (link-props (nth 0 link-props-place-type)))
      ;; Make sure the editor overlay doesn't exist yet
      (when (edraw-editor-overlays-in link-begin link-end)
        (error "Editor already exists"))

      ;; Hide inline link image if it exists
      (when-let ((image-overlay (edraw-org-link-image-overlay-at link-begin)))
        (edraw-org-link-image-set-visible image-overlay nil))

      ;; Remove mouse-face text property
      (edraw-org-link-remove-mouse-face link-begin link-end)

      ;; Create editor
      (let* ((editor-overlay (make-overlay link-begin link-end nil t nil))
             (_editor (edraw-editor
                       :overlay editor-overlay
                       :svg (edraw-org-link-load-svg link-props t)
                       :document-writer (edraw-org-link-make-writer
                                         editor-overlay
                                         edraw-org-link-compress-data-p
                                         edraw-org-link-compress-file-p)
                       :document-writer-accepts-top-level-comments-p t
                       :menu-filter #'edraw-org-link-editor-menu-filter)))
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
      (add-hook 'kill-buffer-query-functions 'edraw-buffer-kill-query nil t)

      (message "%s" (substitute-command-keys "\\[edraw-org-link-finish-edit]:Finish Edit, \\[edraw-org-link-cancel-edit]:Cancel Edit")))))

(defun edraw-org-link-load-svg (link-props
                                &optional accepts-top-level-comments-p)
  (if-let ((data (edraw-org-link-prop-data link-props)))
      (edraw-svg-decode-svg data t accepts-top-level-comments-p)
    (if-let ((file (edraw-org-link-prop-file link-props)))
        (if (file-exists-p file)
            (edraw-svg-read-from-file file accepts-top-level-comments-p)))))

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
               (link-begin (org-element-property :begin link-element))
               (link-end (org-element-property :end link-element))
               (link-props-place-type
                (or (edraw-org-link-element-link-properties link-element t)
                    (error "The type of the editing link is not `edraw:'")))
               (link-props (nth 0 link-props-place-type))
               (in-description-p (nth 1 link-props-place-type)))
          (if-let ((file-path (edraw-org-link-prop-file link-props)))
              ;; file
              (progn
                (edraw-svg-write-to-file svg file-path file-gzip-p) ;;signal an error
                ;; Update inline image
                (image-flush (edraw-org-link-image-create link-props)) ;;update image if overlay already exists
                (edraw-org-link-image-update link-begin link-end link-element) ;;create a new overlay if not exists
                t)
            ;;data
            (setf (alist-get "data" link-props nil nil #'string=)
                  (edraw-svg-encode svg t data-gzip-p))
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
  (let ((editor (or editor (edraw-current-editor))))
    (when (or (not (edraw-modified-p editor))
              (condition-case err
                  (edraw-save editor)
                (error
                 (message "Error=%s" (prin1-to-string err))
                 (yes-or-no-p
                  (format
                   (edraw-msg "Failed to save. %s. Discard changes?")
                   (error-message-string err))))))
      (edraw-org-link-close-editor editor))))

(defun edraw-org-link-cancel-edit (&optional editor)
  (interactive)
  (when-let ((editor (or editor (edraw-current-editor))))
    (when (or (null (edraw-modified-p editor))
              (yes-or-no-p (edraw-msg "Discard changes?")))
      (edraw-org-link-close-editor editor))))

(defun edraw-org-link-close-editor (editor)
  ;; show link image
  (when-let ((editor-overlay (oref editor overlay))
             (buffer (overlay-buffer editor-overlay)))
    (with-current-buffer buffer
      (save-excursion
        ;; Recover inline image
        (when-let ((image-overlay (edraw-org-link-image-overlay-at
                                   (overlay-start editor-overlay))))
          (edraw-org-link-image-set-visible image-overlay t))
        ;; Recover mouse-face
        (edraw-org-link-recover-mouse-face (overlay-start editor-overlay)
                                           (overlay-end editor-overlay)))))
  ;; delete editor overlay
  (edraw-close editor))


;;;; Edit regular file link inline

;;;###autoload
(defun edraw-org-edit-regular-file-link ()
  "Edit the `file:' link to the .edraw.svg file at point.

There is a `[[edraw:file=somefile.edraw.svg]]' format for embedding
an external SVG file in an Org document, but if you don't want to
use `edraw:' link type and want to use the regular `file:' link type
([[file:somefile.edraw.svg]] format), this function might help."
  (interactive)
  (let* ((link (or (org-element-context)
                   (error (edraw-msg "No link at point"))))
         (type (org-element-property :type link))
         (path (org-element-property :path link))
         (beg (org-element-property :begin link))
         (end (org-element-property :end link)))
    (unless (equal type "file")
      (error (edraw-msg "The link at point is not of type `file:'")))
    (unless (string-suffix-p ".edraw.svg" path t)
      (error (edraw-msg "The extension is not .edraw.svg")))
    (when (edraw-editor-overlays-in beg end)
      (error "Editor already exists"))

    ;;Remove org-mode inline image
    (edraw-org--remove-org-inline-images beg end)

    (edraw-edit-svg (when (file-exists-p path)
                      (edraw-svg-read-from-file path t))
                    'edraw-svg
                    beg end
                    (lambda (_ok _svg)
                      ;; Restore org-mode inline image
                      ;;@todo Add settings?
                      (org-display-inline-images nil t beg end))
                    (lambda (svg)
                      (edraw-svg-write-to-file svg path nil)
                      t)
                    ;; Keep file's top-level comments
                    t)))

(defun edraw-org--remove-org-inline-images (beg end)
  (if (version<= "9.6" (org-version))
      (with-no-warnings
        (org-remove-inline-images beg end)) ;; Can pass BEG and END
    ;; Can't pass BEG and END in 9.5 or earlier
    (dolist (ov (overlays-in beg end))
      (when (memq ov org-inline-image-overlays)
        (setq org-inline-image-overlays (delq ov org-inline-image-overlays))
        (delete-overlay ov)))))


;;;; Link Tools

;;;###autoload
(defun edraw-org-link-copy-contents-at-point ()
  "Copies the contents of the link at point.

Copies all shapes inside the data to the clipboard. Copied shapes
can be pasted in the editor or in the shape picker (custom shape list)."
  (interactive)
  (let* ((link-element (edraw-org-link-at-point))
         (props (car (edraw-org-link-element-link-properties link-element nil t)))
         (svg (edraw-org-link-load-svg props t)))
    (unless link-element
      (error (edraw-msg "No link at point")))
    (unless svg
      (error (edraw-msg "Link at point does not contain valid data")))

    (edraw-clipboard-set
     'shape-descriptor-list
     (cl-loop for node in (dom-children (edraw-get-document-body svg))
              for desc = (edraw-shape-descriptor-from-svg-element-without-editor
                          node)
              when desc collect desc))))

(provide 'edraw-org-edit)
;;; edraw-org-edit.el ends here
