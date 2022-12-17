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
                                    link-element nil)))

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
      (add-hook 'kill-buffer-query-functions 'edraw-buffer-kill-query nil t)

      (message "%s" (substitute-command-keys "\\[edraw-org-link-finish-edit]:Finish Edit, \\[edraw-org-link-cancel-edit]:Cancel Edit")))))

(defun edraw-org-link-load-svg (link-props)
  (if-let ((data (edraw-org-link-prop-data link-props)))
      (edraw-svg-decode data t)
    (if-let ((file (edraw-org-link-prop-file link-props)))
        (if (file-exists-p file)
            (edraw-svg-read-from-file file)))))

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
               (link-props-and-place
                (or (edraw-org-link-element-link-properties link-element t)
                    (error "The type of the editing link is not `edraw:'")))
               (link-props (car link-props-and-place))
               (in-description-p (cdr link-props-and-place)))
          (if-let ((file-path (edraw-org-link-prop-file link-props)))
              ;; file
              (progn
                (edraw-svg-write-to-file svg file-path file-gzip-p) ;;signal an error
                ;; Update inline image
                (image-refresh (edraw-org-link-image-create link-props)) ;;update image if overlay already exists
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
        (when-let ((image-overlay (edraw-org-link-image-overlay-at
                                   (overlay-start editor-overlay))))
          (edraw-org-link-image-set-visible image-overlay t)))))
  ;; delete editor overlay
  (edraw-close editor))



(provide 'edraw-org-edit)
;;; edraw-org-edit.el ends here
