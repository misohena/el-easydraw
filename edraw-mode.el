;;; edraw-mode.el --- Edraw File Editing Mode        -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Graphics, Drawing, SVG

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

;; (autoload 'edraw-mode "edraw-mode")
;; (add-to-list 'auto-mode-alist '("\\.edraw\\.svg$" . edraw-mode))
;; NOTE: Set later than other modes for .svg such as image-mode.

;;; Code:

(require 'edraw)

(defvar-local edraw-mode-editor nil)

(defun edraw-mode ()
  "Major mode for editing edraw SVG files.

The following commands are available:

\\{edraw-mode-map}"
  (interactive)

  (major-mode-suspend)

  (let* ((svg (edraw-decode-svg (buffer-substring-no-properties
                                 (point-min) (point-max))
                                nil))
         (editor (edraw-editor
                  :overlay (make-overlay (point-min) (point-max) nil nil t)
                  :svg svg
                  :document-writer 'edraw-mode-write-document)))
    (edraw-initialize editor)
    (setq-local edraw-mode-editor editor)

    ;; Setup Keymap
    (edraw-mode-transfer-overlay-keymap-to-local-map editor)

    ;; Setup modification tracking and data saving
    (edraw-add-hook editor 'change 'edraw-mode-on-changed)
    (add-hook 'before-save-hook 'edraw-mode-on-before-save nil t)
    (with-silent-modifications
      (add-text-properties (point-min) (point-max)
                           (list 'read-only t
                                 'front-sticky '(read-only))))

    ;; Setup major mode finalization
    (add-hook 'change-major-mode-hook 'edraw-mode-finalize-major-mode nil t)

    ;; Start major mode
    (setq mode-name "Easy Draw")
    (setq major-mode 'edraw-mode)
    (run-mode-hooks 'edraw-mode-hook)))



;;;; Finalize Major Mode

(defun edraw-mode-finalize-major-mode ()
  (edraw-mode-save)
  ;; Remove editor overlay
  (remove-overlays (point-min) (point-max))
  ;; Remove read only properties
  (with-silent-modifications
    (remove-list-of-text-properties (point-min) (point-max)
                                    '(read-only front-sticky)))
  (remove-hook 'change-major-mode-hook 'edraw-mode-on-change-major-mode t))



;;;; Keymap

(defvar-local edraw-mode-map nil)

(defun edraw-mode-transfer-overlay-keymap-to-local-map (editor)
  (let* ((ov (edraw-overlay editor))
         (keymap (overlay-get ov 'keymap)))
    ;; Transfer overlay's keymap property to local-map.
    (setq-local edraw-mode-map keymap)
    (use-local-map keymap)
    (overlay-put ov 'keymap nil) ;; remove keymap property
    ;; Track keymap changes.
    ;;(edraw-add-hook editor 'keymap-change 'edraw-mode-on-keymap-change)

    editor))

;; (defun edraw-mode-on-keymap-change (_editor keymap)
;;   (setq-local edraw-mode-map keymap)
;;   (use-local-map keymap)
;;   ;; Return t. EDITOR does not change the overlay's keymap.
;;   t)



;;;; Modification Tracking and Data Saving

(defconst edraw-mode-compress-file-p nil) ;;browser does not support svgz

(defun edraw-mode-on-changed (_type)
  (set-buffer-modified-p t))

(defun edraw-mode-on-before-save ()
  (edraw-mode-save))

(defun edraw-mode-write-document (svg)
  (let ((text (edraw-encode-svg
               svg nil edraw-mode-compress-file-p)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text))))

(defun edraw-mode-save ()
  (when edraw-mode-editor
   (edraw-save edraw-mode-editor)))


(provide 'edraw-mode)
;;; edraw-mode.el ends here
