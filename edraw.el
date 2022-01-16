;;; edraw.el --- Emacs Easy Draw                    -*- lexical-binding: t; -*-

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

;;;

;; (when (re-search-forward "<EDITOR>" nil t) (edraw-editor-create (list (match-beginning 0) (match-end 0) nil nil nil 'evaporate t)))
;; <EDITOR>


;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'edraw-math)
(require 'edraw-path)
(require 'edraw-dom-svg)
(require 'edraw-util)
(require 'edraw-color-picker)

;;;; Editor

;; edraw-editor------------------------------------------+
;;  | |    |                                             |0..1 selected-handle
;;  | |    |1                                            |0..1 selected-anchor
;;  | |  svg-node *<-+ (<-SVG DOM Tree)               edraw-shape-point<|----+
;;  | |    |1   1`---+                                                        |
;;  | |    |0..1                                                              |
;;  | | edraw-shape <|--+-edraw-shape-rect 1------8 edraw-shape-point-rect ---|
;;  | | 0..1|sel-shape  |-edraw-shape-ellipse 1---8 edraw-shape-point-ellipse-|
;;  | +-----+           |-edraw-shape-text 1------1 edraw-shape-point-text ---|
;;  |                   `-edraw-shape-path 1------* edraw-shape-point-path ---+
;;  | 1 tool
;;  `---edraw-editor-tool <|--+-edraw-editor-tool-select
;;                            |-edraw-editor-tool-rect
;;                            |-edraw-editor-tool-ellipse
;;                            |-edraw-editor-tool-text
;;                            `-edraw-editor-tool-path

;;;;; Editor - Variables

(defgroup edraw-editor nil
  "Emacs Easy Draw Editor"
  :prefix "edraw-editor-"
  :group 'multimedia)

(defvar edraw-default-document-properties
  '((width . 560)
    (height . 420)
    (background . "#fff")))
(defvar edraw-default-shape-properties
  ;; To customize property, do the following:
  ;; (with-eval-after-load "edraw"
  ;;   (setf (alist-get 'font-family (alist-get 'text edraw-default-shape-properties)) "Yu Gothic"))
  '((rect
     (stroke . "none")
     (fill . "#ddd")
     (rx . 10)
     (ry . 10))
    (ellipse
     (stroke . "none")
     (fill . "#ddd"))
    (path
     (stroke . "#999")
     (stroke-width . 4)
     (fill . "none")
     ;;(marker-end . "arrow")
     )
    (text
     (font-family . "sans-serif")
     (font-size . 18)
     (text-anchor . "middle")
     (fill . "#222"))))

(defcustom edraw-editor-share-default-shape-properties nil
  "non-nil means that the editors change edraw-default-shape-properties directly."
  :group 'edraw-editor
  :type 'boolean)

(defcustom edraw-editor-default-grid-interval 20
  "The interval of grid lines."
  :group 'edraw-editor
  :type 'number)

(defcustom edraw-editor-default-grid-visible t
  "non-nil means grid lines are displayed by default."
  :group 'edraw-editor
  :type 'boolean)

(defcustom edraw-editor-default-transparent-bg-visible nil
  "non-nil means the transparent background is colored by default."
  :group 'edraw-editor
  :type 'boolean)
(defcustom edraw-editor-transparent-bg-color1 "#ffffff"
  "The first color of the transparent background."
  :group 'edraw-editor
  :type 'string)
(defcustom edraw-editor-transparent-bg-color2 "#cccccc"
  "The second color of the transparent background."
  :group 'edraw-editor
  :type 'string)
(defcustom edraw-editor-transparent-bg-grid-size 8
  "The grid interval of the transparent background."
  :group 'edraw-editor
  :type 'number)

(defconst edraw-anchor-point-radius 3.5)
(defconst edraw-handle-point-radius 3.0)
(defconst edraw-anchor-point-input-radius (+ 1.0 edraw-anchor-point-radius))
(defconst edraw-handle-point-input-radius (+ 2.0 edraw-handle-point-radius))

(defvar edraw-snap-text-to-shape-center t)

(defvar edraw-editor-move-point-on-click t)

(defvar edraw-editor-map
  (let ((km (make-sparse-keymap)))
    (define-key km [down-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [double-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [mouse-3] 'edraw-editor-dispatch-event)
    (define-key km [C-down-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [C-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [S-down-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [S-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km "m" 'edraw-editor-main-menu)
    (define-key km "s" 'edraw-editor-select-tool-select)
    (define-key km "r" 'edraw-editor-select-tool-rect)
    (define-key km "e" 'edraw-editor-select-tool-ellipse)
    (define-key km "a" 'edraw-editor-select-tool-path)
    (define-key km "f" 'edraw-editor-select-tool-freehand)
    (define-key km "t" 'edraw-editor-select-tool-text)
    (define-key km "F" 'edraw-editor-edit-tool-default-fill)
    (define-key km "S" 'edraw-editor-edit-tool-default-stroke)
    (define-key km "#" 'edraw-editor-toggle-grid-visible)
    (define-key km (kbd "M-#") 'edraw-editor-set-grid-interval)
    (define-key km "\"" 'edraw-editor-toggle-transparent-bg-visible)
    (define-key km "db" 'edraw-editor-set-background)
    (define-key km "dr" 'edraw-editor-set-size)
    (define-key km "dt" 'edraw-editor-translate-all-shapes)
    (define-key km "ds" 'edraw-editor-scale-all-shapes)
    (define-key km "deb" 'edraw-editor-export-to-buffer)
    (define-key km "def" 'edraw-editor-export-to-file)
    (define-key km "ded" 'edraw-editor-export-debug-svg-to-buffer)
    (define-key km "z" 'edraw-editor-undo)
    (define-key km "Z" 'edraw-editor-redo)
    ;; Selected Object
    (define-key km (kbd "C-c C-x C-y") 'edraw-editor-paste-and-select)
    (define-key km (kbd "C-c C-x C-w") 'edraw-editor-cut-selected-shapes)
    (define-key km (kbd "C-c C-x M-w") 'edraw-editor-copy-selected-shapes)
    (define-key km (kbd "<delete>") 'edraw-editor-delete-selected)
    (define-key km (kbd "<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "S-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "S-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "S-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "S-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "}") 'edraw-editor-bring-selected-to-front)
    (define-key km (kbd "]") 'edraw-editor-bring-selected-forward)
    (define-key km (kbd "[") 'edraw-editor-send-selected-backward)
    (define-key km (kbd "{") 'edraw-editor-send-selected-to-back)
    (define-key km (kbd "M-]") 'edraw-editor-select-next-shape)
    (define-key km (kbd "M-[") 'edraw-editor-select-previous-shape)
    km))

;;;;; Editor - Constructor

(defun edraw-editor-create (overlay-spec &optional svg)
  "Create a new editor object."
  (let ((overlay
         (cond ((overlayp overlay-spec)
                overlay-spec)
               ((listp overlay-spec)
                (let ((ov (apply 'make-overlay (seq-take overlay-spec 5)))
                      (props (nthcdr 5 overlay-spec)))
                  (cl-loop for (key value) on props by 'cddr
                           do (overlay-put ov key value))
                  ov))
               (t (error "Invalid overlay-spec")))))

    (let ((editor (edraw-editor :overlay overlay :svg svg)))
      (edraw-initialize editor)
      editor)))

(defclass edraw-editor ()
  ((overlay :initarg :overlay :initform nil :reader edraw-overlay)
   (svg :initarg :svg :initform nil)
   (svg-document-size)
   (defrefs)
   (document-writer :initarg :document-writer :initform nil)
   (menu-filter :initarg :menu-filter :initform nil)

   (image-scale
    :initform (image-compute-scaling-factor image-scaling-factor)
    :type number)
   (image)
   (image-update-timer :initform nil)
   (settings
    :initform (list (cons 'grid-visible
                          edraw-editor-default-grid-visible)
                    (cons 'grid-interval
                          edraw-editor-default-grid-interval)
                    (cons 'transparent-bg-visible
                          edraw-editor-default-transparent-bg-visible)
                    ))
   (default-shape-properties
     :initform (if edraw-editor-share-default-shape-properties
                   edraw-default-shape-properties ;;@todo observe changes made by other editors
                 (copy-tree edraw-default-shape-properties)))
   (tool :initform nil :type (or null edraw-editor-tool))
   (selected-shapes :initform nil :type list)
   (selected-anchor :initform nil :type (or null edraw-shape-point))
   (selected-handle :initform nil :type (or null edraw-shape-point))
   (modified-p :initform nil)
   (undo-list :initform nil)
   (redo-list :initform nil)
   (hooks))
  "Editor")

(cl-defmethod edraw-initialize ((editor edraw-editor))
  (oset editor hooks (list
                      (cons 'change (edraw-hook-make))
                      (cons 'selection-change (edraw-hook-make))))

  (edraw-editor-clear-undo-vars)
  (edraw-editor-clear-modified-vars)

  (with-slots (overlay) editor
    (overlay-put overlay 'edraw-editor editor)
    (overlay-put overlay 'keymap edraw-editor-map)
    ;;(overlay-put overlay 'evaporate t)
    (overlay-put overlay 'pointer 'arrow)
    (overlay-put overlay 'help-echo
                 (lambda (_window _object _pos) nil)) ;;Suppress org link's echo
    (overlay-put overlay 'face 'default)) ;;Suppress org link's underline

  (edraw-initialize-svg editor)
  (edraw-update-image editor)

  (edraw-update-toolbar editor)
  (edraw-select-tool editor (edraw-editor-make-tool 'rect))
  ;; Return editor
  editor)

(cl-defmethod edraw-close ((editor edraw-editor))
  (with-slots (overlay) editor
    (when (and overlay (overlay-buffer overlay))
      (edraw-notify-document-close-to-all-shapes editor) ;;should edraw-clear?
      (edraw-update-image-timer-cancel editor)
      (delete-overlay overlay))))

;;;;; Editor - User Settings

(cl-defmethod edraw-get-setting ((editor edraw-editor) key)
  (alist-get key (oref editor settings)))
(cl-defmethod edraw-set-setting ((editor edraw-editor) key value)
  (setf (alist-get key (oref editor settings)) value))

;;;;; Editor - Hooks

(cl-defmethod edraw-add-hook ((editor edraw-editor) hook-type
                              function &rest args)
  (with-slots (hooks) editor
    (when-let ((hook (alist-get hook-type hooks)))
      (apply 'edraw-hook-add hook function args))))

(cl-defmethod edraw-remove-hook ((editor edraw-editor) hook-type
                                 function &rest args)
  (with-slots (hooks) editor
    (when-let ((hook (alist-get hook-type hooks)))
      (apply 'edraw-hook-remove hook function args))))

(cl-defmethod edraw-call-hook ((editor edraw-editor) hook-type
                               &rest args)
  (with-slots (hooks) editor
    (when-let ((hook (alist-get hook-type hooks)))
      (apply 'edraw-hook-call hook args))))

;;;;; Editor - Define Commands

(defun edraw-editor-call-at-point (method &optional editor)
  (when-let ((editor (or editor (edraw-editor-at-input last-input-event))))
    (funcall method editor)))

(defmacro edraw-editor-defcmd (method)
  (let ((method-name (symbol-name method)))
    (unless (string-match "\\`edraw-\\(.+\\)\\'" method-name)
      (error "Method name %s does not start with edraw-" method-name))
    (let ((suffix (match-string 1 method-name)))
      `(defun ,(intern (concat "edraw-editor-" suffix)) (&optional editor)
         (interactive)
         (edraw-editor-call-at-point (quote ,method) editor)))))

;;;;; Editor - SVG Image Update

(cl-defmethod edraw-invalidate-image ((editor edraw-editor))
  "Request an image update."
  (with-slots (image-update-timer) editor
    (unless image-update-timer
      ;; Post update command
      (setq image-update-timer
            (run-at-time 0 nil 'edraw-update-image-on-timer editor)))))

(cl-defmethod edraw-update-image-on-timer ((editor edraw-editor))
  (with-slots (image-update-timer) editor
    (setq image-update-timer nil)
    (edraw-update-image editor)))

(cl-defmethod edraw-update-image-timer-cancel ((editor edraw-editor))
  (with-slots (image-update-timer) editor
    (when image-update-timer
      (cancel-timer image-update-timer)
      (setq image-update-timer nil))))

(cl-defmethod edraw-update-image ((editor edraw-editor))
  "Update the image and apply the image to the overlay."
  (with-slots (overlay svg image) editor
    (edraw-update-image-timer-cancel editor)
    (setq image (edraw-svg-to-image svg
                                    :scale 1.0)) ;;Cancel image-scale effect
    (overlay-put overlay 'display image)))

;;;;; Editor - SVG Structure

;; <svg width= height=> ;;Document(width=, height=)
;;   <defs id="edraw-defs"> ;;Document
;;     <linearGradient>
;;     <radialGradient>
;;     <marker>
;;   </defs>
;;   <rect id="edraw-background"> ;;Document
;;   <g id="edraw-body"> ;;Document
;;     <rect>
;;     <ellipse>
;;     <text>
;;     <path>
;;     <image>
;;   </g>
;;   <g id="edraw-ui-foreground">
;;     <defs id="edraw-ui-defs"></defs>
;;     <style id="edraw-ui-style"></style>
;;     <g id="edraw-ui-grid"></g>
;;   </g>
;; </svg>
;; ;; elements #edraw-ui-* are removed on save

(defconst edraw-editor-svg-defs-id "edraw-defs")
(defconst edraw-editor-svg-background-id "edraw-background")
(defconst edraw-editor-svg-body-id "edraw-body")

(cl-defmethod edraw-initialize-svg ((editor edraw-editor))
  "Allocate the elements needed for the editor to work in the svg tree."
  (with-slots (svg) editor
    ;; Document Elements
    (edraw-initialize-svg-document editor) ;;set `svg'
    ;; UI Elements (Background)
    (edraw-ui-background-svg editor) ;;insert first
    (edraw-ui-defs-svg editor) ;;insert first
    (edraw-initialize-transparent-bg editor)
    ;; UI Elements (Foreground)
    (let ((fore-g (edraw-dom-get-or-create svg 'g "edraw-ui-foreground")))
      (edraw-dom-get-or-create fore-g 'style "edraw-ui-style")
      (edraw-dom-get-or-create fore-g 'g "edraw-ui-grid"))

    (edraw-update-root-transform editor) ;; Document & UI
    (edraw-update-ui-style-svg editor)
    (edraw-update-grid editor)))

(cl-defmethod edraw-ui-defs-svg ((editor edraw-editor))
  (with-slots (svg) editor
    (or (edraw-dom-get-by-id svg "edraw-ui-defs")
        (let ((defs (dom-node 'defs (list (cons 'id "edraw-ui-defs")))))
          (edraw-dom-insert-first svg defs)
          defs))))

(cl-defmethod edraw-ui-background-svg ((editor edraw-editor))
  (with-slots (svg) editor
    (or (edraw-dom-get-by-id svg "edraw-ui-background")
        (let ((g (dom-node 'g (list (cons 'id "edraw-ui-background")))))
          (edraw-dom-insert-first svg g)
          g))))

(cl-defmethod edraw-ui-foreground-svg ((editor edraw-editor))
  (with-slots (svg) editor
    (edraw-dom-get-by-id svg "edraw-ui-foreground")))

;; mix-blend-mode requires librsvg 2.50.0 or later
;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/607
(defconst edraw-editor-ui-style "
.edraw-ui-grid-line {
  stroke: rgba(30, 150, 255, 0.75);
  stroke-dasharray: 2;
  mix-blend-mode: difference;
}
.edraw-ui-anchor-point {
  stroke: #f88; fill: none;
}
.edraw-ui-anchor-point-selected {
  stroke: none; fill: #f88;
}
.edraw-ui-handle-point {
  stroke: #f88; fill: none;
}
.edraw-ui-handle-point-selected {
  stroke: none; fill: #f88;
}
.edraw-ui-handle-line {
  stroke: #f88; fill: none;
}
.edraw-ui-read-rectangle {
  stroke: #f88; fill: none;
  stroke-dasharray: 2;
}")

(cl-defmethod edraw-update-ui-style-svg ((editor edraw-editor))
  (with-slots (svg) editor
    (when-let ((style (edraw-dom-get-by-id svg "edraw-ui-style")))
      (edraw-dom-remove-all-children style)
      (dom-append-child style edraw-editor-ui-style))))

(cl-defmethod edraw-update-root-transform ((editor edraw-editor))
  (with-slots (svg image-scale) editor
    (let ((back-ui (edraw-ui-background-svg editor))
          (background (edraw-svg-background editor)) ;;element or nil
          (body (edraw-svg-body editor))
          (fore-ui (edraw-ui-foreground-svg editor)))
      (when svg
        (dom-set-attribute svg 'width
                           (ceiling (* image-scale (edraw-width editor))))
        (dom-set-attribute svg 'height
                           (ceiling (* image-scale (edraw-height editor)))))
      (when back-ui
        (dom-set-attribute back-ui 'transform
                           (format "scale(%s)" image-scale)))
      (when background
        (dom-set-attribute background 'transform
                           (format "scale(%s)" image-scale)));;@todo adjust width height? I think there will be lines at the right and bottom edges of the image
      (when body
        (dom-set-attribute body 'transform
                           (format "scale(%s)" image-scale)))
      (when fore-ui
        (dom-set-attribute fore-ui 'transform
                           (format "scale(%s) translate(0.5 0.5)" image-scale))))))

(defun edraw-editor-remove-root-transform (svg svg-document-size)
  (let ((back-ui (edraw-dom-get-by-id svg "edraw-ui-background")) ;;Do not use (edraw-ui-background-svg editor)
        (background (edraw-dom-get-by-id svg edraw-editor-svg-background-id))
        (body (edraw-dom-get-by-id svg edraw-editor-svg-body-id))
        (fore-ui (edraw-dom-get-by-id svg "edraw-ui-foreground"))) ;;Do not use (edraw-ui-foreground-svg editor)
    (when svg
      (dom-set-attribute svg 'width (car svg-document-size))
      (dom-set-attribute svg 'height (cdr svg-document-size)))
    (when back-ui
      (edraw-dom-remove-attr back-ui 'transform))
    (when background
      (edraw-dom-remove-attr background 'transform))
    (when body
      (edraw-dom-remove-attr body 'transform))
    (when fore-ui
      (edraw-dom-remove-attr fore-ui 'transform)))
  svg)

(defun edraw-editor-remove-ui-element-from-svg (svg)
  ;;@todo remove :-edraw attributes
  (let ((svg (copy-tree svg)))
    (when-let ((body (edraw-dom-get-by-id svg edraw-editor-svg-body-id)))
      (edraw-dom-remove-attr body 'transform))

    (dolist (elem (dom-by-id svg "^edraw-ui-"))
      (dom-remove-node svg elem))
    svg))

;;;;; Editor - Grid

(cl-defmethod edraw-update-grid ((editor edraw-editor))
  (with-slots (svg) editor
    (when-let ((g (edraw-dom-get-by-id svg "edraw-ui-grid")))
      (edraw-dom-remove-all-children g)
      (when (edraw-get-setting editor 'grid-visible)
        (let ((x-min 0)
              (y-min 0)
              (x-max (edraw-width editor))
              (y-max (edraw-height editor))
              (interval (edraw-get-setting editor 'grid-interval)))
          (cl-loop for x from x-min to x-max by interval
                   do (svg-line g x y-min x y-max :class "edraw-ui-grid-line"))
          (cl-loop for y from y-min to y-max by interval
                   do (svg-line g x-min y x-max y :class "edraw-ui-grid-line")))
        ;;(edraw-dom-invalidate g)
      ))))

(cl-defmethod edraw-set-grid-visible ((editor edraw-editor) visible)
  (edraw-set-setting editor 'grid-visible visible)
  (edraw-update-grid editor)
  (edraw-invalidate-image editor))

(cl-defmethod edraw-get-grid-visible ((editor edraw-editor))
  (edraw-get-setting editor 'grid-visible))

(edraw-editor-defcmd edraw-toggle-grid-visible)
(cl-defmethod edraw-toggle-grid-visible ((editor edraw-editor))
  (edraw-set-grid-visible editor
                          (not (edraw-get-grid-visible editor))))

(defun edraw-editor-set-grid-interval (&optional editor)
  (interactive)
  (when-let ((editor (or editor (edraw-editor-at-input last-input-event))))
    (edraw-set-setting
     editor 'grid-interval
     (read-number (edraw-msg "Grid Interval: ")
                  (edraw-get-setting editor 'grid-interval)))
    (edraw-update-grid editor)
    (edraw-invalidate-image editor)))

;;;;; Editor - Transparent Background

(cl-defmethod edraw-initialize-transparent-bg ((editor edraw-editor))
  (dom-append-child
   (edraw-ui-defs-svg editor)
   (edraw-svg-ui-transparent-bg-pattern)))

(cl-defmethod edraw-update-transparent-bg ((editor edraw-editor))
  (let ((back-ui (edraw-ui-background-svg editor)))
    (edraw-dom-remove-by-id back-ui "edraw-ui-transparent-bg")
    (when (edraw-get-transparent-bg-visible editor)
      (edraw-dom-insert-first
       back-ui
       (edraw-svg-ui-transparent-bg (edraw-width editor)
                                    (edraw-height editor))))))

(cl-defmethod edraw-get-transparent-bg-visible ((editor edraw-editor))
  (edraw-get-setting editor 'transparent-bg-visible))

(cl-defmethod edraw-set-transparent-bg-visible ((editor edraw-editor) visible)
  (edraw-set-setting editor 'transparent-bg-visible visible)
  (edraw-update-transparent-bg editor)
  (edraw-invalidate-image editor)
  visible)

(edraw-editor-defcmd edraw-toggle-transparent-bg-visible)
(cl-defmethod edraw-toggle-transparent-bg-visible ((editor edraw-editor))
  (edraw-set-transparent-bg-visible
   editor
   (not (edraw-get-transparent-bg-visible editor))))

(defun edraw-svg-ui-transparent-bg (width height)
  "Create a svg element of transparent background."
  (dom-node 'g '((id . "edraw-ui-transparent-bg"))
            (dom-node 'rect
                      `((x . 0) (y . 0) (width . ,width) (height . ,height)
                        (fill . ,edraw-editor-transparent-bg-color1)
                        (stroke . "none")))
            (dom-node 'rect
                      `((x . 0) (y . 0) (width . ,width) (height . ,height)
                        (fill . "url(#edraw-ui-pattern-transparent-bg)")
                        (stroke . "none")))))

(defun edraw-svg-ui-transparent-bg-pattern ()
  "Create a svg pattern of transparent background."
  (dom-node 'pattern
            `((id . "edraw-ui-pattern-transparent-bg")
              (x . 0)
              (y . 0)
              (width . ,(* 2 edraw-editor-transparent-bg-grid-size))
              (height . ,(* 2 edraw-editor-transparent-bg-grid-size))
              (patternUnits . "userSpaceOnUse"))
            (dom-node 'rect
                      `((x . ,edraw-editor-transparent-bg-grid-size)
                        (y . 0)
                        (width . ,edraw-editor-transparent-bg-grid-size)
                        (height . ,edraw-editor-transparent-bg-grid-size)
                        (fill . ,edraw-editor-transparent-bg-color2)
                        (stroke . "none")))
            (dom-node 'rect
                      `((x . 0)
                        (y . ,edraw-editor-transparent-bg-grid-size)
                        (width . ,edraw-editor-transparent-bg-grid-size)
                        (height . ,edraw-editor-transparent-bg-grid-size)
                        (fill . ,edraw-editor-transparent-bg-color2)
                        (stroke . "none")))))

;;;;; Editor - Undo

(defvar edraw-editor-inhibit-make-undo-data nil)
(defvar edraw-editor-undo-in-progress nil)
(defvar edraw-editor-redo-in-progress nil)
(defvar edraw-editor-undo-group-level 0)

(defun edraw-editor-clear-undo-vars ()
  (setq edraw-editor-inhibit-make-undo-data nil
        edraw-editor-undo-in-progress nil
        edraw-editor-redo-in-progress nil
        edraw-editor-undo-group-level 0))

(defconst edraw-editor-undo-limit 30)

(cl-defmethod edraw-clear-undo-list ((editor edraw-editor))
  "Discard all undo/redo data."
  (with-slots (undo-list redo-list) editor
    (setq undo-list nil)
    (setq redo-list nil)))

(cl-defmethod edraw-undo-list ((editor edraw-editor))
  "Return a undo data list of the EDITOR.

The top of the list is the newest data.

The format of each data is (TYPE FUNCTION ARGUMENTS...)."
  (oref editor undo-list))

(cl-defmethod edraw-last-undo-data ((editor edraw-editor))
  "Return a recently pushed undo data."
  (with-slots (undo-list) editor
    (car undo-list)))

(cl-defmethod edraw-empty-undo-p ((editor edraw-editor))
  "Return t if there is no undo data in the EDITOR."
  (with-slots (undo-list) editor
    (null undo-list)))

(cl-defmethod edraw-empty-redo-p ((editor edraw-editor))
  "Return t if there is no redo data in the EDITOR."
  (with-slots (redo-list) editor
    (null redo-list)))

(defmacro edraw-push-undo (editor type data)
  `(unless edraw-editor-inhibit-make-undo-data
     (edraw-push-undo-internal ,editor ,type ,data)))

(cl-defmethod edraw-push-undo-internal ((editor edraw-editor) type data)
  "Add a undo data to the EDITOR.

TYPE is a identifier of the undo data.

DATA is a list in the form (FUNCTION ARGUMENTS...).

Undo is performed by applying FUNCTION to ARGUMENTS.
(apply FUNCTION ARGUMENTS)

To call multiple functions at once, specify #'edraw-call-each-args as FUNCTION.

This function deletes all redo data."
  (with-slots (undo-list redo-list) editor
    (unless edraw-editor-redo-in-progress
      (setq redo-list nil))
    (push (cons type data) undo-list)
    (when (= edraw-editor-undo-group-level 0)
      (when-let ((cell (nthcdr edraw-editor-undo-limit undo-list)))
        (setcdr cell nil)))))

(defmacro edraw-make-undo-group (editor type &rest body)
  "Combine undo data pushed in BODY into one."
  (declare (indent 2))
  (let ((var-old-undo-list (gensym))
        (var-editor (gensym))
        (var-data (gensym)))
    `(let* ((,var-editor ,editor)
            (,var-old-undo-list (oref ,var-editor undo-list))
            (,var-data nil)
            (edraw-editor-undo-group-level (1+ edraw-editor-undo-group-level)))
       (oset ,var-editor undo-list nil)
       (unwind-protect
           (prog1
               (progn ,@body)
             (setq ,var-data (edraw-combine-undo-list
                              (oref ,var-editor undo-list))))
         (oset ,var-editor undo-list ,var-old-undo-list)
         (when ,var-data
           (edraw-push-undo-internal ,var-editor ,type ,var-data))))))

(defun edraw-combine-undo-list (undo-list)
  "Make multiple undo data single."
  (cond
   ;; multiple data
   ((cdr undo-list)
    (cons
     #'edraw-call-each-args
     ;;strip type
     (mapcar #'cdr undo-list)))
   ;; single data
   (undo-list
    (cdr (car undo-list)));; strip type
   ;; no data
   (t nil)))

(defun edraw-call-each-args (&rest args)
  (dolist (arg args)
    (apply (car arg) (cdr arg))))


(edraw-editor-defcmd edraw-undo)
(cl-defmethod edraw-undo ((editor edraw-editor))
  "Pop and execute the undo data at the top of the EDITOR's undo-list.

During execution, the variable edraw-editor-undo-in-progress is set to t.

The undo data generated during undo is saved in redo-list."
  (if (edraw-empty-undo-p editor)
      (message (edraw-msg "No undo data"))
    (let ((edraw-editor-undo-in-progress t))
      (with-slots (undo-list redo-list) editor
        (let ((data (car undo-list))
              (old-undo-list (cdr undo-list)))
          (setq undo-list redo-list)
          (unwind-protect
              (edraw-make-undo-group editor 'undo
                (edraw-call-undo-data data))
            (setq redo-list undo-list)
            (setq undo-list old-undo-list)))))))

(edraw-editor-defcmd edraw-redo)
(cl-defmethod edraw-redo ((editor edraw-editor))
  (if (edraw-empty-redo-p editor)
      (message (edraw-msg "No redo data"))
    (let ((edraw-editor-redo-in-progress t))
      (with-slots (redo-list) editor
        (edraw-make-undo-group editor 'redo
          (edraw-call-undo-data (pop redo-list)))))))

(defun edraw-call-undo-data (type-data)
  (apply (car (cdr type-data))
         (cdr (cdr type-data))))

(defmacro edraw-with-push-undo (editor type data &rest body)
  (declare (indent 3))
  (let ((type-var (gensym))
        (data-var (gensym)))
    `(let ((,type-var ,type)
           (,data-var (unless edraw-editor-inhibit-make-undo-data ,data)))
       ,@body
       (when ,data-var
         (edraw-push-undo-internal ,editor ,type-var ,data-var)))))

;;;;; Editor - Document

;; SVG

(cl-defmethod edraw-initialize-svg-document ((editor edraw-editor))
  (with-slots (svg svg-document-size defrefs) editor
    (when (null svg)
      (setq svg (edraw-create-document-svg)))

    ;; Backup SVG Size
    (setq svg-document-size (cons (edraw-svg-attr-length svg 'width)
                                  (edraw-svg-attr-length svg 'height)))

    ;; #edraw-defs
    (if-let ((defs-element (edraw-dom-get-by-id svg edraw-editor-svg-defs-id)))
        (setq defrefs (edraw-svg-defrefs-from-dom
                       defs-element (edraw-dom-get-by-id svg edraw-editor-svg-body-id)))
      (setq defrefs (edraw-svg-defs-as-defrefs edraw-editor-svg-defs-id))
      (edraw-dom-insert-first svg
                              (edraw-svg-defrefs-defs defrefs)))

    ;; #edraw-body
    (edraw-dom-get-or-create svg 'g edraw-editor-svg-body-id)))

(defun edraw-create-document-svg ()
  (let* ((width (alist-get 'width edraw-default-document-properties))
         (height (alist-get 'height edraw-default-document-properties))
         (background (alist-get 'background edraw-default-document-properties))
         (svg (svg-create width height)))
    (when (and background (not (equal background "none")))
      (svg-rectangle svg 0 0 width height
                     :id edraw-editor-svg-background-id
                     :stroke "none"
                     :fill background))
    svg))

(cl-defmethod edraw-svg-body ((editor edraw-editor))
  (edraw-dom-get-by-id (oref editor svg) edraw-editor-svg-body-id))

;; Modification

(defvar edraw-editor-keep-modified-flag nil)

(defun edraw-editor-clear-modified-vars ()
  (setq edraw-editor-keep-modified-flag nil))

(cl-defmethod edraw-on-document-changed ((editor edraw-editor) type)
  (unless edraw-editor-keep-modified-flag
    (edraw-set-modified-p editor t))
  ;;@todo record undo/redo information
  (edraw-invalidate-image editor)
  (edraw-call-hook editor 'change type))

(cl-defmethod edraw-set-modified-p ((editor edraw-editor) flag)
  (with-slots (modified-p) editor
    (setq modified-p flag)))

(cl-defmethod edraw-modified-p ((editor edraw-editor))
  (with-slots (modified-p) editor
    modified-p))

;; Write

(edraw-editor-defcmd edraw-save)
(cl-defmethod edraw-save ((editor edraw-editor))
  (with-slots (document-writer svg svg-document-size) editor
    (when (and document-writer
               (edraw-modified-p editor))
      (let ((doc-svg (edraw-document-svg editor)))
        (prog1
            ;;signal error if failed
            (funcall document-writer doc-svg)
          (edraw-set-modified-p editor nil))))))

(cl-defmethod edraw-document-svg ((editor edraw-editor))
  (with-slots (svg svg-document-size) editor
    (let ((doc-svg (edraw-editor-remove-ui-element-from-svg svg)))
      (edraw-editor-remove-root-transform doc-svg svg-document-size)
      ;; Add xmlns
      (dom-set-attribute doc-svg 'xmlns "http://www.w3.org/2000/svg")
      ;;(dom-set-attribute doc-svg 'xmlns:xlink "http://www.w3.org/1999/xlink")
      ;; Remove empty defs
      (when-let ((defs (edraw-dom-get-by-id doc-svg edraw-editor-svg-defs-id)))
        (when (null (dom-children defs))
          (dom-remove-node doc-svg defs)))
      doc-svg)))

(edraw-editor-defcmd edraw-export-to-buffer)
(cl-defmethod edraw-export-to-buffer ((editor edraw-editor))
  (pop-to-buffer "*Easy Draw SVG*")
  (erase-buffer)
  (edraw-svg-print
   (edraw-document-svg editor)
   nil
   'edraw-svg-print-attr-filter 0)
  (xml-mode))

(edraw-editor-defcmd edraw-export-to-file)
(cl-defmethod edraw-export-to-file ((editor edraw-editor) &optional filename)
  (unless filename
    (setq filename
          (read-file-name (edraw-msg "Write edraw file: ") default-directory)))
  (when (and (stringp filename)
             (not (string-empty-p filename)))
    (when (directory-name-p filename)
      (error "%s is a directory" filename))
    (when (and (file-exists-p filename)
               (not (y-or-n-p (format-message
                               (edraw-msg "File `%s' exists; overwrite? ")
                               filename ))))
      (user-error "Canceled"))
    (with-temp-file filename
      (set-buffer-file-coding-system 'utf-8)
      (edraw-svg-print
       (edraw-document-svg editor)
       nil
       'edraw-svg-print-attr-filter 0))))

(edraw-editor-defcmd edraw-export-debug-svg-to-buffer)
(cl-defmethod edraw-export-debug-svg-to-buffer ((editor edraw-editor))
  (pop-to-buffer "*Easy Draw SVG*")
  (erase-buffer)
  (edraw-svg-print
   (oref editor svg)
   nil
   'edraw-svg-print-attr-filter 0)
  (xml-mode))


;; Clear

(defun edraw-editor-clear (&optional editor)
  (interactive)
  (when-let ((editor (or editor (edraw-editor-at-input last-input-event))))
    (when (edraw-y-or-n-p
           (edraw-msg "Do you want to close the current document?"))
      (edraw-clear editor))))

(cl-defmethod edraw-clear ((editor edraw-editor))
  (edraw-deselect-all-shapes editor)
  (edraw-select-tool editor nil)
  (edraw-notify-document-close-to-all-shapes editor)
  (edraw-clear-undo-list editor)

  (with-slots (svg) editor
    (setq svg nil))

  (edraw-initialize-svg editor)
  (edraw-on-document-changed editor 'document-initialized)
  ;;(edraw-set-modified-p editor nil) ?

  (edraw-select-tool editor (edraw-editor-make-tool 'rect)))

;;;;; Editor - Document - Size

(cl-defmethod edraw-width ((editor edraw-editor))
  (car (oref editor svg-document-size)))

(cl-defmethod edraw-height ((editor edraw-editor))
  (cdr (oref editor svg-document-size)))

(cl-defmethod edraw-set-size ((editor edraw-editor) width height)
  (with-slots (svg-document-size) editor
    (let ((old-width (car svg-document-size))
          (old-height (cdr svg-document-size)))
      (when (or (/= width old-width)
                (/= height old-height))
        (edraw-push-undo
         editor 'document-size
         (list 'edraw-set-size editor old-width old-height))
        (setq svg-document-size (cons width height))
        (edraw-update-background editor)
        (edraw-update-root-transform editor) ;;update <svg width= height=>
        (edraw-update-grid editor)
        (edraw-on-document-changed editor 'document-size)))))

(defun edraw-editor-set-size (&optional editor)
  (interactive)
  (when-let* ((editor (or editor (edraw-editor-at-input last-input-event)))
              (width (read-number
                      (edraw-msg "Document Width: ") (edraw-width editor)))
              (height (read-number
                       (edraw-msg "Document Height: ") (edraw-height editor))))
    (edraw-set-size editor width height)))

;;;;; Editor - Document - Background

(cl-defmethod edraw-svg-background ((editor edraw-editor))
  (edraw-dom-get-by-id (oref editor svg) edraw-editor-svg-background-id))

(cl-defmethod edraw-update-background ((editor edraw-editor))
  (when-let ((element (edraw-svg-background editor)))
    (dom-set-attribute element 'x 0)
    (dom-set-attribute element 'y 0)
    (dom-set-attribute element 'width (edraw-width editor))
    (dom-set-attribute element 'height (edraw-height editor))))

(cl-defmethod edraw-get-background ((editor edraw-editor))
  (when-let ((element (edraw-svg-background editor)))
    (dom-attr element 'fill)))

(cl-defmethod edraw-set-background ((editor edraw-editor) fill)
  (edraw-push-undo
   editor
   'document-background
   (list 'edraw-set-background editor (edraw-get-background editor)))
  (with-slots (svg) editor
    (if (or (null fill) (string= fill "none") (string-empty-p fill))
        ;; remove background
        (edraw-dom-remove-by-id svg edraw-editor-svg-background-id)
      (if-let ((element (edraw-svg-background editor)))
          ;; change fill
          (dom-set-attribute element 'fill fill)
        ;; add background
        (dom-add-child-before
         svg
         (dom-node 'rect (list (cons 'fill fill)
                               (cons 'id edraw-editor-svg-background-id)))
         (edraw-svg-body editor))
        (edraw-update-background editor)
        (edraw-update-root-transform editor) ;;update <rect transform=>
        ))
    (edraw-on-document-changed editor 'document-background)))

(defun edraw-editor-set-background (&optional editor)
  (interactive)
  (when-let ((editor (or editor (edraw-editor-at-input last-input-event))))
    (let* ((current-value (edraw-get-background editor))
           (new-value
            (let ((edraw-editor-inhibit-make-undo-data t)
                  (edraw-editor-keep-modified-flag t))
              (unwind-protect
                  (edraw-color-picker-read-color
                   (edraw-msg "Background Color: ")
                   (or current-value "")
                   '("" "none")
                   `((:color-name-scheme . web)
                     (:on-input-change
                      . ,(lambda (string color)
                           (when (or (member string '("" "none"))
                                     color)
                             ;;@todo suppress notification?
                             (edraw-set-background editor string))))))
                (edraw-set-background editor current-value)))))
      (edraw-set-background editor new-value))))

;;;;; Editor - Document - Shapes

(cl-defmethod edraw-all-shapes ((editor edraw-editor))
  ;; back-to-front
  (delq nil (mapcar (lambda (node)
                      (edraw-shape-from-element node editor 'noerror))
                    (dom-children (edraw-svg-body editor)))))

(edraw-editor-defcmd edraw-translate-all-shapes)
(cl-defmethod edraw-translate-all-shapes ((editor edraw-editor) &optional dx dy)
  (unless dx (setq dx (read-number "Delta X: " 0)))
  (unless dy (setq dy (read-number "Delta Y: " 0)))

  (edraw-make-undo-group editor 'all-shapes-translate
    (let ((delta-xy (edraw-xy dx dy)))
      (dolist (shape (edraw-all-shapes editor))
        (edraw-translate shape delta-xy)))))

(edraw-editor-defcmd edraw-scale-all-shapes)
(cl-defmethod edraw-scale-all-shapes ((editor edraw-editor) &optional sx sy)
  (unless sx (setq sx (read-number "Scale X: " 1.0)))
  (unless sy (setq sy (read-number "Scale Y: " sx)))

  (edraw-make-undo-group editor 'all-shapes-scale
    (let ((matrix (edraw-matrix-scale (float sx) (float sy) 1.0)))
      (dolist (shape (edraw-all-shapes editor))
        (edraw-transform shape matrix)))))

(cl-defmethod edraw-notify-document-close-to-all-shapes ((editor edraw-editor))
  (dolist (node (dom-children (edraw-svg-body editor)))
    (when-let ((shape (edraw-shape-from-element-no-create node)))
      (edraw-notify-change-hook shape 'document-close))))

;; Shape Finding

(cl-defmethod edraw-find-shapes-by-xy ((editor edraw-editor) xy)
  (nreverse ;;front to back
   (delq nil
         (cl-loop for node in (dom-children (edraw-svg-body editor))
                  when (edraw-svg-element-contains-point-p node xy)
                  collect (edraw-shape-from-element node editor 'noerror)))))

(cl-defmethod edraw-find-shape-by-xy-and-menu ((editor edraw-editor)
                                                      xy)
  (let ((shapes (edraw-find-shapes-by-xy editor xy)))
    (if (cdr shapes)
        (edraw-popup-shape-selection-menu shapes)
      (car shapes))))

(cl-defmethod edraw-find-shapes-by-rect ((editor edraw-editor) rect)
  (nreverse ;;front to back
   (delq nil
         (cl-loop for node in (dom-children (edraw-svg-body editor))
                  when (edraw-svg-element-intersects-rect-p node rect)
                  collect (edraw-shape-from-element node editor 'noerror)))))

;;;;; Editor - Document - Menu

(cl-defmethod edraw-get-actions-for-document ((editor edraw-editor))
  (edraw-filter-menu
   editor
   'context-document
   `(((edraw-msg "Document")
      (((edraw-msg "Set Background...") edraw-editor-set-background)
       ((edraw-msg "Resize...") edraw-editor-set-size)
       ((edraw-msg "Translate All...") edraw-editor-translate-all-shapes)
       ((edraw-msg "Scale All...") edraw-editor-scale-all-shapes)
       ((edraw-msg "Clear...") edraw-editor-clear)
       ((edraw-msg "Export to Buffer") edraw-editor-export-to-buffer)
       ((edraw-msg "Export to File") edraw-editor-export-to-file)))
     ((edraw-msg "Undo") edraw-editor-undo
      :enable ,(not (edraw-empty-undo-p editor)))
     ((edraw-msg "Redo") edraw-editor-redo
      :enable ,(not (edraw-empty-redo-p editor)))
     ((edraw-msg "Paste") edraw-editor-paste-and-select
      :enable ,(not (edraw-clipboard-empty-p))))))

(cl-defmethod edraw-popup-context-menu-for-document ((editor edraw-editor))
  (edraw-popup-menu
   (edraw-msg "Document")
   (edraw-get-actions-for-document editor)
   editor))

;;;;; Editor - Selection

(cl-defmethod edraw-last-selected-shape ((editor edraw-editor))
  (car (last (oref editor selected-shapes))))

(cl-defmethod edraw-selected-shapes ((editor edraw-editor))
  (oref editor selected-shapes))

(cl-defmethod edraw-selected-shapes-back-to-front ((editor edraw-editor))
  (seq-filter
   #'edraw-selected-p
   (edraw-all-shapes editor)))

(cl-defmethod edraw-selected-shapes-front-to-back ((editor edraw-editor))
  (nreverse (edraw-selected-shapes-back-to-front editor)))

(cl-defmethod edraw-selected-shape-p ((editor edraw-editor) shape)
  (with-slots (selected-shapes) editor
    (not (null (memq shape selected-shapes)))))

(cl-defmethod edraw-selected-anchor ((editor edraw-editor))
  (oref editor selected-anchor))

(cl-defmethod edraw-selected-handle ((editor edraw-editor))
  (oref editor selected-handle))

(cl-defmethod edraw-add-shape-selection ((editor edraw-editor) shape)
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (when (and shape
               (not (memq shape selected-shapes)))
      (edraw-add-change-hook shape
                             'edraw-on-selected-shape-changed editor)
      (setq selected-shapes (append selected-shapes (list shape)))
      (edraw-update-selection-ui editor)

      (when (and (edraw-property-editor-tracking-selected-shape-p)
                 shape ;;last selected shape
                 (edraw-property-editor-buffer))
        (edraw-edit-properties shape));;@todo create proxy shape and pass it

      (edraw-call-hook editor 'selection-change))))

(cl-defmethod edraw-remove-shape-selection ((editor edraw-editor) shape)
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (when (and shape
               (memq shape selected-shapes))
      (when (and selected-handle
                 (eq (edraw-parent-shape selected-handle) shape))
        (setq selected-handle nil))
      (when (and selected-anchor
                 (eq (edraw-parent-shape selected-anchor) shape))
        (setq selected-anchor nil))
      (setq selected-shapes (delq shape selected-shapes))
      (edraw-remove-change-hook shape
                                'edraw-on-selected-shape-changed editor)
      (edraw-update-selection-ui editor)
      (edraw-call-hook editor 'selection-change))))

(cl-defmethod edraw-select-shape ((editor edraw-editor) shape)
  (with-slots (selected-shapes) editor
    (unless (and (eq (car selected-shapes) shape)
                 (null (cdr selected-shapes)))
      (edraw-deselect-all-shapes editor)
      (edraw-add-shape-selection editor shape))))

(cl-defmethod edraw-deselect-all-shapes ((editor edraw-editor))
  (with-slots (selected-shapes) editor
    (while selected-shapes
      (edraw-remove-shape-selection editor (car selected-shapes)))))

(cl-defmethod edraw-on-selected-shape-changed ((editor edraw-editor)
                                               shape type)
  ;;(message "changed!! %s" type)

  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (cond
     ((eq type 'shape-remove)
      (edraw-remove-shape-selection editor shape))

     ((eq type 'shape-path-data)
      (edraw-deselect-anchor editor) ;; All anchors have been destroyed
      (edraw-update-selection-ui editor))

     ((and selected-anchor
           (memq type '(point-remove))
           ;; No shape in selected-shapes owns selected-anchor
           (not (edraw-point-in-selected-shapes-p editor selected-anchor)))
      (edraw-deselect-anchor editor))

     ((and selected-handle
           (memq type '(point-remove anchor-make-corner))
           ;; No shape in selected-shapes owns selected-handle
           (not (edraw-point-in-selected-shapes-p editor selected-handle)))
      (edraw-deselect-handle editor))

     (t
      (edraw-update-selection-ui editor)))))

(cl-defmethod edraw-point-in-selected-shapes-p ((editor edraw-editor) point)
  (with-slots (selected-shapes) editor
    (seq-find (lambda (shp)
                (edraw-owned-shape-point-p shp point))
              selected-shapes)))

(cl-defmethod edraw-select-anchor ((editor edraw-editor) anchor)
  (with-slots (selected-anchor selected-handle) editor
    (when (and anchor
               (edraw-point-in-selected-shapes-p editor anchor))
      (setq selected-anchor anchor)
      (setq selected-handle nil)
      (edraw-update-selection-ui editor))))

(cl-defmethod edraw-deselect-anchor ((editor edraw-editor))
  (with-slots (selected-anchor selected-handle) editor
    (when selected-anchor
      (setq selected-handle nil)
      (setq selected-anchor nil)
      (edraw-update-selection-ui editor))))

(cl-defmethod edraw-select-handle ((editor edraw-editor) handle)
  (with-slots (selected-anchor selected-handle) editor
    (when (and handle
               (edraw-point-in-selected-shapes-p editor handle))

      (setq selected-anchor (edraw-parent-anchor handle))
      (setq selected-handle handle)
      (edraw-update-selection-ui editor))))

(cl-defmethod edraw-deselect-handle ((editor edraw-editor))
  (with-slots (selected-anchor selected-handle) editor
    (when selected-handle
      (setq selected-handle nil)
      (edraw-update-selection-ui editor))))

(cl-defmethod edraw-update-selection-ui ((editor edraw-editor))
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (if selected-shapes
        ;; Show points
        (let ((g (edraw-svg-ui-shape-points-create-group
                  (edraw-ui-foreground-svg editor))))
          (dolist (shape selected-shapes)
            (edraw-svg-ui-shape-points g
                                       shape selected-anchor selected-handle)))
      ;; Hide points
      (edraw-svg-ui-shape-points-remove-group
       (edraw-ui-foreground-svg editor))))
  (edraw-invalidate-image editor))

(defun edraw-svg-ui-shape-points-remove-group (parent)
  (edraw-dom-remove-by-id parent "edraw-ui-shape-points"))

(defun edraw-svg-ui-shape-points-create-group (parent)
  (if-let ((g (edraw-dom-get-by-id parent "edraw-ui-shape-points")))
      (progn
        (edraw-dom-remove-all-children g)
        g)
    (let ((g (dom-node 'g `((id . "edraw-ui-shape-points")))))
      (dom-append-child parent g)
      g)))

(defun edraw-svg-ui-shape-points (parent
                                  shape
                                  selected-anchor
                                  selected-handle)
  (let ((anchor-points (edraw-get-anchor-points shape))
        (prev-selected-anchor (when selected-anchor
                                (edraw-previous-anchor selected-anchor)))
        (next-selected-anchor (when selected-anchor
                                (edraw-next-anchor selected-anchor))))
    (dolist (anchor anchor-points)
      (let ((anchor-xy (edraw-get-xy-transformed anchor))
            (anchor-selected-p (edraw-same-point-p anchor selected-anchor)))
        (edraw-svg-ui-anchor-point parent anchor-xy anchor-selected-p)

        (when (or anchor-selected-p
                  ;; Include handles of previous and next anchor
                  ;; see: edraw-selectable-handles
                  (edraw-same-point-p anchor prev-selected-anchor)
                  (edraw-same-point-p anchor next-selected-anchor))
          (let ((handle-points (edraw-get-handle-points anchor)))
            (dolist (handle handle-points)
              (edraw-svg-ui-handle-point
               parent
               (edraw-get-xy-transformed handle)
               anchor-xy
               (and selected-handle
                    (edraw-same-point-p handle selected-handle))))))))))

(defun edraw-svg-ui-anchor-point (parent xy &optional selected)
  (let ((r edraw-anchor-point-radius))
    (svg-rectangle parent (- (car xy) r) (- (cdr xy) r) (* 2 r) (* 2 r)
                   :class (if selected
                              "edraw-ui-anchor-point-selected"
                            "edraw-ui-anchor-point"))))

(defun edraw-svg-ui-handle-point (parent handle-xy anchor-xy
                                         &optional selected)
  (svg-line parent
            (car handle-xy) (cdr handle-xy)
            (car anchor-xy) (cdr anchor-xy)
            :class "edraw-ui-handle-line")
  (svg-circle parent
              (car handle-xy) (cdr handle-xy)
              edraw-handle-point-radius
              :class (if selected
                         "edraw-ui-handle-point-selected"
                       "edraw-ui-handle-point")))

(cl-defmethod edraw-selectable-handles ((editor edraw-editor))
  (with-slots (selected-anchor) editor
    (when selected-anchor
      (let ((prev-anchor (edraw-previous-anchor selected-anchor))
            (next-anchor (edraw-next-anchor selected-anchor)))
        (append
         ;; Include handles of previous and next anchor
         ;; see: svg-ui-shape-points
         (edraw-get-handle-points selected-anchor)
         (when prev-anchor (edraw-get-handle-points prev-anchor))
         (when next-anchor (edraw-get-handle-points next-anchor)))))))



;;;;; Editor - Manipulate Selected Shapes

(cl-defmethod edraw-get-actions-for-selected-shapes ((editor edraw-editor))
  (let ((selected-shapes (edraw-selected-shapes editor)))
    `(((edraw-msg "Delete...") edraw-editor-delete-selected)
      ((edraw-msg "Copy") edraw-editor-copy-selected-shapes)
      ((edraw-msg "Cut") edraw-editor-cut-selected-shapes)
      ((edraw-msg "Group") edraw-editor-group-selected-shapes)
      ((edraw-msg "Z-Order")
       ;;@todo check :enable when multiple shapes are selected
       (((edraw-msg "Bring to Front") edraw-editor-bring-selected-to-front
         :enable ,(not (null (and selected-shapes (or (cdr selected-shapes) (not (edraw-front-p (car selected-shapes))))))))
        ((edraw-msg "Bring Forward") edraw-editor-bring-selected-forward
         :enable ,(not (null (and selected-shapes (or (cdr selected-shapes) (not (edraw-front-p (car selected-shapes))))))))
        ((edraw-msg "Send Backward") edraw-editor-send-selected-backward
         :enable ,(not (null (and selected-shapes (or (cdr selected-shapes) (not (edraw-back-p (car selected-shapes))))))))
        ((edraw-msg "Send to Back") edraw-editor-send-selected-to-back
         :enable ,(not (null (and selected-shapes (or (cdr selected-shapes) (not (edraw-back-p (car selected-shapes)))))))))))))

(cl-defmethod edraw-get-summary-for-selected-shapes ((editor edraw-editor))
  (let ((selected-shapes (edraw-selected-shapes editor)))
    (format (edraw-msg "%s Selected Shapes") (length selected-shapes))))

(cl-defmethod edraw-popup-context-menu-for-selected-shapes ((editor edraw-editor))
  (edraw-popup-menu
   (edraw-get-summary-for-selected-shapes editor)
   (edraw-get-actions-for-selected-shapes editor)
   editor))

(defun edraw-editor-move-selected-by-arrow-key (&optional editor n)
  (interactive "i\np")
  (let ((event last-input-event))
    (when-let ((editor (or editor (edraw-editor-at-input event))))
      (let* ((mods (event-modifiers event))
             (d (* (or n 1)
                   (cond
                    ((memq 'meta mods) (read-number (edraw-msg "Moving Distance: ") 20))
                    ((memq 'shift mods) 10)
                    (t 1))))
             (v (pcase (event-basic-type event)
                  ('left (cons (- d) 0))
                  ('right (cons d 0))
                  ('up (cons 0 (- d)))
                  ('down (cons 0 d))
                  (_ (cons 0 0)))))
        (edraw-translate-selected editor v)))))

(cl-defmethod edraw-translate-selected ((editor edraw-editor) xy)
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (cond
     (selected-handle
      (edraw-move-on-transformed selected-handle
                  (edraw-xy-add (edraw-get-xy-transformed selected-handle) xy)))
     (selected-anchor
      (edraw-move-on-transformed selected-anchor
                  (edraw-xy-add (edraw-get-xy-transformed selected-anchor) xy)))
     (selected-shapes
      (edraw-make-undo-group editor 'selected-shapes-translate
        (dolist (shape selected-shapes)
          (edraw-translate shape xy)))))))

(edraw-editor-defcmd edraw-delete-selected)
(cl-defmethod edraw-delete-selected ((editor edraw-editor))
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (cond
     (selected-handle
      (edraw-delete-point selected-handle))
     (selected-anchor
      (edraw-delete-point selected-anchor))
     (selected-shapes
      (edraw-make-undo-group editor 'selected-shapes-delete
        (dolist (shape selected-shapes)
          (edraw-remove shape)))))))

(edraw-editor-defcmd edraw-bring-selected-to-front)
(cl-defmethod edraw-bring-selected-to-front ((editor edraw-editor))
  (when (edraw-selected-shapes editor)
    (edraw-make-undo-group editor 'selected-shapes-bring-to-front
      (dolist (shape (edraw-selected-shapes-back-to-front editor))
        (edraw-bring-to-front shape)))))

(edraw-editor-defcmd edraw-bring-selected-forward)
(cl-defmethod edraw-bring-selected-forward ((editor edraw-editor))
  (when (edraw-selected-shapes editor)
    (edraw-make-undo-group editor 'selected-shapes-bring-forward
      (dolist (shape (edraw-selected-shapes-front-to-back editor))
        (when-let ((next (edraw-next-sibling shape)))
          (unless (edraw-selected-p next) ;; No overtaking
            (edraw-bring-forward shape)))))))

(edraw-editor-defcmd edraw-send-selected-backward)
(cl-defmethod edraw-send-selected-backward ((editor edraw-editor))
  (when (edraw-selected-shapes editor)
    (edraw-make-undo-group editor 'selected-shapes-send-backward
      (dolist (shape (edraw-selected-shapes-back-to-front editor))
        (when-let ((prev (edraw-previous-sibling shape)))
          (unless (edraw-selected-p prev) ;; No overtaking
            (edraw-send-backward shape)))))))

(edraw-editor-defcmd edraw-send-selected-to-back)
(cl-defmethod edraw-send-selected-to-back ((editor edraw-editor))
  (when (edraw-selected-shapes editor)
    (edraw-make-undo-group editor 'selected-shapes-send-to-back
      (dolist (shape (edraw-selected-shapes-front-to-back editor))
        (edraw-send-to-back shape)))))

(edraw-editor-defcmd edraw-select-next-shape)
(cl-defmethod edraw-select-next-shape ((editor edraw-editor))
  (when (edraw-selected-shapes editor)
    (when-let ((front (car (edraw-selected-shapes-front-to-back editor)))
               (next (edraw-next-sibling front)))
      (edraw-select-shape editor next))))

(edraw-editor-defcmd edraw-select-previous-shape)
(cl-defmethod edraw-select-previous-shape ((editor edraw-editor))
  (when (edraw-selected-shapes editor)
    (when-let ((back (car (edraw-selected-shapes-back-to-front editor)))
               (prev (edraw-previous-sibling back)))
      (edraw-select-shape editor prev))))

(edraw-editor-defcmd edraw-group-selected-shapes)
(cl-defmethod edraw-group-selected-shapes ((editor edraw-editor))
  (with-slots (selected-shapes) editor
    (edraw-make-undo-group editor 'create-group
      (let ((group (edraw-create-shape editor (edraw-svg-body editor) 'g)))
        (edraw-shape-group-add-children group selected-shapes)))))

;;;;; Editor - Copy & Paste

(edraw-editor-defcmd edraw-paste)
(cl-defmethod edraw-paste ((editor edraw-editor))
  (when (eq (edraw-clipboard-type) 'shape-descriptor-list)
    (edraw-make-undo-group editor 'paste
      (mapcar
       (lambda (shape-descriptor)
         (edraw-shape-from-shape-descriptor
          editor (edraw-svg-body editor) shape-descriptor))
       (edraw-clipboard-data)))))

(edraw-editor-defcmd edraw-paste-and-select)
(cl-defmethod edraw-paste-and-select ((editor edraw-editor))
  (when-let ((shapes (edraw-paste editor)))
    (edraw-deselect-all-shapes editor)
    (dolist (shape shapes)
      (edraw-add-shape-selection editor shape))))

(edraw-editor-defcmd edraw-copy-selected-shapes)
(cl-defmethod edraw-copy-selected-shapes ((editor edraw-editor))
  (when-let ((selected-shapes (edraw-selected-shapes editor)))
    (edraw-clipboard-set
     'shape-descriptor-list
     (mapcar #'edraw-shape-descriptor selected-shapes))))

(edraw-editor-defcmd edraw-cut-selected-shapes)
(cl-defmethod edraw-cut-selected-shapes ((editor edraw-editor))
  (when-let ((selected-shapes (copy-sequence (edraw-selected-shapes editor))))
    ;; Copy
    (edraw-clipboard-set
     'shape-descriptor-list
     (mapcar #'edraw-shape-descriptor selected-shapes))
    ;; Deselect
    (edraw-deselect-all-shapes editor)
    ;; Remove
    (edraw-make-undo-group editor 'cut
      (dolist (shape selected-shapes)
        (edraw-remove shape)))))

;;;;; Editor - Default Shape Properties

(cl-defmethod edraw-get-default-shape-properties-by-tag ((editor edraw-editor)
                                                         tag)
  (with-slots (default-shape-properties) editor
    (or (assq tag default-shape-properties) ;;already exists
        (let ((tag-props (list tag))) ;;create new
          (push tag-props default-shape-properties)
          tag-props))))

(cl-defmethod edraw-get-default-shape-property ((editor edraw-editor)
                                                tag prop-name)
  (alist-get prop-name
             (alist-get tag (oref editor default-shape-properties))))

(cl-defmethod edraw-set-default-shape-property ((editor edraw-editor)
                                                tag prop-name value)
  (setf
   (alist-get prop-name
              (alist-get tag (oref editor default-shape-properties)))
   value))

(cl-defmethod edraw-set-default-shape-properties-from-shape
  ((editor edraw-editor) shape)
  (when-let ((tag (edraw-shape-type shape)))
    (dolist (prop-info (edraw-get-property-info-list shape))
      ;; Skip required property
      (unless (plist-get (cdr prop-info) :required)
        ;; Get property value
        (let* ((prop-name (car prop-info))
               (value (edraw-get-property shape prop-name)))
          ;; Set property value as default
          (edraw-set-default-shape-property
           editor tag prop-name value))))))

(cl-defmethod edraw-edit-default-shape-properties ((editor edraw-editor) tag)
  (with-slots (default-shape-properties) editor
    (when-let ((alist-head (assq tag default-shape-properties)))
      (edraw-property-editor-open
       (edraw-property-proxy-shape :tag tag :alist-head alist-head
                                   :editor editor
                                   :name (format "default %s" tag))))))

(defun edraw-editor-edit-default-shape-props (&optional editor tag)
  (when-let ((editor (or editor (edraw-editor-at-input last-input-event))))
    (edraw-edit-default-shape-properties editor tag)))

(defun edraw-editor-edit-default-rect-props (&optional editor)
  (interactive)
  (edraw-editor-edit-default-shape-props editor 'rect))

(defun edraw-editor-edit-default-ellipse-props (&optional editor)
  (interactive)
  (edraw-editor-edit-default-shape-props editor 'ellipse))

(defun edraw-editor-edit-default-text-props (&optional editor)
  (interactive)
  (edraw-editor-edit-default-shape-props editor 'text))

(defun edraw-editor-edit-default-path-props (&optional editor)
  (interactive)
  (edraw-editor-edit-default-shape-props editor 'path))

;; edraw-property-proxy-shape

(defclass edraw-property-proxy-shape ()
  ((tag :initarg :tag)
   (alist-head :initarg :alist-head) ;;('rect (prop . value) ...)
   (editor :initarg :editor)
   (name :initarg :name)))

(cl-defmethod edraw-name ((shape edraw-property-proxy-shape))
  (oref shape name))

(cl-defmethod edraw-get-property-info-list ((shape edraw-property-proxy-shape))
  (seq-remove
   (lambda (prop-info) (plist-get (cdr prop-info) :required))
   (edraw-svg-element-get-property-info-list-by-tag (oref shape tag))))

(cl-defmethod edraw-get-property ((shape edraw-property-proxy-shape) prop-name)
  (let ((value (alist-get prop-name (cdr (oref shape alist-head)))))
    (if (numberp value)
        (format "%s" value)
      value)))

(cl-defmethod edraw-set-properties ((shape edraw-property-proxy-shape) prop-list)
  (dolist (prop prop-list)
    (let ((prop-name (car prop))
          (value (cdr prop)))
      (if (null value)
          (setf (alist-get prop-name (cdr (oref shape alist-head))
                           nil 'remove)
                nil)
        (setf (alist-get prop-name (cdr (oref shape alist-head)))
              value))))
  ;; update toolbar (fill & stroke)
  (edraw-update-toolbar (oref shape editor)))

(cl-defmethod edraw-set-property ((shape edraw-property-proxy-shape) prop-name value) ;;@todo generalize
  (edraw-set-properties
   shape
   (list (cons prop-name value))))

(cl-defmethod edraw-add-change-hook ((_shape edraw-property-proxy-shape) _function &rest _args)
  )

(cl-defmethod edraw-remove-change-hook ((_shape edraw-property-proxy-shape) _function &rest _args)
  )

;;;;; Editor - Main Menu

(edraw-editor-defcmd edraw-main-menu)
(cl-defmethod edraw-main-menu ((editor edraw-editor))
  "Show the main menu of EDITOR."
  (let ((selected-shapes (edraw-selected-shapes editor)))
    (edraw-popup-menu
     (edraw-msg "Main Menu")
     (edraw-filter-menu
      editor
      'main-menu
      `(((edraw-msg "Document")
         (((edraw-msg "Set Background...") edraw-editor-set-background)
          ((edraw-msg "Resize...") edraw-editor-set-size)
          ((edraw-msg "Translate All...") edraw-editor-translate-all-shapes)
          ((edraw-msg "Scale All...") edraw-editor-scale-all-shapes)
          ((edraw-msg "Clear...") edraw-editor-clear)
          ((edraw-msg "Export to Buffer") edraw-editor-export-to-buffer)
          ((edraw-msg "Export to File") edraw-editor-export-to-file)))
        ((edraw-msg "View")
         (((edraw-msg "Transparent BG") edraw-editor-toggle-transparent-bg-visible
           :button (:toggle . ,(edraw-get-transparent-bg-visible editor)))
          ((edraw-msg "Grid") edraw-editor-toggle-grid-visible
           :button (:toggle . ,(edraw-get-grid-visible editor)))
          ((edraw-msg "Set Grid Interval...") edraw-editor-set-grid-interval)))
        ((edraw-msg "Selected Object")
         (((edraw-msg "Delete") edraw-editor-delete-selected
           :enable ,(not (null (or (edraw-selected-handle editor)
                                   (edraw-selected-anchor editor)
                                   selected-shapes))))
          ((edraw-msg "Copy") edraw-editor-copy-selected-shapes
           :enable ,(not (null selected-shapes)))
          ((edraw-msg "Cut") edraw-editor-cut-selected-shapes
           :enable ,(not (null selected-shapes)))
          ((edraw-msg "Z-Order")
           ;;@todo check :enable when multiple shapes are selected
           (((edraw-msg "Bring to Front") edraw-editor-bring-selected-to-front
             :enable ,(not (null (and selected-shapes (or (cdr selected-shapes) (not (edraw-front-p (car selected-shapes))))))))
            ((edraw-msg "Bring Forward") edraw-editor-bring-selected-forward
             :enable ,(not (null (and selected-shapes (or (cdr selected-shapes) (not (edraw-front-p (car selected-shapes))))))))
            ((edraw-msg "Send Backward") edraw-editor-send-selected-backward
             :enable ,(not (null (and selected-shapes (or (cdr selected-shapes) (not (edraw-back-p (car selected-shapes))))))))
            ((edraw-msg "Send to Back") edraw-editor-send-selected-to-back
             :enable ,(not (null (and selected-shapes (or (cdr selected-shapes) (not (edraw-back-p (car selected-shapes))))))))))
          ((edraw-msg "Select Next Above") edraw-editor-select-next-shape)
          ((edraw-msg "Select Next Below") edraw-editor-select-previous-shape)))
        ((edraw-msg "Shape's Defaults")
         (((edraw-msg "Rect") edraw-editor-edit-default-rect-props)
          ((edraw-msg "Ellipse") edraw-editor-edit-default-ellipse-props)
          ((edraw-msg "Text") edraw-editor-edit-default-text-props)
          ((edraw-msg "Path") edraw-editor-edit-default-path-props)))
        ;;((edraw-msg "Search Object") edraw-editor-search-object)
        ((edraw-msg "Undo") edraw-editor-undo
         :enable ,(not (edraw-empty-undo-p editor)))
        ((edraw-msg "Redo") edraw-editor-redo
         :enable ,(not (edraw-empty-redo-p editor)))
        ((edraw-msg "Paste") edraw-editor-paste-and-select
         :enable ,(not (edraw-clipboard-empty-p)))

        ((edraw-msg "Save") edraw-editor-save
         :visible ,(not (null (oref editor document-writer)))
         :enable ,(edraw-modified-p editor))
        ))
     editor)))

(cl-defmethod edraw-filter-menu ((editor edraw-editor) menu-type items)
  (with-slots (menu-filter) editor
    (if menu-filter
        (funcall menu-filter menu-type items)
      items)))

;;;;; Editor - Mouse Coordinates

(cl-defmethod edraw-mouse-event-to-xy-snapped ((editor edraw-editor) event)
  (edraw-snap-xy editor (edraw-mouse-event-to-xy editor event)))

(cl-defmethod edraw-mouse-event-to-xy ((editor edraw-editor) event)
  (with-slots (image-scale) editor
    (let* ((xy (posn-object-x-y (event-start event)))
           ;;@todo round? or not
           ;; Must be able to point to integer pixel coordinates on
           ;; high DPI environment(image-scale > 1.0).
           ;; But when adding zoom function, float coordinates is required.
           (x (round (/ (car xy) image-scale)))
           (y (round (/ (cdr xy) image-scale))))
      (cons x y))))

(cl-defmethod edraw-snap-xy ((editor edraw-editor) xy)
  (if (edraw-get-setting editor 'grid-visible)
      (let* ((interval (edraw-get-setting editor 'grid-interval))
             (half-interval (/ interval 2))
             (x (car xy))
             (y (cdr xy)))
        (cons
         (- x (- (mod (+ x half-interval) interval) half-interval))
         (- y (- (mod (+ y half-interval) interval) half-interval))))
    xy))

;;;;; Editor - Input Event

(defun edraw-editor-at (&optional pos)
  (let ((pos (or pos (point))))
    (or
     (seq-some (lambda (ov) (overlay-get ov 'edraw-editor)) (overlays-at pos))
     (seq-some (lambda (ov) (overlay-get ov 'edraw-editor)) (overlays-at (1- pos)))
     (seq-some (lambda (ov) (overlay-get ov 'edraw-editor)) (overlays-in (1- pos) (1+ pos))))))

(defun edraw-editor-at-input (event)
  (if (or (mouse-event-p event)
          (memq (event-basic-type event) '(wheel-up wheel-down)))
      (let* ((mouse-pos (event-start event))
             (window (posn-window mouse-pos))
             (buffer (window-buffer window))
             (pos (posn-point mouse-pos)))
        (when edraw-editor-move-point-on-click
          (select-window window)
          (set-window-point window pos))
        (with-current-buffer buffer
          (edraw-editor-at pos)))
    (edraw-editor-at (point))))

(defun edraw-editor-dispatch-event (event)
  "Call the editor's method corresponding to the EVENT.

For example, if the event name is down-mouse-1, call edraw-on-down-mouse-1. Determine the editor object from the position where the EVENT occurred."
  (interactive "e")

  (when-let ((editor (edraw-editor-at-input event)))
    (let* ((event-name (car event))
           (method-name (intern (concat "edraw-on-" (symbol-name event-name)))))
      (when (fboundp method-name)
        (funcall method-name editor event)))))

(cl-defmethod edraw-on-down-mouse-1 ((editor edraw-editor) down-event)
  (with-slots (tool) editor
    (when tool
      (edraw-on-down-mouse-1 tool down-event))))

(cl-defmethod edraw-on-S-down-mouse-1 ((editor edraw-editor) down-event)
  (with-slots (tool) editor
    (when tool
      (edraw-on-S-down-mouse-1 tool down-event))))

(cl-defmethod edraw-on-C-down-mouse-1 ((editor edraw-editor) down-event)
  (with-slots (tool) editor
    (when tool
      (edraw-on-C-down-mouse-1 tool down-event))))

(cl-defmethod edraw-on-mouse-1 ((editor edraw-editor) down-event)
  (with-slots (tool) editor
    (when tool
      (edraw-on-mouse-1 tool down-event))))

(cl-defmethod edraw-on-double-mouse-1 ((editor edraw-editor) down-event)
  (with-slots (tool) editor
    (when tool
      (edraw-on-double-mouse-1 tool down-event))))

(cl-defmethod edraw-on-S-mouse-1 ((editor edraw-editor) down-event)
  (with-slots (tool) editor
    (when tool
      (edraw-on-S-mouse-1 tool down-event))))

(cl-defmethod edraw-on-C-mouse-1 ((editor edraw-editor) down-event)
  (with-slots (tool) editor
    (when tool
      (edraw-on-C-mouse-1 tool down-event))))

(cl-defmethod edraw-on-mouse-3 ((editor edraw-editor) down-event)
  (with-slots (tool) editor
    (when tool
      (edraw-on-mouse-3 tool down-event))))

;;;;; Editor - Toolbar

(defvar edraw-editor-tool-list '(select rect ellipse path freehand text))
(defvar edraw-editor-tool-map nil)

(defconst edraw-editor-toolbar-button-w 30)
(defconst edraw-editor-toolbar-button-h 24)

(cl-defmethod edraw-update-toolbar ((editor edraw-editor))
  (with-slots (overlay image-scale (current-tool tool)) editor
    (let* (;; Put components
           (icon-w edraw-editor-toolbar-button-w)
           (icon-h edraw-editor-toolbar-button-h)
           (components-g (dom-node 'g))
           (current-tool-class-name
            (and current-tool (eieio-object-class-name current-tool)))
           (padding 4)
           (x padding)
           (y padding)
           (spacing 4)
           (image-map
            (let (image-map)
              ;; main menu button
              (push (edraw-editor-make-toolbar-button
                     components-g
                     x y
                     icon-w icon-h image-scale
                     (edraw-editor-make-icon 'main-menu)
                     'edraw-editor-main-menu
                     (edraw-editor-make-toolbar-help-echo
                      (edraw-msg "Main Menu")
                      'edraw-editor-main-menu)
                     nil)
                    image-map)
              (cl-incf y icon-h)
              (cl-incf y 16) ;;spacing
              ;; tool buttons
              (dolist (tool-id edraw-editor-tool-list)
                (push (edraw-editor-make-tool-button
                       components-g
                       x y
                       icon-w icon-h image-scale
                       tool-id
                       current-tool-class-name)
                      image-map)
                (cl-incf y icon-h)
                (cl-incf y spacing))
              (cl-decf y spacing)
              ;; stroke & fill (tool option)
              (when-let ((shape-type
                          (and current-tool
                               (edraw-shape-type-to-create current-tool))))
                (cl-incf y 16)
                (push (edraw-make-toolbar-color-button
                       editor components-g x y shape-type 'stroke)
                      image-map)
                (cl-incf y icon-h)
                (cl-incf y spacing)
                (push (edraw-make-toolbar-color-button
                       editor components-g x y shape-type 'fill)
                      image-map)
                (cl-incf y icon-h)
                (cl-incf y spacing)
                (push (edraw-editor-make-toolbar-button
                       components-g
                       x y
                       icon-w icon-h image-scale
                       (edraw-editor-make-icon 'edit-tool-properties)
                       'edraw-editor-edit-tool-properties
                       (edraw-editor-make-toolbar-help-echo
                        (format "%s %s"
                                (edraw-msg (capitalize
                                            (symbol-name shape-type)))
                                (edraw-msg "Defaults"))
                        'edraw-editor-edit-tool-properties)
                       nil)
                      image-map)
                (cl-incf y icon-h))

              (nreverse image-map)))
           (bar-w (+ x icon-w padding))
           (bar-h (+ y padding))

           ;; Create SVG Element
           (svg (let ((svg (svg-create (round (* image-scale bar-w))
                                       (round (* image-scale bar-h)))))
                  (svg-gradient svg "icon-fg-gradient" 'linear
                                '((0 . "rgba(255,255,255,0.5)")
                                  (100 . "rgba(255,255,255,0.0)")))
                  (dom-append-child
                   svg
                   (dom-node
                    'defs nil
                    (edraw-svg-ui-transparent-bg-pattern)))
                  (let ((root-g
                         (dom-node
                          'g
                          (list (cons 'transform
                                      (format "scale(%s)" image-scale))))))
                    (dom-append-child svg root-g)
                    (svg-rectangle root-g 0 0 bar-w bar-h :fill "#888")
                    (dom-append-child root-g components-g)
                    svg)))
           ;; Create image
           (image (edraw-svg-to-image svg
                                      :scale 1.0 ;;Cancel image-scale effect
                                      :map image-map))
           ;; Create keymap
           (keymap
            (if edraw-editor-tool-map
                edraw-editor-tool-map
              (let ((km (make-sparse-keymap)))
                (dolist (hot-spot image-map)
                  (let ((key-id (nth 1 hot-spot)))
                    (define-key km (vector key-id 'mouse-1) key-id)))
                km))))
      ;; Put IMAGE to the left side of the editor overlay
      (overlay-put overlay
                   'before-string (propertize "*"
                                              'display image
                                              'face 'default
                                              'keymap keymap)))))

(defun edraw-editor-make-toolbar-button
    (parent x y w h image-scale icon key-id help-echo selected-p)
  (let* ((x0 (floor x))
         (y0 (floor y))
         (x1 (ceiling (+ x w)))
         (y1 (ceiling (+ y h))))
    (svg-rectangle parent
                   x0
                   y0
                   (- x1 x0)
                   (- y1 y0)
                   :fill (if selected-p "#666" "#888") :rx 2 :ry 2)
    (dom-set-attribute icon 'transform (format "translate(%s %s)" x0 y0))
    (dom-append-child parent icon)

    (list (cons 'rect
                (cons (cons (round (* image-scale x0))
                            (round (* image-scale y0)))
                      (cons (round (* image-scale x1))
                            (round (* image-scale y1)))))
          key-id
          (list 'pointer 'hand
                'help-echo help-echo))))

(defun edraw-editor-make-toolbar-help-echo (title command)
  (let* (;; single key only
         (key-event (car
                     (seq-find (lambda (item) (eq (cdr item) command))
                               (cdr edraw-editor-map))))
         (key-str (cond
                   ((null key-event) nil)
                   ((symbolp key-event) (symbol-name key-event))
                   ((integerp key-event) (char-to-string key-event)))))
    (concat
     title ;;localized msg
     (if key-str (concat " (" key-str ")")))))



(defun edraw-editor-make-tool-button
    (parent x y w h image-scale tool-id selected-class-name)
  (edraw-editor-make-toolbar-button
   parent x y w h image-scale
   (edraw-editor-make-tool-icon tool-id)
   (edraw-editor-make-tool-key-id tool-id)
   (edraw-editor-make-tool-help-echo tool-id)
   (eq (edraw-editor-make-tool-class-name tool-id) selected-class-name)))

(defun edraw-editor-make-tool-class-name (tool-id)
  (intern (format "edraw-editor-tool-%s" tool-id)))

(defun edraw-editor-make-tool-key-id (tool-id)
  (edraw-editor-tool-select-function-name tool-id))

(defun edraw-editor-make-tool-click-function (tool-id)
  (edraw-editor-tool-select-function-name tool-id))

(defun edraw-editor-make-tool-help-echo (tool-id)
  (edraw-editor-make-toolbar-help-echo
   (edraw-msg (symbol-name tool-id))
   (edraw-editor-tool-select-function-name tool-id)))


(defun edraw-editor-make-tool (tool-id)
  (funcall (edraw-editor-make-tool-class-name tool-id)))

(defun edraw-editor-tool-select-function-name (tool-id)
  (intern (format "edraw-editor-select-tool-%s" tool-id)))

(defun edraw-editor-define-tool-select-function (tool-id)
  (defalias (edraw-editor-tool-select-function-name tool-id)
    (lambda () (interactive)
      (when-let ((editor (edraw-editor-at-input last-input-event)))
        (edraw-select-tool editor (edraw-editor-make-tool tool-id))))))

(defun edraw-editor-define-tool-select-functions ()
  (dolist (tool-id edraw-editor-tool-list)
    (edraw-editor-define-tool-select-function tool-id)))

(edraw-editor-define-tool-select-functions) ;;defun edraw-editor-select-tool-*

;; Icon 30x24

(defun edraw-editor-make-icon (icon-id)
  (let ((g (dom-node 'g)))
    (funcall (intern (format "edraw-icon-%s" icon-id)) g)
    g))

(defun edraw-icon-main-menu (g)
  (svg-rectangle g 3 4 24 2 :stroke-width 1 :stroke "none" :fill "#eee")
  (svg-rectangle g 3 11 24 2 :stroke-width 1 :stroke "none" :fill "#eee")
  (svg-rectangle g 3 18 24 2 :stroke-width 1 :stroke "none" :fill "#eee"))

(defun edraw-icon-edit-tool-properties (g)
  (svg-rectangle g  3  4  3 3 :stroke "none" :fill "#ccc")
  (svg-rectangle g  9  4 18 3 :stroke "none" :fill "#ccc")
  (svg-rectangle g  3 10  3 3 :stroke "none" :fill "#ccc")
  (svg-rectangle g  9 10 18 3 :stroke "none" :fill "#ccc")
  (svg-rectangle g  3 16  3 3 :stroke "none" :fill "#ccc")
  (svg-rectangle g  9 16 18 3 :stroke "none" :fill "#ccc"))

(defun edraw-editor-make-tool-icon (tool-id)
  (let ((g (dom-node 'g)))
    (funcall (intern (format "edraw-icon-tool-%s" tool-id)) g)
    g))

(defun edraw-icon-tool-select (g)
  (dom-append-child
   g
   (dom-node 'path
             '((d . "M 6 3 L 21 10 17 12 23 18 21 20 15 14 13 18 z")
               (stroke . "#ccc")
               (stroke-width . 1)
               (fill . "url(#icon-fg-gradient)")))))

(defun edraw-icon-tool-rect (g)
  (svg-rectangle
   g 6.5 6.5 18 12 :stroke-width 1 :stroke "#ccc" :gradient "icon-fg-gradient"))

(defun edraw-icon-tool-ellipse (g)
  (svg-ellipse
   g 15 12 9 6 :stroke-width 1 :stroke "#ccc" :gradient "icon-fg-gradient"))

(defun edraw-icon-tool-path (g)
  (svg-node
   g 'path :d "M 4 18 Q 10 6 16 6 Q 22 6 28 18"
   :stroke-width 1 :stroke "#ccc" :fill "none")
  (svg-rectangle g 14 4 4 4 :stroke "none" :gradient "icon-fg-gradient")
  (svg-line g 7 6 25 6 :stroke-width 0.5 :stroke "#ccc")
  (svg-circle g 7 6 0.8 :stroke-width 1 :stroke "#ccc" :fill "none")
  (svg-circle g 25 6 0.8 :stroke-width 1 :stroke "#ccc" :fill "none"))

(defun edraw-icon-tool-freehand (g)
  (svg-node
   g 'path :d "M 4 19 C 15 -3 14 31 27 5"
   :stroke-width 1 :stroke "#ccc" :fill "none"))

(defun edraw-icon-tool-text (g)
  (dom-append-child
   g
   (dom-node 'path
             ;;     8 9 11  14 15 16  19 21 22
             ;; 4.5 +----      +---        -+
             ;;   5 |   +     + +       +   |
             ;;   7 + +       | |         + +
             ;;             + + + +
             ;;   18        +-----+
             '((d . "M 8 4.5 L 22 4.5 L 22 7 L 21 7 Q 21 5 19 5 L 16 5 L 16 17.5 L18 17.5 L 18 18 L 12 18 L 12 17.5 L 14 17.5 L 14 5 L 11 5 Q 9 5 9 7 L 8 7 z")
               (stroke . "#ccc")
               (stroke-width . 1)
               (fill . "url(#icon-fg-gradient)")))))

;; (defun edraw-preview-icon (name)
;;   (interactive "sIcon Name(e.g.tool-text): ")
;;   (message
;;    "%s"
;;    (propertize
;;     "ICON" 'display
;;     (let ((svg (svg-create 30 24)))
;;       (svg-rectangle svg 0 0 40 24 :fill "#888")
;;       (dom-append-child svg (edraw-editor-make-icon (intern name)))
;;       (svg-image svg)))))

(cl-defmethod edraw-make-toolbar-color-button ((editor edraw-editor)
                                               parent x y
                                               shape-type
                                               prop-name)
  (with-slots (image-scale) editor
    (let* ((rect (if (eq prop-name 'stroke)
                     '((0 . 6) . (30 . 18))
                   '((3 . 3) . (27 . 21))))
           (ix (caar rect))
           (iy (cdar rect))
           (iw (- (cadr rect) (caar rect)))
           (ih (- (cddr rect) (cdar rect)))
           (tag-value (edraw-get-selected-tool-default-shape-property
                       editor prop-name)))
      (if (null tag-value)
          (svg-rectangle parent ix iy iw ih :fill "none" :stroke "#666" :stroke-width "1")
        (let* ((value (cdr tag-value))
               (none-p (or (null value) (string= value "none")))
               (icon (let ((g (dom-node 'g)))
                       (cond
                        (none-p
                         (svg-rectangle
                          g ix iy iw ih
                          :fill "none" :stroke "#666" :stroke-width "1")
                         (dom-append-child
                          g
                          (dom-node
                           'path
                           `((d . ,(concat
                                    "M"
                                    (mapconcat #'number-to-string
                                               (list ix iy
                                                     (+ ix 2) iy
                                                     (+ ix iw) (+ iy ih -2)
                                                     (+ ix iw) (+ iy ih)
                                                     (+ ix iw -2) (+ iy ih)
                                                     ix (+ iy 2)) " ")
                                    "Z"))
                             (fill . "#f003")))))
                        (t
                         (svg-rectangle g ix iy iw ih
                                        :fill "#fff" :stroke "#444"
                                        :stroke-width "1")
                         (svg-rectangle g ix iy iw ih
                                        :fill
                                        "url(#edraw-ui-pattern-transparent-bg")
                         (svg-rectangle g ix iy iw ih :fill value)))
                       g))
               (key-id (intern (format "edraw-editor-edit-tool-default-%s"
                                       prop-name))))
          (edraw-editor-make-toolbar-button
           parent
           x y
           edraw-editor-toolbar-button-w edraw-editor-toolbar-button-h
           image-scale
           icon
           key-id
           (edraw-editor-make-toolbar-help-echo
            (format "%s %s"
                    (edraw-msg (capitalize (symbol-name shape-type)))
                    (edraw-msg (capitalize (symbol-name prop-name))))
            key-id)
           nil))))))

(cl-defmethod edraw-edit-selected-tool-default-shape-property
  ((editor edraw-editor) prop-name)
  (when-let ((tag-value (edraw-get-selected-tool-default-shape-property
                         editor prop-name)))
    (let ((new-value (edraw-color-picker-read-color
                      (format "%s %s: " (car tag-value) prop-name)
                      (cdr tag-value)
                      '("" "none")
                      '((:color-name-scheme . 'web)))))
      (edraw-set-selected-tool-default-shape-property
       editor prop-name new-value)
      (edraw-update-toolbar editor))))

(defun edraw-editor-edit-selected-tool-default-shape-property (prop-name)
  (when-let ((editor (edraw-editor-at-input last-input-event)))
    (edraw-edit-selected-tool-default-shape-property editor prop-name)))

(defun edraw-editor-edit-tool-default-fill ()
  (interactive)
  (edraw-editor-edit-selected-tool-default-shape-property 'fill))

(defun edraw-editor-edit-tool-default-stroke ()
  (interactive)
  (edraw-editor-edit-selected-tool-default-shape-property 'stroke))

(defun edraw-editor-edit-tool-properties ()
  (interactive)
  (when-let ((editor (edraw-editor-at-input last-input-event)))
    (when-let ((selected-tool (edraw-selected-tool editor))
               (shape-type (edraw-shape-type-to-create selected-tool)))
      (edraw-edit-default-shape-properties editor shape-type))))



;;;;; Editor - Basic Mouse Reactions

(cl-defmethod edraw-mouse-down-handle-point ((editor edraw-editor)
                                             down-event)
  "Drag a handle point or select it."
  (let* ((down-xy (edraw-mouse-event-to-xy editor down-event))
         (moved-p nil)
         (selected-anchor (edraw-selected-anchor editor))
         (selected-handle (edraw-selected-handle editor))
         (handle (and selected-anchor
                      (edraw-shape-point-find
                       ;; handle points of selected anchor point
                       (edraw-selectable-handles editor)
                       down-xy))))
    (when handle
      (edraw-track-dragging
       down-event
       (lambda (move-event)
         (setq moved-p t)
         (let ((move-xy (edraw-mouse-event-to-xy-snapped editor move-event)))
           ;; If selected handle, move it alone
           (if (and selected-handle
                    (edraw-same-point-p handle selected-handle))
               (edraw-move-on-transformed handle move-xy) ;;notify modification
             (edraw-move-with-opposite-handle-on-transformed handle move-xy)))))
      (unless moved-p
        ;; Click handle point
        (edraw-select-handle editor handle))
      t)))

(cl-defmethod edraw-mouse-down-anchor-point ((editor edraw-editor)
                                             down-event)
  "Drag a anchor point or select it."
  (let* ((down-xy (edraw-mouse-event-to-xy editor down-event))
         (moved-p nil)
         (anchor (seq-some (lambda (shp)
                             (edraw-pick-anchor-point shp down-xy))
                           (edraw-selected-shapes editor))))
    (when anchor
      (edraw-track-dragging
       down-event
       (lambda (move-event)
         (setq moved-p t)
         (let ((move-xy (edraw-mouse-event-to-xy-snapped editor move-event)))
           (edraw-move-on-transformed anchor move-xy)))) ;;notify modification
      (unless moved-p
        ;; Click anchor point
        (edraw-select-anchor editor anchor))
      t)))

(cl-defmethod edraw-mouse-down-shape ((editor edraw-editor) down-event)
  (let* ((down-xy (edraw-mouse-event-to-xy editor down-event))
         (down-xy-snapped (edraw-snap-xy editor down-xy))
         (selected-shapes (edraw-selected-shapes editor))
         (shapes (edraw-find-shapes-by-xy editor down-xy))
         (down-shape (car shapes)) ;;most front
         (down-selected-p (memq down-shape selected-shapes))
         (event-type (if (seq-set-equal-p (event-modifiers down-event)
                                          '(control down))
                         'C-down
                       'down))
         moving-shapes)
    (when down-shape
      ;; Select shape
      (pcase event-type
        ('C-down
         (if down-selected-p
             ;; Remove from selection and prevent dragging
             (progn
               (edraw-remove-shape-selection editor down-shape)
               (setq moving-shapes nil))
           ;; Add to selection
           (edraw-add-shape-selection editor down-shape)
           (edraw-deselect-anchor editor) ;;When pressed delete key next time, I want the selected shapes to be deleted instead of the selected anchor
           (setq moving-shapes (edraw-selected-shapes editor))))
        ('down
         (if down-selected-p
             ;; Deselect other shapes later
             (setq moving-shapes (edraw-selected-shapes editor))
           (edraw-select down-shape)
           (setq moving-shapes (list down-shape)))))

      ;; Move selected shapes
      (when moving-shapes
        (let* ((start-xy down-xy-snapped)
               (last-xy start-xy)
               (moved-p nil))
          (edraw-track-dragging
           down-event
           (lambda (move-event)
             ;; First move
             (unless moved-p
               (setq moved-p t)
               ;; Push undo data
               (edraw-make-undo-group editor 'shapes-move-by-drag
                 (dolist (shp moving-shapes)
                   (edraw-push-undo-translate shp))))
             ;; Move
             (let* ((move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
                    (delta-xy (edraw-xy-sub move-xy last-xy))
                    ;;Disable undo data record
                    (edraw-editor-inhibit-make-undo-data t))
               (dolist (shape moving-shapes)
                 (edraw-translate shape delta-xy)) ;;notify modification
               (setq last-xy move-xy))))
          ;; On click
          (unless moved-p
            (pcase event-type
              ('down
               (if down-selected-p
                   (edraw-select down-shape)))))))
      t)))

(cl-defmethod edraw-context-menu-at-point ((editor edraw-editor) xy)
  (if-let ((shapes (edraw-find-shapes-by-xy editor xy)))
      (let ((selected-shapes (edraw-selected-shapes editor)))
        (if (and (cdr selected-shapes)
                 (edraw-selected-p (car shapes)))
            ;; multiple selected shapes
            (edraw-popup-context-menu-for-selected-shapes editor)
          ;; single selected shape or unselected shape
          (edraw-popup-context-menu (if (cdr shapes)
                                        (edraw-popup-shape-selection-menu shapes)
                                      (car shapes)))))
    ;; document
    (edraw-popup-context-menu-for-document editor)))

(cl-defmethod edraw-read-rectangle ((editor edraw-editor) down-event snap-p)
  (let ((down-xy (if snap-p
                     (edraw-mouse-event-to-xy-snapped editor down-event)
                   (edraw-mouse-event-to-xy editor down-event))))
    (edraw-ui-foreground-svg editor)
    (let ((ui-parent (edraw-ui-foreground-svg editor))
          (ui-preview (dom-node 'rect `((class . "edraw-ui-read-rectangle")
                                        (x . ,(car down-xy))
                                        (y . ,(cdr down-xy))
                                        (width . 1)
                                        (height . 1))))
          move-xy)
      (dom-append-child ui-parent ui-preview)
      (unwind-protect
          (edraw-track-dragging
           down-event
           (lambda (move-event)
             (setq move-xy
                   (if snap-p
                       (edraw-mouse-event-to-xy-snapped editor move-event)
                     (edraw-mouse-event-to-xy editor move-event)))
             (edraw-svg-rect-set-range ui-preview down-xy move-xy)
             (edraw-invalidate-image editor))))
      (dom-remove-node ui-parent ui-preview)
      (edraw-invalidate-image editor)
      (edraw-aabb down-xy (or move-xy down-xy)))))



;;;;; Editor - Editing Tools

(cl-defmethod edraw-select-tool ((editor edraw-editor)
                                 new-tool) ;;edraw-editor-tool
  (when (and (symbolp new-tool) (not (null new-tool)))
    (setq new-tool
          (edraw-editor-make-tool new-tool)))

  (with-slots (tool) editor
    (when tool
      (edraw-on-unselected tool))
    (setq tool new-tool)
    (when tool
      (edraw-on-selected tool editor))
    (edraw-update-toolbar editor)))

(cl-defmethod edraw-selected-tool ((editor edraw-editor))
  (oref editor tool))

(cl-defmethod edraw-get-selected-tool-default-shape-property
  ((editor edraw-editor) prop-name)

  (with-slots (tool) editor
    (let* ((shape-type (when tool (edraw-shape-type-to-create tool))))
      (if shape-type
          (cons
           shape-type
           (edraw-get-default-shape-property editor shape-type prop-name))
        nil))))

(cl-defmethod edraw-set-selected-tool-default-shape-property
  ((editor edraw-editor) prop-name value)

  (with-slots (tool) editor
    (let ((shape-type (when tool (edraw-shape-type-to-create tool))))
      (when shape-type
        (edraw-set-default-shape-property editor shape-type prop-name value)))))



;;;; Tool

(defclass edraw-editor-tool ()
  ((editor
    :type (or null edraw-editor)))
  :abstract t)

(cl-defmethod edraw-tool-type ((tool edraw-editor-tool))
  (let ((name (symbol-name (eieio-object-class-name tool))))
    (when (string-match "\\`edraw-editor-tool-\\(.*\\)\\'" name)
      (intern (match-string 1 name)))))

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool))
  nil)

(cl-defmethod edraw-default-properties ((tool edraw-editor-tool))
  (when-let ((shape-type (edraw-shape-type-to-create tool)))
    (edraw-get-default-shape-properties-by-tag
     (oref tool editor) shape-type))) ;;e.g. (rect (a . "1") (b . "2"))

(cl-defmethod edraw-on-down-mouse-1 ((_tool edraw-editor-tool) _down-event))
(cl-defmethod edraw-on-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-S-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-S-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-C-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-C-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-double-mouse-1 ((_tool edraw-editor-tool) _click-event))

(cl-defmethod edraw-on-mouse-3 ((tool edraw-editor-tool) click-event)
  (with-slots (editor) tool
    (let ((click-xy (edraw-mouse-event-to-xy editor click-event)))

      (cond
       ;; Handle, Anchor
       ((let ((selected-anchor (edraw-selected-anchor editor))
              (selected-shapes (edraw-selected-shapes editor))
              target-spoint
              actions)
          (when (or
                 ;; handle of selected anchor
                 (and selected-anchor
                      (setq target-spoint
                            (edraw-shape-point-find
                             (edraw-selectable-handles editor)
                             click-xy))
                      (setq actions (edraw-get-actions target-spoint)))
                 ;; anchor of selected shape
                 (and selected-shapes
                      (setq target-spoint
                            (seq-some (lambda (shp) (edraw-pick-anchor-point shp click-xy))
                                      selected-shapes))
                      (setq actions (edraw-get-actions target-spoint))))
            (edraw-popup-menu
             (format (edraw-msg "%s Point") ;;"Anchor Point" or "Handle Point"
                     (capitalize
                      (symbol-name
                       (edraw-get-point-type target-spoint))))
             actions target-spoint)
            t)))
       ;; Shape
       (t
        (edraw-context-menu-at-point editor click-xy))))))

(cl-defmethod edraw-on-selected ((tool edraw-editor-tool) (editor edraw-editor))
  (oset tool editor editor))
(cl-defmethod edraw-on-unselected ((tool edraw-editor-tool))
  (oset tool editor nil))



;;;;; Tool - Select Tool

(defclass edraw-editor-tool-select (edraw-editor-tool)
  ())

(cl-defmethod edraw-on-unselected ((_tool edraw-editor-tool-select))
  ;;(edraw-unselect-shape (oref tool editor))
  (cl-call-next-method))

(cl-defmethod edraw-on-C-down-mouse-1 ((tool edraw-editor-tool-select)
                                       down-event)
  (with-slots (editor) tool
    (cond
     ;; Drag or click a shape
     ((edraw-mouse-down-shape editor down-event)))))

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-select)
                                     down-event)
  (with-slots (editor) tool
    (cond
     ;; Drag or click a handle point of selected anchor point
     ((edraw-mouse-down-handle-point editor down-event))

     ;; Drag or click a anchor point of selected shape
     ((edraw-mouse-down-anchor-point editor down-event))

     ;; Drag or click a shape
     ((edraw-mouse-down-shape editor down-event))

     ;; Select by rectangle
     ((let ((rect (edraw-read-rectangle editor down-event nil)))
        (unless (edraw-rect-empty-p rect)
          (edraw-deselect-all-shapes editor)
          (dolist (shp (edraw-find-shapes-by-rect editor rect))
            (edraw-add-shape-selection editor shp))
          t)))

     ;; Deselect
     (t (edraw-deselect-all-shapes editor)))))

(cl-defmethod edraw-on-double-mouse-1 ((tool edraw-editor-tool-select)
                                       click-event)
  (with-slots (editor) tool
    (let* ((click-xy (edraw-mouse-event-to-xy editor click-event))
           (shape (car (edraw-find-shapes-by-xy editor click-xy))))
      (when shape
        (edraw-edit-properties shape)))))



;;;;; Tool - Rect Tool

(defclass edraw-editor-tool-rect (edraw-editor-tool)
  ()
  )

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-rect))
  'rect)

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-rect)
                                     down-event)
  (with-slots (editor) tool
    (let ((down-xy (edraw-mouse-event-to-xy-snapped editor down-event))
          (move-xy nil))
      ;; Preview
      (let* ((edraw-editor-inhibit-make-undo-data t)
             (edraw-editor-keep-modified-flag t)
             (shape (edraw-create-shape ;;notify modification
                     editor
                     (edraw-svg-body editor)
                     'rect
                     'x (car down-xy)
                     'y (cdr down-xy)
                     'width 0
                     'height 0)))
        (unwind-protect
            (progn
              (edraw-select-shape editor shape)

              (edraw-track-dragging
               down-event
               (lambda (move-event)
                 (setq move-xy
                       (edraw-mouse-event-to-xy-snapped editor move-event))
                 (edraw-set-rect shape down-xy move-xy)))) ;;notify modification
          (edraw-remove shape)))

      ;; Create
      (when (and move-xy
                 (not (edraw-xy-empty-aabb-p down-xy move-xy)))
        (let ((shape (edraw-create-shape ;;notify modification
                      editor
                      (edraw-svg-body editor)
                      'rect
                      'x (min (car down-xy) (car move-xy))
                      'y (min (cdr down-xy) (cdr move-xy))
                      'width (abs (- (car down-xy) (car move-xy)))
                      'height (abs (- (cdr down-xy) (cdr move-xy))))))
          (edraw-select-shape editor shape))))))



;;;;; Tool - Ellipse Tool

(defclass edraw-editor-tool-ellipse (edraw-editor-tool)
  ()
  )

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-ellipse))
  'ellipse)

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-ellipse)
                                     down-event)
  (with-slots (editor) tool
    (edraw-deselect-all-shapes editor)
    (let ((down-xy (edraw-mouse-event-to-xy-snapped editor down-event))
          (move-xy nil))
      ;; Preview
      (let* ((edraw-editor-inhibit-make-undo-data t)
             (edraw-editor-keep-modified-flag t)
             (shape (edraw-create-shape ;;notify modification
                     editor
                     (edraw-svg-body editor)
                     'ellipse
                     'cx (car down-xy)
                     'cy (cdr down-xy))))
        (unwind-protect
            (progn
              (edraw-select-shape editor shape)

              (edraw-track-dragging
               down-event
               (lambda (move-event)
                 (setq move-xy
                       (edraw-mouse-event-to-xy-snapped editor move-event))
                 (edraw-set-rect shape down-xy move-xy)))) ;;notify modification
          (edraw-remove shape)))

      ;; Create
      (when (and move-xy
                 (not (edraw-xy-empty-aabb-p down-xy move-xy)))
        (let ((shape (edraw-create-shape ;;notify modification
                      editor
                      (edraw-svg-body editor)
                      'ellipse
                      'cx (* 0.5 (+ (car down-xy) (car move-xy)))
                      'cy (* 0.5 (+ (cdr down-xy) (cdr move-xy)))
                      'rx (* 0.5 (abs (- (car down-xy) (car move-xy))))
                      'ry (* 0.5 (abs (- (cdr down-xy) (cdr move-xy)))))))
          (edraw-select-shape editor shape))))))



;;;;; Tool - Text Tool

(defclass edraw-editor-tool-text (edraw-editor-tool)
  ()
  )

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-text))
  'text)

(cl-defmethod edraw-on-mouse-1 ((tool edraw-editor-tool-text) click-event)
  (edraw-put-text-shape tool click-event edraw-snap-text-to-shape-center))

(cl-defmethod edraw-on-S-mouse-1 ((tool edraw-editor-tool-text) click-event)
  (edraw-put-text-shape tool click-event (not edraw-snap-text-to-shape-center)))

(cl-defmethod edraw-put-text-shape ((tool edraw-editor-tool-text) click-event
                                    snap-to-shape-center-p)
  (let ((text (read-string (edraw-msg "Text: "))))
    (unless (string-empty-p text)
      (with-slots (editor) tool
        (edraw-deselect-all-shapes editor)
        (let* ((click-xy (edraw-mouse-event-to-xy editor click-event))
               (click-xy-snapped
                (or (and snap-to-shape-center-p
                         (edraw-snap-text-to-back-shape-center tool click-xy))
                    (edraw-snap-xy editor click-xy)))
               (shape (edraw-create-shape ;;notify modification
                       editor
                       (edraw-svg-body editor)
                       'text
                       'x (car click-xy-snapped)
                       'y (cdr click-xy-snapped)
                       'text text)))
          (edraw-select-shape editor shape))))))

(cl-defmethod edraw-snap-text-to-back-shape-center ((tool edraw-editor-tool-text) xy)
  (with-slots (editor) tool
    (when-let ((font-size (edraw-svg-length-string-to-number
                           (edraw-get-default-shape-property
                            editor 'text 'font-size)))
               (shape (car (edraw-find-shapes-by-xy editor xy))))
      (when-let ((rect (ignore-errors (edraw-get-rect shape)))
                 (center (edraw-rect-center rect)))
        (when (< (edraw-xy-distance center xy) font-size)
          (cons
           (car center)
           (+ (cdr center) (* 0.4 font-size)))))))) ;;@todo ascent font-size ratio


;;;;; Tool - Path Tool

(defclass edraw-editor-tool-path (edraw-editor-tool)
  ((editing-path
    :initform nil
    :type (or null edraw-shape-path))))

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-path))
  'path)

(cl-defmethod edraw-on-unselected ((tool edraw-editor-tool-path))
  (edraw-clear tool)
  (cl-call-next-method))

(cl-defmethod edraw-mouse-down-continue-path ((tool edraw-editor-tool-path)
                                              down-event)
  (with-slots (editor editing-path) tool
    (when (null editing-path)
      ;;@todo support all selected shapes or all shapes
      (when-let ((selected-path (edraw-cast (edraw-last-selected-shape editor)
                                            'edraw-shape-path)))
        (let* ((down-xy (edraw-mouse-event-to-xy editor down-event))
               (last-anchor (edraw-get-last-anchor-point selected-path))
               (first-anchor (edraw-get-first-anchor-point selected-path))
               (anchor
                (or
                 (when (and last-anchor
                            (not (edraw-in-closed-subpath-p last-anchor))
                            (edraw-hit-input-p last-anchor down-xy))
                   last-anchor)
                 (when (and first-anchor
                            (not (edraw-in-closed-subpath-p first-anchor))
                            (edraw-hit-input-p first-anchor down-xy))
                   (edraw-reverse-path selected-path)
                   first-anchor))))
          (when anchor
            (setq editing-path selected-path)

            (message (edraw-msg "Connected"))
            (edraw-select-anchor editor anchor)

            ;; Drag
            (edraw-drag-handle-on-click-anchor anchor nil down-event editor)
            t
            ))))))

(cl-defmethod edraw-mouse-down-close-path ((tool edraw-editor-tool) down-event)
  "Click the first anchor point of the editing path and drag it."
  (with-slots (editor editing-path) tool
    (when (and editing-path
               (edraw-closable-path-shape-p editing-path))
      (let ((down-xy (edraw-mouse-event-to-xy editor down-event))
            (anchor (edraw-get-first-anchor-point editing-path)))
        (when (and anchor
                   (edraw-hit-input-p anchor down-xy)
                   (edraw-close-path-shape editing-path))
          (message (edraw-msg "Closed"))

          (edraw-select-anchor editor anchor)

          ;; Drag the `backward' handle of ANCHOR
          (edraw-drag-handle-on-click-anchor anchor 'backward down-event editor)
          (edraw-clear tool)
          t
          )))))

(cl-defmethod edraw-mouse-down-connect-to-another-path ((tool edraw-editor-tool) down-event)
  (with-slots (editor editing-path) tool
    (when editing-path
      (let* ((down-xy (edraw-mouse-event-to-xy editor down-event))
             (anchor (car (delq editing-path (edraw-find-end-points-of-path-shapes editor down-xy)))))
        (when anchor
          (when (edraw-connect-path-to-anchor editing-path anchor) ;;notify modification
            (edraw-clear tool)
            (edraw-select-shape editor (edraw-parent-shape anchor))
            (edraw-select-anchor editor anchor)
            (message "Connected")

            (edraw-drag-handle-on-click-anchor anchor 'backward down-event editor)
            t))))))

(defun edraw-drag-handle-on-click-anchor (anchor backward-p down-event editor)
  (let ((anchor-xy (edraw-get-xy-transformed anchor))
        dragging-point
        moved-p)
    (edraw-track-dragging
     down-event
     (lambda (move-event)
       (setq moved-p t)
       (let ((move-xy (edraw-mouse-event-to-xy-snapped editor move-event)))
         (when (null dragging-point)
           (setq dragging-point
                 ;;notify modification
                 (if backward-p
                     (edraw-create-backward-handle anchor)
                   (edraw-create-forward-handle anchor))))
         (when dragging-point
           (edraw-move-with-opposite-handle-on-transformed ;;notify modification
            dragging-point
            (if backward-p
                (edraw-xy-sub (edraw-xy-nmul 2 anchor-xy) move-xy)
              move-xy)))
         )))
    moved-p))

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-path)
                                     down-event)
  (with-slots (editor editing-path) tool
    (when (and editing-path
               (or (edraw-removed-p editing-path)
                   (edraw-closed-path-shape-p editing-path)))
      (setq editing-path nil))

    (let* ((down-xy (edraw-mouse-event-to-xy-snapped editor down-event))
           (ignore-existing-point-p (equal current-prefix-arg '(4)))
           ;;@todo customize
           (enable-connect-p (not ignore-existing-point-p))
           (enable-move-p (not ignore-existing-point-p)))
      (cond
       ((and enable-connect-p
             (or
              (edraw-mouse-down-continue-path tool down-event)

              (edraw-mouse-down-close-path tool down-event)

              (edraw-mouse-down-connect-to-another-path tool down-event))))

       ((and enable-move-p
             (or
              ;; Drag or click a handle point of selected anchor point
              (edraw-mouse-down-handle-point editor down-event)

              ;; Drag or click a anchor point of selected shape
              (edraw-mouse-down-anchor-point editor down-event))))

       (t
        (edraw-make-undo-group editor 'path-tool-add-new-point
          ;; Add a new shape
          (if (null editing-path)
              (progn
                (edraw-deselect-all-shapes editor)
                (setq editing-path
                      (edraw-create-shape ;;notify modification
                       editor (edraw-svg-body editor) 'path))
                ;; Select new shape
                (edraw-select-shape editor editing-path)))

          (let* (;; Add a new point
                 (anchor-point (edraw-add-anchor-point editing-path down-xy)) ;;notify modification
                 dragging-point)

            ;; Select last anchor point
            (edraw-select-anchor editor anchor-point)

            ;; Drag handle points of the new point
            (edraw-track-dragging
             down-event
             (lambda (move-event)
               (let* ((move-xy
                       (edraw-mouse-event-to-xy-snapped editor move-event)))

                 (unless dragging-point
                   (setq dragging-point
                         (edraw-create-forward-handle anchor-point)) ;;notify modification
                   (when dragging-point
                     (edraw-create-backward-handle anchor-point))) ;;notify modification

                 (when dragging-point
                   (edraw-move-with-opposite-handle-symmetry-on-transformed
                    dragging-point
                    move-xy
                    t);;notify modification
                   )))))))))))

(cl-defmethod edraw-clear ((tool edraw-editor-tool-path))
  (with-slots (editor editing-path) tool
    (when editing-path
      (setq editing-path nil))))

(cl-defmethod edraw-find-end-points-of-path-shapes ((editor edraw-editor) xy)
  (let (points)
    (dolist (node (dom-children (edraw-svg-body editor)))
      (when-let ((shape (edraw-shape-from-element node editor 'noerror)))
        (when (cl-typep shape 'edraw-shape-path)
          ;;@todo include endpoint of subpaths?
          (dolist (anchor (list (edraw-get-first-anchor-point shape)
                                (edraw-get-last-anchor-point shape)))
            (when (and anchor
                       (edraw-hit-input-p anchor xy)
                       (not (edraw-in-closed-subpath-p anchor)))
              (push anchor points))))))
    points)) ;;front to back



;;;;; Tool - Freehand Tool

(defclass edraw-editor-tool-freehand (edraw-editor-tool)
  ())

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-freehand))
  'path)

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-freehand)
                                     down-event)
  (with-slots (editor) tool
    (let ((down-xy (edraw-mouse-event-to-xy-snapped editor down-event)))
      (edraw-make-undo-group editor 'freehand-tool-add-path
        ;; Deselect
        (edraw-deselect-all-shapes editor)

        ;; Add a new path shape
        (let ((editing-path (edraw-create-shape ;;modify
                             editor (edraw-svg-body editor) 'path)))

          ;; Add the first point of the path
          (edraw-add-anchor-point editing-path down-xy) ;;modify

          ;; Add new points on dragging
          (edraw-track-dragging
           down-event
           (lambda (move-event)
             (let ((move-xy
                    (edraw-mouse-event-to-xy-snapped editor move-event)))

               ;;@todo realtime simplification & smoothing the path

               (edraw-add-anchor-point editing-path move-xy);;modify
               )))

          ;; Post process
          ;; @todo cancel if not moved
          ;; @todo simplification
          ;; @todo more better smoothing
          (let ((shape-points (edraw-get-anchor-points editing-path)))
            (dolist (spt shape-points)
              (edraw-make-smooth spt)))

          )))))



;;;; Shape

;;
;;

(defun edraw-merge-properties (props-default props-alist)
  (append
   (seq-difference props-default props-alist
                   (lambda (a b) (eq (car a) (car b))))
   props-alist))

(defun edraw-create-shape (editor parent tag &rest props)
  (edraw-create-shape-without-default
   editor parent
   tag
   ;; Complete property values with default values
   (edraw-merge-properties
    (alist-get tag (oref editor default-shape-properties))
    ;; Convert plist to alist
    (cl-loop for (prop-name value) on props by #'cddr
             collect (cons prop-name value)))))

(defun edraw-create-shape-without-default (editor parent tag props-alist)
  (let* ((shape (edraw-shape-from-element
                 (edraw-create-shape-svg-element (oref editor defrefs)
                                                 parent tag props-alist)
                 editor)))
    (edraw-push-undo editor 'shape-create (list 'edraw-remove shape))
    (edraw-on-shape-changed shape 'shape-create)
    shape))

(defun edraw-create-shape-svg-element (defrefs parent tag props-alist)
  (let ((element (dom-node tag)))
    ;; Set properties
    (dolist (prop props-alist)
      (let ((prop-name (car prop))
            (value (cdr prop)))
        (edraw-svg-element-set-property element prop-name value defrefs)))
    ;; Add element to parent
    (when parent
      (dom-append-child parent element))
    element))

(defun edraw-shape-from-element (element editor &optional noerror-node-type)
  "Return the shape object for ELEMENT, creating a new one if needed."
  (if (not (edraw-dom-element-p element))
      (unless noerror-node-type
        (error "Unsupported node type %s" element))
    (or
     (dom-attr element :-edraw-shape)
     (when-let ((shape (pcase (dom-tag element)
                         ('rect (edraw-shape-rect-create element editor))
                         ('ellipse (edraw-shape-ellipse-create element editor))
                         ('circle (edraw-shape-circle-create element editor))
                         ('text (edraw-shape-text-create element editor))
                         ('path (edraw-shape-path-create element editor))
                         ('g (edraw-shape-group-create element editor))
                         (_ (unless noerror-node-type
                              (error "Unsupported tag %s as shape"
                                     (dom-tag element)))))))
       (dom-set-attribute element :-edraw-shape shape)
       shape))))

(defun edraw-shape-from-element-no-create (element)
  (when (edraw-dom-element-p element)
    (dom-attr element :-edraw-shape)))

(defun edraw-popup-shape-selection-menu (shapes)
  "Show a menu to select one from multiple shape objects."
  (if (cdr shapes)
      ;; 2 or more shapes
      (x-popup-menu
       t
       (list
        (edraw-msg "Select an object")
        (cons ""
              (cl-loop for shape in shapes
                       collect (cons (edraw-get-summary shape) shape)))))
    ;; 0 or 1 shapes
    (car shapes)))



;;;;; Shape - Base Class

(defclass edraw-shape ()
  ((element :initarg :element)
   (editor :initarg :editor
           :type edraw-editor)
   (change-hook :initform (edraw-hook-make))
   (removed-p :initform nil))
  :abstract t)

;;;;;; Internal

(cl-defmethod edraw-element ((shape edraw-shape))
  "Used only internally."
  (oref shape element))

(cl-defmethod edraw-parent-element ((shape edraw-shape))
  "Used only internally."
  (with-slots (editor) shape
    (dom-parent
     (edraw-svg-body editor)
     (edraw-element shape))))

;;;;;; Clone

(cl-defmethod edraw-clone ((shape edraw-shape))
  (with-slots (editor) shape
    (when-let ((shape-type (edraw-shape-type shape)))
      (edraw-create-shape-without-default
       editor
       (edraw-parent-element shape)
       shape-type
       ;; Copy all properties
       (mapcar
        (lambda (prop-info)
          (let* ((prop-name (car prop-info))
                 (value (edraw-get-property shape prop-name)))
            (cons prop-name value)))
        (edraw-get-property-info-list shape))))))

(cl-defmethod edraw-shape-descriptor ((shape edraw-shape))
  (list
   (cons :type (edraw-shape-type shape))
   (cons :properties (edraw-get-all-properties shape))))

(defun edraw-shape-from-shape-descriptor (editor parent shape-descriptor)
  (let ((type (alist-get :type shape-descriptor))
        (props (alist-get :properties shape-descriptor)))
    (edraw-create-shape-without-default editor parent type props)))

(cl-defmethod edraw-copy ((shape edraw-shape))
  (edraw-clipboard-set 'shape-descriptor-list
                       (list (edraw-shape-descriptor shape))))

(cl-defmethod edraw-cut ((shape edraw-shape))
  (edraw-clipboard-set 'shape-descriptor-list
                       (list (edraw-shape-descriptor shape)))
  (edraw-remove shape))

;;;;;; Hooks

(cl-defmethod edraw-on-shape-changed ((shape edraw-shape) type)
  (with-slots (editor) shape
    (edraw-on-document-changed editor 'shape)
    (edraw-notify-change-hook shape type)))

(cl-defmethod edraw-notify-change-hook ((shape edraw-shape) type)
  (with-slots (editor change-hook) shape
    (edraw-hook-call change-hook shape type)))

(cl-defmethod edraw-add-change-hook ((shape edraw-shape) function &rest args)
  (with-slots (change-hook) shape
    (apply 'edraw-hook-add change-hook function args)))

(cl-defmethod edraw-remove-change-hook ((shape edraw-shape) function &rest args)
  (with-slots (change-hook) shape
    (apply 'edraw-hook-remove change-hook function args)))

;;;;;; Remove/Translate

(cl-defmethod edraw-remove ((shape edraw-shape))
  (with-slots (element editor removed-p) shape
    (edraw-push-undo
     editor
     'shape-remove
     (list 'edraw-insert shape
           (edraw-node-position shape)
           (edraw-parent-element shape)))
    (dom-remove-node (edraw-parent-element shape) element)
    (setq removed-p t)
    (edraw-on-shape-changed shape 'shape-remove)))

(cl-defmethod edraw-removed-p ((shape edraw-shape))
  (oref shape removed-p))

(cl-defmethod edraw-insert ((shape edraw-shape) &optional pos parent)
  (with-slots (element editor removed-p) shape
    (when removed-p
      (let* ((parent-element
              (cond
               ((cl-typep parent 'edraw-shape) (edraw-element parent))
               ((edraw-dom-element-p parent) parent)
               ((null parent) (edraw-svg-body editor))))
             (pos
              (cond
               ((integerp pos) pos)
               ((eq pos 'first) 0)
               ;;'last or nil
               (t (length (dom-children parent-element))))))
        (edraw-dom-insert-nth parent-element element pos)
        (setq removed-p nil)
        (edraw-push-undo editor 'shape-insert (list 'edraw-remove shape));;@todo if not removed-p?
        (edraw-on-shape-changed shape 'shape-insert)))))

;;(cl-defmethod edraw-translate ((shape edraw-shape) xy) )
;;(cl-defmethod edraw-transform ((shape edraw-shape) matrix) )

;;;;;; Search

(cl-defmethod edraw-pick-anchor-point ((shape edraw-shape) xy)
  (edraw-shape-point-find
   (edraw-get-anchor-points shape)
   xy))

(cl-defmethod edraw-owned-shape-point-p ((shape edraw-shape) spt)
  (seq-some (lambda (anchor)
              (or (edraw-same-point-p anchor spt)
                  (seq-some (lambda (handle) (edraw-same-point-p handle spt))
                            (edraw-get-handle-points anchor))))
            (edraw-get-anchor-points shape)))

;;;;;; Properties

(cl-defmethod edraw-name ((shape edraw-shape))
  (let ((name (eieio-object-name-string shape)))
    (if (string-match "\\`edraw-shape-\\(.*\\)\\'" name)
        (match-string 1 name)
      name)))

(cl-defmethod edraw-get-defrefs ((shape edraw-shape))
  (oref (oref shape editor) defrefs))

(cl-defmethod edraw-get-summary ((shape edraw-shape))
  (edraw-svg-element-summary (edraw-element shape)))

(cl-defmethod edraw-has-property-p ((shape edraw-shape) prop-name)
  (edraw-svg-element-has-property-p (edraw-element shape)
                                    prop-name
                                    (edraw-get-defrefs shape)))

(cl-defmethod edraw-get-property-info-list ((shape edraw-shape))
  (edraw-svg-element-get-property-info-list (edraw-element shape)))

(cl-defmethod edraw-get-property ((shape edraw-shape) prop-name)
  (edraw-svg-element-get-property (edraw-element shape) prop-name
                                  (edraw-get-defrefs shape)))

(cl-defmethod edraw-get-all-properties ((shape edraw-shape));;@todo generalize
  (cl-loop for prop-info in (edraw-get-property-info-list shape)
           collect (let ((prop-name (car prop-info)))
                     (cons prop-name
                           (edraw-get-property shape prop-name)))))

(cl-defmethod edraw-set-properties ((shape edraw-shape) prop-list)
  (edraw-set-properties-internal
   shape prop-list (edraw-undo-list (oref shape editor)) nil))

(cl-defmethod edraw-set-properties-internal ((shape edraw-shape) prop-list
                                             undo-list-end changed)
  (let ((old-prop-list nil)
        (defrefs (edraw-get-defrefs shape)))
    (with-slots (element) shape
      (dolist (prop prop-list)
        (let* ((prop-name (car prop))
               (new-value (cdr prop))
               (old-value (edraw-svg-element-get-property
                           element prop-name defrefs)))
          (when (not (equal new-value old-value))
            ;;(message "%s: %s to %s" prop-name old-value new-value)
            (push (cons prop-name old-value) old-prop-list)
            (edraw-svg-element-set-property element prop-name new-value
                                            defrefs)))))
    (when old-prop-list
      (setq changed t)
      (edraw-push-undo
       (oref shape editor)
       'shape-properties
       (list #'edraw-set-properties shape old-prop-list))
      (edraw-on-shape-changed shape 'shape-properties)))

  ;; Merge undo data
  (unless edraw-editor-inhibit-make-undo-data
    (with-slots (editor) shape
      ;; Merge undo data generated in this function call.
      ;; [top ~ undo-list-end) in undo-list
      (edraw-merge-set-properties-undo-data
       (edraw-undo-list editor) undo-list-end nil 'shape-properties)
      ;; Merge undo data with generated by previous set-properties call.
      ;; Same property set only.
      (edraw-merge-set-properties-undo-data
       (edraw-undo-list editor)
       (cdr undo-list-end) 'shape-properties nil t)))
  changed)

(cl-defmethod edraw-set-property ((shape edraw-shape) prop-name value);;@todo generalize
  (edraw-set-properties
   shape
   (list (cons prop-name value))))

(cl-defmethod edraw-push-undo-properties ((shape edraw-shape) type prop-names)
  (edraw-push-undo
   (oref shape editor)
   type
   (list #'edraw-set-properties shape
         (mapcar (lambda (prop-name)
                   ;; Get from attribute of element
                   ;; (do not use edraw-get-property for path d=)
                   (cons prop-name
                         (let ((value (dom-attr (edraw-element shape) prop-name)))
                           (if (null value)
                               value
                             (format "%s" value)))))
                 prop-names))))

(defun edraw-merge-set-properties-undo-data (begin end &optional match-type
                                                   new-type same-property-set)
  (when (and begin
             (not (eq begin end)))
    (let ((type (nth 0 (car begin)))
          (func (nth 1 (car begin)))
          (shape (nth 2 (car begin)))
          (props (nth 3 (car begin))))
      (when (and (eq func #'edraw-set-properties)
                 (or (null match-type)
                     (eq type match-type)))
        (let ((it (cdr begin)))
          (while (and it
                      (not (eq it end))
                      (or (null match-type)
                          (eq (nth 0 (car it)) match-type)) ;;type
                      (eq (nth 1 (car it)) #'edraw-set-properties) ;;func
                      (eq (nth 2 (car it)) shape) ;;shape
                      (or (null same-property-set)
                          (seq-set-equal-p
                           props
                           (nth 3 (car it)) ;props
                           (lambda (a b) (eq (car a) (car b))))))
            (let ((old-props (nth 3 (car it))))
              (dolist (prop old-props)
                (setf (alist-get (car prop) props nil nil #'eq) (cdr prop))))
            (setq it (cdr it)))
          (setf (nth 3 (car begin)) props)
          (when new-type
            (setf (nth 0 (car begin)) new-type))
          (setcdr begin it)))))
  begin)
;; (edraw-merge-set-properties-undo-data
;;   (list
;;     (list 'type1 'edraw-set-properties nil '((a . "1") (b . "2")))
;;     (list 'type1 'edraw-set-properties nil '((a . "3") (b . "4")))
;;     (list 'type1 'edraw-set-properties nil '((b . "5") (c . "6")))
;;     (list 'type1 'edraw-set-properties t '((b . "7") (c . "8"))))
;;   nil
;;   'type1 'type4 nil)



;;;;;; Siblings

(cl-defmethod edraw-next-sibling ((shape edraw-shape))
  (with-slots (editor) shape
    (let ((parent (edraw-parent-element shape))
          (node (edraw-element shape))
          (shape nil))
      ;; Find next supported element
      (while (and (setq node (edraw-dom-next-sibling parent node))
                  (null (setq shape (edraw-shape-from-element
                                     node editor 'noerror)))))
      shape)))

(cl-defmethod edraw-previous-sibling ((shape edraw-shape))
  (with-slots (editor) shape
    (let ((parent (edraw-parent-element shape))
          (node (edraw-element shape))
          (shape nil))
      ;; Find previous supported element
      (while (and (setq node (edraw-dom-previous-sibling parent node))
                  (null (setq shape (edraw-shape-from-element
                                     node editor 'noerror)))))
      shape)))

;;;;;; Z Order

(cl-defmethod edraw-node-position ((shape edraw-shape))
  (seq-position
   (dom-children (edraw-parent-element shape))
   (edraw-element shape)
   #'eq))

(cl-defmethod edraw-set-node-position ((shape edraw-shape) pos)
  (let ((old-pos (edraw-node-position shape)))
    (unless (equal pos old-pos)
      (edraw-push-undo
       (oref shape editor)
       'shape-z-order (list 'edraw-set-node-position shape old-pos))
      (let ((parent (edraw-parent-element shape))
            (element (edraw-element shape)))
        (dom-remove-node parent element)
        (edraw-dom-insert-nth parent element pos))
      (edraw-on-shape-changed shape 'shape-z-order))))

(cl-defmethod edraw-front-p ((shape edraw-shape))
  (edraw-dom-last-node-p (edraw-parent-element shape) (edraw-element shape)))

(cl-defmethod edraw-back-p ((shape edraw-shape))
  (edraw-dom-first-node-p (edraw-parent-element shape) (edraw-element shape)))

(cl-defmethod edraw-bring-to-front ((shape edraw-shape))
  (let ((old-pos (edraw-node-position shape)))
    (when (edraw-dom-reorder-last (edraw-parent-element shape)
                                  (edraw-element shape))
      (edraw-push-undo (oref shape editor) 'shape-z-order
                       (list 'edraw-set-node-position shape old-pos))
      (edraw-on-shape-changed shape 'shape-z-order))))

(cl-defmethod edraw-bring-forward ((shape edraw-shape))
  (let ((old-pos (edraw-node-position shape)))
    (when (edraw-dom-reorder-next (edraw-parent-element shape)
                                  (edraw-element shape))
      (edraw-push-undo (oref shape editor) 'shape-z-order
                       (list 'edraw-set-node-position shape old-pos))
      (edraw-on-shape-changed shape 'shape-z-order))))

(cl-defmethod edraw-send-backward ((shape edraw-shape))
  (let ((old-pos (edraw-node-position shape)))
    (when (edraw-dom-reorder-prev (edraw-parent-element shape)
                                  (edraw-element shape))
      (edraw-push-undo (oref shape editor) 'shape-z-order
                       (list 'edraw-set-node-position shape old-pos))
      (edraw-on-shape-changed shape 'shape-z-order))))

(cl-defmethod edraw-send-to-back ((shape edraw-shape))
  (let ((old-pos (edraw-node-position shape)))
    (when (edraw-dom-reorder-first (edraw-parent-element shape)
                                   (edraw-element shape))
      (edraw-push-undo (oref shape editor) 'shape-z-order
                       (list 'edraw-set-node-position shape old-pos))
      (edraw-on-shape-changed shape 'shape-z-order))))

;;;;;; Interactive Command

(cl-defmethod edraw-select ((shape edraw-shape))
  (with-slots (editor) shape
    (edraw-select-tool editor 'select)
    (edraw-select-shape editor shape)))

(cl-defmethod edraw-selected-p ((shape edraw-shape))
  (with-slots (editor) shape
    (edraw-selected-shape-p editor shape)))

(cl-defmethod edraw-delete-with-confirm ((shape edraw-shape))
  (when (edraw-y-or-n-p (format "%s %s?"
                                (edraw-msg "Delete")
                                (edraw-get-summary shape)))
    (edraw-remove shape)))

(cl-defmethod edraw-duplicate-and-select ((shape edraw-shape))
  (when-let ((new-shape (edraw-clone shape)))
    (edraw-select new-shape)))

(cl-defmethod edraw-edit-properties ((shape edraw-shape))
  (edraw-property-editor-open shape))

(cl-defmethod edraw-edit-property-paint ((shape edraw-shape) prop-name)
  (let* ((curr-value (edraw-get-property shape prop-name))
         (new-value
          (let ((edraw-editor-inhibit-make-undo-data t)
                (edraw-editor-keep-modified-flag t))
            (unwind-protect
                (edraw-color-picker-read-color
                 (format "%s: " prop-name)
                 (or curr-value "")
                 '("" "none")
                 `((:color-name-scheme . 'web)
                   (:on-input-change
                    . ,(lambda (string color)
                         (when (or (member string '("" "none"))
                                   color)
                           ;;@todo suppress modified flag change and notification
                           (edraw-set-property shape prop-name string))))))
              (edraw-set-property shape prop-name curr-value)))))
    (when (string-empty-p new-value)
      (setq new-value nil))
    (when (not (equal new-value curr-value))
      (edraw-set-property shape prop-name new-value))))

(cl-defmethod edraw-edit-fill ((shape edraw-shape))
  (edraw-edit-property-paint shape 'fill))

(cl-defmethod edraw-edit-stroke ((shape edraw-shape))
  (edraw-edit-property-paint shape 'stroke))

(cl-defmethod edraw-get-actions ((shape edraw-shape))
  `(((edraw-msg "Select") edraw-select)
    ((edraw-msg "Properties...") edraw-edit-properties)
    ((edraw-msg "Set")
     (((edraw-msg "Fill...") edraw-edit-fill)
      ((edraw-msg "Stroke...") edraw-edit-stroke)))
    ((edraw-msg "Z-Order")
     (((edraw-msg "Bring to Front") edraw-bring-to-front
       :enable ,(not (edraw-front-p shape)))
      ((edraw-msg "Bring Forward") edraw-bring-forward
       :enable ,(not (edraw-front-p shape)))
      ((edraw-msg "Send Backward") edraw-send-backward
       :enable ,(not (edraw-back-p shape)))
      ((edraw-msg "Send to Back") edraw-send-to-back
       :enable ,(not (edraw-back-p shape)))))
    ((edraw-msg "Delete...") edraw-delete-with-confirm)
    ((edraw-msg "Duplicate") edraw-duplicate-and-select)
    ((edraw-msg "Copy") edraw-copy)
    ((edraw-msg "Cut") edraw-cut)))

(cl-defmethod edraw-popup-context-menu ((shape edraw-shape))
  (edraw-popup-menu
   (edraw-get-summary shape)
   (edraw-get-actions shape)
   shape))

;;;;;; Transform Property

(cl-defmethod edraw-transform-prop-exists-p ((shape edraw-shape))
  (edraw-has-property-p shape 'transform))

(cl-defmethod edraw-transform-prop-get-matrix ((shape edraw-shape))
  (when-let ((transform-str (edraw-get-property shape 'transform)))
    (ignore-errors
      (edraw-svg-transform-to-matrix transform-str))))

(cl-defmethod edraw-transform-prop-get-inverse-matrix ((shape edraw-shape))
  (if-let ((mat (edraw-transform-prop-get-matrix shape)))
      (edraw-matrix-inverse mat)
    (edraw-matrix)))

(cl-defmethod edraw-transform-prop-multiply ((shape edraw-shape) matrix)
  (unless (edraw-matrix-identity-p matrix)
    (let* ((old-mat (edraw-transform-prop-get-matrix shape))
           (new-mat (edraw-matrix-mul-mat-mat matrix old-mat)))
      (edraw-set-property shape
                          'transform
                          (edraw-svg-transform-from-matrix new-mat)))))

(cl-defmethod edraw-transform-prop-translate ((shape edraw-shape) xy)
  (when (and xy (not (edraw-xy-zero-p xy)))
    (let ((mat (or (edraw-transform-prop-get-matrix shape)
                   (edraw-matrix))))
      (edraw-matrix-translate-add mat (car xy) (cdr xy))
      (edraw-set-property shape
                          'transform
                          (edraw-svg-transform-from-matrix mat)))))



;;;;;; Implemented in Derived Classes
;;(cl-defmethod edraw-get-anchor-points ((shape edraw-shape-*)) )
;;(cl-defmethod edraw-shape-type (edraw-shape-*) )



;;;;; Shape - Rect Boundary

(defclass edraw-shape-with-rect-boundary (edraw-shape)
  ((anchor-points :initform nil)
   (p0p1)) ;;Note that p0(car p0p1) is not always in the upper left
  :abstract t)

(cl-defgeneric edraw-get-rect-from-element
    ((shape edraw-shape-with-rect-boundary)))

(cl-defmethod edraw-make-anchor-points-from-element
  ((shape edraw-shape-with-rect-boundary))
  (with-slots (element anchor-points p0p1) shape
    (when (null anchor-points)
      (setq p0p1 (edraw-get-rect-from-element shape))
      (setq anchor-points
            (list
             ;; Corners
             (edraw-shape-point-rect-boundary :shape shape :ref-x 0 :ref-y 0)
             (edraw-shape-point-rect-boundary :shape shape :ref-x 1 :ref-y 0)
             (edraw-shape-point-rect-boundary :shape shape :ref-x 1 :ref-y 1)
             (edraw-shape-point-rect-boundary :shape shape :ref-x 0 :ref-y 1)
             ;; Sides
             (edraw-shape-point-rect-boundary :shape shape :ref-x 0 :ref-y nil)
             (edraw-shape-point-rect-boundary :shape shape :ref-x 1 :ref-y nil)
             (edraw-shape-point-rect-boundary :shape shape :ref-x nil :ref-y 0)
             (edraw-shape-point-rect-boundary :shape shape :ref-x nil :ref-y 1)
             )))
    anchor-points))

(cl-defmethod edraw-get-anchor-points ((shape edraw-shape-with-rect-boundary))
  (edraw-make-anchor-points-from-element shape)
  (with-slots (anchor-points) shape
    anchor-points))

(cl-defmethod edraw-set-anchor-position ((shape edraw-shape-with-rect-boundary)
                                         anchor
                                         xy)
  (with-slots (anchor-points p0p1) shape
    (let* ((ref-x (edraw-ref-x anchor))
           (ref-y (edraw-ref-y anchor))
           (x (car xy))
           (y (cdr xy))
           (px (pcase ref-x (0 (car p0p1)) (1 (cdr p0p1))))
           (py (pcase ref-y (0 (car p0p1)) (1 (cdr p0p1))))
           (changed (or (and px (/= (car px) x))
                        (and py (/= (cdr py) y)))))
      (when changed
        (when px (setcar px x))
        (when py (setcdr py y))
        (edraw-on-anchor-position-changed shape)))))

(cl-defmethod edraw-get-anchor-position ((shape edraw-shape-with-rect-boundary)
                                         anchor)
  (with-slots (p0p1) shape
    (let* ((ref-x (edraw-ref-x anchor))
           (ref-y (edraw-ref-y anchor))
           (px (pcase ref-x (0 (car p0p1)) (1 (cdr p0p1))))
           (py (pcase ref-y (0 (car p0p1)) (1 (cdr p0p1)))))
      (cons
       (if px (car px) (/ (+ (caar p0p1) (cadr p0p1)) 2))
       (if py (cdr py) (/ (+ (cdar p0p1) (cddr p0p1)) 2))))))

(cl-defmethod edraw-set-rect ((shape edraw-shape-with-rect-boundary) xy0 xy1)
  (edraw-make-anchor-points-from-element shape) ;;Make sure p0p1 is initialized
  ;;@todo 
  (with-slots (p0p1) shape
    (when (or (/= (caar p0p1) (car xy0))
              (/= (cdar p0p1) (cdr xy0))
              (/= (cadr p0p1) (car xy1))
              (/= (cddr p0p1) (cdr xy1)))
      ;;changed
      (setcar (car p0p1) (car xy0))
      (setcdr (car p0p1) (cdr xy0))
      (setcar (cdr p0p1) (car xy1))
      (setcdr (cdr p0p1) (cdr xy1))
      (edraw-on-anchor-position-changed shape))))

(cl-defmethod edraw-get-rect ((shape edraw-shape-with-rect-boundary))
  (edraw-make-anchor-points-from-element shape) ;;Make sure p0p1 is initialized
  (with-slots (p0p1) shape
    (edraw-aabb (car p0p1) (cdr p0p1))))

(cl-defmethod edraw-translate ((shape edraw-shape-with-rect-boundary) xy)
  (if (edraw-transform-prop-exists-p shape)
      (edraw-transform-prop-translate shape xy)
    (edraw-make-anchor-points-from-element shape);;Make sure p0p1 is initialized
    (with-slots (p0p1) shape
      (when (and xy (not (edraw-xy-zero-p xy)))
        (edraw-set-rect shape
                        (cons
                         (+ (caar p0p1) (car xy))
                         (+ (cdar p0p1) (cdr xy)))
                        (cons
                         (+ (cadr p0p1) (car xy))
                         (+ (cddr p0p1) (cdr xy))))
        ;;(edraw-on-shape-changed shape 'shape-translate) ?
        ))))

(cl-defmethod edraw-transform ((shape edraw-shape-with-rect-boundary) matrix)
  (edraw-make-anchor-points-from-element shape) ;;Make sure p0p1 is initialized
  (with-slots (p0p1) shape
    (let ((new-p0 (edraw-matrix-mul-mat-xy matrix (car p0p1)))
          (new-p1 (edraw-matrix-mul-mat-xy matrix (cdr p0p1))))
      (edraw-set-rect shape new-p0 new-p1)
      ;;(edraw-on-shape-changed shape 'shape-transform) ?
      )))

;;;;;; Implemented in Derived Classes
;;(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-*))



;;;;; Shape - Rect

(defun edraw-shape-rect-create (element editor)
  (let ((shape (edraw-shape-rect)))
    (oset shape element element)
    (oset shape editor editor)
    shape))

(defclass edraw-shape-rect (edraw-shape-with-rect-boundary)
  ())

(cl-defmethod edraw-shape-type ((_shape edraw-shape-rect))
  'rect)

(cl-defmethod edraw-get-rect-from-element ((shape edraw-shape-rect))
  (with-slots (element) shape
    (let ((x (or (edraw-svg-attr-coord element 'x) 0))
          (y (or (edraw-svg-attr-coord element 'y) 0))
          (width (or (edraw-svg-attr-length element 'width) 0))
          (height (or (edraw-svg-attr-length element 'height) 0)))
      (cons (cons x y)
            (cons (+ x width) (+ y height))))))

(cl-defmethod edraw-push-undo-translate ((shape edraw-shape-rect))
  (edraw-push-undo-properties shape
                              'shape-rect-anchors
                              (if (edraw-transform-prop-exists-p shape)
                                  '(transform)
                                '(x y width height))))

(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-rect))
  (with-slots (element p0p1) shape
    (edraw-push-undo-properties shape 'shape-rect-anchor '(x y width height))
    (edraw-merge-set-properties-undo-data (edraw-undo-list (oref shape editor)) nil 'shape-rect-anchor)
    (edraw-svg-rect-set-range element (car p0p1) (cdr p0p1))
    (edraw-on-shape-changed shape 'anchor-position)))

(cl-defmethod edraw-set-properties ((shape edraw-shape-rect) prop-list)
  (let ((undo-list-end (edraw-undo-list (oref shape editor)))
        (changed nil))
    ;; apply x= y= width= height= properties
    (let* ((rect (edraw-get-rect shape))
           (old-x (caar rect))
           (old-y (cdar rect))
           (old-w (- (cadr rect) (caar rect)))
           (old-h (- (cddr rect) (cdar rect)))
           (new-x (edraw-alist-get-as-number 'x prop-list old-x))
           (new-y (edraw-alist-get-as-number 'y prop-list old-y))
           (new-w (edraw-alist-get-as-number 'width prop-list old-w))
           (new-h (edraw-alist-get-as-number 'height prop-list old-h)))
      (when (or (/= new-x old-x)
                (/= new-y old-y)
                (/= new-w old-w)
                (/= new-h old-h))
        (setq changed t)
        (edraw-set-rect shape
                        (cons new-x new-y)
                        (cons (+ new-x new-w) (+ new-y new-h)))))
    (setf (alist-get 'x prop-list nil 'remove) nil)
    (setf (alist-get 'y prop-list nil 'remove) nil)
    (setf (alist-get 'width prop-list nil 'remove) nil)
    (setf (alist-get 'height prop-list nil 'remove) nil)
    ;; other properties
    (edraw-set-properties-internal shape prop-list undo-list-end changed)))



;;;;; Shape - Ellipse

(defun edraw-shape-ellipse-create (element editor)
  (let ((shape (edraw-shape-ellipse)))
    (oset shape element element)
    (oset shape editor editor)
    shape))

(defclass edraw-shape-ellipse (edraw-shape-with-rect-boundary)
  ())

(cl-defmethod edraw-shape-type ((_shape edraw-shape-ellipse))
  'ellipse)

(cl-defmethod edraw-get-rect-from-element ((shape edraw-shape-ellipse))
  (with-slots (element) shape
    (let ((cx (or (edraw-svg-attr-coord element 'cx) 0))
          (cy (or (edraw-svg-attr-coord element 'cy) 0))
          (rx (or (edraw-svg-attr-length element 'rx) 0))
          (ry (or (edraw-svg-attr-length element 'ry) 0)))
      (cons (cons (- cx rx) (- cy ry))
            (cons (+ cx rx) (+ cy ry))))))

(cl-defmethod edraw-push-undo-translate ((shape edraw-shape-ellipse))
  (edraw-push-undo-properties shape
                              'shape-ellipse-anchors
                              (if (edraw-transform-prop-exists-p shape)
                                  '(transform)
                                '(cx cy rx ry))))

(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-ellipse))
  (with-slots (element p0p1) shape
    (edraw-push-undo-properties shape 'shape-ellipse-anchor '(cx cy rx ry))
    (edraw-merge-set-properties-undo-data (edraw-undo-list (oref shape editor)) nil 'shape-ellipse-anchor)
    (edraw-svg-ellipse-set-range element (car p0p1) (cdr p0p1))
    (edraw-on-shape-changed shape 'anchor-position)))

(cl-defmethod edraw-set-properties ((shape edraw-shape-ellipse) prop-list)
  (let ((undo-list-end (edraw-undo-list (oref shape editor)))
        (changed nil))
    ;; apply cx= cy= rx= ry= properties
    (let* ((rect (edraw-get-rect shape))
           (old-x (caar rect))
           (old-y (cdar rect))
           (old-w (- (cadr rect) (caar rect)))
           (old-h (- (cddr rect) (cdar rect)))
           (cx (edraw-alist-get-as-number 'cx prop-list (+ old-x (* 0.5 old-w))))
           (cy (edraw-alist-get-as-number 'cy prop-list (+ old-y (* 0.5 old-h))))
           (rx (edraw-alist-get-as-number 'rx prop-list (* 0.5 old-w)))
           (ry (edraw-alist-get-as-number 'ry prop-list (* 0.5 old-h)))
           (new-w (* 2.0 rx))
           (new-h (* 2.0 ry))
           (new-x (- cx rx))
           (new-y (- cy ry)))
      (when (or (/= new-x old-x)
                (/= new-y old-y)
                (/= new-w old-w)
                (/= new-h old-h))
        (setq changed t)
        (edraw-set-rect shape
                        (cons new-x new-y)
                        (cons (+ new-x new-w) (+ new-y new-h)))))
    (setf (alist-get 'cx prop-list nil 'remove) nil)
    (setf (alist-get 'cy prop-list nil 'remove) nil)
    (setf (alist-get 'rx prop-list nil 'remove) nil)
    (setf (alist-get 'ry prop-list nil 'remove) nil)
    ;; other properties
    (edraw-set-properties-internal shape prop-list undo-list-end changed)))



;;;;; Shape - Circle

(defun edraw-shape-circle-create (element editor)
  (let ((shape (edraw-shape-circle)))
    (oset shape element element)
    (oset shape editor editor)
    shape))

(defclass edraw-shape-circle (edraw-shape-with-rect-boundary)
  ())

(cl-defmethod edraw-shape-type ((_shape edraw-shape-circle))
  'circle)

(cl-defmethod edraw-get-rect-from-element ((shape edraw-shape-circle))
  (with-slots (element) shape
    (let ((cx (or (edraw-svg-attr-coord element 'cx) 0))
          (cy (or (edraw-svg-attr-coord element 'cy) 0))
          (r (or (edraw-svg-attr-length element 'r) 0)))
      (cons (cons (- cx r) (- cy r))
            (cons (+ cx r) (+ cy r))))))

(cl-defmethod edraw-push-undo-translate ((shape edraw-shape-circle))
  (edraw-push-undo-properties shape
                              'shape-circle-anchors
                              (if (edraw-transform-prop-exists-p shape)
                                  '(transform)
                                '(cx cy r))))

(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-circle))
  (with-slots (element p0p1) shape
    (edraw-push-undo-properties shape 'shape-circle-anchor '(cx cy r))
    (edraw-merge-set-properties-undo-data (edraw-undo-list (oref shape editor)) nil 'shape-circle-anchor)
    (let ((p0 (car p0p1))
          (p1 (cdr p0p1)))
      (dom-set-attribute element 'cx (* 0.5 (+ (car p0) (car p1))))
      (dom-set-attribute element 'cy (* 0.5 (+ (cdr p0) (cdr p1))))
      (dom-set-attribute element 'r (max (* 0.5 (abs (- (car p0) (car p1))))
                                         (* 0.5 (abs (- (cdr p0) (cdr p1)))))))
    (edraw-on-shape-changed shape 'anchor-position)))

(cl-defmethod edraw-set-anchor-position ((shape edraw-shape-circle)
                                         anchor
                                         xy)
  (with-slots (anchor-points p0p1) shape
    (let* ((ref-x (edraw-ref-x anchor))
           (ref-y (edraw-ref-y anchor))
           (x (car xy))
           (y (cdr xy))
           (px (pcase ref-x (0 (car p0p1)) (1 (cdr p0p1))))
           (py (pcase ref-y (0 (car p0p1)) (1 (cdr p0p1))))
           (ox (pcase ref-x (1 (car p0p1)) (0 (cdr p0p1))))
           (oy (pcase ref-y (1 (car p0p1)) (0 (cdr p0p1))))
           (new-rx (if ox (* 0.5 (abs (- (car ox) x)))))
           (new-ry (if oy (* 0.5 (abs (- (cdr oy) y)))))
           (cx (* 0.5 (+ (caar p0p1) (cadr p0p1))))
           (cy (* 0.5 (+ (cdar p0p1) (cddr p0p1))))
           (changed nil))
      (cond
       ((and px (null py))
        (when (/= x (car px))
          (setcar px x)
          (setcdr (car p0p1) (- cy new-rx))
          (setcdr (cdr p0p1) (+ cy new-rx))
          (setq changed t)))
       ((and py (null px))
        (when (/= y (cdr py))
          (setcdr py y)
          (setcar (car p0p1) (- cx new-ry))
          (setcar (cdr p0p1) (+ cx new-ry))
          (setq changed t)))
       ((and px py)
        (if (< new-rx new-ry)
            (when (/= y (cdr py))
              (setcdr py y)
              (setcar px (if (< x (car ox)) (- (car ox) (* 2 new-ry)) (+ (car ox) (* 2 new-ry))))
              (setq changed t))
          (when (/= x (car px))
            (setcar px x)
            (setcdr py (if (< y (cdr oy)) (- (cdr oy) (* 2 new-rx)) (+ (cdr oy) (* 2 new-rx))))
            (setq changed t)))))

      (when changed
        (edraw-on-anchor-position-changed shape)))))

(cl-defmethod edraw-set-properties ((shape edraw-shape-circle) prop-list)
  (let ((undo-list-end (edraw-undo-list (oref shape editor)))
        (changed nil))
    ;; apply cx= cy= r= properties
    (let* ((rect (edraw-get-rect shape))
           (old-x (caar rect))
           (old-y (cdar rect))
           (old-w (- (cadr rect) (caar rect)))
           (old-h (- (cddr rect) (cdar rect)))
           (cx (edraw-alist-get-as-number 'cx prop-list (+ old-x (* 0.5 old-w))))
           (cy (edraw-alist-get-as-number 'cy prop-list (+ old-y (* 0.5 old-h))))
           (r (edraw-alist-get-as-number 'r prop-list (* 0.5 (max old-w old-h))))
           (new-w (* 2.0 r))
           (new-h (* 2.0 r))
           (new-x (- cx r))
           (new-y (- cy r)))
      (when (or (/= new-x old-x)
                (/= new-y old-y)
                (/= new-w old-w)
                (/= new-h old-h))
        (setq changed t)
        (edraw-set-rect shape
                        (cons new-x new-y)
                        (cons (+ new-x new-w) (+ new-y new-h)))))
    (setf (alist-get 'cx prop-list nil 'remove) nil)
    (setf (alist-get 'cy prop-list nil 'remove) nil)
    (setf (alist-get 'r prop-list nil 'remove) nil)
    ;; other properties
    (edraw-set-properties-internal shape prop-list undo-list-end changed)))



;;;;; Shape - Text

(defun edraw-shape-text-create (element editor)
  (let ((shape (edraw-shape-text)))
    (oset shape element element)
    (oset shape editor editor)
    (oset shape anchor-points (list (edraw-shape-point-text :shape shape)))
    shape))

(defclass edraw-shape-text (edraw-shape)
  ((anchor-points)))

(cl-defmethod edraw-shape-type ((_shape edraw-shape-text))
  'text)

(cl-defmethod edraw-get-anchor-points ((shape edraw-shape-text))
  (oref shape anchor-points))

(cl-defmethod edraw-get-anchor-position ((shape edraw-shape-text))
  (with-slots (element) shape
    (cons
     (or (edraw-svg-attr-coord element 'x) 0)
     (or (edraw-svg-attr-coord element 'y) 0))))

(cl-defmethod edraw-set-anchor-position ((shape edraw-shape-text) xy)
  (with-slots (element) shape
    (when (or (/= (car xy) (or (edraw-svg-attr-coord element 'x) 0))
              (/= (cdr xy) (or (edraw-svg-attr-coord element 'y) 0)))
      (edraw-push-undo-properties shape 'shape-text-anchor '(x y))
      (edraw-merge-set-properties-undo-data (edraw-undo-list (oref shape editor)) nil 'shape-text-anchor nil t)
      (edraw-svg-text-set-xy element xy)
      (edraw-on-shape-changed shape 'anchor-position))))

(cl-defmethod edraw-push-undo-translate ((shape edraw-shape-text))
  (edraw-push-undo-properties shape
                              'shape-text-anchors
                              (if (edraw-transform-prop-exists-p shape)
                                  '(transform)
                                '(x y))))

(cl-defmethod edraw-translate ((shape edraw-shape-text) xy)
  (if (edraw-transform-prop-exists-p shape)
      (edraw-transform-prop-translate shape xy)
    (when (and xy (not (edraw-xy-zero-p xy)))
      (with-slots (element) shape
        (edraw-push-undo-properties shape 'shape-text-anchor '(x y))
        (edraw-merge-set-properties-undo-data (edraw-undo-list (oref shape editor)) nil 'shape-text-anchor nil t)
        (edraw-svg-element-translate element xy)
        (edraw-on-shape-changed shape 'translate)))))

(cl-defmethod edraw-transform ((shape edraw-shape-text) matrix)
  (with-slots (element) shape
    (let* ((xy (cons (or (edraw-svg-attr-coord element 'x) 0)
                     (or (edraw-svg-attr-coord element 'y) 0)))
           (new-xy (edraw-matrix-mul-mat-xy matrix xy)))
      (edraw-push-undo-properties shape 'shape-text-anchor '(x y))
      (edraw-svg-text-set-xy element new-xy)
      (edraw-on-shape-changed shape 'shape-transform)))) ;;or anchor-position?



;;;;; Shape - Path

(defun edraw-shape-path-create (element editor)
  (let ((shape (edraw-shape-path))
        (d (dom-attr element 'd)))
    (oset shape element element)
    (oset shape editor editor)
    (oset shape cmdlist (or (and d
                                 (edraw-path-cmdlist-from-d d))
                            (edraw-path-cmdlist)))
    shape))

(defclass edraw-shape-path (edraw-shape)
  ((cmdlist)))

(cl-defmethod edraw-shape-type ((_shape edraw-shape-path))
  'path)

(cl-defmethod edraw-get-property ((shape edraw-shape-path) prop-name)
  (if (eq prop-name 'd)
      (edraw-path-cmdlist-to-string (oref shape cmdlist))
    (cl-call-next-method)))

(cl-defmethod edraw-set-properties ((shape edraw-shape-path) prop-list)
  (let ((undo-list-end (edraw-undo-list (oref shape editor)))
        (changed nil))
    (when-let ((d-cell (assq 'd prop-list)))
      (let ((d (or (cdr d-cell) "")))
        (with-slots (cmdlist) shape
          (unless (string= d (edraw-path-cmdlist-to-string cmdlist))
            (setq changed t)
            ;;(message "%s => %s" (edraw-path-cmdlist-to-string cmdlist) d)
            (edraw-path-cmdlist-swap cmdlist (edraw-path-cmdlist-from-d d))
            (edraw-push-undo-properties shape 'shape-path-d '(d))
            (edraw-update-path-data shape)
            (edraw-on-shape-changed shape 'shape-path-data))))
      (setf (alist-get 'd prop-list nil 'remove) nil))
    ;; other properties
    (edraw-set-properties-internal shape prop-list undo-list-end changed)))

(cl-defmethod edraw-get-actions ((shape edraw-shape-path))
  (let* ((items (copy-tree (cl-call-next-method)))
         (item-set (seq-find (lambda (item) (equal (car item)
                                                   '(edraw-msg "Set")))
                             items)))

    (when item-set
      (nconc (cadr item-set)
             `(((edraw-msg "Start Marker")
                (((edraw-msg "None") edraw-set-marker-start-none
                  :button (:toggle . ,(null (edraw-get-property shape 'marker-start))))
                 ((edraw-msg "Arrow") edraw-set-marker-start-arrow
                  :button (:toggle . ,(equal (edraw-get-property shape 'marker-start) "arrow")))
                 ((edraw-msg "Circle") edraw-set-marker-start-circle
                  :button (:toggle . ,(equal (edraw-get-property shape 'marker-start) "circle")))))
               ((edraw-msg "End Marker")
                (((edraw-msg "None") edraw-set-marker-end-none
                  :button (:toggle . ,(null (edraw-get-property shape 'marker-end))))
                 ((edraw-msg "Arrow") edraw-set-marker-end-arrow
                  :button (:toggle . ,(equal (edraw-get-property shape 'marker-end) "arrow")))
                 ((edraw-msg "Circle") edraw-set-marker-end-circle
                  :button (:toggle . ,(equal (edraw-get-property shape 'marker-end) "circle"))))))))

    (append
     items
     `(((edraw-msg "Close Path") edraw-close-path-shape
        :enable ,(edraw-closable-path-shape-p shape))
       ((edraw-msg "Open Path") edraw-open-path-shape
        :enable ,(edraw-closed-path-shape-p shape))
       ((edraw-msg "Reverse Path Direction") edraw-reverse-path)
       ))))

(cl-defmethod edraw-set-marker-start-none ((shape edraw-shape-path))
  (edraw-set-marker shape 'marker-start nil))
(cl-defmethod edraw-set-marker-start-arrow ((shape edraw-shape-path))
  (edraw-set-marker shape 'marker-start "arrow"))
(cl-defmethod edraw-set-marker-start-circle ((shape edraw-shape-path))
  (edraw-set-marker shape 'marker-start "circle"))
(cl-defmethod edraw-set-marker-end-none ((shape edraw-shape-path))
  (edraw-set-marker shape 'marker-end nil))
(cl-defmethod edraw-set-marker-end-arrow ((shape edraw-shape-path))
  (edraw-set-marker shape 'marker-end "arrow"))
(cl-defmethod edraw-set-marker-end-circle ((shape edraw-shape-path))
  (edraw-set-marker shape 'marker-end "circle"))
(cl-defmethod edraw-set-marker ((shape edraw-shape-path) prop-name type)
  (edraw-set-properties shape (list (cons prop-name type))))

(cl-defmethod edraw-push-undo-translate ((shape edraw-shape-path))
  (edraw-push-undo-properties shape
                              'shape-path-anchors
                              (if (edraw-transform-prop-exists-p shape)
                                  '(transform)
                                '(d))))

(cl-defmethod edraw-translate ((shape edraw-shape-path) xy)
  (if (edraw-transform-prop-exists-p shape)
      (edraw-transform-prop-translate shape xy)
    (when (and xy (not (edraw-xy-zero-p xy)))
      (with-slots (cmdlist) shape
        (edraw-path-cmdlist-translate cmdlist xy))
      (edraw-push-undo-properties shape 'shape-path-translate '(d))
      (edraw-merge-set-properties-undo-data
       (edraw-undo-list (oref shape editor))
       (cddr (edraw-undo-list (oref shape editor)))
       'shape-path-translate
       nil t)
      (edraw-update-path-data shape)
      (edraw-on-shape-changed shape 'shape-translate))))

(cl-defmethod edraw-transform ((shape edraw-shape-path) matrix)
  (with-slots (cmdlist) shape
    (edraw-path-cmdlist-transform cmdlist matrix))
  (edraw-push-undo-properties shape 'shape-path-transform '(d))
  (edraw-update-path-data shape)
  (edraw-on-shape-changed shape 'shape-transform))

(cl-defmethod edraw-get-anchor-points ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (let (points)
      (edraw-path-cmdlist-loop cmdlist cmd
        (when-let ((ppoint-anchor (edraw-path-cmd-anchor-point cmd nil)))
          (push (edraw-shape-point-path
                 :shape shape
                 :ppoint ppoint-anchor)
                points)))
      (nreverse points))))

(cl-defmethod edraw-pick-point ((shape edraw-shape-path) xy)
  (with-slots (cmdlist) shape
    (when-let ((ppoint (edraw-path-cmdlist-pick-point
                        cmdlist
                        xy
                        edraw-anchor-point-input-radius
                        edraw-handle-point-input-radius)))
      (edraw-shape-point-path
       :shape shape
       :ppoint ppoint))))

;; (cl-defmethod edraw-owned-shape-point-p ((shape edraw-shape-path) spt)
;;   (with-slots (cmdlist) shape
;;   ))

(cl-defmethod edraw-get-nth-point ((shape edraw-shape-path) index)
  (with-slots (cmdlist) shape
    (when-let (ppoint (edraw-path-cmdlist-nth-point cmdlist index))
      (edraw-shape-point-path
       :shape shape
       :ppoint ppoint))))

(cl-defmethod edraw-get-first-anchor-point ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (when-let ((first-ppoint (edraw-path-cmdlist-first-anchor-point cmdlist)))
      (edraw-shape-point-path
       :shape shape
       :ppoint first-ppoint))))

(cl-defmethod edraw-get-last-anchor-point ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (when-let ((last-ppoint (edraw-path-cmdlist-last-anchor-point cmdlist)))
      (edraw-shape-point-path
       :shape shape
       :ppoint last-ppoint))))

(cl-defmethod edraw-add-anchor-point ((shape edraw-shape-path) xy)
  (with-slots (cmdlist) shape
    (let ((anchor-point (edraw-path-cmdlist-add-anchor-point cmdlist xy)))

      (edraw-push-undo-properties shape 'shape-path-transform '(d))
      (edraw-update-path-data shape)
      (edraw-on-shape-changed shape 'anchor-add)

      ;; Return a new shape point
      (edraw-shape-point-path
       :shape shape
       :ppoint anchor-point))))

(cl-defmethod edraw-update-path-data ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (dom-set-attribute (edraw-element shape)
                       'd (edraw-path-cmdlist-to-string cmdlist))
    ;; The caller calls (edraw-on-shape-changed shape)
    ))

(cl-defmethod edraw-on-shape-point-changed ((shape edraw-shape-path) type)
  (edraw-update-path-data shape)
  (edraw-on-shape-changed shape type))

(cl-defmethod edraw-closed-path-shape-p ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (not (null (edraw-path-cmdlist-closed-p cmdlist)))))

(cl-defmethod edraw-closable-path-shape-p ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (not (null (edraw-path-cmdlist-closable-p cmdlist)))))

(cl-defmethod edraw-close-path-shape ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (when (edraw-path-cmdlist-close-path cmdlist)
      (edraw-push-undo-properties shape 'shape-path-close '(d))
      (edraw-update-path-data shape)
      (edraw-on-shape-changed shape 'shape-close-path)
      t)))

(cl-defmethod edraw-open-path-shape ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (when (edraw-path-cmdlist-open-path cmdlist)
      (edraw-push-undo-properties shape 'shape-path-open '(d))
      (edraw-update-path-data shape)
      (edraw-on-shape-changed shape 'shape-open-path)
      t)))

(cl-defmethod edraw-reverse-path ((shape edraw-shape-path))
  "Reverse the order of anchor points in the path."
  (with-slots (cmdlist) shape
    (edraw-path-cmdlist-reverse cmdlist)
    (edraw-push-undo-properties shape 'shape-path-reverse '(d))
    (edraw-update-path-data shape)
    (edraw-on-shape-changed shape 'shape-reverse-path)
    t))

(cl-defmethod edraw-connect-path-to-anchor ((src-shape edraw-shape-path) dst-anchor)
  (with-slots ((src-cmdlist cmdlist)) src-shape
    (let ((dst-ppoint (edraw-shape-point-path-ppoint dst-anchor))
          (dst-shape (edraw-parent-shape dst-anchor)))
      (with-slots ((dst-cmdlist cmdlist)) dst-shape

        (when (and (not (eq dst-shape src-shape))
                   (not (edraw-path-anchor-in-closed-subpath-p dst-ppoint))
                   (or (edraw-path-anchor-first-p dst-ppoint)
                       (and (edraw-path-anchor-last-p dst-ppoint)
                            (edraw-path-cmdlist-reverse dst-cmdlist))))
          (edraw-make-undo-group
           (oref src-shape editor) 'connect-path-to-anchor
           (edraw-path-cmdlist-connect-cmdlist-front dst-cmdlist src-cmdlist)

           (edraw-push-undo-properties src-shape 'shape-append-path '(d))
           (edraw-push-undo-properties dst-shape 'shape-append-path '(d))
           ;; Update dst
           (edraw-update-path-data dst-shape)
           (edraw-on-shape-changed dst-shape 'shape-append-path)
           ;; Remove src
           (edraw-remove src-shape)
           t))))))



;;;;; Shape - Group

(defun edraw-shape-group-create (element editor)
  (let ((shape (edraw-shape-group)))
    (oset shape element element)
    (oset shape editor editor)
    shape))

(defclass edraw-shape-group (edraw-shape-with-rect-boundary)
  ())

(cl-defmethod edraw-shape-type ((_shape edraw-shape-group))
  'g)

(cl-defmethod edraw-get-anchor-points ((_shape edraw-shape-group))
  nil)

(cl-defmethod edraw-get-anchor-position ((_shape edraw-shape-group))
  nil)

(cl-defmethod edraw-set-anchor-position ((_shape edraw-shape-group) _xy)
  nil)

(cl-defmethod edraw-push-undo-translate ((shape edraw-shape-group))
  (edraw-push-undo-properties shape 'shape-rect-anchors '(transform)))

(cl-defmethod edraw-translate ((shape edraw-shape-group) xy)
  (edraw-transform-prop-translate shape xy))

(cl-defmethod edraw-transform ((shape edraw-shape-group) matrix)
  (with-slots (element) shape
    (edraw-push-undo-properties shape 'shape-group-transform '(transform))
    (edraw-svg-element-transform-multiply element matrix)
    (edraw-on-shape-changed shape 'shape-transform)))

(cl-defmethod edraw-shape-group-add-children (group children)
  ;; sort children by z-order
  (setq children
        (sort (copy-sequence children)
              (lambda (a b)
                (< (edraw-node-position a)
                   (edraw-node-position b)))))

  (with-slots (editor) group
    (edraw-make-undo-group editor 'add-shapes-to-group
      ;; remove children
      (dolist (child children)
        (edraw-remove child))
      ;; add children
      (dolist (child children)
        (edraw-insert child nil group)))))


;;;; Shape Point

;;
;; - Control points for changing shape
;; - By manipulating (e.g. moveing) a shape point, the related shape changes
;; - Usually either anchor point or handle point
;; - Shape point objects can be obtained from shape object
;;

;;;;; Shape Point - Point Set

(defun edraw-shape-point-find (point-list xy)
  (seq-find (lambda (spt) (edraw-hit-input-p spt xy))
            point-list))

;;;;; Shape Point - Base Class

(defclass edraw-shape-point ()
  ()
  :abstract t)

(cl-defmethod edraw-previous-anchor ((_spt edraw-shape-point))
  nil)

(cl-defmethod edraw-next-anchor ((_spt edraw-shape-point))
  nil)

(cl-defmethod edraw-get-handle-points ((_spt edraw-shape-point))
  nil)

(cl-defmethod edraw-get-actions ((_spt edraw-shape-point))
  nil)

(cl-defmethod edraw-anchor-p ((spt edraw-shape-point))
  (eq (edraw-get-point-type spt) 'anchor))

(cl-defmethod edraw-handle-p ((spt edraw-shape-point))
  (eq (edraw-get-point-type spt) 'handle))

(cl-defmethod edraw-hit-input-p ((spt edraw-shape-point) xy)
  "Returns non-nil, if the point SPT hits the pointer input(e.g. click) point XY."
  (pcase (edraw-get-point-type spt)
    ('anchor (<= (edraw-xy-distance-l-inf (edraw-get-xy-transformed spt) xy) ;;square
                 edraw-anchor-point-input-radius))
    ('handle (<= (edraw-xy-distance-squared (edraw-get-xy-transformed spt) xy) ;;circle
                 (* edraw-handle-point-input-radius
                    edraw-handle-point-input-radius)))))

(cl-defmethod edraw-get-xy-transformed ((spt edraw-shape-point))
  (when-let ((shape (edraw-parent-shape spt)))
    (edraw-matrix-mul-mat-xy
     (edraw-transform-prop-get-matrix shape)
     (edraw-get-xy spt))))

(cl-defmethod edraw-move-on-transformed ((spt edraw-shape-point)
                                         xy)
  (when-let ((shape (edraw-parent-shape spt))
             (inv-mat (edraw-transform-prop-get-inverse-matrix shape)))
    (edraw-move spt (edraw-matrix-mul-mat-xy inv-mat xy))))


;;;;; Shape Point - Rect Boundary

(defclass edraw-shape-point-rect-boundary (edraw-shape-point)
  ((shape :initarg :shape :type edraw-shape-with-rect-boundary)
   (ref-x :initarg :ref-x :reader edraw-ref-x)
   (ref-y :initarg :ref-y :reader edraw-ref-y)))
(cl-defmethod edraw-get-point-type ((_spt edraw-shape-point-rect-boundary))
  'anchor)
(cl-defmethod edraw-parent-shape ((spt edraw-shape-point-rect-boundary))
  (oref spt shape))
(cl-defmethod edraw-parent-anchor ((_spt edraw-shape-point-rect-boundary))
  nil)
(cl-defmethod edraw-get-xy ((spt edraw-shape-point-rect-boundary))
  (edraw-get-anchor-position (oref spt shape) spt))
(cl-defmethod edraw-move ((spt edraw-shape-point-rect-boundary) xy)
  (edraw-set-anchor-position (oref spt shape) spt xy))
(cl-defmethod edraw-same-point-p ((spt1 edraw-shape-point-rect-boundary) spt2)
  (eq spt1 spt2))

;;;;; Shape Point - Text

(defclass edraw-shape-point-text (edraw-shape-point)
  ((shape :initarg :shape :type edraw-shape-text)))
(cl-defmethod edraw-get-point-type ((_spt edraw-shape-point-text))
  'anchor)
(cl-defmethod edraw-parent-shape ((spt edraw-shape-point-text))
  (oref spt shape))
(cl-defmethod edraw-parent-anchor ((_spt edraw-shape-point-text))
  nil)
(cl-defmethod edraw-get-xy ((spt edraw-shape-point-text))
  (edraw-get-anchor-position (oref spt shape)))
(cl-defmethod edraw-move ((spt edraw-shape-point-text) xy)
  (edraw-set-anchor-position (oref spt shape) xy))
(cl-defmethod edraw-same-point-p ((spt1 edraw-shape-point-text) spt2)
  (eq spt1 spt2))

;;;;; Shape Point - Path

(defclass edraw-shape-point-path (edraw-shape-point)
  ((shape :initarg :shape :type edraw-shape-path)
   (ppoint :initarg :ppoint :reader edraw-shape-point-path-ppoint)))

(cl-defmethod edraw-get-point-type ((spt edraw-shape-point-path))
  (with-slots (ppoint) spt
    (edraw-path-point-type ppoint)))

(cl-defmethod edraw-parent-shape ((spt edraw-shape-point-path))
  (oref spt shape))

(cl-defmethod edraw-parent-anchor ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (when-let ((result-point (edraw-path-handle-parent-anchor ppoint)))
      (edraw-shape-point-path
       :shape shape
       :ppoint result-point))))

(cl-defmethod edraw-previous-anchor ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (when-let ((result-point (edraw-path-point-prev-anchor ppoint)))
      (edraw-shape-point-path
       :shape shape
       :ppoint result-point))))

(cl-defmethod edraw-next-anchor ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (when-let ((result-point (edraw-path-point-next-anchor ppoint)))
      (edraw-shape-point-path
       :shape shape
       :ppoint result-point))))

(cl-defmethod edraw-index-in-path ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (edraw-path-point-index-in-cmdlist ppoint)))

(cl-defmethod edraw-get-xy ((spt edraw-shape-point-path))
  (with-slots (ppoint) spt
    (edraw-xy-clone (edraw-path-point-xy ppoint))))

(cl-defmethod edraw-push-undo-path-point-change ((spt edraw-shape-point-path) type merge)
  (unless edraw-editor-inhibit-make-undo-data
    (with-slots (shape) spt
      (let ((type (intern (format "%s-p%s" type (edraw-index-in-path spt))))
            (prev-undo-data (car (edraw-undo-list (oref shape editor)))))
        ;;(message "type=%s len undo=%s" type (length (edraw-undo-list (oref shape editor))))
        (unless (and merge
                     (eq (nth 0 prev-undo-data) type)
                     (eq (nth 1 prev-undo-data) #'edraw-set-properties)
                     (eq (nth 2 prev-undo-data) shape))
          (edraw-push-undo-properties shape type '(d)))))))

(cl-defmethod edraw-move ((spt edraw-shape-point-path) xy)
  (with-slots (ppoint shape) spt
    (unless (edraw-xy-equal-p xy (edraw-path-point-xy ppoint))
      (edraw-path-point-move-with-related-points ppoint xy)
      (edraw-push-undo-path-point-change spt 'path-point-move t)
      (edraw-on-shape-point-changed shape 'point-move))))

(cl-defmethod edraw-move-with-opposite-handle ((spt edraw-shape-point-path)
                                               xy)
  (with-slots (ppoint shape) spt
    (unless (edraw-xy-equal-p xy (edraw-path-point-xy ppoint))
      (edraw-path-handle-move-with-opposite-handle ppoint xy)
      (edraw-push-undo-path-point-change spt 'path-point-move-with-opposite-handle t)
      (edraw-on-shape-point-changed shape 'point-move))))

(cl-defmethod edraw-move-with-opposite-handle-on-transformed ((spt edraw-shape-point-path) xy)
  (when-let ((shape (edraw-parent-shape spt))
             (inv-mat (edraw-transform-prop-get-inverse-matrix shape)))
    (edraw-move-with-opposite-handle
     spt (edraw-matrix-mul-mat-xy inv-mat xy))))

(cl-defmethod edraw-move-with-opposite-handle-symmetry ((spt edraw-shape-point-path) xy include-same-position-p)
  (with-slots (ppoint shape) spt
    (unless (edraw-xy-equal-p xy (edraw-path-point-xy ppoint))
      (edraw-path-handle-move-with-opposite-handle-symmetry ppoint xy include-same-position-p)
      (edraw-push-undo-path-point-change spt 'path-point-move-with-opposite-handle t)
      (edraw-on-shape-point-changed shape 'point-move))))

(cl-defmethod edraw-move-with-opposite-handle-symmetry-on-transformed ((spt edraw-shape-point-path) xy include-same-position-p)
  (when-let ((shape (edraw-parent-shape spt))
             (inv-mat (edraw-transform-prop-get-inverse-matrix shape)))
    (edraw-move-with-opposite-handle-symmetry
     spt (edraw-matrix-mul-mat-xy inv-mat xy) include-same-position-p)))

(cl-defmethod edraw-same-point-p ((spt1 edraw-shape-point-path) spt2)
  (and spt2
       (object-of-class-p spt2 'edraw-shape-point-path)
       (eq (oref spt1 shape) (oref spt2 shape))
       (eq (oref spt1 ppoint) (oref spt2 ppoint))))
       ;; (= (edraw-path-point-index-in-cmdlist (oref spt1 ppoint))
       ;;    (edraw-path-point-index-in-cmdlist (oref spt2 ppoint)))))

(cl-defmethod edraw-get-handle-points ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (delq nil
          (list
           (when-let ((pp1 (edraw-path-anchor-backward-handle ppoint)))
             (edraw-shape-point-path
              :shape shape
              :ppoint pp1))
           (when-let ((pp2 (edraw-path-anchor-forward-handle ppoint)))
             (edraw-shape-point-path
              :shape shape
              :ppoint pp2))))))

(cl-defmethod edraw-create-forward-handle ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (when (edraw-path-point-anchor-p ppoint)
      (when-let ((handle-ppoint (edraw-path-anchor-create-forward-handle ppoint)))
        (edraw-push-undo-path-point-change spt 'path-point-create-forward-handle nil)
        (edraw-on-shape-point-changed shape 'handle-create) ;;@todo check get or create?
        (edraw-shape-point-path
         :shape shape
         :ppoint handle-ppoint)))))

(cl-defmethod edraw-create-backward-handle ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (when (edraw-path-point-anchor-p ppoint)
      (when-let ((handle-ppoint (edraw-path-anchor-create-backward-handle ppoint)))
        (edraw-push-undo-path-point-change spt 'path-point-create-backward-handle nil)
        (edraw-on-shape-point-changed shape 'handle-create) ;;@todo check get or create?
        (edraw-shape-point-path
         :shape shape
         :ppoint handle-ppoint)))))

(cl-defmethod edraw-get-actions ((spt edraw-shape-point-path))
  (with-slots (ppoint) spt
    (let ((backward-handle (edraw-path-anchor-backward-handle ppoint))
          (forward-handle (edraw-path-anchor-forward-handle ppoint)))
      (cond
       ((edraw-path-point-anchor-p ppoint)
        `(((edraw-msg "Delete Point") edraw-delete-point)
          ((edraw-msg "Split Path at Point") edraw-split-path-at)
          ((edraw-msg "Insert Point Before") edraw-insert-point-before
           :enable ,(not (null (edraw-path-point-prev-anchor ppoint))))
          ((edraw-msg "Make Smooth") edraw-make-smooth)
          ((edraw-msg "Make Corner") edraw-make-corner
           :enable ,(or backward-handle forward-handle))))))))

(cl-defmethod edraw-delete-point ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (when (edraw-path-point-remove ppoint)
      ;; @todo if cmdline is empty or contains Z, M only
      (edraw-push-undo-path-point-change spt 'path-point-delete nil)
      (edraw-on-shape-point-changed shape 'point-remove)
      t)))

(cl-defmethod edraw-insert-point-before ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (when (edraw-path-anchor-insert-midpoint-before ppoint)
      (edraw-push-undo-path-point-change spt 'path-point-insert-before nil)
      (edraw-on-shape-point-changed shape 'anchor-insert)
      t)))

(cl-defmethod edraw-make-smooth ((spt edraw-shape-point-path))
  "Add handles to SPT anchor point."
  (with-slots (ppoint shape) spt
    (edraw-path-anchor-make-smooth ppoint)
    (edraw-push-undo-path-point-change spt 'path-point-smooth nil)
    (edraw-on-shape-point-changed shape 'anchor-make-smooth)
    t))

(cl-defmethod edraw-make-corner ((spt edraw-shape-point-path))
  "Remove handles from SPT anchor point."
  (with-slots (ppoint shape) spt
    (let* ((fh (edraw-path-anchor-forward-handle ppoint))
           (bh (edraw-path-anchor-backward-handle ppoint))
           (f (if fh (edraw-path-point-remove fh))) ;;may destroy next cmd(C => L, -forward-handle-point => nil)
           (b (if bh (edraw-path-point-remove bh)))) ;;may destroy curr cmd(C => L)
      (when (or f b)
        (edraw-push-undo-path-point-change spt 'path-point-corner nil)
        (edraw-on-shape-point-changed shape 'anchor-make-corner)
        t))))

(cl-defmethod edraw-in-closed-subpath-p ((spt edraw-shape-point-path))
  "Returns t if the point SPT is part of a closed subpath in the path shape."
  (with-slots (ppoint) spt
    (edraw-path-anchor-in-closed-subpath-p ppoint)))

(cl-defmethod edraw-split-path-at ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (edraw-make-undo-group (oref shape editor) 'split-path-at-anchor
      (when (edraw-path-anchor-split-path ppoint)
        (with-slots (cmdlist) shape
          (when-let ((new-cmdlists (edraw-path-cmdlist-split-subpaths cmdlist)))

            (edraw-push-undo-properties shape 'split-path-at-anchor-d '(d))

            ;; Apply first cmdlist in new-cmdlists to the original SHAPE
            (edraw-path-cmdlist-swap cmdlist (car new-cmdlists))
            (edraw-update-path-data shape)

            ;; Create new path shapes
            (dolist (new-cmdlist (cdr new-cmdlists))
              (let ((new-shape (edraw-clone shape)))
                (edraw-path-cmdlist-swap (oref new-shape cmdlist) new-cmdlist)
                (edraw-update-path-data new-shape)
                ;;@todo notify new-shape change?
                ))

            (edraw-on-shape-changed shape 'split-path-at-anchor)
            t))))))



;;;; Property Editor

;;
;;

(require 'widget)
(require 'wid-edit)

(defgroup edraw-faces nil
  "Faces used by edraw"
  :prefix "edraw-"
  :group 'faces)

(defface edraw-widget-button
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "Widget button face."
  :group 'edraw-faces)
(defface edraw-widget-button-mouse
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "grey90" :foreground "black")
    (t :inverse-video t))
  "Widget button mouse face."
  :group 'edraw-faces)
(defface edraw-widget-button-pressed
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style pressed-button)
     :background "lightgrey" :foreground "black")
    (t :inverse-video t))
  "Widget button pressed face."
  :group 'edraw-faces)

(defgroup edraw-property-editor nil
  "Emacs Easy Draw Property Editor"
  :prefix "edraw-property-editor-"
  :group 'edraw-editor)

(defcustom edraw-property-editor-apply-immediately t
  "non-nil means that the entered value will be reflected immediately."
  :group 'edraw-property-editor
  :type 'boolean)

(defcustom edraw-property-editor-tracking-selected-shape t
  "non-nil means to switch the editing target of the property
editor when the selected shape changes."
  :group 'edraw-property-editor
  :type 'boolean)

(defun edraw-property-editor-tracking-selected-shape-p ()
  edraw-property-editor-tracking-selected-shape)

(defcustom edraw-property-editor-close-on-remove-shape nil
  "non-nil means close the property editor when the editing target is removed."
  :group 'edraw-property-editor
  :type 'boolean)


(defvar edraw-property-editor-buffer-name "*Easy Draw Property Editor*")

(defvar edraw-property-editor-push-button-map
  (let ((km (make-sparse-keymap)))
    (define-key km [drag-mouse-1] 'ignore)
    (define-key km [double-down-mouse-1] 'edraw-property-editor-widget-button-click)
    (define-key km [triple-down-mouse-1] 'edraw-property-editor-widget-button-click)
    km))

(defvar edraw-property-editor-field-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-field-keymap)
    (define-key km (kbd "C-c C-c") 'edraw-property-editor--apply)
    (define-key km (kbd "C-c C-k") 'edraw-property-editor--close)
    (define-key km [drag-mouse-1] 'ignore)
    (define-key km [double-mouse-1] 'ignore)
    (define-key km [triple-mouse-1] 'ignore)
    (define-key km [C-wheel-down] 'edraw-property-editor-field-wheel-decrease)
    (define-key km [C-wheel-up] 'edraw-property-editor-field-wheel-increase)
    km))

(defvar edraw-property-editor-local-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km (kbd "C-c C-c") 'edraw-property-editor--apply)
    (define-key km (kbd "C-c C-k") 'edraw-property-editor--close)
    (define-key km [drag-mouse-1] 'ignore)
    (define-key km [double-mouse-1] 'ignore)
    (define-key km [double-down-mouse-1] 'ignore)
    (define-key km [triple-mouse-1] 'ignore)
    km))

(defclass edraw-property-editor ()
  ((buffer :initarg :buffer)
   (window :initarg :window)
   (target :initarg :target)
   (widgets)
   (update-timer :initform nil)))

(defvar-local edraw-property-editor--pedit nil)

(defun edraw-property-editor-buffer ()
  (get-buffer edraw-property-editor-buffer-name))

(defun edraw-property-editor-open (target)
  (save-selected-window
    (let* ((buffer (pop-to-buffer edraw-property-editor-buffer-name))
           (window (selected-window))
           (pedit (edraw-property-editor
                   :buffer buffer
                   :window window
                   :target target)))

      (when edraw-property-editor--pedit
        (edraw-destroy edraw-property-editor--pedit))

      (kill-all-local-variables)
      (setq-local edraw-property-editor--pedit pedit)
      (edraw-open pedit))))

(cl-defmethod edraw-open ((pedit edraw-property-editor))
  (with-slots (target widgets) pedit
    (setq widgets nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (use-local-map edraw-property-editor-local-map)

    (setq-local widget-push-button-prefix "")
    (setq-local widget-push-button-suffix "")
    (setq-local widget-link-prefix "")
    (setq-local widget-link-suffix "")
    (setq-local widget-button-face 'edraw-widget-button)
    (setq-local widget-button-pressed-face 'edraw-widget-button-pressed)
    (setq-local widget-mouse-face 'edraw-widget-button-mouse)

    (if target
        (widget-insert (format (edraw-msg "Properties of %s")
                               (or (edraw-name target) "")) "\n")
      (widget-insert (edraw-msg "No target object") "\n\n"))

    (when target
      (edraw-insert-property-widgets pedit))

    (widget-insert (make-string 2 ? ))

    (when target
      (when (cl-typep target 'edraw-shape)
        (widget-create 'push-button
                       :notify 'edraw-property-editor--prev
                       :keymap edraw-property-editor-push-button-map
                       (edraw-msg "Prev"))
        (widget-insert " ")
        (widget-create 'push-button
                       :notify 'edraw-property-editor--next
                       :keymap edraw-property-editor-push-button-map
                       (edraw-msg "Next"))
        (widget-insert " ")
        (widget-create 'push-button
                       :notify 'edraw-property-editor--set-as-default
                       :keymap edraw-property-editor-push-button-map
                       (edraw-msg "Set as default"))
        (widget-insert " "))

      (unless edraw-property-editor-apply-immediately
        (widget-create 'push-button :notify 'edraw-property-editor--apply
                       (edraw-msg "Apply"))
        (widget-insert " ")))

    (widget-create 'push-button :notify 'edraw-property-editor--close
                   (edraw-msg "Close"))
    (widget-insert "\n")
    (widget-insert "\n")

    (widget-setup)

    (widget-forward 1) ;;to first field

    ;; Adjust window height
    (when-let ((parent-window (window-parent)))
      (let* ((parent-window-height (window-height parent-window))
             (max-height (/ parent-window-height 2)))
        (fit-window-to-buffer nil max-height)
        (enlarge-window 1)))

    (edraw-initialize-hooks pedit)

    (message
     (format (substitute-command-keys
              "\\[edraw-property-editor--apply]: %s  \\[edraw-property-editor--close]: %s")
             (edraw-msg "Apply")
             (edraw-msg "Close")))))

(cl-defmethod edraw-insert-property-widgets ((pedit edraw-property-editor))
  (with-slots (target widgets) pedit
    (let* ((prop-info-list (edraw-get-property-info-list target))
           (max-name-width (when prop-info-list
                             (apply #'max
                                    (mapcar
                                     (lambda (prop-info)
                                       (string-width
                                        (symbol-name (car prop-info))))
                                     prop-info-list)))))
      (dolist (prop-info prop-info-list)
        (unless (plist-get (cdr prop-info) :internal)
          (let* ((prop-name (car prop-info))
                 (prop-type (plist-get (cdr prop-info) :type))
                 (prop-required (plist-get (cdr prop-info) :required))
                 (prop-value (edraw-get-property target prop-name))
                 (indent (- max-name-width (string-width (symbol-name prop-name))))
                 (notify (edraw-create-property-updator
                          pedit prop-name prop-type prop-required))
                 (widget (edraw-create-widget
                          pedit notify indent
                          prop-name prop-value prop-type prop-required)))
            (push (list prop-name widget prop-info) widgets)
            )))
      (setq widgets (nreverse widgets)))))

(cl-defmethod edraw-create-property-updator ((pedit edraw-property-editor)
                                             prop-name prop-type prop-required)
  (lambda (widget _changed-widget &optional _event)
    (when edraw-property-editor-apply-immediately
      (with-slots (target) pedit
        (edraw-set-property target
                            prop-name
                            (edraw-widget-value-to-prop-value
                             pedit (widget-value widget)
                             prop-type prop-required))))))

(cl-defmethod edraw-create-widget ((pedit edraw-property-editor)
                                   notify indent
                                   prop-name prop-value prop-type prop-required)
  (pcase prop-type
    (`(or . ,_)
     (edraw-create-menu-choice-widget
      pedit notify indent prop-name prop-value prop-type prop-required))
    ((or 'number 'float 'length 'coordinate 'opacity)
     (edraw-create-number-widget
      pedit notify indent prop-name prop-value prop-type))
    ('paint
     (edraw-create-paint-widget
      pedit notify indent prop-name prop-value prop-type))
    (_
     (edraw-create-text-field-widget
      pedit notify indent prop-name prop-value prop-type))))

(cl-defmethod edraw-create-menu-choice-widget ((pedit edraw-property-editor)
                                               notify indent
                                               prop-name prop-value prop-type
                                               prop-required)
  (widget-insert (make-string indent ? ))
  (let ((types (if prop-required
                   (cdr prop-type) ;;skip (or)
                 ;; nullable
                 (cons nil (cdr prop-type)))))
    (apply
     #'widget-create
     `(menu-choice
       :format ,(format "%s: %%[%s%%] %%v" prop-name (edraw-msg "Choose"))
       :value ,(edraw-prop-value-to-widget-value pedit prop-value prop-type)
       :notify ,notify
       ,@(mapcar
          (lambda (item)
            (cond
             ((null item) (list 'item :tag " " :value nil)) ;;If :tag="", show separator
             ((stringp item) (list 'item :tag item :value item))
             ;;((symbolp item) (list 'editable-field :tag (symbol-name item)))
             ))
          types)))))

(cl-defmethod edraw-create-text-field-widget ((pedit edraw-property-editor)
                                              notify indent
                                              prop-name prop-value prop-type)
  (widget-insert (make-string indent ? ))
  (widget-create
   'editable-field
   :keymap edraw-property-editor-field-map
   :format (format "%s: %%v" prop-name)
   :value (edraw-prop-value-to-widget-value pedit prop-value prop-type)
   :notify notify))

(defvar edraw-property-editor-number-title-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km [down-mouse-1] 'edraw-property-editor-number-dragging)
    (define-key km [wheel-down] 'edraw-property-editor-field-wheel-decrease)
    (define-key km [wheel-up] 'edraw-property-editor-field-wheel-increase)
    km))

(cl-defmethod edraw-create-number-widget ((pedit edraw-property-editor)
                                          notify indent
                                          prop-name prop-value prop-type)
  (let* ((line-begin (line-beginning-position))
         (name-begin (point))
         (name-end
          (progn
            (widget-insert
             (format "%s%s: " (make-string indent ? ) prop-name))
            (point)))
         (widget (widget-create
                  'editable-field
                  :keymap edraw-property-editor-field-map
                  :value (edraw-prop-value-to-widget-value pedit
                                                           prop-value prop-type)
                  :notify notify))
         (field (edraw-property-editor-number-field-create
                 (current-buffer) widget prop-type prop-name)))
    (widget-put widget :edraw-field field)

    ;;`1-' means avoid reacting to field clicks when the value is empty
    (put-text-property name-begin (1- name-end)
                       'keymap edraw-property-editor-number-title-keymap)
    (put-text-property name-begin (1- name-end)
                       'pointer 'hdrag)
    (put-text-property line-begin name-end 'edraw-field field)
    widget))

(defclass edraw-property-editor-number-field ()
  ((buffer :initarg :buffer)
   (widget :initarg :widget)
   (prop-type :initarg :prop-type)
   (prop-name :initarg :prop-name)
   (min-value :initarg :min-value)
   (max-value :initarg :max-value)
   (default-value :initarg :default-value)
   (divisor :initarg :divisor)))

(defun edraw-property-editor-number-field-create (buffer widget
                                                         prop-type prop-name)
  (edraw-property-editor-number-field
   :buffer buffer
   :widget widget
   :prop-type prop-type
   :prop-name prop-name
   :min-value
   (pcase prop-type
     ('length 0)
     ('opacity 0))
   :max-value
   (pcase prop-type
     ('opacity 1))
   :default-value
   (pcase prop-type
     ('opacity 1)
     (_ 0))
   :divisor
   (pcase prop-type
     ('opacity 100.0)
     (_ 1))))

(cl-defmethod edraw-get-value ((field edraw-property-editor-number-field))
  (with-slots (widget default-value) field
    (let ((w-value (widget-value widget)))
      (if (and (stringp w-value)
               (not (string-empty-p w-value)))
          (string-to-number w-value)
        default-value))))

(cl-defmethod edraw-set-value ((field edraw-property-editor-number-field) value)
  (with-slots (widget min-value max-value divisor) field
    (setq value (edraw-clamp value min-value max-value))
    (when (/= divisor 1)
      (setq value (/ (round (* value divisor)) divisor)))
    (widget-value-set widget (number-to-string value))))

(cl-defmethod edraw-increase ((field edraw-property-editor-number-field) delta)
  (edraw-set-value field (+ (edraw-get-value field)
                            (/ delta (oref field divisor)))))

(defun edraw-property-editor-number-dragging (down-event)
  (interactive "e")
  (when-let ((field (edraw-property-editor-field-at down-event)))
    (with-slots (buffer min-value max-value divisor) field
      (let* ((window (posn-window (event-start down-event)))
             (down-x (car (posn-x-y (event-start down-event))))
             (down-pos (posn-point (event-start down-event)))
             (start-value (edraw-get-value field))
             (min-x (when min-value
                      (+ (* divisor (- min-value start-value)) down-x)))
             (max-x (when max-value
                      (+ (* divisor (- max-value start-value)) down-x)))
             ;; Motion events come only character by character.
             ;; However, when the mouse pointer is over an image,
             ;; events come pixel by pixel.
             (ov (with-current-buffer buffer
                   (save-excursion
                     (goto-char down-pos)
                     (make-overlay (line-beginning-position)
                                   (line-beginning-position))))))
        (overlay-put ov 'after-string "\n")
        (edraw-property-editor-number-dragging-image-update
         ov window down-x min-x max-x)
        (unwind-protect
            (edraw-track-dragging
             down-event
             (lambda (move-event)
               (let* ((move-x (car (posn-x-y (event-start move-event))))
                      (delta-value (/ (- move-x down-x) divisor))
                      (new-value (+ start-value delta-value)))
                 (edraw-set-value field new-value)
                 (edraw-property-editor-number-dragging-image-update
                  ov window move-x min-x max-x)))
             nil nil 'window)
          (delete-overlay ov))))))

(defun edraw-property-editor-number-dragging-image-update (ov window
                                                              x min-x max-x)
  (overlay-put ov 'before-string
               (propertize
                " "
                'pointer 'hdrag
                'display
                (let* ((width (window-body-width window t))
                       (height 32)
                       (svg (svg-create width height))
                       (bar-h 6)
                       (thumb-w 2)
                       (thumb-h 20)
                       (cy (* 0.5 height)))
                  (svg-rectangle svg
                                 0 (- cy (* 0.5 bar-h)) width bar-h :fill "#ccc")
                  (svg-rectangle svg
                                 (- (edraw-clamp x min-x max-x) (* 0.5 thumb-w))
                                 (- cy (* 0.5 thumb-h))
                                 thumb-w thumb-h :fill "#fff")
                  (svg-image svg :scale 1.0)))))


(cl-defmethod edraw-create-paint-widget ((pedit edraw-property-editor)
                                         notify indent
                                         prop-name prop-value prop-type)
  (widget-insert (make-string indent ? ))
  (let (field-widget)
    (widget-insert (symbol-name prop-name) ": ")
    (widget-create
     'push-button :notify
     (lambda (&rest _ignore)
       (widget-value-set
        field-widget
        (edraw-read-property-paint-color pedit prop-name field-widget)))
     (edraw-msg "Color"))
    ;;(widget-insert " ")
    (setq field-widget
          (widget-create
           'editable-field
           :keymap edraw-property-editor-field-map
           :format "%v"
           :value (edraw-prop-value-to-widget-value
                   pedit prop-value prop-type)
           :notify notify))
    field-widget))

(cl-defmethod edraw-read-property-paint-color ((pedit edraw-property-editor)
                                               prop-name
                                               field-widget)
  (with-slots (target) pedit
    (let ((current-value (widget-value field-widget)))
      (unwind-protect
          (edraw-color-picker-read-color
           (format "%s: " prop-name)
           current-value
           '("" "none")
           `((:color-name-scheme . 'web)
             ,@(when (and target (cl-typep target 'edraw-shape))
                 (list
                  (cons :on-input-change
                        (lambda (string color)
                          (when (or (member string '("" "none"))
                                    color)
                            ;;@todo suppress modified flag change and notification
                            (edraw-set-property target prop-name string))))))))
        (widget-value-set field-widget current-value)))))



(defun edraw-property-editor-field-at (event)
  (let* ((start (event-start event))
         (window (posn-window start))
         (buffer (window-buffer window))
         (pos (posn-point start)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (let ((inhibit-field-text-motion t))
          (get-text-property (line-beginning-position) 'edraw-field))))))

(defun edraw-property-editor-field-wheel-increase (n event)
  (interactive "p\ne")
  (when-let ((field (edraw-property-editor-field-at event)))
    (edraw-increase field n)))

(defun edraw-property-editor-field-wheel-decrease (n event)
  (interactive "p\ne")
  (edraw-property-editor-field-wheel-increase (- n) event))


(cl-defmethod edraw-prop-value-to-widget-value ((_pedit edraw-property-editor)
                                                prop-value prop-type)
  (pcase prop-type
    (`(or . ,_)
     ;; string or nil
     prop-value)
    (_
     ;; string only
     (format "%s" (or prop-value "")))))

(cl-defmethod edraw-apply-properties ((pedit edraw-property-editor))
  (with-slots (widgets target) pedit
    (let ((prop-values
           (cl-loop
            for widget-info in widgets
            collect
            (let* ((prop-name (nth 0 widget-info))
                   (widget (nth 1 widget-info))
                   (prop-info (nth 2 widget-info))
                   (prop-type (plist-get (cdr prop-info) :type))
                   (prop-required (plist-get (cdr prop-info) :required))
                   (w-value (widget-value widget))
                   (value (edraw-widget-value-to-prop-value
                           pedit w-value prop-type prop-required)))
              (cons prop-name value)))))
      (edraw-set-properties target prop-values))))

(cl-defmethod edraw-widget-value-to-prop-value ((_pedit edraw-property-editor)
                                                w-value prop-type prop-required)
  (if (and (not prop-required)
           (or (and (stringp w-value)
                    (string-empty-p w-value))
               (null w-value)))
      nil ;;property is not required and w-value is an empty string or nil
    (pcase prop-type
      ;; integer
      ('integer
       ;;@todo check
       w-value)

      ;; number
      ((or 'number 'float 'coordinate 'length)
       ;;@todo check
       w-value)

      ;; choice
      (`(or . ,_)
       ;;@todo check
       w-value)

      ;; string?
      (_ w-value))))

(cl-defmethod edraw-update-widgets-value ((pedit edraw-property-editor))
  (with-slots (widgets target) pedit
    (cl-loop
     for widget-info in widgets
     do (let* ((prop-name     (nth 0 widget-info))
               (widget        (nth 1 widget-info))
               (prop-info     (nth 2 widget-info))
               (prop-type     (plist-get (cdr prop-info) :type))
               ;;(prop-required (plist-get (cdr prop-info) :required))
               (prop-value (edraw-get-property target prop-name))
               (w-value (widget-value widget))
               (value (edraw-prop-value-to-widget-value pedit
                                                        prop-value
                                                        prop-type)))
          (unless (equal w-value value)
            ;;(message "target chagned: %s: %s to %s" prop-name w-value value)
            ;; Prevent notification
            (let ((old-notify (widget-get widget :notify)))
              (widget-put widget :notify #'ignore)
              (unwind-protect
                  (widget-value-set widget value)
                (widget-put widget :notify old-notify))))))))



(defun edraw-property-editor--close (&rest _ignore)
  (interactive)
  (when edraw-property-editor--pedit
    (edraw-close edraw-property-editor--pedit)))

(cl-defmethod edraw-close ((pedit edraw-property-editor))
  ;; close window
  (with-slots (window buffer) pedit
    (when (and window
               buffer
               (eq (window-buffer window) buffer)
               (window-parent window))
      (delete-window window)))

  ;; delete buffer
  (kill-buffer edraw-property-editor-buffer-name))

(defun edraw-property-editor--apply (&rest _ignore)
  (interactive)
  (when edraw-property-editor--pedit
    (edraw-apply-properties edraw-property-editor--pedit)))

(defun edraw-property-editor--prevnext (prev-or-next-func)
  (when-let ((pedit edraw-property-editor--pedit))
    (with-slots (target) pedit
      (when (and target
                 (cl-typep target 'edraw-shape))
        (when-let ((new-target (funcall prev-or-next-func target)))
          (if edraw-property-editor-tracking-selected-shape
              (edraw-select new-target)
            ;; destroy PEDIT and open NEW-TARGET
            (edraw-property-editor-open new-target)))))))

(defun edraw-property-editor--prev (&rest _ignore)
  (edraw-property-editor--prevnext 'edraw-previous-sibling))

(defun edraw-property-editor--next (&rest _ignore)
  (edraw-property-editor--prevnext 'edraw-next-sibling))

(defun edraw-property-editor--set-as-default (&rest _ignore)
  (when-let ((pedit edraw-property-editor--pedit))
    (with-slots (target) pedit
      (when (and target
                 (cl-typep target 'edraw-shape))
        (edraw-set-default-shape-properties-from-shape
         (oref target editor)
         target)))))


(cl-defmethod edraw-initialize-hooks ((pedit edraw-property-editor))
  (with-slots (target) pedit
    (when target
      (edraw-add-change-hook target 'edraw-on-target-changed pedit))
    (add-hook 'kill-buffer-hook
              'edraw-property-editor--on-kill-buffer
              nil t)))

(cl-defmethod edraw-uninitialize-hooks ((pedit edraw-property-editor))
  (with-slots (target) pedit
    (when target
      (edraw-remove-change-hook target 'edraw-on-target-changed pedit))
    (remove-hook 'kill-buffer-hook
                 'edraw-property-editor--on-kill-buffer
                 t)))

(defun edraw-property-editor--on-kill-buffer ()
  (when edraw-property-editor--pedit
    (edraw-destroy edraw-property-editor--pedit)
    (setq edraw-property-editor--pedit nil)))

(cl-defmethod edraw-on-target-changed ((pedit edraw-property-editor)
                                       _source type)
  (cond
   ((or (eq type 'shape-remove)
        (eq type 'document-close))
    (if edraw-property-editor-close-on-remove-shape
        (edraw-close pedit)
      ;; destroy pedit and open empty property editor
      (edraw-property-editor-open nil)
      ))
   (t
    (with-slots (update-timer) pedit
      (when (null update-timer)
        (setq update-timer
              (run-at-time 0.1 nil 'edraw-on-update-timer pedit)))))))

(cl-defmethod edraw-on-update-timer ((pedit edraw-property-editor))
  (with-slots (update-timer) pedit
    (setq update-timer nil)
    (edraw-update-widgets-value pedit)))

(cl-defmethod edraw-destroy ((pedit edraw-property-editor))
  ;;(message "in edraw-destroy pedit")
  (edraw-uninitialize-hooks pedit)
  (with-slots (buffer target update-timer) pedit
    (setq target nil)
    (setq buffer nil)
    (when update-timer
      (cancel-timer update-timer)
      (setq update-timer nil))))


(defun edraw-property-editor-widget-button-click (event)
  "An alternative to widget-button-click for double-click and triple-click."
  (interactive "e")
  (cl-letf (((symbol-function 'widget-button-release-event-p)
             'edraw-property-editor-widget-button-release-event-p))
    (widget-button-click event)))

(defun edraw-property-editor-widget-button-release-event-p (event)
  "An alternative to widget-button-release-event-p for double-click and triple-click."
  (and (eventp event)
       (memq (event-basic-type event) '(mouse-1 mouse-2 mouse-3))
       (or (and (or (memq 'double (event-modifiers event)) ;;double click
                    (memq 'triple (event-modifiers event))) ;;triple click
                (null (memq 'down (event-modifiers event))))
           (memq 'click (event-modifiers event))
           (memq 'drag (event-modifiers event)))))



;;;; Clipboard

(defvar edraw-clipboard-type-data nil)

(defun edraw-clipboard-set (type data)
  (setq edraw-clipboard-type-data (cons type data)))

(defun edraw-clipboard-empty-p ()
  (null edraw-clipboard-type-data))

(defun edraw-clipboard-type ()
  (car edraw-clipboard-type-data))

(defun edraw-clipboard-data ()
  (cdr edraw-clipboard-type-data))




;;;; SVG Encode / Decode

;;
;;

(defun edraw-decode-svg (data base64-p)
  (with-temp-buffer
    (insert data)
    (edraw-decode-buffer base64-p)
    (libxml-parse-xml-region (point-min) (point-max))))

(defun edraw-encode-svg (svg base64-p gzip-p)
  (with-temp-buffer
    (edraw-svg-print
     svg
     nil
     'edraw-svg-print-attr-filter)
    (edraw-encode-buffer base64-p gzip-p)
    (buffer-string)))




;;;; SVG File I/O

(defun edraw-make-file-writer (path gzip-p)
  (lambda (svg)
    (edraw-write-svg-to-file svg path gzip-p)))

(defun edraw-write-svg-to-file (svg path gzip-p)
  (with-temp-file path
    (insert (edraw-encode-svg svg nil gzip-p))
    (set-buffer-file-coding-system 'utf-8)))

(defun edraw-read-svg-from-file (path)
  (edraw-decode-svg
   (with-temp-buffer
     (insert-file-contents path)
     (buffer-substring-no-properties (point-min) (point-max)))
   nil))



(provide 'edraw)
;;; edraw.el ends here
