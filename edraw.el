;;; edraw.el --- Emacs Easy Draw                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Homepage: https://github.com/misohena/el-easydraw
;; Keywords: Graphics,Drawing,SVG

;; Package-Version: 1.0.1
;; Package-Requires: ((emacs "27.1"))

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
(require 'edraw-property-editor)

(autoload 'edraw-shape-picker-open "edraw-shape-picker")
(declare-function edraw-shape-picker-connect "edraw-shape-picker")
(declare-function edraw-shape-picker-disconnect "edraw-shape-picker")
(declare-function edraw-shape-picker-selected-args "edraw-shape-picker")


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
  "Drawing editor."
  :tag "Edraw Editor"
  :prefix "edraw-editor-"
  :group 'edraw)

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

;;NOTE: Referenced by edraw-property-editor.el, edraw-shape-picker.el for read color
(defcustom edraw-editor-image-scaling-factor nil
  "The scaling factor for editor images. By default the editors
uses the value of `image-scaling-factor' variable."
  :group 'edraw-editor
  :type '(choice number
                 (const :tag "Use `image-scaling-factor'" nil)))

(defcustom edraw-editor-default-grid-interval 20
  "The interval of grid lines."
  :group 'edraw-editor
  :type 'number)

(defcustom edraw-editor-default-grid-visible t
  "non-nil means grid lines are displayed by default."
  :group 'edraw-editor
  :type 'boolean)

(defcustom edraw-editor-default-transparent-bg-visible t
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

(defcustom edraw-editor-auto-view-enlargement-max-size (cons 560 420)
  "When zoomed, editing view will automatically resize up to this size.

When nil, disable auto view enlargement."
  :group 'edraw-editor
  :type '(choice (const nil :tag "Disable auto view enlargement")
                 (cons
                  :tag "Maximum Size"
                  (integer :tag "Width")
                  (integer :tag "Height"))))

(defconst edraw-grid-display-min-interval 4.0)

(defconst edraw-anchor-point-radius 3.5)
(defconst edraw-handle-point-radius 3.0)
(defconst edraw-anchor-point-input-radius (+ 1.0 edraw-anchor-point-radius))
(defconst edraw-handle-point-input-radius (+ 2.0 edraw-handle-point-radius))

(defvar edraw-snap-text-to-shape-center t)

(defvar edraw-editor-move-point-on-click t)

(defvar edraw-editor-map
  (let ((km (make-sparse-keymap)))
    (define-key km [remap self-insert-command] 'ignore)
    (define-key km [down-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [C-down-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [S-down-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [M-down-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [M-S-down-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [C-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [S-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [double-mouse-1] 'edraw-editor-dispatch-event)
    (define-key km [down-mouse-2] 'edraw-editor-scroll-by-dragging)
    (define-key km [mouse-3] 'edraw-editor-dispatch-event)
    (define-key km (vector (intern (format "C-%s" mouse-wheel-up-event))) 'edraw-editor-zoom-out-by-mouse)
    (define-key km (vector (intern (format "C-%s" mouse-wheel-down-event))) 'edraw-editor-zoom-in-by-mouse)
    (define-key km " " 'edraw-editor-interactive-scroll-and-zoom)
    (define-key km "m" 'edraw-editor-main-menu)
    (define-key km "s" 'edraw-editor-select-tool-select)
    (define-key km "r" 'edraw-editor-select-tool-rect)
    (define-key km "e" 'edraw-editor-select-tool-ellipse)
    (define-key km "a" 'edraw-editor-select-tool-path)
    (define-key km "f" 'edraw-editor-select-tool-freehand)
    (define-key km "t" 'edraw-editor-select-tool-text)
    (define-key km "u" 'edraw-editor-select-tool-custom-shape)
    (define-key km "F" 'edraw-editor-edit-tool-default-fill)
    (define-key km "S" 'edraw-editor-edit-tool-default-stroke)
    (define-key km "#" 'edraw-editor-toggle-grid-visible)
    (define-key km (kbd "M-#") 'edraw-editor-set-grid-interval)
    (define-key km "\"" 'edraw-editor-toggle-transparent-bg-visible)
    (define-key km "db" 'edraw-editor-set-background)
    (define-key km "dr" 'edraw-editor-set-size)
    (define-key km "dc" 'edraw-editor-crop)
    (define-key km "dtt" 'edraw-editor-translate-all-shapes)
    (define-key km "dts" 'edraw-editor-scale-all-shapes)
    (define-key km "dtr" 'edraw-editor-rotate-all-shapes)
    (define-key km "deb" 'edraw-editor-export-to-buffer)
    (define-key km "def" 'edraw-editor-export-to-file)
    (define-key km "ded" 'edraw-editor-export-debug-svg-to-buffer)
    (define-key km "z" 'edraw-editor-undo)
    (define-key km "Z" 'edraw-editor-redo)
    (define-key km "+" 'edraw-editor-zoom-in)
    (define-key km "-" 'edraw-editor-zoom-out)
    (define-key km "0" 'edraw-editor-reset-scroll-and-zoom)
    (define-key km "v+" 'edraw-editor-zoom-in)
    (define-key km "v-" 'edraw-editor-zoom-out)
    (define-key km "v0" 'edraw-editor-reset-view)
    (define-key km "vr" 'edraw-editor-set-view-size-spec)
    ;; Selected Object
    (define-key km "A" 'edraw-editor-toggle-selection-all)
    (define-key km [remap yank] 'edraw-editor-paste-and-select)
    (define-key km [remap kill-region] 'edraw-editor-cut-selected-shapes)
    (define-key km [remap kill-ring-save] 'edraw-editor-copy-selected-shapes)
    ;; (define-key km (kbd "C-c C-x C-y") 'edraw-editor-paste-and-select)
    ;; (define-key km (kbd "C-c C-x C-w") 'edraw-editor-cut-selected-shapes)
    ;; (define-key km (kbd "C-c C-x M-w") 'edraw-editor-copy-selected-shapes)
    (define-key km "Tt" 'edraw-editor-translate-selected)
    (define-key km "Ts" 'edraw-editor-scale-selected)
    (define-key km "Tr" 'edraw-editor-rotate-selected)
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
    (define-key km (kbd "M-S-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-S-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-S-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-S-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "}") 'edraw-editor-bring-selected-to-front)
    (define-key km (kbd "]") 'edraw-editor-bring-selected-forward)
    (define-key km (kbd "[") 'edraw-editor-send-selected-backward)
    (define-key km (kbd "{") 'edraw-editor-send-selected-to-back)
    (define-key km (kbd "M-]") 'edraw-editor-select-next-shape)
    (define-key km (kbd "M-[") 'edraw-editor-select-previous-shape)
    km))

(defvar edraw-editor-disable-line-prefix t
  "Disable line-prefix and wrap-prefix properties on editor overlays.

There is a bug in Emacs where the coordinates of mouse events are
misaligned when line-prefix is used and the image is at the top
of the window.

When this variable is t, the line-prefix and wrap-prefix
properties of the editors are set to the empty string to
disable the line-prefix and wrap-prefix already set in the
buffer.

line-prefix and wrap-prefix are used in org-indent.")

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
   (svg-document-view-box)
   (defrefs)
   (document-writer :initarg :document-writer :initform nil)
   (menu-filter :initarg :menu-filter :initform nil)

   (image-scale
    :initform (image-compute-scaling-factor
               (or edraw-editor-image-scaling-factor image-scaling-factor))
    :type number)
   (image)
   (image-update-timer :initform nil)
   (invalid-ui-parts :initform nil)
   (scroll-transform :initform (list 0 0 1)) ;;dx dy scale
   (view-size :initform nil) ;; nil means document size
   (settings
    :initform (list (cons 'grid-visible
                          edraw-editor-default-grid-visible)
                    (cons 'grid-interval
                          edraw-editor-default-grid-interval)
                    (cons 'transparent-bg-visible
                          edraw-editor-default-transparent-bg-visible)
                    (cons 'view-size-spec nil) ;;User specified view size
                    (cons 'transform-method 'auto)))
   (extra-properties :initform nil)
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
    (when edraw-editor-disable-line-prefix ;;for Emacs's line-prefix bug
      (overlay-put overlay 'line-prefix "")
      (overlay-put overlay 'wrap-prefix ""))
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
  (defvar edraw-editor-default-tool) ;;defcustom
  (edraw-select-tool editor (edraw-editor-make-tool edraw-editor-default-tool))
  ;; Return editor
  editor)

(cl-defmethod edraw-close ((editor edraw-editor))
  (with-slots (overlay) editor
    (when (and overlay (overlay-buffer overlay))
      (edraw-property-editor-close) ;;Before change selection and delete shapes
      (edraw-deselect-all-shapes editor)
      (edraw-select-tool editor nil) ;;deselect tool (some tools need to disconnect)
      (edraw-notify-document-close-to-all-shapes editor) ;;should edraw-clear?
      (edraw-update-image-timer-cancel editor)
      (delete-overlay overlay))))

;;;;; Editor - User Settings

(cl-defmethod edraw-get-setting ((editor edraw-editor) key)
  (alist-get key (oref editor settings)))
(cl-defmethod edraw-set-setting ((editor edraw-editor) key value)
  (setf (alist-get key (oref editor settings)) value))

;;;;; Editor - Extra Properties

(cl-defmethod edraw-get-extra-prop ((editor edraw-editor) key)
  (alist-get key (oref editor extra-properties)))
(cl-defmethod edraw-set-extra-prop ((editor edraw-editor) key value)
  (setf (alist-get key (oref editor extra-properties)) value))

;;;;; Editor - Hooks

(cl-defmethod edraw-define-hook-type ((editor edraw-editor) hook-type)
  (with-slots (hooks) editor
    (unless (alist-get hook-type hooks)
      (push (cons hook-type (edraw-hook-make)) hooks))))

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

;;;;; Editor - Lookup Editor

(defvar edraw-current-editor nil)

(defun edraw-current-editor (&optional noerror)
  (let ((editor (or edraw-current-editor
                    (edraw-editor-at-input last-command-event))))
    (when (and (null editor)
               (not noerror))
      (error (edraw-msg "No editor here")))
    editor))

(defun edraw-editor-overlays-in (beg end)
  (seq-filter (lambda (ov) (overlay-get ov 'edraw-editor))
              (overlays-in beg end)))

(defun edraw-editor-at (&optional pos)
  (let ((pos (or pos (point))))
    (or
     (seq-some (lambda (ov) (overlay-get ov 'edraw-editor)) (overlays-at pos))
     (seq-some (lambda (ov) (overlay-get ov 'edraw-editor)) (overlays-at (1- pos)))
     (seq-some (lambda (ov) (overlay-get ov 'edraw-editor)) (overlays-in (1- pos) (1+ pos))))))

(defun edraw-editor-at-input (event)
  (if (or (mouse-event-p event)
          (memq (event-basic-type event)
                '(wheel-up wheel-down 'mouse-4 'mouse-5 'drag-n-drop)))
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

;;;;; Editor - Define Commands

(defun edraw-editor-call-at-point (method &optional editor args)
  (let ((editor (or editor (edraw-current-editor))))
    (apply method editor args)))

(defmacro edraw-editor-defcmd (method-symbol arg-list &rest rest)
  ;; Basically intended to be used instead of cl-defmethod.
  ;;
  ;; Example:
  ;; (edraw-editor-defcmd edraw-some-command ;;<METHOD-SYMBOL>
  ;;   ((editor edraw-editor) &optional xy) ;;<ARG-LIST>
  ;;   "DOC" ;;<REST>...
  ;;   ...)
  ;;
  ;; However, interactive can be specified.
  (declare (indent 2))
  (unless (equal (car arg-list) '(editor edraw-editor))
    (error "Defcmd's argument list must start with (editor edraw-editor)"))

  (let ((method-name (symbol-name method-symbol)))
    (unless (string-match "\\`edraw-\\(.+\\)\\'" method-name)
      (error "Method name %s does not start with edraw-" method-name))
    (let* ((suffix (match-string 1 method-name))
           (cmd-symbol (intern (concat "edraw-editor-" suffix)))
           (doc
            (when (stringp (car rest))
              (prog1 (car rest) (setq rest (cdr rest)))))
           (interactive-spec
            (when (eq (car-safe (car rest)) 'interactive)
              (prog1 (cdar rest) (setq rest (cdr rest)))))
           (doc-arg-list
            (concat
             "(fn EDITOR"
             (mapconcat (lambda (arg-name)
                          (concat
                           " "
                           (let ((str (symbol-name arg-name)))
                             (if (= (elt str 0) ?&)
                                 str
                               (upcase str)))))
                        (cdr arg-list)
                        "")
             ")")))

      (when doc
        (setq rest (cons doc rest)))

      `(progn
         ;; Define command (edraw-editor-* (&optional editor) ...)
         (defun ,cmd-symbol (editor &rest args)
           ,(concat doc "\n\n" doc-arg-list)
           (interactive ,@(or interactive-spec
                              '((list (edraw-current-editor)))))
           (edraw-editor-call-at-point (quote ,method-symbol) editor args))
         ;; Define generic
         (cl-defgeneric ,method-symbol ,(cons 'OBJECT (cdr arg-list)))
         ;; Define method (edraw-* ((editor edraw-editor) ...) ...)
         (cl-defmethod ,method-symbol ,arg-list
           ,@rest
           )))))

;;;;; Editor - Undo

(defconst edraw-editor-undo-limit 30) ;;@todo To defcustom

;;@todo Should the following variables be held by individual editor objects?

(defvar edraw-editor-inhibit-make-undo-data nil)
(defvar edraw-editor-inhibit-discarding-undo-data nil)
(defvar edraw-editor-undo-in-progress nil)
(defvar edraw-editor-redo-in-progress nil)
(defvar edraw-editor-undo-group-level 0)

(defun edraw-editor-clear-undo-vars ()
  (setq edraw-editor-inhibit-make-undo-data nil
        edraw-editor-inhibit-discarding-undo-data nil
        edraw-editor-undo-in-progress nil
        edraw-editor-redo-in-progress nil
        edraw-editor-undo-group-level 0))


(cl-defmethod edraw-clear-undo-list ((editor edraw-editor))
  "Discard all undo/redo data."
  (oset editor undo-list nil)
  (oset editor redo-list nil))

(cl-defmethod edraw-undo-list ((editor edraw-editor))
  "Return a undo data list of the EDITOR.

The top of the list is the newest data.

The format of each data is (TYPE FUNCTION ARGUMENTS...)."
  (oref editor undo-list))

(cl-defmethod edraw-last-undo-data ((editor edraw-editor))
  "Return a recently pushed undo data."
  (car (oref editor undo-list)))

(cl-defmethod edraw-empty-undo-p ((editor edraw-editor))
  "Return t if there is no undo data in the EDITOR."
  (null (oref editor undo-list)))

(cl-defmethod edraw-empty-redo-p ((editor edraw-editor))
  "Return t if there is no redo data in the EDITOR."
  (null (oref editor redo-list)))

;; Undo Data Structure

(defmacro edraw-undo-data-type (data) `(car ,data))
(defmacro edraw-undo-data-func (data) `(cadr ,data))
(defmacro edraw-undo-data-args (data) `(cddr ,data))
(defmacro edraw-undo-data-arg0 (data) `(caddr ,data))
(defmacro edraw-undo-data-arg1 (data) `(cadddr ,data))
(defun edraw-undo-data-starts-with-args-p (data type func &rest args)
  "Return t if undo DATA has the same TYPE, FUNC, and the argument
list starts with ARGS."
  (and (eq (edraw-undo-data-type data) type)
       (eq (edraw-undo-data-func data) func)
       (cl-loop for x = (edraw-undo-data-args data) then (cdr x)
                for y in args
                unless x return nil
                unless (eq (car x) y) return nil
                finally return t)))

;; Push Undo Data

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
    ;; Discard undo data exceeding the limit.
    (unless (or (> edraw-editor-undo-group-level 0)
                edraw-editor-inhibit-discarding-undo-data)
      (when-let ((cell (nthcdr edraw-editor-undo-limit undo-list)))
        (setcdr cell nil)))))

;; No Undo Data

(defmacro edraw-editor-with-no-undo-data (&rest body)
  (declare (indent 0))
  `(let ((edraw-editor-inhibit-make-undo-data t))
     ,@body))

;; Undo Group

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

;; Undo Command

(edraw-editor-defcmd edraw-undo ((editor edraw-editor))
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

(edraw-editor-defcmd edraw-redo ((editor edraw-editor))
  (if (edraw-empty-redo-p editor)
      (message (edraw-msg "No redo data"))
    (let ((edraw-editor-redo-in-progress t))
      (with-slots (redo-list) editor
        (edraw-make-undo-group editor 'redo
          (edraw-call-undo-data (pop redo-list)))))))

(defun edraw-call-undo-data (type-data)
  (apply (car (cdr type-data))
         (cdr (cdr type-data))))

(cl-defmethod edraw-undo-all ((editor edraw-editor))
  "Executes all UNDO data recorded in EDITOR.

For use with `edraw-editor-with-temp-undo-list',
`edraw-editor-with-temp-modifications', etc."
  (while (not (edraw-empty-undo-p editor))
    (edraw-undo editor)))

;; Independent Undo List

(defmacro edraw-editor-with-temp-undo-list (editor &rest body)
  "Evaluate the BODY under a new independent undo list."
  (declare (indent 1))
  ;;@todo Increase undo limit?
  (let ((sym-editor (gensym 'editor-))
        (sym-old-undo-list (gensym 'old-undo-list-))
        (sym-old-redo-list (gensym 'old-redo-list-)))
    `(let* ((,sym-editor ,editor)
            (,sym-old-undo-list (oref ,sym-editor undo-list))
            (,sym-old-redo-list (oref ,sym-editor redo-list)))
       (setf (oref ,sym-editor undo-list) nil
             (oref ,sym-editor redo-list) nil)
       (unwind-protect
           (progn
             ,@body)
         (setf (oref ,sym-editor undo-list) ,sym-old-undo-list
               (oref ,sym-editor redo-list) ,sym-old-redo-list)))))

(defmacro edraw-editor-with-temp-modifications (editor &rest body)
  "Evaluate BODY and then execute all UNDO data recorded in EDITOR."
  (declare (indent 1))
  (let ((sym-editor (gensym 'editor-)))
    `(let ((,sym-editor ,editor)
           (edraw-editor-inhibit-make-undo-data nil) ;; Record undo data
           (edraw-editor-inhibit-discarding-undo-data t)) ;; Keep all undo data
       (edraw-editor-with-temp-undo-list ,sym-editor
         (unwind-protect
             (progn
               ,@body)
           ;; Undo all changes
           (edraw-undo-all ,sym-editor))))))


;;;;; Editor - Document
;;;;;; Editor - Document - Whole Document
;;;;;;; Document SVG Structure

;; Document structure in SVG
;; <svg>
;;   <defs id="edraw-defs"> ... </defs>
;;   <rect id="edraw-background" />
;;   <g id="edraw-body"> ... </g>
;; </svg>

(defconst edraw-editor-svg-defs-id "edraw-defs")
(defconst edraw-editor-svg-background-id "edraw-background")
(defconst edraw-editor-svg-body-id "edraw-body")

(cl-defmethod edraw-initialize-svg-document ((editor edraw-editor))
  (with-slots (svg svg-document-size svg-document-view-box defrefs) editor
    (when (null svg)
      (setq svg (edraw-create-document-svg)))

    ;; Backup SVG Size
    (setq svg-document-size (cons (edraw-svg-attr-length svg 'width)
                                  (edraw-svg-attr-length svg 'height)))
    ;; Backup SVG View Box
    (setq svg-document-view-box (dom-attr svg 'viewBox))

    ;; #edraw-defs
    (if-let ((defs-element (edraw-dom-get-by-id svg edraw-editor-svg-defs-id)))
        (setq defrefs (edraw-svg-defrefs-from-dom
                       defs-element (edraw-dom-get-by-id svg edraw-editor-svg-body-id)))
      (setq defrefs (edraw-svg-defs-as-defrefs edraw-editor-svg-defs-id))
      (edraw-dom-insert-first svg
                              (edraw-svg-defrefs-defs defrefs)))

    ;; #edraw-body
    (edraw-dom-get-or-create svg 'g edraw-editor-svg-body-id)

    ;; Restore Point Connections
    (edraw-restore-point-connections editor)))

(cl-defmethod edraw-restore-point-connections ((editor edraw-editor))
  (edraw-editor-with-no-undo-data
    (dolist (element
             (dom-elements (edraw-svg-body editor)
                           'data-edraw-point-connections ""))
      (when-let ((shape (edraw-shape-from-element element editor t)))
        (edraw-restore-point-connections shape)))))

(defun edraw-create-document-svg (&optional width height background children)
  (let* ((width (or width (alist-get 'width edraw-default-document-properties)))
         (height (or height (alist-get 'height edraw-default-document-properties)))
         (background (or background (alist-get 'background edraw-default-document-properties)))
         (svg (svg-create width height)))
    (when (and background (not (equal background "none")))
      (svg-rectangle svg 0 0 width height
                     :id edraw-editor-svg-background-id
                     :stroke "none"
                     :fill background))

    ;; #edraw-body
    ;; If children='(nil), create #edraw-body only.
    (when children
      (let ((body (edraw-dom-get-or-create svg 'g edraw-editor-svg-body-id)))
        (dolist (child children)
          (when child ;;Enable '(nil) to create body node only.
            (dom-append-child body child)))))
    svg))

(defun edraw-get-document-body (svg)
  (edraw-dom-get-by-id svg edraw-editor-svg-body-id))

(cl-defmethod edraw-svg-body ((editor edraw-editor))
  (edraw-get-document-body (oref editor svg)))

;;;;;;; Modification

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

(defmacro edraw-editor-with-silent-modifications (&rest body)
  (declare (indent 0))
  `(let ((edraw-editor-inhibit-make-undo-data t)
         (edraw-editor-keep-modified-flag t))
     ,@body))

;;;;;;; Write

(edraw-editor-defcmd edraw-save ((editor edraw-editor))
  (with-slots (document-writer svg) editor
    (when (and document-writer
               (edraw-modified-p editor))
      (let ((doc-svg (edraw-document-svg editor)))
        (prog1
            ;;signal error if failed
            (funcall document-writer doc-svg)
          (edraw-set-modified-p editor nil))))))

(cl-defmethod edraw-document-svg ((editor edraw-editor))
  (with-slots (svg svg-document-size svg-document-view-box) editor
    (let ((doc-svg (edraw-editor-remove-ui-element-from-svg svg)))
      (edraw-editor-remove-scroll-transform doc-svg)
      (edraw-editor-remove-root-transform doc-svg
                                          svg-document-size
                                          svg-document-view-box)
      ;; Add xmlns
      (edraw-svg-set-attr-string doc-svg 'xmlns "http://www.w3.org/2000/svg")
      ;;(edraw-svg-set-attr-string doc-svg 'xmlns:xlink "http://www.w3.org/1999/xlink")
      ;; Remove empty defs
      (when-let ((defs (edraw-dom-get-by-id doc-svg edraw-editor-svg-defs-id)))
        (when (null (dom-children defs))
          (dom-remove-node doc-svg defs)))
      doc-svg)))

(edraw-editor-defcmd edraw-export-to-buffer ((editor edraw-editor))
  (pop-to-buffer "*Easy Draw SVG*")
  (erase-buffer)
  (edraw-svg-print
   (edraw-document-svg editor)
   nil
   'edraw-svg-print-attr-filter 0)
  (xml-mode))

(edraw-editor-defcmd edraw-export-to-file ((editor edraw-editor)
                                           &optional filename)
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

(edraw-editor-defcmd edraw-export-debug-svg-to-buffer ((editor edraw-editor))
  (pop-to-buffer "*Easy Draw SVG*")
  (erase-buffer)
  (edraw-svg-print
   (oref editor svg)
   nil
   'edraw-svg-print-attr-filter 0)
  (xml-mode))


;;;;;;; Clear

(edraw-editor-defcmd edraw-clear ((editor edraw-editor))
  (interactive
   (if (edraw-y-or-n-p
        (edraw-msg "Do you want to close the current document?"))
       (list nil)
     (signal 'quit nil)))

  (edraw-deselect-all-shapes editor)
  (edraw-select-tool editor nil)
  (edraw-notify-document-close-to-all-shapes editor)
  (edraw-clear-undo-list editor)

  (edraw-reset-view editor)

  (with-slots (svg) editor
    (setq svg nil))

  (edraw-initialize-svg editor)
  (edraw-on-document-changed editor 'document-initialized)
  ;;(edraw-set-modified-p editor nil) ?

  (edraw-select-tool editor (edraw-editor-make-tool 'rect)))

;;;;;; Editor - Document - Size

(cl-defmethod edraw-width ((editor edraw-editor))
  (car (oref editor svg-document-size)))

(cl-defmethod edraw-height ((editor edraw-editor))
  (cdr (oref editor svg-document-size)))

(edraw-editor-defcmd edraw-set-size ((editor edraw-editor) width height)
  (interactive
   (let* ((editor (edraw-current-editor))
          (width (read-number
                  (edraw-msg "Document Width: ") (edraw-width editor)))
          (height (read-number
                   (edraw-msg "Document Height: ") (edraw-height editor))))
     (list editor width height)))
  (with-slots (svg-document-size) editor
    (let ((old-width (car svg-document-size))
          (old-height (cdr svg-document-size)))
      (when (or (/= width old-width)
                (/= height old-height))
        (edraw-push-undo
         editor 'document-size
         (list 'edraw-set-size editor old-width old-height))
        (setq svg-document-size (cons width height))
        (edraw-update-background editor) ;; Update Document Background
        (edraw-invalidate-ui-parts editor 'all) ;; Update <svg width= height=> etc...
        (edraw-on-document-changed editor 'document-size)))))

;;;;;; Editor - Document - Crop

(edraw-editor-defcmd edraw-crop ((editor edraw-editor) rect)
  (interactive
   (let* ((editor (edraw-current-editor))
          rect)
     (while (null rect)
       (let ((event (read-event
                     (edraw-msg "Drag the cropping range."))))
         (cond
          ((eq (car-safe event) 'down-mouse-1)
           (setq rect (edraw-read-rectangle editor event t))) ;;snap
          ((or (eq (car-safe event) 'mouse-3)
               (eq event ?q)
               (eq event ? ))
           (signal 'quit nil))
          ;; Scroll and Zoom
          ((eq (car-safe event) 'down-mouse-2)
           (edraw-editor-scroll-by-dragging event))
          ((eq (car-safe event) mouse-wheel-down-event)
           (edraw-zoom-in editor))
          ((eq (car-safe event) mouse-wheel-up-event)
           (edraw-zoom-out editor))
          ((eq event ?0)
           (edraw-reset-scroll-and-zoom editor))
          ((memq (event-basic-type event) '(left up right down))
           (edraw-editor-scroll-by-arrow-key editor)
           ))))
     (list editor rect)))

  (when (edraw-rect-empty-p rect)
    (error (edraw-msg "The crop range is empty.")))

  (edraw-make-undo-group editor 'document-crop
    (unless (edraw-xy-zero-p (edraw-rect-lt rect))
      (edraw-translate-all-shapes editor (edraw-xy-neg (edraw-rect-lt rect))))
    (edraw-set-size editor (edraw-rect-width rect) (edraw-rect-height rect))))

;;;;;; Editor - Document - View Box

(edraw-editor-defcmd edraw-set-view-box ((editor edraw-editor) new-value)
  (interactive
   (let* ((editor (edraw-current-editor))
          (view-box (read-string
                     (edraw-msg "SVG viewBox: ")
                     (or (oref editor svg-document-view-box) ""))))
     (list editor (if (string-empty-p view-box) nil view-box))))

  (with-slots (svg-document-view-box) editor
    (let ((old-value svg-document-view-box))
      (unless (equal new-value old-value)
        (edraw-push-undo
         editor 'document-view-box
         (list 'edraw-set-view-box editor old-value))
        ;; The value set here is reflected to the SVG element by
        ;; `edraw-editor-remove-root-transform' function
        (setq svg-document-view-box new-value)
        (edraw-on-document-changed editor 'document-view-box)))))

;;;;;; Editor - Document - Background

(cl-defmethod edraw-svg-background ((editor edraw-editor))
  (edraw-dom-get-by-id (oref editor svg) edraw-editor-svg-background-id))

(cl-defmethod edraw-background-left ((_editor edraw-editor))
  0)

(cl-defmethod edraw-background-top ((_editor edraw-editor))
  0)

(cl-defmethod edraw-background-right ((editor edraw-editor))
  (edraw-width editor))

(cl-defmethod edraw-background-bottom ((editor edraw-editor))
  (edraw-height editor))

(cl-defmethod edraw-background-width ((editor edraw-editor))
  (edraw-width editor))

(cl-defmethod edraw-background-height ((editor edraw-editor))
  (edraw-height editor))

(cl-defmethod edraw-update-background ((editor edraw-editor))
  (when-let ((element (edraw-svg-background editor)))
    (edraw-svg-set-attr-number element 'x (edraw-background-left editor))
    (edraw-svg-set-attr-number element 'y (edraw-background-top editor))
    (edraw-svg-set-attr-number element 'width (edraw-background-width editor))
    (edraw-svg-set-attr-number element 'height (edraw-background-height editor))))

(cl-defmethod edraw-get-background ((editor edraw-editor))
  (when-let ((element (edraw-svg-background editor)))
    (dom-attr element 'fill)))

(defun edraw-editor-background-fill-empty-p (fill)
  (or (null fill) (string= fill "none") (string-empty-p fill)))

(defun edraw-editor-background-fill-opaque-p (fill)
  (when (stringp fill)
    (when-let ((color (edraw-color-picker-color-from-string
                       fill
                       '((:color-name-scheme . web) (:enable-opacity . t)))))
      (>= (edraw-color-a color) 1))))

(edraw-editor-defcmd edraw-set-background ((editor edraw-editor) fill)
  (interactive
   (let* ((editor (edraw-current-editor))
          (current-value (edraw-get-background editor))
          (new-value
           (edraw-editor-with-silent-modifications
             (unwind-protect
                 (edraw-color-picker-read-color
                  (edraw-msg "Background Color: ")
                  (or current-value "")
                  '("" "none")
                  `((:color-name-scheme . web)
                    (:no-color . "none")
                    (:on-input-change
                     . ,(lambda (string color)
                          (when (or (member string '("" "none"))
                                    color)
                            ;;@todo suppress notification?
                            (edraw-set-background editor string))))
                    (:scale-direct . ,(oref editor image-scale))))
               (edraw-set-background editor current-value)))))
     (list editor new-value)))

  (edraw-push-undo
   editor
   'document-background
   (list 'edraw-set-background editor (edraw-get-background editor)))
  (with-slots (svg) editor
    (if (edraw-editor-background-fill-empty-p fill)
        ;; remove background
        (edraw-dom-remove-by-id svg edraw-editor-svg-background-id)
      (if-let ((element (edraw-svg-background editor)))
          ;; change fill
          (edraw-svg-set-attr-string element 'fill fill)
        ;; add background
        (dom-add-child-before
         svg
         (dom-node 'rect (list (cons 'fill fill)
                               (cons 'id edraw-editor-svg-background-id)))
         (edraw-svg-body editor))
        (edraw-update-background editor)
        (edraw-invalidate-ui-parts editor 'scroll-transform) ;;update <rect transform=>
        ))
    (edraw-invalidate-ui-parts editor 'transparent-bg) ;;update opaque state
    (edraw-on-document-changed editor 'document-background)))

;;;;;; Editor - Document - Shapes

(cl-defmethod edraw-all-shapes ((editor edraw-editor))
  ;; back-to-front
  (delq nil (mapcar (lambda (node)
                      (edraw-shape-from-element node editor 'noerror))
                    (dom-children (edraw-svg-body editor)))))

(cl-defmethod edraw-back-shape ((editor edraw-editor))
  (seq-some (lambda (node) (edraw-shape-from-element node editor 'noerror))
            (dom-children (edraw-svg-body editor))))

(cl-defmethod edraw-front-shape ((editor edraw-editor))
  (seq-some (lambda (node) (edraw-shape-from-element node editor 'noerror))
            (reverse (dom-children (edraw-svg-body editor)))))

(defun edraw-read-translate-params ()
  (edraw-xy
   (read-number (edraw-msg "Delta X: ") 0)
   (read-number (edraw-msg "Delta Y: ") 0)))

(defmacro edraw-set-translate-params ()
  `(unless xy
     (setq xy (edraw-read-translate-params))))

(edraw-editor-defcmd edraw-translate-all-shapes ((editor edraw-editor) xy)
  (interactive
   (let* ((editor (edraw-current-editor))
          (shapes (edraw-all-shapes editor)))
     (when (null shapes)
       (error (edraw-msg "No shapes")))
     (list editor (edraw-read-translate-params))))

  (when-let ((shapes (edraw-all-shapes editor)))
    (edraw-make-undo-group editor 'all-shapes-translate
      (dolist (shape shapes)
        (edraw-translate shape xy)))))

(defun edraw-read-origin-number (axis candidates)
  (let ((prompt (format "Origin %s(%s): "
                        axis
                        (mapconcat
                         (lambda (cand)
                           (format "%s=%g%s" (nth 0 cand) (nth 1 cand)
                                   (if (nth 2 cand) "(default)" "")))
                         candidates
                         ", ")))
        result)
    (while (null result)
      (setq result
            (let ((input (read-string prompt)))
              (cond
               ;; Default
               ((equal input "")
                (nth 1 (seq-find (lambda (cand) (nth 2 cand)) candidates)))
               ;; Candidate
               ((nth 1 (seq-find (lambda (cand)
                                   (string-prefix-p input (nth 0 cand) t))
                                 candidates)))
               ;; Number
               (t (ignore-errors (read input))))))
      (unless (numberp result)
        (message (edraw-msg "Please enter a number, %s, or empty.")
                 (mapconcat #'car candidates ", "))
        (sit-for 2)
        (setq result nil)))
    result))

(defun edraw-read-origin-xy (aabb)
  (edraw-xy
   (if aabb
       (edraw-read-origin-number
        "X"
        `(("left" ,(edraw-rect-left aabb))
          ("center" ,(/ (+ (edraw-rect-left aabb) (edraw-rect-right aabb)) 2) t)
          ("right" ,(edraw-rect-right aabb))))
     (read-number "Origin X: " 0))
   (if aabb
       (edraw-read-origin-number
        "Y"
        `(("top" ,(edraw-rect-top aabb))
          ("center" ,(/ (+ (edraw-rect-top aabb) (edraw-rect-bottom aabb)) 2) t)
          ("bottom" ,(edraw-rect-bottom aabb))))
     (read-number "Origin Y: " 0))))

(defun edraw-read-scale-params (aabb &optional origin-xy sx sy)
  (let* ((sx (or sx (read-number (edraw-msg "Scale X: ") 1.0)))
         (sy (or sy (read-number (edraw-msg "Scale Y: ") sx)))
         (_ (when (and (= sx 1) (= sy 1))
              (error (edraw-msg "No need to scale"))))
         (origin-xy (or origin-xy (edraw-read-origin-xy aabb))))
    (list origin-xy sx sy)))

(defmacro edraw-set-scale-params (aabb-expr)
  `(let ((result (edraw-read-scale-params ,aabb-expr origin-xy sx sy)))
     (setq origin-xy (nth 0 result)
           sx (nth 1 result)
           sy (nth 2 result))))

(edraw-editor-defcmd edraw-scale-all-shapes ((editor edraw-editor)
                                             origin-xy sx sy)
  (interactive
   (let* ((editor (edraw-current-editor))
          (shapes (edraw-all-shapes editor)))
     (when (null shapes)
       (error (edraw-msg "No shapes")))
     (cons
      editor
      (edraw-read-scale-params
       (edraw-rect 0 0 (edraw-width editor) (edraw-height editor))))))

  (when-let ((shapes (edraw-all-shapes editor)))
    (edraw-make-undo-group editor 'all-shapes-scale
      (let ((matrix (edraw-matrix-move-origin-xy (edraw-matrix-scale sx sy 1) origin-xy)))
        (dolist (shape shapes)
          (edraw-transform shape matrix))))))

(defun edraw-read-rotate-params (aabb &optional origin-xy angle)
  (let* ((angle (or angle (read-number (edraw-msg "Angle: ") 0)))
         (_ (when (= (mod angle 360) 0)
              (error (edraw-msg "No need to rotate"))))
         (origin-xy (or origin-xy (edraw-read-origin-xy aabb))))
    (list origin-xy angle)))

(defmacro edraw-set-rotate-params (aabb-expr)
  `(let ((result (edraw-read-rotate-params ,aabb-expr origin-xy angle)))
     (setq origin-xy (nth 0 result)
           angle (nth 1 result))))

(edraw-editor-defcmd edraw-rotate-all-shapes ((editor edraw-editor)
                                              origin-xy angle)
  (interactive
   (let* ((editor (edraw-current-editor))
          (shapes (edraw-all-shapes editor)))
     (when (null shapes)
       (error (edraw-msg "No shapes")))
     (cons
      editor
      (edraw-read-rotate-params (edraw-rect 0 0
                                            (edraw-width editor)
                                            (edraw-height editor))))))
  (when-let ((shapes (edraw-all-shapes editor)))
    (edraw-make-undo-group editor 'all-shapes-rotate
      (let ((matrix (edraw-matrix-move-origin-xy
                     (edraw-matrix-rotate angle) origin-xy)))
        (dolist (shape shapes)
          (edraw-transform shape matrix))))))

(cl-defmethod edraw-notify-document-close-to-all-shapes ((editor edraw-editor))
  (dolist (node (dom-children (edraw-svg-body editor)))
    (when-let ((shape (edraw-shape-from-element-no-create node)))
      (edraw-notify-change-hook shape 'document-close))))

;; Shape Finding

(cl-defmethod edraw-find-shape-by-internal-id ((editor edraw-editor) id)
  (when-let ((element (car (dom-elements
                            (edraw-svg-body editor)
                            'data-edraw-id
                            (concat "\\`" id "\\'")))))
    (edraw-shape-from-element element editor t)))

(cl-defmethod edraw-find-shapes-by-xy ((shapes list) xy)
  (seq-filter
   (lambda (shape)
     (edraw-svg-element-contains-point-p (edraw-element shape) xy))
   shapes))

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

(cl-defmethod edraw-find-shapes-by-rect ((shapes list) rect)
  (seq-filter
   (lambda (shape)
     (edraw-svg-element-intersects-rect-p (edraw-element shape) rect))
   shapes))

(cl-defmethod edraw-find-shapes-by-rect ((editor edraw-editor) rect)
  (nreverse ;;front to back
   (delq nil
         (cl-loop for node in (dom-children (edraw-svg-body editor))
                  when (edraw-svg-element-intersects-rect-p node rect)
                  collect (edraw-shape-from-element node editor 'noerror)))))

;;;;;; Editor - Document - Menu

(cl-defmethod edraw-get-actions-for-document ((editor edraw-editor))
  (edraw-filter-menu
   editor
   'context-document
   `(((edraw-msg "Document")
      (((edraw-msg "Set Background...") edraw-editor-set-background)
       ((edraw-msg "Resize...") edraw-editor-set-size)
       ((edraw-msg "Crop...") edraw-editor-crop)
       ((edraw-msg "View Box...") edraw-editor-set-view-box)
       ((edraw-msg "Transform")
        (((edraw-msg "Translate All...") edraw-editor-translate-all-shapes)
         ((edraw-msg "Scale All...") edraw-editor-scale-all-shapes)
         ((edraw-msg "Rotate All...") edraw-editor-rotate-all-shapes)
         ,(edraw-transform-method-menu editor)))
       ((edraw-msg "Clear...") edraw-editor-clear)
       ((edraw-msg "Export to Buffer") edraw-editor-export-to-buffer)
       ((edraw-msg "Export to File") edraw-editor-export-to-file)))
     ((edraw-msg "Select All") edraw-editor-toggle-selection-all
      :visible ,(not (edraw-selected-shapes editor))
      :enable ,(not (edraw-selected-shapes editor)))
     ((edraw-msg "Deselect All") edraw-editor-toggle-selection-all
      :visible ,(not (null (edraw-selected-shapes editor)))
      :enable ,(not (null (edraw-selected-shapes editor))))
     ((edraw-msg "Undo") edraw-editor-undo
      :enable ,(not (edraw-empty-undo-p editor)))
     ((edraw-msg "Redo") edraw-editor-redo
      :enable ,(not (edraw-empty-redo-p editor)))
     ((edraw-msg "Paste") edraw-editor-paste-and-select
      :enable ,(not (edraw-clipboard-empty-p))))))

(cl-defmethod edraw-popup-context-menu-for-document ((editor edraw-editor))
  (let ((edraw-current-editor editor))
    (edraw-popup-menu
     (edraw-msg "Document")
     (edraw-get-actions-for-document editor)
     editor)))

;;;;; Editor - View
;;;;;; Editor - View - SVG Image Update

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
    (edraw-update-ui-parts editor)
    (setq image (edraw-svg-to-image svg
                                    :scale 1.0)) ;;Cancel image-scale effect
    (overlay-put overlay 'display image)))

;;;;;; Editor - View - SVG Structure

;; <svg width= height= viewBox=> ;;Document(width=, height=, viewBox=)
;;   <defs id="edraw-ui-defs"></defs> ;;UI
;;   <g id="edraw-ui-background"> ;;UI
;;     <g id="edraw-ui-transparent-bg">...</g>
;;   </g>
;;   <defs id="edraw-defs"> ;;Document
;;     <linearGradient>
;;     <radialGradient>
;;     <marker>
;;   </defs>
;;   <rect id="edraw-background" /> ;;Document
;;   <g id="edraw-body"> ;;Document
;;     <rect />
;;     <ellipse />
;;     <text />
;;     <path />
;;     <image />
;;   </g>
;;   <g id="edraw-ui-foreground"> ;;UI
;;     <style id="edraw-ui-style"></style>
;;     <g id="edraw-ui-grid">...</g>
;;     <g id="edraw-ui-shape-points">...</g>
;;   </g>
;; </svg>
;; ;; elements #edraw-ui-* are removed on save

(cl-defmethod edraw-initialize-svg ((editor edraw-editor))
  "Allocate the elements needed for the editor to work in the svg tree."
  (with-slots (svg) editor
    ;; Document Elements
    (edraw-initialize-svg-document editor) ;;set `svg'
    ;; UI Elements (Background)
    (edraw-ui-background-svg editor) ;;insert first
    (edraw-ui-defs-svg editor) ;;insert first
    (edraw-initialize-transparent-bg editor)
    (edraw-update-transparent-bg editor)
    ;; UI Elements (Foreground)
    (let ((fore-g (edraw-dom-get-or-create svg 'g "edraw-ui-foreground")))
      (edraw-dom-get-or-create fore-g 'style "edraw-ui-style")
      (edraw-dom-get-or-create fore-g 'g "edraw-ui-grid"))
    (edraw-update-ui-style-svg editor)
    (edraw-update-grid editor)
    ;;(edraw-update-selection-ui editor)

    (edraw-update-root-transform editor) ;;<svg width= height= viewBox=>
    (edraw-update-scroll-transform editor) ;;background, body transform=
    ))

(cl-defmethod edraw-invalidate-ui-parts ((editor edraw-editor) part)
  (with-slots (invalid-ui-parts) editor
    (cond
     ((eq invalid-ui-parts 'all)
      nil)
     ((eq part 'all)
      (setq invalid-ui-parts 'all)
      (edraw-invalidate-image editor))
     ((listp invalid-ui-parts)
      (unless (memq part invalid-ui-parts)
        (push part invalid-ui-parts)
        (edraw-invalidate-image editor))))))

(cl-defmethod edraw-update-ui-parts ((editor edraw-editor))
  (with-slots (invalid-ui-parts) editor
    (edraw-update-ui-parts-immediate editor invalid-ui-parts)
    (setq invalid-ui-parts nil)))

(cl-defmethod edraw-update-ui-parts-immediate ((editor edraw-editor)
                                               invalid-ui-parts)
  ;; Target: svg width= height= viewBox=
  ;; Deps: view-size
  (when (or (eq invalid-ui-parts 'all) (memq 'root-transform invalid-ui-parts))
    (edraw-update-root-transform editor))
  ;; Target: rect#edraw-background transform=
  ;;          g#edraw-body transform=
  ;; Deps: scroll-transform
  (when (or (eq invalid-ui-parts 'all) (memq 'scroll-transform invalid-ui-parts))
    (edraw-update-scroll-transform editor))
  ;; Target: g#edraw-ui-background > g#edraw-ui-transparent-bg
  ;; Deps: background state, view-size
  (when (or (eq invalid-ui-parts 'all) (memq 'transparent-bg invalid-ui-parts))
    (edraw-update-transparent-bg editor))
  ;; Target: g#edraw-ui-foreground > g#edraw-ui-style
  ;;(edraw-update-ui-style-svg editor) ;; Immutable
  ;; Target: g#edraw-ui-foreground > g#edraw-ui-grid
  ;; Deps: scroll-transform, view-size
  (when (or (eq invalid-ui-parts 'all) (memq 'grid invalid-ui-parts))
    (edraw-update-grid editor))
  ;; Target: g#edraw-ui-foreground > #edraw-ui-shape-points
  ;; Deps: selection state, scroll-transform, view-size
  (when (or (eq invalid-ui-parts 'all) (memq 'selection-ui invalid-ui-parts))
    (edraw-update-selection-ui editor)))



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
  /*[Too Slow] mix-blend-mode: difference; */
}
.edraw-ui-axis-line {
  stroke: rgba(255, 50, 30, 0.75);
  stroke-dasharray: 2;
  /*[Too Slow] mix-blend-mode: difference; */
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
.edraw-ui-shape-boundary {
  stroke: #f88; fill: none;
  stroke-dasharray: 4,2;
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
    (when svg
      (let ((width (edraw-scroll-view-width editor))
            (height (edraw-scroll-view-height editor)))
        (edraw-svg-set-attr-number svg 'width
                                   (ceiling (* image-scale width)))
        (edraw-svg-set-attr-number svg 'height
                                   (ceiling (* image-scale height)))
        (edraw-svg-set-attr-string svg 'viewBox
                                   (format "0 0 %s %s" width height))))))

(defun edraw-editor-remove-root-transform (svg svg-document-size
                                               svg-document-view-box)
  (when svg
    (edraw-svg-set-attr-number svg 'width (car svg-document-size))
    (edraw-svg-set-attr-number svg 'height (cdr svg-document-size))
    (if svg-document-view-box
        (edraw-svg-set-attr-string svg 'viewBox svg-document-view-box)
      (dom-remove-attribute svg 'viewBox)))
  svg)

(defun edraw-editor-remove-ui-element-from-svg (svg)
  ;;@todo remove :-edraw attributes
  (let ((svg (copy-tree svg)))
    (when-let ((body (edraw-dom-get-by-id svg edraw-editor-svg-body-id)))
      (edraw-dom-remove-attr body 'transform))

    (dolist (elem (dom-by-id svg "^edraw-ui-"))
      (dom-remove-node svg elem))
    svg))

;;;;;; Editor - View - Transparent Background

(cl-defmethod edraw-initialize-transparent-bg ((editor edraw-editor))
  (dom-append-child
   (edraw-ui-defs-svg editor)
   (edraw-svg-ui-transparent-bg-pattern)))

(cl-defmethod edraw-update-transparent-bg ((editor edraw-editor))
  (let ((back-ui (edraw-ui-background-svg editor)))
    (edraw-dom-remove-by-id back-ui "edraw-ui-transparent-bg")
    (when (and (edraw-get-transparent-bg-visible editor)
               ;; Is not visible area covered by opaque background?
               (not (and (edraw-editor-background-fill-opaque-p
                          (edraw-get-background editor))
                         (<= (edraw-background-left editor)
                             (edraw-scroll-visible-area-left editor))
                         (>= (edraw-background-right editor)
                             (edraw-scroll-visible-area-right editor))
                         (<= (edraw-background-top editor)
                             (edraw-scroll-visible-area-top editor))
                         (>= (edraw-background-bottom editor)
                             (edraw-scroll-visible-area-bottom editor)))))
      (edraw-dom-insert-first
       back-ui
       (edraw-svg-ui-transparent-bg
        (edraw-scroll-view-width editor)
        (edraw-scroll-view-height editor))))))

(cl-defmethod edraw-get-transparent-bg-visible ((editor edraw-editor))
  (edraw-get-setting editor 'transparent-bg-visible))

(cl-defmethod edraw-set-transparent-bg-visible ((editor edraw-editor) visible)
  (edraw-set-setting editor 'transparent-bg-visible visible)
  (edraw-invalidate-ui-parts editor 'transparent-bg)
  visible)

(edraw-editor-defcmd edraw-toggle-transparent-bg-visible ((editor edraw-editor))
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

;;;;;; Editor - View - Grid

(cl-defmethod edraw-update-grid ((editor edraw-editor))
  (with-slots (svg) editor
    (when-let ((g (edraw-dom-get-by-id svg "edraw-ui-grid")))
      (edraw-dom-remove-all-children g)
      (when (edraw-get-setting editor 'grid-visible)
        (let* ((interval (edraw-get-setting editor 'grid-interval))
               (scaled-interval (* interval (edraw-scroll-scale editor)))
               (step-lines (if (< scaled-interval edraw-grid-display-min-interval)
                               (ceiling
                                (/ (float edraw-grid-display-min-interval)
                                   scaled-interval))
                             1))
               (step-interval (* step-lines interval))
               (x0 (edraw-grid-ceil (edraw-scroll-visible-area-left editor)
                                    step-interval))
               (y0 (edraw-grid-ceil (edraw-scroll-visible-area-top editor)
                                    step-interval))
               (x1 (edraw-grid-ceil (edraw-scroll-visible-area-right editor)
                                    step-interval))
               (y1 (edraw-grid-ceil (edraw-scroll-visible-area-bottom editor)
                                    step-interval))
               (view-x-min 0)
               (view-y-min 0)
               (view-x-max (edraw-scroll-view-width editor))
               (view-y-max (edraw-scroll-view-height editor)))
          (cl-loop for x from x0 to x1 by step-interval
                   for xv = (edraw-scroll-transform-x editor x)
                   do (svg-line g xv view-y-min xv view-y-max
                                :class (if (= x 0)
                                           "edraw-ui-axis-line"
                                         "edraw-ui-grid-line")))
          (cl-loop for y from y0 to y1 by step-interval
                   for yv = (edraw-scroll-transform-y editor y)
                   do (svg-line g view-x-min yv view-x-max yv
                                :class (if (= y 0)
                                           "edraw-ui-axis-line"
                                         "edraw-ui-grid-line"))))

        ;;(edraw-dom-invalidate g)
        ))))

(cl-defmethod edraw-set-grid-visible ((editor edraw-editor) visible)
  (edraw-set-setting editor 'grid-visible visible)
  (edraw-invalidate-ui-parts editor 'grid))

(cl-defmethod edraw-get-grid-visible ((editor edraw-editor))
  (edraw-get-setting editor 'grid-visible))

(edraw-editor-defcmd edraw-toggle-grid-visible ((editor edraw-editor))
  (edraw-set-grid-visible editor
                          (not (edraw-get-grid-visible editor))))

(edraw-editor-defcmd edraw-set-grid-interval ((editor edraw-editor) interval)
  (interactive
   (let ((editor (edraw-current-editor)))
     (list
      editor
      (read-number (edraw-msg "Grid Interval: ")
                   (edraw-get-setting editor 'grid-interval)))))

  (edraw-set-setting editor 'grid-interval interval)
  (edraw-invalidate-ui-parts editor 'grid))

;;;;;; Editor - View - Selection UI

(cl-defmethod edraw-update-selection-ui ((editor edraw-editor))
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (if selected-shapes
        ;; Show points
        (let ((g (edraw-svg-ui-shape-points-create-group
                  (edraw-ui-foreground-svg editor))))
          (dolist (shape selected-shapes)
            (edraw-svg-ui-shape-points g editor
                                       shape selected-anchor selected-handle))
          ;; Boundary
          (dolist (shape selected-shapes)
            ;; Local AABB
            (when (memq (edraw-shape-type shape) '(text image g))
              (let ((aabb (edraw-shape-aabb-local shape)))
                (unless (edraw-rect-empty-p aabb)
                  (let* ((matrix
                          (edraw-matrix-mul-mat-mat
                           (edraw-scroll-transform-matrix editor)
                           (edraw-transform-prop-get-matrix shape)))
                         (points (mapcar
                                  (lambda (xy)
                                    (edraw-matrix-mul-mat-xy matrix xy))
                                  (edraw-rect-corner-points aabb))))
                    (svg-polygon g points
                                 :class
                                 "edraw-ui-shape-boundary")))))
            ;; ;; Global AABB
            ;; (when (memq (edraw-shape-type shape) '(text image g))
            ;;   (let ((aabb (edraw-scroll-transform-rect
            ;;                editor (edraw-shape-aabb shape))))
            ;;     (unless (edraw-rect-empty-p aabb)
            ;;       (svg-rectangle g
            ;;                      (edraw-rect-left aabb)
            ;;                      (edraw-rect-top aabb)
            ;;                      (edraw-rect-width aabb)
            ;;                      (edraw-rect-height aabb)
            ;;                      :class
            ;;                      "edraw-ui-shape-boundary"))))
            ))
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
                                  editor
                                  shape
                                  selected-anchor
                                  selected-handle)
  (let ((anchor-points (edraw-get-anchor-points shape))
        (prev-selected-anchor (when selected-anchor
                                (edraw-previous-anchor selected-anchor)))
        (next-selected-anchor (when selected-anchor
                                (edraw-next-anchor selected-anchor))))
    (dolist (anchor anchor-points)
      (let ((anchor-xy
             (edraw-scroll-transform-xy editor
                                        (edraw-get-xy-transformed anchor)))
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
               (edraw-scroll-transform-xy editor
                                          (edraw-get-xy-transformed handle))
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

;;;;;; Editor - View - Scroll and Zoom

;;;;;;; Get Parameters

(cl-defmethod edraw-scroll-pos-x ((editor edraw-editor))
  (nth 0 (oref editor scroll-transform)))

(cl-defmethod edraw-scroll-pos-y ((editor edraw-editor))
  (nth 1 (oref editor scroll-transform)))

(cl-defmethod edraw-scroll-scale ((editor edraw-editor))
  (nth 2 (oref editor scroll-transform)))

;;;;;;; Transform Coordinates

;; Scale then translate. To avoid problems with floating point numbers.

(cl-defmethod edraw-scroll-transform-xy ((editor edraw-editor) xy)
  (when xy
    (let ((scale (edraw-scroll-scale editor)))
      (edraw-xy
       (+ (* scale (edraw-x xy)) (edraw-scroll-pos-x editor))
       (+ (* scale (edraw-y xy)) (edraw-scroll-pos-y editor))))))

(cl-defmethod edraw-scroll-transform-x ((editor edraw-editor) x)
  (+ (* (edraw-scroll-scale editor) x) (edraw-scroll-pos-x editor)))

(cl-defmethod edraw-scroll-transform-y ((editor edraw-editor) y)
  (+ (* (edraw-scroll-scale editor) y) (edraw-scroll-pos-y editor)))

(cl-defmethod edraw-scroll-transform-rect ((editor edraw-editor) rect)
  (edraw-rect-pp
   (edraw-scroll-transform-xy editor (edraw-rect-xy0 rect))
   (edraw-scroll-transform-xy editor (edraw-rect-xy1 rect))))

(cl-defmethod edraw-scroll-transform-matrix ((editor edraw-editor))
  (let ((scale (edraw-scroll-scale editor))
        (dx (edraw-scroll-pos-x editor))
        (dy (edraw-scroll-pos-y editor)))
    (edraw-matrix-mul-mat-mat
     (edraw-matrix-translate dx dy 0)
     (edraw-matrix-scale scale scale 1))))

(cl-defmethod edraw-scroll-reverse-transform-xy ((editor edraw-editor) xy)
  (when xy
    (let ((scale (edraw-scroll-scale editor)))
      (edraw-xy
       (/ (float (- (edraw-x xy) (edraw-scroll-pos-x editor))) scale)
       (/ (float (- (edraw-y xy) (edraw-scroll-pos-y editor))) scale)))))

;;;;;;; Screen Area

(cl-defmethod edraw-scroll-view-width ((editor edraw-editor))
  (with-slots (view-size) editor
    (if view-size
        (car view-size)
      (edraw-width editor))))

(cl-defmethod edraw-scroll-view-height ((editor edraw-editor))
  (with-slots (view-size) editor
    (if view-size
        (cdr view-size)
      (edraw-height editor))))

(cl-defmethod edraw-scroll-view-xy-from-mouse-event ((editor edraw-editor) event)
  (edraw-xy-round ;;@todo round or not?
   (edraw-mouse-event-to-xy-on-scroll-view editor event)))

;;;;;;; Visible Area

(cl-defmethod edraw-scroll-visible-area-left ((editor edraw-editor))
  (/ (- (edraw-scroll-pos-x editor)) (edraw-scroll-scale editor)))

(cl-defmethod edraw-scroll-visible-area-top ((editor edraw-editor))
  (/ (- (edraw-scroll-pos-y editor)) (edraw-scroll-scale editor)))

(cl-defmethod edraw-scroll-visible-area-right ((editor edraw-editor))
  (/ (- (edraw-scroll-view-width editor) (edraw-scroll-pos-x editor))
     (edraw-scroll-scale editor)))

(cl-defmethod edraw-scroll-visible-area-bottom ((editor edraw-editor))
  (/ (- (edraw-scroll-view-height editor) (edraw-scroll-pos-y editor))
     (edraw-scroll-scale editor)))

(cl-defmethod edraw-scroll-visible-area-width ((editor edraw-editor))
  (/ (edraw-scroll-view-width editor) (edraw-scroll-scale editor)))

(cl-defmethod edraw-scroll-visible-area-height ((editor edraw-editor))
  (/ (edraw-scroll-view-height editor) (edraw-scroll-scale editor)))

;;;;;;; SVG Manipulation

(cl-defmethod edraw-update-scroll-transform ((editor edraw-editor))
  (with-slots (svg) editor
    (when svg
      (let ((background (edraw-svg-background editor)) ;;element or nil
            (body (edraw-svg-body editor))
            (transform (format "translate(%s %s) scale(%s)"
                               (edraw-scroll-pos-x editor)
                               (edraw-scroll-pos-y editor)
                               (edraw-scroll-scale editor))))
        (when background
          (edraw-svg-set-attr-string background 'transform transform)) ;;@todo adjust width height? I think there will be lines at the right and bottom edges of the image
        (when body
          (edraw-svg-set-attr-string body 'transform transform))))))

(defun edraw-editor-remove-scroll-transform (svg)
  (when svg
    (let ((background (edraw-dom-get-by-id svg edraw-editor-svg-background-id))
          (body (edraw-dom-get-by-id svg edraw-editor-svg-body-id))) ;;Do not use (edraw-svg-body editor)
      (when background
        (edraw-dom-remove-attr background 'transform))
      (when body
        (edraw-dom-remove-attr body 'transform)))))

;;;;;;; Set Parameters

(cl-defmethod edraw-set-scroll-transform ((editor edraw-editor) x y scale)
  (with-slots (scroll-transform) editor
    (setq scroll-transform
          (list
           (if x (round x) ;;Keep the scroll amount as an integer
             (nth 0 scroll-transform))
           (if y (round y) ;;Keep the scroll amount as an integer
             (nth 1 scroll-transform))
           (or scale
               (nth 2 scroll-transform))))
    (edraw-invalidate-ui-parts editor 'all)))

(cl-defmethod edraw-enlarge-view-automatically ((editor edraw-editor) new-scale)
  (when (and (> new-scale 1.0)
             edraw-editor-auto-view-enlargement-max-size
             (null (edraw-get-setting editor 'view-size-spec)));;when not specified by user
    (with-slots (view-size) editor
      (let ((curr-view-w (edraw-scroll-view-width editor))
            (curr-view-h (edraw-scroll-view-height editor)))
        (let ((new-view-w
               (edraw-clamp
                (* (edraw-width editor) new-scale)
                curr-view-w
                (car edraw-editor-auto-view-enlargement-max-size)))
              (new-view-h
               (edraw-clamp
                (* (edraw-height editor) new-scale)
                curr-view-h
                (cdr edraw-editor-auto-view-enlargement-max-size))))
          (when (or (> new-view-w curr-view-w)
                    (> new-view-h curr-view-h))
            (setq view-size (cons new-view-w new-view-h))
            ;; It is the caller's responsibility to update SVG
            ))))))

;;;;;;; Commands

(edraw-editor-defcmd edraw-reset-scroll-and-zoom ((editor edraw-editor))
  (edraw-set-scroll-transform editor 0 0 1))

(cl-defmethod edraw-scroll ((editor edraw-editor) dx dy)
  (edraw-set-scroll-transform
   editor
   (when dx (+ (edraw-scroll-pos-x editor) dx))
   (when dy (+ (edraw-scroll-pos-y editor) dy))
   nil))

(cl-defmethod edraw-zoom ((editor edraw-editor) magnification &optional cx cy)
  ;; (cx, cy) are the coordinates in view-width/height.

  (let* ((old-scale (edraw-scroll-scale editor))
         (new-scale (* old-scale magnification))
         (old-view-w (edraw-scroll-view-width editor))
         (old-view-h (edraw-scroll-view-height editor))
         ;; Enlarge editing view automatically
         (_ (edraw-enlarge-view-automatically editor new-scale))
         (new-view-w (edraw-scroll-view-width editor))
         (new-view-h (edraw-scroll-view-height editor)))

    (unless cx (setq cx (/ old-view-w 2)))
    (unless cy (setq cy (/ old-view-h 2)))

    (edraw-set-scroll-transform
     editor
     ;; Distributes the amount that the scaled view size protrudes
     ;; from the new view size by the ratio of cx and old-view-w.
     ;;  (old-view-w * magnification - new-view-w) * (cx / old-view-w)
     (- (* (edraw-scroll-pos-x editor) magnification)
        (/ (* (- (* old-view-w (float magnification)) new-view-w) cx)
           old-view-w))
     (- (* (edraw-scroll-pos-y editor) magnification)
        (/ (* (- (* old-view-h (float magnification)) new-view-h) cy)
           old-view-h))
     (* (edraw-scroll-scale editor) magnification))))

(edraw-editor-defcmd edraw-zoom-in ((editor edraw-editor))
  (edraw-zoom editor 2))

(edraw-editor-defcmd edraw-zoom-out ((editor edraw-editor))
  (edraw-zoom editor 0.5))

(defun edraw-editor-zoom-by-mouse (event magnification)
  (when-let* ((editor (edraw-editor-at-input event))
              (center-xy-on-screen
               (edraw-scroll-view-xy-from-mouse-event editor event)))
    (edraw-zoom editor magnification
                (edraw-x center-xy-on-screen)
                (edraw-y center-xy-on-screen))))

(defun edraw-editor-zoom-in-by-mouse (event)
  (interactive "e")
  (edraw-editor-zoom-by-mouse event 2))

(defun edraw-editor-zoom-out-by-mouse (event)
  (interactive "e")
  (edraw-editor-zoom-by-mouse event 0.5))

(defun edraw-editor-scroll-by-arrow-key (&optional editor n)
  (interactive "i\np")
  (let ((event last-input-event))
    (when-let ((editor (or editor (edraw-editor-at-input event))))
      (let* ((mods (event-modifiers event))
             (d (* (or n 1)
                   (cond
                    ((memq 'meta mods) (read-number (edraw-msg "Moving Distance: ") 20))
                    ((memq 'shift mods) 50)
                    (t 1))))
             (v (pcase (event-basic-type event)
                  ('left (cons (- d) 0))
                  ('right (cons d 0))
                  ('up (cons 0 (- d)))
                  ('down (cons 0 d))
                  (_ (cons 0 0)))))
        (edraw-scroll editor (car v) (cdr v))))))

(defun edraw-editor-scroll-by-dragging (down-event)
  (interactive "e")
  (when-let* ((editor (edraw-editor-at-input down-event)))
    ;; Wait for mouse down event
    (unless (consp down-event)
      (setq down-event nil)
      (while (null down-event)
        (let ((new-event (read-event)))
          (cond
           ((eq (car-safe new-event) 'down-mouse-1)
            (setq down-event new-event))))))

    ;; Drag and scroll
    (let ((button (event-basic-type down-event)))
      (unless (memq button '(mouse-1 mouse-2 mouse-3))
        (error "edraw-editor-scroll-by-dragging does not support events other than mouse-1 through mouse-3."))
      (unless (memq 'down (event-modifiers down-event))
        (error "edraw-editor-scroll-by-dragging requires a down event."))

      (let ((scroll-xy-start (edraw-xy (edraw-scroll-pos-x editor)
                                       (edraw-scroll-pos-y editor)))
            (down-xy (edraw-scroll-view-xy-from-mouse-event
                      editor down-event)))
        (edraw-track-dragging
         down-event
         (lambda (move-event)
           (let* ((move-xy (edraw-scroll-view-xy-from-mouse-event
                            editor move-event))
                  (new-scroll-xy(edraw-xy-add scroll-xy-start
                                              (edraw-xy-sub move-xy down-xy))))
             (edraw-set-scroll-transform
              editor
              (edraw-x new-scroll-xy)
              (edraw-y new-scroll-xy)
              nil))))))))

(edraw-editor-defcmd edraw-interactive-scroll-and-zoom ((editor edraw-editor))
  (let (quit)
    (while (not quit)
      (let ((event (read-event
                    (edraw-msg "drag:Scroll, wheel:Zoom, 0:reset, q/r-click:quit"))))
        (cond
         ;; Drag and scroll
         ((eq (car-safe event) 'down-mouse-1)
          (edraw-editor-scroll-by-dragging event))
         ((or (eq (car-safe event) 'mouse-3)
              (eq event ?q)
              (eq event ? ))
          (setq quit t))
         ((eq (car-safe event) mouse-wheel-down-event)
          (edraw-zoom-in editor))
         ((eq (car-safe event) mouse-wheel-up-event)
          (edraw-zoom-out editor))
         ((eq event ?0)
          (edraw-reset-scroll-and-zoom editor))
         ((memq (event-basic-type event) '(left up right down))
          (edraw-editor-scroll-by-arrow-key editor)
          ))))))

(edraw-editor-defcmd edraw-set-view-size-spec ((editor edraw-editor) wh)
  (interactive
   (let* ((editor (edraw-current-editor))
          (w (edraw-read-integer-or-nil
              (edraw-msg "View Width(or Empty): ") (edraw-scroll-view-width editor)))
          (h (when w
               (edraw-read-integer
                (edraw-msg "View Height: ") (edraw-scroll-view-height editor)))))
     (list editor (if (and w h) (cons w h)))))

  (edraw-set-setting editor 'view-size-spec wh) ;;nil or (w . h)
  (setf (oref editor view-size) wh)
  (edraw-invalidate-ui-parts editor 'all))

(edraw-editor-defcmd edraw-reset-view-size ((editor edraw-editor))
  (edraw-set-view-size-spec editor nil))

(edraw-editor-defcmd edraw-reset-view ((editor edraw-editor))
  (edraw-reset-view-size editor)
  (edraw-reset-scroll-and-zoom editor))


;;;;; Editor - Selection

(cl-defmethod edraw-last-selected-shape ((editor edraw-editor))
  (car (last (oref editor selected-shapes))))

(cl-defmethod edraw-selected-shapes ((editor edraw-editor))
  (oref editor selected-shapes))

(cl-defgeneric edraw-selected-shapes-aabb (selector)
  "Return axis aligned bounding box of shapes selected by SELECTOR.")
(cl-defmethod edraw-selected-shapes-aabb ((editor edraw-editor))
  (edraw-shape-aabb (edraw-selected-shapes editor)))

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
      (edraw-invalidate-ui-parts editor 'selection-ui)

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
      (edraw-invalidate-ui-parts editor 'selection-ui)
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

(cl-defmethod edraw-select-shapes ((editor edraw-editor) shapes)
  (edraw-deselect-all-shapes editor)
  (dolist (shape shapes) ;;@todo Add edraw-add-shapes-selection?
    (edraw-add-shape-selection editor shape))
  shapes)

(cl-defmethod edraw-select-all-shapes ((editor edraw-editor))
  (edraw-select-shapes editor (edraw-all-shapes editor)))

(edraw-editor-defcmd edraw-toggle-selection-all ((editor edraw-editor))
  (if (edraw-selected-shapes editor)
      (edraw-deselect-all-shapes editor)
    (edraw-select-all-shapes editor)))


(cl-defmethod edraw-on-selected-shape-changed ((editor edraw-editor)
                                               shape type)
  ;;(message "changed!! %s" type)

  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (cond
     ((eq type 'shape-remove)
      (edraw-remove-shape-selection editor shape))

     ((eq type 'shape-path-data)
      (edraw-deselect-anchor editor) ;; All anchors have been destroyed
      (edraw-invalidate-ui-parts editor 'selection-ui))

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
      (edraw-invalidate-ui-parts editor 'selection-ui)))))

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
      (edraw-invalidate-ui-parts editor 'selection-ui))))

(cl-defmethod edraw-deselect-anchor ((editor edraw-editor))
  (with-slots (selected-anchor selected-handle) editor
    (when selected-anchor
      (setq selected-handle nil)
      (setq selected-anchor nil)
      (edraw-invalidate-ui-parts editor 'selection-ui))))

(cl-defmethod edraw-select-handle ((editor edraw-editor) handle)
  (with-slots (selected-anchor selected-handle) editor
    (when (and handle
               (edraw-point-in-selected-shapes-p editor handle))

      (setq selected-anchor (edraw-parent-anchor handle))
      (setq selected-handle handle)
      (edraw-invalidate-ui-parts editor 'selection-ui))))

(cl-defmethod edraw-deselect-handle ((editor edraw-editor))
  (with-slots (selected-anchor selected-handle) editor
    (when selected-handle
      (setq selected-handle nil)
      (edraw-invalidate-ui-parts editor 'selection-ui))))

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
      ((edraw-msg "Duplicate") edraw-editor-duplicate-selected-shapes)
      ((edraw-msg "Group") edraw-editor-group-selected-shapes)
      ((edraw-msg "Transform")
       (((edraw-msg "Translate...") edraw-editor-translate-selected)
        ((edraw-msg "Scale...") edraw-editor-scale-selected)
        ((edraw-msg "Rotate...") edraw-editor-rotate-selected)
        ,(edraw-transform-method-menu editor)))
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
  (let ((edraw-current-editor editor))
    (edraw-popup-menu
     (edraw-get-summary-for-selected-shapes editor)
     (edraw-get-actions-for-selected-shapes editor)
     editor)))

(defvar edraw-editor-move-selected-by-arrow-key--last-op nil)
(defvar edraw-editor-move-selected-by-arrow-key--delta nil)
(defvar edraw-editor-move-selected-by-arrow-key--last-undo-pushed-p nil)

(defun edraw-editor-move-selected-by-arrow-key (&optional editor n)
  (interactive "i\nP")
  (let ((event last-input-event))
    (unless editor (setq editor (edraw-editor-at-input event)))
    (when editor
      (let* ((mods (event-modifiers event))
             (d (* (if (consp n)
                       (read-number (edraw-msg "Moving Distance: ") 20)
                     (prefix-numeric-value n))
                   (if (memq 'shift mods) 10 1))) ;;@todo customize?
             (v (pcase (event-basic-type event)
                  ('left (edraw-xy (- d) 0))
                  ('right (edraw-xy d 0))
                  ('up (edraw-xy 0 (- d)))
                  ('down (edraw-xy 0 d))
                  (_ (edraw-xy 0 0))))
             (op (if (and (memq 'meta mods)
                          ;; Do not duplicate anchor or handle
                          (null (edraw-selected-anchor editor))
                          (null (edraw-selected-handle editor)))
                     'duplicate
                   'translate)))
        (pcase op
          ('duplicate
           (edraw-make-undo-group editor
               'selected-shapes-duplicate-and-translate
             (edraw-duplicate-selected-shapes editor)
             (edraw-translate-selected editor v)
             (edraw-display-selected-object-coordinates editor)))

          ('translate
           ;; Combine consecutive translations into a single operation
           (if (and (eq last-command
                        'edraw-editor-move-selected-by-arrow-key)
                    (eq edraw-editor-move-selected-by-arrow-key--last-op
                        'translate)
                    edraw-editor-move-selected-by-arrow-key--last-undo-pushed-p)
               ;; Undo using last pushed undo data and accumulate vector
               (progn
                 (edraw-undo editor)
                 (setq edraw-editor-move-selected-by-arrow-key--delta
                       (edraw-xy-add
                        edraw-editor-move-selected-by-arrow-key--delta v)))
             (setq edraw-editor-move-selected-by-arrow-key--delta v))
           ;; Translate
           (edraw-make-undo-group editor
               'selected-shapes-translate-by-arrow-key
             (edraw-translate-selected
              editor
              edraw-editor-move-selected-by-arrow-key--delta)
             ;; Is UNDO data pushed?
             (setq edraw-editor-move-selected-by-arrow-key--last-undo-pushed-p
                   (not (edraw-empty-undo-p editor))))
           (edraw-display-selected-object-coordinates editor)))
        ;; Record last operation type
        (setq edraw-editor-move-selected-by-arrow-key--last-op op)))))

(cl-defmethod edraw-display-selected-object-coordinates ((editor edraw-editor))
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (cond
     ((or selected-handle selected-anchor)
      (let ((xy (edraw-get-xy (or selected-handle selected-anchor))))
        (message "X:%s Y:%s" (edraw-x xy) (edraw-y xy))))
     (selected-shapes
      (let ((rect (edraw-shape-aabb selected-shapes)))
        (message "L:%s T:%s R:%s B:%s CX:%s CY:%s"
                 (edraw-rect-left rect)
                 (edraw-rect-top rect)
                 (edraw-rect-right rect)
                 (edraw-rect-bottom rect)
                 (edraw-rect-cx rect)
                 (edraw-rect-cy rect)))))))

(edraw-editor-defcmd edraw-translate-selected ((editor edraw-editor) xy)
  (interactive
   (let ((editor (edraw-current-editor)))
     (when (null (edraw-selected-shapes editor))
       (error (edraw-msg "No shape selected")))
     (list
      editor
      (edraw-read-translate-params))))
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (cond
     (selected-handle
      (edraw-move-on-transformed
       selected-handle
       (edraw-xy-add (edraw-get-xy-transformed selected-handle) xy)))
     (selected-anchor
      (edraw-move-on-transformed
       selected-anchor
       (edraw-xy-add (edraw-get-xy-transformed selected-anchor) xy)))
     (selected-shapes
      (edraw-make-undo-group editor 'selected-shapes-translate
        (dolist (shape selected-shapes)
          (edraw-translate shape xy)))))))

(edraw-editor-defcmd edraw-scale-selected ((editor edraw-editor)
                                           origin-xy sx sy)
  (interactive
   (let ((editor (edraw-current-editor)))
     (when (null (edraw-selected-shapes editor))
       (error (edraw-msg "No shape selected")))
     (cons
      editor
      (edraw-read-scale-params (edraw-selected-shapes-aabb editor)))))
  (edraw-transform-selected
   editor
   (edraw-matrix-move-origin-xy (edraw-matrix-scale sx sy 1) origin-xy)))

(edraw-editor-defcmd edraw-rotate-selected ((editor edraw-editor)
                                            origin-xy angle)
  (interactive
   (let ((editor (edraw-current-editor)))
     (when (null (edraw-selected-shapes editor))
       (error (edraw-msg "No shape selected")))
     (cons
      editor
      (edraw-read-rotate-params (edraw-selected-shapes-aabb editor)))))
  (edraw-transform-selected
   editor
   (edraw-matrix-move-origin-xy (edraw-matrix-rotate angle) origin-xy)))

(cl-defmethod edraw-transform-selected ((editor edraw-editor) matrix)
  (unless (edraw-matrix-identity-p matrix)
    (with-slots (selected-shapes selected-anchor selected-handle) editor
      (cond
       (selected-handle
        (edraw-move-on-transformed selected-handle
                                   (edraw-matrix-mul-mat-xy
                                    matrix
                                    (edraw-get-xy-transformed selected-handle))))
       (selected-anchor
        (edraw-move-on-transformed selected-anchor
                                   (edraw-matrix-mul-mat-xy
                                    matrix
                                    (edraw-get-xy-transformed selected-anchor))))
       (selected-shapes
        (edraw-make-undo-group editor 'selected-shapes-transform
          (dolist (shape selected-shapes)
            (edraw-transform shape matrix))))))))

(edraw-editor-defcmd edraw-delete-selected ((editor edraw-editor))
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

(edraw-editor-defcmd edraw-bring-selected-to-front ((editor edraw-editor))
  (when (edraw-selected-shapes editor) ;;@todo error?
    (edraw-make-undo-group editor 'selected-shapes-bring-to-front
      (dolist (shape (edraw-selected-shapes-back-to-front editor))
        (edraw-bring-to-front shape)))))

(edraw-editor-defcmd edraw-bring-selected-forward ((editor edraw-editor))
  (when (edraw-selected-shapes editor) ;;@todo error?
    (edraw-make-undo-group editor 'selected-shapes-bring-forward
      (dolist (shape (edraw-selected-shapes-front-to-back editor))
        (when-let ((next (edraw-next-sibling shape)))
          (unless (edraw-selected-p next) ;; No overtaking
            (edraw-bring-forward shape)))))))

(edraw-editor-defcmd edraw-send-selected-backward ((editor edraw-editor))
  (when (edraw-selected-shapes editor) ;;@todo error?
    (edraw-make-undo-group editor 'selected-shapes-send-backward
      (dolist (shape (edraw-selected-shapes-back-to-front editor))
        (when-let ((prev (edraw-previous-sibling shape)))
          (unless (edraw-selected-p prev) ;; No overtaking
            (edraw-send-backward shape)))))))

(edraw-editor-defcmd edraw-send-selected-to-back ((editor edraw-editor))
  (when (edraw-selected-shapes editor) ;;@todo error?
    (edraw-make-undo-group editor 'selected-shapes-send-to-back
      (dolist (shape (edraw-selected-shapes-front-to-back editor))
        (edraw-send-to-back shape)))))

(edraw-editor-defcmd edraw-select-next-shape ((editor edraw-editor))
  (if (edraw-selected-shapes editor)
      (when-let ((front (car (edraw-selected-shapes-front-to-back editor)))
                 (next (edraw-next-sibling front)))
        (edraw-select-shape editor next))
    (when-let ((back (edraw-back-shape editor)))
      (edraw-select-shape editor back))))

(edraw-editor-defcmd edraw-select-previous-shape ((editor edraw-editor))
  (if (edraw-selected-shapes editor)
      (when-let ((back (car (edraw-selected-shapes-back-to-front editor)))
                 (prev (edraw-previous-sibling back)))
        (edraw-select-shape editor prev))
    (when-let ((front (edraw-front-shape editor)))
      (edraw-select-shape editor front))))

(edraw-editor-defcmd edraw-group-selected-shapes ((editor edraw-editor))
  (with-slots (selected-shapes) editor
    (when (null selected-shapes)
      (error (edraw-msg "No shape selected")))
    (edraw-make-undo-group editor 'create-group
      (let ((group (edraw-create-shape editor (edraw-svg-body editor) 'g)))
        (edraw-shape-group-add-children group selected-shapes)))))

;;;;; Editor - Copy & Paste

(edraw-editor-defcmd edraw-paste ((editor edraw-editor))
  (when (eq (edraw-clipboard-type) 'shape-descriptor-list)
    (edraw-make-undo-group editor 'paste
      (edraw-shape-from-shape-descriptor-list
       editor (edraw-svg-body editor)
       (if-let ((selected-shapes (edraw-selected-shapes editor)))
           ;; after selected shapes
           (1+ (cl-loop for s in selected-shapes
                        maximize (edraw-node-position s)))
         ;; top most
         nil)
       (edraw-clipboard-data)))))

(edraw-editor-defcmd edraw-paste-and-select ((editor edraw-editor))
  (when-let ((shapes (edraw-paste editor)))
    (edraw-select-shapes editor shapes)
    shapes))

(edraw-editor-defcmd edraw-copy-selected-shapes ((editor edraw-editor))
  (when-let ((selected-shapes (edraw-selected-shapes-back-to-front editor)))
    (edraw-clipboard-set
     'shape-descriptor-list
     (mapcar #'edraw-shape-descriptor selected-shapes))))

(edraw-editor-defcmd edraw-cut-selected-shapes ((editor edraw-editor))
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

(edraw-editor-defcmd edraw-duplicate-selected-shapes ((editor edraw-editor))
  (edraw-select-shapes editor
                       (edraw-duplicate-shapes (edraw-selected-shapes editor))))

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
      (unless (plist-get prop-info :required)
        ;; Get property value
        (let* ((prop-name (plist-get prop-info :name))
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
  (let ((editor (or editor (edraw-current-editor))))
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

(cl-defmethod edraw-last-undo-data ((_shape edraw-property-proxy-shape))
  nil)

(cl-defmethod edraw-undo ((_shape edraw-property-proxy-shape))
  nil)

(cl-defmethod edraw-get-property-info-list ((shape edraw-property-proxy-shape))
  (seq-remove
   (lambda (prop-info) (plist-get prop-info :required))
   (edraw-svg-element-get-property-info-list-by-tag (oref shape tag))))

(cl-defmethod edraw-get-property ((shape edraw-property-proxy-shape) prop-name)
  (alist-get prop-name (cdr (oref shape alist-head))))

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

;;;;; Editor - Transform Method

(cl-defmethod edraw-set-transform-method ((editor edraw-editor) method)
  (edraw-set-setting editor 'transform-method method))

(cl-defmethod edraw-get-transform-method ((editor edraw-editor))
  (edraw-get-setting editor 'transform-method))

(edraw-editor-defcmd edraw-set-transform-method-auto
    ((editor edraw-editor))
  (edraw-set-transform-method editor 'auto))

(edraw-editor-defcmd edraw-set-transform-method-transform-property
    ((editor edraw-editor))
  (edraw-set-transform-method editor 'transform-property))

(edraw-editor-defcmd edraw-set-transform-method-anchor-points
    ((editor edraw-editor))
  (edraw-set-transform-method editor 'anchor-points))

(defun edraw-transform-method-menu (obj)
  `((edraw-msg "Transform Method")
    ;; @todo Use :radio instead of :toggle. However, when using :radio, Japanese characters are garbled on MS-Windows.
    (((edraw-msg "Auto") edraw-editor-set-transform-method-auto
      :button (:toggle . ,(eq (edraw-get-transform-method obj) 'auto)))
     ((edraw-msg "\"transform\" Property") edraw-editor-set-transform-method-transform-property
      :button (:toggle . ,(eq (edraw-get-transform-method obj) 'transform-property)))
     ((edraw-msg "Anchor Points") edraw-editor-set-transform-method-anchor-points
      :button (:toggle . ,(eq (edraw-get-transform-method obj) 'anchor-points))))))

;;;;; Editor - Main Menu

(edraw-editor-defcmd edraw-main-menu ((editor edraw-editor))
  "Show the main menu of EDITOR."
  (let ((selected-shapes (edraw-selected-shapes editor))
        (edraw-current-editor editor))
    (edraw-popup-menu
     (edraw-msg "Main Menu")
     (edraw-filter-menu
      editor
      'main-menu
      `(((edraw-msg "Document")
         (((edraw-msg "Set Background...") edraw-editor-set-background)
          ((edraw-msg "Resize...") edraw-editor-set-size)
          ((edraw-msg "Crop...") edraw-editor-crop)
          ((edraw-msg "View Box...") edraw-editor-set-view-box)
          ((edraw-msg "Transform")
           (((edraw-msg "Translate All...") edraw-editor-translate-all-shapes)
            ((edraw-msg "Scale All...") edraw-editor-scale-all-shapes)
            ((edraw-msg "Rotate All...") edraw-editor-rotate-all-shapes)
            ,(edraw-transform-method-menu editor)))
          ((edraw-msg "Clear...") edraw-editor-clear)
          ((edraw-msg "Export to Buffer") edraw-editor-export-to-buffer)
          ((edraw-msg "Export to File") edraw-editor-export-to-file)))
        ((edraw-msg "View")
         (((edraw-msg "Transparent BG") edraw-editor-toggle-transparent-bg-visible
           :button (:toggle . ,(edraw-get-transparent-bg-visible editor)))
          ((edraw-msg "Grid") edraw-editor-toggle-grid-visible
           :button (:toggle . ,(edraw-get-grid-visible editor)))
          ((edraw-msg "Set Grid Interval...") edraw-editor-set-grid-interval)
          ((edraw-msg "Set View Size...") edraw-editor-set-view-size-spec)
          ((edraw-msg "Reset View") edraw-editor-reset-view)
          ((edraw-msg "Zoom In") edraw-editor-zoom-in)
          ((edraw-msg "Zoom Out") edraw-editor-zoom-out)
          ((edraw-msg "Reset Scroll and Zoom") edraw-editor-reset-scroll-and-zoom)
          ((edraw-msg "Scroll and Zoom") edraw-editor-interactive-scroll-and-zoom)))
        ((edraw-msg "Selected Object")
         (((edraw-msg "Delete") edraw-editor-delete-selected
           :enable ,(not (null (or (edraw-selected-handle editor)
                                   (edraw-selected-anchor editor)
                                   selected-shapes))))
          ((edraw-msg "Copy") edraw-editor-copy-selected-shapes
           :enable ,(not (null selected-shapes)))
          ((edraw-msg "Cut") edraw-editor-cut-selected-shapes
           :enable ,(not (null selected-shapes)))
          ((edraw-msg "Group") edraw-editor-group-selected-shapes)
          ((edraw-msg "Transform")
           (((edraw-msg "Translate...") edraw-editor-translate-selected)
            ((edraw-msg "Scale...") edraw-editor-scale-selected)
            ((edraw-msg "Rotate...") edraw-editor-rotate-selected)
            ,(edraw-transform-method-menu editor)))
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
  (edraw-snap-xy editor (edraw-mouse-event-to-xy-raw editor event)))

;; For object search purposes, you should use non-snap version.
;; This version may return non-integer coordinates due to zoom
;; magnification and automatic scaling in high DPI environments.
(cl-defmethod edraw-mouse-event-to-xy-raw ((editor edraw-editor) event)
  (edraw-scroll-reverse-transform-xy
   editor
   (edraw-mouse-event-to-xy-on-scroll-view editor event)))

(cl-defmethod edraw-mouse-event-to-xy-on-scroll-view ;;@todo rename
  ((editor edraw-editor) event)
  (with-slots (image-scale) editor
    (let* ((xy-on-overlay (posn-object-x-y (event-start event)))
           (xy-on-scroll-view
            ;; Apply image-scale only.
            (edraw-xy (/ (car xy-on-overlay) image-scale)
                      (/ (cdr xy-on-overlay) image-scale))))
      xy-on-scroll-view)))

(cl-defmethod edraw-snap-xy ((editor edraw-editor) xy)
  (if (edraw-get-setting editor 'grid-visible)
      ;; Snap to grid lines
      (let* ((interval (edraw-get-setting editor 'grid-interval)))
        (edraw-xy
         (edraw-grid-round (edraw-x xy) interval)
         (edraw-grid-round (edraw-y xy) interval)))
    ;; Snap to pixels
    ;; Must be able to point to integer pixel coordinates on
    ;; high DPI environment(image-scale > 1.0).
    ;;@todo Add settings to snap to coordinates below 1.0
    (edraw-xy-round xy)))

;;;;; Editor - Input Event

(defconst edraw-event-remap-table
  `((,mouse-wheel-up-event . wheel-down)
    (,mouse-wheel-down-event . wheel-up)))

(defun edraw-editor-event-handler-name (event)
  (let* ((event-name (car event))
         ;; Remap mouse-5 to wheel-down
         (event-name (alist-get event-name edraw-event-remap-table
                                event-name))
         (method-name (intern (concat "edraw-on-" (symbol-name event-name)))))
    method-name))

(defun edraw-editor-dispatch-event (event)
  "Call the editor's method corresponding to the EVENT.

For example, if the event name is down-mouse-1, call
edraw-on-down-mouse-1. Determine the editor object from the
position where the EVENT occurred."
  (interactive "e")

  (when-let ((editor (edraw-editor-at-input event)))
    (let ((method-name (edraw-editor-event-handler-name event)))
      (when (fboundp method-name)
        (funcall method-name editor event)))))

(cl-defmethod edraw-call-tool-event-handler ((editor edraw-editor) event)
  (with-slots (tool) editor
    (when tool
      (let ((method-name (edraw-editor-event-handler-name event)))
        (when (fboundp method-name)
          (funcall method-name tool event))))))

(cl-defmethod edraw-on-down-mouse-1 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-S-down-mouse-1 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-C-down-mouse-1 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-M-down-mouse-1 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-M-S-down-mouse-1 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-mouse-1 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-double-mouse-1 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-S-mouse-1 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-C-mouse-1 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-mouse-3 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

;;;;; Editor - Toolbar

(defvar edraw-editor-tool-list
  '(select rect ellipse path freehand text image custom-shape))

(defcustom edraw-editor-default-tool 'select
  "The first tool selected when opening the editor."
  :type `(choice ,(mapcar (lambda (x) `(const ,x)) edraw-editor-tool-list))
  :group 'edraw-editor)

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
    (edraw-svg-set-attr-string icon 'transform (format "translate(%s %s)" x0 y0))
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
   (edraw-editor-make-tool-title tool-id)
   (edraw-editor-tool-select-function-name tool-id)))

(defun edraw-editor-make-tool-title (tool-id)
  (let ((fun (intern (format "edraw-editor-tool-%s-title" tool-id))))
    (if (and fun (fboundp fun))
        (funcall fun)
      (edraw-msg (capitalize (symbol-name tool-id))))))

(defun edraw-editor-make-tool (tool-id)
  (funcall (edraw-editor-make-tool-class-name tool-id)))

(defun edraw-editor-tool-select-function-name (tool-id)
  (intern (format "edraw-editor-select-tool-%s" tool-id)))

(defun edraw-editor-define-tool-select-function (tool-id)
  (defalias (edraw-editor-tool-select-function-name tool-id)
    (lambda () (interactive)
      (let ((editor (edraw-current-editor)))
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

(defun edraw-icon-tool-image (g)
  (svg-rectangle
   g 4.5 4.5 22 15 :stroke-width 1 :stroke "#ccc" :gradient "icon-fg-gradient")
  (svg-ellipse
   g 11 9 2.5 2.5 :fill "#888")
  (svg-node
   g 'path :d "M10 13 14 15 20 10 26 16 26 18 5 18 10 13Z" :fill "#666"))

(defun edraw-icon-tool-custom-shape (g)
  (svg-node
   g 'path :d "M14,4C16,2 18,0 20,2C22,4 18,6 20,8C22,10 26,8 26,12C26,14 22,12 20,14C18,16 24,20 20,22C16,24 16,16 12,16C8,16 4,20 4,16C4,14 6,12 8,10C10,8 4,6 6,4C8,2 12,6 14,4Z"
   :stroke-width 1 :stroke "#ccc" :gradient "icon-fg-gradient"))

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
                    (pcase prop-name
                      ('fill (edraw-msg "Fill"))
                      ('stroke (edraw-msg "Stroke"))
                      (_ (edraw-msg (capitalize (symbol-name prop-name))))))
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
                      `((:color-name-scheme . web)
                        (:no-color . "none")
                        (:scale-direct . ,(oref editor image-scale))))))
      (edraw-set-selected-tool-default-shape-property
       editor prop-name new-value)
      (edraw-update-toolbar editor))))

(defun edraw-editor-edit-selected-tool-default-shape-property (prop-name)
  (let ((editor (edraw-current-editor)))
    (edraw-edit-selected-tool-default-shape-property editor prop-name)))

(defun edraw-editor-edit-tool-default-fill ()
  (interactive)
  (edraw-editor-edit-selected-tool-default-shape-property 'fill))

(defun edraw-editor-edit-tool-default-stroke ()
  (interactive)
  (edraw-editor-edit-selected-tool-default-shape-property 'stroke))

(defun edraw-editor-edit-tool-properties ()
  (interactive)
  (let ((editor (edraw-current-editor)))
    (when-let ((selected-tool (edraw-selected-tool editor))
               (shape-type (edraw-shape-type-to-create selected-tool)))
      (edraw-edit-default-shape-properties editor shape-type))))



;;;;; Editor - Basic Mouse Reactions

(cl-defmethod edraw-mouse-down-handle-point ((editor edraw-editor)
                                             down-event)
  "Drag a handle point or select it."
  (let* ((down-xy (edraw-mouse-event-to-xy-raw editor down-event)) ;;Do not any rounding coordinates
         (selected-anchor (edraw-selected-anchor editor))
         (selected-handle (edraw-selected-handle editor))
         (handle (and selected-anchor
                      (edraw-shape-point-find
                       ;; handle points of selected anchor point
                       (edraw-selectable-handles editor)
                       down-xy))))
    (when handle
      (let ((anchor-xy (edraw-get-xy (edraw-parent-anchor handle)))
            (shift-p (memq 'shift (event-modifiers down-event)))
            (move-xy nil))
        (edraw-editor-with-temp-modifications editor
          (edraw-track-dragging
           down-event
           (lambda (move-event)
             (edraw-undo-all editor) ;; Cancel previous move
             (setq move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
             (when shift-p
               (setq move-xy (edraw-xy-snap-to-45deg move-xy anchor-xy)))
             ;; If selected handle, move it alone
             (if (and selected-handle
                      (edraw-same-point-p handle selected-handle))
                 (edraw-move-on-transformed handle move-xy) ;;notify modification
               (edraw-move-with-opposite-handle-on-transformed handle move-xy)))))
        (if move-xy
            ;; Fix position
            ;; If selected handle, move it alone
            (if (and selected-handle
                     (edraw-same-point-p handle selected-handle))
                (edraw-move-on-transformed handle move-xy) ;;notify modification
              (edraw-move-with-opposite-handle-on-transformed handle move-xy))
          ;; Click handle point
          (edraw-select-handle editor handle)
          (edraw-display-selected-object-coordinates editor)))
      t)))

(cl-defmethod edraw-mouse-down-anchor-point ((editor edraw-editor)
                                             down-event)
  "Drag a anchor point or select it."
  (let* ((down-xy (edraw-mouse-event-to-xy-raw editor down-event)) ;;Do not any rounding coordinates
         (move-xy nil)
         (anchor (seq-some (lambda (shp)
                             (edraw-pick-anchor-point shp down-xy))
                           (edraw-selected-shapes editor))))
    (when anchor
      (let ((original-xy (edraw-get-xy anchor))
            (shift-p (memq 'shift (event-modifiers down-event))))
        (edraw-editor-with-temp-modifications editor
          (edraw-track-dragging
           down-event
           (lambda (move-event)
             (edraw-undo-all editor) ;; Cancel previous move
             (setq move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
             (when shift-p
               (setq move-xy (edraw-xy-snap-to-45deg move-xy original-xy)))
             (edraw-move-on-transformed anchor move-xy)))) ;;notify modification
        (if move-xy
            ;; Fix position
            (edraw-move-on-transformed anchor move-xy) ;;notify modification
          ;; Click the anchor point if the mouse does not move
          (edraw-select-anchor editor anchor)
          (edraw-display-selected-object-coordinates editor)))
      t)))

(cl-defmethod edraw-mouse-down-shape ((editor edraw-editor) down-event)
  (let* ((down-xy (edraw-mouse-event-to-xy-raw editor down-event)) ;;Do not any rounding coordinates
         (down-xy-snapped (edraw-snap-xy editor down-xy))
         (selected-shapes (edraw-selected-shapes editor))
         (down-shape (car (edraw-find-shapes-by-xy editor down-xy))) ;;front
         (down-selected-p (memq down-shape selected-shapes))
         (modifiers (event-modifiers down-event))
         (shift-p (memq 'shift modifiers))
         (event-type (edraw-make-event-modifiers-symbol
                      (remq 'shift modifiers)))
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
               (move-xy nil))
          ;; Preview while dragging
          (edraw-editor-with-temp-modifications editor
            (edraw-track-dragging
             down-event
             (lambda (move-event)
               ;; Cancel previous move
               (edraw-undo-all editor)
               ;; Move
               (setq move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
               (let ((delta-xy (edraw-xy-sub move-xy start-xy)))
                 (when shift-p
                   (setq delta-xy (edraw-xy-snap-to-45deg delta-xy)))
                 (dolist (shape moving-shapes)
                   (edraw-translate shape delta-xy)))))) ;;notify modification
          ;; Commit
          (if move-xy
              ;; On Drag
              (edraw-make-undo-group editor 'shapes-move-by-drag
                (let ((delta-xy (edraw-xy-sub move-xy start-xy)))
                  (when shift-p
                    (setq delta-xy (edraw-xy-snap-to-45deg delta-xy)))
                  (dolist (shape moving-shapes)
                    (edraw-translate shape delta-xy)))) ;;notify modification
            ;; On Click
            (pcase event-type
              ('down
               (if down-selected-p
                   (edraw-select down-shape)))))))
      t)))

(cl-defmethod edraw-mouse-drag-shape-duplicate ((editor edraw-editor)
                                                down-event)
  (let* ((down-xy (edraw-mouse-event-to-xy-raw editor down-event)) ;;Do not any rounding coordinates
         (down-xy-snapped (edraw-snap-xy editor down-xy))
         (down-shape (car (edraw-find-shapes-by-xy editor down-xy)))) ;;front
    (when down-shape
      (let* ((selected-shapes (edraw-selected-shapes editor))
             (down-selected-p (memq down-shape selected-shapes))
             (shift-p (memq 'shift (event-modifiers down-event)))
             (target-shapes (if down-selected-p
                                selected-shapes
                              (list down-shape)))
             (start-xy down-xy-snapped)
             (move-xy nil)
             (delta-xy nil))
        ;; Preview while dragging
        (edraw-editor-with-temp-modifications editor
          (let ((preview-shapes (edraw-duplicate-shapes target-shapes)))
            (edraw-editor-with-temp-modifications editor
              (edraw-track-dragging
               down-event
               (lambda (move-event)
                 ;; Cancel previous move
                 (edraw-undo-all editor)
                 ;; Move
                 (setq move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
                 (setq delta-xy (edraw-xy-sub move-xy start-xy))
                 (when shift-p
                   (setq delta-xy (edraw-xy-snap-to-45deg delta-xy)))
                 (dolist (shape preview-shapes)
                   (edraw-translate shape delta-xy))))))) ;;notify modification
        ;; Commit
        (if move-xy
            ;; On Drag
            (edraw-make-undo-group editor 'shapes-duplicate-by-drag
              (let ((new-shapes (edraw-duplicate-shapes target-shapes)))
                (dolist (shape new-shapes)
                  (edraw-translate shape delta-xy)) ;;notify modification
                (edraw-select-shapes editor new-shapes)))
          ;; On Click
          ))
      ;; Handled
      t)))

(cl-defmethod edraw-context-menu-at-point ((editor edraw-editor) xy)
  (if-let ((shapes (edraw-find-shapes-by-xy editor xy)))
      (let ((selected-shapes (edraw-selected-shapes editor)))
        (if (and (cdr selected-shapes)
                 (edraw-selected-p (car shapes)))
            ;; multiple selected shapes
            (edraw-popup-context-menu-for-selected-shapes editor)
          ;; single selected shape or deselected shapes
          (edraw-popup-context-menu (if (cdr shapes)
                                        (edraw-popup-shape-selection-menu shapes)
                                      (car shapes)))))
    ;; document
    (edraw-popup-context-menu-for-document editor)))

(cl-defmethod edraw-read-rectangle ((editor edraw-editor) down-event snap-p)
  (let ((down-xy (if snap-p
                     (edraw-mouse-event-to-xy-snapped editor down-event)
                   (edraw-mouse-event-to-xy-raw editor down-event)))
        (shift-p (memq 'shift (event-modifiers down-event))))
    (edraw-ui-foreground-svg editor)
    (let ((ui-parent (edraw-ui-foreground-svg editor))
          (ui-preview (dom-node 'rect `((class . "edraw-ui-read-rectangle")
                                        (x . ,(edraw-scroll-transform-x
                                               editor (car down-xy)))
                                        (y . ,(edraw-scroll-transform-x
                                               editor (cdr down-xy)))
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
                     (edraw-mouse-event-to-xy-raw editor move-event)))
             (when shift-p
               (setq move-xy (edraw-xy-snap-to-square move-xy down-xy)))
             (edraw-svg-rect-set-range
              ui-preview
              (edraw-scroll-transform-xy editor down-xy)
              (edraw-scroll-transform-xy editor move-xy))
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
      (edraw-on-deselected tool))
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
(cl-defmethod edraw-on-C-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-S-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-M-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-M-S-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-C-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-S-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-double-mouse-1 ((_tool edraw-editor-tool) _click-event))

(cl-defmethod edraw-on-mouse-3 ((tool edraw-editor-tool) click-event)
  (with-slots (editor) tool
    (let ((click-xy (edraw-mouse-event-to-xy-raw editor click-event))) ;;Do not any rounding coordinates

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
             (let ((point-type (edraw-get-point-type target-spoint)))
               (pcase point-type
                 ('anchor (edraw-msg "Anchor Point"))
                 ('handle (edraw-msg "Handle Point"))
                 (_ (format (edraw-msg "%s Point")
                            (capitalize
                             (symbol-name
                              point-type))))))
             actions target-spoint)
            t)))
       ;; Shape
       (t
        (edraw-context-menu-at-point editor click-xy))))))

(cl-defgeneric edraw-on-selected (target selector)
  "Called when TARGET is selected by SELECTOR.")
(cl-defmethod edraw-on-selected ((tool edraw-editor-tool) (editor edraw-editor))
  (oset tool editor editor))

(cl-defgeneric edraw-on-deselected (target)
  "Called when TARGET is deselected.")
(cl-defmethod edraw-on-deselected ((tool edraw-editor-tool))
  (oset tool editor nil))



;;;;; Tool - Select Tool

(defun edraw-editor-tool-select-title () (edraw-msg "Select Tool"))

(defclass edraw-editor-tool-select (edraw-editor-tool)
  ())

(cl-defmethod edraw-on-deselected ((_tool edraw-editor-tool-select))
  ;;(edraw-deselect-shape (oref tool editor))
  (cl-call-next-method))

(cl-defmethod edraw-on-M-S-down-mouse-1 ((tool edraw-editor-tool-select)
                                         down-event)
  (edraw-on-M-down-mouse-1 tool down-event))

(cl-defmethod edraw-on-M-down-mouse-1 ((tool edraw-editor-tool-select)
                                       down-event)
  (with-slots (editor) tool
    (edraw-mouse-drag-shape-duplicate editor down-event)))

(cl-defmethod edraw-on-C-down-mouse-1 ((tool edraw-editor-tool-select)
                                       down-event)
  (with-slots (editor) tool
    (cond
     ;; Drag or click a shape
     ((edraw-mouse-down-shape editor down-event)))))

(cl-defmethod edraw-on-S-down-mouse-1 ((tool edraw-editor-tool-select)
                                       down-event)
  (edraw-on-down-mouse-1 tool down-event))

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
          (edraw-select-shapes editor
                               (edraw-find-shapes-by-rect editor rect))
          t)))

     ;; Deselect
     (t (edraw-deselect-all-shapes editor)))))

(cl-defmethod edraw-on-double-mouse-1 ((tool edraw-editor-tool-select)
                                       click-event)
  (with-slots (editor) tool
    (let* ((click-xy (edraw-mouse-event-to-xy-raw editor click-event))
           (shape (car (edraw-find-shapes-by-xy editor click-xy))))
      (when shape
        (edraw-edit-properties shape)))))



;;;;; Tool - Rect Tool

(defun edraw-editor-tool-rect-title () (edraw-msg "Rect Tool"))

(defclass edraw-editor-tool-rect (edraw-editor-tool)
  ()
  )

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-rect))
  'rect)

(cl-defmethod edraw-on-S-down-mouse-1 ((tool edraw-editor-tool-rect)
                                       down-event)
  (edraw-on-down-mouse-1 tool down-event))

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-rect)
                                     down-event)
  (with-slots (editor) tool
    (let ((down-xy (edraw-mouse-event-to-xy-snapped editor down-event))
          (move-xy nil)
          (shift-p (memq 'shift (event-modifiers down-event))))
      ;; Preview
      (edraw-editor-with-silent-modifications
        (let* ((shape (edraw-create-shape ;;notify modification
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
                   (when shift-p
                     (setq move-xy (edraw-xy-snap-to-square move-xy down-xy)))
                   (edraw-set-rect shape down-xy move-xy)))) ;;notify modification
            (edraw-remove shape))))

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

(defun edraw-editor-tool-ellipse-title () (edraw-msg "Ellipse Tool"))

(defclass edraw-editor-tool-ellipse (edraw-editor-tool)
  ()
  )

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-ellipse))
  'ellipse)

(cl-defmethod edraw-on-S-down-mouse-1 ((tool edraw-editor-tool-ellipse)
                                       down-event)
  (edraw-on-down-mouse-1 tool down-event))

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-ellipse)
                                     down-event)
  (with-slots (editor) tool
    (edraw-deselect-all-shapes editor)
    (let ((down-xy (edraw-mouse-event-to-xy-snapped editor down-event))
          (move-xy nil)
          (shift-p (memq 'shift (event-modifiers down-event))))
      ;; Preview
      (edraw-editor-with-silent-modifications
        (let* ((shape (edraw-create-shape ;;notify modification
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
                   (when shift-p
                     (setq move-xy (edraw-xy-snap-to-square move-xy down-xy)))
                   (edraw-set-rect shape down-xy move-xy)))) ;;notify modification
            (edraw-remove shape))))

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

(defun edraw-editor-tool-text-title () (edraw-msg "Text Tool"))

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
        (let* ((click-xy (edraw-mouse-event-to-xy-raw editor click-event))
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
      (when-let ((rect (ignore-errors (edraw-shape-aabb shape)))
                 (center (edraw-rect-center rect)))
        (when (< (edraw-xy-distance center xy) font-size)
          ;;(message "Snapped to the center of background shape")
          (cons
           (car center)
           (+ (cdr center) (* 0.4 font-size)))))))) ;;@todo ascent font-size ratio


;;;;; Tool - Image Tool

(defun edraw-editor-tool-image-title () (edraw-msg "Image Tool"))

(defclass edraw-editor-tool-image (edraw-editor-tool)
  ()
  )

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-image))
  'image)

(cl-defmethod edraw-on-S-down-mouse-1 ((tool edraw-editor-tool-image)
                                       down-event)
  (edraw-on-down-mouse-1 tool down-event))

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-image)
                                     down-event)
  (with-slots (editor) tool
    (when-let ((rect (edraw-read-rectangle editor down-event t))
               (file (read-file-name (edraw-msg "Image File: "))))

      (when (edraw-rect-empty-p rect)
        (let* ((image-spec (create-image file nil nil :scale 1))
               (image-size (progn
                             (image-flush image-spec)
                             (image-size image-spec t))))
          (edraw-xy-assign (edraw-rect-xy1 rect)
                           (edraw-xy-add (edraw-rect-xy1 rect) image-size))))

      ;; Create
      (let ((shape (edraw-create-shape ;;notify modification
                    editor
                    (edraw-svg-body editor)
                    'image
                    'x (edraw-rect-left rect)
                    'y (edraw-rect-top rect)
                    'width (edraw-rect-width rect)
                    'height (edraw-rect-height rect)
                    'href (file-relative-name file))))
        (edraw-select-shape editor shape)))))



;;;;; Tool - Path Tool

(defun edraw-editor-tool-path-title () (edraw-msg "Path Tool"))

(defclass edraw-editor-tool-path (edraw-editor-tool)
  ((editing-path
    :initform nil
    :type (or null edraw-shape-path))))

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-path))
  'path)

(cl-defmethod edraw-on-deselected ((tool edraw-editor-tool-path))
  (edraw-clear tool)
  (cl-call-next-method))

(cl-defmethod edraw-mouse-down-continue-path ((tool edraw-editor-tool-path)
                                              down-event)
  (with-slots (editor editing-path) tool
    (when (null editing-path)
      (edraw-make-undo-group editor 'path-tool-continue-path
        ;;@todo support all selected shapes or all shapes
        (when-let ((selected-path (edraw-cast (edraw-last-selected-shape editor)
                                              'edraw-shape-path)))
          (let* ((down-xy (edraw-mouse-event-to-xy-raw editor down-event)) ;;Do not any rounding coordinates
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
                     (edraw-get-last-anchor-point selected-path)))))
            (when anchor
              (setq editing-path selected-path)

              (message (edraw-msg "Connected"))
              (edraw-select-anchor editor anchor)

              ;; Drag
              (edraw-drag-handle-on-click-anchor anchor nil down-event editor)
              t
              )))))))

(cl-defmethod edraw-mouse-down-close-path ((tool edraw-editor-tool) down-event)
  "Click the first anchor point of the editing path and drag it."
  (with-slots (editor editing-path) tool
    (when (and editing-path
               (edraw-closable-path-shape-p editing-path))
      (let ((down-xy (edraw-mouse-event-to-xy-raw editor down-event)) ;;Do not any rounding coordinates
            (anchor (edraw-get-first-anchor-point editing-path)))
        (when (and anchor
                   (edraw-hit-input-p anchor down-xy))
          (edraw-make-undo-group editor 'path-tool-close-path
            (when (edraw-close-path-shape editing-path)
              (message (edraw-msg "Closed"))

              (edraw-select-anchor editor anchor)

              ;; Drag the `backward' handle of ANCHOR
              (edraw-drag-handle-on-click-anchor anchor 'backward down-event editor)
              (edraw-clear tool)
              t
              )))))))

(cl-defmethod edraw-mouse-down-connect-to-another-path ((tool edraw-editor-tool) down-event)
  (with-slots (editor editing-path) tool
    (when editing-path
      (let* ((down-xy (edraw-mouse-event-to-xy-raw editor down-event)) ;;Do not any rounding coordinates
             (anchor (car (delq editing-path (edraw-find-end-points-of-path-shapes editor down-xy)))))
        (when anchor
          (edraw-make-undo-group editor 'path-tool-connect-to-another-path
            (when (edraw-connect-path-to-anchor editing-path anchor) ;;notify modification
              (edraw-clear tool)
              (edraw-select-shape editor (edraw-parent-shape anchor))
              (edraw-select-anchor editor anchor)
              (message (edraw-msg "Connected"))

              (edraw-drag-handle-on-click-anchor anchor 'backward down-event editor)
              t)))))))

(defun edraw-drag-handle-on-click-anchor (anchor backward-p down-event editor)
  (let ((shift-p (memq 'shift (event-modifiers down-event)))
        (anchor-xy (edraw-get-xy-transformed anchor))
        (anchor-index (edraw-anchor-index-in-path anchor))
        (shape (edraw-parent-shape anchor))
        dragging-point
        move-xy)
    (edraw-editor-with-temp-modifications editor
      (edraw-track-dragging
       down-event
       (lambda (move-event)
         (setq move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
         (when shift-p
           (setq move-xy (edraw-xy-snap-to-45deg move-xy anchor-xy)))
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
    (when move-xy
      ;; ANCHOR may have been invalidated by UNDO handle creation,
      ;; so reacquire it.
      (let* ((anchor (edraw-get-nth-anchor-point shape anchor-index))
             (dragging-point
              ;;notify modification
              (if backward-p
                  (edraw-create-backward-handle anchor)
                (edraw-create-forward-handle anchor))))
        (edraw-select-anchor editor anchor)
        (when dragging-point
          (edraw-move-with-opposite-handle-on-transformed ;;notify modification
           dragging-point
           (if backward-p
               (edraw-xy-sub (edraw-xy-nmul 2 anchor-xy) move-xy)
             move-xy))
          t)))))

(cl-defmethod edraw-on-S-down-mouse-1 ((tool edraw-editor-tool-path)
                                       down-event)
  (edraw-on-down-mouse-1 tool down-event))

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

          (let* ((shift-p (memq 'shift (event-modifiers down-event)))
                 (last-anchor (edraw-get-last-anchor-point editing-path))
                 (last-xy (when last-anchor (edraw-get-xy last-anchor)))
                 (new-xy (if (and shift-p last-xy)
                             (edraw-xy-snap-to-45deg down-xy last-xy)
                           down-xy))
                 ;; Add a new point
                 (anchor-point (edraw-add-anchor-point editing-path new-xy)) ;;notify modification
                 dragging-point
                 move-xy)

            ;; Select last anchor point
            (edraw-select-anchor editor anchor-point)
            (edraw-display-selected-object-coordinates editor)

            ;; Drag handle points of the new point
            (edraw-editor-with-temp-modifications editor
              (edraw-track-dragging
               down-event
               (lambda (move-event)
                 (setq move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
                 (when shift-p
                   (setq move-xy (edraw-xy-snap-to-45deg move-xy new-xy)))

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
                   ))))

            (when move-xy
              ;; ANCHOR-POINT may have been invalidated by UNDO handle creation,
              ;; so reacquire it.
              (let* ((anchor-point (edraw-get-last-anchor-point editing-path))
                     (dragging-point (edraw-create-forward-handle anchor-point))) ;;notify modification
                (edraw-select-anchor editor anchor-point)
                (when dragging-point
                  (edraw-create-backward-handle anchor-point)
                  (edraw-move-with-opposite-handle-symmetry-on-transformed
                   dragging-point
                   move-xy
                   t);;notify modification
                  ))))))))))

(cl-defmethod edraw-clear ((tool edraw-editor-tool-path))
  (with-slots (editor editing-path) tool
    (when editing-path
      (setq editing-path nil))))

(cl-defmethod edraw-find-end-points-of-path-shapes ((editor edraw-editor) xy)
  (let (points)
    (dolist (node (dom-children (edraw-svg-body editor)))
      (when-let ((shape (edraw-shape-from-element node editor 'noerror)))
        (when (edraw-shape-path-p shape);;or derived?
          ;;@todo include endpoint of subpaths?
          (dolist (anchor (list (edraw-get-first-anchor-point shape)
                                (edraw-get-last-anchor-point shape)))
            (when (and anchor
                       (edraw-hit-input-p anchor xy)
                       (not (edraw-in-closed-subpath-p anchor)))
              (push anchor points))))))
    points)) ;;front to back



;;;;; Tool - Freehand Tool

(defun edraw-editor-tool-freehand-title () (edraw-msg "Freehand Tool"))

(defclass edraw-editor-tool-freehand (edraw-editor-tool)
  ())

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-freehand))
  'path)

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-freehand)
                                     down-event)
  (with-slots (editor) tool
    (let* ((down-xy (edraw-mouse-event-to-xy-snapped editor down-event))
           (last-xy down-xy)
           points)
      ;; Deselect
      (edraw-deselect-all-shapes editor)
      ;; Preview
      (edraw-editor-with-silent-modifications
        ;; Add a new path shape
        (let ((preview-path (edraw-create-shape
                             editor (edraw-svg-body editor) 'path)))
          (unwind-protect
              (progn
                ;; Add the first point of the path
                (push down-xy points)
                (edraw-add-anchor-point preview-path down-xy)

                ;; Add new points on dragging
                (edraw-track-dragging
                 down-event
                 (lambda (move-event)
                   (let ((move-xy
                          (edraw-mouse-event-to-xy-snapped editor move-event)))
                     ;;@todo realtime simplification & smoothing the path

                     (unless (edraw-xy-equal-p move-xy last-xy)
                       (push move-xy points)
                       (edraw-add-anchor-point preview-path move-xy)
                       (setq last-xy move-xy))))))
            (edraw-remove preview-path))))

      ;; Create
      (when (>= (length points) 2)
        (setq points (nreverse points))
        (let ((path-data
               (if (edraw-get-setting editor 'grid-visible) ;;@todo Checking while snapping should be a dedicated predicate.
                   ;; If snapped to grid, don't smooth.
                   (concat
                    "M"
                    (mapconcat (lambda (xy)
                                 (concat
                                  (edraw-to-string (edraw-x xy))
                                  " "
                                  (edraw-to-string (edraw-y xy))))
                               points
                               "L"))
                 ;; Fit bezier curve
                 (edraw-xy-points-to-smooth-path-data points))))
          (when path-data
            ;; Add a new path shape
            (edraw-create-shape ;;modify
             editor (edraw-svg-body editor) 'path
             'd path-data)))))))


;;;;; Tool - Custom Shape Tool

(defun edraw-editor-tool-custom-shape-title () (edraw-msg "Custom Shape Tool"))

(defclass edraw-editor-tool-custom-shape (edraw-editor-tool)
  ((on-picker-notify :initform nil)
   (picker-buffer :initform nil)
   (selected-shape-descriptor-list :initform nil)
   (selected-picker-entry-properties :initform nil)))

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-custom-shape))
  ;;@todo Default values should be per tool, not per type
  'path) ;;Although not only path, default properties can be set only for path

(cl-defmethod edraw-on-selected ((tool edraw-editor-tool-custom-shape)
                                 (_editor edraw-editor))
  (prog1 (cl-call-next-method)
    (edraw-connect-to-shape-picker tool)))

(cl-defmethod edraw-on-deselected ((tool edraw-editor-tool-custom-shape))
  (edraw-disconnect-from-shape-picker tool)
  (cl-call-next-method))

(cl-defmethod edraw-connect-to-shape-picker ((tool edraw-editor-tool-custom-shape))
  (let ((editor-buffer (current-buffer))) ;;@todo get from overlay?
    (with-slots (on-picker-notify picker-buffer) tool
      (when (or (null picker-buffer)
                (not (buffer-live-p picker-buffer)))
        (unless on-picker-notify
          (setq on-picker-notify
                (lambda (type &rest args)
                  ;; NOTE: Called from the picker buffer.
                  (if (buffer-live-p editor-buffer)
                      (with-current-buffer editor-buffer
                        ;; Call with editor's buffer
                        (edraw-on-shape-picker-notify tool type args))
                    ;; editor is killed!
                    (edraw-disconnect-from-shape-picker tool)))))
        (setq picker-buffer (edraw-shape-picker-open)) ;; Open common buffer
        (edraw-shape-picker-connect picker-buffer on-picker-notify)
        ;; If you want to detect that a buffer has been killed arbitrarily, do:
        ;; (with-current-buffer picker-buffer (add-hook 'kill-buffer-hook <callback> nil t))
        ;; And add following before disconnect:
        ;; (with-current-buffer picker-buffer (remove-hook 'kill-buffer-hook <callback> t))

        ;; Update selection
        (edraw-update-custom-shape-selection tool)))))

(cl-defmethod edraw-update-custom-shape-selection ((tool edraw-editor-tool-custom-shape))
  (with-slots (picker-buffer) tool
    (when-let ((args (edraw-shape-picker-selected-args picker-buffer)))
      (apply #'edraw-select-shape-picker-shape tool args))
    ;;else (edraw-select-shape-picker-shape tool nil nil) ;;Do not deselect. Keep current selection
    ))

(cl-defmethod edraw-disconnect-from-shape-picker ((tool edraw-editor-tool-custom-shape) &optional no-disconnect-needed)
  (with-slots (on-picker-notify picker-buffer) tool
    (when picker-buffer
      (when (and (not no-disconnect-needed)
                 (buffer-live-p picker-buffer)) ;;live buffer
        (edraw-shape-picker-disconnect picker-buffer on-picker-notify)) ;;Close automatically
      (setq on-picker-notify nil)
      (setq picker-buffer nil))))

(cl-defmethod edraw-on-shape-picker-notify
  ((tool edraw-editor-tool-custom-shape) type args)
  "Callback from edraw-shape-picker buffer."
  ;; NOTE: Called from editor's buffer.
  (pcase type
    ('exit
     (edraw-disconnect-from-shape-picker tool t))
    ('deselect
     (edraw-select-shape-picker-shape tool nil nil))
    ('select
     (apply #'edraw-select-shape-picker-shape tool args))))

(cl-defmethod edraw-select-shape-picker-shape
  ((tool edraw-editor-tool-custom-shape) shape-def props)
  (with-slots (selected-shape-descriptor-list
               selected-picker-entry-properties
               editor)
      tool
    (if shape-def
        (progn
          (setq selected-shape-descriptor-list
                (edraw-shape-descriptor-list-from-shape-picker-shape
                 shape-def editor))
          (setq selected-picker-entry-properties props)
          (message (edraw-msg "Select %s")
                   (or (plist-get props :name)
                       (edraw-msg "<no name>"))))
      (setq selected-shape-descriptor-list nil)
      (setq selected-picker-entry-properties nil))))

(cl-defmethod edraw-on-S-down-mouse-1 ((tool edraw-editor-tool-custom-shape)
                                       down-event)
  (edraw-on-down-mouse-1 tool down-event))

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-custom-shape)
                                     down-event)
  (edraw-update-custom-shape-selection tool)

  (with-slots (selected-shape-descriptor-list editor) tool
    (when selected-shape-descriptor-list ;;selected
      (let ((down-xy (edraw-mouse-event-to-xy-snapped editor down-event))
            (move-xy nil)
            (shift-p (memq 'shift (event-modifiers down-event))))
        ;; Preview
        (edraw-editor-with-temp-undo-list editor
          (let* ((edraw-editor-keep-modified-flag t)
                 (shapes (edraw-editor-with-no-undo-data
                           (edraw-create-selected-custom-shapes tool)))
                 (ref-box (edraw-selected-custom-shapes-ref-box tool shapes)))
            (unwind-protect
                (progn
                  (edraw-make-undo-group editor 'put-custom-shape--preview
                    (edraw-translate shapes down-xy))

                  (edraw-track-dragging
                   down-event
                   (lambda (move-event)
                     (setq move-xy
                           (edraw-mouse-event-to-xy-snapped editor move-event))
                     (when shift-p
                       (setq move-xy (edraw-xy-snap-to-square move-xy down-xy)))
                     ;; Cancel previous transform
                     (unless (edraw-empty-undo-p editor)
                       (edraw-undo editor))
                     ;; Apply new transform
                     (let ((matrix
                            (if (edraw-xy-equal-p move-xy down-xy)
                                (edraw-matrix-translate-xy down-xy)
                              (edraw-matrix-fit-rect-to-rect
                               ref-box (edraw-rect-pp down-xy move-xy)))))
                       (edraw-make-undo-group editor 'put-custom-shape--preview
                         (edraw-transform shapes matrix))))))
              (dolist (shape shapes)
                (edraw-remove shape)))))

        ;; Create
        (cond
         ;; Click or same points
         ((or (null move-xy)
              (edraw-xy-equal-p down-xy move-xy))
          (edraw-make-undo-group editor 'put-custom-shape
            (edraw-translate ;;@todo should I use edraw-transform? or not?
             (edraw-create-selected-custom-shapes tool)
             down-xy)))
         ;; Drag
         ((and move-xy
               (not (edraw-xy-equal-p down-xy move-xy)))
          (edraw-make-undo-group editor 'put-custom-shape
            (let* ((shapes (edraw-create-selected-custom-shapes tool))
                   (ref-box (edraw-selected-custom-shapes-ref-box tool shapes))
                   (matrix (edraw-matrix-fit-rect-to-rect
                            ref-box (edraw-rect-pp down-xy move-xy))))
              (unless (edraw-matrix-identity-p matrix)
                (edraw-transform shapes matrix))))))))))

(cl-defmethod edraw-create-selected-custom-shapes
  ((tool edraw-editor-tool-custom-shape))
  (with-slots (selected-shape-descriptor-list
               selected-picker-entry-properties
               editor)
      tool
    (when selected-shape-descriptor-list
      (when-let ((shapes (edraw-shape-from-shape-descriptor-list
                          editor
                          (edraw-svg-body editor)
                          nil ;;top most
                          selected-shape-descriptor-list)))

        ;; Apply default properties
        (let ((keep-properties (plist-get selected-picker-entry-properties
                                          :keep-properties))
              ;;@todo Default values should be per tool, not per type
              (default-props (cdr
                              (edraw-get-default-shape-properties-by-tag
                               editor 'path))))
          ;; Filter default properties
          (pcase keep-properties
            ('nil )
            ('none )
            ('all
             (setq default-props nil))
            ((pred listp)
             (setq default-props
                   (seq-remove (lambda (prop)
                                 (memq (car prop) keep-properties))
                               default-props))))
          (dolist (shape shapes)
            (edraw-set-properties shape default-props)))

        ;; Select the shapes
        (edraw-select-shapes editor shapes)
        shapes))))

(cl-defmethod edraw-selected-custom-shapes-ref-box
  ((tool edraw-editor-tool-custom-shape) shapes)
  (when shapes
    (with-slots (selected-picker-entry-properties editor) tool
      (or
       (plist-get selected-picker-entry-properties :ref-box)
       (edraw-shape-aabb shapes)))))


;;;; Shape

;;;;; Shape Object Types & Creation

(defvar edraw-shape-types
  '((rect
     (:from-element . edraw-shape-rect-create)
     (:create-element . dom-node))
    (ellipse
     (:from-element . edraw-shape-ellipse-create)
     (:create-element . dom-node))
    (circle
     (:from-element . edraw-shape-circle-create)
     (:create-element . dom-node))
    (text
     (:from-element . edraw-shape-text-create)
     (:create-element . dom-node))
    (image
     (:from-element . edraw-shape-image-create)
     (:create-element . dom-node))
    (path
     (:from-element . edraw-shape-path-create)
     (:create-element . dom-node))
    (g
     (:from-element . edraw-shape-group-create)
     (:create-element . dom-node)))
  "Alist of shape object types.")

(defun edraw-shape-element-from-type (type)
  "Create a new SVG element of shape TYPE."
  (let ((create-element
         (alist-get :create-element (alist-get type edraw-shape-types))))
    (unless create-element
      (error "Unsupported shape type %s" type))
    (funcall create-element type)))

(defun edraw-shape-type-from-element (element)
  "Return a symbol that represents the shape type of the SVG ELEMENT."
  (or (dom-attr element 'data-edraw-type) ;;<?? data-edraw-type="type" ...>
      (dom-tag element))) ;;<type ...>

(defun edraw-shape-from-element-create (element editor)
  (when-let* ((type (edraw-shape-type-from-element element))
              (constructor (alist-get :from-element
                                      (alist-get type edraw-shape-types)))
              (shape (funcall constructor element editor)))
    ;; Attach the created SHAPE object to the ELEMENT.
    (dom-set-attribute element :-edraw-shape shape)
    shape))

(defun edraw-shape-from-element-no-create (element)
  (when (edraw-dom-element-p element)
    (dom-attr element :-edraw-shape)))

(defun edraw-shape-from-element (element editor &optional noerror-node-type)
  "Return the shape object for ELEMENT, creating a new one if needed."
  (if (not (edraw-dom-element-p element))
      (unless noerror-node-type
        (error "Unsupported node type %s" element))
    (or
     ;; Already created
     (edraw-shape-from-element-no-create element)
     ;; Create new
     (edraw-shape-from-element-create element editor)
     ;; Error
     (if noerror-node-type
         nil
       (error "Unsupported SVG element %s" (prin1-to-string element))))))


;;
;;

(defun edraw-merge-properties (props-default props-alist)
  (append
   (seq-difference props-default props-alist
                   (lambda (a b) (eq (car a) (car b))))
   props-alist))

(defun edraw-create-shape (editor parent type &rest props)
  (edraw-create-shape-without-default
   editor parent nil
   type
   ;; Complete property values with default values
   (edraw-merge-properties
    (alist-get type (oref editor default-shape-properties))
    ;; Convert plist to alist
    (cl-loop for (prop-name value) on props by #'cddr
             collect (cons prop-name value)))))

(defun edraw-create-shape-without-default (editor parent index type props-alist)
  (let* ((shape (edraw-shape-from-element
                 (edraw-create-shape-svg-element (oref editor defrefs)
                                                 parent index type props-alist)
                 editor)))
    (edraw-make-undo-group editor 'shape-create
      (edraw-push-undo editor 'shape-create (list 'edraw-remove shape))
      (edraw-on-shape-changed shape 'shape-create))
    shape))

(defun edraw-create-shape-svg-element (defrefs parent index type props-alist)
  (let ((element (edraw-shape-element-from-type type)))
    ;; Set properties
    (dolist (prop props-alist)
      (let ((prop-name (car prop))
            (value (cdr prop)))
        (edraw-svg-element-set-property element prop-name value defrefs)))
    ;; Add element to parent
    (when parent
      (if index
          (edraw-dom-insert-nth parent element index)
        (dom-append-child parent element)))
    element))

;;;;; Selection Menu

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
   (removed-p :initform nil)
   (point-connections :initform nil)
   (point-connection-referrers :initform nil))
  :abstract t)

(defun edraw-shape-derived-p (obj)
  (and obj (cl-typep obj 'edraw-shape)))

(cl-defmethod edraw-property-editor-shape-p
  ((_target edraw-shape))
  t) ;;see: edraw-property-editor.el

(cl-defgeneric edraw-shape-type (shape) ;; Must be implement in derived classes
  "Return a symbol representing the type of SHAPE.")

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

;;;;;; SVG

(cl-defgeneric edraw-svg-string (obj)
  "Make SVG string from OBJ.")
(cl-defmethod edraw-svg-string ((shape edraw-shape))
  (edraw-svg-to-string (edraw-element shape)
                       nil 'edraw-svg-print-attr-filter 0 t))

;;;;;; Clone

(cl-defmethod edraw-clone ((shape edraw-shape))
  (with-slots (editor) shape
    (when-let ((shape-type (edraw-shape-type shape)))
      (edraw-make-undo-group editor 'clone-shape
        (let ((new-shape
               (edraw-create-shape-without-default
                editor
                (edraw-parent-element shape)
                (1+ (edraw-node-position shape)) ;; After original shape
                shape-type
                ;; Copy all properties
                (mapcar
                 (lambda (prop-info)
                   (let* ((prop-name (plist-get prop-info :name))
                          (value (edraw-get-property shape prop-name)))
                     (cons prop-name value)))
                 (edraw-get-property-info-list shape)))))
          ;; Copy all children and insert
          (dolist (child (edraw-children shape))
            (let ((new-child (edraw-clone child)))
              (edraw-remove new-child)
              (edraw-insert new-shape new-child)))
          new-shape)))))

(cl-defmethod edraw-convert-to-path-shape ((shape edraw-shape))
  (with-slots (editor) shape
    (when-let ((cmdlist
                (edraw-svg-element-to-path-cmdlist (edraw-element shape)))
               (shape-descriptor
                (edraw-shape-descriptor shape)))
      (edraw-shape-descriptor-set-type shape-descriptor 'path)
      (edraw-shape-descriptor-put-property shape-descriptor 'd
                                           (edraw-path-cmdlist-to-string
                                            cmdlist))
      (edraw-make-undo-group editor 'convert-to-path
        ;; Create a new shape
        (when-let ((new-shape (edraw-shape-from-shape-descriptor
                               editor
                               (edraw-parent-element shape)
                               (edraw-node-position shape)
                               shape-descriptor)))
          ;; Select the new shape
          (when (edraw-selected-shape-p editor shape)
            (edraw-add-shape-selection editor new-shape))
          ;; Remove the old shape
          (edraw-remove shape))))))

;;;;;; Shape Descriptor

(cl-defmethod edraw-copy ((shape edraw-shape))
  (edraw-clipboard-set 'shape-descriptor-list
                       (list (edraw-shape-descriptor shape))))

(cl-defmethod edraw-cut ((shape edraw-shape))
  (edraw-clipboard-set 'shape-descriptor-list
                       (list (edraw-shape-descriptor shape)))
  (edraw-remove shape))

(cl-defmethod edraw-shape-descriptor ((shape edraw-shape))
  (nconc
   (list
    (cons :type (edraw-shape-type shape))
    (cons :properties (edraw-get-all-properties shape)))
   (when-let ((children (edraw-children shape)))
     (list
      (cons :children (mapcar #'edraw-shape-descriptor children))))))

(defun edraw-shape-descriptor-from-svg-element-without-editor (element)
  (when (edraw-dom-element-p element)
    (nconc
     (list
      (cons :type (edraw-shape-type-from-element element))
      (cons :properties
            (mapcar
             (lambda (prop-info)
               (let* ((prop-name (plist-get prop-info :name))
                      (prop-value (edraw-svg-element-get-property
                                   element prop-name nil))) ;;defrefs=nil
                 (cons prop-name prop-value)))
             (edraw-svg-element-get-property-info-list element))))
     (when (eq (edraw-shape-type-from-element element) 'g) ;;@todo type check??
       (when-let ((children (dom-children element)))
         (list
          (cons :children
                (mapcar
                 #'edraw-shape-descriptor-from-svg-element-without-editor
                 children))))))))

(defun edraw-shape-descriptor-set-type (shape-descriptor type)
  (setf (alist-get :type shape-descriptor) type))

(defun edraw-shape-descriptor-put-property (shape-descriptor prop-name value)
  ;;@todo remove property if value is nil?
  (setf (alist-get prop-name (alist-get :properties shape-descriptor))
        value))

(defun edraw-shape-from-shape-descriptor (editor parent index shape-descriptor)
  (let* ((type (alist-get :type shape-descriptor))
         (props (alist-get :properties shape-descriptor))
         (children-descriptor (alist-get :children shape-descriptor))
         (shape (edraw-create-shape-without-default editor parent index type props)))
    (when children-descriptor
      (dolist (child-descriptor children-descriptor)
        (edraw-shape-from-shape-descriptor
         editor (edraw-element shape) nil child-descriptor)))
    shape))

(defun edraw-shape-from-shape-descriptor-list (editor
                                               parent index
                                               shape-descriptor-list)
  (mapcar (lambda (shape-descriptor)
            (edraw-shape-from-shape-descriptor
             editor parent
             (when (integerp index) (prog1 index (cl-incf index)))
             shape-descriptor))
          shape-descriptor-list))

(defun edraw-shape-descriptor-from-svg-element (svg-element editor)
  (when (edraw-dom-element-p svg-element)
    (if-let ((shape (edraw-shape-from-element svg-element editor 'noerror)))
        (edraw-shape-descriptor shape)
      (message (edraw-msg "Unsupported SVG element: %s") svg-element)
      nil)))

(defun edraw-shape-descriptor-list-from-svg-string (svg-text editor)
  (when-let ((container (edraw-svg-decode (concat "<g>" svg-text "</g>") nil)))
    (cl-loop for node in (dom-children container)
             for desc = (edraw-shape-descriptor-from-svg-element node editor)
             when desc collect desc)))

(defun edraw-shape-descriptor-list-from-shape-picker-shape (shape-def editor)
  (cond
   ((eq (car-safe shape-def) 'shape-descriptor-list)
    (cdr shape-def))
   ((stringp shape-def)
    (edraw-shape-descriptor-list-from-svg-string shape-def editor))
   ((edraw-dom-element-p shape-def)
    (list (edraw-shape-descriptor-from-svg-element shape-def editor)))
   (t
    (message (edraw-msg "Unknown type of shape definition"))
    nil)))

(defun edraw-shape-descriptor-to-svg-element (shape-descriptor)
  (let* ((type (alist-get :type shape-descriptor))
         (props (alist-get :properties shape-descriptor))
         (children-descriptor (alist-get :children shape-descriptor))
         ;;@todo defrefs??? What happens when use marker attributes?
         (defrefs (edraw-svg-defrefs (dom-node 'defs))) ;;Dummy defrefs
         (element
          (edraw-create-shape-svg-element
           defrefs
           nil ;;parent
           nil ;;index
           type ;;type
           props)))
    (when children-descriptor
      (dolist (child-descriptor children-descriptor)
        (dom-append-child
         element
         (edraw-shape-descriptor-to-svg-element child-descriptor))))
    element))

(defun edraw-shape-descriptor-to-svg-string (shape-descriptor &optional editor)
  (if editor
      (when-let ((shape (edraw-shape-from-shape-descriptor editor nil nil
                                                           shape-descriptor)))
        (edraw-svg-string shape))
    (when-let ((element
                (edraw-shape-descriptor-to-svg-element shape-descriptor)))
      (edraw-svg-encode element nil nil))))

(defun edraw-shape-descriptor-list-to-svg-string (shape-descriptor-list &optional editor)
  (mapconcat
   (lambda (shape-descriptor)
     (edraw-shape-descriptor-to-svg-string shape-descriptor editor))
   shape-descriptor-list
   ""))

;;;;;; Hooks

(cl-defmethod edraw-on-shape-changed ((shape edraw-shape) type)
  (with-slots (editor) shape
    (edraw-update-related-point-connections shape type)
    (edraw-on-document-changed editor 'shape)
    (edraw-notify-change-hook shape type)
    (edraw-notify-ancestors-of-change shape type)))

(cl-defmethod edraw-notify-change-hook ((shape edraw-shape) type)
  (with-slots (editor change-hook) shape
    (edraw-hook-call change-hook shape type)))

(cl-defmethod edraw-add-change-hook ((shape edraw-shape) function &rest args)
  (with-slots (change-hook) shape
    (apply 'edraw-hook-add change-hook function args)))

(cl-defmethod edraw-remove-change-hook ((shape edraw-shape) function &rest args)
  (with-slots (change-hook) shape
    (apply 'edraw-hook-remove change-hook function args)))

(cl-defmethod edraw-notify-ancestors-of-change ((shape edraw-shape) type)
  "Notify ancestor nodes of change in descendant nodes."
  (let ((ancestor shape))
    (while (setq ancestor (edraw-parent ancestor))
      (edraw-on-descendant-changed ancestor shape type))))

(cl-defmethod edraw-on-descendant-changed ((_shape edraw-shape)
                                           _changed-shape _type)
  )

;;;;;; Parent

(cl-defmethod edraw-parent ((shape edraw-shape))
  (with-slots (editor) shape
    (edraw-shape-from-element-no-create ;;Returns nil if under document root
     (edraw-parent-element shape))))

;;;;;; Children

(cl-defmethod edraw-children ((shape edraw-shape))
  (with-slots (editor) shape
    (delq nil (mapcar (lambda (node)
                        (edraw-shape-from-element node editor 'noerror))
                      (dom-children (edraw-element shape))))))

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

;;;;;; Insert

(cl-defmethod edraw-insert (parent (shape edraw-shape) &optional pos)
  (with-slots (element editor removed-p) shape
    (when removed-p
      (let* ((parent-element
              (cond
               ((edraw-shape-derived-p parent) (edraw-element parent))
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
        (edraw-make-undo-group editor 'shape-insert
          (edraw-push-undo editor 'shape-insert (list 'edraw-remove shape));;@todo if not removed-p?
          (edraw-on-shape-changed shape 'shape-insert))))))

;;;;;; Remove

(cl-defmethod edraw-remove ((shape edraw-shape))
  (with-slots (element editor removed-p) shape
    (when (or (not removed-p)
              (edraw-parent-element shape))
      (edraw-make-undo-group editor 'shape-remove
        (edraw-push-undo
         editor
         'shape-remove
         (list 'edraw-insert
               (edraw-parent-element shape)
               shape
               (edraw-node-position shape)))
        (dom-remove-node (edraw-parent-element shape) element)
        (setq removed-p t)
        (edraw-on-shape-changed shape 'shape-remove))))) ;; Call edraw-update-related-point-connections

(cl-defmethod edraw-removed-p ((shape edraw-shape))
  (oref shape removed-p))

;;;;;; Z Order (Sibling Relationship)

(cl-defmethod edraw-node-siblings-count ((shape edraw-shape))
  (length (dom-children (edraw-parent-element shape))))

(cl-defmethod edraw-node-position ((shape edraw-shape))
  (seq-position
   (dom-children (edraw-parent-element shape))
   (edraw-element shape)
   #'eq))

(cl-defmethod edraw-set-node-position ((shape edraw-shape) new-pos)
  (let ((old-pos (edraw-node-position shape))
        (editor (oref shape editor)))
    (unless (equal new-pos old-pos)
      (edraw-make-undo-group editor 'shape-z-order
        (edraw-push-undo editor 'shape-z-order
                         (list 'edraw-set-node-position shape old-pos))
        (let ((parent (edraw-parent-element shape))
              (element (edraw-element shape)))
          (dom-remove-node parent element)
          (edraw-dom-insert-nth parent element new-pos))
        (edraw-on-shape-changed shape 'shape-z-order)))))

(cl-defmethod edraw-front-p ((shape edraw-shape))
  (edraw-dom-last-node-p (edraw-parent-element shape) (edraw-element shape)))

(cl-defmethod edraw-back-p ((shape edraw-shape))
  (edraw-dom-first-node-p (edraw-parent-element shape) (edraw-element shape)))

(cl-defmethod edraw-bring-to-front ((shape edraw-shape))
  (edraw-set-node-position shape (1- (edraw-node-siblings-count shape))))

(cl-defmethod edraw-bring-forward ((shape edraw-shape))
  (let* ((old-pos (edraw-node-position shape))
         (new-pos (1+ old-pos))
         (num-siblings (edraw-node-siblings-count shape)))
    (when (< new-pos num-siblings)
      (edraw-set-node-position shape new-pos))))

(cl-defmethod edraw-send-backward ((shape edraw-shape))
  (let* ((old-pos (edraw-node-position shape))
         (new-pos (1- old-pos)))
    (when (>= new-pos 0)
      (edraw-set-node-position shape new-pos))))

(cl-defmethod edraw-send-to-back ((shape edraw-shape))
  (edraw-set-node-position shape 0))

;;;;;; Transform

(cl-defmethod edraw-get-transform-method ((shape edraw-shape))
  (edraw-get-transform-method (oref shape editor)))

(cl-defgeneric edraw-translate (object xy)
  "Translate OBJECT by vector XY.")

(cl-defmethod edraw-translate ((objects list) xy)
  (dolist (object objects)
    (edraw-translate object xy)))

(cl-defmethod edraw-translate ((shape edraw-shape) &optional xy)
  (edraw-set-translate-params)
  (edraw-transform
   shape
   (edraw-matrix-translate (edraw-x xy) (edraw-y xy) 0)))

(cl-defgeneric edraw-transform (object matrix)
  "Transform OBJECT by MATRIX.

Transforms an OBJECT according to the default method defined by
the OBJECT itself.

For example, if the OBJECT is some shape, it may be transformed
by moving the anchor points, or the entire shape including line
width etc. may be transformed by the transform property.

If you want to reliably change the transform property, use
`edraw-transform-prop-multiply'.

If you want to transform the anchor point coordinates, use
`edraw-transform-anchor-points'.
")

(cl-defmethod edraw-transform ((shape edraw-shape) matrix)
  (pcase (edraw-get-transform-method shape)
    ('auto
     (edraw-transform-auto shape matrix))
    ('transform-property
     (edraw-transform-prop-multiply shape matrix))
    ('anchor-points
     (edraw-transform-anchor-points shape matrix))))

(cl-defmethod edraw-transform ((shapes list) matrix)
  (dolist (shape shapes)
    (edraw-transform shape matrix)))

(cl-defmethod edraw-scale ((shape edraw-shape) &optional origin-xy sx sy)
  (edraw-set-scale-params (edraw-shape-aabb shape))

  (edraw-transform
   shape
   (edraw-matrix-move-origin-xy (edraw-matrix-scale sx sy 1) origin-xy)))

(cl-defmethod edraw-rotate ((shape edraw-shape) &optional origin-xy angle)
  (edraw-set-rotate-params (edraw-shape-aabb shape))

  (edraw-transform
   shape
   (edraw-matrix-move-origin-xy (edraw-matrix-rotate angle) origin-xy)))

(cl-defmethod edraw-transform-anchor-points ((shape edraw-shape) matrix)
  (let ((transform-mat (edraw-transform-prop-get-matrix shape)))
    (if (edraw-matrix-identity-p transform-mat)
        ;; No transform property (Same as local transform).
        (edraw-transform-anchor-points-local shape matrix)
      ;; Use transform^-1 * matrix * transform
      (let ((transform-mat-inv (edraw-matrix-inverse transform-mat)))
        (when transform-mat-inv ;;@todo if nil?
          (edraw-transform-anchor-points-local
           shape
           (edraw-matrix-mul-mat-mat
            (edraw-matrix-mul-mat-mat
             transform-mat-inv
             matrix)
            transform-mat)))))))

(cl-defmethod edraw-apply-transform-prop-to-anchor-points ((shape edraw-shape))
  (edraw-make-undo-group
      (oref shape editor) 'apply-transform-prop-to-anchor-points
    ;; Apply transform property to anchor point coordinates.
    (edraw-transform-anchor-points-local
     shape
     (edraw-transform-prop-get-matrix shape))
    ;; Remove transform property.
    (edraw-set-property shape 'transform nil)))

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

;;;;;; Internal ID

(cl-defmethod edraw-internal-id ((shape edraw-shape))
  "Return the internal ID dedicated to edraw.

Returns the data-edraw-id attribute if it exists. If not,
generate a new ID, set it to the data-edraw-id attribute, and
return it."
  (with-slots (element editor) shape
    (or (dom-attr element 'data-edraw-id)
        (let (id)
          (while (progn
                   (setq id (edraw-internal-id-gen))
                   (edraw-find-shape-by-internal-id editor id)))
          (dom-set-attribute element 'data-edraw-id id)
          id))))

(defun edraw-internal-id-gen ()
  "Generate an ID for internal use."
  (let* ((rnd (sha1 (format "%s%s%s%s%s%s%s"
                            (random)
                            (current-time)
                            (user-uid)
                            (emacs-pid)
                            (user-full-name)
                            user-mail-address
                            (recent-keys)))))
    (substring rnd 0 8)))

;;;;;; UNDO

(cl-defmethod edraw-last-undo-data ((shape edraw-shape))
  (edraw-last-undo-data (oref shape editor)))

(cl-defmethod edraw-undo ((shape edraw-shape))
  (edraw-undo (oref shape editor)))

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
  "Return t if SHAPE holds the value of the property named PROP-NAME.

Return nil if no value is specified."
  (edraw-svg-element-has-property-p (edraw-element shape)
                                    prop-name
                                    (edraw-get-defrefs shape)))

(cl-defmethod edraw-can-have-property-p ((shape edraw-shape) prop-name)
  "Return t if SHAPE can have a property named PROP-NAME.

Return nil if the property named PROP-NAME is not valid for SHAPE."
  (edraw-svg-element-can-have-property-p (edraw-element shape)
                                         prop-name))

(cl-defmethod edraw-get-property-info-list ((shape edraw-shape))
  (edraw-svg-element-get-property-info-list (edraw-element shape)))

(cl-defmethod edraw-get-property ((shape edraw-shape) prop-name)
  (edraw-svg-element-get-property (edraw-element shape) prop-name
                                  (edraw-get-defrefs shape)))

(cl-defmethod edraw-get-property-as-length ((shape edraw-shape) prop-name
                                            &optional default-value)
  (or (edraw-svg-length-string-to-number (edraw-get-property shape prop-name))
      (or default-value 0)))

(cl-defmethod edraw-get-all-properties ((shape edraw-shape));;@todo generalize
  (cl-loop for prop-info in (edraw-get-property-info-list shape)
           collect (let ((prop-name (plist-get prop-info :name)))
                     (cons prop-name
                           (edraw-get-property shape prop-name)))))

(cl-defmethod edraw-set-properties ((shape edraw-shape) prop-list)
  "Returns t if the property is actually changed."
  (edraw-set-properties-internal shape prop-list nil))

(cl-defmethod edraw-set-properties-internal ((shape edraw-shape) prop-list
                                             old-prop-list)
  "Returns t if the property is actually changed."
  (let ((defrefs (edraw-get-defrefs shape)))
    (with-slots (element) shape
      (dolist (prop prop-list)
        (let* ((prop-name (car prop))
               (new-value (cdr prop))
               (old-value (edraw-svg-element-get-property element prop-name
                                                          defrefs)))
          (unless (equal new-value old-value)
            ;;(message "%s: %s to %s" prop-name old-value new-value)
            (push (cons prop-name old-value) old-prop-list)
            (edraw-svg-element-set-property element prop-name new-value
                                            defrefs)))))
    (when old-prop-list
      (let ((editor (oref shape editor)))
        (edraw-make-undo-group editor 'shape-properties
          (edraw-push-undo
           editor
           'shape-properties
           (list #'edraw-set-properties shape old-prop-list))
          (edraw-on-shape-properties-changed shape old-prop-list)
          ;; NOTE: Other connected shapes may change.
          (edraw-on-shape-changed shape 'shape-properties)))
      t)))

(cl-defmethod edraw-on-shape-properties-changed ((_shape edraw-shape)
                                                 _old-prop-list)
  )

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
                         ;; nil, string, or number
                         (dom-attr (edraw-element shape) prop-name)))
                 prop-names))))

(cl-defmethod edraw-set-all-properties-as-default ((shape edraw-shape))
  (edraw-set-default-shape-properties-from-shape
   (oref shape editor)
   shape))

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

(defun edraw-duplicate-shapes (shapes)
  (when shapes
    (with-slots (editor) (car shapes)
      (let* (;; Sort by Z order
             (sorted-shapes
              (seq-sort-by #'edraw-node-position #'< shapes))
             (shape-descriptor-list
              (mapcar #'edraw-shape-descriptor sorted-shapes))
             (z-index
              (1+ (edraw-node-position (car (last sorted-shapes))))))

        (edraw-make-undo-group editor 'duplicate-shapes
          (edraw-shape-from-shape-descriptor-list
           editor (edraw-svg-body editor)
           z-index
           shape-descriptor-list))))))

(cl-defmethod edraw-edit-properties ((shape edraw-shape))
  (edraw-property-editor-open shape))

(cl-defmethod edraw-edit-property-paint ((shape edraw-shape) prop-name)
  (let* ((curr-value (edraw-get-property shape prop-name))
         (new-value
          (edraw-editor-with-silent-modifications
            (unwind-protect
                (edraw-color-picker-read-color
                 (format "%s: " prop-name)
                 (or curr-value "")
                 '("" "none")
                 `((:color-name-scheme . web)
                   (:no-color . "none")
                   (:on-input-change
                    . ,(lambda (string color)
                         (when (or (member string '("" "none"))
                                   color)
                           ;;@todo suppress modified flag change and notification
                           (edraw-set-property shape prop-name string))))
                   (:scale-direct . ,(oref (oref shape editor) image-scale))))
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
     (((edraw-msg "Fill...") edraw-edit-fill
       :visible ,(edraw-can-have-property-p shape 'fill))
      ((edraw-msg "Stroke...") edraw-edit-stroke
       :visible ,(edraw-can-have-property-p shape 'stroke))
      ((edraw-msg "Href...") edraw-edit-href
       :visible ,(edraw-can-have-property-p shape 'href))
      ((edraw-msg "Font Size...") edraw-edit-font-size
       :visible ,(edraw-can-have-property-p shape 'font-size))))
    ((edraw-msg "Transform")
     (((edraw-msg "Translate...") edraw-translate)
      ((edraw-msg "Scale...") edraw-scale)
      ((edraw-msg "Rotate...") edraw-rotate)
      ((edraw-msg "Apply transform property to anchors") edraw-apply-transform-prop-to-anchor-points
       :enable ,(and (edraw-transform-prop-exists-p shape)
                     (not (edraw-matrix-identity-p
                           (edraw-transform-prop-get-matrix shape)))))
      ,(edraw-transform-method-menu shape)))
    ((edraw-msg "Z-Order")
     (((edraw-msg "Bring to Front") edraw-bring-to-front
       :enable ,(not (edraw-front-p shape)))
      ((edraw-msg "Bring Forward") edraw-bring-forward
       :enable ,(not (edraw-front-p shape)))
      ((edraw-msg "Send Backward") edraw-send-backward
       :enable ,(not (edraw-back-p shape)))
      ((edraw-msg "Send to Back") edraw-send-to-back
       :enable ,(not (edraw-back-p shape)))))
    ((edraw-msg "Glue")
     (((edraw-msg "Glue to selected or overlapped shape") edraw-glue-to-selected-or-overlapped-shape
       :enable ,(null (edraw-get-point-connections shape)))
      ((edraw-msg "Unglue All") edraw-unglue-all
       :enable ,(not (null (edraw-get-point-connections shape))))))
    ((edraw-msg "Delete...") edraw-delete-with-confirm)
    ((edraw-msg "Duplicate") edraw-duplicate-and-select)
    ((edraw-msg "Copy") edraw-copy)
    ((edraw-msg "Cut") edraw-cut)))

(cl-defmethod edraw-popup-context-menu ((shape edraw-shape))
  (edraw-popup-menu
   (edraw-get-summary shape)
   (edraw-get-actions shape)
   shape))

;;;;;; Boundary

(cl-defgeneric edraw-shape-aabb (object)
  "Return the axis aligned bounding box of the OBJECT.")

(cl-defmethod edraw-shape-aabb ((shape edraw-shape))
  (edraw-svg-shape-aabb (edraw-element shape))) ;;@todo cache?

(cl-defmethod edraw-shape-aabb ((shapes list))
  (let (aabb)
    (dolist (shape shapes)
      (setq aabb (edraw-rect-union aabb (edraw-shape-aabb shape))))
    aabb))

(cl-defgeneric edraw-shape-aabb-local (object)
  "Return the local axis aligned bounding box of the OBJECT.")

(cl-defmethod edraw-shape-aabb-local ((shape edraw-shape))
  (edraw-svg-shape-aabb (edraw-element shape) nil t)) ;;@todo cache?

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

;;;;;; Anchor Point

(cl-defgeneric edraw-get-anchor-points (object) ;; Must be implement in derived classes
  "Return a list of anchor points that OBJECT has.")

(cl-defgeneric edraw-get-anchor-point-count (object)
  "Return (length (edraw-get-anchor-points OBJECT)).

Some classes have efficient implementations."
  (length (edraw-get-anchor-points object)))

(cl-defgeneric edraw-get-nth-anchor-point (object index)
  "Return (nth INDEX (edraw-get-anchor-points OBJECT)).

Some classes have efficient implementations."
  (nth index (edraw-get-anchor-points object)))

;;;;;; Point Connection

(cl-defmethod edraw-glue-destination-of-selected-or-overlapped-shape
  ((shape edraw-shape))
  "Return the shape that is the glue destination."
  (with-slots (editor) shape
    (let* ((selected-shapes ;;without this shape
            (remq shape (edraw-selected-shapes editor)))
           ;;(remq shape (edraw-all-shapes editor)))))
           (dst-shape
            (or
             ;; the selected shape
             (and (null (cdr selected-shapes))
                  (car selected-shapes))
             ;; the most front overlapping shape
             (car (edraw-find-shapes-by-rect
                   (reverse
                    (or selected-shapes ;;@todo sort?
                        (remq shape (edraw-all-shapes editor))))
                   (edraw-shape-aabb shape))))))
      dst-shape)))

(cl-defmethod edraw-glue-to-selected-or-overlapped-shape ((shape edraw-shape))
  ;; Determine the destination shape
  (let* ((dst-shape (edraw-glue-destination-of-selected-or-overlapped-shape shape)))
    (when dst-shape
      (edraw-make-undo-group (oref shape editor) 'glue-to-selected-or-overlapped-shape
        (let ((conn (edraw-point-connection
                     :src (edraw-point-connection-src-aabb
                           :shape shape :x-ratio 0.5 :y-ratio 0.5)
                     :dst (edraw-point-connection-dst-shape
                           :shape dst-shape))))
          ;; Update coordinates before adding CONN to SHAPE.
          (edraw-update conn)
          ;; Add CONN to SHAPE.
          (edraw-add-point-connection shape conn)
          ;; Do not update coordinates after adding CONN.
          ;; When undoing, CONN must be removed before undoing XY move.
          ;;(edraw-update-all-point-connections shape)
          )))))

(cl-defmethod edraw-unglue-all ((shape edraw-shape))
  (edraw-make-undo-group (oref shape editor) 'unglue-all
    (edraw-remove-all-point-connections shape)))

;;;;; Shape - Rect Boundary

(defclass edraw-shape-with-rect-boundary (edraw-shape)
  ((anchor-points :initform nil)
   (p0p1)) ;;Note that p0(car p0p1) is not always in the upper left
  :abstract t)

(cl-defgeneric edraw-get-rect-local ((shape edraw-shape-with-rect-boundary)))

(cl-defmethod edraw-make-anchor-points-from-element
  ((shape edraw-shape-with-rect-boundary))
  (with-slots (element anchor-points p0p1) shape
    (when (null anchor-points)
      (setq p0p1 (edraw-get-rect-local shape))
      (setq anchor-points
            (list
             ;; Check `edraw-get-anchor-point-count' when changing
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

(cl-defmethod edraw-get-anchor-point-count ((_shape edraw-shape-with-rect-boundary))
  ;; (edraw-make-anchor-points-from-element shape)
  ;; (with-slots (anchor-points) shape
  ;;   (length anchor-points))
  8) ;; !!!!!

(cl-defmethod edraw-get-anchor-points ((shape edraw-shape-with-rect-boundary))
  (edraw-make-anchor-points-from-element shape)
  (with-slots (anchor-points) shape
    anchor-points))

(cl-defmethod edraw-set-anchor-position ((shape edraw-shape-with-rect-boundary)
                                         anchor
                                         xy)
  "Returns t if the property is actually changed."
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
        (let ((old-p0p1 (cons (cons (caar p0p1) (cdar p0p1))
                              (cons (cadr p0p1) (cddr p0p1)))))
          (when px (setcar px x))
          (when py (setcdr py y))
          (edraw-on-anchor-position-changed shape old-p0p1))))))

(cl-defmethod edraw-get-anchor-position ((shape edraw-shape-with-rect-boundary)
                                         anchor)
  (with-slots (p0p1) shape
    (let* ((ref-x (edraw-ref-x anchor))
           (ref-y (edraw-ref-y anchor))
           (px (pcase ref-x (0 (car p0p1)) (1 (cdr p0p1))))
           (py (pcase ref-y (0 (car p0p1)) (1 (cdr p0p1)))))
      (cons
       (if px (car px) (* 0.5 (+ (caar p0p1) (cadr p0p1))))
       (if py (cdr py) (* 0.5 (+ (cdar p0p1) (cddr p0p1))))))))

(cl-defmethod edraw-set-rect ((shape edraw-shape-with-rect-boundary) xy0 xy1)
  (edraw-make-anchor-points-from-element shape) ;;Make sure p0p1 is initialized
  ;;@todo
  (with-slots (p0p1) shape
    (when (or (/= (caar p0p1) (car xy0))
              (/= (cdar p0p1) (cdr xy0))
              (/= (cadr p0p1) (car xy1))
              (/= (cddr p0p1) (cdr xy1)))
      ;;changed
      (let ((old-p0p1 (cons (cons (caar p0p1) (cdar p0p1))
                            (cons (cadr p0p1) (cddr p0p1)))))
        (setcar (car p0p1) (car xy0))
        (setcdr (car p0p1) (cdr xy0))
        (setcar (cdr p0p1) (car xy1))
        (setcdr (cdr p0p1) (cdr xy1))
        (edraw-on-anchor-position-changed shape old-p0p1)))))

(cl-defmethod edraw-set-p0p1-without-notify
  ((shape edraw-shape-with-rect-boundary) left top right bottom)
  (edraw-make-anchor-points-from-element shape) ;;Make sure p0p1 is initialized
  ;; Change the coordinates of p0p1 without changing the left/right and
  ;; top/bottom positional relationships
  (with-slots (p0p1) shape
    (let ((p0 (car p0p1))
          (p1 (cdr p0p1)))
      (if (<= (edraw-x p0) (edraw-x p1))
          (setf (edraw-x p0) left
                (edraw-x p1) right)
        (setf (edraw-x p0) right
              (edraw-x p1) left))

      (if (<= (edraw-y p0) (edraw-y p1))
          (setf (edraw-y p0) top
                (edraw-y p1) bottom)
        (setf (edraw-y p0) bottom
              (edraw-y p1) top)))))

(cl-defmethod edraw-transform-auto ((shape edraw-shape-with-rect-boundary) matrix)
  (cond
   ;; `transform' property already used.
   ((edraw-transform-prop-exists-p shape)
    (edraw-transform-prop-multiply shape matrix))
   ;; Rotation (including Skew) cannot be expressed only by moving
   ;; anchor points, so use the transform property.
   ((edraw-matrix-contains-rotation-p matrix)
    (edraw-transform-prop-multiply shape matrix))
   ;; Move anchor points.
   (t
    (edraw-transform-anchor-points-local shape matrix))))

(cl-defmethod edraw-transform-anchor-points-local ((shape edraw-shape-with-rect-boundary) matrix)
  (edraw-make-anchor-points-from-element shape) ;;Make sure p0p1 is initialized
  (with-slots (p0p1) shape
    (let ((new-p0 (edraw-matrix-mul-mat-xy matrix (car p0p1)))
          (new-p1 (edraw-matrix-mul-mat-xy matrix (cdr p0p1))))
      (edraw-set-rect shape new-p0 new-p1)
      ;;(edraw-on-shape-changed shape 'shape-transform) ?
      )))

;;;;;; Implemented in Derived Classes

;;(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-*) old-p0p1)


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

(cl-defmethod edraw-get-rect-local ((shape edraw-shape-rect))
  (let ((x (edraw-get-property-as-length shape 'x 0))
        (y (edraw-get-property-as-length shape 'y 0))
        (width (edraw-get-property-as-length shape 'width 0))
        (height (edraw-get-property-as-length shape 'height 0)))
    (cons (cons x y)
          (cons (+ x width) (+ y height)))))

(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-rect) _old-p0p1)
  "Returns t if the property is actually changed."
  (with-slots (p0p1) shape
    ;; Update x,y,width,height from p0p1
    ;; @todo Suppress p0p1 changes in edraw-on-shape-properties-changed?
    (edraw-set-properties
     shape
     (list
      (cons 'x      (min (caar p0p1) (cadr p0p1)))
      (cons 'y      (min (cdar p0p1) (cddr p0p1)))
      (cons 'width  (abs (- (caar p0p1) (cadr p0p1))))
      (cons 'height (abs (- (cdar p0p1) (cddr p0p1))))))))

(cl-defmethod edraw-on-shape-properties-changed ((shape edraw-shape-rect)
                                                 old-prop-list)
  (when (seq-find (lambda (name-value)
                    (memq (car name-value) '(x y width height)))
                  old-prop-list)
    ;; Update p0p1 from x,y,width,height
    (let* ((left   (edraw-get-property-as-length shape 'x 0))
           (top    (edraw-get-property-as-length shape 'y 0))
           (width  (edraw-get-property-as-length shape 'width 0))
           (height (edraw-get-property-as-length shape 'height 0))
           (right  (+ left width))
           (bottom (+ top height)))

      (edraw-set-p0p1-without-notify shape left top right bottom))))

(cl-defmethod edraw-get-actions ((_shape edraw-shape-rect))
  (let* ((items (copy-tree (cl-call-next-method))))
    (append
     items
     `(((edraw-msg "Convert To Path") edraw-convert-to-path-shape)))))



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

(cl-defmethod edraw-get-rect-local ((shape edraw-shape-ellipse))
  (let ((cx (edraw-get-property-as-length shape 'cx 0))
        (cy (edraw-get-property-as-length shape 'cy 0))
        (rx (edraw-get-property-as-length shape 'rx 0))
        (ry (edraw-get-property-as-length shape 'ry 0)))
    (cons (cons (- cx rx) (- cy ry))
          (cons (+ cx rx) (+ cy ry)))))

(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-ellipse) _old-p0p1)
  "Returns t if the property is actually changed."
  (with-slots (p0p1) shape
    ;; Update x,y,width,height from p0p1
    ;; @todo Suppress p0p1 changes in edraw-on-shape-properties-changed?
    (edraw-set-properties
     shape
     (list
      (cons 'cx (* 0.5 (+ (caar p0p1) (cadr p0p1))))
      (cons 'cy (* 0.5 (+ (cdar p0p1) (cddr p0p1))))
      (cons 'rx (* 0.5 (abs (- (caar p0p1) (cadr p0p1)))))
      (cons 'ry (* 0.5 (abs (- (cdar p0p1) (cddr p0p1)))))))))

(cl-defmethod edraw-on-shape-properties-changed ((shape edraw-shape-ellipse)
                                                 old-prop-list)
  (when (seq-find (lambda (name-value)
                    (memq (car name-value) '(cx cy rx ry)))
                  old-prop-list)
    ;; Update p0p1 from cx,cy,rx,ry
    (let* ((cx (edraw-get-property-as-length shape 'cx 0))
           (cy (edraw-get-property-as-length shape 'cy 0))
           (rx (edraw-get-property-as-length shape 'rx 0))
           (ry (edraw-get-property-as-length shape 'ry 0))
           (left   (- cx rx))
           (top    (- cy ry))
           (right  (+ cx rx))
           (bottom (+ cy ry)))

      (edraw-set-p0p1-without-notify shape left top right bottom))))

(cl-defmethod edraw-get-actions ((_shape edraw-shape-ellipse))
  (let* ((items (copy-tree (cl-call-next-method))))
    (append
     items
     `(((edraw-msg "Convert To Path") edraw-convert-to-path-shape)))))



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

(cl-defmethod edraw-get-rect-local ((shape edraw-shape-circle))
  (let ((cx (edraw-get-property-as-length shape 'cx 0))
        (cy (edraw-get-property-as-length shape 'cy 0))
        (r (edraw-get-property-as-length shape 'r 0)))
    (cons (cons (- cx r) (- cy r))
          (cons (+ cx r) (+ cy r)))))

(cl-defmethod edraw-set-anchor-position ((shape edraw-shape-circle)
                                         anchor
                                         xy)
  "Returns t if the property is actually changed."
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
           (changed nil)
           (old-p0p1 (cons (cons (caar p0p1) (cdar p0p1))
                           (cons (cadr p0p1) (cddr p0p1)))))
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
        (edraw-on-anchor-position-changed shape old-p0p1)))))

(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-circle) _old-p0p1)
  "Returns t if the property is actually changed."
  (with-slots (p0p1) shape
    ;; Update x,y,width,height from p0p1
    ;; @todo Suppress p0p1 changes in edraw-on-shape-properties-changed?
    (edraw-set-properties
     shape
     (let* ((p0 (car p0p1))
            (p1 (cdr p0p1)))
       (list
        (cons 'cx (* 0.5 (+ (car p0) (car p1))))
        (cons 'cy (* 0.5 (+ (cdr p0) (cdr p1))))
        (cons 'r (max (* 0.5 (abs (- (car p0) (car p1))))
                      (* 0.5 (abs (- (cdr p0) (cdr p1)))))))))))

(cl-defmethod edraw-on-shape-properties-changed ((shape edraw-shape-circle)
                                                 old-prop-list)
  (when (seq-find (lambda (name-value)
                    (memq (car name-value) '(cx cy r)))
                  old-prop-list)
    ;; Update p0p1 from cx,cy,r
    (let* ((cx (edraw-get-property-as-length shape 'cx 0))
           (cy (edraw-get-property-as-length shape 'cy 0))
           (r (edraw-get-property-as-length shape 'r 0))
           (left   (- cx r))
           (top    (- cy r))
           (right  (+ cx r))
           (bottom (+ cy r)))

      (edraw-set-p0p1-without-notify shape left top right bottom))))

(cl-defmethod edraw-get-actions ((_shape edraw-shape-circle))
  (let* ((items (copy-tree (cl-call-next-method))))
    (append
     items
     `(((edraw-msg "Convert To Path") edraw-convert-to-path-shape)))))



;;;;; Shape - Text

(defun edraw-shape-text-create (element editor)
  (let ((shape (edraw-shape-text)))
    (oset shape element element)
    (oset shape editor editor)
    (oset shape anchor-points (list (edraw-shape-point-text :shape shape))) ;; Check `edraw-get-anchor-point-count' when changing
    shape))

(defclass edraw-shape-text (edraw-shape)
  ((anchor-points)))

(cl-defmethod edraw-shape-type ((_shape edraw-shape-text))
  'text)

(cl-defmethod edraw-get-anchor-points ((shape edraw-shape-text))
  (oref shape anchor-points))

(cl-defmethod edraw-get-anchor-point-count ((_shape edraw-shape-text))
  1) ;; !!!!!

(cl-defmethod edraw-get-anchor-position ((shape edraw-shape-text))
  (with-slots (element) shape
    (cons
     (or (edraw-svg-attr-coord element 'x) 0)
     (or (edraw-svg-attr-coord element 'y) 0))))

(cl-defmethod edraw-set-anchor-position ((shape edraw-shape-text) xy)
  "Returns t if the property is actually changed."
  (when (or (/= (edraw-x xy) (edraw-get-property-as-length shape 'x 0))
            (/= (edraw-y xy) (edraw-get-property-as-length shape 'y 0)))
    (edraw-set-properties
     shape
     (list
      (cons 'x (edraw-x xy))
      (cons 'y (edraw-y xy))))))

(cl-defmethod edraw-transform-auto ((shape edraw-shape-text) matrix)
  (cond
   ;; `transform' property already used.
   ((edraw-transform-prop-exists-p shape)
    (edraw-transform-prop-multiply shape matrix))
   ;; Scaling and rotation (including skew) cannot be expressed only
   ;; by moving anchor points, so use the transform property.
   ((not (edraw-matrix-translation-only-p matrix))
    (edraw-transform-prop-multiply shape matrix))
   ;; Move anchor points.
   (t
    (edraw-transform-anchor-points-local shape matrix))))

(cl-defmethod edraw-transform-anchor-points-local ((shape edraw-shape-text) matrix)
  (with-slots (element) shape
    (let* ((xy (cons (edraw-get-property-as-length shape 'x 0)
                     (edraw-get-property-as-length shape 'y 0)))
           (new-xy (edraw-matrix-mul-mat-xy matrix xy)))

      (edraw-set-properties
       shape
       (list
        (cons 'x (edraw-x new-xy))
        (cons 'y (edraw-y new-xy)))))))

(cl-defmethod edraw-edit-font-size ((shape edraw-shape-text))
  (let ((font-size (edraw-read-number-or-nil
                    (edraw-msg "Font Size: ")
                    (edraw-get-property shape 'font-size))))
    (edraw-set-property shape 'font-size font-size)))



;;;;; Shape - Image

(defun edraw-shape-image-create (element editor)
  (let ((shape (edraw-shape-image)))
    (oset shape element element)
    (oset shape editor editor)
    shape))

(defclass edraw-shape-image (edraw-shape-with-rect-boundary)
  ())

(cl-defmethod edraw-shape-type ((_shape edraw-shape-image))
  'image)

(cl-defmethod edraw-get-rect-local ((shape edraw-shape-image))
  (let ((x (edraw-get-property-as-length shape 'x 0))
        (y (edraw-get-property-as-length shape 'y 0))
        (width (edraw-get-property-as-length shape 'width 0))
        (height (edraw-get-property-as-length shape 'height 0)))
    (cons (cons x y)
          (cons (+ x width) (+ y height)))))

(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-image) _old-p0p1)
  "Returns t if the property is actually changed."
  (with-slots (p0p1) shape
    ;; Update x,y,width,height from p0p1
    ;; @todo Suppress p0p1 changes in edraw-on-shape-properties-changed?
    (edraw-set-properties
     shape
     (list
      (cons 'x      (min (caar p0p1) (cadr p0p1)))
      (cons 'y      (min (cdar p0p1) (cddr p0p1)))
      (cons 'width  (abs (- (caar p0p1) (cadr p0p1))))
      (cons 'height (abs (- (cdar p0p1) (cddr p0p1))))))))

(cl-defmethod edraw-on-shape-properties-changed ((shape edraw-shape-image)
                                                 old-prop-list)
  (when (seq-find (lambda (name-value)
                    (memq (car name-value) '(x y width height)))
                  old-prop-list)
    ;; Update p0p1 from x,y,width,height
    (let* ((left   (edraw-get-property-as-length shape 'x 0))
           (top    (edraw-get-property-as-length shape 'y 0))
           (width  (edraw-get-property-as-length shape 'width 0))
           (height (edraw-get-property-as-length shape 'height 0))
           (right  (+ left width))
           (bottom (+ top height)))

      (edraw-set-p0p1-without-notify shape left top right bottom))))

(cl-defmethod edraw-edit-href ((shape edraw-shape-image))
  (let ((filename (read-file-name (edraw-msg "Image File: ")
                                  nil
                                  (edraw-get-property shape 'href)
                                  t)))
    (edraw-set-property shape 'href (file-relative-name filename))))


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
  (let (old-prop-list)
    (when-let ((d-cell (assq 'd prop-list)))
      (with-slots (cmdlist) shape
        (let ((new-d (or (cdr d-cell) ""))
              (old-d-attr (dom-attr (edraw-element shape) 'd))
              (old-d-prop (edraw-path-cmdlist-to-string cmdlist)))
          (unless (or (string= new-d old-d-attr)
                      (string= new-d old-d-prop))
            ;;(message "Change path data %s => %s" old-d-prop new-d)
            ;; Update cmdlist
            (edraw-path-cmdlist-swap cmdlist (edraw-path-cmdlist-from-d new-d))
            ;; Update d= attribute
            (edraw-update-path-data shape)
            (push (cons 'd old-d-attr) old-prop-list))))
      (setf (alist-get 'd prop-list nil 'remove) nil))
    ;; other properties
    (edraw-set-properties-internal shape prop-list old-prop-list)))

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
       ((edraw-msg "Make Smooth") edraw-make-smooth)

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

(cl-defmethod edraw-transform-auto ((shape edraw-shape-path) matrix)
  (cond
   ;; `transform' property already used.
   ((edraw-transform-prop-exists-p shape)
    (edraw-transform-prop-multiply shape matrix))
   ;; Move anchor points.
   (t
    (edraw-transform-anchor-points-local shape matrix))))

(cl-defmethod edraw-transform-anchor-points-local ((shape edraw-shape-path) matrix)
  (with-slots (cmdlist) shape
    (edraw-path-cmdlist-transform cmdlist matrix))
  (edraw-make-undo-group (oref shape editor) 'shape-path-transform
    (edraw-push-undo-properties shape 'shape-path-transform '(d))
    (edraw-update-path-data shape)
    (edraw-on-shape-changed shape 'shape-transform)))

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

(cl-defmethod edraw-get-anchor-point-count ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (edraw-path-cmdlist-anchor-point-count cmdlist)))

(cl-defmethod edraw-pick-point ((shape edraw-shape-path) xy)
  (with-slots (cmdlist) shape
    (let ((scale (if-let ((editor (oref shape editor)))
                     (float (edraw-scroll-scale editor))
                   1.0)))
      (when-let ((ppoint (edraw-path-cmdlist-pick-point
                          cmdlist
                          xy
                          (/ edraw-anchor-point-input-radius scale)
                          (/ edraw-handle-point-input-radius scale))))
        (edraw-shape-point-path
         :shape shape
         :ppoint ppoint)))))

;; (cl-defmethod edraw-owned-shape-point-p ((shape edraw-shape-path) spt)
;;   (with-slots (cmdlist) shape
;;   ))

(cl-defmethod edraw-get-nth-point ((shape edraw-shape-path) index)
  (with-slots (cmdlist) shape
    (when-let (ppoint (edraw-path-cmdlist-nth-point cmdlist index))
      (edraw-shape-point-path
       :shape shape
       :ppoint ppoint))))

(cl-defmethod edraw-get-nth-anchor-point ((shape edraw-shape-path) index)
  (with-slots (cmdlist) shape
    (when-let ((ppoint (edraw-path-cmdlist-nth-anchor-point cmdlist index)))
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

      (edraw-make-undo-group (oref shape editor) 'shape-path-transform
        (edraw-push-undo-properties shape 'shape-path-transform '(d))
        (edraw-update-path-data shape)
        (edraw-on-shape-changed shape 'anchor-add))

      ;; Return a new shape point
      (edraw-shape-point-path
       :shape shape
       :ppoint anchor-point))))

(cl-defmethod edraw-move-nth-point ((shape edraw-shape-path) index xy)
  (let ((spt (edraw-get-nth-point shape index)))
    (if spt
        (edraw-move spt xy)
      ;;(message "Warning: Point %s not found" index)
      )))

(cl-defmethod edraw-move-nth-points ((shape edraw-shape-path) list-index-xy)
  (dolist (index-xy list-index-xy)
    (edraw-move-nth-point shape (car index-xy) (cdr index-xy))))

(cl-defmethod edraw-update-path-data ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (edraw-svg-set-attr-string (edraw-element shape)
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
      (edraw-make-undo-group (oref shape editor) 'shape-path-close
        (edraw-push-undo-properties shape 'shape-path-close '(d))
        (edraw-update-path-data shape)
        (edraw-on-shape-changed shape 'shape-close-path))
      t)))

(cl-defmethod edraw-open-path-shape ((shape edraw-shape-path))
  (with-slots (cmdlist) shape
    (when (edraw-path-cmdlist-open-path cmdlist)
      (edraw-make-undo-group (oref shape editor) 'shape-path-open
        (edraw-push-undo-properties shape 'shape-path-open '(d))
        (edraw-update-path-data shape)
        (edraw-on-shape-changed shape 'shape-open-path))
      t)))

(cl-defmethod edraw-reverse-path ((shape edraw-shape-path))
  "Reverse the order of anchor points in the path."
  (with-slots (cmdlist) shape
    (edraw-path-cmdlist-reverse cmdlist)
    (edraw-make-undo-group (oref shape editor) 'shape-path-reverse
      (edraw-push-undo-properties shape 'shape-path-reverse '(d))
      (edraw-update-path-data shape)
      (edraw-on-shape-changed shape 'shape-reverse-path))
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

(defun edraw-xy-points-to-smooth-path-data (xy-points)
  (let* ((points
          (edraw-xy-remove-consecutive-same-points xy-points))
         (points-vector (apply #'vector points))
         (size (length points-vector)))
    (when (>= size 2)
      (let* ((bezier-segments
              (edraw-fit-bezier-curve points-vector))
             (d
              (concat
               (format
                "M%s %s"
                (edraw-to-string (edraw-x (aref (car bezier-segments) 0)))
                (edraw-to-string (edraw-y (aref (car bezier-segments) 0))))
               (mapconcat
                (lambda (segment)
                  (format "C%s %s %s %s %s %s"
                          (edraw-to-string (edraw-x (aref segment 1)))
                          (edraw-to-string (edraw-y (aref segment 1)))
                          (edraw-to-string (edraw-x (aref segment 2)))
                          (edraw-to-string (edraw-y (aref segment 2)))
                          (edraw-to-string (edraw-x (aref segment 3)))
                          (edraw-to-string (edraw-y (aref segment 3)))))
                bezier-segments ""))))
        d))))

(cl-defmethod edraw-make-smooth ((path edraw-shape-path))
  (when-let ((d (edraw-xy-points-to-smooth-path-data
                 (mapcar #'edraw-get-xy (edraw-get-anchor-points path)))))
    (edraw-set-property path 'd d)
    ;; Succeeded
    t))



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

(cl-defmethod edraw-on-descendant-changed ((shape edraw-shape-group)
                                           _changed-shape _type)
  (let ((aabb (edraw-shape-aabb-local shape)))
    (if aabb
        (edraw-set-p0p1-without-notify shape
                                       (edraw-rect-left aabb)
                                       (edraw-rect-top aabb)
                                       (edraw-rect-right aabb)
                                       (edraw-rect-bottom aabb))
      (edraw-set-p0p1-without-notify shape 0 0 0 0)))
  ;;@todo Should it be a different kind of notification?
  (edraw-on-shape-changed shape 'group-contents))

(cl-defmethod edraw-get-rect-local ((shape edraw-shape-group))
  (or (edraw-shape-aabb-local shape)
      (edraw-rect 0 0 0 0)))

(cl-defmethod edraw-set-p0p1-without-notify
  ((shape edraw-shape-with-rect-boundary) left top right bottom)
  (edraw-make-anchor-points-from-element shape) ;;Make sure p0p1 is initialized
  ;; Change the coordinates of p0p1
  (with-slots (p0p1) shape
    (let ((p0 (car p0p1))
          (p1 (cdr p0p1)))
      (setf (edraw-x p0) left
            (edraw-x p1) right
            (edraw-y p0) top
            (edraw-y p1) bottom))))


(cl-defmethod edraw-on-anchor-position-changed ((shape edraw-shape-group) old-p0p1)
  "Returns t if the property is actually changed."
  (with-slots (p0p1) shape
    ;; Transform descendants from current local aabb to fit p0p1.
    (let ((src-rect old-p0p1)
          (dst-rect p0p1))
      ;; If the width of the src-rect is 0, x of anchor points cannot be moved.
      (when (= (edraw-rect-width src-rect) 0)
        (setcar (car p0p1) (caar old-p0p1))
        (setcar (cdr p0p1) (cadr old-p0p1)))
      ;; If the height of the src-rect is 0, y of anchor points cannot be moved.
      (when (= (edraw-rect-height src-rect) 0)
        (setcdr (car p0p1) (cdar old-p0p1))
        (setcdr (cdr p0p1) (cddr old-p0p1)))

      ;;@todo Support more transform methods.
      ;;(edraw-transform-anchor-points-local
      (let ((matrix (edraw-matrix-fit-rect-to-rect src-rect dst-rect)))
        (unless (edraw-matrix-identity-p matrix)
          (edraw-transform-local shape matrix)
          ;; Changed
          ;;@todo Check if descendants were actually changed.
          t)))))

(cl-defmethod edraw-transform-auto ((shape edraw-shape-group) matrix)
  (cond
   ;; `transform' property already used.
   ((edraw-transform-prop-exists-p shape)
    (edraw-transform-prop-multiply shape matrix))
   ;; Transform child shapes. (No transform property)
   (t
    (edraw-transform-local shape matrix))))

(cl-defmethod edraw-transform-local ((group edraw-shape-group) matrix)
  ;; Transform child shapes.
  (with-slots (editor) group
    (edraw-make-undo-group editor 'group-transform
      (dolist (child (edraw-children group))
        (edraw-transform child matrix)))))

(cl-defmethod edraw-transform-anchor-points-local ((group edraw-shape-group) matrix)
  ;; Transform anchor points of child shapes.
  (with-slots (editor) group
    (edraw-make-undo-group editor 'group-transform-anchor-points
      (dolist (child (edraw-children group))
        (edraw-transform-anchor-points child matrix)))))

(cl-defmethod edraw-shape-group-add-children ((group edraw-shape-group) children)
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
        (edraw-insert group child nil)))))

(cl-defmethod edraw-ungroup ((group edraw-shape-group)
                             &optional apply-transform-p)
  (with-slots (editor) group
    (edraw-make-undo-group editor 'ungroup-group
      (let ((children (edraw-children group))
            (matrix (edraw-transform-prop-get-matrix group)))
        ;; remove children from the group
        (dolist (child children)
          (edraw-remove child))
        ;; transform
        (when (and (not (edraw-matrix-identity-p matrix))
                   apply-transform-p)
          (dolist (child children)
            (edraw-transform child matrix)))
        ;; add children to under the parent of the group
        (dolist (child children)
          (edraw-insert (edraw-parent group) child nil))
        ;; remove the group
        (edraw-remove group)))))

(cl-defmethod edraw-ungroup-interactive ((group edraw-shape-group))
  (edraw-ungroup
   group
   (unless (edraw-matrix-identity-p (edraw-transform-prop-get-matrix group))
     (y-or-n-p (edraw-msg "Apply group's transform property to children?")))))

(cl-defmethod edraw-get-actions ((_shape edraw-shape-group))
  (let* ((items (copy-tree (cl-call-next-method))))
    (append
     items
     `(((edraw-msg "Ungroup") edraw-ungroup-interactive)))))

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

  (let ((scale (if-let* ((shape (edraw-parent-shape spt))
                         (editor (oref shape editor)))
                   (float (edraw-scroll-scale editor))
                 1.0))) ;;@todo Should I add the argument SCALE?

    (pcase (edraw-get-point-type spt)
      ('anchor (<= (edraw-xy-distance-l-inf (edraw-get-xy-transformed spt) xy) ;;square
                   (/ edraw-anchor-point-input-radius scale)))
      ('handle (<= (edraw-xy-distance-squared (edraw-get-xy-transformed spt) xy) ;;circle
                   (let ((r (/ edraw-handle-point-input-radius scale)))
                     (* r r)))))))

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
(cl-defmethod edraw-move ((spt edraw-shape-point-rect-boundary) &optional xy)
  (edraw-set-anchor-position (oref spt shape) spt
                             (or xy (edraw-read-shape-point-xy spt))))
(cl-defmethod edraw-same-point-p ((spt1 edraw-shape-point-rect-boundary) spt2)
  (eq spt1 spt2))

(defun edraw-read-shape-point-xy (spt)
  (let ((xy (edraw-get-xy spt)))
    (edraw-xy
     (read-number (edraw-msg "X: ") (edraw-x xy))
     (read-number (edraw-msg "Y: ") (edraw-y xy)))))

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
(cl-defmethod edraw-move ((spt edraw-shape-point-text) &optional xy)
  (edraw-set-anchor-position (oref spt shape)
                             (or xy (edraw-read-shape-point-xy spt))))
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
  (with-slots (ppoint) spt
    (edraw-path-point-index-in-cmdlist ppoint)))

(cl-defmethod edraw-anchor-index-in-path ((spt edraw-shape-point-path))
  (with-slots (ppoint) spt
    (edraw-path-anchor-point-index-in-cmdlist ppoint)))

(cl-defmethod edraw-get-xy ((spt edraw-shape-point-path))
  (with-slots (ppoint) spt
    (edraw-xy-clone (edraw-path-point-xy ppoint))))

(cl-defmethod edraw-push-undo-path-d-change ((spt edraw-shape-point-path) type)
  "Register undo data that restores the entire d property.

It is best to avoid using this function as much as
possible. Because undoing invalidates all point objects."
  (unless edraw-editor-inhibit-make-undo-data
    (with-slots (shape) spt
      (let* ((index (edraw-index-in-path spt))
             (type (intern (format "%s-p%s" type index))))
        ;;(message "type=%s len undo=%s" type (length (edraw-undo-list (oref shape editor))))
        (edraw-push-undo-properties shape type '(d))))))

(cl-defmethod edraw-on-path-point-move ((spt edraw-shape-point-path)
                                        type
                                        old-xy
                                        &optional
                                        opposite-index-old-xy)
  (with-slots (shape) spt
    (let* ((index (edraw-index-in-path spt))
           (type (intern (format "%s-p%s" type index)))
           (editor (oref shape editor))
           ;; Check previous undo data before making undo group
           ;; [Grouping consecutive changes to the same target is now
           ;; the responsibility of a higher command, so the following
           ;; are no longer used.]
           ;; (prev-undo-data-same-target-p
           ;;  (edraw-undo-data-starts-with-args-p
           ;;   (edraw-last-undo-data editor)
           ;;   type
           ;;   (if opposite-index-old-xy
           ;;       #'edraw-move-nth-points
           ;;     #'edraw-move-nth-point)
           ;;   shape))
           (prev-undo-data-same-target-p nil))
      ;;(message "type=%s len undo=%s" type (length (edraw-undo-list (oref shape editor))))

      (edraw-make-undo-group (oref shape editor) type
        ;; Register undo data.
        ;; This function does not register the entire d property
        ;; as undo data.  Registering the d property as undo data
        ;; would cause all point objects to become invalid when
        ;; undoing, preventing them from being dragged. Undoing
        ;; while dragging is necessary for group transformations.
        (unless edraw-editor-inhibit-make-undo-data
          (unless prev-undo-data-same-target-p
            (edraw-push-undo
             editor type
             (if opposite-index-old-xy
                 ;; Always push opposite xy even if the coordinates
                 ;; haven't changed for easy implementation of merging.
                 (list #'edraw-move-nth-points shape
                       (list (cons index old-xy)
                             opposite-index-old-xy))
               (list #'edraw-move-nth-point shape index old-xy)))))

        ;; Notify point move.
        (edraw-on-shape-point-changed shape 'point-move)))))

(cl-defmethod edraw-move ((spt edraw-shape-point-path) &optional xy)
  (unless xy
    (setq xy (edraw-read-shape-point-xy spt)))
  (with-slots (ppoint shape) spt
    (let ((old-xy (edraw-xy-clone (edraw-path-point-xy ppoint))))
      (unless (edraw-xy-equal-p xy old-xy)
        (edraw-path-point-move-with-related-points ppoint xy)
        (edraw-on-path-point-move spt 'path-point-move old-xy)))))

(cl-defmethod edraw-get-opposite-handle-index-xy ((spt edraw-shape-point-path))
  (with-slots (ppoint) spt
    (when-let ((opposite-ppoint (edraw-path-handle-another-handle ppoint)))
      (cons
       (edraw-path-point-index-in-cmdlist opposite-ppoint)
       (edraw-xy-clone (edraw-path-point-xy opposite-ppoint))))))

(cl-defmethod edraw-move-with-opposite-handle ((spt edraw-shape-point-path)
                                               xy)
  (with-slots (ppoint shape) spt
    (let ((old-xy (edraw-xy-clone (edraw-path-point-xy ppoint))))
      (unless (edraw-xy-equal-p xy old-xy)
        (let ((opposite-index-old-xy (edraw-get-opposite-handle-index-xy spt)))
          (edraw-path-handle-move-with-opposite-handle ppoint xy)
          (edraw-on-path-point-move
           spt
           'path-point-move-with-opposite-handle old-xy
           opposite-index-old-xy))))))

(cl-defmethod edraw-move-with-opposite-handle-on-transformed ((spt edraw-shape-point-path) xy)
  (when-let ((shape (edraw-parent-shape spt))
             (inv-mat (edraw-transform-prop-get-inverse-matrix shape)))
    (edraw-move-with-opposite-handle
     spt (edraw-matrix-mul-mat-xy inv-mat xy))))

(cl-defmethod edraw-move-with-opposite-handle-symmetry ((spt edraw-shape-point-path) xy include-same-position-p)
  (with-slots (ppoint shape) spt
    (let ((old-xy (edraw-xy-clone (edraw-path-point-xy ppoint))))
      (unless (edraw-xy-equal-p xy old-xy)
        (let ((opposite-index-old-xy (edraw-get-opposite-handle-index-xy spt)))
          (edraw-path-handle-move-with-opposite-handle-symmetry ppoint xy include-same-position-p)
          (edraw-on-path-point-move
           spt
           'path-point-move-with-opposite-handle old-xy
           opposite-index-old-xy))))))

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
        (edraw-make-undo-group (oref shape editor)
            'path-point-create-forward-handle
          ;;@todo Avoid using edraw-push-undo-path-d-change?
          (edraw-push-undo-path-d-change spt 'path-point-create-forward-handle)
          (edraw-on-shape-point-changed shape 'handle-create)) ;;@todo check get or create?
        (edraw-shape-point-path
         :shape shape
         :ppoint handle-ppoint)))))

(cl-defmethod edraw-create-backward-handle ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (when (edraw-path-point-anchor-p ppoint)
      (when-let ((handle-ppoint (edraw-path-anchor-create-backward-handle ppoint)))
        (edraw-make-undo-group (oref shape editor)
            'path-point-create-backward-handle
          ;;@todo Avoid using edraw-push-undo-path-d-change?
          (edraw-push-undo-path-d-change spt 'path-point-create-backward-handle)
          (edraw-on-shape-point-changed shape 'handle-create)) ;;@todo check get or create?
        (edraw-shape-point-path
         :shape shape
         :ppoint handle-ppoint)))))

(cl-defmethod edraw-get-actions ((spt edraw-shape-point-path))
  (with-slots (ppoint) spt
    (let ((backward-handle (edraw-path-anchor-backward-handle ppoint))
          (forward-handle (edraw-path-anchor-forward-handle ppoint))
          (glued-p (edraw-glued-p spt)))
      (cond
       ((edraw-path-point-anchor-p ppoint)
        `(((edraw-msg "Delete Point") edraw-delete-point)
          ((edraw-msg "Split Path at Point") edraw-split-path-at)
          ((edraw-msg "Insert Point Before") edraw-insert-point-before
           :enable ,(not (null (edraw-path-point-prev-anchor ppoint))))
          ((edraw-msg "Move by Coordinates...") edraw-move)
          ((edraw-msg "Make Smooth") edraw-make-smooth)
          ((edraw-msg "Make Corner") edraw-make-corner
           :enable ,(or backward-handle forward-handle))
          ((edraw-msg "Glue to selected or overlapped shape") edraw-glue-to-selected-or-overlapped-shape
           :visible ,(not glued-p)
           :enable ,(edraw-can-be-glued-to-selected-or-overlapped-shape spt))
          ((edraw-msg "Unglue") edraw-unglue
           :visible ,glued-p)))
       ((edraw-path-point-handle-p ppoint)
        `(((edraw-msg "Delete Point") edraw-delete-point)
          ((edraw-msg "Move by Coordinates...") edraw-move)
          ))))))

(cl-defmethod edraw-delete-point ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (edraw-make-undo-group (oref shape editor) 'path-point-delete
      ;; Unglue if SPT is glued.
      (when (edraw-glued-p spt)
        (edraw-unglue spt))
      ;; Delete Point
      (when (edraw-path-point-remove ppoint)
        ;; @todo if cmdline is empty or contains Z, M only
          ;;@todo Avoid using edraw-push-undo-path-d-change?
        (edraw-push-undo-path-d-change spt 'path-point-delete)
        (edraw-on-shape-point-changed shape 'point-remove)
        t))))

(cl-defmethod edraw-insert-point-before ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (when (edraw-path-anchor-insert-midpoint-before ppoint)
      (edraw-make-undo-group (oref shape editor) 'path-point-insert-before
          ;;@todo Avoid using edraw-push-undo-path-d-change?
        (edraw-push-undo-path-d-change spt 'path-point-insert-before)
        (edraw-on-shape-point-changed shape 'anchor-insert)
        t))))

(cl-defmethod edraw-make-smooth ((spt edraw-shape-point-path))
  "Add handles to SPT anchor point."
  (with-slots (ppoint shape) spt
    (edraw-make-undo-group (oref shape editor) 'path-point-smooth
      (edraw-path-anchor-make-smooth ppoint)
      ;;@todo Avoid using edraw-push-undo-path-d-change?
      (edraw-push-undo-path-d-change spt 'path-point-smooth)
      (edraw-on-shape-point-changed shape 'anchor-make-smooth)
      t)))

(cl-defmethod edraw-make-corner ((spt edraw-shape-point-path))
  "Remove handles from SPT anchor point."
  (with-slots (ppoint shape) spt
    (let* ((fh (edraw-path-anchor-forward-handle ppoint))
           (bh (edraw-path-anchor-backward-handle ppoint))
           (f (if fh (edraw-path-point-remove fh))) ;;may destroy next cmd(C => L, -forward-handle-point => nil)
           (b (if bh (edraw-path-point-remove bh)))) ;;may destroy curr cmd(C => L)
      (when (or f b)
        (edraw-make-undo-group (oref shape editor) 'path-point-corner
          ;;@todo Avoid using edraw-push-undo-path-d-change?
          (edraw-push-undo-path-d-change spt 'path-point-corner)
          (edraw-on-shape-point-changed shape 'anchor-make-corner)
          t)))))

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

(cl-defmethod edraw-glue-destination-of-selected-or-overlapped-shape
  ((spt edraw-shape-point-path))
  "Return the shape that is the glue destination."
  (with-slots (ppoint shape) spt
    (with-slots (editor) shape
      (let* ((selected-shapes ;;without this shape
              (remq shape (edraw-selected-shapes editor)))
             ;;(remq shape (edraw-all-shapes editor)))))
             (dst-shape
              (or
               ;; the selected shape
               (and (null (cdr selected-shapes))
                    (car selected-shapes))
               ;; the most front overlapping shape
               (car (edraw-find-shapes-by-xy
                     (reverse ;; front to back
                      (or selected-shapes ;;@todo sort?
                          (remq shape (edraw-all-shapes editor))))
                     (edraw-get-xy spt))))))
        dst-shape))))

(cl-defmethod edraw-can-be-glued-to-selected-or-overlapped-shape ((spt edraw-shape-point-path))
  (with-slots (ppoint shape) spt
    (and (edraw-anchor-p spt)
         (not (edraw-glued-p spt))
         (edraw-glue-destination-of-selected-or-overlapped-shape spt))))

(cl-defmethod edraw-glue-to-selected-or-overlapped-shape ((spt edraw-shape-point-path))
  ;; Determine the destination shape
  (when (edraw-anchor-p spt)
    (with-slots (shape) spt
      (let* ((dst-shape (edraw-glue-destination-of-selected-or-overlapped-shape spt))
             (num-anchors (edraw-get-anchor-point-count shape))
             (anchor-index (edraw-anchor-index-in-path spt)))
        (when (and dst-shape anchor-index)
          ;; Reverse anchor index
          (when (>= anchor-index (/ num-anchors 2))
            (setq anchor-index (- anchor-index num-anchors)))
          ;;(message "Connect src=(%s %s) dst=%s" (edraw-name shape) anchor-index (edraw-name dst-shape))

          (edraw-make-undo-group (oref shape editor) 'glue-to-selected-or-overlapped-shape
            (let ((conn (edraw-point-connection
                         :src (edraw-point-connection-src-anchor
                               :shape shape :index anchor-index)
                         :dst (edraw-point-connection-dst-shape
                               :shape dst-shape))))
              ;; Update XY before adding CONN to SHAPE.
              (edraw-update conn)
              ;; Add CONN to SHAPE.
              (edraw-add-point-connection shape conn)
              ;; Do not update XY after adding CONN.
              ;; When undoing, CONN must be removed before undoing XY move.
              ;;(edraw-update-all-point-connections shape)
              )))))))

(cl-defmethod edraw-unglue ((spt edraw-shape-point-path))
  (when (edraw-anchor-p spt)
    (with-slots (shape) spt
      (edraw-remove-point-connection
       shape
       (edraw-point-connection-src-anchor
        :shape shape
        :index (edraw-anchor-index-in-path spt))))))

(cl-defmethod edraw-glued-p ((spt edraw-shape-point-path))
  (when (edraw-anchor-p spt)
    (with-slots (shape) spt
      (not (null (edraw-find-point-connection
                  shape
                  (edraw-point-connection-src-anchor
                   :shape shape
                   :index (edraw-anchor-index-in-path spt))))))))

;;;; Point Connection

;; Point Connection is a mechanism to glue connection source points of
;; shapes to connection destination points.

;;;;; Point Connection Source

(defclass edraw-point-connection-src ()
  ((shape :type edraw-shape :initarg :shape))
  "A class that represents a connection source point."
  :abstruct t)

(cl-defmethod edraw-equal ((_src1 edraw-point-connection-src)
                           (_src2 edraw-point-connection-src))
  "Return non-nil if SRC1 and SRC2 point to the same point."
  nil)

(cl-defmethod edraw-update ((src edraw-point-connection-src))
  "Return the coordinates where the connection SRC should be."
  (or
   ;; Compute the destination coordinates
   (with-slots (shape) src
     (when-let ((conn (edraw-find-point-connection shape src)))
       (edraw-update conn)))
   ;; Return the current source coordinates
   (edraw-get-xy src)))

;;;;;; Point Connection Source Path Anchor Point

(defclass edraw-point-connection-src-anchor (edraw-point-connection-src)
  ((index :type integer :initarg :index)))

(cl-defmethod edraw-equal ((src1 edraw-point-connection-src-anchor)
                           (src2 edraw-point-connection-src-anchor))
  "Return non-nil if SRC1 and SRC2 point to the same point."
  (and
   (eq (oref src1 shape) (oref src2 shape))
   (eq (edraw-normalized-index src1) (edraw-normalized-index src2))))

(cl-defmethod edraw-to-string ((src edraw-point-connection-src-anchor))
  ;;@todo Add option to output shape id?
  ;; A(index)
  (format "A(%s)" (oref src index)))

(cl-defmethod edraw-normalized-index ((src edraw-point-connection-src-anchor))
  (with-slots (shape index) src
    (if (>= index 0)
        index
      (+ (edraw-get-anchor-point-count shape) index))))

(cl-defmethod edraw-next-inside ((src edraw-point-connection-src-anchor))
  (with-slots (shape index) src
    (let ((next-index (if (>= index 0) (1+ index) (1- index)))
          (count (edraw-get-anchor-point-count shape)))
      (when (and (< next-index count)
                 (>= next-index (- count)))
        (edraw-point-connection-src-anchor :shape shape :index next-index)))))

(cl-defmethod edraw-get-xy ((src edraw-point-connection-src-anchor))
  "Return the current coordinates of the connection SRC."
  (with-slots (shape index) src
    (when-let ((spt (edraw-get-nth-anchor-point shape (edraw-normalized-index src))))
      (edraw-get-xy spt))))

(cl-defmethod edraw-set-xy ((src edraw-point-connection-src-anchor) xy)
  "Move SRC to XY."
  (when xy
    (with-slots (shape index) src
      (when-let ((spt (edraw-get-nth-anchor-point shape (edraw-normalized-index src))))
        ;;(message "set-xy %s %s %s" (edraw-name shape) (edraw-normalized-index src) xy)
        (edraw-move spt xy))))
  xy)

;;;;;; Point Connection Source Attribute Pair

(defclass edraw-point-connection-src-attrs (edraw-point-connection-src)
  ((attr-x :type symbol :initarg :attr-x)
   (attr-y :type symbol :initarg :attr-y)))

(cl-defmethod edraw-equal ((src1 edraw-point-connection-src-attrs)
                           (src2 edraw-point-connection-src-attrs))
  "Return non-nil if SRC1 and SRC2 point to the same point."
  (and
   (eq (oref src1 shape) (oref src2 shape))
   (eq (oref src1 attr-x) (oref src2 attr-x))
   (eq (oref src1 attr-y) (oref src2 attr-y))))

(cl-defmethod edraw-to-string ((src edraw-point-connection-src-attrs))
  ;;@todo Add option to output shape id?
  ;; ATTRS(attr-x attr-y)
  (with-slots (attr-x attr-y) src
    (format "ATTRS(%s %s)" attr-x attr-y)))

(cl-defmethod edraw-next-inside ((_src edraw-point-connection-src-attrs))
  nil)

(cl-defmethod edraw-get-xy ((src edraw-point-connection-src-attrs))
  "Return the current coordinates of the connection SRC."
  (with-slots (shape attr-x attr-y) src
    (edraw-xy
     (edraw-get-property shape attr-x)
     (edraw-get-property shape attr-y))))

(cl-defmethod edraw-set-xy ((src edraw-point-connection-src-attrs) xy)
  "Move SRC to XY."
  (when xy
    (with-slots (shape attr-x attr-y) src
      (edraw-set-properties
       shape
       (list (cons attr-x (edraw-x xy))
             (cons attr-y (edraw-y xy))))))
  xy)

;;;;;; Point Connection Source AABB

(defclass edraw-point-connection-src-aabb (edraw-point-connection-src)
  ((x-ratio :type float :initarg :x-ratio)
   (y-ratio :type float :initarg :y-ratio)))

(cl-defmethod edraw-equal ((src1 edraw-point-connection-src-aabb)
                           (src2 edraw-point-connection-src-aabb))
  "Return non-nil if SRC1 and SRC2 point to the same point."
  (and
   (eq (oref src1 shape) (oref src2 shape))
   (eq (oref src1 x-ratio) (oref src2 x-ratio))
   (eq (oref src1 y-ratio) (oref src2 y-ratio))))

(cl-defmethod edraw-to-string ((src edraw-point-connection-src-aabb))
  ;;@todo Add option to output shape id?
  ;; AABB(x-ratio y-ratio)
  (with-slots (x-ratio y-ratio) src
    (format "AABB(%s %s)" x-ratio y-ratio)))

(cl-defmethod edraw-next-inside ((_src edraw-point-connection-src-aabb))
  nil)

(cl-defmethod edraw-get-xy ((src edraw-point-connection-src-aabb))
  "Return the current coordinates of the connection SRC."
  (with-slots (shape x-ratio y-ratio) src
    (when-let ((aabb (edraw-shape-aabb shape)))
      (edraw-xy
       (+ (edraw-rect-left aabb)
          (* x-ratio (edraw-rect-width aabb)))
       (+ (edraw-rect-top aabb)
          (* y-ratio (edraw-rect-height aabb)))))))

(cl-defmethod edraw-set-xy ((src edraw-point-connection-src-aabb) xy)
  "Move SRC to XY."
  (when xy
    (when-let ((curr-xy (edraw-get-xy src)))
      (edraw-translate (oref src shape) (edraw-xy-sub xy curr-xy))))
  xy)

;;;;; Point Connection Destination

(defclass edraw-point-connection-dst ()
  ((shape :type edraw-shape :initarg :shape))
  "A class that represents where to glue the connection source point."
  :abstruct t)

(cl-defmethod edraw-shape-center ((dst edraw-point-connection-dst))
  (edraw-rect-center (edraw-shape-aabb (oref dst shape))))

;;;;;; Point Connection Destination with Specified Shape

(defclass edraw-point-connection-dst-shape (edraw-point-connection-dst)
  ())

(cl-defmethod edraw-to-string ((dst edraw-point-connection-dst-shape))
  ;; OBJ(id)
  (format "OBJ(%s)" (edraw-internal-id (oref dst shape))))

(defvar edraw-point-connection-dst-shape--updating-dst nil)

(cl-defmethod edraw-update-src ((dst edraw-point-connection-dst-shape)
                                (src edraw-point-connection-src))
  (if (memq dst edraw-point-connection-dst-shape--updating-dst)
      ;; If there is a circular reference, discontinue the calculation
      ;; and return the center point of DST.
      (edraw-shape-center dst)
    (let ((edraw-point-connection-dst-shape--updating-dst
           (cons dst edraw-point-connection-dst-shape--updating-dst)))
      ;; intersection(dst.shape.center to next-inside(src), dst.shape.edge)
      (let* ((src-next-inside (edraw-next-inside src))
             (xy-src-next-inside (when src-next-inside
                                   (edraw-update src-next-inside))))
        (or
         (when xy-src-next-inside
           (let* ((dst-shape-center (edraw-shape-center dst))
                  (dir (edraw-xy-sub xy-src-next-inside dst-shape-center))
                  (len-sq (edraw-xy-length-squared dir)))
             (when (> len-sq 1e-6)
               (let ((xy (car (last (edraw-svg-element-and-line-intersections
                                     (edraw-element (oref dst shape))
                                     dst-shape-center
                                     dir)))))
                 ;;(message "Compute dst %s to %s (dir=%s) = %s" dst-shape-center xy-src-next-inside dir xy)
                 (edraw-set-xy src xy)
                 xy))))
         (let ((xy (edraw-shape-center dst)))
           (edraw-set-xy src xy)
           xy))))))

;;;;;; Point Connection Destination with Specified Shape and Direction

(defclass edraw-point-connection-dst-shape-dir (edraw-point-connection-dst)
  ((dir :type number :initarg :dir)))

(cl-defmethod edraw-to-string ((dst edraw-point-connection-dst-shape-dir))
  ;; OBJDIR(id dir)
  (with-slots (shape dir) dst
    (format "OBJDIR(%s %s)" (edraw-internal-id shape) dir)))

(cl-defmethod edraw-update-src ((dst edraw-point-connection-dst-shape-dir)
                                (src edraw-point-connection-src))
  ;; intersection(dst.shape.center for dir, dst.shape.edge)
  (let* ((dst-shape-center (edraw-shape-center dst))
         (angle (degrees-to-radians (oref dst dir)))
         (dir (edraw-xy (cos angle) (sin angle)))
         (xy (car (last (edraw-svg-element-and-line-intersections
                         (edraw-element (oref dst shape))
                         dst-shape-center
                         dir)))))
    (edraw-set-xy src xy)
    xy))


;;;;; Point Connection Class

(defclass edraw-point-connection ()
  ((src :type edraw-point-connection-src :initarg :src :reader edraw-src)
   (dst :type edraw-point-connection-dst :initarg :dst :reader edraw-dst))
  "A class that represents which point to glue to where.")

(cl-defmethod edraw-to-string ((conn edraw-point-connection))
  ;; Aindex:OBJ(id)
  (with-slots (src dst) conn
    (concat (edraw-to-string src) ":" (edraw-to-string dst))))

(cl-defmethod edraw-update ((conn edraw-point-connection))
  (with-slots (src dst) conn
    (edraw-update-src dst src))) ;; Return xy

(cl-defmethod edraw-detach ((conn edraw-point-connection))
  (let* ((src (edraw-src conn))
         (shape (oref src shape)))
    (edraw-remove-point-connection shape src)))

(cl-defmethod edraw-detach-dst ((conn edraw-point-connection))
  (edraw-remove-point-connection-referrer (oref (edraw-dst conn) shape) conn))

;;;;; Point Connection Update

(defvar edraw-shape-updated-point-connections nil)

(defun edraw-point-connection--update-list (connections)
  ;; Prevents endless loops with circular updates.
  (if edraw-shape-updated-point-connections
      (edraw-point-connection--update-list-internal connections)
    (let ((edraw-shape-updated-point-connections (list nil)))
      (edraw-point-connection--update-list-internal connections))))

(defun edraw-point-connection--update-list-internal (connections)
  (dolist (conn connections)
    (unless (memq conn (cdr edraw-shape-updated-point-connections))
      (push conn (cdr edraw-shape-updated-point-connections))
      (edraw-update conn))))

;;;;; Shape

;;;;;; Add/Remove/Find Point Connection

(cl-defmethod edraw-get-point-connections ((shape edraw-shape))
  (oref shape point-connections))

(cl-defmethod edraw-set-point-connections ((shape edraw-shape) conn)
  (oset shape point-connections conn))

(cl-defmethod edraw-get-point-connection-referrers ((shape edraw-shape))
  (oref shape point-connection-referrers))

(cl-defmethod edraw-set-point-connection-referrers ((shape edraw-shape) value)
  (oset shape point-connection-referrers value))

(cl-defmethod edraw-find-point-connection ((shape edraw-shape)
                                           (src edraw-point-connection-src))
  (cl-loop for conn in (edraw-get-point-connections shape)
           when (edraw-equal (edraw-src conn) src)
           return conn))

(cl-defmethod edraw-add-point-connection ((shape edraw-shape)
                                          (conn edraw-point-connection))
  (unless (edraw-find-point-connection shape (edraw-src conn)) ;;Already exists?
    (edraw-set-point-connections
     shape
     (cons conn (edraw-get-point-connections shape)))

    ;; Add referrer
    (when-let ((dst-shape (oref (edraw-dst conn) shape)))
      (edraw-set-point-connection-referrers
       dst-shape
       (cons conn (edraw-get-point-connection-referrers dst-shape))))

    ;; Update data attribute
    (edraw-update-point-connections-attribute shape)

    ;; Add undo data
    (with-slots (editor) shape
      (edraw-push-undo editor 'point-connection-add
                       (list 'edraw-remove-point-connection
                             shape
                             conn)))))

(cl-defmethod edraw-add-point-connection ((shape edraw-shape)
                                          (connections list))
  (dolist (conn connections)
    (edraw-add-point-connection shape conn)))

(cl-defmethod edraw-remove-point-connection ((shape edraw-shape)
                                             (conn edraw-point-connection))
  (edraw-remove-point-connection shape (edraw-src conn)))

(cl-defmethod edraw-remove-point-connection ((shape edraw-shape)
                                             (src edraw-point-connection-src))
  (let* ((lst (cons nil (edraw-get-point-connections shape)))
         (p lst))
    ;; Find connection
    (while (and (cdr p)
                (not (edraw-equal (edraw-src (cadr p)) src)))
      (setq p (cdr p)))

    ;; Found
    (when (cdr p)
      (let ((conn (cadr p)))
        ;; Delete
        (setcdr p (cddr p))
        (edraw-set-point-connections shape (cdr lst))

        ;; Remove referrer
        (edraw-detach-dst conn)

        ;; Update data attribute
        (edraw-update-point-connections-attribute shape)

        ;; Add undo data
        (with-slots (editor) shape
          (edraw-push-undo editor 'point-connection-remove
                           (list 'edraw-add-point-connection
                                 shape
                                 conn)))))))

(cl-defmethod edraw-remove-point-connection-referrer ((shape edraw-shape)
                                                      (conn edraw-point-connection))
  (edraw-set-point-connection-referrers
   shape
   (delq conn (edraw-get-point-connection-referrers shape))))


(cl-defmethod edraw-remove-all-point-connections ((shape edraw-shape))
  (let ((connections (edraw-get-point-connections shape)))
    (when connections
      ;; Add undo data
      (with-slots (editor) shape
        (edraw-push-undo editor 'point-connection-remove
                         (list 'edraw-add-point-connection
                               shape
                               (seq-copy connections))))

      ;; Remove connection referrers
      (mapc #'edraw-detach-dst connections)
      ;; Remove connections
      (edraw-set-point-connections shape nil)

      ;; Update data attribute
      (edraw-update-point-connections-attribute shape))))

(cl-defmethod edraw-remove-all-point-connection-referrers ((shape edraw-shape))
  ;; called when SHAPE is removed
  (while (edraw-get-point-connection-referrers shape)
    (let ((conn (car (edraw-get-point-connection-referrers shape))))
      (edraw-detach conn))))

(cl-defmethod edraw-update-point-connections-attribute ((shape edraw-shape))
  (dom-set-attribute
   (edraw-element shape)
   'data-edraw-point-connections
   (mapconcat #'edraw-to-string
              (edraw-get-point-connections shape)
              ",")))

;;;;;; Update Point Connections

(cl-defmethod edraw-update-related-point-connections ((shape edraw-shape)
                                                      change-type)
  (let ((related-connections
         (append
          (edraw-get-point-connections shape)
          (edraw-get-point-connection-referrers shape))))
    (when related-connections
      (if (eq change-type 'shape-remove)
          ;; Removed
          ;;@todo remove point-connections?
          (edraw-remove-all-point-connection-referrers shape)
        ;; Changed
        (edraw-point-connection--update-list related-connections)))))

(cl-defmethod edraw-update-all-point-connections ((shape edraw-shape))
  (edraw-point-connection--update-list (edraw-get-point-connections shape)))

;;;;;; Restore Point Connections

(cl-defmethod edraw-restore-point-connections ((shape edraw-shape))
  (when-let* ((attr-str (dom-attr (edraw-element shape) 'data-edraw-point-connections))
              (connections
               (condition-case err
                   (edraw-point-connection-parse-attribute attr-str shape
                                                           (oref shape editor))
                 (error
                  (message "Failed to restore connection (shape=%s): %s"
                           (edraw-name shape)
                           err)
                  nil))))
    (edraw-remove-all-point-connections shape)
    (edraw-add-point-connection shape connections)))

(defun edraw-point-connection-parse-attribute (str src-shape editor)
  (let ((str-len (length str))
        (input (cons str 0))
        connections)
    (cl-symbol-macrolet ((str (car input)) (pos (cdr input)))
      ;; <connection> , <connection> ...
      (while (progn
               (edraw-point-connection-skip-ws input)
               (< pos str-len))
        (when connections
          (edraw-point-connection-skip-single-char input ?,))
        (push (edraw-point-connection-parse-connection input src-shape editor)
              connections)))
    (nreverse connections)))

(defun edraw-point-connection-parse-connection (input src-shape editor)
  (cl-symbol-macrolet ((str (car input)) (pos (cdr input)))
    ;; <src> : <dst>
    (let ((src (edraw-point-connection-parse-src input src-shape))
          (_ (edraw-point-connection-skip-ws input))
          (_ (edraw-point-connection-skip-single-char input ?:))
          (dst (edraw-point-connection-parse-dst input editor)))
      (edraw-point-connection :src src :dst dst))))

(defun edraw-point-connection-skip-ws (input)
  (cl-symbol-macrolet ((str (car input)) (pos (cdr input)))
    (let ((str-len (length str))
          (i pos))
      (while (and (< i str-len) (= (elt str i) ? ))
        (cl-incf i))
      (setf pos i))))

(defun edraw-point-connection-skip-single-char (input expected-char)
  (cl-symbol-macrolet ((str (car input)) (pos (cdr input)))
    (unless (= (elt str pos) expected-char)
      (error "Unexpected %s (Expected %s) at %s" (elt str pos) expected-char input))
    (cl-incf pos)))

(defun edraw-point-connection-parse-function (input)
  (cl-symbol-macrolet ((str (car input)) (pos (cdr input)))
    ;; <name> ( <args> )
    (unless (equal (string-match " *\\([A-Z0-9_]+\\) *( *\\([^)]*\\) *)" str pos) pos)
      (error "Invalid point connection function %s" input))
    (setf pos (match-end 0))
    (let ((name (match-string 1 str))
          (args (match-string 2 str)))
      (cons name (split-string args " ")))))

(defconst edraw-point-connection-src-types
  '(("A" . edraw-point-connection-parse-src-anchor)
    ("ANCHOR" . edraw-point-connection-parse-src-anchor)
    ("ATTRS" . edraw-point-connection-parse-src-attrs)
    ("AABB" . edraw-point-connection-parse-src-aabb)))

(defun edraw-point-connection-parse-src (input src-shape)
  (let* ((fun (edraw-point-connection-parse-function input))
         (name (car fun))
         (args (cdr fun)))
    (if-let ((constructor (alist-get name edraw-point-connection-src-types nil nil #'string=)))
        (funcall constructor src-shape args)
      (error "Invalid point connection src name %s" name))))

(defun edraw-point-connection-parse-src-anchor (src-shape args)
  (edraw-point-connection-src-anchor
   :shape src-shape
   :index (string-to-number (nth 0 args))))

(defun edraw-point-connection-parse-src-attrs (src-shape args)
  (edraw-point-connection-src-attrs
   :shape src-shape
   :attr-x (intern (nth 0 args)) :attr-y (intern (nth 1 args))))

(defun edraw-point-connection-parse-src-aabb (src-shape args)
  (edraw-point-connection-src-aabb
   :shape src-shape
   :x-ratio (float (string-to-number (nth 0 args)))
   :y-ratio (float (string-to-number (nth 1 args)))))

(defconst edraw-point-connection-dst-types
  '(("OBJ" . edraw-point-connection-parse-dst-shape)
    ("OBJDIR" . edraw-point-connection-parse-dst-shape-dir)))

(defun edraw-point-connection-parse-dst (input editor)
  (let* ((fun (edraw-point-connection-parse-function input))
         (name (car fun))
         (args (cdr fun))
         (id (nth 0 args))
         (dst-shape (edraw-find-shape-by-internal-id editor id)))

    (unless dst-shape
      (error "Cannot find shape %s" id))
    (if-let ((constructor (alist-get name edraw-point-connection-dst-types nil nil #'string=)))
        (funcall constructor dst-shape args)
      (error "Invalid point connection dst name %s" name))))

(defun edraw-point-connection-parse-dst-shape (dst-shape _args)
  (edraw-point-connection-dst-shape :shape dst-shape))

(defun edraw-point-connection-parse-dst-shape-dir (dst-shape args)
  (edraw-point-connection-dst-shape-dir
   :shape dst-shape
   :dir (string-to-number (nth 1 args))))


;;;; Utility

(defun edraw-buffer-kill-query ()
  "Query user to discard all editor changes.

This function is for registering with the `kill-buffer-query-functions' hook."
  (save-excursion
    (save-restriction
      (widen)
      (seq-every-p
       (lambda (ov)
         (if ov
             (let ((editor (overlay-get ov 'edraw-editor)))
               (or (null editor)
                   (not (edraw-modified-p editor))
                   (progn
                     (goto-char (overlay-start ov))
                     (y-or-n-p
                      (edraw-msg "Edraw editor has unsaved changes. Discard changes ?")))))
           t))
       (overlays-in (point-min) (point-max))))))

;;;; Simple Editor Function

(defun edraw-edit-svg (source _source-type &optional ov-beg ov-end on-finish writer)
  ;; @todo Convert SOURCE to SVG tree by SOURCE-TYPE
  ;; SOURCE-TYPE : SOURCE => SVG
  ;; edraw-svg : (svg .... (g id="edraw-body" ...)) => no conv
  ;; general-svg : (svg <children>) => (svg ... (g id="edraw-body" <children>))
  ;; element : (rect ...) => (svg ... (g id="edraw-body"  (rect ...)))
  ;; element-list : ((rect ...) (circle ...) ...) => (svg ... (g id="edraw-body"  (rect ...) (circle ...) ...))
  ;;
  ;; Convert SVG tree back to SOURCE on finish

  (let* ((original-buffer (current-buffer))
         (new-buffer
          (unless (and ov-beg ov-end)
            (prog1
                ;; [Change current buffer!!]
                (pop-to-buffer (generate-new-buffer "*Easy Draw Shape Editor*"))
              (with-silent-modifications
                (insert " "))
              (goto-char (point-min))
              (setq ov-beg (point-min)
                    ov-end (point-max))
              (setq-local buffer-read-only t)))))

    ;; Create editor
    (let* ((svg source) ;;@todo convert
           (editor-overlay (make-overlay ov-beg ov-end nil t nil))
           (editor (edraw-editor
                    :overlay editor-overlay
                    :svg svg
                    :document-writer writer
                    :menu-filter #'edraw-edit-svg--menu-filter
                    )))
      (edraw-initialize editor)

      ;; Add key bindings
      (overlay-put editor-overlay 'keymap
                   (edraw-edit-svg--make-keymap
                    (or (overlay-get editor-overlay 'keymap)
                        edraw-editor-map)))

      ;; Set info to the editor object
      (edraw-set-extra-prop editor 'edraw-edit-svg--original-buffer
                            original-buffer)
      (edraw-set-extra-prop editor 'edraw-edit-svg--new-buffer
                            new-buffer)

      ;; Add Finish Hook
      (when on-finish
        (edraw-define-hook-type editor 'edraw-edit-svg--finish)
        (edraw-add-hook editor 'edraw-edit-svg--finish
                        (lambda (&rest args)
                          (if (buffer-live-p original-buffer)
                              (with-current-buffer original-buffer
                                (apply on-finish args))
                            (message (edraw-msg "The buffer has been killed"))))))

      ;; Hook kill buffer
      (add-hook 'kill-buffer-query-functions 'edraw-buffer-kill-query nil t)

      (message "%s"
               (substitute-command-keys
                (format
                 "\\[%s]:%s, \\[%s]:%s"
                 'edraw-edit-svg--finish-edit
                 (edraw-msg "Finish Edit")
                 'edraw-edit-svg--cancel-edit
                 (edraw-msg "Cancel Edit")
                 )))
      editor)))

(defun edraw-edit-svg--menu-filter (menu-type items)
  (pcase menu-type
    ('main-menu
     (append
      items
      '(((edraw-msg "Finish Edit") edraw-edit-svg--finish-edit)
        ((edraw-msg "Cancel Edit") edraw-edit-svg--cancel-edit))))
    (_ items)))

(defun edraw-edit-svg--make-keymap (original-keymap)
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km original-keymap)
    (define-key km (kbd "C-c C-c") 'edraw-edit-svg--finish-edit)
    (define-key km (kbd "C-c C-k") 'edraw-edit-svg--cancel-edit)
    km))

(defun edraw-edit-svg--finish-edit (&optional editor)
  (interactive)
  (let ((editor (or editor (edraw-current-editor))))
    (when (or (not (and (oref editor document-writer)
                        (edraw-modified-p editor)))
              (condition-case err
                  (edraw-save editor)
                (error
                 (yes-or-no-p
                  (format
                   (edraw-msg "Failed to save. %s. Discard changes?")
                   (error-message-string err))))))
      (edraw-edit-svg--close-editor editor)
      (edraw-call-hook editor 'edraw-edit-svg--finish
                       t
                       ;;@todo Convert by SOURCE-TYPE spec
                       (edraw-document-svg editor)))))

(defun edraw-edit-svg--cancel-edit (&optional editor)
  (interactive)
  (let ((editor (or editor (edraw-current-editor))))
    (when (or (null (edraw-modified-p editor))
              (yes-or-no-p (edraw-msg "Discard changes?")))
      (edraw-edit-svg--close-editor editor)
      (edraw-call-hook editor 'edraw-edit-svg--finish
                       nil nil))))

(defun edraw-edit-svg--close-editor (editor)
  (edraw-close editor)
  (when-let ((buffer (edraw-get-extra-prop editor 'edraw-edit-svg--new-buffer)))
    (when (and buffer
               (eq (current-buffer) buffer))
      (quit-window t))))



(provide 'edraw)
;;; edraw.el ends here
