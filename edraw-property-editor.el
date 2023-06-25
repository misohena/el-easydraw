;;; edraw-property-editor.el --- Property Editor    -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

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

(require 'svg)
(require 'widget)
(require 'wid-edit)
(require 'edraw-util)
(require 'edraw-math)
(require 'edraw-color-picker)
(require 'edraw-editor-util)
(require 'edraw-dom-svg)

(declare-function edraw-node-position "edraw")
(declare-function edraw-node-siblings-count "edraw")

;;;; Property Editor Target

;;;;; Property Editor Target - Base

(defclass edraw-properties-holder ()
  ()
  :abstract t)

;;;;; Property Editor Target - Interface

(defclass edraw-property-editor-target (edraw-properties-holder)
 ())

(cl-defmethod edraw-name
  ((_target edraw-property-editor-target)))
(cl-defmethod edraw-undo-block-begin
  ((_target edraw-property-editor-target)))
(cl-defmethod edraw-undo-block-end
  ((_target edraw-property-editor-target) _backup))
(cl-defmethod edraw-undo-all
  ((_target edraw-property-editor-target)))
(cl-defmethod edraw-last-undo-data
  ((_target edraw-property-editor-target)))
(cl-defmethod edraw-undo
  ((_target edraw-property-editor-target)))
(cl-defmethod edraw-get-property-info-list
  ((_target edraw-property-editor-target)))
(cl-defmethod edraw-get-property
  ((_target edraw-property-editor-target) _prop-name))
(cl-defmethod edraw-set-property
  ((_target edraw-property-editor-target) _prop-name _value))
(cl-defmethod edraw-set-properties
  ((_target edraw-property-editor-target) _prop-list))
(cl-defmethod edraw-select
  ((_target edraw-property-editor-target)))
(cl-defmethod edraw-set-all-properties-as-default
  ((_target edraw-property-editor-target)))
(cl-defmethod edraw-add-change-hook
  ((_target edraw-property-editor-target) _function &rest _args))
(cl-defmethod edraw-remove-change-hook
  ((_target edraw-property-editor-target) _function &rest _args))
(cl-defgeneric edraw-property-editor-shape-p
  (_target)
  nil)

;;;;; Property Editor Target - Alist

(defclass edraw-alist-properties-holder (edraw-properties-holder)
  ((prop-info-list :initarg :prop-info-list)
   (alist-head :initarg :alist-head) ;;(??? (prop . value) ...)
   (editor :initarg :editor)
   (name :initarg :name)
   (change-hook :initform (edraw-hook-make))))

(cl-defmethod edraw-set-alist-head ((holder edraw-alist-properties-holder)
                                    alist-head)
  (oset holder alist-head alist-head))

(cl-defmethod edraw-set-prop-info-list ((holder edraw-alist-properties-holder)
                                        prop-info-list)
  (oset holder prop-info-list prop-info-list))

(cl-defmethod edraw-get-editor ((holder edraw-alist-properties-holder))
  (oref holder editor))

(cl-defmethod edraw-name ((holder edraw-alist-properties-holder))
  (oref holder name))

(cl-defmethod edraw-undo-block-begin ((_holder edraw-alist-properties-holder)))
(cl-defmethod edraw-undo-block-end ((_holder edraw-alist-properties-holder) _backup))
(cl-defmethod edraw-undo-all ((_holder edraw-alist-properties-holder)))
(cl-defmethod edraw-last-undo-data ((_holder edraw-alist-properties-holder)))
(cl-defmethod edraw-undo ((_holder edraw-alist-properties-holder)))

(cl-defmethod edraw-get-property-info-list ((holder edraw-alist-properties-holder))
  (oref holder prop-info-list))

(cl-defmethod edraw-get-property ((holder edraw-alist-properties-holder) prop-name)
  (alist-get prop-name (cdr (oref holder alist-head))))

(cl-defmethod edraw-set-properties ((holder edraw-alist-properties-holder) prop-list)
  (let (changed)
    (dolist (prop prop-list)
      (let* ((prop-name (car prop))
             (new-value (cdr prop))
             (prev (let ((prev (oref holder alist-head)))
                     (while (and (cdr prev)
                                 (not (eq (caadr prev) prop-name)))
                       (setq prev (cdr prev)))
                     prev))
             (old-value (cdadr prev)))
        (unless (equal new-value old-value)
          (cond
           ;; Remove
           ((null new-value)
            (setcdr prev (cddr prev)))
           ;; Add
           ((null (cdr prev))
            (setcdr prev (cons (cons prop-name new-value) nil)))
           ;; Modify
           (t
            (setcdr (cadr prev) new-value)))
          (setq changed t))))
    (when changed
      (edraw-hook-call (oref holder change-hook) holder 'alist-properties))
    changed))

(cl-defmethod edraw-set-property ((holder edraw-alist-properties-holder) prop-name value) ;;@todo generalize
  (edraw-set-properties
   holder
   (list (cons prop-name value))))

(cl-defmethod edraw-add-change-hook ((holder edraw-alist-properties-holder) function &rest args)
  (apply 'edraw-hook-add (oref holder change-hook) function args))

(cl-defmethod edraw-remove-change-hook ((holder edraw-alist-properties-holder) function &rest args)
  (apply 'edraw-hook-remove (oref holder change-hook) function args))


;;;; Property Editor Variables


(defgroup edraw-faces nil
  "Faces in Edraw."
  :tag "Edraw Faces"
  :group 'edraw)

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
  "Edit object properties in list format."
  :tag "Edraw Property Editor"
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
    (define-key km [down-mouse-2] 'ignore)
    (define-key km [mouse-2] 'edraw-property-editor--close)
    (define-key km [mouse-3] 'edraw-property-editor--menu)
    (define-key km [C-wheel-down] 'edraw-property-editor-field-wheel-decrease)
    (define-key km [C-wheel-up] 'edraw-property-editor-field-wheel-increase)
    km))

(defvar edraw-property-editor-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km (kbd "C-c C-c") 'edraw-property-editor--apply)
    (define-key km (kbd "C-c C-k") 'edraw-property-editor--close)
    (define-key km [drag-mouse-1] 'ignore)
    (define-key km [double-mouse-1] 'ignore)
    (define-key km [double-down-mouse-1] 'ignore)
    (define-key km [triple-mouse-1] 'ignore)
    (define-key km [down-mouse-2] 'ignore)
    (define-key km [mouse-2] 'edraw-property-editor--close)
    (define-key km [mouse-3] 'edraw-property-editor--menu)
    km))


;;;; Major Mode

(define-derived-mode edraw-property-editor-mode nil "Eprops")

;;;; Property Editor


(defclass edraw-property-editor ()
  ((buffer :initarg :buffer)
   (display :initarg :display)
   (target :initform nil)
   (widgets)
   (update-timer :initform nil)
   (last-edit-undo-data :initform nil)
   (last-edit-prop-name :initform nil)
   (options :initform nil)))

(defvar-local edraw-property-editor--pedit nil)

(defun edraw-property-editor-buffer ()
  (get-buffer edraw-property-editor-buffer-name))

(defun edraw-property-editor-close ()
  (when-let ((buffer (edraw-property-editor-buffer)))
    (with-current-buffer buffer
      (when edraw-property-editor--pedit
        (edraw-close edraw-property-editor--pedit)))))

(defun edraw-property-editor-open (target &optional options)
  (let* ((buffer (get-buffer-create edraw-property-editor-buffer-name))
         ;; Get property editor object
         (pedit (or (with-current-buffer buffer edraw-property-editor--pedit)
                    (edraw-property-editor-create-object buffer))))
    ;; Update options
    (oset pedit options options)

    (unless (eq target (oref pedit target))
      (with-current-buffer buffer
        ;; Release current target
        (edraw-unobserve-target pedit)

        ;; Activating major mode does following:
        ;; - Call (kill-all-local-variables)
        ;; - Call (use-local-map edraw-property-editor-mode-map)
        ;;@todo need every time?
        (edraw-property-editor-mode)

        (setq-local edraw-property-editor--pedit pedit)
        (edraw-observe-target pedit target)))

    ;; Open window or frame
    (edraw-display-buffer pedit)))

(defun edraw-property-editor-create-object (buffer)
  (edraw-property-editor
   :buffer buffer
   :display (edraw-buffer-display
             :buffer buffer
             :frame-parameters-default
             (append
              '((title . "Edraw Property Editor"))
              edraw-buffer-display-frame-parameters-default)
             :frame-parameters-last
             (edraw-ui-state-get 'property-editor 'frame-parameters-last)
             :frame-mode
             (edraw-ui-state-get 'property-editor 'frame-mode)
             :frame-mode-line-p
             (edraw-ui-state-get 'property-editor 'frame-mode-line-p)
             :frame-child-p
             (edraw-ui-state-get 'property-editor 'frame-child-p)
             :save-function
             (lambda (_obj key value)
               (edraw-ui-state-set 'property-editor key value)
               (edraw-ui-state-save)))))

(defun edraw-property-editor-target-shape-p (target)
  (and target
       ;;(cl-typep target 'edraw-shape) ;;warning
       (edraw-property-editor-shape-p target)))

(cl-defmethod edraw-observe-target ((pedit edraw-property-editor) new-target)
  (with-slots (target) pedit
    (edraw-unobserve-target pedit)

    (setq target new-target)
    (edraw-set-last-edit pedit nil nil)
    (edraw-update-buffer pedit)
    (edraw-initialize-hooks pedit)))

(cl-defmethod edraw-update-buffer ((pedit edraw-property-editor))
  (with-slots (target widgets) pedit
    (setq widgets nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    (setq-local widget-push-button-prefix "")
    (setq-local widget-push-button-suffix "")
    (setq-local widget-link-prefix "")
    (setq-local widget-link-suffix "")
    (setq-local widget-button-face 'edraw-widget-button)
    (setq-local widget-button-pressed-face 'edraw-widget-button-pressed)
    (setq-local widget-mouse-face 'edraw-widget-button-mouse)

    ;; Title
    (if target
        (widget-insert (format (edraw-msg "Properties of %s")
                               (or (edraw-name target) ""))
                       "\n")
      (widget-insert (edraw-msg "No target object") "\n\n"))

    ;; Prev / Next
    (when (edraw-property-editor-target-shape-p target)
      ;;(widget-insert (make-string 2 ? ))
      (let* ((shape-index (edraw-property-editor--shape-index))
             (num-shapes (edraw-property-editor--num-shapes))
             (num-shapes-digits (if (< num-shapes 10)
                                    1
                                  (1+ (floor (log num-shapes 10)))))
             prev next)
        (setq prev
              (widget-create 'push-button
                             :notify 'edraw-property-editor--prev
                             :keymap edraw-property-editor-push-button-map
                             (edraw-msg "Prev")))
        (when (<= shape-index 0)
          (widget-apply prev :deactivate))
        (widget-insert " "
                       (format (format "%%%dd/%%%dd"
                                       num-shapes-digits
                                       num-shapes-digits)
                               (1+ shape-index) num-shapes)
                       " ")
        (setq next
              (widget-create 'push-button
                             :notify 'edraw-property-editor--next
                             :keymap edraw-property-editor-push-button-map
                             (edraw-msg "Next")))
        (when (>= shape-index (1- num-shapes))
          (widget-apply next :deactivate)))
      (widget-insert "\n"))

    ;; Properties
    (when target
      (setq widgets (edraw-insert-property-widgets pedit target 0)))

    ;; Bottom
    (widget-insert (make-string 2 ? ))
    (when target
      (when (edraw-property-editor-target-shape-p target)
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
    (widget-insert " ")
    (widget-create 'push-button :notify 'edraw-property-editor--menu
                   (edraw-msg "Menu"))
    (widget-insert "\n")

    (widget-insert "\n")

    (widget-setup)

    (widget-forward 1) ;;to first field

    ;; (message
    ;;  (format (substitute-command-keys
    ;;           "\\[edraw-property-editor--apply]: %s  \\[edraw-property-editor--close]: %s")
    ;;          (edraw-msg "Apply")
    ;;          (edraw-msg "Close")))
    ))

(cl-defmethod edraw-insert-property-widgets ((pedit edraw-property-editor)
                                             target
                                             margin-left)
  (let* ((prop-info-list (edraw-get-property-info-list target))
         (max-name-width (when prop-info-list
                           (apply #'max
                                  (mapcar
                                   (lambda (prop-info)
                                     (string-width
                                      (symbol-name
                                       (plist-get prop-info :name))))
                                   prop-info-list))))
         widgets)
    (dolist (prop-info prop-info-list)
      (unless (plist-get prop-info :internal)
        (let* ((indent (- max-name-width
                          (string-width
                           (symbol-name (plist-get prop-info :name))))))
          (push (edraw-create-prop-widget pedit target prop-info margin-left indent)
                widgets))))
    ;; Return widgets
    (nreverse widgets)))

;;;;; Window/Frame

(cl-defmethod edraw-display-buffer ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-display-buffer display)))


;;;;; Prop Widget

(cl-defmethod edraw-create-prop-widget ((pedit edraw-property-editor)
                                        target prop-info margin-left indent)
  (let* ((notify (edraw-create-prop-widget-updator
                  pedit target prop-info)))
    (edraw-property-editor-prop-widget-create-widget
     target prop-info margin-left indent notify pedit)))


(defclass edraw-property-editor-prop-widget ()
  ((widget :initarg :widget)
   (target :initarg :target)
   (prop-info :initarg :prop-info)))

(cl-defmethod edraw-get-target ((pw edraw-property-editor-prop-widget))
  (oref pw target))

(cl-defmethod edraw-widget-delete ((pw edraw-property-editor-prop-widget))
  (widget-delete (oref pw widget)))

;;;;;; Prop Widget - Update Property From Widget

(defvar edraw-property-editor-prop-widget--notification-suppressed nil)

(defun edraw-property-editor-prop-widget-value-set-without-notify
    (widget new-w-value)
  "Same as widget-value-set, but suppresses widget change notifications."
  (let ((edraw-property-editor-prop-widget--notification-suppressed t))
    (widget-value-set widget new-w-value)))

(defun edraw-property-editor-prop-widget-value-set (widget new-w-value)
  "Same as widget-value-set, but suppresses property update
once. widget-value-set updates the same property four times."
  ;; Change widget text without notification
  (edraw-property-editor-prop-widget-value-set-without-notify
   widget new-w-value)
  ;; Notify only once
  (widget-apply widget :notify widget nil));;event=nil

(cl-defmethod edraw-last-edit-undo-data ((pedit edraw-property-editor))
  (oref pedit last-edit-undo-data))

(cl-defmethod edraw-last-edit-prop-name ((pedit edraw-property-editor))
  (oref pedit last-edit-prop-name))

(cl-defmethod edraw-set-last-edit ((pedit edraw-property-editor)
                                   undo-data prop-name)
  (oset pedit last-edit-undo-data undo-data)
  (oset pedit last-edit-prop-name prop-name))

(cl-defmethod edraw-create-prop-widget-updator ((pedit edraw-property-editor)
                                                target
                                                prop-info)
  (let ((prop-name (plist-get prop-info :name)))
    (lambda (widget _changed-widget &optional event)
      ;;(message "on widget changed %s event=%s" (plist-get prop-info :name) event)
      ;; Called 4 times per widget-value-set call.
      ;; 1. delete chars (event=(before-change BEG END))
      ;; 2. delete chars (event=(after-change BEG END))
      ;; 3. insert chars (event=(before-change BEG END))
      ;; 4. insert chars (event=(after-change BEG END))
      (when (and
             (not (eq (car-safe event) 'before-change)) ;;Ignore before-change
             (not edraw-property-editor-prop-widget--notification-suppressed))
        (if (eq target (oref pedit target))
            ;; For top-level target
            (when edraw-property-editor-apply-immediately
              (progn
                ;;(message "Set property toplevel value=%s" (widget-value widget))
                (edraw-set-target-property-value
                 pedit prop-name
                 (edraw-property-editor-widget-value-to-prop-value
                  (widget-value widget) prop-info))))
          ;; For sub-level targets
          ;;(message "Set property sublevel value=%s" (widget-value widget))

          ;; NOTE: For sub-property, even if
          ;; edraw-property-editor-apply-immediately is nil, it will
          ;; be reflected immediately.
          ;; Because the value must already be reflected in the widget
          ;; when the Apply button is pressed.
          (edraw-set-property
           target prop-name
           (edraw-property-editor-widget-value-to-prop-value
            (widget-value widget) prop-info)))))))

(cl-defmethod edraw-set-target-property-value ((pedit edraw-property-editor)
                                               prop-name prop-value)
  (when-let ((target (oref pedit target)))
    (let ((undo-before-change (edraw-last-undo-data target)))
      ;; Consecutive change to the same target and same property?
      (when (and (eq undo-before-change (edraw-last-edit-undo-data pedit))
                 (eq prop-name (edraw-last-edit-prop-name pedit)))
        (edraw-undo target)
        (setq undo-before-change (edraw-last-undo-data target)))
      ;; Change property
      ;;(message "Set property %s %s" prop-name prop-value)
      (edraw-set-property
       target
       prop-name
       prop-value)
      ;; Record last change
      (let ((undo-after-change (edraw-last-undo-data target)))
        (if (eq undo-before-change undo-after-change)
            ;; No undo data generated (Probably same values)
            (edraw-set-last-edit pedit nil nil)
          ;; New undo data generated
          (edraw-set-last-edit pedit undo-after-change prop-name))))))

;;;;;; Prop Widget - Create Widget

(defun edraw-property-editor-prop-widget-create-widget (target
                                                        prop-info
                                                        margin-left
                                                        indent
                                                        notify
                                                        pedit)
  (let* ((prop-name (plist-get prop-info :name))
         (prop-value (edraw-get-property target prop-name))
         (prop-type (plist-get prop-info :type))
         (prop-number-p (plist-get prop-info :number-p)))
    (cond
     (prop-number-p
      (edraw-property-editor-create-number-widget
       (+ margin-left indent) target prop-name prop-value prop-info notify))
     ((eq (car-safe prop-type) 'or)
      (edraw-property-editor-create-menu-choice-widget
       (+ margin-left indent) target prop-name prop-value prop-info notify))
     ((eq prop-type 'paint)
      (edraw-property-editor-create-paint-widget
       (+ margin-left indent) target prop-name prop-value prop-info notify
       (oref pedit options)))
     ((eq prop-type 'marker)
      (edraw-property-editor-create-marker-widget
       margin-left indent target prop-name prop-value prop-info notify
       pedit))
     (t
      (edraw-property-editor-create-text-field-widget
       (+ margin-left indent) target prop-name prop-value prop-info notify)))))

;;;;;; Menu Choice Widget

(defun edraw-property-editor-create-menu-choice-widget (indent
                                                        target
                                                        prop-name prop-value
                                                        prop-info notify)
  (widget-insert (make-string indent ? ))
  (let* ((prop-required (plist-get prop-info :required))
         (prop-type (plist-get prop-info :type))
         (types (if prop-required
                    (cdr prop-type) ;;skip (or)
                  ;; nullable
                  (cons nil (cdr prop-type))))
         (widget
          (apply
           #'widget-create
           `(menu-choice
             :format ,(format "%s: %%[%s%%] %%v" prop-name (edraw-msg "Choose"))
             :value ,(edraw-property-editor-prop-value-to-widget-value
                      prop-value prop-info)
             :notify ,notify
             ,@(mapcar
                (lambda (item)
                  (cond
                   ((null item) (list 'item :tag " " :value nil)) ;;If :tag="", show separator
                   ((stringp item) (list 'item :tag item :value item))
                   ;;((symbolp item) (list 'editable-field :tag (symbol-name item)))
                   ))
                types)))))
    (edraw-property-editor-prop-widget
     :widget widget
     :target target
     :prop-info prop-info)))

;;;;;; Text Field Widget

(defun edraw-property-editor-create-text-field-widget (indent
                                                       target
                                                       prop-name prop-value
                                                       prop-info notify)
  (widget-insert (make-string indent ? ))
  (edraw-property-editor-prop-widget
   :widget (widget-create
            'editable-field
            :keymap edraw-property-editor-field-map
            :format (format "%s: %%v" prop-name)
            :value (edraw-property-editor-prop-value-to-widget-value
                    prop-value prop-info)
            :notify notify)
   :target target
   :prop-info prop-info))

;;;;;; Number Widget

(defvar edraw-property-editor-number-title-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km [down-mouse-1] 'edraw-property-editor-number-dragging)
    (define-key km [wheel-down] 'edraw-property-editor-field-wheel-decrease)
    (define-key km [wheel-up] 'edraw-property-editor-field-wheel-increase)
    km))

(defun edraw-property-editor-create-number-widget (indent
                                                   target
                                                   prop-name prop-value
                                                   prop-info notify)
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
                  :value (edraw-property-editor-prop-value-to-widget-value
                          prop-value prop-info)
                  :notify notify))
         (field (edraw-property-editor-number-field-create
                 (current-buffer) widget prop-info)))
    (widget-put widget :edraw-field field)

    ;;`1-' means avoid reacting to field clicks when the value is empty
    (put-text-property name-begin (1- name-end)
                       'keymap edraw-property-editor-number-title-keymap)
    (put-text-property name-begin (1- name-end)
                       'pointer 'hdrag)
    (put-text-property line-begin name-end 'edraw-field field)
    (edraw-property-editor-prop-widget
     :widget widget
     :target target
     :prop-info prop-info)))

(defclass edraw-property-editor-number-field ()
  ((buffer :initarg :buffer)
   (widget :initarg :widget)
   (prop-info :initarg :prop-info)
   (min-value :initarg :min-value)
   (max-value :initarg :max-value)
   (default-value :initarg :default-value)
   (divisor :initarg :divisor)))

(defun edraw-property-editor-number-field-create (buffer widget prop-info)
  (let ((prop-type (plist-get prop-info :type)))
    (edraw-property-editor-number-field
     :buffer buffer
     :widget widget
     :prop-info prop-info
     :min-value (pcase prop-type
                  ('length 0)
                  ('opacity 0))
     :max-value (pcase prop-type
                  ('opacity 1))
     :default-value (pcase prop-type
                      ('opacity 1)
                      (_ 0))
     :divisor (pcase prop-type
                ('opacity 100.0)
                (_ 1)))))

(cl-defmethod edraw-get-value ((field edraw-property-editor-number-field))
  (with-slots (widget default-value prop-info) field
    (let ((w-value (widget-value widget))
          (to-number (plist-get prop-info :to-number)))
      (if (and (stringp w-value)
               (not (string-empty-p w-value)))
          (funcall to-number w-value)
        default-value))))

(cl-defmethod edraw-set-value ((field edraw-property-editor-number-field) value)
  (with-slots (widget min-value max-value divisor prop-info) field
    (setq value (edraw-clamp value min-value max-value))
    (when (/= divisor 1)
      (setq value (/ (round (* value divisor)) divisor)))
    (edraw-property-editor-prop-widget-value-set
     widget
     (funcall (plist-get prop-info :to-string) value))))

(cl-defmethod edraw-increase ((field edraw-property-editor-number-field) delta)
  (edraw-set-value field (+ (edraw-get-value field)
                            (/ delta (oref field divisor)))))

(defcustom edraw-property-editor-number-dragging-use-slider-bar nil
  "Non-nil means show the slider bar while dragging the property name."
  :group 'edraw-property-editor
  :type 'boolean)

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
             ;; If mouse-fine-grained-tracking is nil,
             ;; motion events come only character by character.
             ;; However, when the mouse pointer is over an image,
             ;; events come pixel by pixel.
             (ov (when edraw-property-editor-number-dragging-use-slider-bar
                   (with-current-buffer buffer
                     (save-excursion
                       (goto-char down-pos)
                       (make-overlay (line-beginning-position)
                                     (line-beginning-position)))))))
        (when ov
          (overlay-put ov 'after-string "\n")
          (edraw-property-editor-number-dragging-image-update
           ov window down-x min-x max-x))
        (unwind-protect
            (let ((mouse-fine-grained-tracking t))
              (edraw-track-dragging
               down-event
               (lambda (move-event)
                 (let* ((move-x (car (posn-x-y (event-start move-event))))
                        (delta-value (/ (- move-x down-x) divisor))
                        (new-value (+ start-value delta-value)))
                   (edraw-set-value field new-value)
                   (when ov
                     (edraw-property-editor-number-dragging-image-update
                      ov window move-x min-x max-x))
                   ))
               nil nil 'window t))
          (when ov
            (delete-overlay ov)))))))

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

;;;;;; Paint Widget

(defun edraw-property-editor-create-paint-widget (indent
                                                  target
                                                  prop-name prop-value
                                                  prop-info notify options)
  (widget-insert (make-string indent ? ))
  (let (field-widget)
    (widget-insert (symbol-name prop-name) ": ")
    (widget-create
     'push-button :notify
     (lambda (&rest _ignore)
       (edraw-property-editor-prop-widget-value-set
        field-widget
        (if edraw-property-editor-apply-immediately ;;@todo or target cannot preview
            (edraw-property-editor-read-property-paint-color-with-preview
             target prop-name field-widget options)
          (edraw-property-editor-read-property-paint-color-without-preview
           prop-name field-widget options))))
     (edraw-msg "Color"))
    ;;(widget-insert " ")
    (setq field-widget
          (widget-create
           'editable-field
           :keymap edraw-property-editor-field-map
           :format "%v"
           :value (edraw-property-editor-prop-value-to-widget-value
                   prop-value prop-info)
           :notify notify))
    (edraw-property-editor-prop-widget
     :widget field-widget
     :target target
     :prop-info prop-info)))

(defun edraw-property-editor-read-property-paint-color-without-preview
    (prop-name field-widget options)
  (let ((old-value (widget-value field-widget)))
    (edraw-color-picker-read-color
     (format "%s: " prop-name)
     old-value
     '("" "none")
     `((:color-name-scheme . web)
       (:no-color . "none")
       ,@(when-let ((image-scale (alist-get 'image-scale options)))
           (list
            (cons :scale-direct image-scale)))
       ,@(when-let ((recent-colors (alist-get 'recent-colors options)))
           (list
            (cons :recent-colors recent-colors)))))))

(defun edraw-property-editor-read-property-paint-color-with-preview
    (target prop-name field-widget options)
  (let ((old-value (widget-value field-widget))
        (undo-backup (edraw-undo-block-begin target)))
    (unwind-protect
        (edraw-color-picker-read-color
         (format "%s: " prop-name)
         old-value
         '("" "none")
         `((:color-name-scheme . web)
           (:no-color . "none")
           ;; Preview
           ,@(list
              (cons
               :on-input-change
               (lambda (string color)
                 (when (or (member string '("" "none"))
                           color)
                   ;; Undo previous change
                   (edraw-undo-all target)
                   ;;@todo suppress modified flag change and notification
                   (edraw-set-property target prop-name string)))))
           ,@(when-let ((image-scale (alist-get 'image-scale options)))
               (list
                (cons :scale-direct image-scale)))
           ,@(when-let ((recent-colors (alist-get 'recent-colors options)))
               (list
                (cons :recent-colors recent-colors)))))
      ;; Restore value for no undo support target
      (edraw-set-property target prop-name old-value)
      ;; Undo previous change
      (edraw-undo-all target)
      (edraw-undo-block-end target undo-backup))))

;;;;;; Marker Widget

(defclass edraw-property-editor-marker-widget
  (edraw-property-editor-prop-widget)
  ((margin-left :initarg :margin-left)
   (choice-widget :initarg :choice-widget)
   (properties-button :initarg :properties-button)
   (marker-prop-list :initarg :marker-prop-list)
   (open-p :initform nil)
   (subprops-marker-type :initform nil)
   (subprops-widgets :initform nil)
   (subprops-overlay :initform nil)
   (buffer :initarg :buffer)
   (pedit :initarg :pedit)))

(cl-defmethod edraw-update-widget-value ((marker-widget
                                          edraw-property-editor-marker-widget))
  (with-slots (choice-widget) marker-widget
    (let ((old-value (widget-value choice-widget)))
      (cl-call-next-method)
      ;; Make sure marker-prop-list points to widget-value.
      ;; It is not enough to do it from the choice-widget's
      ;; notify. Because notify is not called when changing between the
      ;; same type using widget-value-set (because no text change event is
      ;; fired).
      (let ((new-value (widget-value choice-widget)))
        (unless (equal new-value old-value)
          (edraw-update-subprops marker-widget))))))

(defun edraw-property-editor-create-marker-widget (margin-left
                                                   indent
                                                   target
                                                   prop-name prop-value
                                                   prop-info notify
                                                   pedit)
  (widget-insert (make-string (+ margin-left indent) ? ))

  (let* (marker-widget

         ;; Type Selector
         (choice-widget
          (apply
           #'widget-create
           `(menu-choice
             :format ,(format "%s: %%[%s%%] %%v" prop-name (edraw-msg "Choose"))
             :value ,(cond
                      ((stringp prop-value)
                       (if (assoc prop-value edraw-svg-marker-types)
                           (edraw-svg-marker prop-value nil)
                         nil));; Unknown Type
                      ((edraw-svg-marker-p prop-value)
                       (edraw-property-editor-prop-value-to-widget-value
                        prop-value prop-info)))
             :notify ,(lambda (widget changed-widget &optional event)
                        ;;(message "on choice-widget changed value=%s event=%s" (widget-value widget) event)
                        (edraw-on-widget-change marker-widget)
                        (funcall notify widget changed-widget event))
             ,@(edraw-property-editor-default-marker-items
                prop-info (oref pedit options)))))
         ;; Space
         (_ (insert " "))
         ;; Properties Button
         (properties-button
          (widget-create
           'push-button
           :notify (lambda (&rest _args)
                     (edraw-on-properties-button marker-widget))
           :keymap edraw-property-editor-push-button-map
           "..."))
         ;; Line Break
         (_ (insert "\n"))

         (marker-prop-list (edraw-alist-properties-holder
                            :prop-info-list nil
                            :alist-head (cons nil nil)
                            :editor nil
                            :name "Marker")))

    (edraw-add-change-hook
     marker-prop-list
     (lambda (&rest _ignore)
       ;; Update UI
       ;;(message "on marker propery changed prop-list=%s choice-widget-value=%s" (oref marker-prop-list alist-head) (widget-value choice-widget))
       (widget-apply choice-widget :notify choice-widget nil)))

    (setq marker-widget
          (edraw-property-editor-marker-widget
           :target target
           :prop-info prop-info
           :buffer (oref pedit buffer)
           :widget choice-widget
           :choice-widget choice-widget
           :properties-button properties-button
           :margin-left margin-left
           :marker-prop-list marker-prop-list
           :pedit pedit))
    (edraw-update-subprops marker-widget)

    marker-widget))

(defun edraw-property-editor-default-marker-items (prop-info options)
  (let* ((marker-types (nconc
                        (mapcar #'car edraw-svg-marker-types)
                        (unless (plist-get prop-info :required) ;;nullable?
                          (list nil))))
         (items (mapcar
                 (lambda (type)
                   (cond
                    ((null type)
                     (list 'item
                           ;;If :tag="", show separator
                           :tag "     " :value nil :format "%t"))
                    ((stringp type)
                     (list
                      'item
                      :tag type
                      :format "%t"
                      :value (edraw-svg-marker
                              type
                              (alist-get
                               type
                               (alist-get 'marker-defaults options)
                               nil nil #'equal))
                      :match (lambda (_widget value)
                               (or
                                (and (edraw-svg-marker-p value)
                                     (equal (edraw-svg-marker-type value) type))
                                (and (stringp value)
                                     (string= value type))))))))
                 marker-types)))
    items))

(cl-defmethod edraw-on-widget-change ((marker-widget
                                       edraw-property-editor-marker-widget))
  (edraw-update-subprops marker-widget))

(cl-defmethod edraw-on-properties-button ((marker-widget
                                           edraw-property-editor-marker-widget))
  ;; Toggle
  (oset marker-widget open-p (not (oref marker-widget open-p)))
  ;; Update subprops
  (edraw-update-subprops marker-widget))

(cl-defmethod edraw-update-properties-button
  ((marker-widget edraw-property-editor-marker-widget))
  (widget-apply (oref marker-widget properties-button)
                (if (edraw-svg-marker-prop-info-list
                     (edraw-svg-marker-type
                      (widget-value (oref marker-widget choice-widget))))
                    :activate
                  :deactivate)))

(cl-defmethod edraw-update-subprops ((marker-widget
                                      edraw-property-editor-marker-widget))
  ;;(message "edraw-update-subprops")
  (edraw-update-marker-prop-list marker-widget)
  (edraw-update-properties-button marker-widget)
  (with-slots (choice-widget subprops-marker-type open-p subprops-widgets) marker-widget
    (if (not open-p)
        ;; Closed
        (progn
          (edraw-remove-subprops marker-widget)
          (setq subprops-marker-type nil))
      ;; Open
      (let* ((new-marker-value (widget-value choice-widget))
             (new-marker-type (edraw-svg-marker-type new-marker-value)))
        (if (equal new-marker-type subprops-marker-type)
            ;; Same type
            (dolist (pw subprops-widgets)
              (edraw-update-widget-value pw))
          ;; Different type
          (edraw-remove-subprops marker-widget)
          (edraw-insert-subprops marker-widget)
          (setq subprops-marker-type new-marker-type)
          )))))

(cl-defmethod edraw-remove-subprops ((marker-widget
                                      edraw-property-editor-marker-widget))
  (with-slots (subprops-widgets subprops-overlay) marker-widget
    ;; Delete widgets
    (when subprops-widgets
      (dolist (pw subprops-widgets)
        (edraw-widget-delete pw))
      (setq subprops-widgets nil))
    ;; Delete region
    (when subprops-overlay
      (when (overlay-buffer subprops-overlay)
        (with-current-buffer (overlay-buffer subprops-overlay)
          (let ((inhibit-read-only t))
            (delete-region
             (overlay-start subprops-overlay)
             (overlay-end subprops-overlay)))
          (delete-overlay subprops-overlay)))
      (setq subprops-overlay nil))))

(cl-defmethod edraw-update-marker-prop-list
  ((marker-widget edraw-property-editor-marker-widget))
  (with-slots (marker-prop-list choice-widget) marker-widget
    (let ((marker-value (widget-value choice-widget)))
      (edraw-set-alist-head
       marker-prop-list
       (if marker-value
           (edraw-svg-marker-props-head marker-value)
         (cons nil nil)))
      (edraw-set-prop-info-list
       marker-prop-list
       (if marker-value
           (edraw-svg-marker-prop-info-list
            (edraw-svg-marker-type marker-value))
         nil)))))

(cl-defmethod edraw-insert-subprops ((marker-widget
                                      edraw-property-editor-marker-widget))
  (with-slots (buffer
               choice-widget marker-type marker-prop-list
               margin-left
               subprops-widgets subprops-overlay
               pedit)
      marker-widget
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (widget-get choice-widget :to))
          (forward-line)
          (let* ((beg (point))
                 (widgets (prog1
                              (edraw-insert-property-widgets
                               pedit marker-prop-list (+ margin-left 12))
                            (widget-setup)))
                 (end (point))
                 (ov (let ((ov (make-overlay beg end nil t nil)))
                       (overlay-put ov 'evaporate t)
                       ov)))
            (setq subprops-widgets widgets
                  subprops-overlay ov)))))))



;;;;;; Increase/Decrease Value By Wheel

;;@todo Add feature to change menu-choice-widget by mouse wheel

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

;;;;; Property Value To Widget Value

(cl-defmethod edraw-update-widgets-value ((pedit edraw-property-editor))
  (with-slots (widgets) pedit
    (dolist (prop-widget widgets)
      (edraw-update-widget-value prop-widget))))

(cl-defmethod edraw-update-widget-value ((pw edraw-property-editor-prop-widget))
  (with-slots (widget prop-info) pw
    (let* ((old-w-value (widget-value widget))
           (prop-name (plist-get prop-info :name))
           (prop-value (edraw-get-property (edraw-get-target pw) prop-name))
           (new-w-value (edraw-property-editor-prop-value-to-widget-value
                         prop-value
                         prop-info)))
      (unless (edraw-property-editor-equal-widget-value old-w-value new-w-value
                                                        prop-info)
        ;;(message "update-widget-value: %s: %s to %s" prop-name old-w-value new-w-value)
        ;; Prevent notification
        (edraw-property-editor-prop-widget-value-set-without-notify
         widget new-w-value)))))

(defun edraw-property-editor-prop-value-to-widget-value (prop-value prop-info)
  (pcase (plist-get prop-info :type)
    (`(or . ,_)
     ;; string or nil
     prop-value)
    ('marker
     ;; marker descriptor
     prop-value)
    (_
     ;; string only
     (funcall (plist-get prop-info :to-string) prop-value))))

(defun edraw-property-editor-equal-widget-value (wv1 wv2 prop-info)
  (cond
   ((plist-get prop-info :number-p)
    (or
     (equal wv1 wv2)
     (let ((to-number (plist-get prop-info :to-number)))
       ;;@todo empty-string-p (opacity: "" = 1.0)
       ;;@todo very small difference (100.01 = 100.009999999999)
       ;; 100.0 = 100 = 100.
       (= (funcall to-number wv1) (funcall to-number wv2)))))
   (t
    (equal wv1 wv2))))

;;;;; Widget Value to Property Value

(cl-defmethod edraw-get-as-property-value ((pw
                                            edraw-property-editor-prop-widget))
  "Returns the current PW value as (property name . property value)."
  (with-slots (widget prop-info) pw
    (cons (plist-get prop-info :name)
          (edraw-property-editor-widget-value-to-prop-value
           (widget-value widget) prop-info))))

(cl-defmethod edraw-apply-properties ((pedit edraw-property-editor))
  "Applies all property values being edited by PEDIT to the target object."
  (with-slots (widgets target) pedit
    (edraw-set-properties
     target
     (mapcar #'edraw-get-as-property-value widgets))))

(defun edraw-property-editor-widget-value-to-prop-value (w-value prop-info)
  (cond
   ((eq (plist-get prop-info :type) 'marker)
    w-value)
   ((or (and (stringp w-value) (string-empty-p w-value))
        (null w-value))
    ;; w-value is an empty string or nil
    ;; (if (not (plist-get prop-info :required))
    ;;     nil ;;property is not required and
    ;;   ;;@todo default value???
    ;;   nil)
    nil)
   (t
    ;;@todo error check
    (funcall (plist-get prop-info :from-string) w-value))))

;;;;; Bottom Buttons

(defun edraw-property-editor--buffer-on-last-event ()
  ;; For mouse event
  (if (consp last-command-event)
      (window-buffer (posn-window (event-start last-command-event)))
    (current-buffer)))

(defmacro edraw-property-editor--with-event-buffer (&rest body)
  `(with-current-buffer (edraw-property-editor--buffer-on-last-event)
     (when edraw-property-editor--pedit
       ,@body)))

(defun edraw-property-editor--close (&rest _ignore)
  (interactive)
  (edraw-property-editor--with-event-buffer
   (edraw-close edraw-property-editor--pedit)))

(cl-defmethod edraw-close ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-close display)))

(defun edraw-property-editor--apply (&rest _ignore)
  (interactive)
  (edraw-property-editor--with-event-buffer
   (edraw-apply-properties edraw-property-editor--pedit)))

(defun edraw-property-editor--shape-index ()
  (when-let ((pedit edraw-property-editor--pedit))
    (with-slots (target) pedit
      (when (edraw-property-editor-target-shape-p target)
        (edraw-node-position target)))))

(defun edraw-property-editor--num-shapes ()
  (when-let ((pedit edraw-property-editor--pedit))
    (with-slots (target) pedit
      (when (edraw-property-editor-target-shape-p target)
        (edraw-node-siblings-count target)))))

(defun edraw-property-editor--prevnext (prev-or-next-func)
  (when-let ((pedit edraw-property-editor--pedit))
    (with-slots (target) pedit
      (when (edraw-property-editor-target-shape-p target)
        (when-let ((new-target (funcall prev-or-next-func target)))
          (if edraw-property-editor-tracking-selected-shape
              (edraw-select new-target)
            ;; destroy PEDIT and open NEW-TARGET
            (edraw-property-editor-open new-target)))))))

(defun edraw-property-editor--prev (&rest _ignore)
  (interactive)
  (edraw-property-editor--with-event-buffer
   (edraw-property-editor--prevnext 'edraw-previous-sibling)))

(defun edraw-property-editor--next (&rest _ignore)
  (interactive)
  (edraw-property-editor--with-event-buffer
   (edraw-property-editor--prevnext 'edraw-next-sibling)))

(defun edraw-property-editor--set-as-default (&rest _ignore)
  (interactive)
  (edraw-property-editor--with-event-buffer
   (with-slots (target) edraw-property-editor--pedit
     (when (edraw-property-editor-target-shape-p target)
       (edraw-set-all-properties-as-default target)))))

(defun edraw-property-editor--menu (&rest _ignore)
  (interactive)
  (edraw-property-editor--with-event-buffer
   (let ((pedit edraw-property-editor--pedit))
     (edraw-popup-menu
      (edraw-msg "Property Editor")
      `((
         ,(if (edraw-get-frame-mode (oref pedit display))
              (edraw-msg "To Window")
            (edraw-msg "To Frame"))
         edraw-property-editor--toggle-frame-mode)
        ((edraw-msg "Frame")
         (((edraw-msg "Mode Line") edraw-property-editor--toggle-frame-mode-line-p
           :button (:toggle . ,(edraw-get-frame-mode-line-p pedit)))
          ((edraw-msg "Child Frame") edraw-property-editor--toggle-frame-child-p
           :button (:toggle . ,(edraw-get-frame-child-p pedit)))
          ((edraw-msg "Top Most") edraw-property-editor--toggle-frame-top-most-p
           :button (:toggle . ,(edraw-get-frame-top-most-p pedit)))))
        ((edraw-msg "Apply") edraw-property-editor--apply)
        ((edraw-msg "Close") edraw-property-editor--close))))))

(defun edraw-property-editor--toggle-frame-mode ()
  (interactive)
  (edraw-property-editor--with-event-buffer
   (edraw-toggle-frame-mode edraw-property-editor--pedit)))

(cl-defmethod edraw-toggle-frame-mode ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-toggle-frame-mode display)))

(cl-defmethod edraw-get-frame-mode ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-get-frame-mode display)))

(defun edraw-property-editor--toggle-frame-mode-line-p ()
  (interactive)
  (edraw-property-editor--with-event-buffer
   (edraw-toggle-frame-mode-line-p edraw-property-editor--pedit)))

(cl-defmethod edraw-toggle-frame-mode-line-p ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-toggle-frame-mode-line-p display)))

(cl-defmethod edraw-get-frame-mode-line-p ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-get-frame-mode-line-p display)))

(defun edraw-property-editor--toggle-frame-child-p ()
  (interactive)
  (edraw-property-editor--with-event-buffer
   (edraw-toggle-frame-child-p edraw-property-editor--pedit)))

(cl-defmethod edraw-toggle-frame-child-p ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-toggle-frame-child-p display)))

(cl-defmethod edraw-get-frame-child-p ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-get-frame-child-p display)))

(defun edraw-property-editor--toggle-frame-top-most-p ()
  (interactive)
  (edraw-property-editor--with-event-buffer
   (edraw-toggle-frame-top-most-p edraw-property-editor--pedit)))

(cl-defmethod edraw-toggle-frame-top-most-p ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-set-frame-parameter
     display 'z-group
     (if (eq (edraw-get-frame-parameter display 'z-group) 'above)
         nil
       'above))))

(cl-defmethod edraw-get-frame-top-most-p ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (eq (edraw-get-frame-parameter display 'z-group) 'above)))


;;;;; Synchronizing Property Editor with Target

(cl-defmethod edraw-initialize-hooks ((pedit edraw-property-editor))
  (with-slots (target buffer) pedit
    (when target
      (edraw-add-change-hook target 'edraw-on-target-changed pedit))
    (when buffer
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook
                  'edraw-property-editor--on-kill-buffer
                  nil t)))))

(cl-defmethod edraw-uninitialize-hooks ((pedit edraw-property-editor))
  (with-slots (target buffer) pedit
    (when target
      (edraw-remove-change-hook target 'edraw-on-target-changed pedit))
    (when buffer
      (with-current-buffer buffer
        (remove-hook 'kill-buffer-hook
                     'edraw-property-editor--on-kill-buffer
                     t)))))

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
  ;;(message "edraw-on-update-timer")
  (with-slots (update-timer buffer) pedit
    (setq update-timer nil)
    (when buffer
      (with-current-buffer buffer
        (edraw-update-widgets-value pedit)))))

(cl-defmethod edraw-cancel-update-timer ((pedit edraw-property-editor))
  (with-slots (update-timer) pedit
    (when update-timer
      (cancel-timer update-timer)
      (setq update-timer nil))))

(cl-defmethod edraw-destroy ((pedit edraw-property-editor))
  ;;(message "in edraw-destroy pedit")
  (edraw-unobserve-target pedit)
  (with-slots (display buffer) pedit
    (edraw-destroy display)
    (setq buffer nil)))

(cl-defmethod edraw-unobserve-target ((pedit edraw-property-editor))
  (with-slots (target) pedit
    (edraw-uninitialize-hooks pedit)
    (edraw-cancel-update-timer pedit)
    (setq target nil)))


;;;; Utility

(defun edraw-property-editor-widget-button-click (event)
  "An alternative to widget-button-click for double-click and triple-click."
  (interactive "e")
  (cl-letf (((symbol-function 'widget-button-release-event-p)
             'edraw-property-editor-widget-button-release-event-p))
    (widget-button-click event)))

(defun edraw-property-editor-widget-button-release-event-p (event)
  "An alternative to widget-button-release-event-p for double-click
and triple-click."
  (and (eventp event)
       (memq (event-basic-type event) '(mouse-1 mouse-2 mouse-3))
       (or (and (or (memq 'double (event-modifiers event)) ;;double click
                    (memq 'triple (event-modifiers event))) ;;triple click
                (null (memq 'down (event-modifiers event))))
           (memq 'click (event-modifiers event))
           (memq 'drag (event-modifiers event)))))


(provide 'edraw-property-editor)
;;; edraw-property-editor.el ends here
