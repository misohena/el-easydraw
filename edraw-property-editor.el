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

(declare-function edraw-node-position "edraw")
(declare-function edraw-node-siblings-count "edraw")

;;;; Property Editor Target


(defclass edraw-property-editor-target ()
 ())

(cl-defmethod edraw-name
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
   (update-timer :initform nil)))

(defvar-local edraw-property-editor--pedit nil)

(defun edraw-property-editor-buffer ()
  (get-buffer edraw-property-editor-buffer-name))

(defun edraw-property-editor-close ()
  (when-let ((buffer (edraw-property-editor-buffer)))
    (with-current-buffer buffer
      (when edraw-property-editor--pedit
        (edraw-close edraw-property-editor--pedit)))))

(defun edraw-property-editor-open (target)
  (let* ((buffer (get-buffer-create edraw-property-editor-buffer-name))
         ;; Get property editor object
         (pedit (or (with-current-buffer buffer edraw-property-editor--pedit)
                    (edraw-property-editor-create-object buffer))))
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
        (edraw-observe-target pedit target))

      ;; Open window or frame
      (edraw-display-buffer pedit))))

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
      (edraw-insert-property-widgets pedit))

    ;; Bottom
    (widget-insert (make-string 2 ? ))
    (when target
      (widget-create 'push-button
                     :notify 'edraw-property-editor--set-as-default
                     :keymap edraw-property-editor-push-button-map
                     (edraw-msg "Set as default"))
      (widget-insert " ")

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

(cl-defmethod edraw-insert-property-widgets ((pedit edraw-property-editor))
  (with-slots (target widgets) pedit
    (let* ((prop-info-list (edraw-get-property-info-list target))
           (max-name-width (when prop-info-list
                             (apply #'max
                                    (mapcar
                                     (lambda (prop-info)
                                       (string-width
                                        (symbol-name
                                         (plist-get prop-info :name))))
                                     prop-info-list)))))
      (dolist (prop-info prop-info-list)
        (unless (plist-get prop-info :internal)
          (let* ((indent (- max-name-width
                            (string-width
                             (symbol-name (plist-get prop-info :name))))))
            (push (edraw-property-editor-prop-widget-create
                   target prop-info indent)
                  widgets))))
      (setq widgets (nreverse widgets)))))

;;;;; Window/Frame

(cl-defmethod edraw-display-buffer ((pedit edraw-property-editor))
  (with-slots (display) pedit
    (edraw-display-buffer display)))


;;;;; Prop Widget

(defclass edraw-property-editor-prop-widget ()
  ((widget :initarg :widget)
   (prop-info :initarg :prop-info)))

(defun edraw-property-editor-prop-widget-create (target prop-info indent)
  (let* ((notify (edraw-property-editor-prop-widget-create-updator
                  target prop-info))
         (widget (edraw-property-editor-prop-widget-create-widget
                  target prop-info indent notify)))
    (edraw-property-editor-prop-widget
     :widget widget
     :prop-info prop-info)))

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

(defun edraw-property-editor-prop-widget-create-updator (target prop-info)
  (lambda (widget _changed-widget &optional _event)
    ;; Called 4 times per widget-value-set call.
    ;; 1. delete chars (event=(before-change BEG END))
    ;; 2. delete chars (event=(after-change BEG END))
    ;; 3. insert chars (event=(before-change BEG END))
    ;; 4. insert chars (event=(after-change BEG END))
    (when (and edraw-property-editor-apply-immediately
               (not edraw-property-editor-prop-widget--notification-suppressed))
      (edraw-set-property
       target
       (plist-get prop-info :name)
       (edraw-property-editor-widget-value-to-prop-value
        (widget-value widget)
        prop-info)))))

(defun edraw-property-editor-prop-widget-create-widget (target
                                                        prop-info
                                                        indent
                                                        notify)
  (let* ((prop-name (plist-get prop-info :name))
         (prop-value (edraw-get-property target prop-name))
         (prop-type (plist-get prop-info :type))
         (prop-number-p (plist-get prop-info :number-p)))
    (cond
     (prop-number-p
      (edraw-property-editor-create-number-widget
       indent prop-name prop-value prop-info notify))
     ((eq (car-safe prop-type) 'or)
      (edraw-property-editor-create-menu-choice-widget
       indent prop-name prop-value prop-info notify))
     ((eq prop-type 'paint)
      (edraw-property-editor-create-paint-widget
       indent prop-name prop-value prop-info notify target))
     (t
      (edraw-property-editor-create-text-field-widget
       indent prop-name prop-value prop-info notify)))))

;;;;;; Menu Choice Widget

(defun edraw-property-editor-create-menu-choice-widget
    (indent prop-name prop-value prop-info notify)
  (widget-insert (make-string indent ? ))
  (let* ((prop-required (plist-get prop-info :required))
         (prop-type (plist-get prop-info :type))
         (types (if prop-required
                    (cdr prop-type) ;;skip (or)
                  ;; nullable
                  (cons nil (cdr prop-type)))))
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

;;;;;; Text Field Widget

(defun edraw-property-editor-create-text-field-widget
    (indent prop-name prop-value prop-info notify)
  (widget-insert (make-string indent ? ))
  (widget-create
   'editable-field
   :keymap edraw-property-editor-field-map
   :format (format "%s: %%v" prop-name)
   :value (edraw-property-editor-prop-value-to-widget-value
           prop-value prop-info)
   :notify notify))

;;;;;; Number Widget

(defvar edraw-property-editor-number-title-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km [down-mouse-1] 'edraw-property-editor-number-dragging)
    (define-key km [wheel-down] 'edraw-property-editor-field-wheel-decrease)
    (define-key km [wheel-up] 'edraw-property-editor-field-wheel-increase)
    km))

(defun edraw-property-editor-create-number-widget
    (indent prop-name prop-value prop-info notify)
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
    widget))

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

(defun edraw-property-editor-create-paint-widget
    (indent prop-name prop-value prop-info notify target)
  (widget-insert (make-string indent ? ))
  (let (field-widget)
    (widget-insert (symbol-name prop-name) ": ")
    (widget-create
     'push-button :notify
     (lambda (&rest _ignore)
       (edraw-property-editor-prop-widget-value-set
        field-widget
        (edraw-property-editor-read-property-paint-color target prop-name
                                                         field-widget)))
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
    field-widget))

(defun edraw-property-editor-read-property-paint-color (target
                                                        prop-name field-widget)
  (defvar edraw-editor-image-scaling-factor) ;;edraw.el
  (let ((current-value (widget-value field-widget)))
    (unwind-protect
        (edraw-color-picker-read-color
         (format "%s: " prop-name)
         current-value
         '("" "none")
         `((:color-name-scheme . web)
           (:no-color . "none")
           ,@(when (edraw-property-editor-target-shape-p target)
               (list
                (cons :on-input-change
                      (lambda (string color)
                        (when (or (member string '("" "none"))
                                  color)
                          ;;@todo suppress modified flag change and notification
                          (edraw-set-property target prop-name string))))))
           ,@(when (and (boundp 'edraw-editor-image-scaling-factor)
                        edraw-editor-image-scaling-factor)
               (list
                (cons :scale-direct edraw-editor-image-scaling-factor)))))
      (edraw-property-editor-prop-widget-value-set
       field-widget current-value))))

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

(defun edraw-property-editor-prop-value-to-widget-value (prop-value prop-info)
  (pcase (plist-get prop-info :type)
    (`(or . ,_)
     ;; string or nil
     prop-value)
    (_
     ;; string only
     (funcall (plist-get prop-info :to-string) prop-value))))

(cl-defmethod edraw-update-widget-value
  ((prop-widget edraw-property-editor-prop-widget) target)
  (with-slots (widget prop-info) prop-widget
    (let* ((old-w-value (widget-value widget))
           (prop-name (plist-get prop-info :name))
           (prop-value (edraw-get-property target prop-name))
           (new-w-value (edraw-property-editor-prop-value-to-widget-value
                         prop-value
                         prop-info)))
      (unless (edraw-property-editor-equal-value old-w-value new-w-value
                                                 prop-info)
        ;;(message "target chagned: %s: %s to %s" prop-name old-w-value new-w-value)
        ;; Prevent notification
        (edraw-property-editor-prop-widget-value-set-without-notify
         widget new-w-value)))))

(defun edraw-property-editor-equal-value (wv1 wv2 prop-info)
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

(cl-defmethod edraw-update-widgets-value ((pedit edraw-property-editor))
  (with-slots (widgets target) pedit
    (dolist (prop-widget widgets)
      (edraw-update-widget-value prop-widget target))))

;;;;; Widget Value to Property Value

(cl-defmethod edraw-get-as-property-value
  ((prop-widget edraw-property-editor-prop-widget))
  "Returns the current PROP-WIDGET value as (property name . property value)."
  (with-slots (widget prop-info) prop-widget
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
  (if (or (and (stringp w-value) (string-empty-p w-value))
          (null w-value))
      ;; w-value is an empty string or nil
      ;; (if (not (plist-get prop-info :required))
      ;;     nil ;;property is not required and
      ;;   ;;@todo default value???
      ;;   nil)
      nil
    ;;@todo error check
    (funcall (plist-get prop-info :from-string) w-value)))

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
