;;; edraw-widget.el --- Widget                       -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

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
(require 'wid-edit)
(require 'edraw-color-picker)
(require 'edraw-dom-svg)



;;;; `edraw-web-color' Widget

;; Example:
;; (progn (pop-to-buffer (generate-new-buffer "*Widget Example*")) (widget-create 'edraw-web-color) (use-local-map widget-keymap) (widget-setup))

(define-widget 'edraw-web-color 'editable-field
  "Choose a web color (with sample)."
  ;; Derived from `color' widget (wid-edit.el)
  :value "black"
  :format "%{%t%}: %v %{      %}\n"
  :tag "Color"
  :value-create 'edraw-widget-web-color-value-create
  :size 26 ;; Can contain "rgba(255,255,255,1.2345)"
  :completions (cons "none" (mapcar #'car edraw-color-css-color-names))
  :sample-face-get 'edraw-widget-web-color-sample-face-get
  :notify 'edraw-widget-web-color-notify
  :match #'edraw-widget-web-color-match
  :validate #'edraw-widget-web-color-validate
  :action 'edraw-widget-web-color-action
  :choose-tag " Choose ")

(defun edraw-widget-web-color-value-create (widget)
  (widget-field-value-create widget)
  (widget-insert " ")
  (widget-create-child-and-convert
   widget 'push-button
   :tag (widget-get widget :choose-tag)
   :action 'edraw-widget-web-color--choose-action)
  (widget-insert " "))

(defun edraw-widget-web-color-read-color (old-color)
  (edraw-color-picker-read-color
   nil
   old-color
   '("" "none")
   `((:color-name-scheme . web)
     (:no-color . "none"))))

(defun edraw-widget-web-color--choose-action (widget &optional _event)
  (let* ((wp (widget-get widget :parent))
         (old-color (widget-value wp))
         (new-color (edraw-widget-web-color-read-color old-color)))
    (widget-value-set wp new-color)))

(defun edraw-widget-web-color-sample-face-get (widget)
  (let ((color (condition-case nil
                   (edraw-color-from-string (widget-value widget))
                 (error (widget-get widget :value)))))
    (if color
        ;;@todo Use SVG image for sample
        (list (cons 'background-color (edraw-to-string-hex (edraw-change-a color 1.0)))) ;;Force a=1.0
      'default)))

(defun edraw-widget-web-color-action (widget &optional event)
  "Prompt for a color."
  (let* ((old-color (widget-value widget))
         (new-color (edraw-widget-web-color-read-color old-color)))
    (when new-color
      (widget-value-set widget new-color)
      (widget-setup)
      (widget-apply widget :notify widget event))))

(defun edraw-widget-web-color-notify (widget child &optional event)
  "Update the sample, and notify the parent."
  (overlay-put (widget-get widget :sample-overlay)
               'face (widget-apply widget :sample-face-get))
  (widget-default-notify widget child event))

(defun edraw-widget-web-color-match (_widget value)
  "Non-nil if VALUE is a defined color or a RGB hex string."
  (and (stringp value)
       (or (string= value "")
           (string= value "none")
           (assoc value edraw-color-css-color-names)
           (string-match edraw-color-string-patterns-re value))))

(defun edraw-widget-web-color-validate (widget)
  "Check that WIDGET's value is a valid color."
  (let ((value (widget-value widget)))
    (unless (edraw-widget-web-color-match widget value)
      (widget-put widget :error (format "Invalid color: %S" value))
      widget)))



;;;; `edraw-checkbox' Widget

(define-widget 'edraw-checkbox 'checkbox
  "My improved checkbox widget."
  :action 'edraw-widget-checkbox-action)

(defun edraw-widget-checkbox-action (widget &optional event)
  "Toggle checkbox, set active state of sibling, and notify parent."
  ;; Derived from `widget-checkbox-action'

  ;; Notification should be done after all state changes
  ;; (widget-toggle-action widget event)

  ;; Change checkbox state
  (widget-value-set widget (not (widget-value widget)))
  ;; Change activate state
  (let ((sibling (widget-get-sibling widget)))
    (when sibling
      (if (widget-value widget)
          (widget-apply sibling :activate)
        (widget-apply sibling :deactivate))
      (widget-clear-undo)))
  ;; Finally, call event handlers
  (widget-apply widget :notify widget event)
  (run-hook-with-args 'widget-edit-functions widget))



;;;; Check List

(defun edraw-widget-checklist-add-item (widget type chosen)
  "Create checklist item in WIDGET of type TYPE.
If the item is checked, CHOSEN is a cons whose cdr is the value."
  ;; Derived from `widget-checklist-add-item'

  ;; Use edraw-checkbox instead of checkbox.
  (cl-letf* ((orig-fun (symbol-function 'widget-create-child-and-convert))
             ((symbol-function 'widget-create-child-and-convert)
              (lambda (parent type &rest args)
                (when (eq type 'checkbox)
                  (setq type 'edraw-checkbox))
                (apply orig-fun parent type args))))
    (widget-checklist-add-item widget type chosen)))



;;;; `edraw-attribute-list' Widget

(define-widget 'edraw-attribute-list 'checklist
  "Widget for editing attributes.
The following properties have special meanings for this widget:

:value is a plist of attributes.

:default-attributes, if non-nil, is a plist of defaults for attributes."
  :tag "Attributes"
  ;; Note: If ":" is not included, a "Bad format" error will occur in
  ;; Customize UI (see: cus-edit.el)
  :format "%{%t%}:\n%v"
  :button-args '(:help-echo "Control whether this attribute has any effect.")
  :visibility-button-help-echo "Show or hide all attributes."
  :visibility-button-on-tag "Hide Unused Attributes"
  :visibility-button-off-tag "Show All Attributes"
  :convert-widget 'edraw-widget-attribute-list-convert-widget
  :value-create 'edraw-widget-attribute-list-value-create
  :greedy t

  :default-attributes nil
  :show-all-attributes nil ;; Current state of whether to display all attributes
  )

(defun edraw-widget-attribute-list-convert-widget (widget)
  "Convert :args as widget types in WIDGET."
  (widget-put
   widget
   :args (mapcar (lambda (arg)
                   (widget-convert
                    arg
                    :deactivate 'edraw-widget-attribute-list-deactivate
                    :activate 'edraw-widget-attribute-list-activate
                    :delete 'edraw-widget-attribute-list-delete))
                 (widget-get widget :args)))
  widget)

(defun edraw-widget-attribute-list-value-create (widget)
  ;; Derived from `custom-face-edit-value-create'
  (let ((show-all-p (widget-get widget :show-all-attributes)))
    ;; First element in line
    (unless (looking-back "^ *" (line-beginning-position))
      (insert ?\n))

    ;; Add extra spaces
    (insert-char ?\s (or (widget-get widget :extra-offset) 0));; Fix: Problem when :extra-offset is nil.

    ;; Add item widgets
    (let ((alist
           (widget-checklist-match-find widget (widget-get widget :value)))
          (defaults
           (widget-checklist-match-find widget (widget-get
                                                widget :default-attributes))))
      (if (or alist defaults show-all-p)
          (dolist (prop (widget-get widget :args))
            (let ((entry (or (assq prop alist)
                             (assq prop defaults))))
              (if (or entry show-all-p)
                  ;; Fix: Error when item is cons widget.
                  ;; Use edraw-checkbox instead of checkbox.
                  (edraw-widget-checklist-add-item widget prop entry))))
        (insert (propertize "-- Empty --" 'face 'shadow) ?\n)))

    ;; Add visibility toggle button
    (let ((indent (widget-get widget :indent)))
      (if indent (insert-char ?\s (widget-get widget :indent))))
    (let ((buttons  (widget-get widget :buttons)))
      (push (widget-create-child-and-convert
             widget 'visibility
             :help-echo (widget-get widget :visibility-button-help-echo) ;;Fix
             :button-face 'edraw-widget-attribute-list-visibility
             :pressed-face 'edraw-widget-attribute-list-visibility
             :mouse-face 'highlight
             :on (widget-get widget :visibility-button-on-tag) ;;Fix
             :off (widget-get widget :visibility-button-off-tag) ;;Fix
             :on-glyph nil
             :off-glyph nil
             :always-active t
             :action 'edraw-widget-attribute-list-visibility-action
             show-all-p)
            buttons)
      (widget-put widget :buttons buttons))
    (insert ?\n)
    (widget-put widget :children (nreverse (widget-get widget :children)))))

(defface edraw-widget-attribute-list-visibility
  '((t :height 0.8 :inherit link))
  "Face for the `edraw-widget-attribute-list-visibility' widget."
  :group 'edraw-widget-attribute-list)

(defun edraw-widget-attribute-list-visibility-action (widget &rest _ignore)
  ;; Derived from `custom-face-edit-value-visibility-action'
  ;; Toggle hiding of attributes.
  (let ((parent (widget-get widget :parent)))
    (widget-put parent :show-all-attributes
                (not (widget-get parent :show-all-attributes)))
    (edraw-widget-attribute-list-redraw parent)))

(defun edraw-widget-attribute-list-redraw (widget)
  "Redraw WIDGET with current settings."
  ;; Derived from `custom-redraw'
  (let ((line (count-lines (point-min) (point)))
        (column (current-column))
        (pos (point))
        (from (marker-position (widget-get widget :from)))
        (to (marker-position (widget-get widget :to))))
    (save-excursion
      (widget-value-set widget (widget-value widget))
      ;;(edraw-widget-attribute-list-redraw-magic widget)
      (widget-setup))
    (when (and (>= pos from) (<= pos to))
      (condition-case nil
          (progn
            (goto-char (point-min))
            (forward-line (if (> column 0)
                              (1- line)
                            line))
            (move-to-column column))
        (error nil)))))

(defun edraw-widget-attribute-list-deactivate (widget)
  "Make edraw-widget-attribute-list widget WIDGET inactive for user modifications."
  ;; Derived from `custom-face-edit-deactivate'
  (unless (widget-get widget :inactive)
    (let ((tag (edraw-widget-attribute-list-attribute-tag widget))
          (from (copy-marker (widget-get widget :from)))
          (value (widget-value widget))
          (inhibit-read-only t)
          (inhibit-modification-hooks t))
      (save-excursion
        (goto-char from)
        (widget-default-delete widget)
        (insert tag ": " (propertize "--" 'face 'shadow) "\n")
        (widget-put widget :inactive
                    (cons value (cons from (- (point) from))))))))

(defun edraw-widget-attribute-list-activate (widget)
  "Make edraw-attribute-list widget WIDGET active for user modifications."
  ;; Derived from `custom-face-edit-activate'
  (let ((inactive (widget-get widget :inactive))
        (inhibit-read-only t)
        (inhibit-modification-hooks t))
    (when (consp inactive)
      (save-excursion
        (goto-char (car (cdr inactive)))
        (delete-region (point) (+ (point) (cdr (cdr inactive))))
        (widget-put widget :inactive nil)
        (widget-apply widget :create)
        (widget-value-set widget (car inactive))
        (widget-setup)))))

(defun edraw-widget-attribute-list-delete (widget)
  "Remove WIDGET from the buffer."
  ;; Derived from `custom-face-edit-delete'
  (let ((inactive (widget-get widget :inactive))
        (inhibit-read-only t)
        (inhibit-modification-hooks t))
    (if (not inactive)
        ;; Widget is alive, we don't have to do anything special
        (widget-default-delete widget)
      ;; WIDGET is already deleted because we did so to deactivate it;
      ;; now just get rid of the label we put in its place.
      (delete-region (car (cdr inactive))
                     (+ (car (cdr inactive)) (cdr (cdr inactive))))
      (widget-put widget :inactive nil))))

(defun edraw-widget-attribute-list-attribute-tag (widget)
  "Return the first :tag property in WIDGET or one of its children."
  ;; Derived from `custom-face-edit-attribute-tag'
  (let ((tag (widget-get widget :tag)))
    (or (and (not (equal tag "")) tag)
        (let ((children (widget-get widget :children)))
          (while (and (null tag) children)
            (setq tag (edraw-widget-attribute-list-attribute-tag (pop children))))
          tag))))



;;;; `edraw-attribute-plist' Widget

;; Example:
;; (progn
;;   (pop-to-buffer (generate-new-buffer "*Widget Example*"))
;;   (widget-create `(edraw-attribute-plist
;;                    :tag "Props"
;;                    :notify
;;                    ,(lambda (w &rest _) (message "%s" (message "%s" (prin1-to-string (widget-value w)))))
;;                    :greedy t ;; Use attrs after mismatched
;;                    :value (stroke-width 123.45 stroke "red" unknown "uval" fill "green" )
;;                    (fill
;;                     (string :tag "Fill"
;;                             :help-echo "Fill color"))
;;                    (stroke
;;                     (string :tag "Stroke"
;;                             :help-echo "Stroke color"))
;;                    (stroke-width
;;                     (number :tag "Stroke Width"
;;                             :help-echo "Stroke width"))))
;;   (use-local-map widget-keymap)
;;   (widget-setup))

(defun edraw-widget-attribute-plist-args (attributes)
  (cl-loop for (key type) in attributes
           collect `(group :inline t
                           :format "%v"
                           :sibling-args ,(widget-get type :sibling-args)
                           (const :format "" :value ,key)
                           ,type)))

(define-widget 'edraw-attribute-plist 'edraw-attribute-list
  ""
  :convert-widget 'edraw-widget-attribute-plist-convert)

(defun edraw-widget-attribute-plist-convert (widget)
  (widget-put
   widget
   :args (edraw-widget-attribute-plist-args (widget-get widget :args)))
  widget)



;;;; `edraw-attribute-alist' Widget

;; Example:
;; (progn
;;   (pop-to-buffer (generate-new-buffer "*Widget Example*"))
;;   (widget-create 'edraw-attribute-alist
;;                  :tag "Props"
;;                  :format "%v"
;;                  :notify
;;                  (lambda (w &rest _) (message "%s" (prin1-to-string (widget-value w))))
;;                  :greedy t ;; Use attrs after mismatched
;;                  :value '((stroke-width . 123.45) (stroke . "red") (unknown . "uval") (fill . "green"))
;;                  '(fill
;;                    (string :tag "Fill"
;;                            :help-echo "Fill color"))
;;                  '(stroke
;;                    (string :tag "Stroke"
;;                            :help-echo "Stroke color"))
;;                  '(stroke-width
;;                    (number :tag "Stroke Width"
;;                            :help-echo "Stroke width")))
;;   (use-local-map widget-keymap)
;;   (widget-setup))

(defun edraw-widget-attribute-alist-args (attributes)
  (cl-loop for (key type) in attributes
           collect
           `(cons :inline nil
                  :tag nil
                  :format "%v"
                  :sibling-args ,(widget-get type :sibling-args)
                  :value ,(cons key (widget-get type :value))
                  (const :format "" :value ,key)
                  ,type)))

(define-widget 'edraw-attribute-alist 'edraw-attribute-list
  ""
  :convert-widget 'edraw-widget-attribute-alist-convert)

(defun edraw-widget-attribute-alist-convert (widget)
  (widget-put
   widget
   :args (edraw-widget-attribute-alist-args (widget-get widget :args)))
  widget)



;;;; `edraw-properties' Widget

;; Example:
;; (progn
;;   (pop-to-buffer (generate-new-buffer "*Widget Example*"))
;;   (widget-create '(edraw-properties
;;                    :tag "Rect"
;;                    :value ((rx . 10) (ry . 20) (fill . "green"))
;;                    :svg-tag rect))
;;   (widget-insert "\n\n")
;;   (use-local-map widget-keymap)
;;   (widget-setup))

;; Example2:
;; (progn
;;   (pop-to-buffer (generate-new-buffer "*Widget Example*"))
;;   (widget-create (edraw-widget-properties
;;                   (edraw-svg-tag-get-property-info-list 'rect)
;;                   :value '((rx . 10) (ry . 20) (fill . "green"))))
;;   (widget-insert "\n\n")
;;   (use-local-map widget-keymap)
;;   (widget-setup))

(define-widget 'edraw-properties 'edraw-attribute-alist
  ""
  :format "%t\n%v"
  :greedy t
  :convert-widget 'edraw-widget-properties-convert-args)

(defun edraw-widget-properties-convert-args (widget)
  (widget-put
   widget
   :args (edraw-widget-properties-args
          (or (when-let ((svg-tag (widget-get widget :svg-tag)))
                (edraw-svg-tag-get-property-info-list svg-tag))
              (widget-get widget :prop-info-list)
              (car (widget-get widget :args)))))
  widget)

(defun edraw-widget-properties (prop-info-list &rest args)
  `(edraw-attribute-alist
    ,@args
    :format "%t\n%v"
    :args ,(edraw-widget-properties-args prop-info-list)))

(defun edraw-widget-properties-args (prop-info-list)
  (delq nil
        (mapcar #'edraw-widget-properties-prop-field
                prop-info-list)))

(defun edraw-widget-properties-prop-field (prop-info)
  (unless (edraw-svg-prop-info-required-p prop-info)
    (let* ((name (edraw-svg-prop-info-name prop-info))
           (tag (capitalize (symbol-name name)))
           (type (edraw-svg-prop-info-type prop-info))
           (number-p (edraw-svg-prop-info-number-p prop-info)))
      (cond
       ;; Number
       (number-p
        (list name (list 'number :tag tag)))
       ;; String
       ((or (eq type 'string)
            (eq type 'text) ;;@todo text widget?
            (eq type 'font-family))
        (list name (list 'string :tag tag)))
       ;; Paint
       ((eq type 'paint)
        (list name (list 'edraw-web-color :tag tag)))
       ;; Choice
       ((and (listp type) (eq (car type) 'or))
        (list name (append
                    (list 'menu-choice :tag tag)
                    (mapcar (lambda (val) (list 'const val)) (cdr type)))))
       ;; Marker?
       ;;@todo Support marker
       ))))



(provide 'edraw-widget)
;;; edraw-widget.el ends here
