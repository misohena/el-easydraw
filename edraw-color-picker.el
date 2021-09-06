;;; edraw-color-picker.el --- Color Picker           -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Color Picker

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

;; Color picker functions implemented by SVG and their applications.

;; Show color picker in minibuffer:
;; - (edraw-color-picker-read-color) (<= eval here !!)

;; Insert the selected color into the buffer:
;; - (edraw-color-picker-insert-color)
;; - (edraw-color-picker-replace-color-at-point)

;; A function that opens a color picker near the point:
;; - edraw-color-picker-open-near-point

;; A function that displays a color picker using an overlay:
;; - edraw-color-picker-overlay

;; The core class of the color picker:
;; - edraw-color-picker

;; options:
;; :ok
;;    function : callback (funcall f picker)
;; :cancel
;;    function : callback (funcall f picker)
;; :enable-opacity
;;    boolean
;; :enable-recent-colors
;;    boolean
;; :color-float-format (default: 4)
;;    integer : Number of digits after the decimal point
;;    string : format string (e.g. "%s")
;;    function : format function (funcall f value)
;; :color-format
;;    symbol
;;      nil : hex or rgb
;;      'hex : #RRGGBB or #RRGGBBAA
;;      'rgba : rgba(R, G, B, A)
;; :color-name-scheme
;;    symbol
;;      'emacs : Use emacs color name
;;      'web : Use web color keyword
;; :scale
;;    float : scaling factor for color picker image

(require 'eieio)
(require 'svg)
(require 'edraw-color)
(require 'edraw-util)

;;; Code:

(defconst edraw-color-picker-font-family "Arial")
(defconst edraw-color-picker-font-size 14)
(defconst edraw-color-picker-font-acent 0.85)

(defvar edraw-color-picker-recent-colors-max-size 32)
(defvar edraw-color-picker-recent-colors
  (list
   "#000000ff"
   "#0000ffff"
   "#00ff00ff"
   "#00ffffff"
   "#ff0000ff"
   "#ff00ffff"
   "#ffff00ff"
   "#ffffffff"
   "#00000080"
   "#0000ff80"
   "#00ff0080"
   "#00ffff80"
   "#ff000080"
   "#ff00ff80"
   "#ffff0080"
   "#ffffff80"))


;;;; Recent Colors

(defun edraw-color-picker-add-to-recent-colors (color)
  ;;@todo validate color more
  (let ((color (if (stringp color)
                   color
                 (edraw-to-string color))))
    (setq edraw-color-picker-recent-colors
          (cons
           color
           (seq-remove (lambda (c)
                         (string=
                          (edraw-to-string (edraw-color-from-string c))
                          color))
                       edraw-color-picker-recent-colors)))
    (setq edraw-color-picker-recent-colors
          (seq-take edraw-color-picker-recent-colors
                    edraw-color-picker-recent-colors-max-size))
    ;;@todo save to file
    ))


;;;; SVG Common


(defun edraw-color-picker-rect (x y w h fill &optional stroke &rest attrs)
  (dom-node 'rect
            `((x . ,x) (y . ,y) (width . ,w) (height . ,h)
              ,@(if fill `((fill . ,fill)))
              (stroke . ,(or stroke "none"))
              ,@attrs)))

(defconst edraw-color-picker-transparent-bg-grid-size 8)
(defconst edraw-color-picker-transparent-bg-color1 "#ffffff")
(defconst edraw-color-picker-transparent-bg-color2 "#cccccc")

(defun edraw-color-picker-transparent-bg-pattern ()
  (let ((grid-size edraw-color-picker-transparent-bg-grid-size)
        (color2 edraw-color-picker-transparent-bg-color2))
    (dom-node
     'pattern
     `((id . "edraw-cp-transparent-bg")
       (x . 0) (y . 0) (width . ,(* 2 grid-size)) (height . ,(* 2 grid-size))
       (patternUnits . "userSpaceOnUse"))
     (edraw-color-picker-rect grid-size 0 grid-size grid-size color2)
     (edraw-color-picker-rect 0 grid-size grid-size grid-size color2))))

(defun edraw-color-picker-linear-gradient (id dx dy colors)
  (apply
   'dom-node
   'linearGradient
   `((id . ,id)
     (x1 . ,(if (< dx 0) 1 0))
     (y1 . ,(if (< dy 0) 1 0))
     (x2 . ,(if (< dx 0) (+ 1 dx) dx))
     (y2 . ,(if (< dy 0) (+ 1 dy) dy)))
   (seq-map-indexed
    (lambda (color index)
      (dom-node 'stop `((stop-color . ,(edraw-to-string color))
                        (offset . ,(/ (float index) (1- (length colors)))))))
    colors)))


;;;; Area


(defun edraw-color-picker-area-type-p (obj)
  "Return non-nil, if OBJ is an object of type
`edraw-color-picker-area' or one of its derived classes."
  (cl-typep obj 'edraw-color-picker-area))

(defclass edraw-color-picker-area ()
  ((name :initarg :name)
   (spacing :initarg :spacing :initform 0)
   (width :initarg :width)
   (height :initarg :height)
   (left :initarg :left)
   (top :initarg :top)
   (create-element :initarg :create-element :initform nil)
   (on-mouse :initarg :on-mouse :initform nil)
   (on-click :initarg :on-click :initform nil)
   ))

(cl-defmethod edraw-contains-point-p ((area edraw-color-picker-area) xy)
  (with-slots (left top width height) area
    (and
     (<= left (car xy) (+ left width))
     (<= top (cdr xy) (+ top height)))))

(cl-defmethod edraw-dispatch-mouse-xy ((area edraw-color-picker-area) xy)
  (with-slots (left top on-mouse) area
    (when on-mouse
      (funcall on-mouse area
               (cons
                (- (car xy) left)
                (- (cdr xy) top))))))

(cl-defmethod edraw-dispatch-click ((area edraw-color-picker-area))
  (with-slots (on-click) area
    (when on-click
      (funcall on-click area))))

(cl-defmethod edraw-create-element ((area edraw-color-picker-area))
  (with-slots (create-element) area
    (when create-element
      (funcall create-element area))))

;;;;; Area - Button

(defclass edraw-color-picker-area-button (edraw-color-picker-area)
  ((text :initarg :text)))

(cl-defmethod edraw-create-element ((area edraw-color-picker-area-button))
  (with-slots (left top width height text) area
    (dom-node
     'g nil
     (edraw-color-picker-rect
      left top width height "#ccc" "#222"
      (cons 'rx 2)
      (cons 'ry 2))
     (dom-node
      'text
      `((font-family . ,edraw-color-picker-font-family)
        (font-size . ,edraw-color-picker-font-size)
        (x . ,(+ left (* 0.5 width)))
        (y . ,(+ top
                 (* 0.5 height)
                 (* (- edraw-color-picker-font-acent 0.5)
                    edraw-color-picker-font-size)))
        (text-anchor . "middle")
        (fill . "#222")
        (stroke . "none"))
      text))))

;;;;; Area - Colored

(defclass edraw-color-picker-area-colored (edraw-color-picker-area)
  ((target-value :initarg :target-value)
   (on-value-change :initarg :on-value-change :initform nil)
   (get-current-color :initarg :get-current-color :initform nil)
   (defs)
   (cursor :initform nil)
   (gradient-colors :initarg :gradient-colors)
   (gradient-element :initform nil)
   (update-gradient :initarg :update-gradient :initform nil)
  ))

(cl-defmethod edraw-link-value ((area edraw-color-picker-area-colored))
  (with-slots (target-value on-value-change) area
    (when (and target-value on-value-change)
      (edraw-add-hook
       target-value
       (lambda ()
         (funcall on-value-change area))))))

(cl-defmethod edraw-get-value ((area edraw-color-picker-area-colored))
  (with-slots (target-value) area
    (when target-value
      (edraw-get-value target-value))))

(cl-defmethod edraw-set-value ((area edraw-color-picker-area-colored) new-value)
  (with-slots (target-value) area
    (when target-value
      (edraw-set-value target-value new-value))))


(cl-defmethod edraw-link-gradient-colors ((area edraw-color-picker-area-colored))
  (with-slots (gradient-colors update-gradient) area
    (when (and gradient-colors update-gradient)
      (edraw-add-hook
       gradient-colors
       (lambda ()
         (edraw-update-gradient area))))))

(cl-defmethod edraw-update-gradient ((area edraw-color-picker-area-colored))
  (with-slots (update-gradient) area
    (when update-gradient
      (funcall update-gradient area))))

(cl-defmethod edraw-get-current-color ((area edraw-color-picker-area-colored))
  (with-slots (get-current-color) area
    (when get-current-color
      (funcall get-current-color area))))

;;;;; Area - Preview

(defun edraw-color-picker-area-preview (name &rest args)
  (apply
   'edraw-color-picker-area-colored
   :name name
   :width 56
   :height 32
   :create-element
   (lambda (this)
     (edraw-link-value this)
     (with-slots (left top width height gradient-element) this
       (dom-node
        'g nil
        (edraw-color-picker-rect
         left top width height "#ffffff")
        (edraw-color-picker-rect
         left top width height
         "url(#edraw-cp-transparent-bg)")
        (setq gradient-element
              (edraw-color-picker-rect
               (+ 0.5 left)
               (+ 0.5 top)
               width height
               (edraw-to-string (edraw-get-current-color this))
               "#000000")))))
   :get-current-color
   (lambda (this)
     (edraw-get-value this))
   :on-value-change
   (lambda (this)
     (with-slots (gradient-element) this
       (dom-set-attribute
        gradient-element
        'fill
        (edraw-to-string (edraw-get-current-color this)))))
   args))

;;;;; Area - Palette Entry

(defun edraw-color-picker-area-palette (name &rest args)
  (apply
   'edraw-color-picker-area-colored
   :name name
   ;;:width 40
   ;;:height 30
   :create-element
   (lambda (this)
     (edraw-link-value this)
     (with-slots (left top width height) this
       (dom-node
        'g nil
        (edraw-color-picker-rect
         left top width height "#ffffff")
        (edraw-color-picker-rect
         left top width height
         "url(#edraw-cp-transparent-bg)")
        (edraw-color-picker-rect
         (+ 0.5 left)
         (+ 0.5 top)
         width height
         (edraw-to-string (edraw-get-current-color this))
         "#000000"))))
   :get-current-color
   (lambda (this)
     ;;@todo remove bad gradient-colors usage
     (with-slots (gradient-colors) this
       (car gradient-colors)))
   :on-mouse
   (lambda (this _xy)
     (edraw-set-value this (edraw-get-current-color this)))
   args))

;;;;; Area - 2D Area

(defun edraw-color-picker-area-2d (name &rest args)
  (apply
   'edraw-color-picker-area-colored
   :name name
   :width 255
   :height 255
   :create-element
   (lambda (this)
     (edraw-link-value this)
     (edraw-link-gradient-colors this)
     (with-slots (left top width height defs cursor) this
       (let ((node
              (dom-node
               'g nil
               (setq defs
                     (dom-node
                      'defs
                      nil
                      (dom-node 'clipPath
                                `((id . ,(format "edraw-cp-%s-clip" name)))
                                (edraw-color-picker-rect
                                 left top width height nil))))
               (edraw-color-picker-rect
                left top width height (format "url(#edraw-cp-%s-h)" name))
               (edraw-color-picker-rect
                left top width height (format "url(#edraw-cp-%s-v)" name))
               (setq cursor
                     (let ((value (edraw-get-value this)))
                       (edraw-color-picker-2d-cursor
                        name
                        left top width height (car value) (cdr value)
                        (> (edraw-luminance (edraw-get-current-color this)) 0.7)))))))
         (edraw-update-gradient this)
         node)))
   :get-current-color
   (lambda (this)
     (with-slots (gradient-colors) this
       (let ((value (edraw-get-value this)))
         (edraw-color-from-2d-gradient
          (car value) (cdr value)
          (car (edraw-get-value gradient-colors))
          (cdr (edraw-get-value gradient-colors))))))
   :update-gradient
   (lambda (this)
     (with-slots (defs gradient-element gradient-colors) this
       (when gradient-element
         (dom-remove-node defs (car gradient-element))
         (dom-remove-node defs (cdr gradient-element)))
       (setq gradient-element
             (cons (edraw-color-picker-linear-gradient
                    (format "edraw-cp-%s-h" name) 1 0
                    (car (edraw-get-value gradient-colors)))
                   (edraw-color-picker-linear-gradient
                    (format "edraw-cp-%s-v" name) 0 -1
                    (cdr (edraw-get-value gradient-colors)))))
       (dom-append-child defs (car gradient-element))
       (dom-append-child defs (cdr gradient-element))))
   :on-mouse
   (lambda (this xy)
     (with-slots (width height) this
       (edraw-set-value
        this
        (cons
         (max 0 (min 1 (/ (float (car xy)) width)))
         (max 0 (min 1 (- 1 (/ (float (cdr xy)) height))))))))
   :on-value-change
   (lambda (this)
     (with-slots (left top width height cursor) this
       (let ((value (edraw-get-value this)))
         (edraw-color-picker-2d-cursor-move
          cursor left top width height (car value) (cdr value)
          (> (edraw-luminance (edraw-get-current-color this)) 0.7)))))
   args))

(defun edraw-color-picker-2d-cursor (name x y w h value-x value-y black-p)
  (dom-node 'circle
            `((cx . ,(+ x (* w value-x)))
              (cy . ,(+ y (* h (- 1 value-y))))
              (r . 5)
              (fill . "none")
              (stroke . ,(if black-p "#000000" "#ffffff"))
              (clip-path . ,(format "url(#edraw-cp-%s-clip)" name)))))

(defun edraw-color-picker-2d-cursor-move (element x y w h value-x value-y black-p)
  (when element
    (dom-set-attribute element 'cx (+ x (* w value-x)))
    (dom-set-attribute element 'cy (+ y (* h (- 1 value-y))))
    (dom-set-attribute element 'stroke (if black-p "#000000" "#ffffff"))))

;;;;; Area - 1D Area

(defun edraw-color-picker-area-1d (name &rest args)
  (apply
   'edraw-color-picker-area-colored
   :name name
   :width 40
   :height 255
   :create-element
   (lambda (this)
     (edraw-link-value this)
     (edraw-link-gradient-colors this)
     (with-slots (left top width height defs cursor) this
       (let ((node
              (apply
               'dom-node
               'g nil
               (delq nil
                     (list
                      (setq defs (dom-node 'defs))
                      (when (string= name "opacity")
                        (edraw-color-picker-rect
                         (+ 6 left) top (- width 12) height "#ffffff"))
                      (when (string= name "opacity")
                        (edraw-color-picker-rect
                         (+ 6 left) top (- width 12) height
                         "url(#edraw-cp-transparent-bg)"))
                      (edraw-color-picker-rect
                       (+ 6 left) top (- width 12) height
                       (format "url(#edraw-cp-%s-v)" name))
                      (setq cursor
                            (edraw-color-picker-1d-cursor
                             left top width height
                             (edraw-get-value this))))))))
         (edraw-update-gradient this)
         node)))
   :get-current-color
   (lambda (this)
     (with-slots (gradient-colors) this
       (edraw-color-from-1d-gradient
        (edraw-get-value this)
        (edraw-get-value gradient-colors))))
   :update-gradient
   (lambda (this)
     (with-slots (defs gradient-element gradient-colors) this
       (when gradient-element
         (dom-remove-node defs gradient-element))
       (setq gradient-element
             (edraw-color-picker-linear-gradient
              (format "edraw-cp-%s-v" name) 0 -1
              (edraw-get-value gradient-colors)))
       (dom-append-child defs gradient-element)))
   :on-mouse
   (lambda (this xy)
     (with-slots (height) this
       (edraw-set-value this
                        (max 0 (min 1 (- 1 (/ (float (cdr xy)) height)))))))
   :on-value-change
   (lambda (this)
     (with-slots (left top width height cursor) this
       (let ((value (edraw-get-value this)))
         (edraw-color-picker-1d-cursor-move
          cursor left top width height value))))
   args))

(defun edraw-color-picker-1d-cursor (x y w h value)
  (dom-node
   'path
   `((d . ,(format
            "M6,0 l-4,-4 -2,0 0,8 2,0 4,-4Z M%s,0 l4,-4 2,0 0,8 -2,0 -4,-4Z"
            (- w 6)))
     (stroke . "#000")
     (fill . "#ccc")
     (transform . ,(format "translate(%s %s)"
                           x (+ y (* h (- 1 value))))))))

(defun edraw-color-picker-1d-cursor-move (element x y _w h value)
  (when element
    (dom-set-attribute
     element 'transform
     (format "translate(%s %s)"
             x (+ y (* h (- 1 value)))))))


;;;; Observable


(defclass edraw-color-picker-observable ()
  ((value :initarg :value)
   (hooks :initform nil)))

(cl-defmethod edraw-get-value ((value-obj edraw-color-picker-observable))
  (oref value-obj value))

(cl-defmethod edraw-set-value ((value-obj edraw-color-picker-observable)
                               new-value)
  (oset value-obj value new-value)
  (edraw-notify-change value-obj))

(cl-defmethod edraw-notify-change ((value-obj edraw-color-picker-observable))
  (dolist (fun (oref value-obj hooks))
    (funcall fun)))

(cl-defmethod edraw-add-hook ((value-obj edraw-color-picker-observable) fun)
  (with-slots (hooks) value-obj
    (setq hooks (cons fun hooks))))


;;;; Model


(defclass edraw-color-picker-model ()
  ((color-z)
   (color-xy)
   (opacity)
   (color-1)
   (color-2)
   (color-result)
   (color1d-v-colors)
   (color2d-hv-colors)
   (opacity-v-colors)
   (color-rgba-setter)
   (hooks :initform (edraw-hook-make))))

(cl-defmethod edraw-get-current-color ((model edraw-color-picker-model))
  (with-slots (color-result) model
    (edraw-get-value color-result)))

(cl-defmethod edraw-set-current-color ((model edraw-color-picker-model) color)
  (with-slots (color-rgba-setter) model
    (when (cl-typep color 'edraw-color)
      (edraw-set-value color-rgba-setter color))))

(defun edraw-color-picker-model-create (initial-color)
  (let ((model (edraw-color-picker-model)))
    (with-slots (color-z
                 color-xy
                 opacity
                 color-1
                 color-2
                 color-result
                 color1d-v-colors
                 color2d-hv-colors
                 opacity-v-colors
                 color-rgba-setter)
        model
      ;; Initialize slots
      (setq
       color-z (edraw-color-picker-observable
                :value (/ (edraw-hue initial-color) 360.0))
       color-xy (edraw-color-picker-observable
                 :value (cons (edraw-saturation initial-color)
                              (edraw-brightness initial-color)))
       opacity (edraw-color-picker-observable
                :value (oref initial-color a))

       color-1 (edraw-color-f 1 0 0) ;;from color-z color1d-v-colors
       color-2 (edraw-color-f 1 0 0) ;;from color-xy color2d-(h|v)-colors
       color-result (edraw-color-picker-observable
                     :value (edraw-color-f 1 0 0)) ;;from color-2 opacity

       color1d-v-colors (edraw-color-picker-observable
                         :value
                         (list (edraw-color-f 1 0 0)
                               (edraw-color-f 1 1 0)
                               (edraw-color-f 0 1 0)
                               (edraw-color-f 0 1 1)
                               (edraw-color-f 0 0 1)
                               (edraw-color-f 1 0 1)
                               (edraw-color-f 1 0 0)))
       color2d-hv-colors (edraw-color-picker-observable
                          :value
                          (cons
                           (list (edraw-color-f 1 1 1)
                                 color-1)
                           (list (edraw-color-f 0 0 0 1)
                                 (edraw-color-f 0 0 0 0))))
       opacity-v-colors (edraw-color-picker-observable
                         :value
                         (list (edraw-change-a color-2 0)
                               (edraw-change-a color-2 1))))

      ;; Update model from color-z, color-xy, opacity
      (edraw-update-colors model)
      (let ((update-colors (lambda () (edraw-update-colors model))))
        (edraw-add-hook color-z update-colors)
        (edraw-add-hook color-xy update-colors)
        (edraw-add-hook opacity update-colors))

      ;; Update model from color-rgba-setter
      (setq color-rgba-setter
            (edraw-color-picker-observable
             :value (edraw-color-f 0 0 0 0)))
      (let ((update-from-color-rgba-setter
             (lambda ()
               (let ((edraw-color-picker-model-suppress-change-hook t))
                 (let ((color (edraw-get-value color-rgba-setter)))
                   (edraw-set-value color-z (/ (edraw-hue color) 360.0))
                   (edraw-set-value color-xy (cons
                                              (edraw-saturation color)
                                              (edraw-brightness color)))
                   (edraw-set-value opacity (oref color a))))
               (edraw-call-hooks model))))
        (edraw-add-hook color-rgba-setter update-from-color-rgba-setter)))
    model))

(cl-defmethod edraw-update-colors ((model edraw-color-picker-model))
  (with-slots (color-z
               color-xy
               opacity
               color-1
               color-2
               color-result
               color1d-v-colors
               color2d-hv-colors
               opacity-v-colors)
      model

    (setq color-1 (edraw-color-from-1d-gradient
                   (edraw-get-value color-z)
                   (edraw-get-value color1d-v-colors)))

    (setf (elt (car (edraw-get-value color2d-hv-colors)) 1) color-1)
    (edraw-notify-change color2d-hv-colors)

    (setq color-2 (edraw-color-from-2d-gradient
                   (car (edraw-get-value color-xy))
                   (cdr (edraw-get-value color-xy))
                   (car (edraw-get-value color2d-hv-colors))
                   (cdr (edraw-get-value color2d-hv-colors))))

    (setf (elt (edraw-get-value opacity-v-colors) 0)
          (edraw-change-a color-2 0.0))
    (setf (elt (edraw-get-value opacity-v-colors) 1)
          (edraw-change-a color-2 1.0))
    (edraw-notify-change opacity-v-colors)

    (edraw-set-value color-result
                     (edraw-change-a color-2 (edraw-get-value opacity)))

    ;; (let ((message-log-max nil))
    ;;   (message "%s" (edraw-to-string (edraw-get-value color-result))))
    (unless edraw-color-picker-model-suppress-change-hook
      (edraw-call-hooks model))))

(cl-defmethod edraw-call-hooks ((model edraw-color-picker-model))
  (with-slots (hooks) model
    (edraw-hook-call hooks)))

(defvar edraw-color-picker-model-suppress-change-hook nil)

(cl-defmethod edraw-add-hook ((model edraw-color-picker-model) function &rest args)
  (with-slots (hooks) model
    (apply 'edraw-hook-add hooks function args)))


;;;; Areas


(defun edraw-color-picker-areas-create (model padding-left padding-top options)
  (list
   `(move-dx ,padding-left)
   `(move-dy ,padding-top)

   ;; Main Line
   (edraw-color-picker-area-2d
    "color2d"
    :spacing 0
    :target-value (oref model color-xy)
    :gradient-colors (oref model color2d-hv-colors))
   (edraw-color-picker-area-1d
    "color1d"
    :spacing 8
    :target-value (oref model color-z)
    :gradient-colors (oref model color1d-v-colors))

   (when (alist-get :enable-opacity options t)
     (edraw-color-picker-area-1d
      "opacity"
      :spacing 4
      :target-value (oref model opacity)
      :gradient-colors (oref model opacity-v-colors)))

   ;; Right Bar
   '(move-dx 12)
   '(flow-dir down)

   (edraw-color-picker-area-preview
    "Preview"
    :target-value (oref model color-result))

   (edraw-color-picker-area-button
    :name "ok"
    :spacing 8
    :width (* 4 edraw-color-picker-font-size)
    :height 24
    :text "OK")
   (edraw-color-picker-area-button
    :name "cancel"
    :spacing 8
    :width (* 4 edraw-color-picker-font-size)
    :height 24
    :text "Cancel")
   '(flow-dir right)

   ;; Recent Colors
   'move-to-left
   'move-to-top
   `(move-dx ,padding-left)
   `(move-dy ,padding-top)
   `(move-dy 256)
   '(move-dy 14)
   (when (and (alist-get :enable-recent-colors options t)
              edraw-color-picker-recent-colors)
     (list 'element
           (lambda (x y _left _top _right _bottom)
             (dom-node 'text
                       `((x . ,x)
                         (y . ,(- y 3))
                         (font-family . edraw-color-picker-font-family)
                         (font-size . 10)
                         (fill . "#ccc")
                         (stroke . "none"))
                       "Recent Colors"))))
   (when (alist-get :enable-recent-colors options t)
     (list 'generate
           (lambda (x _y _left _top right _bottom)
             (let* ((spacing 2)
                    (w 24)
                    (h 20)
                    (num-entries (floor (/ (- right x) (+ w spacing)))))
               (cl-loop for color in edraw-color-picker-recent-colors
                        for i from 0 to (1- num-entries)
                        collect
                        (edraw-color-picker-area-palette
                         (format "Recent%s" i)
                         :spacing spacing
                         :width w
                         :height h
                         ;;@todo remove bad gradient-colors usage
                         :gradient-colors (list (edraw-color-from-string color))
                         :target-value (oref model color-rgba-setter)))))))))

(defun edraw-color-picker-areas-layout (spec-list)
  (let* ((left 0)
         (top 0)
         (right left)
         (bottom top)
         (x left)
         (y top)
         (flow-dir 'right)
         areas)
    (while spec-list
      (let ((spec (car spec-list)))
        (setq spec-list (cdr spec-list))
        (pcase spec
          ('return (setq x left)
                   (setq y bottom))
          ('move-to-left (setq x left))
          ('move-to-top (setq y top))
          (`(move-dx ,dx) (setq x (+ x dx)))
          (`(move-dy ,dy) (setq y (+ y dy)))
          (`(flow-dir ,dir) (setq flow-dir dir))
          (`(generate ,fun)
           (setq spec-list
                 (append (funcall fun x y left top right bottom)
                         spec-list))
           )
          (`(element ,fun)
           (push (list 'element
                       (funcall fun x y left top right bottom))
                 areas))
          ((and (pred edraw-color-picker-area-type-p)
                area)
           (pcase flow-dir
             ('right (setq x (+ x (oref area spacing))))
             ('down (setq y (+ y (oref area spacing))))
             ('left (setq x (- x (oref area spacing) (oref area width))))
             ('up (setq y (- y (oref area spacing) (oref area height)))))
           (oset area left x)
           (oset area top y)
           (setq right (max right (+ x (oref area width))))
           (setq bottom (max bottom (+ y (oref area height))))
           (pcase flow-dir
             ('right (setq x (+ x (oref area width))))
             ('down (setq y (+ y (oref area height)))))
           (push area areas)))))
    (list
     (nreverse areas)
     right
     bottom)))

(defun edraw-color-picker-areas-create-element (areas)
  (mapcar
   (lambda (area)
     (pcase area
       ((pred edraw-color-picker-area-type-p)
        (edraw-create-element area))
       ;;SVG element
       (`(element ,element)
        element)))
   areas))

(defun edraw-color-picker-areas-find-by-xy (areas xy)
  (seq-find (lambda (area)
              (when (edraw-color-picker-area-type-p area)
                (edraw-contains-point-p area xy)))
            areas))

(defun edraw-color-picker-areas-find-by-name (areas name)
  (seq-find (lambda (area)
              (when (edraw-color-picker-area-type-p area)
                (equal (oref area name) name)))
            areas))

(defun edraw-color-picker-areas-click-by-name (areas name)
  (when-let ((area (edraw-color-picker-areas-find-by-name areas name)))
    (edraw-dispatch-click area)))

(defun edraw-color-picker-areas-on-down-mouse-1 (areas down-event image-scale updator)
  (when-let ((down-xy (edraw-color-picker-mouse-to-xy down-event image-scale))
             (area (edraw-color-picker-areas-find-by-xy areas down-xy)))
    (edraw-dispatch-mouse-xy area down-xy)
    (funcall updator)

    (let ((inside-p t))
      (edraw-track-dragging
       down-event
       (lambda (move-event)
         (let ((move-xy (edraw-color-picker-mouse-to-xy move-event image-scale)))
           (unless (edraw-contains-point-p area move-xy)
             (setq inside-p nil))
           (edraw-dispatch-mouse-xy area move-xy)
           (funcall updator))))
      (when inside-p
        (edraw-dispatch-click area)))))


;;;; Mouse Event


(defun edraw-color-picker-mouse-to-xy (event image-scale)
  (let* ((xy (posn-object-x-y (event-start event)))
         (x (round (/ (car xy) image-scale)))
         (y (round (/ (cdr xy) image-scale))))
    (cons x y)))


;;;; Color Picker

(defclass edraw-color-picker ()
  ((initial-color :initarg :initial-color :initform (edraw-color-f 1 0 0 1))
   (model)
   (svg)
   (areas)
   (image-width :reader edraw-image-width)
   (image-height :reader edraw-image-height)
   (image-scale)
   (display :initarg :display)
   (hooks :initform (list
                     (cons 'color-change (edraw-hook-make))
                     (cons 'ok (edraw-hook-make))
                     (cons 'cancel (edraw-hook-make))))))

(cl-defmethod edraw-initialize ((picker edraw-color-picker)
                                &optional options)
  (let* ((initial-color (edraw-color-picker-ensure-color
                         (oref picker initial-color)
                         options))
         (model (edraw-color-picker-model-create initial-color))
         (padding 16)
         (padding-top 16)
         (padding-bottom 12)
         ;; Controls
         (layout-spec (edraw-color-picker-areas-create
                       model padding padding-top options))

         ;; Layout
         (areas-right-bottom (edraw-color-picker-areas-layout layout-spec))
         (areas (nth 0 areas-right-bottom))
         (right (nth 1 areas-right-bottom))
         (bottom (nth 2 areas-right-bottom))
         (areas-width (+ right padding))
         (areas-height (+ bottom padding-bottom))

         ;; SVG Root Element
         (image-scale (* (image-compute-scaling-factor image-scaling-factor)
                         (alist-get :scale options 1.0)))
         (image-width (ceiling (* areas-width image-scale)))
         (image-height (ceiling (* areas-height image-scale)))
         (svg (svg-create image-width image-height))
         (body (apply
                'dom-node
                'g `((transform . ,(format "scale(%s)" image-scale)))
                ;; Defs
                (dom-node 'defs nil (edraw-color-picker-transparent-bg-pattern))
                ;; Background
                (edraw-color-picker-rect 0 0 areas-width areas-height "#444")
                ;; Areas
                (edraw-color-picker-areas-create-element areas))))
    (dom-append-child svg body)

    (oset picker initial-color initial-color)
    (oset picker model model)
    (oset picker svg svg)
    (oset picker areas areas)
    (oset picker image-width image-width)
    (oset picker image-height image-height)
    (oset picker image-scale image-scale)

    ;; Setup event routing
    (when-let ((button (edraw-color-picker-areas-find-by-name areas "ok")))
      (oset button on-click
            (lambda (_area)
              ;; Add color to recent-colors
              (when (alist-get :enable-recent-colors options t)
                (edraw-color-picker-add-to-recent-colors
                 (edraw-get-current-color picker)))
              ;; Callback
              (when-let ((fun (alist-get :ok options)))
                (funcall fun picker))
              (edraw-hook-call (alist-get 'ok (oref picker hooks)) picker))))
    (when-let ((button (edraw-color-picker-areas-find-by-name areas "cancel")))
      (oset button on-click
            (lambda (_area)
              ;; Callback
              (when-let ((fun (alist-get :cancel options)))
                (funcall fun picker))
              (edraw-hook-call (alist-get 'cancel (oref picker hooks)) picker))))
    (edraw-add-hook
     model
     (lambda ()
       (edraw-hook-call (alist-get 'color-change (oref picker hooks)) picker)))))

(cl-defmethod edraw-add-hook ((picker edraw-color-picker) hook-type
                              function &rest args)
  (with-slots (hooks) picker
    (when-let ((hook (alist-get hook-type hooks)))
      (apply 'edraw-hook-add hook function args))))

(cl-defmethod edraw-close ((picker edraw-color-picker))
  (with-slots (display) picker
    (when display
      (edraw-close display))))

(cl-defmethod edraw-get-current-color ((picker edraw-color-picker))
  (edraw-get-current-color (oref picker model)))

(cl-defmethod edraw-set-current-color ((picker edraw-color-picker) color)
  (edraw-set-current-color (oref picker model) color)
  (edraw-update (oref picker display) picker))

(cl-defmethod edraw-get-image ((picker edraw-color-picker))
  (svg-image (oref picker svg)))

(cl-defmethod edraw-on-down-mouse-1 ((picker edraw-color-picker) down-event)
  (with-slots (areas image-scale display) picker
    (edraw-color-picker-areas-on-down-mouse-1
     areas down-event image-scale
     (lambda () (edraw-update display picker)))))

(cl-defmethod edraw-click-area ((picker edraw-color-picker) name)
  (with-slots (areas) picker
    (edraw-color-picker-areas-click-by-name areas name)))


;;;; Overlay Display


(defun edraw-color-picker-overlay
    (overlay-or-args-props target-property &optional initial-color options)
  "Create a color picker that display using an overlay.

OVERLAY-OR-ARGS-PROPS : An overlay object or a list of arguments
to create an overlay. The first five elements of the list are
arguments to make-overlay. The rest is a plist to pass to
overlay-put.

TARGET-PROPERTY : Overlay property to set the color-picker image.
Specify one of 'display, 'before-string, or 'after-string."

  (let* ((display (edraw-color-picker-display-overlay
                   :overlay (edraw-color-picker-make-overlay
                             overlay-or-args-props)
                   :target-property (or target-property 'display)
                   :keymap (make-sparse-keymap)))
         (picker (edraw-color-picker
                  :initial-color initial-color
                  :display display)))
    (edraw-initialize display picker)
    (edraw-initialize picker options)
    (edraw-update display picker)
    picker))

(defclass edraw-color-picker-display-overlay ()
  ((overlay :initarg :overlay)
   (target-property :initarg :target-property)
   (keymap :initarg :keymap)))

(cl-defmethod edraw-initialize ((display edraw-color-picker-display-overlay)
                                picker)
  (with-slots (overlay target-property keymap) display
    ;; Set overlay properties
    (when (eq target-property 'display)
      (overlay-put overlay 'face 'default)
      (overlay-put overlay 'keymap keymap)
      (overlay-put overlay 'pointer 'arrow))

    ;; Set mouse handler
    (define-key keymap [down-mouse-1]
      (lambda (down-event)
        (interactive "e")
        (edraw-on-down-mouse-1 picker down-event)))))

(cl-defmethod edraw-close ((display edraw-color-picker-display-overlay))
  (with-slots (overlay target-property) display
    (pcase target-property
      ('display
       (overlay-put overlay 'display nil))
      ((or 'before-string 'after-string)
       (overlay-put overlay target-property nil)))
    (delete-overlay overlay)))

(cl-defmethod edraw-update ((display edraw-color-picker-display-overlay)
                            (picker edraw-color-picker))
  (with-slots (overlay target-property keymap) display
    (pcase target-property
      ('display
       (overlay-put overlay 'display (edraw-get-image picker)))
      ((or 'before-string 'after-string)
       (overlay-put overlay target-property
                    (propertize
                     "*"
                     'display (edraw-get-image picker)
                     'face 'default
                     'keymap keymap
                     'pointer 'arrow))))))

(defun edraw-color-picker-make-overlay (overlay-or-args-props)
  "If OVERLAY-OR-ARGS-PROPS is an overlay, return it as is.

If OVERLAY-OR-ARGS-PROPS is a list, create a new overlay and
return it. The first five elements of the list are arguments to
make-overlay. The rest is a plist to pass to overlay-put."
  (cond ((overlayp overlay-or-args-props) overlay-or-args-props)
        ((listp overlay-or-args-props)
         (let ((ov (apply 'make-overlay (seq-take overlay-or-args-props 5)))
               (props (nthcdr 5 overlay-or-args-props)))
           (cl-loop for (key value) on props by 'cddr
                    do (overlay-put ov key value))
           ov))
        (t (error "Invalid overlay-or-args-props"))))

(defun edraw-color-picker-move-overlay-at-point (overlay picker)
  "Move OVERLAY above or below the current point.

OVERLAY uses the display property to display the color PICKER."
  (when-let ((pos-in-win (pos-visible-in-window-p nil nil t)))
    (when (or (< (point-min) (line-beginning-position))
              (< (line-end-position) (point-max)))
      (let* (;; y
             (image-height (edraw-image-height picker))
             (win-h (window-body-height nil t))
             (pos-y (cadr pos-in-win))
             (above-p (or (= (line-end-position) (point-max))
                          (< (- win-h pos-y) image-height)))
             ;; x
             (ave-char-width (/ (float (window-width nil t)) (window-width)))
             (pos-x (car pos-in-win))
             (win-w (window-body-width nil t))
             (image-w (edraw-image-width picker))
             (picker-left (max 0 (min (- win-w image-w)
                                      (- pos-x (/ image-w 2)))))
             (picker-left-chars (+ (window-hscroll)
                                   (floor (/ picker-left ave-char-width))))
             (picker-left-string (make-string picker-left-chars ? )))
        (move-overlay overlay
                      (if above-p
                          (1- (line-beginning-position)) (line-end-position))
                      (if above-p
                          (line-beginning-position) (1+ (line-end-position))))
        (overlay-put overlay 'before-string (concat "\n" picker-left-string))
        (overlay-put overlay 'after-string "\n")
        t))))


;;;; Applications

(defun edraw-color-picker-open-near-point (&optional initial-color options)
  (interactive)

  ;;@todo use child-frame

  (unless (assq :scale options)
    (setf (alist-get :scale options) 0.75))

  (let* ((overlay (let ((ov (make-overlay (point) (point) nil t nil)))
                    (delete-overlay ov)
                    ov))
         (picker (edraw-color-picker-overlay
                  overlay 'display initial-color options))

         (on-ok (lambda (&rest _) (edraw-close picker)))
         (on-cancel (lambda (&rest _) (edraw-close picker))))

    (edraw-color-picker-move-overlay-at-point overlay picker)
    (overlay-put overlay 'evaporate t)

    (define-key (overlay-get overlay 'keymap) (kbd "C-c C-c")
      (lambda () (interactive) (edraw-click-area picker "ok")))
    (define-key (overlay-get overlay 'keymap) (kbd "C-c C-k")
      (lambda () (interactive) (edraw-click-area picker "cancel")))
    (edraw-add-hook picker 'ok on-ok)
    (edraw-add-hook picker 'cancel on-cancel)
    (message "C-c C-c: OK, C-c C-k: Cancel")
    picker))

(defun edraw-color-picker-insert-color (&optional initial-color options)
  "Insert a color selected by color picker."
  (interactive)

  (unless (assq :color-name-scheme options)
    (setf (alist-get :color-name-scheme options) 'web))

  (let ((picker (edraw-color-picker-open-near-point initial-color options)))
    (edraw-add-hook
     picker 'ok
     (lambda (&rest _)
       (insert (edraw-color-picker-color-to-string
                (edraw-get-current-color picker)
                options)))))
  t)

(defun edraw-color-picker-replace-color-at-point (&optional options)
  "Replace the color at the point with the color selected by color picker."
  (interactive)

  (unless (assq :color-name-scheme options)
    (setf (alist-get :color-name-scheme options) 'web))

  (let ((pos (point))
        (line-begin (line-beginning-position))
        (line-end (line-end-position))
        range
        format-index) ;;index of edraw-color-string-patterns
    ;; Find color string at point
    (save-excursion
      (goto-char line-begin)
      (while (and (null range)
                  (re-search-forward edraw-color-string-patterns-re line-end t))
        (when (<= (match-beginning 0) pos (match-end 0))
          (setq range (cons
                       (match-beginning 0)
                       (match-end 0)))
          (setq format-index
                (/ (seq-position (cddr (match-data)) t (lambda (a b) (and a b)))
                   2)))))

    ;; If not found, insert a new color string
    (unless range
      (setq range (cons pos pos)))

    ;; Open color picker near the point
    (let* ((str (buffer-substring-no-properties (car range) (cdr range)))
           (initial-color (edraw-color-picker-color-from-string str options)))
      (let ((picker (edraw-color-picker-open-near-point initial-color options)))
        ;; OK
        (edraw-add-hook
         picker 'ok
         (lambda (&rest _)
           ;; Replace color string as same format
           (save-excursion
             (goto-char (car range))
             (delete-char (- (cdr range) (car range)))
             (insert
              (edraw-color-picker-color-to-string
               (edraw-get-current-color picker)
               (cons
                (cons :color-format
                      (cadr (nth format-index edraw-color-string-patterns)))
                options)))))))))
  t)

(defun edraw-color-picker-read-color (&optional
                                      prompt initial-color
                                      allow-strings options)
  "Read a color from minibuffer or color picker."
  (interactive)

  (when (eq allow-strings t)
    (setq allow-strings '(""))) ;;allow-empty

  (let* ((overlay (let ((ov (make-overlay (point) (point) nil t nil)))
                    (delete-overlay ov)
                    (overlay-put ov 'after-string "\n")
                    ov))
         (picker (edraw-color-picker-overlay
                  overlay 'before-string initial-color options))
         (buffer nil)
         (on-post-command
          (lambda ()
            (condition-case err
                (when-let ((color (edraw-color-picker-color-from-string
                                   (minibuffer-contents-no-properties)
                                   options)))
                  (edraw-set-current-color picker color))
              (error (message "err=%s" err)))))
         (on-minibuffer-setup
          (lambda ()
            (unless buffer
              (setq buffer (current-buffer))
              (move-overlay overlay (point-min) (point-min) buffer)
              (add-hook 'post-command-hook on-post-command nil t))))
         (minibuffer-setup-hook (cons on-minibuffer-setup
                                      minibuffer-setup-hook))
         (on-ok
          (lambda (_picker)
            (with-current-buffer buffer
              (exit-minibuffer))))
         (on-cancel
          (lambda (_picker)
            (with-current-buffer buffer
              (minibuffer-keyboard-quit))))
         (on-color-change
          (lambda (_picker)
            (when buffer
              (let* ((current-color
                      (edraw-get-current-color picker))
                     (current-color-str
                      (edraw-color-picker-color-to-string current-color
                                                          options))
                     (minibuffer-color
                      (edraw-color-picker-color-from-string
                       (minibuffer-contents)
                       options))
                     (minibuffer-color-str
                      (if minibuffer-color
                          (edraw-color-picker-color-to-string
                           minibuffer-color options))))
                (when (or (null minibuffer-color)
                          (not (string= current-color-str
                                        minibuffer-color-str)))
                  (with-current-buffer buffer
                    (delete-minibuffer-contents)
                    (goto-char (minibuffer-prompt-end))
                    (insert current-color-str))))))))

    (edraw-add-hook picker 'ok on-ok)
    (edraw-add-hook picker 'cancel on-cancel)
    (edraw-add-hook picker 'color-change on-color-change)

    (unwind-protect
        (let ((max-mini-window-height 1.0)
              (actual-prompt
               (or prompt
                   (format
                    "Color (%s name or %s%s): "
                    (alist-get :color-name-scheme options 'emacs)
                    (if (alist-get 'enable-opacity options t)
                        "#RGBA" "#RGB")
                    (if allow-strings
                        (concat
                         " or "
                         (mapconcat
                          (lambda (s) (if (string-empty-p s) "empty" s))
                          allow-strings
                          " or "))
                      ""))))
              (initial-input
               (cond
                ((cl-typep initial-color 'edraw-color)
                 (edraw-color-picker-color-to-string initial-color
                                                     options))
                ((stringp initial-color)
                 initial-color)
                (t
                 (edraw-color-picker-color-to-string
                  (edraw-get-current-color picker) options))))
              (result nil))
          (while (null result)
            (let ((input
                   (read-string actual-prompt initial-input)))
              (setq buffer nil) ;;minibuffer is killed
              (when (or (member input allow-strings)
                        (edraw-color-picker-color-from-string input options))
                (setq result input))))
          (when-let ((result-color
                      (edraw-color-picker-color-from-string result options)))
            ;; Avoid color name
            (edraw-color-picker-add-to-recent-colors result-color))
          result)
      (edraw-close picker)
      (delete-overlay overlay))))


;;;; Color Utility


(defun edraw-color-picker-color-to-string (color options)
  "Convert COLOR edraw-color object to string.

Valid OPTIONS are:
(:color-float-format . num-digits or format-string or format-function)
(:color-format . nil or 'hex or 'rgb)
(:enable-opacity . nil or t)
"
  (let ((edraw-color-string-float-format
         (alist-get :color-float-format options 4))
        (color (if (alist-get :enable-opacity options t)
                   color
                 (edraw-change-a color 1.0))))
    (pcase (alist-get :color-format options)
      ('nil
       (edraw-to-string color))
      ('hex
       (edraw-to-string-hex color))
      ((or 'rgb 'rgba)
       (edraw-to-string-rgba color)))))

(defun edraw-color-picker-color-from-string (string options)
  "Convert STRING to edraw-color object.

Valid options are:
(:color-name-scheme . 'emacs or 'web)
(:enable-opacity . nil or t)
"
  (let ((edraw-color-name-scheme
         (alist-get :color-name-scheme options 'emacs)) ;; or 'web
        (color (edraw-color-from-string string)))
    (if (alist-get :enable-opacity options t)
        color
      (if (and color (= (oref color a) 1.0))
          color
        nil))))

(defun edraw-color-picker-ensure-color (obj options)
  (if (cl-typep obj 'edraw-color)
      obj
    (or (if (stringp obj) (edraw-color-picker-color-from-string obj options))
        (edraw-color-ensure (car edraw-color-picker-recent-colors))
        (edraw-color-f 1 0 0 1))))


(provide 'edraw-color-picker)
;;; edraw-color-picker.el ends here
