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
;; - (edraw-color-picker-replace-color-at)
;; - (edraw-color-picker-replace-or-insert-color-at-point)

;; To use it while editing css or html:
;;   (autoload 'edraw-color-picker-replace-color-at "edraw-color-picker" nil t)
;;   (autoload 'edraw-color-picker-replace-or-insert-color-at-point "edraw-color-picker" nil t)
;;   (defun my-edraw-color-picker-enable ()
;;     ;; Replaces the color of the clicked location
;;     (local-set-key [mouse-1] #'edraw-color-picker-replace-color-at)
;;     ;; C-c C-o replaces the color in place or adds color
;;     (local-set-key (kbd "C-c C-o") #'edraw-color-picker-replace-or-insert-color-at-point))
;;   (add-hook 'css-mode-hook 'my-edraw-color-picker-enable)
;;   (add-hook 'mhtml-mode-hook 'my-edraw-color-picker-enable)


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
;; :no-color
;;    nil : disabled
;;    string : a string indicating no color (e.g. "none")
;; :enable-opacity
;;    boolean
;; :enable-palette-colors
;;    boolean
;; :enable-recent-colors
;;    boolean
;; :palette-colors
;;    edraw-list : a list of palette color strings
;; :recent-colors
;;    edraw-list : a list of recently used color strings
;; :color-float-format (default: 4)
;;    integer : number of digits after the decimal point
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
;;            (rate from default image size)
;; :scale-direct
;;    float : scaling factor for color picker image
;;            (rate from pixel size)
;; :transient-keymap-var
;;    symbol : A variable that holds the keymap used to determine the
;;             key bindings to display in the tooltip.
;;             The keymap set by `set-transient-map' (
;;             `overriding-terminal-local-map') is ignored when converting
;;             command names to key bindings, so use this in that case.

(require 'image)
(require 'eieio)
(require 'svg)
(require 'edraw-color)
(require 'edraw-util)
(require 'delsel)

;;; Code:

(defconst edraw-color-picker-font-family "Arial")
(defconst edraw-color-picker-font-size 14)
(defconst edraw-color-picker-font-acent 0.85)

(defvar edraw-color-picker-model-suppress-change-hook nil)

;;;; Customize

(defgroup edraw-color-picker nil
  "Color picker."
  :tag "Edraw Color Picker"
  :prefix "edraw-color-picker-"
  :group 'edraw)

(defcustom edraw-color-picker-use-frame-p t
  "Non-nil means use child frame to display color picker."
  :group 'edraw-color-picker
  :type 'boolean)

(defcustom edraw-color-picker-near-point-scale 1.0
  "A scaling factor when displaying the color picker near the point."
  :group 'edraw-color-picker
  :type 'float)


;;;; Model

;;;;; Observable

(defclass edraw-color-picker-observable ()
  ((change-hook :initform (edraw-hook-make))))

(cl-defmethod edraw-notify-change ((observable edraw-color-picker-observable))
  (edraw-hook-call (oref observable change-hook)))

(cl-defmethod edraw-add-change-hook ((observable edraw-color-picker-observable)
                                     fun &rest args)
  (apply 'edraw-hook-add (oref observable change-hook) fun args))

;;;;; Observable Value

(defclass edraw-color-picker-observable-value (edraw-color-picker-observable)
  ((value :initarg :value)))

(cl-defmethod edraw-get-value ((value-obj edraw-color-picker-observable-value))
  (oref value-obj value))

(cl-defmethod edraw-set-value ((value-obj edraw-color-picker-observable-value)
                               new-value)
  (oset value-obj value new-value)
  (edraw-notify-change value-obj))

;;;;; Color Picker Model

(defclass edraw-color-picker-model (edraw-color-picker-observable)
  ((color-z)
   (color-xy)
   (opacity)
   (color-1)
   (color-2)
   (color-result)
   (color1d-v-colors)
   (color2d-hv-colors)
   (opacity-v-colors)
   (color-rgba-setter)))

(cl-defmethod edraw-get-current-color ((model edraw-color-picker-model))
  (with-slots (color-result) model
    (edraw-get-value color-result)))

(cl-defmethod edraw-set-current-color ((model edraw-color-picker-model) color)
  (with-slots (color-rgba-setter color-result) model
    (when (and (cl-typep color 'edraw-color)
               (not (edraw-color-equal-p color (edraw-get-value color-result))))
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
       color-z (edraw-color-picker-observable-value
                :value (/ (edraw-hue initial-color) 360.0))
       color-xy (edraw-color-picker-observable-value
                 :value (cons (edraw-saturation initial-color)
                              (edraw-brightness initial-color)))
       opacity (edraw-color-picker-observable-value
                :value (oref initial-color a))

       color-1 (edraw-color-f 1 0 0) ;;from color-z color1d-v-colors
       color-2 (edraw-color-f 1 0 0) ;;from color-xy color2d-(h|v)-colors
       color-result (edraw-color-picker-observable-value
                     :value (edraw-color-f 1 0 0)) ;;from color-2 opacity

       color1d-v-colors (edraw-color-picker-observable-value
                         :value
                         (list (edraw-color-f 1 0 0)
                               (edraw-color-f 1 1 0)
                               (edraw-color-f 0 1 0)
                               (edraw-color-f 0 1 1)
                               (edraw-color-f 0 0 1)
                               (edraw-color-f 1 0 1)
                               (edraw-color-f 1 0 0)))
       color2d-hv-colors (edraw-color-picker-observable-value
                          :value
                          (cons
                           (list (edraw-color-f 1 1 1)
                                 color-1)
                           (list (edraw-color-f 0 0 0 1)
                                 (edraw-color-f 0 0 0 0))))
       opacity-v-colors (edraw-color-picker-observable-value
                         :value
                         (list (edraw-change-a color-2 0)
                               (edraw-change-a color-2 1))))

      ;; Update model from color-z, color-xy, opacity
      (edraw-update-colors model)
      (let ((update-colors (lambda () (edraw-update-colors model))))
        (edraw-add-change-hook color-z update-colors)
        (edraw-add-change-hook color-xy update-colors)
        (edraw-add-change-hook opacity update-colors))

      ;; Update model from color-rgba-setter
      (setq color-rgba-setter
            (edraw-color-picker-observable-value
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
               (edraw-notify-change model))))
        (edraw-add-change-hook color-rgba-setter
                               update-from-color-rgba-setter)))
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
      (edraw-notify-change model))))

;;;;; Palette Model

(defun edraw-color-picker-normalize-color-string (color)
  "Return normalized COLOR string."
  ;;@todo validate more
  (edraw-to-string
   (if (stringp color)
       (edraw-color-from-string color)
     color)))

(defclass edraw-color-picker-palette-model (edraw-color-picker-observable)
  ((options :initarg :options)
   (option-key :initarg :option-key)
   (colors-var :initarg :colors-var)
   (colors-default-var :initarg :colors-default-var)))

(cl-defmethod edraw-as-container ((palette edraw-color-picker-palette-model))
  (with-slots (options option-key colors-var) palette
    (if-let* ((option-cell (assq option-key options)))
        (cdr option-cell)
      (symbol-value colors-var))))

(cl-defmethod edraw-to-new-list ((palette edraw-color-picker-palette-model))
  (edraw-to-new-list
   (edraw-as-container palette)))

(cl-defmethod edraw-as-list ((palette edraw-color-picker-palette-model))
  (edraw-as-list
   (edraw-as-container palette)))

(cl-defmethod edraw-empty-p ((palette edraw-color-picker-palette-model))
  (edraw-empty-p (edraw-as-list palette)))

(cl-defmethod edraw-assign ((palette edraw-color-picker-palette-model)
                            colors)
  (setq colors
        (mapcar #'edraw-color-picker-normalize-color-string colors))
  (with-slots (options option-key colors-var) palette
    (if-let* ((option-cell (assq option-key options)))
        ;; Write back to the OPTIONS alist
        (setcdr option-cell (edraw-assign (cdr option-cell) colors))
      ;; or `edraw-color-picker-???-colors' variable.
      (set colors-var (edraw-assign (symbol-value colors-var) colors))))
  ;; Update Areas (SVG Elements)
  (edraw-notify-change palette)
  palette)

(cl-defmethod edraw-set-nth ((palette edraw-color-picker-palette-model)
                             n
                             color)
  (edraw-assign
   palette
   ;; @todo Expand size?
   (edraw-set-nth (edraw-to-new-list palette) n
                  (edraw-color-picker-normalize-color-string color))))

(cl-defmethod edraw-push-front-limit ((palette edraw-color-picker-palette-model)
                                      color max-size)
  (let ((color-str (edraw-color-picker-normalize-color-string color)))
    (edraw-assign
     palette
     (seq-take ;; Limit number of colors
      (cons color-str ;; Push the color to front
            ;; Remove same color
            (seq-remove (lambda (c)
                          (string=
                           (edraw-color-picker-normalize-color-string c)
                           color-str))
                        (edraw-as-list palette)))
      max-size))))

(cl-defmethod edraw-nth-color ((palette edraw-color-picker-palette-model) n)
  (when-let* ((color-str (nth n (edraw-as-list palette))))
    (edraw-color-from-string color-str)))

(cl-defmethod edraw-reset-to-default
  ((palette edraw-color-picker-palette-model))
  (edraw-assign palette (symbol-value (oref palette colors-default-var))))

(cl-defmethod edraw-write-to-file ((palette edraw-color-picker-palette-model)
                                   file)
  (with-temp-file file
    (insert "# edraw-colors\n")
    (dolist (color (edraw-as-list palette))
      (insert (edraw-color-picker-normalize-color-string color) "\n"))))

(cl-defmethod edraw-read-from-file ((palette edraw-color-picker-palette-model)
                                    file)
  (let (colors)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (unless (looking-at-p "# edraw-colors")
        (error "Not in edraw-colors format")) ;; @todo This error is barely visible
      (forward-line)
      (while (not (eobp))
        (when (looking-at
               (concat " *\\(" edraw-color-string-patterns-re "\\)"))
          (when-let* ((color (edraw-color-from-string (match-string 1))))
            (push color colors)))
        (forward-line)))
    ;; Ensure at least the same length as the default color list
    (cl-loop repeat (- (length (symbol-value (oref palette colors-default-var)))
                       (length colors))
             do (push (edraw-color-rgb 0 0 0) colors))
    (setq colors (nreverse colors))
    (edraw-assign palette colors)))

(cl-defmethod edraw-save-interactive ((palette
                                       edraw-color-picker-palette-model))
  (let ((file (read-file-name (edraw-msg "Save colors to file: ")
                              nil nil nil "palette.edraw-colors")))
    (when (or (not (file-exists-p file))
              (y-or-n-p (edraw-msg "Do you want to overwrite?")))
      (edraw-write-to-file palette file))))

(cl-defmethod edraw-load-interactive ((palette
                                       edraw-color-picker-palette-model))
  (let ((file (read-file-name (edraw-msg "Load colors from file: ") nil nil t)))
    (edraw-read-from-file palette file)))

;;;;;; Palette Colors

(defconst edraw-color-picker-palette-colors-default
  '("#000000ff"
    "#000000ff"
    ;; Pale Colors
    "#b7d4f2ff"
    "#b4e8bdff"
    "#f7b4b4ff"
    "#f1e09eff"
    "#d5bdd4ff"
    ;;"#a8e3ecff"
    ;; Vibrant Colors
    "#7baee6ff"
    "#71e584ff"
    "#f57171ff"
    "#e8c94eff"
    "#ac87aaff"
    ;;"#5bcddeff"
    ;; Gray
    "#101010ff"
    "#404040ff"
    "#c0c0c0ff"
    "#f8f8f8ff"
))

(defvar edraw-color-picker-palette-colors
  (edraw-list edraw-color-picker-palette-colors-default))

(defun edraw-color-picker-get-palette-colors-model (options)
  (edraw-color-picker-palette-model
   :options options
   :option-key :palette-colors
   :colors-var 'edraw-color-picker-palette-colors
   :colors-default-var 'edraw-color-picker-palette-colors-default))

;;;;;; Recent Colors

(defvar edraw-color-picker-recent-colors-max-size 32)

(defconst edraw-color-picker-recent-colors-default
  '("#000000ff"
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

(defvar edraw-color-picker-recent-colors
  (edraw-list edraw-color-picker-recent-colors-default))

(defun edraw-color-picker-get-recent-colors-model (options)
  (edraw-color-picker-palette-model
   :options options
   :option-key :recent-colors
   :colors-var 'edraw-color-picker-recent-colors
   :colors-default-var 'edraw-color-picker-recent-colors-default))

(defun edraw-color-picker-get-recent-colors (options)
  (edraw-as-list
   (edraw-color-picker-get-recent-colors-model options)))

(defun edraw-color-picker-add-recent-color (options color)
  (edraw-push-front-limit
   (edraw-color-picker-get-recent-colors-model options)
   color
   edraw-color-picker-recent-colors-max-size))

(defun edraw-color-picker-make-history-list (options initial-color)
  "Create a color history list for `read-from-minibuffer'."
  (let ((hist (mapcar #'edraw-color-picker-normalize-color-string
                      (edraw-color-picker-get-recent-colors options))))
    (when (or
           ;; Use car of HIST as initial-color
           (null initial-color)
           ;; Same color
           (and
            hist
            (equal (edraw-color-picker-normalize-color-string initial-color)
                   (car hist))))
      (pop hist))
    hist))


;;;; SVG Common

(defun edraw-color-picker-rect (x y w h fill &optional stroke &rest attrs)
  "Create an SVG rect element."
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
      (dom-node 'stop
                `((stop-color . ,(edraw-to-string (edraw-change-a color 1.0)))
                  (stop-opacity . ,(edraw-color-a color))
                  (offset . ,(/ (float index) (1- (length colors)))))))
    colors)))


;;;; Area

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
   (on-r-click :initarg :on-r-click :initform nil)
   (image-map-id-props :initarg :image-map-id-props :initform nil)
   (areas-info :initarg :areas-info :initform nil)
   ))

(defun edraw-color-picker-area-or-derived-p (obj)
  "Return non-nil, if OBJ is an object of type
`edraw-color-picker-area' or one of its derived classes."
  (cl-typep obj 'edraw-color-picker-area))

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

(cl-defmethod edraw-dispatch-r-click ((area edraw-color-picker-area))
  (with-slots (on-r-click) area
    (when on-r-click
      (funcall on-r-click area))))

(cl-defmethod edraw-create-element ((area edraw-color-picker-area))
  (with-slots (create-element) area
    (when create-element
      (funcall create-element area))))

(cl-defmethod edraw-invalidate-image-map ((area edraw-color-picker-area))
  "Mark that the image-map needs to be recreated."
  (with-slots (areas-info) area
    (when areas-info
      (edraw-plist-set! areas-info :invalid-image-map t))))

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

;;;;; Area - No Color Button ("none")

(defclass edraw-color-picker-area-no-color (edraw-color-picker-area)
  ())

(cl-defmethod edraw-create-element ((area edraw-color-picker-area-no-color))
  (with-slots (left top width height) area
    (dom-node
     'g nil
     (edraw-color-picker-rect
      left top width height "#000000")
     (edraw-color-picker-rect
      (+ left 0.5) (+ top 0.5) (- width 1) (- height 1) "#ffffff")
     (dom-node
      'path
      `((d .
           ,(concat "M"
                    (mapconcat
                     #'number-to-string
                     (list
                      (+ left 7) (+ top 0.5)
                      (+ left 0.5) (+ top 0.5)
                      (+ left width -7) (+ top height -0.5)
                      (+ left width -0.5) (+ top height -0.5))
                     " ")
                    "Z"))
        (stroke . "none")
        (fill . "#f00000"))))))

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
      (edraw-add-change-hook
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
      (edraw-add-change-hook
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
                        (> (edraw-relative-luminance
                            (edraw-get-current-color this))
                           0.7)))))))
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
          (> (edraw-relative-luminance (edraw-get-current-color this)) 0.7)))))
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

;;;;; Area - Palette Entry

(defclass edraw-color-picker-area-palette-entry
  (edraw-color-picker-area-colored)
  ((palette-index :initarg :palette-index)
   (palette-model :initarg :palette-model)
   (keymap-var :initarg :keymap-var)
   (select-cmd-name-fun :initarg :select-cmd-name-fun)
   (picker-model :initarg :picker-model)
   (label-format :initarg :label-format)))

(cl-defmethod initialize-instance :after
  ((area edraw-color-picker-area-palette-entry) &rest _args)
  (oset area image-map-id-props
        (list 'hot-spot (list 'pointer 'arrow)))
  (oset area on-mouse
        (lambda (this _xy)
          (edraw-set-value this (edraw-get-current-color this))))
  (oset area on-r-click
        (lambda (area &rest _)
          (edraw-on-r-click area)))
  (edraw-update-help-echo area))

(cl-defmethod edraw-create-element ((area
                                     edraw-color-picker-area-palette-entry))
  (edraw-link-value area)
  (with-slots (left top width height gradient-element) area
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
            (edraw-to-string (edraw-get-current-color area))
            "#383838")))))

(cl-defmethod edraw-get-current-color ((area
                                        edraw-color-picker-area-palette-entry))
  ;;@todo remove bad gradient-colors usage
  (car (oref area gradient-colors)))

(cl-defmethod edraw-set-current-color ((area
                                        edraw-color-picker-area-palette-entry)
                                       color)
  (with-slots (gradient-colors) area
    (when (cl-typep color 'edraw-color)
      (setcar gradient-colors color)
      (edraw-update-gradient area)
      (edraw-update-help-echo area))))

(cl-defmethod edraw-update-gradient ((area
                                      edraw-color-picker-area-palette-entry))
  (dom-set-attribute
   (oref area gradient-element)
   'fill
   (edraw-to-string (edraw-get-current-color area))))

(cl-defmethod edraw-update-help-echo ((area
                                       edraw-color-picker-area-palette-entry))
  (with-slots (image-map-id-props)
      area
    (unless image-map-id-props
      (setf image-map-id-props (list 'hot-spot nil)))
    (edraw-plist-set (cadr image-map-id-props)
                     'help-echo
                     (edraw-make-help-echo area))
    (edraw-invalidate-image-map area)))

(cl-defmethod edraw-make-help-echo ((area
                                     edraw-color-picker-area-palette-entry))
  (with-slots (palette-index keymap-var select-cmd-name-fun) area
    (concat
     ;; Color
     (edraw-to-string (edraw-get-current-color area))
     ;; Key
     (when select-cmd-name-fun
       (let ((command (funcall select-cmd-name-fun palette-index)))
         (when (and (commandp command)
                    keymap-var
                    (where-is-internal command (symbol-value keymap-var)))
           (substitute-command-keys (format " \\<%s>(\\[%s])"
                                            keymap-var command)))
         ;; ((where-is-internal command)
         ;;  (substitute-command-keys (format " (\\[%s])" command)))
         )))))

(cl-defmethod edraw-on-r-click ((area edraw-color-picker-area-palette-entry))
  ;;(message "Click %s" (oref area palette-index))
  (with-slots (picker-model palette-model palette-index label-format) area
    (when-let* ((current-color (edraw-get-current-color picker-model)))
      (edraw-popup-menu
       (format label-format (1+ palette-index))
       `(((edraw-msg "Change to Current Color")
          ,(lambda ()
             (edraw-set-nth palette-model palette-index current-color)
             (edraw-set-current-color area current-color)))
         ((edraw-msg "Palette")
          (((edraw-msg "Save...")
            ,(lambda ()
               (edraw-save-interactive palette-model)))
           ((edraw-msg "Load...")
            ,(lambda ()
               (edraw-load-interactive palette-model)))
           ((edraw-msg "Reset to Default")
            ,(lambda ()
               (when (y-or-n-p (edraw-msg "Do you want to restore the palette to its initial state?"))
                 (edraw-reset-to-default palette-model)))))))))))

(defun edraw-color-picker-palette-areas-update (palette all-areas)
  (cl-loop
   for area in all-areas
   for colors = (edraw-as-list palette) then (cdr colors)
   for color = (edraw-color-from-string (or (car colors) "#000000"))
   do (edraw-set-current-color area color)))


;;;; Areas (Layout)


(defun edraw-color-picker-areas-create (model padding-left padding-top options
                                              areas-info)
  `(
    (move-dx ,padding-left)
    (move-dy ,padding-top)

    ;; Main Line
    (flow-dir right)
    ,(edraw-color-picker-area-2d
      "color2d"
      :spacing 0
      :target-value (oref model color-xy)
      :gradient-colors (oref model color2d-hv-colors))
    ,(edraw-color-picker-area-1d
      "color1d"
      :spacing 8
      :target-value (oref model color-z)
      :gradient-colors (oref model color1d-v-colors))

    ,(when (alist-get :enable-opacity options t)
       (edraw-color-picker-area-1d
        "opacity"
        :spacing 4
        :target-value (oref model opacity)
        :gradient-colors (oref model opacity-v-colors)))

    ;; Right Bar
    (move-dx 12)
    (flow-dir down)

    ,(edraw-color-picker-area-preview
      "Preview"
      :target-value (oref model color-result))

    ,(edraw-color-picker-area-button
      :name "ok"
      :spacing 8
      :width (* 4 edraw-color-picker-font-size)
      :height 24
      :text "OK")
    ,(edraw-color-picker-area-button
      :name "cancel"
      :spacing 8
      :width (* 4 edraw-color-picker-font-size)
      :height 24
      :text "Cancel")
    ,(when (alist-get :no-color options)
       (edraw-color-picker-area-no-color
        :name "no-color"
        :spacing 32
        :width (* 4 edraw-color-picker-font-size)
        :height 24))

    ;; Palette
    ,@(when (alist-get :enable-palette options t)
        (let ((palette (edraw-color-picker-get-palette-colors-model options)))
          (unless (edraw-empty-p palette)
            `((flow-dir right)
              move-to-left
              move-to-bottom
              (move-dx ,padding-left)
              (move-dy 8)
              (generate
               ,(lambda (x _y _left _top right _bottom)
                  (edraw-color-picker-generate-palette-areas
                   x right palette
                   "Palette%s"
                   (edraw-msg "Palette Color #%d")
                   model
                   options
                   #'edraw-color-picker-select-palette-color-fname
                   areas-info)))))))

    ;; Recent Colors
    ,@(when (alist-get :enable-recent-colors options t)
        (let ((palette (edraw-color-picker-get-recent-colors-model options)))
          (unless (edraw-empty-p palette)
            `((flow-dir right)
              move-to-left
              move-to-bottom
              (move-dx ,padding-left)
              (move-dy 14)
              (element
               ,(lambda (x y _left _top _right _bottom)
                  (dom-node 'text
                            `((x . ,x)
                              (y . ,(- y 3))
                              (font-family . edraw-color-picker-font-family)
                              (font-size . 9)
                              (fill . "#888")
                              (stroke . "none"))
                            "Recent Colors")))
              (generate
               ,(lambda (x _y _left _top right _bottom)
                  (edraw-color-picker-generate-palette-areas
                   x right palette
                   "Recent%s"
                   (edraw-msg "Recent Color #%d")
                   model
                   options
                   #'edraw-color-picker-select-recent-color-fname
                   areas-info)))))))))

(defun edraw-color-picker-generate-palette-areas
    (x right palette name-format label-format model options select-cmd-name-fun
       areas-info)
  (let* ((spacing 1)
         (w 25)
         (h 20)
         (max-entries (floor (/ (+ (- right x) spacing)
                                (+ w spacing))))
         (areas
          (cl-loop for i from 0 below max-entries
                   for colors = (edraw-as-list palette) then (cdr colors)
                   for color = (or (car colors) (edraw-color-rgb 0 0 0))
                   collect
                   (edraw-color-picker-area-palette-entry
                    :areas-info areas-info
                    :name (format name-format i)
                    :spacing (if (zerop i) 0 spacing)
                    :width w
                    :height h
                    ;;@todo remove bad gradient-colors usage
                    :gradient-colors (list (edraw-color-from-string color))
                    :target-value (oref model color-rgba-setter)
                    :keymap-var (alist-get :transient-keymap-var options)
                    :select-cmd-name-fun select-cmd-name-fun
                    :palette-model palette
                    :palette-index i
                    :picker-model model
                    :label-format label-format))))

    (edraw-add-change-hook
     palette
     (lambda ()
       (edraw-color-picker-palette-areas-update palette areas)))

    areas))

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
          ('move-to-right (setq x right))
          ('move-to-top (setq y top))
          ('move-to-bottom (setq y bottom))
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
          ((and (pred edraw-color-picker-area-or-derived-p)
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
       ((pred edraw-color-picker-area-or-derived-p)
        (edraw-create-element area))
       ;;SVG element
       (`(element ,element)
        element)))
   areas))

(defun edraw-color-picker-areas-create-image-map (areas image-scale)
  (delq nil
        (mapcar
         (lambda (area)
           (when (edraw-color-picker-area-or-derived-p area)
             (when-let ((image-map-id-props (oref area image-map-id-props)))
               (let* ((x0 (oref area left))
                      (y0 (oref area top))
                      (x1 (+ x0 (oref area width)))
                      (y1 (+ y0 (oref area height))))
                 (cons
                  (cons 'rect
                        (cons (cons (round (* image-scale x0))
                                    (round (* image-scale y0)))
                              (cons (round (* image-scale x1))
                                    (round (* image-scale y1)))))
                  image-map-id-props)))))
         areas)))

(defun edraw-color-picker-areas-find-by-xy (areas xy)
  (seq-find (lambda (area)
              (when (edraw-color-picker-area-or-derived-p area)
                (edraw-contains-point-p area xy)))
            areas))

(defun edraw-color-picker-areas-find-by-name (areas name)
  (seq-find (lambda (area)
              (when (edraw-color-picker-area-or-derived-p area)
                (equal (oref area name) name)))
            areas))

(defun edraw-color-picker-areas-click-by-name (areas name)
  (when-let ((area (edraw-color-picker-areas-find-by-name areas name)))
    (edraw-dispatch-click area)))

(defun edraw-color-picker-areas-on-down-mouse (areas down-event image-scale
                                                     updator)
  (when-let* ((down-xy (edraw-color-picker-mouse-to-xy
                        down-event image-scale down-event))
              (area (edraw-color-picker-areas-find-by-xy areas down-xy)))
    (let* ((inside-p t)
           ;; Generate detailed movement events even on fringes and scrollbars
           (mouse-fine-grained-tracking t)
           (basic-type (event-basic-type down-event)))

      (when (eq basic-type 'mouse-1)
        (edraw-dispatch-mouse-xy area down-xy)
        (funcall updator))

      (edraw-track-dragging
       down-event
       (lambda (move-event)
         (let ((move-xy (edraw-color-picker-mouse-to-xy
                         move-event image-scale down-event)))
           (unless (edraw-contains-point-p area move-xy)
             (setq inside-p nil))
           (when (eq basic-type 'mouse-1)
             (edraw-dispatch-mouse-xy area move-xy)
             (funcall updator))))
       nil nil nil nil
       ;; Allow out of image
       t
       ;; Keep echo area
       t)
      (when inside-p
        (pcase basic-type
          ('mouse-1
           (edraw-dispatch-click area)
           (funcall updator))
          ('mouse-3
           (edraw-dispatch-r-click area)
           (funcall updator)))))))


;;;; Mouse Event


(defun edraw-color-picker-mouse-to-xy (move-event image-scale down-event)
  (let* ((move-pos (event-start move-event))
         (down-pos (event-start down-event))
         (xy
          (if (edraw-posn-same-object-p move-pos down-pos)
              ;; In the target image
              (posn-object-x-y move-pos)
            ;; Out of the target image
            (let ((delta-xy (edraw-posn-delta-xy-frame-to-object down-pos))
                  (xy-on-frame (edraw-posn-x-y-on-frame move-pos)))
              (if (and delta-xy xy-on-frame)
                  (cons (+ (car delta-xy) (car xy-on-frame))
                        (+ (cdr delta-xy) (cdr xy-on-frame)))
                (cons 0 0)))))
         (x (round (/ (car xy) image-scale)))
         (y (round (/ (cdr xy) image-scale))))
    (cons x y)))


;;;; Color Picker

(defun edraw-color-picker-create (uninitialized-display
                                  &optional initial-color options)
  "Create a color picker object and initialize it."
  (let ((picker (edraw-color-picker
                 :initial-color initial-color
                 :display uninitialized-display
                 :options options)))
    ;; Initialize the display object and link it to the picker object
    (edraw-initialize uninitialized-display picker)
    ;; First update
    (edraw-update picker)
    picker))

(defclass edraw-color-picker ()
  ((initial-color :initarg :initial-color :initform (edraw-color-f 1 0 0 1))
   (model)
   (svg)
   (areas)
   (areas-info)
   (image-width :reader edraw-image-width)
   (image-height :reader edraw-image-height)
   (image-scale)
   (image-map)
   (options :initarg :options :reader edraw-options)
   (display :initarg :display :reader edraw-get-display)
   (hooks :initform (list
                     (cons 'closed (edraw-hook-make))
                     (cons 'color-change (edraw-hook-make))
                     (cons 'ok (edraw-hook-make))
                     (cons 'cancel (edraw-hook-make))
                     (cons 'no-color (edraw-hook-make))))))

(cl-defmethod initialize-instance :after ((picker edraw-color-picker)
                                          &rest _args)
  (let* ((options (oref picker options))
         (initial-color (edraw-color-picker-ensure-color
                         (oref picker initial-color)
                         options))
         (model (edraw-color-picker-model-create initial-color))
         (padding 12)
         (padding-top 12)
         (padding-bottom 12)
         ;; Controls
         (areas-info (list :invalid-image-map t))
         (layout-spec (edraw-color-picker-areas-create
                       model padding padding-top options areas-info))

         ;; Layout
         (areas-right-bottom (edraw-color-picker-areas-layout layout-spec))
         (areas (nth 0 areas-right-bottom))
         (right (nth 1 areas-right-bottom))
         (bottom (nth 2 areas-right-bottom))
         (areas-width (+ right padding))
         (areas-height (+ bottom padding-bottom))

         ;; SVG Root Element
         (image-scale
          (alist-get :scale-direct options
                     (* (image-compute-scaling-factor image-scaling-factor)
                        (alist-get :scale options 1.0))))
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
    (oset picker areas-info areas-info)
    (oset picker image-width image-width)
    (oset picker image-height image-height)
    (oset picker image-scale image-scale)
    (oset picker image-map
          (edraw-color-picker-areas-create-image-map areas image-scale))

    ;; Setup event routing
    (when-let ((button (edraw-color-picker-areas-find-by-name areas "ok")))
      (oset button on-click
            (lambda (_area)
              ;; Add color to recent-colors
              (when (alist-get :enable-recent-colors options t)
                (edraw-color-picker-add-recent-color
                 options
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
    (when-let ((button (edraw-color-picker-areas-find-by-name areas "no-color")))
      (oset button on-click
            (lambda (_area)
              ;; Callback
              (edraw-hook-call (alist-get 'no-color (oref picker hooks)) picker))))
    (edraw-add-change-hook
     model
     (lambda ()
       (edraw-hook-call (alist-get 'color-change (oref picker hooks)) picker)))))

(cl-defmethod edraw-add-hook ((picker edraw-color-picker) hook-type
                              function &rest args)
  (with-slots (hooks) picker
    (when-let ((hook (alist-get hook-type hooks)))
      (apply 'edraw-hook-add hook function args))))

(cl-defmethod edraw-closed-p ((picker edraw-color-picker))
  (with-slots (display) picker
    (when display
      (edraw-closed-p display))))

(cl-defmethod edraw-close ((picker edraw-color-picker))
  (with-slots (display) picker
    (when display
      (unless (edraw-closed-p display)
        (edraw-close display)
        (edraw-hook-call (alist-get 'closed (oref picker hooks)) picker)))))

(cl-defmethod edraw-update ((picker edraw-color-picker))
  (with-slots (display) picker
    (when display
      (edraw-update display))))

(cl-defmethod edraw-get-current-color ((picker edraw-color-picker))
  (edraw-get-current-color (oref picker model)))

(cl-defmethod edraw-set-current-color ((picker edraw-color-picker) color)
  (edraw-set-current-color (oref picker model) color)
  (edraw-update picker))

(cl-defmethod edraw-get-image ((picker edraw-color-picker))
  (with-slots (areas-info image-map areas image-scale svg) picker
    ;; Update image-map if invalid
    (when (plist-get areas-info :invalid-image-map)
      (setq image-map
            (edraw-color-picker-areas-create-image-map areas image-scale))
      (edraw-plist-set! areas-info :invalid-image-map nil))
    ;; Create image
    (svg-image svg
               :scale 1.0 ;;Cancel image-scale effect
               :map image-map)))

(cl-defmethod edraw-on-down-mouse ((picker edraw-color-picker) down-event)
  (with-slots (areas image-scale) picker
    (edraw-color-picker-areas-on-down-mouse
     areas down-event image-scale
     (lambda () (edraw-update picker)))))

(cl-defmethod edraw-click-area ((picker edraw-color-picker) name)
  (with-slots (areas) picker
    (edraw-color-picker-areas-click-by-name areas name)))

(cl-defmethod edraw-buffer ((picker edraw-color-picker))
  (edraw-buffer (oref picker display)))

;;;;; Palette

;;;;;; Palette

(cl-defmethod edraw-select-palette-color ((picker edraw-color-picker) index)
  (when-let* ((color (edraw-nth-color
                      (edraw-color-picker-get-palette-colors-model
                       (oref picker options))
                      index)))
    (edraw-set-current-color picker color)))

(defun edraw-color-picker-select-palette-color (index)
  (interactive "p")
  (when-let ((picker (edraw-color-picker-at-input last-command-event)))
    (edraw-select-palette-color picker index)))

(defun edraw-color-picker-select-palette-color-fname (i)
  (intern (format "edraw-color-picker-select-palette-color-%d" i)))

;; Define select-palette-color-<n> commands
(dotimes (i 10)
  (defalias (edraw-color-picker-select-palette-color-fname i)
    (lambda ()
      (interactive)
      (edraw-color-picker-select-palette-color i))))

(defun edraw-color-picker-define-keys-for-palette-colors (keymap)
  (dotimes (i 10)
    (define-key keymap (kbd (format "C-%d" (% (1+ i) 10)))
      (edraw-color-picker-select-palette-color-fname i))))

;;;;;; Recent Colors

(cl-defmethod edraw-select-recent-color ((picker edraw-color-picker) index)
  (when-let* ((color (edraw-nth-color
                      (edraw-color-picker-get-recent-colors-model
                       (oref picker options))
                      index)))
    (edraw-set-current-color picker color)))

(defun edraw-color-picker-select-recent-color (index)
  (interactive "p")
  (when-let ((picker (edraw-color-picker-at-input last-command-event)))
    (edraw-select-recent-color picker index)))

(defun edraw-color-picker-select-recent-color-fname (i)
  (intern (format "edraw-color-picker-select-recent-color-%d" i)))

;; Define select-recent-color-<n> commands
(dotimes (i 10)
  (defalias (edraw-color-picker-select-recent-color-fname i)
    (lambda ()
      (interactive)
      (edraw-color-picker-select-recent-color i))))

(defun edraw-color-picker-define-keys-for-recent-colors (keymap)
  (dotimes (i 10)
    (define-key keymap (kbd (format "C-%d" (% (1+ i) 10)))
      (edraw-color-picker-select-recent-color-fname i))))


;;;; Color Picker Search

(defun edraw-color-picker-at-input (event)
  (if (or (mouse-event-p event)
          (memq (event-basic-type event)
                '(wheel-up wheel-down mouse-4 mouse-5 drag-n-drop)))
      (let* ((mouse-pos (event-start event))
             (window (posn-window mouse-pos))
             (buffer (window-buffer window))
             (pos (posn-point mouse-pos)))
        ;; (when move-point-on-click-p
        ;;   (select-window window)
        ;;   (set-window-point window pos))
        (with-current-buffer buffer
          (edraw-color-picker-at pos)))
    (edraw-color-picker-at (point))))

(defvar-local edraw-color-picker-finder nil)

(defun edraw-color-picker-at (pos)
  (or (and edraw-color-picker-finder
           (funcall edraw-color-picker-finder pos))
      ;; for overlay display
      (edraw-color-picker-overlaid-at pos)
      ;;@todo search text property?
      ))


;;;; Overlay Display

(defvar edraw-color-picker-map
  (let ((km (make-sparse-keymap)))
    (define-key km [down-mouse-1] #'edraw-color-picker-on-down-mouse)
    (define-key km [down-mouse-3] #'edraw-color-picker-on-down-mouse)
    (define-key km [hot-spot down-mouse-1] #'edraw-color-picker-on-down-mouse)
    (define-key km [hot-spot down-mouse-3] #'edraw-color-picker-on-down-mouse)
    (define-key km [drag-mouse-1] 'ignore)
    (define-key km [drag-mouse-3] 'ignore)
    (define-key km [mouse-1] 'ignore)
    (define-key km [mouse-3] 'ignore)
    (define-key km [double-down-mouse-1] 'ignore)
    (define-key km [double-down-mouse-3] 'ignore)
    (define-key km [double-drag-mouse-1] 'ignore)
    (define-key km [double-drag-mouse-3] 'ignore)
    (define-key km [double-mouse-1] 'ignore)
    (define-key km [double-mouse-3] 'ignore)
    (define-key km [triple-down-mouse-1] 'ignore)
    (define-key km [triple-down-mouse-3] 'ignore)
    (define-key km [triple-drag-mouse-1] 'ignore)
    (define-key km [triple-drag-mouse-3] 'ignore)
    (define-key km [triple-mouse-1] 'ignore)
    (define-key km [triple-mouse-3] 'ignore)
    (edraw-color-picker-define-keys-for-palette-colors km)
    km))

(defun edraw-color-picker-overlay
    (overlay-or-args-props target-property &optional initial-color options)
  "Create a color picker that display using an overlay.

OVERLAY-OR-ARGS-PROPS : An overlay object or a list of arguments
to create an overlay. The first five elements of the list are
arguments to make-overlay. The rest is a plist to pass to
overlay-put.

TARGET-PROPERTY : Overlay property to set the color-picker image.
Specify one of \\='display, \\='before-string, or \\='after-string."

  (edraw-color-picker-create (edraw-color-picker-display-overlay
                              :overlay (edraw-color-picker-make-overlay
                                        overlay-or-args-props)
                              :target-property (or target-property 'display)
                              :keymap edraw-color-picker-map)
                             initial-color
                             options))

(defclass edraw-color-picker-display-overlay ()
  ((overlay :initarg :overlay)
   (target-property :initarg :target-property)
   (keymap :initarg :keymap)
   (picker)
   (target-frame :initform nil :writer edraw-set-target-frame)))

(cl-defmethod edraw-overlay ((display edraw-color-picker-display-overlay))
  (oref display overlay))

(cl-defmethod edraw-buffer ((display edraw-color-picker-display-overlay))
  (overlay-buffer (oref display overlay)))

(cl-defmethod edraw-initialize ((display edraw-color-picker-display-overlay)
                                picker)
  (with-slots (overlay target-property keymap) display
    (oset display picker picker)
    ;; Set overlay properties
    (when (eq target-property 'display)
      (overlay-put overlay 'edraw-color-picker picker)
      (overlay-put overlay 'face 'default)
      (overlay-put overlay 'keymap keymap)
      (overlay-put overlay 'pointer 'arrow))))

(cl-defmethod edraw-closed-p ((display edraw-color-picker-display-overlay))
  (with-slots (overlay) display
    (when overlay
      (null (overlay-buffer overlay)))))

(cl-defmethod edraw-close ((display edraw-color-picker-display-overlay))
  (with-slots (overlay target-property) display
    (edraw-flush-image display)
    (pcase target-property
      ('display
       (overlay-put overlay 'display nil))
      ((or 'before-string 'after-string)
       (overlay-put overlay target-property nil)))
    ;;@todo delete here? (If change here, also change edraw-closed-p)
    (delete-overlay overlay)))

(cl-defmethod edraw-update ((display edraw-color-picker-display-overlay))
  (edraw-flush-image display)
  (with-slots (overlay target-property keymap picker) display
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

(cl-defmethod edraw-flush-image ((display edraw-color-picker-display-overlay))
  (with-slots (overlay target-property keymap picker target-frame) display
    (pcase target-property
      ('display
       (let ((spec (overlay-get overlay 'display)))
         (when (consp spec) ;; imagep
           (ignore-errors (image-flush spec target-frame)))))
      ((or 'before-string 'after-string)
       (let ((text (overlay-get overlay target-property)))
         (when (and text (not (string-empty-p text)))
           (let ((spec (get-text-property 0 'display text)))
             (when (consp spec) ;; imagep
               (ignore-errors (image-flush spec target-frame))))))))))

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
        (overlay-put overlay 'wrap-prefix "") ;;Emacs has a bug that shifts mouse coordinates
        (overlay-put overlay 'line-prefix "")
        (overlay-put overlay 'before-string (concat "\n" picker-left-string))
        (overlay-put overlay 'after-string "\n")
        t))))

(defun edraw-color-picker-overlaid-at (pos)
  (seq-some (lambda (ov) (overlay-get ov 'edraw-color-picker))
            (overlays-at pos)))

(defun edraw-color-picker-on-down-mouse (down-event)
  (interactive "e")
  (when-let ((picker (edraw-color-picker-at-input down-event)))
    (edraw-on-down-mouse picker down-event)))

;;;; Frame Display

(defvar edraw-color-picker-frame-parameters
  '(
    ;; *Basic
    ;;(display)
    ;;(display-type)
    (title . nil)
    (name . " *Color Picker Frame*")
    ;;(explicit-name)
    ;; *Position
    ;;(left . (+ 100))
    ;;(top . 100)
    ;;(icon-left)
    ;;(icon-top)
    (user-position . t)
    (z-group . above)
    ;; *Size
    ;;(width . (text-pixels . 400))
    ;;(height . (text-pixels . 300))
    ;;(user-size)
    ;;(min-width)
    ;;(min-height)
    ;;(fullscreen)
    ;;(fullscreen-restore)
    ;;(fit-frame-to-buffer-margins)
    ;;(fit-frame-to-buffer-sizes)
    ;; *Layout
    (border-width . 0)
    (internal-border-width . 0)
    (child-frame-border-width . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (scroll-bar-width . 0)
    (scroll-bar-height . 0)
    (left-fringe . 0)
    (right-fringe . 0)
    (right-divider-width . 0)
    (bottom-divider-width . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    ;;(tool-bar-position)
    (tab-bar-lines . 0)
    (line-spacing . 0)
    (no-special-glyphs . t)
    ;; *Buffer
    (minibuffer . nil)
    ;;(buffer-predicate . (lambda (_buffer) nil))
    ;;(buffer-list)
    (unsplittable . t)
    ;; *Frame Interaction
    ;;(parent-frame . ,(selected-frame))
    ;;(delete-before . <related-frame>)
    ;;(mouse-wheel-frame)
    (no-other-frame . t)
    ;;(auto-hide-function)
    ;;(minibuffer-exit)
    ;;(keep-ratio . nil)
    ;; *Mouse Dragging
    ;;(drag-internal-border)
    ;;(drag-with-header-line)
    ;;(drag-with-tab-line)
    ;;(drag-with-mode-line)
    ;;(snap-width)
    ;;(top-visible)
    ;;(bottom-visible)
    ;; *Management
    (visibility . t)
    ;;(auto-raise)
    ;;(auto-lower)
    ;;(icon-type)
    ;;(icon-name)
    ;;(window-id)
    ;;(outer-window-id)
    ;;(wait-for-wm)
    ;;(sticky)
    ;;(inhibit-double-buffering)
    ;;(skip-taskbar)
    ;;(no-focus-on-map)
    ;;(no-accept-focus . t)
    (undecorated . t) ;; No caption, borders, buttons
    ;;(override-redirect . t)
    ;;(ns-appearance)
    ;;(ns-transparent-titlebar)
    ;; *Cursor
    (cursor-type . nil)
    ;; *Font and Color
    ;;(font-backend)
    ;;(background-mode)
    ;;(tty-color-mode)
    ;;(screen-gamma)
    ;;(alpha)
    ))

(defun edraw-color-picker-frame-parameters ()
  edraw-color-picker-frame-parameters)

(defclass edraw-color-picker-display-frame ()
  ((overlay-display)
   (frame)
   (buffer)
   (frame-position :initform nil)))

(cl-defmethod edraw-overlay ((display edraw-color-picker-display-frame))
  (edraw-overlay (oref display overlay-display)))

(cl-defmethod edraw-buffer ((display edraw-color-picker-display-frame))
  (oref display buffer))

(cl-defmethod edraw-initialize ((display edraw-color-picker-display-frame)
                                picker)
  (defvar tab-bar-format) ;;Emacs 28.1
  (let* (;; Create buffer
         (buffer
          (generate-new-buffer " *Color Picker*"))
         ;; Create overlay display
         (overlay-display
          (with-current-buffer buffer
            ;; Initialize local variables
            (setq-local mode-line-format nil
                        header-line-format nil
                        tab-line-format nil
                        tab-bar-format nil
                        truncate-lines nil
                        show-trailing-whitespace nil
                        display-line-numbers nil)
            ;; Create overlay
            (insert (propertize "*"
                                'read-only t
                                'front-sticky '(read-only)))
            (let* ((ov (make-overlay (point-min) (point-max) buffer nil t))
                   (overlay-display (edraw-color-picker-display-overlay
                                     :overlay ov
                                     :target-property 'display
                                     :keymap edraw-color-picker-map)))
              (edraw-initialize overlay-display picker)
              overlay-display)))
         ;; Compute frame size and position
         (width (edraw-image-width picker))
         (height (edraw-image-height picker))
         (position
          ;; Note: the selected frame, selected window, and current
          ;;       buffer may be used to compute frame position.
          (edraw-compute-frame-position display width height))
         ;; Create frame
         (frame
          (let* ((before-make-frame-hook nil)
                 (after-make-frame-functions nil)
                 (frame (edraw-color-picker--get-unused-frame
                         (append
                          `((parent-frame . ,(selected-frame))
                            (width . (text-pixels . ,width))
                            (height . (text-pixels . ,height))
                            (left . ,(car position))
                            (top . ,(cdr position)))
                          (edraw-color-picker-frame-parameters)))))
            frame))
         ;; Get Window
         (window (frame-root-window frame)))

    ;; Set the frame where the overlay will be displayed.
    ;; Required for `image-flush'.
    (edraw-set-target-frame overlay-display frame)

    ;; Initialize Window
    (let ((old-buffer (window-buffer window)))
      (unless (eq buffer old-buffer)
        ;; Kill previous buffer if window dedicated
        (edraw-color-picker--kill-dedicated-buffer window)
        ;; Set new buffer
        (set-window-buffer window buffer)
        (set-window-dedicated-p window t)))

    ;; Set to slots
    (oset display overlay-display overlay-display)
    (oset display frame frame)
    (oset display buffer buffer)))

(cl-defmethod edraw-closed-p ((display edraw-color-picker-display-frame))
  (null (oref display frame)))

(cl-defmethod edraw-close ((display edraw-color-picker-display-frame))
  (with-slots (overlay-display buffer frame) display
    (when frame
      (edraw-close overlay-display)

      ;; Release buffer resources
      (with-current-buffer buffer
        (with-silent-modifications
          (erase-buffer))
        (kill-all-local-variables))
      ;; Hide frame
      (edraw-color-picker--hide-frame frame)
      (setq frame nil))))

(cl-defmethod edraw-update ((display edraw-color-picker-display-frame))
  (edraw-update (oref display overlay-display)))

(cl-defmethod edraw-compute-frame-position ((display
                                             edraw-color-picker-display-frame)
                                            width height)
  (let ((fp (oref display frame-position)))
    (cond
     ((functionp fp) (funcall fp width height))
     (t
      (edraw-color-picker-frame-position-near-point width height)
      ;;(cons 0 0)
      ))))

(defun edraw-color-picker-frame-position-near-point (width height)
  (let* ((window-edges (window-inside-pixel-edges))
         (window-left (nth 0 window-edges))
         (window-top (nth 1 window-edges))
         (window-right (nth 2 window-edges))
         (window-bottom (nth 3 window-edges))
         (point-pos (pos-visible-in-window-p (point) nil t))
         (point-left (+ window-left (or (car point-pos) 0)))
         (point-top (+ window-top (or (cadr point-pos) 0)))
         (point-bottom (+ point-top (default-line-height)))
         (x (max window-left
                 (min (- window-right width)
                      (- point-left (/ width 2)))))
         (y (- point-top height)))

    (when (< y window-top)
      (setq y point-bottom))

    ;; Adjustments to the frame
    (let* ((frame-edges (frame-edges))
           (frame-left 0)
           (frame-top 0)
           (frame-right (- (nth 2 frame-edges) (nth 0 frame-edges)))
           (frame-bottom (- (nth 3 frame-edges) (nth 1 frame-edges))))
      ;; If it doesn't fit above or below the point in the window
      (when (> (+ y height) window-bottom)
        ;; Choose a wider direction within the frame
        (let ((space-height-above (- point-top frame-top))
              (space-height-below (- frame-bottom point-bottom)))
          (setq y (if (>= space-height-above space-height-below)
                      (- point-top height)
                    point-bottom))))

      ;; Fit inside the frame (top left priority)
      (setq x (max frame-left (min (- frame-right width) x)))
      (setq y (max frame-top (min (- frame-bottom height) y))))

    (cons x y)))

;;;;; Recycle Frame

(defvar edraw-color-picker--unused-frames nil)

(defun edraw-color-picker--get-unused-frame (frame-parameters
                                             &optional parent-frame)
  (let* ((parent-frame (or parent-frame (selected-frame)))
         (frame (seq-find (lambda (frame)
                            (and frame
                                 (frame-live-p frame)
                                 (eq (frame-parent frame) parent-frame)
                                 (not (frame-visible-p frame))))
                          edraw-color-picker--unused-frames)))
    (if frame
        ;; Reuse
        (progn
          (setq edraw-color-picker--unused-frames
                (delq frame edraw-color-picker--unused-frames))
          (modify-frame-parameters frame frame-parameters)
          frame)
      ;; New
      (make-frame frame-parameters))))

(defun edraw-color-picker--hide-frame (frame)
  (let ((parent-frame (and (eq (selected-frame) frame)
                           (frame-parent frame))))
    (make-frame-invisible frame t)
    ;; Invisible frames can interfere with motion events, so move them
    ;; out of the way. (Emacs 29.1 for Windows)
    ;; Affects edraw-transform-interactive.
    (set-frame-position frame -1000 -1000)
    ;; Transfor focus to parent
    (when parent-frame
      (select-frame parent-frame)))
  (setq edraw-color-picker--unused-frames
        (nconc edraw-color-picker--unused-frames
               (list frame)))
  (edraw-color-picker--cleanup-frames))

(defun edraw-color-picker--cleanup-frames ()
  (setq edraw-color-picker--unused-frames
        (cl-loop with parent-alist = nil
                 for frame in edraw-color-picker--unused-frames
                 if (and frame
                         (frame-live-p frame)
                         ;; max frame count per same parent
                         (<= (cl-incf (alist-get
                                       (frame-parent frame)
                                       parent-alist 0))
                             2))
                 collect frame
                 else do (edraw-color-picker--kill-frame frame))))

(defun edraw-color-picker--kill-dedicated-buffer (window)
  (when (and window
             (window-live-p window)
             (window-dedicated-p window))
    (let ((buffer (window-buffer window)))
      (when buffer
        (set-window-dedicated-p window nil)
        (kill-buffer buffer)))))

(defun edraw-color-picker--kill-frame (frame)
  (when (and frame (frame-live-p frame))
    ;; Kill dedicated buffer
    (edraw-color-picker--kill-dedicated-buffer (frame-root-window frame))
    ;; Kill frame
    (delete-frame frame t)))

(defun edraw-color-picker-delete-all-unused-frames ()
  "Delete all unused frames that are kept for faster processing."
  (interactive)
  (mapc #'edraw-color-picker--kill-frame edraw-color-picker--unused-frames)
  (setq edraw-color-picker--unused-frames nil))



;;;; Applications

;;;;; Display in Current Buffer

(defun edraw-color-picker-open-near-point (&optional initial-color options)
  (interactive)

  (unless (assq :scale options)
    (setf (alist-get :scale options) edraw-color-picker-near-point-scale))

  (let* ((picker (if edraw-color-picker-use-frame-p
                     ;; Use child frame
                     (edraw-color-picker-create
                      (edraw-color-picker-display-frame)
                      initial-color options)
                   ;; Use overlay
                   (let* ((overlay (make-overlay (point) (point) nil t nil))
                          (picker (edraw-color-picker-overlay
                                   overlay 'display initial-color options)))
                     (edraw-color-picker-move-overlay-at-point overlay picker)
                     (overlay-put overlay 'evaporate t)
                     picker)))

         (on-ok (lambda (&rest _) (edraw-close picker)))
         (on-cancel (lambda (&rest _) (edraw-close picker))))

    (edraw-add-hook picker 'ok on-ok)
    (edraw-add-hook picker 'cancel on-cancel)
    picker))

;;;;; Display with Transient Map

(defvar edraw-color-picker--transient-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-c") #'edraw-color-picker--transient-map-click-ok)
    (define-key km (kbd "M-p") #'edraw-color-picker--transient-map-previous-history-color)
    (define-key km (kbd "M-n") #'edraw-color-picker--transient-map-next-history-color)
    (edraw-color-picker-define-keys-for-palette-colors km)
    km))

(defvar edraw-color-picker--transient-map-info nil
  "A list of information about color pickers currently being controlled by
the transient-map.
Each element of the list is in the format (COLOR-PICKER . PLIST).
The information from the most recently started transient-map is added to
the top of the list. It is removed from the list when the transient-map
exits.")

(defun edraw-color-picker--transient-map-info-push (picker &rest props)
  (push (cons picker props)
        edraw-color-picker--transient-map-info))

(defun edraw-color-picker--transient-map-info-pop (picker)
  (setq edraw-color-picker--transient-map-info
        (seq-remove (lambda (x) (eq (car x) picker))
                    edraw-color-picker--transient-map-info)))

(defun edraw-color-picker--transient-map-info-current ()
  (car edraw-color-picker--transient-map-info))

(defun edraw-color-picker--transient-map-current-picker (&rest _args)
  "A function that is set to the `edraw-color-picker-finder'
variable. Ensure that `edraw-color-picker-at' can find the color
picker. Make `edraw-color-picker-select-recent-color' work
correctly."
  (let ((picker (car (edraw-color-picker--transient-map-info-current))))
    (when (and picker (not (edraw-closed-p picker)))
      picker)))

(defconst edraw-color-picker--transient-map-help
  "\\[keyboard-quit]:Cancel\
 \\<edraw-color-picker--transient-keymap>\
 \\[edraw-color-picker--transient-map-click-ok]:OK\
  \\[edraw-color-picker--transient-map-previous-history-color]\
/\\[edraw-color-picker--transient-map-next-history-color]:Recent Color")

(defun edraw-color-picker--set-transient-map (picker
                                              keymap options initial-color)
  (setq-local edraw-color-picker-finder
              #'edraw-color-picker--transient-map-current-picker)
  (let* ((help (substitute-command-keys edraw-color-picker--transient-map-help))
         (exit-transient-map-fun
          (set-transient-map
           keymap
           (lambda () ;; keep-pred:
             (and (not (edraw-closed-p picker))
                  (edraw-color-picker--transient-map-keep-pred
                   ;; Pass the actual keymap to keep-pred
                   keymap)
                  (progn (edraw-echo help) t)))
           (lambda () ;; on-exit:
             ;; (message "Exit transient-map")
             (edraw-close picker)
             ;; Remove the current picker object
             (edraw-color-picker--transient-map-info-pop picker)

             (edraw-echo "")))))

    ;; When PICKER is closed by mouse action, exit the transient-map.
    (edraw-add-hook picker 'closed
                    (lambda (&rest _) (funcall exit-transient-map-fun)))

    ;; Add the current picker object
    (edraw-color-picker--transient-map-info-push
     picker
     :hist-list (edraw-color-picker-make-history-list options initial-color)
     :hist-pos 0)

    (edraw-echo help)))

(defun edraw-color-picker--transient-map-keep-pred (keymap)
  (or
   (memq this-command
         '(;; Allow switching frames
           handle-switch-frame
           edraw-color-picker-on-down-mouse
           ignore))
   ;; Check this-command is in KEYMAP
   ;; See `set-transient-map' function
   (let ((mc (lookup-key
              keymap ;;edraw-color-picker-map
              (this-command-keys-vector))))
     (when mc
       ;; Consider remapping
       (setq mc (or (and (symbolp mc) (command-remapping mc)) mc))
       (eq this-command mc)))))

(defun edraw-color-picker--transient-map-click-ok ()
  (interactive)
  (when-let* ((picker (edraw-color-picker--transient-map-current-picker)))
    (edraw-click-area picker "ok")))

(defun edraw-color-picker--goto-history-color (index)
  (let* ((info (edraw-color-picker--transient-map-info-current))
         (picker (car info)))
    (when (and info picker (not (edraw-closed-p picker)))
      (when (= (plist-get (cdr info) :hist-pos) 0)
        (edraw-plist-set (cdr info)
                         :hist-current
                         (edraw-color-picker-normalize-color-string
                          (edraw-get-current-color picker))))
      (let ((color-str
             (cond
              ((zerop index)
               (plist-get (cdr info) :hist-current))
              ((>= index 1)
               (nth (1- index) (plist-get (cdr info) :hist-list))))))
        (when color-str
          (edraw-set-current-color picker
                                   (edraw-color-from-string color-str))
          (edraw-plist-set (cdr info) :hist-pos index))))))

(defun edraw-color-picker--transient-map-previous-history-color (n)
  (interactive "p")
  (unless (zerop n)
    (when-let* ((info (edraw-color-picker--transient-map-info-current))
                (hist-list (plist-get (cdr info) :hist-list))
                (hist-pos (plist-get (cdr info) :hist-pos)))
      (edraw-color-picker--goto-history-color
       (min (+ hist-pos n) (length hist-list))))))

(defun edraw-color-picker--transient-map-next-history-color (n)
  (interactive "p")
  (unless (zerop n)
    (when-let* ((info (edraw-color-picker--transient-map-info-current))
                (hist-pos (plist-get (cdr info) :hist-pos)))
      (edraw-color-picker--goto-history-color
       (max (- hist-pos n) 0)))))

(defun edraw-color-picker-open-with-transient-map (&optional
                                                   initial-color options)
  "Open a color picker near the point and start the transient-map.

Open a color picker with `edraw-color-picker-open-near-point'.

Use the keymap set in the variable specified by the
:transient-keymap-var option in OPTIONS as the transient-map.
The default keymap is `edraw-color-picker--transient-keymap'"
  (unless (assq :transient-keymap-var options)
    (setf (alist-get :transient-keymap-var options)
          'edraw-color-picker--transient-keymap))

  (let ((picker (edraw-color-picker-open-near-point initial-color options)))
    ;; Start transient-map
    (edraw-color-picker--set-transient-map
     picker
     (symbol-value (alist-get :transient-keymap-var options))
     options initial-color)
    ;; Return picker
    picker))


;;;;; Insert Color

(defvar edraw-color-picker-insert-default-color-scheme 'web)

;;;###autoload
(defun edraw-color-picker-insert-color (&optional initial-color options)
  "Insert a color selected by color picker."
  (interactive)

  (unless (assq :color-name-scheme options)
    (setf (alist-get :color-name-scheme options)
          edraw-color-picker-insert-default-color-scheme))

  (let ((picker (edraw-color-picker-open-with-transient-map
                 initial-color options)))
    ;; OK
    (edraw-add-hook
     picker 'ok
     (lambda (&rest _)
       ;; Close first
       (edraw-close picker)
       ;; Insert
       (insert (edraw-color-picker-color-to-string
                (edraw-get-current-color picker)
                options))))
    ;; Echo current color
    (edraw-add-hook picker 'color-change
                    #'edraw-color-picker-echo-current-color))
  t)

(defun edraw-color-picker-echo-current-color (picker)
  ;; (edraw-color-picker-color-to-string c (edraw-options picker))
  (let* ((c (edraw-get-current-color picker))
         (r (edraw-color-r c)) (r8 (round (* r 255)))
         (g (edraw-color-g c)) (g8 (round (* g 255)))
         (b (edraw-color-b c)) (b8 (round (* b 255)))
         (a (edraw-color-a c)) (a8 (round (* a 255)))
         (h (edraw-hue c))
         (s (edraw-saturation c))
         (b (edraw-brightness c))
         (rl (edraw-relative-luminance c)))
    (edraw-echo-format
     "\
R:%5.1f%%(%3d,%02X), G:%5.1f%%(%3d,%02X), \
B:%5.1f%%(%3d,%02X), A:%5.1f%%(%3d,%02X)
H:%5.1fdeg, S:%5.1f%%, B:%5.1f%%, RL:%5.1f%%"
     (* r 100) r8 r8
     (* g 100) g8 g8
     (* b 100) b8 b8
     (* a 100) a8 a8
     h
     (* s 100)
     (* b 100)
     (* rl 100))))

;;;;; Replace Color

;;;###autoload
(defun edraw-color-picker-replace-or-insert-color-at-point (&optional options)
  (interactive)
  (or (edraw-color-picker-replace-color-at (point) options)
      (edraw-color-picker-insert-color nil options)))

;;;###autoload
(defun edraw-color-picker-replace-color-at-point (&optional options)
  "Replace the color at the point with the color selected by color picker."
  (interactive)
  (edraw-color-picker-replace-color-at (point) options))

;;;###autoload
(defun edraw-color-picker-replace-color-at (position &optional options)
  "Replace the color at POSITION with the color selected by color picker."
  (interactive "d")

  (unless (assq :color-name-scheme options)
    (setf (alist-get :color-name-scheme options)
          edraw-color-picker-insert-default-color-scheme))

  (when-let ((match-result (edraw-color-picker-lookup-color-at
                            position
                            (alist-get :color-name-scheme options)))
             (beg (nth 0 match-result))
             (end (nth 1 match-result))
             ;; Index of `edraw-color-string-patterns'
             (format-index (nth 2 match-result)))
    ;; Open color picker near the point
    (let* ((str (buffer-substring-no-properties beg end))
           (initial-color (edraw-color-picker-color-from-string str options))
           (picker (edraw-color-picker-open-with-transient-map
                    initial-color options)))
      ;; OK
      (edraw-add-hook
       picker 'ok
       (lambda (&rest _)
         ;; Close first
         (edraw-close picker)
         ;; Replace color string as same format
         (save-excursion
           ;;@todo Use marker?
           (goto-char beg)
           (delete-region beg end)
           (insert
            (edraw-color-picker-lookup-color-to-string
             (edraw-get-current-color picker) format-index options)))))
      ;; Echo current color
      (edraw-add-hook picker 'color-change
                      #'edraw-color-picker-echo-current-color))
    t))

;;;;; Color Name Lookup From Buffer

(defvar edraw-color-picker-lookup-color-name-regexp-alist nil)

(defun edraw-color-picker-lookup-color-name-regexp (name-scheme)
  (or
   (alist-get name-scheme edraw-color-picker-lookup-color-name-regexp-alist)

   (let ((regexp
          (pcase name-scheme
            ('web (regexp-opt (mapcar #'car edraw-color-web-keywords)))
            ('emacs (regexp-opt (defined-colors))))))
     (when regexp
       (push (cons name-scheme regexp)
             edraw-color-picker-lookup-color-name-regexp-alist)
       regexp))))

(defun edraw-color-picker-lookup-color-regexp (name-scheme)
  (concat "\\(?:" edraw-color-string-patterns-re "\\|"
          ;; last index
          "\\(" (edraw-color-picker-lookup-color-name-regexp name-scheme) "\\)"
          "\\)"))

(defun edraw-color-picker-lookup-color-at (position name-scheme)
  (save-excursion
    (goto-char position)
    (goto-char (line-beginning-position))
    (let ((line-end (line-end-position))
          (result nil)
          (regexp (edraw-color-picker-lookup-color-regexp name-scheme)))
      (while (and (null result)
                  (re-search-forward regexp line-end t))
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (when (and (<= beg position) (< position end))
            (setq result
                  (list beg
                        end
                        ;; Index of `edraw-color-string-patterns'
                        (/ (cl-position-if-not #'null (cddr (match-data)))
                           2))))))
      result)))

(defun edraw-color-picker-lookup-color-to-string (color format-index options)
  (or
   ;; last index
   (when (= format-index (length edraw-color-string-patterns))
     (or
      (pcase (alist-get :color-name-scheme options)
        ('web (edraw-to-string-web-keyword color))
        ('emacs (edraw-to-string-emacs-color-name color)))
      (edraw-color-picker-color-to-string color options)))
   ;; Use edraw-color-string-patterns
   (edraw-color-picker-color-to-string
    color
    (cons
     (cons :color-format
           ;; hex or rgb
           (cadr (nth format-index edraw-color-string-patterns)))
     options))))

;;;;; Read Color from Minibuffer

(defvar edraw-color-picker-read-color--history nil)

;;;###autoload
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
                  overlay 'before-string initial-color
                  (cons
                   '(:transient-keymap-var
                     . edraw-color-picker-minibuffer-mode-map)
                   options)))
         (on-minibuffer-setup
          (lambda ()
            (edraw-color-picker-minibuffer--on-minibuffer-setup picker)))
         (minibuffer-setup-hook (cons on-minibuffer-setup
                                      minibuffer-setup-hook))
         (edraw-color-picker-read-color--history
          (edraw-color-picker-make-history-list options initial-color)))

    ;; Add hooks to picker
    (edraw-add-hook picker 'ok #'edraw-color-picker-minibuffer--on-ok)
    (edraw-add-hook picker 'cancel #'edraw-color-picker-minibuffer--on-cancel)
    (when (alist-get :no-color options)
      (edraw-add-hook picker 'no-color
                      #'edraw-color-picker-minibuffer--on-no-color))
    (edraw-add-hook picker 'color-change
                    #'edraw-color-picker-minibuffer--on-color-change)

    (unwind-protect
        (let ((max-mini-window-height 1.0)
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
            (let ((input (read-string (edraw-color-picker-minibuffer--prompt
                                       prompt allow-strings options)
                                      initial-input
                                      'edraw-color-picker-read-color--history)))
              (when (or (member input allow-strings)
                        (edraw-color-picker-color-from-string input options))
                (setq result input))))
          (when-let ((result-color
                      (edraw-color-picker-color-from-string result options)))
            ;; Avoid color name
            (edraw-color-picker-add-recent-color options result-color))
          result)
      (edraw-close picker)
      (delete-overlay overlay))))

(defun edraw-color-picker-minibuffer--prompt (prompt allow-strings options)
  "Create a prompt for `edraw-color-picker-read-color'."
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

(defvar-local edraw-color-picker-minibuffer--picker nil)
(defvar-local edraw-color-picker-minibuffer--buffer-contents nil)
(defvar-local edraw-color-picker-minibuffer--in-post-command-p nil)

(define-minor-mode edraw-color-picker-minibuffer-mode
  "Defines keybindings for the color picker in the minibuffer."
  :keymap (let ((km (make-sparse-keymap)))
            (edraw-color-picker-define-keys-for-palette-colors km)
            km))

(defun edraw-color-picker-minibuffer--on-minibuffer-setup (picker)
  "Initialize minibuffer for `edraw-color-picker-read-color'"
  (unless edraw-color-picker-minibuffer-mode
    (edraw-color-picker-minibuffer-mode)
    ;; Initialize local variables
    (setq-local
     edraw-color-picker-finder #'edraw-color-picker-in-minibuffer
     edraw-color-picker-minibuffer--picker picker
     edraw-color-picker-minibuffer--in-post-command-p nil
     edraw-color-picker-minibuffer--buffer-contents nil)
    ;; Display overlay at the beginning of the minibuffer
    (move-overlay (edraw-overlay (edraw-get-display picker))
                  (point-min) (point-min) (current-buffer))
    ;; Update color picker each time command is executed
    (add-hook 'post-command-hook
              #'edraw-color-picker-minibuffer--on-post-command nil t)))

(defun edraw-color-picker-in-minibuffer (&rest _args)
  "A function that is set to the `edraw-color-picker-finder'
variable. Ensure that `edraw-color-picker-at' can find the color
picker. Make `edraw-color-picker-select-recent-color' work
correctly."
  edraw-color-picker-minibuffer--picker)

(defun edraw-color-picker-minibuffer--on-post-command ()
  "Update color picker each time command is executed."
  (when edraw-color-picker-minibuffer-mode
    (setq edraw-color-picker-minibuffer--in-post-command-p t)
    (condition-case err
        (let* ((picker edraw-color-picker-minibuffer--picker)
               (options (edraw-options picker))
               (picker-color (edraw-get-current-color picker))
               (picker-color-str (edraw-color-picker-color-to-string
                                  picker-color options))
               (minibuffer-string (minibuffer-contents-no-properties))
               (minibuffer-color (edraw-color-picker-color-from-string
                                  minibuffer-string options)))
          ;; update color picker
          (when (and
                 ;; not equals string representation of picker color
                 ;; (set by last on-color-change)
                 (not (string= minibuffer-string
                               picker-color-str))
                 ;; is valid color
                 minibuffer-color
                 ;; not equals picker color
                 (not (edraw-color-equal-p minibuffer-color
                                           picker-color)))
            (edraw-set-current-color picker minibuffer-color))
          ;; callback minibuffer string change
          ;; (include invalid string. e.g. "none")
          (edraw-color-picker-minibuffer--notify-input-change
           minibuffer-string
           minibuffer-color))
      (error
       (message "err=%s" err)))
    (setq edraw-color-picker-minibuffer--in-post-command-p nil)))

(defun edraw-color-picker-minibuffer--notify-input-change (string color)
  "Notifies the callback specified by the :on-input-change option
of input changes."
  ;; assert current-buffer is minibuffer
  (unless (equal string edraw-color-picker-minibuffer--buffer-contents)
    (setq edraw-color-picker-minibuffer--buffer-contents string)
    (when-let ((picker edraw-color-picker-minibuffer--picker)
               (options (edraw-options picker))
               (callback (alist-get :on-input-change options)))
      (funcall callback string color))))

(defun edraw-color-picker-minibuffer--on-color-change (picker)
  "Called when the color picker color changes."
  (let ((buffer (edraw-buffer picker)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (unless edraw-color-picker-minibuffer--in-post-command-p

          (let* ((options (edraw-options picker))
                 (picker-color (edraw-get-current-color picker))
                 (picker-color-str (edraw-color-picker-color-to-string
                                    picker-color options))
                 (minibuffer-string (with-current-buffer buffer
                                      (minibuffer-contents-no-properties)))
                 (minibuffer-color (edraw-color-picker-color-from-string
                                    minibuffer-string options))
                 (minibuffer-color-str
                  (if minibuffer-color
                      (edraw-color-picker-color-to-string
                       minibuffer-color
                       options))))
            (unless (equal picker-color-str minibuffer-color-str)
              ;; Update minibuffer text
              (edraw-color-picker-minibuffer--set-contents picker-color-str)
              ;; Callback
              (edraw-color-picker-minibuffer--notify-input-change
               picker-color-str
               picker-color
               ;;or (edraw-color-picker-color-from-string picker-color-str)?
               ;;NG: minibuffer-color
               ))))))))

(defun edraw-color-picker-minibuffer--on-ok (picker)
  (with-current-buffer (edraw-buffer picker)
    ;; Close minibuffer
    (exit-minibuffer)))

(defun edraw-color-picker-minibuffer--on-cancel (picker)
  (with-current-buffer (edraw-buffer picker)
    ;; Abort input
    (minibuffer-keyboard-quit)))

(defun edraw-color-picker-minibuffer--on-no-color (picker)
  (with-current-buffer (edraw-buffer picker)
    ;; Set minibuffer text to invalid value (e.g. none)
    (edraw-color-picker-minibuffer--set-contents
     (alist-get :no-color (oref picker options) ""))))

(defun edraw-color-picker-minibuffer--set-contents (string)
  "Change the content text in the minibuffer to STRING."
  (delete-minibuffer-contents)
  (goto-char (minibuffer-prompt-end))
  (insert string))



;;;; Color Utility


(defun edraw-color-picker-color-to-string (color options)
  "Convert COLOR edraw-color object to string.

Valid OPTIONS are:
(:color-float-format . num-digits or format-string or format-function)
(:color-format . nil or \\='hex or \\='rgb)
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
       (edraw-to-string-rgba-or-rgb color)))))

(defun edraw-color-picker-color-from-string (string options)
  "Convert STRING to edraw-color object.

Valid options are:
(:color-name-scheme . emacs or web)
(:enable-opacity . nil or t)
"
  (let ((edraw-color-name-scheme (alist-get :color-name-scheme options 'emacs)))
    (let ((color (edraw-color-from-string string)))
      (if (alist-get :enable-opacity options t)
          color
        (if (and color (= (oref color a) 1.0))
            color
          nil)))))

(defun edraw-color-picker-ensure-color (obj options)
  (if (cl-typep obj 'edraw-color)
      obj
    (or (if (stringp obj) (edraw-color-picker-color-from-string obj options))
        (let ((last-color (car (edraw-color-picker-get-recent-colors options))))
          ;; Don't throw an error if the last color cannot be obtained
          ;; or is abnormal.
          (when last-color
            (ignore-errors (edraw-color-ensure last-color))))
        (edraw-color-f 1 0 0 1))))


(provide 'edraw-color-picker)
;;; edraw-color-picker.el ends here
