;;; edraw.el --- Emacs Easy Draw                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Homepage: https://github.com/misohena/el-easydraw
;; Keywords: Graphics,Drawing,SVG

;; Package-Version: 1.2.1
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

(require 'mwheel)
(require 'cl-lib)
(require 'eieio)
(require 'edraw-widget)
(require 'edraw-math)
(require 'edraw-path)
(require 'edraw-dom-svg)
(require 'edraw-util)
(require 'edraw-color-picker)
(require 'edraw-property-editor)

(autoload 'edraw-convert-file-to-edraw-svg "edraw-import")
(autoload 'edraw-shape-picker-open "edraw-shape-picker")
(declare-function edraw-shape-picker-connect "edraw-shape-picker")
(declare-function edraw-shape-picker-disconnect "edraw-shape-picker")
(declare-function edraw-shape-picker-selected-args "edraw-shape-picker")
(declare-function image-size "image.c")
(declare-function image-flush "image.c")


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


(defconst edraw-package-default-document-properties
  '((width . 560)
    (height . 420)
    (background . "#fff"))
  "Default document properties provided by the package.

More properties may be added to this list in the future.")

(defcustom edraw-default-document-properties
  nil
  "Default document properties.

Values of properties not specified in this variable are retrieved
from the `edraw-package-default-document-properties' variable."
  :group 'edraw-editor
  :type `(edraw-attribute-alist
          :tag "Properties"
          :show-all-attributes t
          ;;:default-attributes ,edraw-package-default-document-properties
          (width (integer :tag "Width" :value 560))
          (height (integer :tag "Height" :value 420))
          (background (edraw-web-color :tag "Background Color" :value "#fff"))))

(defun edraw-default-document-property-get (key)
  (cdr (or (assq key edraw-default-document-properties)
           (assq key edraw-package-default-document-properties))))



(defcustom edraw-editor-share-default-shape-properties nil
  "non-nil means that the editors change edraw-default-shape-properties directly."
  :group 'edraw-editor
  :type 'boolean)

(defconst edraw-package-default-stroke "#101010"
  "Default line color.

Basically, it is applied to stroke attributes of elements,
but it is applied to fill attributes of line-shaped elements
such as text elements and character-shaped paths.")

(defconst edraw-package-default-fill "#f8f8f8"
  "Default fill color.

This color is used to fill enclosed shapes such as
rectangles. The color must be such that text or lines of the
`edraw-package-default-stroke' color can be placed above it.")

(defconst edraw-package-default-shape-properties
  `((rect
     (fill . ,edraw-package-default-fill)
     (stroke . ,edraw-package-default-stroke)
     (stroke-width . 2))
    (ellipse
     (fill . ,edraw-package-default-fill)
     (stroke . ,edraw-package-default-stroke)
     (stroke-width . 2))
    (path
     (fill . "none")
     (stroke . ,edraw-package-default-stroke)
     (stroke-width . 2)
     ;;(marker-end . "arrow")
     )
    (text
     (fill . ,edraw-package-default-stroke) ;; Not edraw-package-default-fill
     (font-size . 16)
     (font-family . "sans-serif")
     (text-anchor . "middle"))
    (image))
  "Default shape properties provided by the package.

More shape types and properties may be added to this list in the future.")

;; NOTE:
;; Default properties can also be customized in the following ways
;; (for compatibility):
;; (with-eval-after-load "edraw"
;;   (setf (alist-get 'font-family
;;                    (alist-get 'text edraw-default-shape-properties))
;;         "Yu Gothic"))

(defcustom edraw-default-shape-properties
  nil
  "Default shape properties.

Values of properties not specified in this variable are retrieved
from the `edraw-package-default-shape-properties' variable.

If initial defaults are saved as presets, they will take precedence."
  :group 'edraw-editor
  :type `(edraw-attribute-alist
          :tag "Shape Types"
          :show-all-attributes t
          :convert-widget edraw-default-shape-properties--convert-widget))

(defun edraw-default-shape-properties--convert-widget (widget)
  (widget-put
   widget :args
   ;; NOTE: This function cannot be called when determining the :type
   ;; of defcustom. edraw-shape-type-to-create,
   ;; edraw-get-property-info-list, etc. are not defined at that point.
   ;; Also, shape-type may be added later from other packages.
   (mapcar (lambda (shape-type)
             `(,shape-type
               (edraw-properties
                :tag ,(capitalize (symbol-name shape-type))
                :prop-info-list ,(edraw-get-property-info-list
                                  (edraw-shape-type-to-class
                                   shape-type)))))
           (mapcar #'car edraw-package-default-shape-properties)))
  widget)

(defun edraw-default-shape-properties--get ()
  "Return the default shape properties alist."
  (let ((dsprops
         (if edraw-editor-share-default-shape-properties
             ;;@todo observe changes made by other editors
             edraw-default-shape-properties
           (copy-tree edraw-default-shape-properties))))
    ;; Apply package default
    (cl-loop for (shape-type . props) in edraw-package-default-shape-properties
             do
             (cl-loop for (key . value) in props
                      unless (assq key (alist-get shape-type dsprops))
                      do (setf (alist-get key (alist-get shape-type dsprops))
                               ;; Protect package-default
                               (copy-tree value))))
    dsprops))

(defconst edraw-package-default-shape-properties-for-each-tool
  `(;; (<tool-class-symbol> . shape-type-default | <properties-alist>)
    (edraw-editor-tool-rect . shape-type-default)
    (edraw-editor-tool-ellipse . shape-type-default)
    (edraw-editor-tool-path . shape-type-default)
    (edraw-editor-tool-freehand . shape-type-default)
    ;; Example of specifying line width and color of path only for
    ;; handwriting tool:
    ;;(edraw-editor-tool-freehand . ((stroke . "#222") (stroke-width . 1) (fill . "none")))
    (edraw-editor-tool-text . shape-type-default)
    (edraw-editor-tool-image . shape-type-default)
    (edraw-editor-tool-custom-shape . ((fill . ,edraw-package-default-fill)
                                       (stroke . ,edraw-package-default-stroke)
                                       (stroke-width . 2)))
    (edraw-editor-tool-generator . shape-type-default))
  "Default shape properties for each tool provided by the package.

More tools may be added to this list in the future.")

(defcustom edraw-default-shape-properties-for-each-tool
  nil
  "Default properties for shapes created by each tool.

If properties are not specified, the
`edraw-default-shape-properties' variable specifications will be
used.

If initial defaults are saved as presets, they will take precedence."
  :group 'edraw-editor
  :type `(edraw-attribute-alist
          :tag "Tools"
          :show-all-attributes t
          :convert-widget
          edraw-default-shape-properties-for-each-tool--convert-widget))

(defun edraw-default-shape-properties-for-each-tool--convert-widget (widget)
  (widget-put
   widget :args
   ;; NOTE: This function cannot be called when determining the :type
   ;; of defcustom. edraw-shape-type-to-create,
   ;; edraw-get-property-info-list, etc. are not defined at that point.
   ;; Also, tool-class may be added later from other packages.
   (cl-loop for (tool-class . _)
            in edraw-package-default-shape-properties-for-each-tool
            for shape-type = (edraw-shape-type-to-create tool-class)
            when shape-type
            collect
            `(,tool-class
              (choice
               :value shape-type-default ;;default value
               :tag ,(edraw-name tool-class)
               (const :tag "Use shape type defaults" shape-type-default)
               (edraw-properties
                :tag "Specify properties"
                :prop-info-list ,(edraw-get-property-info-list
                                  (edraw-shape-type-to-class
                                   shape-type)))))))
  widget)

(defun edraw-default-shape-properties-for-each-tool--get ()
  (let ((tool-props
         (if edraw-editor-share-default-shape-properties
             ;;@todo observe changes made by other editors
             edraw-default-shape-properties-for-each-tool
           (copy-tree edraw-default-shape-properties-for-each-tool))))
    (cl-loop for (tool-class . value)
             in edraw-package-default-shape-properties-for-each-tool
             unless (assq tool-class tool-props)
             do (setf (alist-get tool-class tool-props)
                      ;; Protect package-default
                      (copy-tree value)))
    tool-props))



(defcustom edraw-default-marker-properties
  ;; '(("arrow" (markerWidth . "20") (markerHeight . "10")))
  nil
  "Default marker properties.

If initial defaults are saved as presets, they will take precedence."
  :group 'edraw-editor
  :type `(edraw-attribute-alist
          :tag "Marker Types"
          :show-all-attributes t
          :args
          ,(cl-loop for marker-type in (edraw-svg-marker-type-all)
                    collect
                    `(,marker-type
                      (edraw-properties
                       :tag ,marker-type
                       :prop-info-list ,(edraw-svg-marker-prop-info-list
                                         marker-type))))))

(defun edraw-default-marker-properties--get ()
  (copy-tree edraw-default-marker-properties))



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

(defcustom edraw-editor-move-distance-by-arrow-key
  '((nil . 1)
    ((shift) . 10)
    ((control) . snap)
    ((control shift) . 0.125))
  "Amount of object movement using arrow keys.

Specify with an alist whose key is the modifier key list and the
amount of movement is the value.

If the symbol `grid' is specified as the value, the grid interval
setting will be used.

If the symbol `snap' is specified as the value, move to next grid
line.

Used by the `edraw-editor-move-selected-by-arrow-key' command."
  :group 'edraw-editor
  :type '(list :tag "Modifiers-Distance" :extra-offset 2
               (cons :format "No modifiers:\n    %v"
                     (const :format "" nil)
                     (choice (number :tag "Pixels")
                             (const :tag "Grid Intervals" grid)
                             (const :tag "Next Grid Line" snap)))
               (cons  :format "Shift:\n    %v"
                      (const :format "" (shift))
                      (choice (number :tag "Pixels")
                              (const :tag "Grid Intervals" grid)
                              (const :tag "Next Grid Line" snap)))
               (cons  :format "Control:\n    %v"
                      (const :format "" (control))
                      (choice (number :tag "Pixels")
                              (const :tag "Grid Intervals" grid)
                              (const :tag "Next Grid Line" snap)))
               (cons :format "Control+Shift:\n    %v"
                     (const :format "" (control shift))
                     (choice (number :tag "Pixels")
                             (const :tag "Grid Intervals" grid)
                             (const :tag "Next Grid Line" snap)))))

(defcustom edraw-editor-scroll-distance-by-arrow-key
  '((nil . 50)
    ((shift) . 100)
    ((control) . 200)
    ((control shift) . 400))
  "Amount of scrolling using arrow keys.

Specify with an alist whose key is the modifier key list and the
amount of scrolling is the value.

If the symbol `grid' is specified as the value, the grid interval
setting will be used.

Used by the `edraw-editor-scroll-by-arrow-key' command."
  :group 'edraw-editor
  :type '(list :tag "Modifiers-Distance" :extra-offset 2
               (cons :format "No modifiers:\n    %v"
                     (const :format "" nil)
                     (choice (number :tag "Pixels")
                             (const :tag "Grid Intervals" grid)))
               (cons  :format "Shift:\n    %v"
                      (const :format "" (shift))
                      (choice (number :tag "Pixels")
                              (const :tag "Grid Intervals" grid)))
               (cons  :format "Control:\n    %v"
                      (const :format "" (control))
                      (choice (number :tag "Pixels")
                              (const :tag "Grid Intervals" grid)))
               (cons :format "Control+Shift:\n    %v"
                     (const :format "" (control shift))
                     (choice (number :tag "Pixels")
                             (const :tag "Grid Intervals" grid)))))

(defcustom edraw-editor-default-transparent-bg-visible t
  "non-nil means the transparent background is colored by default."
  :group 'edraw-editor
  :type 'boolean)
(defcustom edraw-editor-transparent-bg-color1 "#ffffff"
  "The first color of the transparent background."
  :group 'edraw-editor
  :type '(edraw-web-color :tag "Color"))
(defcustom edraw-editor-transparent-bg-color2 "#cccccc"
  "The second color of the transparent background."
  :group 'edraw-editor
  :type '(choice (edraw-web-color :tag "Color")
                 (const :tag "Not used (Use color1 only)" nil)))
(defcustom edraw-editor-transparent-bg-grid-size 8
  "The grid interval of the transparent background."
  :group 'edraw-editor
  :type 'number)

(defcustom edraw-editor-auto-view-enlargement-max-size
  (cons
   (list 'window 0.9375 0 320 1024)
   (list 'window 0.625 0 320 1024))
  "When zoomed, editing view will automatically resize up to this size.

nil : disable auto view enlargement.

(W . H) : Auto enlarge to the size specified by W and H as the upper limit.
          Each values can be one of the following:

- Integer : Number of pixels (before scaling for the whole editor)
- Float : Ratio to frame size
- (window RATIO MINUS MIN MAX) : clamp(WINDOWSIZE * RATIO - MINUS, MIN, MAX)

Note: All pixel counts are before applying the editor-wide scaling factor."
  :group 'edraw-editor
  :type '(choice (const :tag "Disable auto view enlargement" nil)
                 (cons
                  :tag "Maximum Size"
                  (choice (integer :tag "Width")
                          (float :tag "Ratio to frame width" :value 1.0)
                          (list
                           :tag "Calculate from window width"
                           (const :format "" window)
                           (float :tag "Ratio to window width" :value 1.0)
                           (integer :tag "Minus pixels")
                           (choice :tag "Minimum"
                                   (const :tag "0" nil)
                                   (integer :tag "Pixels"))
                           (choice :tag "Maximum"
                                   (const :tag "max-image-size" nil)
                                   (integer :tag "Pixels" :value 2048))))
                  (choice (integer :tag "Height")
                          (float :tag "Ratio to frame height" :value 1.0)
                          (list
                           :tag "Calculate from window height"
                           (const :format "" window)
                           (float :tag "Ratio to window height" :value 1.0)
                           (integer :tag "Pixels")
                           (choice :tag "Minimum"
                                   (const :tag "0" nil)
                                   (integer :tag "Pixels"))
                           (choice :tag "Maximum"
                                   (const :tag "max-image-size" nil)
                                   (integer :tag "Pixels" :value 2048)))))))

(defcustom edraw-editor-show-help-when-selecting-tool-p t
  "Non-nil means display help for tool when it is selected."
  :group 'edraw-editor
  :type 'boolean)

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
    (define-key km [down-mouse-3] 'edraw-editor-dispatch-event) ;; Disable context-menu-mode
    (define-key km [mouse-3] 'edraw-editor-dispatch-event)
    (define-key km (vector (intern (format "C-%s" mouse-wheel-up-event))) 'edraw-editor-zoom-out-by-mouse)
    (define-key km (vector (intern (format "C-%s" mouse-wheel-down-event))) 'edraw-editor-zoom-in-by-mouse)
    (define-key km " " 'edraw-editor-interactive-scroll-and-zoom)
    (when (version<= "28" emacs-version)
      (define-key km [remap context-menu-open] 'edraw-editor-popup-context-menu))
    (define-key km (kbd "<apps>") 'edraw-editor-popup-context-menu)
    (define-key km (kbd "<menu>") 'edraw-editor-popup-context-menu)
    (define-key km (kbd "S-<f10>") 'edraw-editor-popup-context-menu)
    (define-key km "m" 'edraw-editor-popup-context-menu)
    ;;(define-key km "m" 'edraw-editor-main-menu)
    (define-key km "s" 'edraw-editor-select-tool-select)
    (define-key km "r" 'edraw-editor-select-tool-rect)
    (define-key km "e" 'edraw-editor-select-tool-ellipse)
    (define-key km "a" 'edraw-editor-select-tool-path)
    (define-key km "f" 'edraw-editor-select-tool-freehand)
    (define-key km "t" 'edraw-editor-select-tool-text)
    (define-key km "i" 'edraw-editor-select-tool-image)
    (define-key km "u" 'edraw-editor-select-tool-custom-shape)
    (define-key km "x" 'edraw-editor-select-tool-generator)
    (define-key km "F" 'edraw-editor-edit-tool-default-fill)
    (define-key km "S" 'edraw-editor-edit-tool-default-stroke)
    (define-key km "?" 'edraw-editor-show-help-for-selected-tool)
    (define-key km "#" 'edraw-editor-toggle-grid-visible)
    (define-key km (kbd "M-#") 'edraw-editor-set-grid-interval)
    (define-key km "\"" 'edraw-editor-toggle-transparent-bg-visible)
    (define-key km "db" 'edraw-editor-set-background)
    (define-key km "dr" 'edraw-editor-set-size)
    (define-key km "dc" 'edraw-editor-crop)
    (define-key km "dvb" 'edraw-editor-set-viewbox)
    (define-key km "dtt" 'edraw-editor-translate-all-shapes)
    (define-key km "dts" 'edraw-editor-scale-all-shapes)
    (define-key km "dtr" 'edraw-editor-rotate-all-shapes)
    (define-key km "deb" 'edraw-editor-export-to-buffer)
    (define-key km "def" 'edraw-editor-export-to-file)
    (define-key km "dif" 'edraw-editor-import-from-file)
    (define-key km "ded" 'edraw-editor-export-debug-svg-to-buffer)
    (define-key km "d!!" 'edraw-editor-clear-temporary-states-all-shapes)
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
    (define-key km "D" 'edraw-editor-duplicate-selected-shapes)
    (define-key km "!v" 'edraw-editor-toggle-visibility-selected)
    (define-key km "!p" 'edraw-editor-toggle-pickability-selected)
    (define-key km "!!" 'edraw-editor-clear-temporary-states-selected)
    (define-key km [remap yank] 'edraw-editor-paste-and-select)
    (define-key km [remap kill-region] 'edraw-editor-cut-selected-shapes)
    (define-key km [remap kill-ring-save] 'edraw-editor-copy-selected-shapes)
    ;; It's too complicated to remember, so use C-y, C-w, M-w
    ;; (define-key km (kbd "C-c C-x C-y") 'edraw-editor-paste-and-select)
    ;; (define-key km (kbd "C-c C-x C-w") 'edraw-editor-cut-selected-shapes)
    ;; (define-key km (kbd "C-c C-x M-w") 'edraw-editor-copy-selected-shapes)
    (define-key km "Tt" 'edraw-editor-translate-selected)
    (define-key km "Ts" 'edraw-editor-scale-selected)
    (define-key km "Tr" 'edraw-editor-rotate-selected)
    (define-key km "Ti" 'edraw-editor-transform-selected-interactive)
    ;; -transform-selected-interactive cannot move only the selected point.
    ;; Tt, Ts, Tr can only move a selected anchor/handle point.
    ;; (define-key km "T" 'edraw-editor-transform-selected-interactive)
    (define-key km (kbd "C-t") 'edraw-editor-transform-selected-interactive)
    (define-key km "g" 'edraw-editor-group-selected-shapes)
    (define-key km "G" 'edraw-editor-ungroup-selected-shapes)
    (define-key km "pf" 'edraw-editor-edit-fill-selected)
    (define-key km "ps" 'edraw-editor-edit-stroke-selected)
    (define-key km "pp" 'edraw-editor-edit-properties-of-selected-shapes)
    (define-key km "p<" 'edraw-editor-set-marker-start-next-selected)
    (define-key km "p>" 'edraw-editor-set-marker-end-next-selected)
    (define-key km (kbd "M-RET") 'edraw-editor-edit-properties-of-selected-shapes)
    (define-key km (kbd "<delete>") 'edraw-editor-delete-selected)
    (define-key km (kbd "<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "S-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "S-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "S-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "S-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "C-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "C-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "C-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "C-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "C-S-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "C-S-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "C-S-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "C-S-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-S-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-S-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-S-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-S-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-C-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-C-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-C-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-C-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-C-S-<left>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-C-S-<right>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-C-S-<up>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "M-C-S-<down>") 'edraw-editor-move-selected-by-arrow-key)
    (define-key km (kbd "}") 'edraw-editor-bring-selected-to-front)
    (define-key km (kbd "]") 'edraw-editor-bring-selected-forward)
    (define-key km (kbd "[") 'edraw-editor-send-selected-backward)
    (define-key km (kbd "{") 'edraw-editor-send-selected-to-back)
    (define-key km (kbd "M-]") 'edraw-editor-select-next-shape)
    (define-key km (kbd "M-[") 'edraw-editor-select-previous-shape)
    (define-key km (kbd "TAB") 'edraw-editor-select-next-shape)
    (define-key km (kbd "S-TAB") 'edraw-editor-select-previous-shape)
    (define-key km (kbd "<backtab>") 'edraw-editor-select-previous-shape)
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

;;;;; Editor - Color

(defclass edraw-editor-recent-colors ()
  ((ui-state :initarg :ui-state)))

(cl-defmethod edraw-get-recent-colors ((obj edraw-editor-recent-colors))
  (with-slots (ui-state) obj
    (edraw-ui-state-get ui-state 'color-picker 'recent-colors
                        edraw-color-picker-recent-colors-default)))

(cl-defmethod edraw-set-recent-colors ((obj edraw-editor-recent-colors)
                                       (colors list))
  (with-slots (ui-state) obj
    (edraw-ui-state-set ui-state 'color-picker 'recent-colors colors)
    (edraw-ui-state-save ui-state)
    obj))

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

    (edraw-editor :overlay overlay :svg svg)))

(defclass edraw-editor ()
  ((overlay :initarg :overlay :initform nil :reader edraw-overlay)
   (keymap :initarg :keymap :initform (identity edraw-editor-map))
   (svg :initarg :svg :initform nil)
   (svg-document-size)
   (svg-document-viewbox)
   (svg-document-original-width)
   (svg-document-original-height)
   (svg-document-original-viewbox)
   (svg-document-comments :initform nil) ;; (PRE-COMMENTS . POST-COMMENTS)
   (deftbl)
   (document-writer :initarg :document-writer :initform nil)
   (document-writer-accepts-top-level-comments-p
    :initarg :document-writer-accepts-top-level-comments-p :initform nil)
   (menu-filter :initarg :menu-filter :initform nil)

   (image-base-uri :initform nil)
   (image-scale
    :initform (image-compute-scaling-factor
               (or edraw-editor-image-scaling-factor image-scaling-factor))
    :type number)
   (image :initform nil)
   (image-toolbar :initform nil)
   (image-update-timer :initform nil)
   (invalid-ui-parts :initform nil)
   (scroll-transform :initform (list 0 0 1)) ;;dx dy scale
   (view-size :initform nil) ;; nil means document size
   (ui-state
    :initarg :ui-state :initform (edraw-ui-state-object-default)
    :documentation
    "Stores information that is shared across editing sessions (may
 change slot names in the future). Settings are shared with other
 editors open at the same time without recreating the editor.")
   (settings
    :initform (list (cons 'grid-visible
                          edraw-editor-default-grid-visible)
                    (cons 'grid-interval
                          edraw-editor-default-grid-interval)
                    (cons 'transparent-bg-visible
                          edraw-editor-default-transparent-bg-visible)
                    (cons 'view-size-spec nil) ;;User specified view size
                    (cons 'transform-method 'auto))
    :documentation
    "Stores settings that should be kept separately for each editor.")
   (selection-ui-visible :initform t)
   (extra-properties
    :initform nil
    :documentation
    "A storage area for users of the edraw-editor class. It is never
used by the edraw-editor class.")
   (default-shape-properties
    :initform (identity (edraw-default-shape-properties--get)))
   (default-shape-properties-for-each-tool
    :initform (identity (edraw-default-shape-properties-for-each-tool--get)))
   (default-marker-properties
    :initform (identity (edraw-default-marker-properties--get)))
   (tool :initform nil :type (or null edraw-editor-tool))
   (selected-shapes :initform nil :type list)
   (selected-anchor :initform nil :type (or null edraw-shape-point))
   (selected-handle :initform nil :type (or null edraw-shape-point))
   (modified-p :initform nil)
   (undo-list :initform nil)
   (redo-list :initform nil)
   (hooks
    :initform (list
               (cons 'change (edraw-hook-make))
               (cons 'selection-change (edraw-hook-make)))))
  "Editor")

(cl-defmethod edraw-initialize ((editor edraw-editor))
  "Do nothing.

This method is defined for backward compatibility.

edraw-editor initialization is now called automatically."
  editor)

(cl-defmethod initialize-instance :after ((editor edraw-editor) &rest _args)
  (edraw-editor-initialize editor))

(defun edraw-editor-initialize (editor)
  (edraw-editor-clear-undo-vars)
  (edraw-editor-clear-modified-vars)

  (edraw-define-hook-type editor 'before-image-update)

  (edraw-initialize-default-shape-properties editor)
  (edraw-initialize-default-marker-properties editor)

  (with-slots (overlay keymap) editor
    (when edraw-editor-disable-line-prefix ;;for Emacs's line-prefix bug
      (overlay-put overlay 'line-prefix "")
      (overlay-put overlay 'wrap-prefix ""))
    (overlay-put overlay 'edraw-editor editor)
    (overlay-put overlay 'keymap keymap)
    ;;(overlay-put overlay 'evaporate t)
    (overlay-put overlay 'pointer 'arrow)
    (overlay-put overlay 'help-echo
                 (lambda (_window _object _pos) nil)) ;;Suppress org link's echo
    (overlay-put overlay 'face 'default)) ;;Suppress org link's underline

  (edraw-initialize-svg editor)
  (edraw-reset-scroll-and-zoom editor)
  (edraw-update-image editor)

  (edraw-update-toolbar editor)
  (edraw-select-tool-default editor)
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
      (edraw-flush-image editor)
      (edraw-flush-toolbar-image editor)
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

(defvar edraw-editor-defcmd--interactive-p nil)

(defun edraw-editor-defcmd-interactive-p ()
  edraw-editor-defcmd--interactive-p)

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
  (declare (indent 2)
           (debug
            (&define                    ; this means we are defining something
             [&name [sexp             ;Allow (setf ...) additionally to symbols.
                     [&rest cl-generic--method-qualifier-p] ;qualifiers
                     listp]             ; arguments
                    cl--generic-edebug-make-name nil]
             lambda-doc                 ; documentation string
             def-body)))
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
           (let ((edraw-editor-defcmd--interactive-p
                  (called-interactively-p 'interactive)))
             (edraw-editor-call-at-point (quote ,method-symbol) editor args)))
         ;; Define generic
         (cl-defgeneric ,method-symbol ,(cons 'OBJECT (cdr arg-list)))
         ;; Define method (edraw-* ((editor edraw-editor) ...) ...)
         (cl-defmethod ,method-symbol ,arg-list
           ,@rest
           )))))

;;;;; Editor - Property Editor

(defun edraw-editor-open-property-editor (editor target)
  (edraw-property-editor-open
   target
   `((image-scale . ,edraw-editor-image-scaling-factor)
     (recent-colors . ,(edraw-editor-recent-colors
                        :ui-state (oref editor ui-state)))
     (marker-defaults . ,(oref editor default-marker-properties))
     (ui-state . ,(oref editor ui-state))
     )))

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

(defmacro edraw-undo-data (type func-args) `(cons ,type ,func-args))
(defmacro edraw-undo-data-type (data) `(car ,data))
(defmacro edraw-undo-data-func-args (data) `(cdr ,data))
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

To call multiple functions at once, specify #\\='edraw-call-each-args as FUNCTION.

This function deletes all redo data."
  (with-slots (undo-list redo-list) editor
    (unless edraw-editor-redo-in-progress
      (setq redo-list nil))
    (push (edraw-undo-data type data) undo-list)
    ;; Discard undo data exceeding the limit.
    (unless (or (> edraw-editor-undo-group-level 0)
                edraw-editor-inhibit-discarding-undo-data)
      (when-let ((cell (nthcdr edraw-editor-undo-limit undo-list)))
        (setcdr cell nil)))))

;; No Undo Data

(defmacro edraw-editor-with-no-undo-data (&rest body)
  (declare (indent 0) (debug (body)))
  `(let ((edraw-editor-inhibit-make-undo-data t))
     ,@body))

;; Undo Group

(cl-defmethod edraw-undo-block-begin ((editor edraw-editor))
  (let ((undo-backup (list
                      (oref editor undo-list)
                      edraw-editor-inhibit-make-undo-data
                      edraw-editor-inhibit-discarding-undo-data)))
    (oset editor undo-list nil)
    (setq edraw-editor-inhibit-make-undo-data nil) ;; Record undo data
    (setq edraw-editor-inhibit-discarding-undo-data t) ;; Keep all undo data
    (cl-incf edraw-editor-undo-group-level)
    undo-backup))

(cl-defmethod edraw-undo-block-end ((editor edraw-editor) undo-backup)
  (let ((undo-data (edraw-combine-undo-list (oref editor undo-list))))
    (cl-decf edraw-editor-undo-group-level)
    (setq edraw-editor-inhibit-discarding-undo-data (nth 2 undo-backup))
    (setq edraw-editor-inhibit-make-undo-data (nth 1 undo-backup))
    (oset editor undo-list (nth 0 undo-backup))
    undo-data))

(defmacro edraw-make-undo-group (editor type &rest body)
  "Combine undo data pushed in BODY into one."
  (declare (indent 2) (debug (sexp sexp body)))
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
     (mapcar (lambda (data) (edraw-undo-data-func-args data)) undo-list)))
   ;; single data
   (undo-list
    (edraw-undo-data-func-args (car undo-list)));; strip type
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
  (declare (indent 1) (debug (sexp body)))
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
  (declare (indent 1) (debug (sexp body)))
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
  (with-slots (svg
               svg-document-size
               svg-document-viewbox
               svg-document-original-width
               svg-document-original-height
               svg-document-original-viewbox
               svg-document-comments deftbl)
      editor
    ;; Strip top-level comments
    (let ((svg-comments (edraw-dom-split-top-nodes svg)))
      (setq svg (car svg-comments))
      (setq svg-document-comments (cdr svg-comments)))

    ;; Check specified SVG (Only accepts nil or svg element)
    (when (and svg
               (not (edraw-dom-tag-eq svg 'svg)))
      (warn "The root of the DOM passed to edraw-editor as SVG is not an SVG element.")
      (setq svg nil))

    ;; Create default SVG
    (when (null svg)
      (setq svg (edraw-create-document-svg)))

    ;; Backup SVG Attributes
    (setq svg-document-original-width (dom-attr svg 'width))
    (setq svg-document-original-height (dom-attr svg 'height))
    (setq svg-document-original-viewbox (dom-attr svg 'viewBox))

    ;; SVG View Box
    (setq svg-document-viewbox (edraw-svg-parse-viewbox
                                svg-document-original-viewbox))

    ;; SVG Editor Size
    (setq svg-document-size
          (let ((w (edraw-svg-attr-length svg 'width))
                (h (edraw-svg-attr-length svg 'height)))
            (when (or (null w) (null h))
              (warn "Width and height are not specified for svg element. Apply default document size."))
            (cons
             (or w (edraw-default-document-property-get 'width))
             (or h (edraw-default-document-property-get 'height)))))

    ;; #edraw-defs
    (if-let ((defs-element (edraw-dom-get-by-id svg edraw-editor-svg-defs-id)))
        (setq deftbl (edraw-svg-deftbl-from-dom
                      defs-element
                      (edraw-dom-get-by-id svg edraw-editor-svg-body-id)))
      (setq deftbl (edraw-svg-defs-as-deftbl edraw-editor-svg-defs-id))
      (edraw-dom-insert-first svg
                              (edraw-svg-deftbl-defs-element deftbl)))

    ;; #edraw-body
    (edraw-dom-get-or-create svg 'g edraw-editor-svg-body-id)

    ;; Make it possible to retrieve parents of elements
    (edraw-dom-update-parent-links svg)

    ;; Restore Point Connections
    (edraw-restore-point-connections editor)))

(cl-defmethod edraw-restore-point-connections ((editor edraw-editor))
  (edraw-restore-point-connections--dom editor (edraw-svg-body editor)))

(cl-defmethod edraw-restore-point-connections--dom ((editor edraw-editor) dom)
  (edraw-editor-with-no-undo-data
    (dolist (element
             (dom-elements dom 'data-edraw-point-connections ""))
      (when-let ((shape (edraw-shape-from-element element editor t)))
        (edraw-restore-point-connections shape)))))

(defun edraw-create-document-svg (&optional width height background children)
  (let* ((width (or width (edraw-default-document-property-get 'width)))
         (height (or height (edraw-default-document-property-get 'height)))
         (background (or background (edraw-default-document-property-get 'background)))
         (svg (edraw-svg-create width height)))
    (when (and background (not (equal background "none")))
      (edraw-svg-rect 0 0 width height
                      :parent svg
                      :id edraw-editor-svg-background-id
                      :stroke "none"
                      :fill background))

    ;; #edraw-body
    ;; If children='(nil), create #edraw-body only.
    (when children
      (let ((body (edraw-dom-get-or-create svg 'g edraw-editor-svg-body-id)))
        (dolist (child children)
          (when child ;;Enable '(nil) to create body node only.
            (edraw-dom-append-child body child)))))
    svg))

(defun edraw-get-document-body (svg)
  ;; NOTE: SVG may contain top-level comments. But `edraw-dom-get-by-id'
  ;;       works fine, so don't use `edraw-dom-split-top-nodes'.
  (edraw-dom-get-by-id
   svg
   edraw-editor-svg-body-id))

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
  (declare (indent 0) (debug (body)))
  `(let ((edraw-editor-inhibit-make-undo-data t)
         (edraw-editor-keep-modified-flag t))
     ,@body))

;;;;;;; Write

(edraw-editor-defcmd edraw-save ((editor edraw-editor))
  (with-slots (document-writer svg) editor
    (when (and document-writer
               (edraw-modified-p editor))
      (let ((doc-svg
             (edraw-document-svg
              editor
              (oref editor document-writer-accepts-top-level-comments-p))))
        (prog1
            ;;signal error if failed
            (funcall document-writer doc-svg)
          (edraw-set-modified-p editor nil))))))

(cl-defmethod edraw-document-svg ((editor edraw-editor)
                                  &optional with-top-level-comments-p)
  (with-slots (svg
               svg-document-original-width
               svg-document-original-height
               svg-document-original-viewbox
               svg-document-comments)
      editor
    (let ((doc-svg (edraw-dom-copy-tree svg)))
      (edraw-editor-remove-ui-elements-from-svg doc-svg)
      (edraw-editor-remove-scroll-transform doc-svg)
      (edraw-editor-remove-root-transform doc-svg
                                          svg-document-original-width
                                          svg-document-original-height
                                          svg-document-original-viewbox)

      (when-let ((defs (edraw-dom-get-by-id doc-svg edraw-editor-svg-defs-id)))
        ;; Remove unreferenced definition elements
        ;;
        ;; Definition elements such as markers are usually deleted
        ;; automatically. However, if an element with a reference
        ;; attribute is removed from the DOM tree, the reference
        ;; information is not removed from deftbl and the definition
        ;; element may remain. This problem is tolerated for now,
        ;; considering Undo and temporary removal of elements.
        ;;
        ;; `edraw-svg-deftbl-from-dom' includes
        ;; `edraw-svg-deftbl-remove-unreferenced-definitions', so
        ;; unreferenced definition elements are removed.
        (edraw-svg-deftbl-from-dom defs
                                   (edraw-dom-get-by-id
                                    doc-svg edraw-editor-svg-body-id))

        ;; Remove empty defs
        (when (null (dom-children defs))
          (edraw-dom-remove-node doc-svg defs)))

      ;; Add xmlns
      (dom-set-attribute doc-svg 'xmlns "http://www.w3.org/2000/svg")
      ;; Adjust xmlns:xlink and xlink:href
      (edraw-svg-compatibility-fix doc-svg)

      (if with-top-level-comments-p
          ;; Append pre&post comments
          (edraw-dom-merge-top-nodes doc-svg
                                     (car svg-document-comments)
                                     (cdr svg-document-comments))
        doc-svg))))

(edraw-editor-defcmd edraw-export-to-buffer ((editor edraw-editor))
  (pop-to-buffer "*Easy Draw SVG*")
  (erase-buffer)
  (edraw-svg-print (edraw-document-svg editor t) nil nil 0)
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
      (edraw-svg-print (edraw-document-svg editor t) nil nil 0))))

(edraw-editor-defcmd edraw-export-debug-svg-to-buffer ((editor edraw-editor))
  (pop-to-buffer "*Easy Draw SVG*")
  (erase-buffer)
  (edraw-svg-print (oref editor svg) nil nil 0)
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

  (edraw-select-tool-default editor))

;;;;;; Editor - Document - Size

(cl-defmethod edraw-width ((editor edraw-editor))
  (car (oref editor svg-document-size)))

(cl-defmethod edraw-height ((editor edraw-editor))
  (cdr (oref editor svg-document-size)))

(cl-defmethod edraw-viewbox-left ((editor edraw-editor))
  (if-let ((vbox (oref editor svg-document-viewbox))) (nth 0 vbox) 0))

(cl-defmethod edraw-viewbox-top ((editor edraw-editor))
  (if-let ((vbox (oref editor svg-document-viewbox))) (nth 1 vbox) 0))

(cl-defmethod edraw-viewbox-right ((editor edraw-editor))
  (if-let ((vbox (oref editor svg-document-viewbox)))
      (+ (nth 0 vbox) (nth 2 vbox))
    (edraw-width editor)))

(cl-defmethod edraw-viewbox-bottom ((editor edraw-editor))
  (if-let ((vbox (oref editor svg-document-viewbox)))
      (+ (nth 1 vbox) (nth 3 vbox))
    (edraw-height editor)))

(cl-defmethod edraw-viewbox-width ((editor edraw-editor))
  (if-let ((vbox (oref editor svg-document-viewbox)))
      (nth 2 vbox)
    (edraw-width editor)))

(cl-defmethod edraw-viewbox-height ((editor edraw-editor))
  (if-let ((vbox (oref editor svg-document-viewbox)))
      (nth 3 vbox)
    (edraw-height editor)))

(edraw-editor-defcmd edraw-set-size ((editor edraw-editor) width height)
  (interactive
   (let* ((editor (edraw-current-editor))
          (default-w (edraw-width editor))
          (default-h (edraw-height editor))
          (units
           `(("%" . ,(lambda (n def) (round (/ (* n def) 100.0))))
             ("px" . 1)))
          (width (edraw-read-number-with-unit
                  (edraw-msg "Document Width [px|%]: ") default-w
                  units #'integerp))
          (height (edraw-read-number-with-unit
                   (edraw-msg "Document Height [px|%]: ") default-h
                   units #'integerp)))
     (list editor width height)))
  (with-slots (svg-document-size
               svg-document-original-width
               svg-document-original-height)
      editor
    (let* ((width-str (edraw-to-string width))
           (height-str (edraw-to-string height))
           (width (string-to-number width-str)) ;; Avoid numerical errors
           (height (string-to-number height-str)) ;; Avoid numerical errors
           (old-width (car svg-document-size))
           (old-height (cdr svg-document-size)))
      (when (or (/= width old-width)
                (/= height old-height))
        (edraw-push-undo
         editor 'document-size
         (list 'edraw-set-size editor old-width old-height))
        (setq svg-document-size (cons width height))
        (setq svg-document-original-width width-str)
        (setq svg-document-original-height height-str)
        (edraw-update-background editor) ;; Update Document Background
        (edraw-invalidate-ui-parts editor 'all) ;; Update <svg width= height=> etc...
        (edraw-on-document-changed editor 'document-size)))))

;;;;;; Editor - Document - Crop

(defun edraw-editor-read-rectangle-interactively (prompt &optional editor)
  (let ((editor (or editor (edraw-current-editor)))
        rect)
    (while (null rect)
      (let ((event (read-event prompt)))
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
    rect))

(edraw-editor-defcmd edraw-crop ((editor edraw-editor) rect)
  (interactive
   (let ((editor (edraw-current-editor)))
     (list editor
           (edraw-editor-read-rectangle-interactively
            (edraw-msg "Drag the cropping range.")
            editor))))

  (when (edraw-rect-empty-p rect)
    (error (edraw-msg "The crop range is empty.")))

  (edraw-make-undo-group editor 'document-crop
    (unless (edraw-xy-zero-p (edraw-rect-lt rect))
      (edraw-translate-all-shapes editor (edraw-xy-neg (edraw-rect-lt rect))))
    (edraw-set-size editor (edraw-rect-width rect) (edraw-rect-height rect))))

;;;;;; Editor - Document - View Box

(edraw-editor-defcmd edraw-set-viewbox ((editor edraw-editor) new-value)
  (interactive
   (let* ((editor (edraw-current-editor))
          (prompt (edraw-msg "SVG viewBox ([<min-x> <min-y> <width> <height>] or empty): "))
          (initial-input (or (oref editor svg-document-original-viewbox) "")))
     (list
      editor
      (let (input)
        (while (progn
                 (setq input (read-string prompt initial-input))
                 (not (or (string-empty-p input)
                          (edraw-svg-parse-viewbox input)))))
        (if (string-empty-p input) nil input)))))

  (with-slots (svg-document-viewbox svg-document-original-viewbox) editor
    (let ((old-value svg-document-original-viewbox))
      (unless (equal new-value old-value)
        (edraw-push-undo
         editor 'document-viewbox
         (list 'edraw-set-viewbox editor old-value))
        ;; The value set here is reflected to the SVG element by
        ;; `edraw-editor-remove-root-transform' function
        (setq svg-document-original-viewbox new-value)
        (setq svg-document-viewbox (edraw-svg-parse-viewbox new-value))
        (edraw-update-background editor) ;; Update Document Background
        (edraw-invalidate-ui-parts editor 'all) ;; Update <svg width= height=> etc...
        (edraw-on-document-changed editor 'document-viewbox)
        (edraw-reset-view editor)))))

;;;;;; Editor - Document - Background

(defcustom edraw-editor-background-align 'viewbox
  "Target to align the range of the background rectangle.

This setting will be reflected the next time you change the
document size or view box."
  :group 'edraw-editor
  :type '(choice (const viewbox)
                 (const document)))

(cl-defmethod edraw-svg-background ((editor edraw-editor))
  (edraw-dom-get-by-id (oref editor svg) edraw-editor-svg-background-id))

(cl-defmethod edraw-background-left ((editor edraw-editor))
  (pcase edraw-editor-background-align
    ('viewbox (or (nth 0 (oref editor svg-document-viewbox)) 0))
    (_ 0)))

(cl-defmethod edraw-background-top ((editor edraw-editor))
  (pcase edraw-editor-background-align
    ('viewbox (or (nth 1 (oref editor svg-document-viewbox)) 0))
    (_ 0)))

(cl-defmethod edraw-background-right ((editor edraw-editor))
  (pcase edraw-editor-background-align
    ('viewbox (if-let ((vbox (oref editor svg-document-viewbox)))
                  (+ (nth 0 vbox) (nth 2 vbox)) (edraw-width editor)))
    (_ (edraw-width editor))))

(cl-defmethod edraw-background-bottom ((editor edraw-editor))
  (pcase edraw-editor-background-align
    ('viewbox (if-let ((vbox (oref editor svg-document-viewbox)))
                  (+ (nth 1 vbox) (nth 3 vbox)) (edraw-height editor)))
    (_ (edraw-height editor))))

(cl-defmethod edraw-background-width ((editor edraw-editor))
  (pcase edraw-editor-background-align
    ('viewbox (if-let ((vbox (oref editor svg-document-viewbox)))
                  (nth 2 vbox) (edraw-width editor)))
    (_ (edraw-width editor))))

(cl-defmethod edraw-background-height ((editor edraw-editor))
  (pcase edraw-editor-background-align
    ('viewbox (if-let ((vbox (oref editor svg-document-viewbox)))
                  (nth 3 vbox) (edraw-height editor)))
    (_ (edraw-height editor))))

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
                    (:scale-direct . ,(oref editor image-scale))
                    (:recent-colors . ,(edraw-editor-recent-colors
                                        :ui-state (oref editor ui-state)))))
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
        (edraw-dom-add-child-before
         svg
         (edraw-svg-rect 0 0 0 0 :fill fill :id edraw-editor-svg-background-id)
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
     (read-number (edraw-msg "Origin X: ") 0))
   (if aabb
       (edraw-read-origin-number
        "Y"
        `(("top" ,(edraw-rect-top aabb))
          ("center" ,(/ (+ (edraw-rect-top aabb) (edraw-rect-bottom aabb)) 2) t)
          ("bottom" ,(edraw-rect-bottom aabb))))
     (read-number (edraw-msg "Origin Y: ") 0))))

(defun edraw-read-scale-params (aabb &optional origin-xy sx sy)
  (let* ((sx (or sx (edraw-read-number-with-unit
                     (edraw-msg "Scale X [px|%]: ") 1.0
                     `(("%" . ,(lambda (n _def)
                                 (/ n 100.0)))
                       ("px" . ,(lambda (n _def)
                                  (let ((w (edraw-rect-width aabb)))
                                    (if (zerop w)
                                        1.0
                                      (/ n (float w))))))))))
         (sy (or sy (edraw-read-number-with-unit
                     (edraw-msg "Scale Y [px|%]: ") sx
                     `(("%" . ,(lambda (n _def)
                                 (/ n 100.0)))
                       ("px" . ,(lambda (n _def)
                                  (let ((h (edraw-rect-height aabb)))
                                    (if (zerop h)
                                        1.0
                                      (/ n (float h))))))))))
         (_ (when (and (= sx 1) (= sy 1))
              (error (edraw-msg "No need to scale"))))
         (origin-xy (or origin-xy (edraw-read-origin-xy aabb))))
    (list origin-xy sx sy)))

(defmacro edraw-set-scale-params (aabb-expr)
  (declare (debug (sexp)))
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
       ;;(edraw-rect 0 0 (edraw-width editor) (edraw-height editor))
       (edraw-rect (edraw-viewbox-left editor) (edraw-viewbox-top editor)
                   (edraw-viewbox-right editor) (edraw-viewbox-bottom editor))
       ))))

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
  (declare (debug (sexp)))
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
      (edraw-read-rotate-params
       ;;(edraw-rect 0 0 (edraw-width editor) (edraw-height editor))
       (edraw-rect (edraw-viewbox-left editor) (edraw-viewbox-top editor)
                   (edraw-viewbox-right editor) (edraw-viewbox-bottom editor))
       ))))
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

(cl-defmethod edraw-find-shapes-by-xy ((shapes list) xy
                                       &optional pick-forced)
  (when shapes
    (let* ((editor (edraw-get-editor (car shapes)))
           (pick-radius (edraw-find-shapes-by-xy--pick-radius editor)))
      (seq-filter
       (lambda (shape)
         (when (or pick-forced (edraw-visible-and-pickable-p shape))
           (edraw-svg-element-contains-point-p (edraw-element shape) xy
                                               pick-radius t)))
       shapes))))

(cl-defmethod edraw-find-shapes-by-xy ((editor edraw-editor) xy
                                       &optional pick-forced)
  (let ((pick-radius (edraw-find-shapes-by-xy--pick-radius editor))
        result);;front to back
    (dolist (node (dom-children (edraw-svg-body editor)))
      (when (and (edraw-dom-element-p node)
                 (or pick-forced
                     (let ((shape (edraw-shape-from-element-no-create node)))
                       (or (null shape)
                           (edraw-visible-and-pickable-p shape))))
                 (edraw-svg-element-contains-point-p node xy pick-radius t))
        (push (edraw-shape-from-element node editor 'noerror) result)))
    result))

(defconst edraw-find-shapes-by-xy--pick-radius 2)

(cl-defmethod edraw-find-shapes-by-xy--pick-radius ((editor edraw-editor))
  (/ edraw-find-shapes-by-xy--pick-radius (edraw-scroll-scale editor)))

(cl-defmethod edraw-find-shape-by-xy-and-menu ((editor edraw-editor) xy)
  (let ((shapes (edraw-find-shapes-by-xy editor xy)))
    (if (cdr shapes)
        (edraw-popup-shape-selection-menu shapes)
      (car shapes))))

(cl-defmethod edraw-find-shapes-by-rect ((shapes list) rect)
  (seq-filter
   (lambda (shape)
     (when (edraw-visible-and-pickable-p shape)
       (edraw-svg-element-intersects-rect-p (edraw-element shape) rect)))
   shapes))

(cl-defmethod edraw-find-shapes-by-rect ((editor edraw-editor) rect)
  (let (result);;front to back
    (dolist (node (dom-children (edraw-svg-body editor)))
      (when (and (edraw-dom-element-p node)
                 (let ((shape (edraw-shape-from-element-no-create node)))
                   (or (null shape)
                       (edraw-visible-and-pickable-p shape)))
                 (edraw-svg-element-intersects-rect-p node rect))
        (push (edraw-shape-from-element node editor 'noerror) result)))
    result))


;;;;;; Editor - Document - Import

(edraw-editor-defcmd edraw-import-from-file ((editor edraw-editor)
                                             file)
  (interactive
   (list (edraw-current-editor)
         (read-file-name "Import from File: ")))

  (edraw-import-from-edraw-svg editor
                               (edraw-convert-file-to-edraw-svg file)))

(cl-defmethod edraw-import-from-edraw-svg ((editor edraw-editor) dom)
  (let* ((src-svg (car (edraw-dom-split-top-nodes dom)))
         (_ (unless (and (edraw-dom-element-p dom)
                         (eq (edraw-dom-tag dom) 'svg))
              (error (edraw-msg "Not SVG data"))))
         (src-body (edraw-dom-get-by-id src-svg edraw-editor-svg-body-id))
         (_ (unless src-body
              (error (edraw-msg "Not SVG for Edraw"))))
         (_ (when (null (edraw-dom-children src-body))
              (error (edraw-msg "Empty SVG data"))))
         (src-defs (edraw-dom-get-by-id src-svg edraw-editor-svg-defs-id))
         (src-deftbl (edraw-svg-deftbl-from-dom src-defs src-body)))

    ;; Update referrer attributes
    ;; (Transfer the referenced defs from SRC-DEFTBL to deftbl owned by EDITOR)
    ;; @todo Remove invalid (unsupported) marker attributes?
    (edraw-svg-deftbl-update-referrers-in-dom src-body
                                              (oref editor deftbl)
                                              src-deftbl)

    ;; Remove IDs that are already in use
    (let ((dst-body (edraw-svg-body editor)))
      (edraw-dom-remove-attr src-body 'id) ;; Remove id="edraw-body"
      (edraw-dom-do src-body
                    (lambda (node _ancestors)
                      (when (edraw-dom-element-p node)
                        (when-let ((id (dom-attr node 'id)))
                          (when (edraw-dom-get-by-id dst-body id)
                            (lwarn 'edraw-editor :warning
                                   (edraw-msg "ID `%s' is already in use, so remove it")
                                   id)
                            (edraw-dom-remove-attr node 'id)))))))

    ;; Update parent links
    (edraw-dom-update-parent-links src-body)

    ;; Add
    ;; @todo Group? or Flat?
    (edraw-make-undo-group editor 'import
      (let* ((group (edraw-create-shape-default ;;@todo Use -without-default?
                     editor
                     (edraw-svg-body editor)
                     'g))
             (group-element (edraw-element group)))
        (dolist (child (edraw-dom-children src-body))
          (edraw-dom-append-child group-element child))

        ;; Update point connections
        ;; @todo Updating point connections is meaningless because it is canceled when ungrouping.
        ;;(edraw-restore-point-connections--dom editor group-element)

        ;; Select the group
        (edraw-select group)

        ;; Translate GROUP to center of the view area
        (let ((aabb (edraw-shape-aabb group))
              (vl (edraw-scroll-visible-area-left editor))
              (vt (edraw-scroll-visible-area-top editor))
              (vr (edraw-scroll-visible-area-right editor))
              (vb (edraw-scroll-visible-area-bottom editor)))
          ;; Use transform property. because:
          ;; - To avoid losing numerical precision
          ;; - Text may not be moved (tspan (x=, y=, dx=, dy= attributes) of imported text may not comply with edraw specifications)
          (if (and (<= vl (edraw-rect-left aabb))
                   (<= vt (edraw-rect-top aabb))
                   (<= (edraw-rect-right aabb) vr)
                   (<= (edraw-rect-bottom aabb) vb))
              (edraw-set-property group
                                  'transform
                                  (edraw-svg-transform-from-matrix (edraw-matrix)))
            (edraw-transform-prop-multiply
             group
             (edraw-matrix-translate-xy
              (edraw-xy-sub
               (edraw-xy (* 0.5 (+ vl vr))
                         (* 0.5 (+ vt vb)))
               (edraw-rect-center aabb))))))))))


;;;;;; Editor - Document - Menu

(cl-defmethod edraw-menu-document ((editor edraw-editor))
  `(,(edraw-msg "Document")
    ,(edraw-filter-menu-items
      editor 'document
      `(((edraw-msg "Set Background...") edraw-editor-set-background)
        ((edraw-msg "Resize...") edraw-editor-set-size)
        ((edraw-msg "Crop...") edraw-editor-crop)
        ((edraw-msg "View Box...") edraw-editor-set-viewbox)
        ((edraw-msg "Transform")
         (((edraw-msg "Translate All...") edraw-editor-translate-all-shapes)
          ((edraw-msg "Scale All...") edraw-editor-scale-all-shapes)
          ((edraw-msg "Rotate All...") edraw-editor-rotate-all-shapes)
          ,(edraw-transform-method-menu editor)))
        ((edraw-msg "Clear...") edraw-editor-clear)
        ((edraw-msg "Export to Buffer") edraw-editor-export-to-buffer)
        ((edraw-msg "Export to File...") edraw-editor-export-to-file)
        ((edraw-msg "Import from File...") edraw-editor-import-from-file)))))

(cl-defmethod edraw-menu-document-quick ((editor edraw-editor))
  `(,(edraw-msg "Document Quick")
    ,(edraw-filter-menu-items
      editor 'document-quick
      `(;;((edraw-msg "Search Object") edraw-editor-search-object)
        ((edraw-msg "Select All") edraw-editor-toggle-selection-all
         :visible ,(not (edraw-selected-shapes editor))
         :enable ,(not (edraw-selected-shapes editor)))
        ((edraw-msg "Deselect All") edraw-editor-toggle-selection-all
         :visible ,(not (null (edraw-selected-shapes editor)))
         :enable ,(not (null (edraw-selected-shapes editor))))
        ((edraw-msg "Clear Temporary States")
         edraw-editor-clear-temporary-states-all-shapes)
        ((edraw-msg "Undo") edraw-editor-undo
         :enable ,(not (edraw-empty-undo-p editor)))
        ((edraw-msg "Redo") edraw-editor-redo
         :enable ,(not (edraw-empty-redo-p editor)))
        ((edraw-msg "Paste") edraw-editor-paste-and-select
         :enable ,(not (edraw-clipboard-empty-p)))))))

(cl-defmethod edraw-menu-document-context ((editor edraw-editor))
  `(,(edraw-msg "Document")
    ,(edraw-filter-menu-items
      editor 'context-document
      `(,(edraw-menu-document editor)
        ,@(cadr (edraw-menu-document-quick editor))))))

(cl-defmethod edraw-popup-context-menu-for-document ((editor edraw-editor))
  (let ((edraw-current-editor editor))
    (edraw-popup-menu nil (edraw-menu-document-context editor) editor)))

;;;;; Editor - View
;;;;;; Editor - View - SVG Image Update

(cl-defmethod edraw-set-base-uri ((editor edraw-editor) filename-or-nil)
  (oset editor image-base-uri filename-or-nil)
  (edraw-invalidate-image editor))

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
  (with-slots (overlay svg image image-base-uri) editor
    (edraw-update-image-timer-cancel editor)
    (edraw-update-ui-parts editor)
    (edraw-call-hook editor 'before-image-update)

    (edraw-flush-image editor)

    (setq image
          (apply #'create-image
                 (edraw-svg-to-string svg
                                      #'edraw-editor-svg-node-filter
                                      nil)
                 'svg t
                 `(;; Set Base URI (Default value is `buffer-file-name')
                   ,@(when image-base-uri
                       (list :base-uri image-base-uri))
                   ;; Cancel image-scale effect
                   :scale 1.0)))
    (overlay-put overlay 'display image)))

(cl-defmethod edraw-flush-image ((editor edraw-editor))
  (with-slots (image) editor
    (when image
      (image-flush image)
      (setq image nil))))

(defun edraw-editor-svg-node-filter (dom)
  (if-let ((shape (edraw-shape-from-element-no-create dom)))
      (edraw-visible-p shape)
    t))

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
        (let ((defs (edraw-dom-element 'defs :id "edraw-ui-defs")))
          (edraw-dom-insert-first svg defs)
          defs))))

(cl-defmethod edraw-ui-background-svg ((editor edraw-editor))
  (with-slots (svg) editor
    (or (edraw-dom-get-by-id svg "edraw-ui-background")
        (let ((g (edraw-svg-group :id "edraw-ui-background")))
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
}
.edraw-ui-transform-origin,
.edraw-ui-transform-handle,
.edraw-ui-transform-boundary {
  stroke: #88f; fill: none;
}")

(cl-defmethod edraw-update-ui-style-svg ((editor edraw-editor))
  (with-slots (svg) editor
    (when-let ((style (edraw-dom-get-by-id svg "edraw-ui-style")))
      (edraw-dom-remove-all-children style)
      (edraw-dom-append-child style edraw-editor-ui-style))))

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

(defun edraw-editor-remove-root-transform (svg
                                           svg-document-original-width
                                           svg-document-original-height
                                           svg-document-original-viewbox)
  (when svg
    (edraw-svg-set-attr-number svg 'width svg-document-original-width)
    (edraw-svg-set-attr-number svg 'height svg-document-original-height)
    (if svg-document-original-viewbox
        (edraw-svg-set-attr-string svg 'viewBox svg-document-original-viewbox)
      (edraw-dom-remove-attr svg 'viewBox)))
  svg)

(defun edraw-editor-remove-ui-elements-from-svg (svg)
  "Remove elements that starts with `#edraw-ui-'."
  (dolist (elem (dom-by-id svg "\\`edraw-ui-"))
    (edraw-dom-remove-node svg elem)))

;;;;;; Editor - View - Transparent Background

(cl-defmethod edraw-initialize-transparent-bg ((editor edraw-editor))
  (edraw-dom-append-child
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
  (edraw-svg-group
   :id "edraw-ui-transparent-bg"
   (edraw-svg-rect 0 0 width height
                   :fill edraw-editor-transparent-bg-color1
                   :stroke "none")
   (when (and edraw-editor-transparent-bg-color2
              (not (equal edraw-editor-transparent-bg-color1
                          edraw-editor-transparent-bg-color2)))
     (edraw-svg-rect 0 0 width height
                     :fill "url(#edraw-ui-pattern-transparent-bg)"
                     :stroke "none"))))

(defun edraw-svg-ui-transparent-bg-pattern ()
  "Create a svg pattern of transparent background."
  (edraw-dom-element
   'pattern
   :id "edraw-ui-pattern-transparent-bg"
   :x 0
   :y 0
   :width (* 2 edraw-editor-transparent-bg-grid-size)
   :height (* 2 edraw-editor-transparent-bg-grid-size)
   :patternUnits "userSpaceOnUse"
   (edraw-svg-rect edraw-editor-transparent-bg-grid-size
                   0
                   edraw-editor-transparent-bg-grid-size
                   edraw-editor-transparent-bg-grid-size
                   :fill edraw-editor-transparent-bg-color2
                   :stroke "none")
   (edraw-svg-rect 0
                   edraw-editor-transparent-bg-grid-size
                   edraw-editor-transparent-bg-grid-size
                   edraw-editor-transparent-bg-grid-size
                   :fill edraw-editor-transparent-bg-color2
                   :stroke "none")))

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
                   do (edraw-svg-line xv view-y-min xv view-y-max
                                      :parent g
                                      :class (if (= x 0)
                                                 "edraw-ui-axis-line"
                                               "edraw-ui-grid-line")))
          (cl-loop for y from y0 to y1 by step-interval
                   for yv = (edraw-scroll-transform-y editor y)
                   do (edraw-svg-line view-x-min yv view-x-max yv
                                      :parent g
                                      :class (if (= y 0)
                                                 "edraw-ui-axis-line"
                                               "edraw-ui-grid-line"))))))))

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
  (with-slots (selected-shapes
               selected-anchor
               selected-handle
               selection-ui-visible)
      editor
    (if (and selected-shapes
             selection-ui-visible)
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
                    (edraw-svg-polygon points
                                       :parent g
                                       :class "edraw-ui-shape-boundary")))))
            ;; ;; Global AABB
            ;; (when (memq (edraw-shape-type shape) '(text image g))
            ;;   (let ((aabb (edraw-scroll-transform-rect
            ;;                editor (edraw-shape-aabb shape))))
            ;;     (unless (edraw-rect-empty-p aabb)
            ;;       (edraw-svg-rect
            ;;                      (edraw-rect-left aabb)
            ;;                      (edraw-rect-top aabb)
            ;;                      (edraw-rect-width aabb)
            ;;                      (edraw-rect-height aabb)
            ;;                      :parent g
            ;;                      :class
            ;;                      "edraw-ui-shape-boundary"))))
            ))
      ;; Hide points
      (edraw-svg-ui-shape-points-remove-group
       (edraw-ui-foreground-svg editor)))))

(defun edraw-svg-ui-shape-points-remove-group (parent)
  (edraw-dom-remove-by-id parent "edraw-ui-shape-points"))

(defun edraw-svg-ui-shape-points-create-group (parent)
  (if-let ((g (edraw-dom-get-by-id parent "edraw-ui-shape-points")))
      (progn
        (edraw-dom-remove-all-children g)
        g)
    (edraw-svg-group :id "edraw-ui-shape-points" :parent parent)))

(defun edraw-svg-ui-shape-points (parent
                                  editor
                                  shape
                                  selected-anchor
                                  selected-handle)
  (let ((anchor-points (edraw-get-anchor-points shape))
        (prev-selected-anchor (when selected-anchor
                                (edraw-previous-anchor-round selected-anchor)))
        (next-selected-anchor (when selected-anchor
                                (edraw-next-anchor-round selected-anchor))))
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
    (edraw-svg-rect (- (car xy) r) (- (cdr xy) r) (* 2 r) (* 2 r)
                    :parent parent
                    :class (if selected
                               "edraw-ui-anchor-point-selected"
                             "edraw-ui-anchor-point"))))

(defun edraw-svg-ui-handle-point (parent handle-xy anchor-xy
                                         &optional selected)
  (edraw-svg-line (car handle-xy) (cdr handle-xy)
                  (car anchor-xy) (cdr anchor-xy)
                  :parent parent
                  :class "edraw-ui-handle-line")
  (edraw-svg-circle (car handle-xy) (cdr handle-xy)
                    edraw-handle-point-radius
                    :parent parent
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
      ;;(edraw-width editor)
      (edraw-viewbox-width editor)
      )))

(cl-defmethod edraw-scroll-view-height ((editor edraw-editor))
  (with-slots (view-size) editor
    (if view-size
        (cdr view-size)
      ;;(edraw-height editor)
      (edraw-viewbox-height editor)
      )))

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
                (* ;;(edraw-width editor)
                   (edraw-viewbox-width editor)
                   new-scale)
                curr-view-w
                (max
                 curr-view-w
                 (edraw-auto-view-enlargement-max editor t))))
              (new-view-h
               (edraw-clamp
                (* ;;(edraw-height editor)
                   (edraw-viewbox-height editor)
                   new-scale)
                curr-view-h
                (max
                 curr-view-h
                 (edraw-auto-view-enlargement-max editor nil)))))
          (when (or (> new-view-w curr-view-w)
                    (> new-view-h curr-view-h))
            (setq view-size (cons new-view-w new-view-h))
            ;; It is the caller's responsibility to update SVG
            ))))))

(cl-defmethod edraw-auto-view-enlargement-max ((editor edraw-editor)
                                               x-p)
  (let ((spec (if x-p
                  (car edraw-editor-auto-view-enlargement-max-size)
                (cdr edraw-editor-auto-view-enlargement-max-size)))
        (image-scale (oref editor image-scale)))
    (pcase spec
      ;; Number of pixels (before scaling)
      ((pred integerp) spec)
      ;; Ratio to frame body size
      ((pred floatp)
       (floor
        (/ (* spec (if x-p (frame-text-width) (frame-text-height)))
           image-scale)))
      ;; Ratio to window body size
      (`(window ,ratio ,minus ,min ,max)
       (let* ((ratio (or ratio 1.0))
              (wsize (/ (if x-p
                            (window-body-width nil t)
                          (window-body-height nil t))
                        image-scale))
              (minus (or minus 0))
              (min (or min 0))
              (max (or max
                       (/ (if x-p
                              (edraw-max-image-width)
                            (edraw-max-image-height))
                          image-scale))))
         (floor (edraw-clamp (- (* ratio wsize) minus) min max))))
      (_ (if x-p 560 420)))))


;;;;;;; Commands

(edraw-editor-defcmd edraw-reset-scroll-and-zoom ((editor edraw-editor))
  (edraw-set-scroll-transform editor
                              (- (edraw-viewbox-left editor))
                              (- (edraw-viewbox-top editor))
                              1))

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

(defun edraw-editor-scroll-distance-by-arrow-key (editor mods)
  (pcase (alist-get
          (seq-difference mods '(meta click double triple drag down)
                          'eq)
          edraw-editor-scroll-distance-by-arrow-key
          1
          nil #'seq-set-equal-p)
    ((and (pred numberp) n)
     n)
    ('grid
     (edraw-get-setting editor 'grid-interval))
    (_ 10)))

(defun edraw-editor-scroll-by-arrow-key (&optional editor n)
  (interactive "i\np")
  (let ((event last-input-event))
    (when-let ((editor (or editor (edraw-editor-at-input event))))
      (let* ((mods (event-modifiers event))
             (d (* (or n 1)
                   (if (memq 'meta mods)
                       (read-number (edraw-msg "Moving Distance: ") 20)
                     (edraw-editor-scroll-distance-by-arrow-key editor mods))))
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
                    (edraw-msg "r-click:quit, drag:Scroll, wheel:Zoom,\nSPC/q/C-g:quit, [S|C|M-]arrow keys:Scroll, +/-:Zoom, 0:reset"))))
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
         ((eq event ?+)
          (edraw-zoom-in editor))
         ((eq event ?-)
          (edraw-zoom-out editor))
         ((memq (event-basic-type event) '(left up right down))
          (edraw-editor-scroll-by-arrow-key editor)
          ))))))

(edraw-editor-defcmd edraw-set-view-size-spec ((editor edraw-editor) wh)
  (interactive
   (if current-prefix-arg ;;C-u => reset view
       (list (edraw-current-editor) nil)
     (let* ((editor (edraw-current-editor))
            (w (round (read-number (edraw-msg "View Width: ")
                                   (edraw-scroll-view-width editor))))
            (h (round (read-number (edraw-msg "View Height: ")
                                   (edraw-scroll-view-height editor)))))
       (list editor (cons w h)))))

  (unless (or (null wh)
              (and (consp wh)
                   (>= (car wh) 1)
                   (<= (car wh) (edraw-max-image-width))
                   (>= (cdr wh) 1)
                   (<= (cdr wh) (edraw-max-image-height))))
    (error (edraw-msg "Invalid view size")))

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
  (when-let ((spt (oref editor selected-anchor)))
    (if (edraw-valid-point-p spt)
        spt
      ;; (message "Warning: The selected anchor is invalid")
      nil)))

(cl-defmethod edraw-selected-handle ((editor edraw-editor))
  (when-let ((spt (oref editor selected-handle)))
    (if (and (edraw-valid-point-p spt)
             (edraw-active-handle-p spt))
        spt
      ;; (message "Warning: The selected handle is invalid")
      nil)))

(cl-defmethod edraw-selecting-point-p ((editor edraw-editor))
  (when (or (oref editor selected-anchor)
            (oref editor selected-handle))
    t))

(cl-defmethod edraw-add-shape-selection ((editor edraw-editor) shape)
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (when (and shape
               (not (memq shape selected-shapes)))
      (edraw-add-change-hook shape
                             'edraw-on-selected-shape-changed editor)
      (setq selected-shapes (append selected-shapes (list shape)))
      (edraw-invalidate-ui-parts editor 'selection-ui)

      (edraw-update-property-editor-on-selection-change editor)

      (edraw-call-hook editor 'selection-change))))

(cl-defmethod edraw-update-property-editor-on-selection-change ((editor edraw-editor))
  (with-slots (selected-shapes) editor
    (when (and (edraw-property-editor-tracking-selected-shape-p)
               selected-shapes
               (edraw-property-editor-buffer))
      (edraw-edit-properties
       (edraw-selected-multiple-shapes-or-shape editor)))))

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
    (edraw-select-all-shapes editor))
  (when (edraw-editor-defcmd-interactive-p)
    (edraw-display-selected-object-info editor)))


(cl-defmethod edraw-on-selected-shape-changed ((editor edraw-editor)
                                               shape type hint)
  ;;(message "changed!! %s" type)

  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (cond
     ((eq type 'shape-remove)
      (edraw-remove-shape-selection editor shape))

     ((eq type 'shape-properties)
      (if (and selected-anchor (assq 'd hint))
          (edraw-deselect-anchor editor) ;; All anchors have been destroyed
        (edraw-invalidate-ui-parts editor 'selection-ui)))

     ((and selected-anchor
           (memq type '(anchor-remove anchor-transfer-out))
           ;; No shape in selected-shapes owns selected-anchor
           ;; @todo Inefficient
           (not (edraw-point-in-selected-shapes-p editor selected-anchor)))
      (edraw-deselect-anchor editor))

     ((and selected-handle
           (memq type '(handle-remove))
           ;; No shape in selected-shapes owns selected-handle
           ;; @todo Inefficient
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
      (let ((prev-anchor (edraw-previous-anchor-round selected-anchor))
            (next-anchor (edraw-next-anchor-round selected-anchor)))
        (append
         ;; Include handles of previous and next anchor
         ;; see: svg-ui-shape-points
         (edraw-get-handle-points selected-anchor)
         (when prev-anchor (edraw-get-handle-points prev-anchor))
         (when next-anchor (edraw-get-handle-points next-anchor)))))))

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

(cl-defmethod edraw-display-selected-object-info ((editor edraw-editor)
                                                  &optional addinfo)
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (cond
     (selected-handle
      (message "%s%s"
               (format (edraw-msg "Handle (%g,%g) selected")
                       (edraw-x (edraw-get-xy selected-handle))
                       (edraw-y (edraw-get-xy selected-handle)))
               (or addinfo "")))
     (selected-anchor
      (message "%s%s"
               (format (edraw-msg "Anchor (%g,%g) selected")
                       (edraw-x (edraw-get-xy selected-anchor))
                       (edraw-y (edraw-get-xy selected-anchor)))
               (or addinfo "")))
     (selected-shapes
      (let ((rect (edraw-shape-aabb selected-shapes)))
        (message "%s%s"
                 (if (cdr selected-shapes)
                     (format (edraw-msg "%s objects (%g,%g)-(%g,%g) selected")
                             (length selected-shapes)
                             (edraw-rect-left rect)
                             (edraw-rect-top rect)
                             (edraw-rect-right rect)
                             (edraw-rect-bottom rect))
                   (format (edraw-msg "%s (%g,%g)-(%g,%g) selected")
                           (capitalize
                            (edraw-shape-type-name (car selected-shapes)))
                           (edraw-rect-left rect)
                           (edraw-rect-top rect)
                           (edraw-rect-right rect)
                           (edraw-rect-bottom rect)))
                 (or addinfo ""))))
     (t (message "%s%s"
                 (edraw-msg "No objects selected")
                 (or addinfo ""))))))


;;;;; Editor - Manipulate Selected Shapes

(cl-defmethod edraw-selected-multiple-shapes ((editor edraw-editor))
  (if-let ((selected-shapes (oref editor selected-shapes)))
      (edraw-multiple-shapes :shapes selected-shapes :editor editor)
    (error (edraw-msg "No shape selected"))))

(cl-defmethod edraw-selected-multiple-shapes-or-shape ((editor edraw-editor))
  (if-let ((selected-shapes (oref editor selected-shapes)))
      (if (cdr selected-shapes)
          (edraw-multiple-shapes :shapes selected-shapes :editor editor)
        (car selected-shapes))
    (error (edraw-msg "No shape selected"))))

(cl-defmethod edraw-menu-items-selected-shapes ((editor edraw-editor))
  (when-let* ((selected-shapes (edraw-selected-shapes editor))
              (shapes (edraw-multiple-shapes
                       :shapes selected-shapes :editor editor)))
    (edraw-filter-menu-items
     editor 'selected-shapes
     (edraw-menu-items-shape-common shapes))))

(cl-defmethod edraw-menu-selected-shapes ((editor edraw-editor))
  (if (edraw-selected-shapes editor)
      `(,(edraw-msg "Selected Object")
        ,(edraw-menu-items-selected-shapes editor))
    `(,(edraw-msg "Selected Object")
      (("Dummy" ignore))
      :enable nil)))

(cl-defmethod edraw-menu-selected-shapes-context ((editor edraw-editor))
  `(,(edraw-get-summary-for-selected-shapes editor)
    ,(edraw-menu-items-selected-shapes editor)))

(cl-defmethod edraw-get-summary-for-selected-shapes ((editor edraw-editor))
  (let ((selected-shapes (edraw-selected-shapes editor)))
    (format (edraw-msg "%s Selected Shapes") (length selected-shapes))))

(cl-defmethod edraw-popup-context-menu-for-selected-shapes ((editor
                                                             edraw-editor))
  (let ((edraw-current-editor editor))
    (edraw-popup-menu nil (edraw-menu-selected-shapes-context editor) editor)))

(defun edraw-editor-move-distance-by-arrow-key (editor mods)
  (pcase (alist-get
          (seq-difference mods '(meta click double triple drag down)
                          'eq)
          edraw-editor-move-distance-by-arrow-key
          1
          nil #'seq-set-equal-p)
    ((and (pred numberp) n)
     n)
    ('grid
     (edraw-get-setting editor 'grid-interval))
    ('snap
     (if (edraw-selecting-point-p editor) ;;@todo if multiple points are selected, do not snap
         'snap
       (edraw-get-setting editor 'grid-interval)))
    (_ 1)))

(defvar edraw-editor-move-selected-by-arrow-key--last-op nil)
(defvar edraw-editor-move-selected-by-arrow-key--delta nil)
(defvar edraw-editor-move-selected-by-arrow-key--last-undo-pushed-p nil)

(defun edraw-editor-move-selected-by-arrow-key (&optional editor n)
  (interactive "i\nP")
  (let ((event last-input-event))
    (unless editor (setq editor (edraw-editor-at-input event)))
    (when editor
      (let* ((mods (event-modifiers event))
             (times (if (consp n)
                        (read-number (edraw-msg "Moving Distance: ")
                                     (edraw-get-setting editor 'grid-interval))
                      (prefix-numeric-value n)))
             (d-spec (edraw-editor-move-distance-by-arrow-key editor mods))
             (d (* times (if (eq d-spec 'snap) 1 d-spec)))
             (v (pcase (event-basic-type event)
                  ('left (edraw-xy (- d) 0))
                  ('right (edraw-xy d 0))
                  ('up (edraw-xy 0 (- d)))
                  ('down (edraw-xy 0 d))
                  (_ (edraw-xy 0 0))))
             (op (cond
                  ((and (memq 'meta mods)
                        ;; Do not duplicate anchor or handle
                         ;;@todo if multiple points are selected?
                        (not (edraw-selecting-point-p editor)))
                   'duplicate)
                  ((and (eq d-spec 'snap)
                        ;; Do not snap shape
                        ;;@todo if multiple points are selected?
                        (edraw-selecting-point-p editor))
                   'snap)
                  (t
                   'translate))))
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
           (edraw-display-selected-object-coordinates editor))

          ('snap
           (when-let ((spt (or (edraw-selected-handle editor)
                               (edraw-selected-anchor editor))))
             (let* ((xy (edraw-get-xy spt))
                    (x (edraw-x xy))
                    (y (edraw-y xy))
                    (vx (edraw-x v))
                    (vy (edraw-y v))
                    (grid-interval (edraw-get-setting editor 'grid-interval))
                    (new-x (if (= vx 0)
                               x
                             (edraw-grid-round
                              (cond
                               ((= x (edraw-grid-round x grid-interval))
                                (+ x (* vx grid-interval)))
                               ((< vx 0)
                                (- (edraw-grid-floor x grid-interval)
                                   (* (1- times) grid-interval)))
                               ((> vx 0)
                                (+ (edraw-grid-ceil x grid-interval)
                                   (* (1- times) grid-interval)))
                               (t x))
                              grid-interval)))
                    (new-y (if (= vy 0)
                               y
                             (edraw-grid-round
                              (cond
                               ((= y (edraw-grid-round y grid-interval))
                                (+ y (* vy grid-interval)))
                               ((< vy 0)
                                (- (edraw-grid-floor y grid-interval)
                                   (* (1- times) grid-interval)))
                               ((> vy 0)
                                (+ (edraw-grid-ceil y grid-interval)
                                   (* (1- times) grid-interval)))
                               (t y))
                              grid-interval))))
               ;; @todo Combine consecutive translations into a single operation
               (edraw-make-undo-group editor
                   'selected-shapes-snap-move-by-arrow-key
                 (edraw-move spt (edraw-xy new-x new-y)))
               (edraw-display-selected-object-coordinates editor)))))
        ;; Record last operation type
        (setq edraw-editor-move-selected-by-arrow-key--last-op op)))))

(edraw-editor-defcmd edraw-transform-selected-interactive ((editor edraw-editor))
  (edraw-transform-interactive
   (edraw-selected-multiple-shapes-or-shape editor)))

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
      (edraw-translate (edraw-selected-multiple-shapes editor) xy)))))

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
        (edraw-transform (edraw-selected-multiple-shapes editor) matrix))))))

(edraw-editor-defcmd edraw-delete-selected ((editor edraw-editor))
  (with-slots (selected-shapes selected-anchor selected-handle) editor
    (cond
     (selected-handle
      (edraw-delete-point selected-handle))
     (selected-anchor
      (edraw-delete-point selected-anchor))
     (selected-shapes
      (edraw-remove (edraw-selected-multiple-shapes-or-shape editor))))))

(edraw-editor-defcmd edraw-bring-selected-to-front ((editor edraw-editor))
  (edraw-bring-to-front (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-bring-selected-forward ((editor edraw-editor))
  (edraw-bring-forward (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-send-selected-backward ((editor edraw-editor))
  (edraw-send-backward (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-send-selected-to-back ((editor edraw-editor))
  (edraw-send-to-back (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-select-next-shape ((editor edraw-editor))
  (if (edraw-selected-shapes editor)
      (edraw-select-next-shape (edraw-selected-multiple-shapes-or-shape editor))
    (when-let ((back (edraw-back-shape editor)))
      (edraw-select-shape editor back))))

(edraw-editor-defcmd edraw-select-previous-shape ((editor edraw-editor))
  (if (edraw-selected-shapes editor)
      (edraw-select-previous-shape (edraw-selected-multiple-shapes-or-shape editor))
    (when-let ((front (edraw-front-shape editor)))
      (edraw-select-shape editor front))))

(edraw-editor-defcmd edraw-group-selected-shapes ((editor edraw-editor))
  (edraw-group (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-ungroup-selected-shapes ((editor edraw-editor))
  (edraw-ungroup-interactive (edraw-selected-multiple-shapes editor)))


(edraw-editor-defcmd edraw-combine-selected-paths ((editor edraw-editor))
  (edraw-combine-paths (edraw-selected-multiple-shapes editor)))


(edraw-editor-defcmd edraw-edit-properties-of-selected-shapes ((editor edraw-editor))
  (edraw-editor-open-property-editor
   editor
   (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-edit-fill-selected ((editor edraw-editor))
  (edraw-edit-fill (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-edit-stroke-selected ((editor edraw-editor))
  (edraw-edit-stroke (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-edit-href-selected ((editor edraw-editor))
  (edraw-edit-href (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-edit-font-size-selected ((editor edraw-editor))
  (edraw-edit-font-size (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-set-marker-start-none-selected ((editor
                                                            edraw-editor))
  (edraw-set-marker-start-none (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-set-marker-start-arrow-selected ((editor
                                                             edraw-editor))
  (edraw-set-marker-start-arrow (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-set-marker-start-circle-selected ((editor
                                                              edraw-editor))
  (edraw-set-marker-start-circle (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-set-marker-end-none-selected ((editor
                                                          edraw-editor))
  (edraw-set-marker-end-none (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-set-marker-end-arrow-selected ((editor
                                                           edraw-editor))
  (edraw-set-marker-end-arrow (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-set-marker-end-circle-selected ((editor
                                                            edraw-editor))
  (edraw-set-marker-end-circle (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-set-marker-start-next-selected ((editor
                                                            edraw-editor))
  (edraw-set-marker-start-next (edraw-selected-multiple-shapes editor)))

(edraw-editor-defcmd edraw-set-marker-end-next-selected ((editor
                                                          edraw-editor))
  (edraw-set-marker-end-next (edraw-selected-multiple-shapes editor)))

;; Temporary State

(edraw-editor-defcmd edraw-toggle-visibility-selected ((editor edraw-editor))
  (edraw-toggle-visibility
   (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-toggle-pickability-selected ((editor edraw-editor))
  (edraw-toggle-pickability
   (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-clear-temporary-states-selected ((editor
                                                             edraw-editor))
  (edraw-clear-temporary-states
   (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-clear-temporary-states-all-shapes ((editor
                                                               edraw-editor))
  (dolist (shape (edraw-all-shapes editor))
    (edraw-clear-temporary-states shape)))


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
  (edraw-copy (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-cut-selected-shapes ((editor edraw-editor))
  (edraw-cut (edraw-selected-multiple-shapes-or-shape editor)))

(edraw-editor-defcmd edraw-duplicate-selected-shapes ((editor edraw-editor))
  (edraw-duplicate-and-select (edraw-selected-multiple-shapes-or-shape editor)))

;;;;; Editor - Default Shape Properties

(cl-defmethod edraw-initialize-default-shape-properties ((editor edraw-editor))
  (with-slots (ui-state) editor
    ;;@todo Make it possible to disable reading from preset. Because it involves file access.
    (when ui-state
      ;; For each preset
      (pcase-dolist (`(,name . ,data) (edraw-preset-enum ui-state 'shape))
        ;; For each special preset
        (when (edraw-preset-name-special-p name)
          (pcase (edraw-preset-name-special-category name)
            ('initial-default-shape
             (when-let* ((shape-type (edraw-preset-name-special-subtype name))
                         (object (edraw-get-default-shape-object editor
                                                                 shape-type
                                                                 t)))
               (edraw-preset-apply object data 'not-required)))
            ('initial-default-shape-for-tool
             (let ((tool-class (edraw-preset-name-special-subtype name)))
               (when (edraw-editor-tool-class-p tool-class)
                 (when-let ((object (edraw-get-default-shape-object-for-tool
                                     editor tool-class)))
                   (edraw-preset-apply object data 'not-required)))))))))))

(cl-defmethod edraw-initialize-default-marker-properties ((editor edraw-editor))
  (with-slots (ui-state) editor
    ;;@todo Make it possible to disable reading from preset. Because it involves file access.
    (when ui-state
      ;; For each preset
      (pcase-dolist (`(,name . ,data) (edraw-preset-enum ui-state 'marker))
        ;; For each special preset
        (when (edraw-preset-name-special-p name)
          (pcase (edraw-preset-name-special-category name)
            ('initial-default-marker
             (when-let* ((marker-type (edraw-preset-name-special-subtype name))
                         (object (edraw-get-default-marker-object editor
                                                                  marker-type)))
               (edraw-preset-apply object data 'not-required)))))))))

(cl-defmethod edraw-get-default-shape-properties-container
  ((editor edraw-editor) shape-type &optional create-p ignore-selected-tool-p)
  "Return a cons cell whose cdr is the default shape property alist."
  (or
   ;; From current tool
   (and
    (not ignore-selected-tool-p)
    (when (eq (edraw-shape-type-to-create (edraw-selected-tool editor))
              shape-type)
      (let* ((cell (assq (edraw-selected-tool-class editor)
                         (oref editor default-shape-properties-for-each-tool)))
             (props-spec (cdr cell)))
        (when (and cell
                   ;; CELL is (<tool-class> . <alist>) not (<tool-class> . <symbol>)
                   (listp props-spec))
          cell))))
   ;; From shape type
   (assq shape-type (oref editor default-shape-properties))
   ;; Create new
   (when create-p
     (let ((cell (cons shape-type nil)))
       (push cell (oref editor default-shape-properties))
       cell))))

(cl-defmethod edraw-get-default-shape-properties ((editor edraw-editor)
                                                  shape-type)
  "Return the default shape property alist of shape type SHAPE-TYPE."
  (cdr (edraw-get-default-shape-properties-container editor shape-type)))

(cl-defmethod edraw-get-default-shape-properties-container-for-tool
  ((editor edraw-editor) tool-class &optional create-p)
  (when (edraw-editor-tool-class-p tool-class)
    (or
     (let* ((cell (assq tool-class (oref editor default-shape-properties-for-each-tool)))
            (props-spec (cdr cell)))
       (if cell
           (if (listp props-spec) ;; CELL is (<tool-class> . <alist>) not (<tool-class> . <symbol>)
               cell
             (when create-p
               (setcdr cell nil)
               cell))
         (when create-p
           (let ((cell (cons tool-class nil)))
             (push cell (oref editor default-shape-properties-for-each-tool))
             cell)))))))

(cl-defmethod edraw-get-default-shape-property ((editor edraw-editor)
                                                shape-type prop-name)
  (alist-get prop-name
             (edraw-get-default-shape-properties editor shape-type)))

(cl-defmethod edraw-set-default-shape-property ((editor edraw-editor)
                                                shape-type prop-name value)
  (setf (alist-get
         prop-name
         (cdr
          (edraw-get-default-shape-properties-container editor shape-type t)))
        value))

(cl-defmethod edraw-set-default-shape-properties-from-shape
  ((editor edraw-editor) shape)
  (when-let ((shape-type (edraw-shape-type shape)))
    (dolist (prop-info (edraw-get-property-info-list shape))
      ;; Skip required property
      (unless (edraw-svg-prop-info-required-p prop-info)
        ;; Get property value
        (let* ((prop-name (edraw-svg-prop-info-name prop-info))
               (value (edraw-get-property shape prop-name)))
          ;; Set property value as default
          (edraw-set-default-shape-property
           editor shape-type prop-name value))))
    ;; Update toolbar
    (edraw-update-toolbar editor)))

(cl-defmethod edraw-get-default-shape-object ((editor edraw-editor) shape-type
                                              &optional
                                              ignore-selected-tool-p)
  (when-let ((alist-head (edraw-get-default-shape-properties-container
                          editor shape-type t ignore-selected-tool-p))
             (shape-class (edraw-shape-type-to-class shape-type)))
    (edraw-property-proxy-shape
     :alist-head alist-head
     :editor editor
     :name (format "default %s"
                   (let ((key (car alist-head)))
                     (if (edraw-editor-tool-class-p key)
                         ;; Show tool name
                         ;; 'edraw-editor-tool-rect => "Rect Tool"
                         (let ((tool-class key))
                           (edraw-name tool-class))
                       ;; Show shape type
                       shape-type)))
     :prop-info-list
     (seq-remove
      (lambda (prop-info) (edraw-svg-prop-info-required-p prop-info))
      (edraw-get-property-info-list shape-class))
     :preset-type 'shape
     :preset-subtype shape-type ;; edraw-shape-type value (g, not group)
     )))

(cl-defmethod edraw-get-default-shape-object-for-tool ((editor edraw-editor)
                                                       tool-class)
  (when-let ((alist-head (edraw-get-default-shape-properties-container-for-tool
                          editor tool-class t))
             (shape-type (edraw-shape-type-to-create tool-class))
             (shape-class (edraw-shape-type-to-class shape-type)))
    (edraw-property-proxy-shape
     :alist-head alist-head
     :editor editor
     :name (format "default %s"
                   ;; Show tool name
                   ;; 'edraw-editor-tool-rect => "Rect Tool"
                   (edraw-name tool-class))
     :prop-info-list
     (seq-remove
      (lambda (prop-info) (edraw-svg-prop-info-required-p prop-info))
      (edraw-get-property-info-list shape-class))
     :preset-type 'shape
     :preset-subtype shape-type ;; edraw-shape-type value (g, not group)
     )))

(cl-defmethod edraw-edit-default-shape-properties ((editor edraw-editor)
                                                   shape-type)
  (when-let (object (edraw-get-default-shape-object editor shape-type))
    (edraw-editor-open-property-editor editor object)))

(defun edraw-editor-edit-default-shape-props (&optional editor shape-type)
  (let ((editor (or editor (edraw-current-editor))))
    (edraw-edit-default-shape-properties editor shape-type)))

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

(defclass edraw-property-proxy-shape (edraw-alist-properties-holder)
  ())

(cl-defmethod edraw-set-properties ((shape edraw-property-proxy-shape) _prop-list)
  (prog1 (cl-call-next-method)
    ;;@todo Use change method?
    ;; update toolbar (fill & stroke)
    (edraw-update-toolbar (oref shape editor))))

;;;;; Editor - Default Marker Properties

(cl-defmethod edraw-get-default-marker-properties ((editor edraw-editor)
                                                   marker-type)
  ;; Related: edraw-property-editor-create-marker-widget
  (when (stringp marker-type)
    (edraw-svg-marker marker-type
                      (alist-get marker-type
                                 (oref editor default-marker-properties)
                                 nil nil #'equal))))

(defun edraw-editor-edit-default-marker-arrow-props (&optional editor)
  (interactive)
  (edraw-editor-edit-default-marker-props editor "arrow"))

(defun edraw-editor-edit-default-marker-circle-props (&optional editor)
  (interactive)
  (edraw-editor-edit-default-marker-props editor "circle"))

(defun edraw-editor-edit-default-marker-props (&optional editor marker-type)
  (let ((editor (or editor (edraw-current-editor))))
    (edraw-edit-default-marker-properties editor marker-type)))

(cl-defmethod edraw-get-default-marker-object ((editor edraw-editor)
                                               marker-type)
  (when (symbolp marker-type)
    (setq marker-type (symbol-name marker-type)))
  (when (stringp marker-type)
    (with-slots (default-marker-properties) editor
      (let ((alist-head (assoc marker-type default-marker-properties)))
        (unless alist-head
          (push (list marker-type) default-marker-properties)
          (setq alist-head (car default-marker-properties)))
        (edraw-alist-properties-holder
         :alist-head alist-head
         :editor editor
         :name (format "default %s marker" marker-type)
         :prop-info-list (edraw-svg-marker-prop-info-list marker-type)
         :preset-type 'marker
         :preset-subtype (intern marker-type))))))

(cl-defmethod edraw-edit-default-marker-properties ((editor edraw-editor)
                                                    marker-type)
  (when-let ((marker-object (edraw-get-default-marker-object editor
                                                             marker-type)))
    (edraw-editor-open-property-editor editor marker-object)))


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
    (((edraw-msg "Auto")
      edraw-editor-set-transform-method-auto
      :cmd-for-selected edraw-editor-set-transform-method-auto
      :button (:toggle . ,(eq (edraw-get-transform-method obj)
                              'auto)))
     ((edraw-msg "\"transform\" Property")
      edraw-editor-set-transform-method-transform-property
      :cmd-for-selected edraw-editor-set-transform-method-transform-property
      :button (:toggle . ,(eq (edraw-get-transform-method obj)
                              'transform-property)))
     ((edraw-msg "Anchor Points")
      edraw-editor-set-transform-method-anchor-points
      :cmd-for-selected edraw-editor-set-transform-method-anchor-points
      :button (:toggle . ,(eq (edraw-get-transform-method obj)
                              'anchor-points))))))

;;;;; Editor - Main Menu

(cl-defmethod edraw-menu-view ((editor edraw-editor))
  `(,(edraw-msg "View")
    ,(edraw-filter-menu-items
      editor 'view
      `(((edraw-msg "Transparent BG") edraw-editor-toggle-transparent-bg-visible
         :button (:toggle . ,(edraw-get-transparent-bg-visible editor)))
        ((edraw-msg "Grid") edraw-editor-toggle-grid-visible
         :button (:toggle . ,(edraw-get-grid-visible editor)))
        ((edraw-msg "Set Grid Interval...") edraw-editor-set-grid-interval)
        ((edraw-msg "Set View Size...") edraw-editor-set-view-size-spec)
        ((edraw-msg "Reset View") edraw-editor-reset-view)
        ((edraw-msg "Zoom In") edraw-editor-zoom-in)
        ((edraw-msg "Zoom Out") edraw-editor-zoom-out)
        ((edraw-msg "Reset Scroll and Zoom") edraw-editor-reset-scroll-and-zoom)
        ((edraw-msg "Scroll and Zoom") edraw-editor-interactive-scroll-and-zoom)))))

(cl-defmethod edraw-menu-default-config ((editor edraw-editor))
  `(,(edraw-msg "Default Config")
    ,(edraw-filter-menu-items
      editor 'default-config
      `(((edraw-msg "Shape")
         (((edraw-msg "Rect") edraw-editor-edit-default-rect-props)
          ((edraw-msg "Ellipse") edraw-editor-edit-default-ellipse-props)
          ((edraw-msg "Text") edraw-editor-edit-default-text-props)
          ((edraw-msg "Path") edraw-editor-edit-default-path-props)))
        ((edraw-msg "Marker")
         (((edraw-msg "Arrow") edraw-editor-edit-default-marker-arrow-props)
          ((edraw-msg "Circle") edraw-editor-edit-default-marker-circle-props)))))))

(cl-defmethod edraw-menu-main ((editor edraw-editor))
  `(,(edraw-msg "Main Menu")
    ,(edraw-filter-menu-items
      editor 'main-menu
      `(,(edraw-menu-document editor)
        ,(edraw-menu-view editor)
        ,(edraw-menu-selected-shapes editor)
        ,(edraw-menu-default-config editor)
        ,@(cadr (edraw-menu-document-quick editor))

        ((edraw-msg "Save") edraw-editor-save
         :visible ,(not (null (oref editor document-writer)))
         :enable ,(edraw-modified-p editor))))))

(edraw-editor-defcmd edraw-main-menu ((editor edraw-editor))
  "Show the main menu of EDITOR."
  (let ((edraw-current-editor editor))
    (edraw-popup-menu nil (edraw-menu-main editor) editor)))

(cl-defmethod edraw-filter-menu-items ((editor edraw-editor) menu-type items)
  (with-slots (menu-filter) editor
    (if menu-filter
        (funcall menu-filter menu-type items)
      items)))

;;;;; Editor - Context Menu

(edraw-editor-defcmd edraw-popup-context-menu ((editor edraw-editor))
  "Show context menu of EDITOR."
  (if-let ((selected-shapes (edraw-selected-shapes editor)))
      (if (cdr selected-shapes)
          ;; Multiple selected shapes
          (edraw-popup-context-menu-for-selected-shapes editor)
        ;; Single selected shape
        (edraw-popup-context-menu (car selected-shapes)))
    ;; No selected shape
    (edraw-main-menu editor)))

;;;;; Editor - Mouse Coordinates

(defun edraw-convert-mouse-event-on-down-object (move-event down-event)
  "Convert the mouse event to retain the position on the object when
it is pressed down.

MOVE-EVENT is the event to be converted. It is not clear which
part of the Emacs frame it points to.

DOWN-EVENT is the event that occurs when the mouse is pressed down.

MOVE-EVENT is converted to have the position (coordinates) on the
object where the DOWN-EVENT occurred, and returned."
  (let ((move-pos (event-start move-event))
        (down-pos (event-start down-event)))
    (if ;;(edraw-posn-same-object-p move-pos down-pos)
        ;; (and (eq (posn-window move-pos)
        ;;          (posn-window down-pos))
        ;;      (equal (posn-point move-pos)
        ;;             (posn-point down-pos))
        ;;      (eq (posn-object move-pos)
        ;;          ???))
        ;; Since it is difficult to determine whether the object being
        ;; pointed to is a toolbar, calculations are always performed
        ;; based on the displacement of frame coordinates.
        nil
        ;; In the target image
        move-event
      ;; Out of the target image
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Click-Events.html
      (let* ((new-event (copy-sequence move-event))
             ;; Copy from down-event!
             (new-pos (copy-sequence (nth 1 down-event))))
        (setf (nth 1 new-event) new-pos)
        (setf (nth 8 new-pos)
              (let ((delta-xy (edraw-posn-delta-xy-frame-to-object down-pos))
                    (xy-on-frame (edraw-posn-x-y-on-frame
                                  move-pos
                                  ;; When pointing at the button part
                                  ;; of the toolbar, (posn-area
                                  ;; move-pos) becomes the symbol
                                  ;; specified in :map.
                                  ;; (e.g.edraw-editor-select-tool-text)
                                  t)))
                (if (and delta-xy xy-on-frame)
                    (cons (+ (car delta-xy) (car xy-on-frame))
                          (+ (cdr delta-xy) (cdr xy-on-frame)))
                  (cons 0 0))))
        ;;(message "%s" (posn-object-x-y new-pos))
        ;;(message "area=%s x-y=%s object-x-y=%s" (posn-area move-pos) (posn-x-y move-pos) (posn-object-x-y move-pos))
        new-event))))

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
    (let ((scroll-scale (edraw-scroll-scale editor)))
      (if (<= scroll-scale 1)
          ;; When not zooming
          (edraw-xy-round xy)
        ;; When zooming, Snap to coordinates of reciprocal of the scale
        (let ((inv-scale (/ 1.0 scroll-scale)))
          (edraw-xy
           (edraw-grid-round (edraw-x xy) inv-scale)
           (edraw-grid-round (edraw-y xy) inv-scale)))))))

(cl-defmethod edraw-mouse-click-info ((editor edraw-editor) down-event)
  (let ((down-xy (edraw-mouse-event-to-xy-raw editor down-event)))
    (concat
     " - ["
     (edraw-msg "Click:")
     (format "%g,%g" (edraw-x down-xy) (edraw-y down-xy))
     "]")))

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

(cl-defmethod edraw-update-tool-last-prefix-arg ((editor edraw-editor))
  (when-let ((tool (oref editor tool)))
    (edraw-set-prefix-arg-last-down-mouse tool)))

(cl-defmethod edraw-on-down-mouse-1 ((editor edraw-editor) down-event)
  (edraw-update-tool-last-prefix-arg editor)
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

(cl-defmethod edraw-on-down-mouse-3 ((editor edraw-editor) down-event)
  (edraw-update-tool-last-prefix-arg editor)
  (edraw-call-tool-event-handler editor down-event))

(cl-defmethod edraw-on-mouse-3 ((editor edraw-editor) down-event)
  (edraw-call-tool-event-handler editor down-event))

;;;;; Editor - Toolbar

(defvar edraw-editor-tool-list
  '(edraw-editor-tool-select
    edraw-editor-tool-rect
    edraw-editor-tool-ellipse
    edraw-editor-tool-path
    edraw-editor-tool-freehand
    edraw-editor-tool-text
    edraw-editor-tool-image
    edraw-editor-tool-custom-shape
    edraw-editor-tool-generator))

(defconst edraw-editor-toolbar-button-w 30)
(defconst edraw-editor-toolbar-button-h 24)

(cl-defmethod edraw-update-toolbar ((editor edraw-editor))
  (with-slots (overlay keymap image-scale image-toolbar (current-tool tool))
      editor
    (let* (;; Put components
           (icon-w edraw-editor-toolbar-button-w)
           (icon-h edraw-editor-toolbar-button-h)
           (components-g (edraw-svg-group))
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
                      'edraw-editor-main-menu
                      keymap)
                     nil)
                    image-map)
              (cl-incf y icon-h)
              (cl-incf y 16) ;;spacing
              ;; tool buttons
              (dolist (tool-class edraw-editor-tool-list)
                (push (edraw-editor-make-tool-button
                       components-g
                       x y
                       icon-w icon-h image-scale
                       tool-class
                       current-tool-class-name
                       keymap)
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
                        'edraw-editor-edit-tool-properties
                        keymap)
                       nil)
                      image-map)
                (cl-incf y icon-h))

              (nreverse image-map)))
           (bar-w (+ x icon-w padding))
           (bar-h (+ y padding))

           ;; Create SVG Element
           (svg (let ((svg (edraw-svg-create
                            (round (* image-scale bar-w))
                            (round (* image-scale bar-h)))))
                  (svg-gradient svg "icon-fg-gradient" 'linear
                                '((0 . "rgba(255,255,255,0.5)")
                                  (100 . "rgba(255,255,255,0.0)")))
                  (edraw-dom-append-child
                   (dom-by-tag svg 'defs)
                   (edraw-svg-ui-transparent-bg-pattern))
                  (edraw-svg-group ;;root-g
                   :parent svg
                   :transform (format "scale(%s)" image-scale)
                   (edraw-svg-rect 0 0 bar-w bar-h :fill "#888")
                   components-g)
                  svg))
           ;; Create image
           (image (edraw-svg-to-image svg
                                      :scale 1.0 ;;Cancel image-scale effect
                                      :map image-map))
           ;; Create keymap
           ;;@todo cache?
           (toolbar-keymap
            (let ((km (make-sparse-keymap)))
              (dolist (hot-spot image-map)
                (let ((key-id (nth 1 hot-spot)))
                  (define-key km (vector key-id 'mouse-1) key-id)))
              km)))
      ;; Flush old image
      (edraw-flush-toolbar-image editor)
      ;; Put IMAGE to the left side of the editor overlay
      (setq image-toolbar image)
      (overlay-put overlay
                   'before-string (propertize "*"
                                              'display image
                                              'face 'default
                                              'keymap toolbar-keymap
                                              'pointer 'arrow)))))

(cl-defmethod edraw-flush-toolbar-image ((editor edraw-editor))
  (with-slots (image-toolbar) editor
    (when image-toolbar
      (image-flush image-toolbar)
      (setq image-toolbar nil))))

(defun edraw-editor-make-toolbar-button
    (parent x y w h image-scale icon key-id help-echo selected-p)
  (let* ((x0 (floor x))
         (y0 (floor y))
         (x1 (ceiling (+ x w)))
         (y1 (ceiling (+ y h))))
    (edraw-svg-rect x0 y0 (- x1 x0) (- y1 y0)
                    :parent parent
                    :fill (if selected-p "#666" "#888") :rx 2 :ry 2)
    (edraw-svg-set-attr-string icon 'transform (format "translate(%s %s)" x0 y0))
    (edraw-dom-append-child parent icon)

    (list (cons 'rect
                (cons (cons (round (* image-scale x0))
                            (round (* image-scale y0)))
                      (cons (round (* image-scale x1))
                            (round (* image-scale y1)))))
          key-id
          (list 'pointer 'hand
                'help-echo help-echo))))

(defun edraw-editor-make-toolbar-help-echo (title command keymap)
  (let ((binding (edraw-where-is-string command keymap t)))
    (concat
     title ;;localized msg
     (if binding (concat " (" binding ")")))))



(defun edraw-editor-make-tool-button
    (parent x y w h image-scale tool-class selected-class-name keymap)
  (edraw-editor-make-toolbar-button
   parent x y w h image-scale
   (edraw-icon tool-class)
   (edraw-editor-make-tool-key-id tool-class)
   (edraw-editor-make-tool-help-echo tool-class keymap)
   (eq tool-class selected-class-name)))

(defun edraw-editor-make-tool-key-id (tool-class)
  (edraw-editor-tool-select-function-name tool-class))

(defun edraw-editor-make-tool-click-function (tool-class)
  (edraw-editor-tool-select-function-name tool-class))

(defun edraw-editor-make-tool-help-echo (tool-class keymap)
  (edraw-editor-make-toolbar-help-echo
   (edraw-name tool-class)
   (edraw-editor-tool-select-function-name tool-class)
   keymap))

(defun edraw-editor-make-tool (tool-class)
  (funcall tool-class))

(defun edraw-editor-tool-select-function-name (tool-class)
  (let ((tool-class-str (symbol-name tool-class)))
    (if (string-match "\\`edraw-editor-tool-\\([-a-z0-9]+\\)\\'" tool-class-str)
        (intern (format "edraw-editor-select-tool-%s"
                        (match-string 1 tool-class-str)))
      (intern (format "edraw-editor-select-tool--%s" tool-class)))))

(defun edraw-editor-define-tool-select-function (tool-class)
  (defalias (edraw-editor-tool-select-function-name tool-class)
    (lambda () (interactive)
      (let ((editor (edraw-current-editor)))
        (edraw-select-tool editor (edraw-editor-make-tool tool-class))))))

(defun edraw-editor-define-tool-select-functions ()
  (dolist (tool-class edraw-editor-tool-list)
    (edraw-editor-define-tool-select-function tool-class)))

(edraw-editor-define-tool-select-functions) ;;defun edraw-editor-select-tool-*

;; Icon 30x24

(defun edraw-editor-make-icon (icon-id)
  (funcall (intern (format "edraw-icon-%s" icon-id))))

(defun edraw-icon-main-menu ()
  (edraw-svg-group
   (edraw-svg-rect 3 4 24 2 :stroke-width 1 :stroke "none" :fill "#eee")
   (edraw-svg-rect 3 11 24 2 :stroke-width 1 :stroke "none" :fill "#eee")
   (edraw-svg-rect 3 18 24 2 :stroke-width 1 :stroke "none" :fill "#eee")))

(defun edraw-icon-edit-tool-properties ()
  (edraw-svg-group
   (edraw-svg-rect 3  4  3 3 :stroke "none" :fill "#ccc")
   (edraw-svg-rect 9  4 18 3 :stroke "none" :fill "#ccc")
   (edraw-svg-rect 3 10  3 3 :stroke "none" :fill "#ccc")
   (edraw-svg-rect 9 10 18 3 :stroke "none" :fill "#ccc")
   (edraw-svg-rect 3 16  3 3 :stroke "none" :fill "#ccc")
   (edraw-svg-rect 9 16 18 3 :stroke "none" :fill "#ccc")))

;; (defun edraw-preview-icon (name)
;;   (interactive "sIcon Name(e.g.tool-text): ")
;;   (message
;;    "%s"
;;    (propertize
;;     "ICON" 'display
;;     (let ((svg (svg-create 30 24)))
;;       (svg-rectangle svg 0 0 40 24 :fill "#888")
;;       (edraw-dom-append-child svg (edraw-editor-make-icon (intern name)))
;;       (svg-image svg)))))

(cl-defmethod edraw-make-toolbar-color-button ((editor edraw-editor)
                                               parent x y
                                               shape-type
                                               prop-name)
  (with-slots (keymap image-scale) editor
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
          (edraw-svg-rect ix iy iw ih
                          :parent parent
                          :fill "none" :stroke "#666" :stroke-width "1")
        (let* ((value (cdr tag-value))
               (none-p (or (null value) (string= value "none")))
               (icon (cond
                      (none-p
                       (edraw-svg-group
                        (edraw-svg-rect
                         ix iy iw ih
                         :fill "none" :stroke "#666" :stroke-width "1")
                        (edraw-svg-path
                         (concat
                          "M"
                          (mapconcat #'number-to-string
                                     (list ix iy
                                           (+ ix 2) iy
                                           (+ ix iw) (+ iy ih -2)
                                           (+ ix iw) (+ iy ih)
                                           (+ ix iw -2) (+ iy ih)
                                           ix (+ iy 2))
                                     " ")
                          "Z")
                         :fill "#f003")))
                      (t
                       (edraw-svg-group
                        (edraw-svg-rect ix iy iw ih
                                        :fill "#fff" :stroke "#444"
                                        :stroke-width "1")
                        (edraw-svg-rect ix iy iw ih
                                        :fill
                                        "url(#edraw-ui-pattern-transparent-bg")
                        (edraw-svg-rect ix iy iw ih :fill value)))))
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
            key-id
            keymap)
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
                        (:scale-direct . ,(oref editor image-scale))
                        (:recent-colors . ,(edraw-editor-recent-colors
                                            :ui-state (oref editor ui-state)))))))
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
            (meta-p (or (memq 'meta (event-modifiers down-event))
                        (memq 'alt (event-modifiers down-event))))
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
             (if (or meta-p
                     (and selected-handle
                          (edraw-same-point-p handle selected-handle)))
                 (edraw-move-on-transformed handle move-xy) ;;notify modification
               (edraw-move-with-opposite-handle-on-transformed handle move-xy)))))
        (if move-xy
            ;; Fix position
            ;; If selected handle, move it alone
            (progn
              (if (or meta-p
                      (and selected-handle
                           (edraw-same-point-p handle selected-handle)))
                  (edraw-move-on-transformed handle move-xy) ;;notify modification
                (edraw-move-with-opposite-handle-on-transformed handle move-xy))
              (message (edraw-msg "Moved handle to (%s,%s)")
                       (edraw-x (edraw-get-xy handle))
                       (edraw-y (edraw-get-xy handle))))
          ;; Click handle point
          (edraw-select-handle editor handle)
          (edraw-display-selected-object-info editor)))
      t)))

(cl-defmethod edraw-mouse-down-anchor-point ((editor edraw-editor)
                                             down-event)
  "Drag a anchor point or select it."
  (let* ((down-xy (edraw-mouse-event-to-xy-raw editor down-event)) ;;Do not any rounding coordinates
         (anchor (seq-some (lambda (shp)
                             (edraw-pick-anchor-point shp down-xy))
                           (edraw-selected-shapes editor))))
    (when anchor
      (let* ((original-xy (edraw-get-xy-transformed anchor))
             (shift-p (memq 'shift (event-modifiers down-event)))
             (opposite-vecs (when shift-p
                              (edraw-get-opposite-point-vectors anchor)))
             (move-xy nil))
        (edraw-editor-with-temp-modifications editor
          (edraw-track-dragging
           down-event
           (lambda (move-event)
             (edraw-undo-all editor) ;; Cancel previous move
             (setq move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
             (when shift-p
               (setq move-xy
                     (if opposite-vecs
                         (edraw-xy-snap-to-rect-diagonal move-xy original-xy (car opposite-vecs) (cdr opposite-vecs))
                       (edraw-xy-snap-to-45deg move-xy original-xy))))
             (edraw-move-on-transformed anchor move-xy)))) ;;notify modification
        (if move-xy
            ;; Fix position
            (progn
              (edraw-move-on-transformed anchor move-xy) ;;notify modification
              (message (edraw-msg "Moved anchor to (%s,%s)")
                       (edraw-x (edraw-get-xy anchor))
                       (edraw-y (edraw-get-xy anchor))))
          ;; Click the anchor point if the mouse does not move
          (edraw-select-anchor editor anchor)
          (edraw-display-selected-object-info editor)))
      t)))

(cl-defmethod edraw-mouse-down-shape ((editor edraw-editor) down-event)
  "Common reactions when a shape is pressed down with the mouse.

Returns t if there is a shape at the pressed position, otherwise returns nil.

If it returns t, this function always consumes the mouse up
event. For example, it consumes the mouse-1 event corresponding
to down-mouse-1 and processes drag and click."
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
               (edraw-update-property-editor-on-selection-change editor)
               (setq moving-shapes nil))
           ;; Add to selection and move
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
      (if moving-shapes
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
                      (edraw-translate shape delta-xy)) ;;notify modification
                    (message (edraw-msg "Moved by (%s,%s)")
                             (edraw-x delta-xy)
                             (edraw-y delta-xy))))
              ;; On Click
              (pcase event-type
                ('down
                 (when down-selected-p
                   (edraw-select down-shape))))
              (edraw-display-selected-object-info
               editor
               (edraw-mouse-click-info editor down-event))))
        ;; Consume `UP' event
        (edraw-track-dragging down-event (lambda (_move-event)))
        (edraw-display-selected-object-info
         editor
         (edraw-mouse-click-info editor down-event)))
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
            (progn
              (edraw-make-undo-group editor 'shapes-duplicate-by-drag
                (let ((new-shapes (edraw-duplicate-shapes target-shapes)))
                  (dolist (shape new-shapes)
                    (edraw-translate shape delta-xy)) ;;notify modification
                  (edraw-select-shapes editor new-shapes)))
              (message (edraw-msg "Duplicated %s shapes")
                       (length target-shapes)))
          ;; On Click
          ))
      ;; Handled
      t)))

(cl-defmethod edraw-context-menu-at-point ((editor edraw-editor) xy
                                           &optional pick-forced)
  (if-let ((shapes (edraw-find-shapes-by-xy editor xy pick-forced)))
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
          (ui-preview (edraw-svg-rect
                       (edraw-scroll-transform-x editor (car down-xy))
                       (edraw-scroll-transform-x editor (cdr down-xy))
                       1
                       1
                       :class "edraw-ui-read-rectangle"))
          move-xy)
      (edraw-dom-append-child ui-parent ui-preview)
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
             (edraw-invalidate-image editor)))
        (edraw-dom-remove-node ui-parent ui-preview)
        (edraw-invalidate-image editor))
      (edraw-aabb down-xy (or move-xy down-xy)))))



;;;;; Editor - Editing Tools

(cl-defmethod edraw-select-tool-default ((editor edraw-editor))
  (defvar edraw-editor-tool-list)
  (defvar edraw-editor-default-tool) ;;defcustom
  (edraw-select-tool editor
                     (edraw-editor-make-tool
                      (if (edraw-editor-tool-class-p edraw-editor-default-tool)
                          edraw-editor-default-tool
                        (car edraw-editor-tool-list)))))

(cl-defmethod edraw-select-tool ((editor edraw-editor)
                                 new-tool) ;;edraw-editor-tool object or class or nil
  (when (edraw-editor-tool-class-p new-tool)
    (setq new-tool (edraw-editor-make-tool new-tool)))

  (with-slots (tool) editor
    (when tool
      (edraw-on-deselected tool))
    (setq tool new-tool)
    (when tool
      (edraw-on-selected tool editor)
      (when edraw-editor-show-help-when-selecting-tool-p
        (edraw-print-help tool)))
    (edraw-update-toolbar editor)))

(cl-defmethod edraw-selected-tool ((editor edraw-editor))
  (oref editor tool))

(cl-defmethod edraw-selected-tool-class ((editor edraw-editor))
  (when-let ((current-tool (oref editor tool)))
    (edraw-tool-class current-tool)))

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

(edraw-editor-defcmd edraw-show-help-for-selected-tool ((editor edraw-editor))
  "Show help text for currently selected tool."
  (with-slots (tool) editor
    (when tool
      (edraw-print-help tool))))



;;;; Tool

(defun edraw-editor-tool-class-p (class)
  "Return non-nil if CLASS is a derived class name symbol of
 `edraw-editor-tool'."
  (and (class-p class)
       (child-of-class-p class 'edraw-editor-tool)))

(defclass edraw-editor-tool ()
  ((editor :type (or null edraw-editor))
   (prefix-arg-last-down-mouse :initform nil))
  :abstract t)

(cl-defmethod edraw-tool-class ((tool edraw-editor-tool))
  "Returns the class name of the TOOL object as a symbol."
  (eieio-object-class-name tool))

(cl-defmethod edraw-tool-type ((tool edraw-editor-tool))
  (let ((name (symbol-name (eieio-object-class-name tool))))
    (when (string-match "\\`edraw-editor-tool-\\(.*\\)\\'" name)
      (intern (match-string 1 name)))))

(cl-defmethod edraw-set-prefix-arg-last-down-mouse ((tool edraw-editor-tool))
  (oset tool prefix-arg-last-down-mouse current-prefix-arg))

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool))
  nil)

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool))
  )

(cl-defmethod edraw-on-down-mouse-1 ((_tool edraw-editor-tool) _down-event))
(cl-defmethod edraw-on-C-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-S-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-M-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-M-S-down-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-C-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-S-mouse-1 ((_tool edraw-editor-tool) _click-event))
(cl-defmethod edraw-on-double-mouse-1 ((_tool edraw-editor-tool) _click-event))

(cl-defmethod edraw-on-down-mouse-3 ((_tool edraw-editor-tool) _down-event))
(cl-defmethod edraw-on-mouse-3 ((tool edraw-editor-tool) click-event)
  (with-slots (editor) tool
    (let ((pick-forced (oref tool prefix-arg-last-down-mouse)) ;; C-u => ignore temporary state
          (click-xy (edraw-mouse-event-to-xy-raw editor click-event))) ;;Do not any rounding coordinates

      (cond
       ;; Handle
       ((when-let ((target-spoint (and (edraw-selected-anchor editor)
                                       (edraw-shape-point-find
                                        (edraw-selectable-handles editor)
                                        click-xy
                                        pick-forced))))
          (edraw-popup-context-menu target-spoint))) ;; Return t if popuped
       ;; Anchor
       ((when-let ((target-spoint (seq-some
                                   (lambda (shp)
                                     (edraw-pick-anchor-point shp click-xy
                                                              pick-forced))
                                   (edraw-selected-shapes editor))))
          (edraw-popup-context-menu target-spoint))) ;; Return t if popuped
       ;; Shape
       (t
        (edraw-context-menu-at-point editor click-xy pick-forced))))))

(cl-defgeneric edraw-on-selected (target selector)
  "Called when TARGET is selected by SELECTOR.")
(cl-defmethod edraw-on-selected ((tool edraw-editor-tool) (editor edraw-editor))
  (oset tool editor editor))

(cl-defgeneric edraw-on-deselected (target)
  "Called when TARGET is deselected.")
(cl-defmethod edraw-on-deselected ((tool edraw-editor-tool))
  (oset tool editor nil))



;;;;; Tool - Select Tool

(defclass edraw-editor-tool-select (edraw-editor-tool)
  ())

(cl-defmethod edraw-name ((_class (subclass edraw-editor-tool-select)))
  (edraw-msg "Select Tool"))

(cl-defmethod edraw-icon ((_class (subclass edraw-editor-tool-select)))
  (edraw-svg-group
   (edraw-svg-path "M 6 3 L 21 10 17 12 23 18 21 20 15 14 13 18 z"
                   :stroke "#ccc"
                   :stroke-width 1
                   :fill "url(#icon-fg-gradient)")))

(cl-defmethod edraw-on-deselected ((_tool edraw-editor-tool-select))
  ;;(edraw-deselect-shape (oref tool editor))
  (cl-call-next-method))

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool-select))
  (message (edraw-msg "[Select Tool] Click:Select, Drag:Range select or Move, M-Drag:Duplicate and move, S-Click:45-degree, Double Click:Properties")))

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
     ((edraw-mouse-down-shape editor down-event))

     ;; Select by rectangle
     ((let ((rect (edraw-read-rectangle editor down-event nil)))
        (unless (edraw-rect-empty-p rect)
          (dolist (shape (edraw-find-shapes-by-rect editor rect))
            ;; Toggle selection state
            (if (edraw-selected-p shape)
                (edraw-remove-shape-selection editor shape)
              (edraw-add-shape-selection editor shape)))
          t)
        (edraw-display-selected-object-info
         editor
         (edraw-mouse-click-info editor down-event))
        )))))

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
          (edraw-display-selected-object-info
           editor
           (concat " - ["
                   (edraw-msg "Range:")
                   (format "X=%g Y=%g W=%g H=%g"
                           (edraw-rect-left rect)
                           (edraw-rect-top rect)
                           (edraw-rect-width rect)
                           (edraw-rect-height rect))
                   "]"))
          t)))

     ;; Deselect
     (t
      (edraw-deselect-all-shapes editor)
      (edraw-display-selected-object-info
       editor
       (edraw-mouse-click-info editor down-event))))))

(cl-defmethod edraw-on-double-mouse-1 ((tool edraw-editor-tool-select)
                                       click-event)
  (with-slots (editor) tool
    (let* ((click-xy (edraw-mouse-event-to-xy-raw editor click-event))
           (shape (car (edraw-find-shapes-by-xy editor click-xy))))
      (when shape
        (edraw-edit-properties shape)))))



;;;;; Tool - Rect Tool

(defclass edraw-editor-tool-rect (edraw-editor-tool)
  ()
  )

(cl-defmethod edraw-name ((_class (subclass edraw-editor-tool-rect)))
  (edraw-msg "Rect Tool"))

(cl-defmethod edraw-icon ((_class (subclass edraw-editor-tool-rect)))
  (edraw-svg-group
   (edraw-svg-rect
    6.5 6.5 18 12
    :stroke-width 1 :stroke "#ccc" :fill "url(#icon-fg-gradient)")))

(cl-defmethod edraw-shape-type-to-create ((_class (subclass edraw-editor-tool-rect)))
  'rect)

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-rect))
  'rect)

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool-rect))
  (message (edraw-msg "[Rect Tool] Drag:Add rect, S-Drag:Square")))

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
        (let* ((shape (edraw-create-shape-default ;;notify modification
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
        (let ((shape (edraw-create-shape-default ;;notify modification
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

(cl-defmethod edraw-name ((_class (subclass edraw-editor-tool-ellipse)))
  (edraw-msg "Ellipse Tool"))

(cl-defmethod edraw-icon ((_class (subclass edraw-editor-tool-ellipse)))
  (edraw-svg-group
   (edraw-svg-ellipse
    15 12 9 6
    :stroke-width 1 :stroke "#ccc" :fill "url(#icon-fg-gradient)")))

(cl-defmethod edraw-shape-type-to-create ((_class (subclass edraw-editor-tool-ellipse)))
  'ellipse)

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-ellipse))
  'ellipse)

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool-ellipse))
  (message (edraw-msg "[Ellipse Tool] Drag:Add ellipse, S-Drag:Square")))

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
        (let* ((shape (edraw-create-shape-default ;;notify modification
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
        (let ((shape (edraw-create-shape-default ;;notify modification
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
  ())

(cl-defmethod edraw-name ((_class (subclass edraw-editor-tool-text)))
  (edraw-msg "Text Tool"))

(cl-defmethod edraw-icon ((_class (subclass edraw-editor-tool-text)))
  (edraw-svg-group
   (edraw-svg-path
    ;;     8 9 11  14 15 16  19 21 22
    ;; 4.5 +----      +---        -+
    ;;   5 |   +     + +       +   |
    ;;   7 + +       | |         + +
    ;;             + + + +
    ;;   18        +-----+
    "M 8 4.5 L 22 4.5 L 22 7 L 21 7 Q 21 5 19 5 L 16 5 L 16 17.5 L18 17.5 L 18 18 L 12 18 L 12 17.5 L 14 17.5 L 14 5 L 11 5 Q 9 5 9 7 L 8 7 z"
    :stroke "#ccc" :stroke-width 1 :fill "url(#icon-fg-gradient)")))

(cl-defmethod edraw-shape-type-to-create ((_class (subclass edraw-editor-tool-text)))
  'text)

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-text))
  'text)

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool-text))
  (message (edraw-msg "[Text Tool] Click:Add or Change, C-u Click:Add, C-Click:Glue")))

(cl-defmethod edraw-on-mouse-1 ((tool edraw-editor-tool-text) click-event)
  (or (and (not (oref tool prefix-arg-last-down-mouse))
           (edraw-change-text-shape tool click-event))
      (edraw-put-text-shape tool click-event edraw-snap-text-to-shape-center)))

(cl-defmethod edraw-on-S-mouse-1 ((tool edraw-editor-tool-text) click-event)
  (edraw-put-text-shape tool click-event (not edraw-snap-text-to-shape-center)))

(cl-defmethod edraw-on-C-mouse-1 ((tool edraw-editor-tool-text) click-event)
  (edraw-put-text-shape tool click-event (not edraw-snap-text-to-shape-center)))

(cl-defmethod edraw-change-text-shape ((tool edraw-editor-tool text)
                                       click-event)
  (let* ((editor (oref tool editor))
         (down-xy (edraw-mouse-event-to-xy-raw editor click-event)) ;;Do not any rounding coordinates
         (down-shape (car (edraw-find-shapes-by-xy editor down-xy)))) ;;front
    (when (and down-shape
               (edraw-shape-text-p down-shape))
      (edraw-select-shape editor down-shape)
      (let* ((old-text (edraw-get-property down-shape 'text))
             (new-text (read-string (edraw-msg "Change Text: ") (or old-text ""))))
        (edraw-set-property down-shape 'text new-text)
        t))))

(cl-defmethod edraw-put-text-shape ((tool edraw-editor-tool-text) click-event
                                    snap-to-shape-center-p)
  (with-slots (editor) tool
    (let* ((click-xy (edraw-mouse-event-to-xy-raw editor click-event))
           (click-xy-snapped
            (or (and snap-to-shape-center-p
                     (edraw-snap-text-to-back-shape-center tool click-xy))
                (edraw-snap-xy editor click-xy)))
           (glue-dst-shape
            (when (memq 'control (event-modifiers click-event))
              (or (car (edraw-find-shapes-by-xy editor click-xy))
                  (error (edraw-msg "No glue target")))))
           (text (read-string (if glue-dst-shape
                                  (edraw-msg "Glued Text: ")
                                (edraw-msg "Text: ")))))
      (unless (string-empty-p text)
        (edraw-deselect-all-shapes editor)
        (edraw-make-undo-group editor 'text-tool-create
          (let ((shape (edraw-create-shape-default ;;notify modification
                        editor
                        (edraw-svg-body editor)
                        'text
                        'x (car click-xy-snapped)
                        'y (cdr click-xy-snapped)
                        'text text)))

            (when glue-dst-shape
              (edraw-glue-to shape glue-dst-shape))

            (edraw-select-shape editor shape)))))))

(cl-defmethod edraw-snap-text-to-back-shape-center ((tool edraw-editor-tool-text) xy)
  (with-slots (editor) tool
    (when-let ((font-size (edraw-svg-attr-length-to-number
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

(defclass edraw-editor-tool-image (edraw-editor-tool)
  ()
  )

(cl-defmethod edraw-name ((_class (subclass edraw-editor-tool-image)))
  (edraw-msg "Image Tool"))

(cl-defmethod edraw-icon ((_class (subclass edraw-editor-tool-image)))
  (edraw-svg-group
   (edraw-svg-rect
    4.5 4.5 22 15
    :stroke-width 1 :stroke "#ccc" :fill "url(#icon-fg-gradient)")
   (edraw-svg-ellipse
    11 9 2.5 2.5 :fill "#888")
   (edraw-svg-path
    "M10 13 14 15 20 10 26 16 26 18 5 18 10 13Z" :fill "#666")))

(cl-defmethod edraw-shape-type-to-create ((_class (subclass edraw-editor-tool-image)))
  'image)

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-image))
  'image)

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool-image))
  (message (edraw-msg "[Image Tool] Click:Add image(original size), Drag:Add image(specified size), S-Drag:Square")))

(cl-defmethod edraw-on-S-down-mouse-1 ((tool edraw-editor-tool-image)
                                       down-event)
  (edraw-on-down-mouse-1 tool down-event))

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-image)
                                     down-event)
  (with-slots (editor) tool
    (when-let ((rect (edraw-read-rectangle editor down-event t))
               (image-file
                (expand-file-name
                 (read-file-name (edraw-msg "Image File: ") nil nil t))))

      ;; Warn that images in different directories cannot be displayed
      ;; for security reasons.
      ;; @todo Should it error? Or copy to base-dir? Or embed base64 data?
      (let ((base-dir (expand-file-name default-directory))
            (image-dir (file-name-directory image-file)))
        (unless (string-prefix-p base-dir image-dir)
          (message (edraw-msg "WARNING: Images in other directories cannot be displayed for security reasons"))))

      ;; Get image size
      (when (edraw-rect-empty-p rect)
        (let* ((image-spec (create-image image-file nil nil :scale 1))
               (image-size (ignore-errors
                             (progn
                               (image-flush image-spec)
                               (image-size image-spec t)))))
          (unless image-size
            (message (edraw-msg "Failed to get image size"))
            (setq image-size (edraw-xy 100 100)))
          (edraw-xy-assign (edraw-rect-xy1 rect)
                           (edraw-xy-add (edraw-rect-xy1 rect) image-size))))

      ;; Create
      (let ((shape (edraw-create-shape-default ;;notify modification
                    editor
                    (edraw-svg-body editor)
                    'image
                    'x (edraw-rect-left rect)
                    'y (edraw-rect-top rect)
                    'width (edraw-rect-width rect)
                    'height (edraw-rect-height rect)
                    (edraw-svg-href-symbol) (file-relative-name image-file))))
        (edraw-select-shape editor shape)))))



;;;;; Tool - Path Tool

(defclass edraw-editor-tool-path (edraw-editor-tool)
  ((last-anchor
    :initform nil
    :type (or null edraw-shape-point-path-anchor))))

(cl-defmethod edraw-name ((_class (subclass edraw-editor-tool-path)))
  (edraw-msg "Path Tool"))

(cl-defmethod edraw-icon ((_class (subclass edraw-editor-tool-path)))
  (edraw-svg-group
   (edraw-svg-path "M 4 18 Q 10 6 16 6 Q 22 6 28 18"
                   :stroke-width 1 :stroke "#ccc" :fill "none")
   (edraw-svg-rect 14 4 4 4 :stroke "none" :fill "url(#icon-fg-gradient)")
   (edraw-svg-line 7 6 25 6 :stroke-width 0.5 :stroke "#ccc")
   (edraw-svg-circle 7 6 0.8 :stroke-width 1 :stroke "#ccc" :fill "none")
   (edraw-svg-circle 25 6 0.8 :stroke-width 1 :stroke "#ccc" :fill "none")))

(cl-defmethod edraw-shape-type-to-create ((_class (subclass edraw-editor-tool-path)))
  'path)

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-path))
  'path)

(cl-defmethod edraw-on-deselected ((tool edraw-editor-tool-path))
  (edraw-clear tool)
  (cl-call-next-method))

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool-path))
  (message (edraw-msg "[Path Tool] Click:Add Anchor, Drag:Add Anchor and Handles, a:New Path
(On Endpoint) Click:Continue/Connect, C-u Click:Add Anchor
(On Point) Click:Select, Drag:Move / (On Handle) M-Drag:Move
(On Anchor) M-Click:Make Corner, M-Drag:Recreate Handles
(On Another Shape) C-Click:Glue / S-Click/drag: Limit to 45 degrees")))

(cl-defmethod edraw-clear ((tool edraw-editor-tool-path))
  "Reset the tool state."
  (with-slots (editor last-anchor) tool
    (when last-anchor
      (setq last-anchor nil))))

(cl-defmethod edraw-validate-last-anchor ((tool edraw-editor-tool-path))
  "Check the validity of the `last-anchor' slot and set it to nil
 if it is invalid."
  (with-slots (last-anchor) tool
    (when last-anchor
      (unless (and (edraw-valid-point-p last-anchor)
                   (edraw-endpoint-p last-anchor))
        ;; (message "Last anchor is invalid")
        (setq last-anchor nil)))))

(cl-defmethod edraw-on-S-down-mouse-1 ((tool edraw-editor-tool-path) down-event)
  (edraw-on-down-mouse-1 tool down-event))

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-path) down-event)
  (edraw-validate-last-anchor tool)

  (with-slots (editor last-anchor) tool

    (let* ((ignore-existing-point-p (equal current-prefix-arg '(4)))
           ;;@todo customize
           (enable-connect-p (not ignore-existing-point-p))
           (enable-move-p (not ignore-existing-point-p)))
      (cond
       ((and enable-connect-p
             (or
              (edraw-mouse-down-continue-path tool down-event)

              (edraw-mouse-down-connect-to-same-path tool down-event)

              (edraw-mouse-down-connect-to-another-path tool down-event))))
       ((and enable-move-p
             (or
              ;; Drag or click a handle point of selected anchor point
              (edraw-mouse-down-handle-point editor down-event)

              ;; Drag or click a anchor point of selected shape
              (edraw-mouse-down-anchor-point editor down-event))))
       (t
        (edraw-mouse-down-add-anchor tool down-event))))))

(cl-defmethod edraw-mouse-down-add-anchor ((tool edraw-editor-tool-path)
                                           down-event)
  "Add anchor when mouse is down in path tool."
  (with-slots (editor last-anchor) tool
    (edraw-make-undo-group editor 'path-tool-add-new-point
      (let* (;; Get the path shape to be edited
             (shape (if last-anchor
                        (edraw-parent-shape last-anchor)
                      ;; Add a new shape and select it
                      (edraw-deselect-all-shapes editor)
                      (let ((new-shape
                             (edraw-create-shape-default ;;notify modification
                              editor (edraw-svg-body editor) 'path)))
                        (edraw-select-shape editor new-shape)
                        new-shape)))
             ;; Calculate coordinates
             (shift-p (memq 'shift (event-modifiers down-event)))
             (last-xy (when last-anchor (edraw-get-xy last-anchor)))
             (down-xy (edraw-mouse-event-to-xy-snapped editor down-event))
             (new-xy (if (and shift-p last-xy)
                         (edraw-xy-snap-to-45deg down-xy last-xy)
                       down-xy))
             ;; Add a new anchor
             (new-anchor
              ;;notify modification
              (if last-anchor
                  (edraw-add-anchor-to-open-end last-anchor new-xy)
                (edraw-add-anchor shape new-xy))))
        (unless new-anchor
          (error "Failed to add new anchor"))

        ;; Select last anchor point
        (edraw-select-anchor editor new-anchor)
        (edraw-display-selected-object-coordinates editor)

        ;; Drag handle points of the new point
        (unless (edraw-create-symmetrical-handles-on-anchor
                 new-anchor down-event editor shift-p)
          ;; Re-display anchor position
          (edraw-display-selected-object-coordinates editor))

        (setq last-anchor new-anchor)))))

(cl-defmethod edraw-mouse-down-continue-path
  ((tool edraw-editor-tool-path) down-event)
  "Start continuing a path when mouse is down in path tool."
  (with-slots (editor last-anchor) tool
    (when (null last-anchor)
      (when-let (;; Find the end anchor that was mouse down
                 (anchor
                  ;;@todo support all selected shapes or all shapes
                  (when-let ((selected-path
                              (edraw-cast (edraw-last-selected-shape editor)
                                          'edraw-shape-path)))
                    (let ((target-anchors
                           (edraw-get-endpoints selected-path))
                          (down-xy
                           ;;Do not any rounding coordinates
                           (edraw-mouse-event-to-xy-raw editor down-event)))
                      (seq-find (lambda (anchor)
                                  (edraw-hit-input-p anchor down-xy))
                                target-anchors)))))
        (setq last-anchor anchor)

        (message (edraw-msg "Continue"))
        (edraw-select-anchor editor anchor)

        ;; Drag (Add handles)
        (edraw-drag-handle-on-click-anchor anchor nil nil down-event editor)
        t))))

(cl-defmethod edraw-mouse-down-connect-to-same-path
  ((tool edraw-editor-tool) down-event)
  "Connect endpoints within the same path when mouse is down in path tool."
  (with-slots (editor last-anchor) tool
    (when last-anchor
      (when-let (;; Find the end anchor that was mouse down
                 (anchor
                  (let* ((shape (edraw-parent-shape last-anchor))
                         (down-xy
                          ;;Do not any rounding coordinates
                          (edraw-mouse-event-to-xy-raw editor down-event)))
                    (seq-find (lambda (anchor)
                                (and
                                 ;; Exclude last-anchor
                                 (not (edraw-same-point-p anchor last-anchor))
                                 (edraw-hit-input-p anchor down-xy)))
                              (edraw-get-endpoints shape)))))
        (edraw-make-undo-group editor 'path-tool-connect-subpaths
          (let ((from-anchor last-anchor)
                ;; Connect LAST-ANCHOR to pressed ANCHOR
                (new-anchor (edraw-connect last-anchor anchor))) ;;notify modification
            (when new-anchor
              (edraw-clear tool)
              (edraw-select-anchor editor new-anchor)
              (if (edraw-in-closed-subpath-p new-anchor)
                  (message (edraw-msg "Closed"))
                (message (edraw-msg "Connected"))))

            ;; Drag the `backward' handle of NEW-ANCHOR
            (edraw-drag-handle-on-click-anchor new-anchor from-anchor t
                                               down-event editor)
            t))))))

(cl-defmethod edraw-mouse-down-connect-to-another-path
  ((tool edraw-editor-tool) down-event)
  "Connect endpoints of different paths when mouse is down in path tool."
  (with-slots (editor last-anchor) tool
    (when last-anchor
      (when-let (;; Find the end anchor that was mouse down
                 (anchor
                  (let ((editing-path (edraw-parent-shape last-anchor))
                        (down-xy
                         ;;Do not any rounding coordinates
                         (edraw-mouse-event-to-xy-raw editor down-event)))
                    (seq-some
                     (lambda (shape)
                       (let ((path (edraw-cast shape 'edraw-shape-path)))
                         (when (and path
                                    (not (eq path editing-path)))
                           (seq-find (lambda (anchor)
                                       (edraw-hit-input-p anchor down-xy))
                                     (edraw-get-endpoints path)))))
                     ;; @todo Inefficient
                     (nreverse (edraw-all-shapes editor))))))
        (edraw-make-undo-group editor 'path-tool-connect-to-another-path
          (let ((from-anchor last-anchor)
                ;; Connect last anchor to pressed anchor
                (new-anchor (edraw-connect last-anchor anchor))) ;;notify modification
            (when new-anchor
              (edraw-clear tool)
              (edraw-select-shape editor (edraw-parent-shape new-anchor))
              (edraw-select-anchor editor new-anchor)
              (message (edraw-msg "Connected")))

            (edraw-drag-handle-on-click-anchor new-anchor from-anchor t
                                               down-event editor)
            t))))))

(defun edraw-drag-handle-on-click-anchor (anchor from-anchor opposite-p
                                                 down-event editor)
  "Set the ANCHOR handle position by dragging when mouse is down in path tool.

FROM-ANCHOR is the anchor clicked just before ANCHOR. It is used
to know the input direction. If ANCHOR is the first point,
specify nil.

When OPPOSITE-P is non-nil, the input coordinates are the point
symmetrical to the mouse coordinates."
  (let ((shift-p (memq 'shift (event-modifiers down-event)))
        (anchor-xy (edraw-get-xy-transformed anchor))
        (backward-p
         (if from-anchor
             (edraw-same-point-p (edraw-previous-anchor-round anchor)
                                 from-anchor)
           (edraw-first-anchor-p anchor)))
        dragged-handle
        move-xy)
    (edraw-editor-with-temp-modifications editor
      (edraw-track-dragging
       down-event
       (lambda (move-event)
         (setq move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
         (when shift-p
           (setq move-xy (edraw-xy-snap-to-45deg move-xy anchor-xy)))
         (unless dragged-handle
           (setq dragged-handle
                 (if backward-p
                     (edraw-get-backward-handle anchor)
                   (edraw-get-forward-handle anchor))))
         (edraw-move-with-opposite-handle-on-transformed ;;notify modification
          dragged-handle
          (if opposite-p
              (edraw-xy-sub (edraw-xy-nmul 2 anchor-xy) move-xy)
            move-xy)))))

    ;; Note: Make sure that undo during the drag above does not
    ;; invalidate the ANCHOR, otherwise you must re-obtain the anchor
    ;; using `edraw-anchor-index-in-path' and `edraw-get-nth-anchor-point'.
    (when (and move-xy dragged-handle)
      ;;(edraw-select-anchor editor anchor)
      (edraw-move-with-opposite-handle-on-transformed ;;notify modification
       dragged-handle
       (if opposite-p
           (edraw-xy-sub (edraw-xy-nmul 2 anchor-xy) move-xy)
         move-xy))
      t)))

(defun edraw-create-symmetrical-handles-on-anchor
    (anchor down-event editor shift-p)
  "Set the position of the ANCHOR handles by dragging when mouse is
down in path tool."
  (let (move-xy dragged-handle)
    ;; Drag handle points of the ANCHOR
    (let ((anchor-xy (edraw-get-xy anchor))
          (backward-p (not (edraw-last-anchor-p anchor))))
      (edraw-editor-with-temp-modifications editor
        (edraw-track-dragging
         down-event
         (lambda (move-event)
           (setq move-xy (edraw-mouse-event-to-xy-snapped editor move-event))
           (when shift-p
             (setq move-xy (edraw-xy-snap-to-45deg move-xy anchor-xy)))

           (unless dragged-handle
             (setq dragged-handle
                   (if backward-p
                       (edraw-get-backward-handle anchor)
                     (edraw-get-forward-handle anchor))))

           (edraw-move-with-opposite-handle-symmetry-on-transformed
            dragged-handle
            move-xy);;notify modification
           ))))

    ;; Note: Make sure that undo during the drag above does not
    ;; invalidate the ANCHOR, otherwise you must re-obtain the anchor
    ;; using `edraw-anchor-index-in-path' and `edraw-get-nth-anchor-point'.
    (when (and move-xy dragged-handle)
      (edraw-select-anchor editor anchor)
      (edraw-move-with-opposite-handle-symmetry-on-transformed
       dragged-handle
       move-xy);;notify modification
      t)))


(cl-defmethod edraw-on-C-down-mouse-1 ((tool edraw-editor-tool-path)
                                       down-event)
  (edraw-validate-last-anchor tool)

  ;; Add a new anchor and glue the anchor and the overlapping shape
  (with-slots (editor last-anchor) tool
    ;; C-Click
    (let* ((editing-path (when last-anchor
                           (edraw-parent-shape last-anchor)))
           (down-xy (edraw-mouse-event-to-xy-raw editor down-event))
           ;; Find glue target
           (dst-shape (car (remq editing-path
                                 (edraw-find-shapes-by-xy editor down-xy)))))
      (unless dst-shape
        (error (edraw-msg "No glue target")))

      (edraw-make-undo-group editor 'path-tool-add-new-point
        ;; Add a new shape
        (when (null editing-path)
          (edraw-deselect-all-shapes editor)
          (setq editing-path
                (edraw-create-shape-default ;;notify modification
                 editor (edraw-svg-body editor) 'path))
          ;; Select new shape
          (edraw-select-shape editor editing-path))

        (let* ((new-xy down-xy)
               ;; Add a new point
               (new-anchor
                (progn
                  ;;notify modification
                  (if last-anchor
                      (edraw-add-anchor-to-open-end last-anchor new-xy)
                    (edraw-add-anchor editing-path new-xy)))))

          ;; Glue
          (edraw-glue-to new-anchor dst-shape)

          ;; Select last anchor point
          (edraw-select-anchor editor new-anchor)

          ;; Next
          (if last-anchor
              ;; End path
              (edraw-clear tool)
            ;; Start path
            (setq last-anchor new-anchor)))))))

(cl-defmethod edraw-on-M-down-mouse-1 ((tool edraw-editor-tool-path)
                                       down-event)
  (edraw-validate-last-anchor tool)

  (with-slots (editor) tool
    (or
     (edraw-mouse-down-handle-point editor down-event)
     (edraw-recreate-symmetrical-handles-on-click-anchor tool down-event)
     )))

(cl-defmethod edraw-recreate-symmetrical-handles-on-click-anchor
  ((tool edraw-editor-tool-path) down-event)
  (let* ((editor (oref tool editor))
         ;;Do not any rounding coordinates
         (down-xy (edraw-mouse-event-to-xy-raw editor down-event))
         (anchor (seq-some (lambda (shp)
                             (edraw-pick-anchor-point shp down-xy))
                           (edraw-selected-shapes editor))))
    (when anchor
      (edraw-select-anchor editor anchor)
      (or
       ;; Drag
       (edraw-create-symmetrical-handles-on-anchor
        anchor down-event editor (memq 'shift (event-modifiers down-event)))
       ;; Click
       (edraw-make-corner anchor))
      t)))



;;;;; Tool - Freehand Tool

(defcustom edraw-editor-tool-freehand-dragged-outside-view 'draw-outside
  "Behavior when dragged outside view in the Freehand Tool."
  :group 'edraw-editor
  :type '(choice
          (const :tag "Finish drawing" nil)
          (const :tag "Draw outside view area" draw-outside)
          (const :tag "Clip to view area" clip)))

(defclass edraw-editor-tool-freehand (edraw-editor-tool)
  ())

(cl-defmethod edraw-name ((_class (subclass edraw-editor-tool-freehand)))
  (edraw-msg "Freehand Tool"))

(cl-defmethod edraw-icon ((_class (subclass edraw-editor-tool-freehand)))
  (edraw-svg-group
   (edraw-svg-path
    "M 4 19 C 15 -3 14 31 27 5"
    :stroke-width 1 :stroke "#ccc" :fill "none")))

(cl-defmethod edraw-shape-type-to-create ((_class (subclass edraw-editor-tool-freehand)))
  'path)

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-freehand))
  'path)

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool-freehand))
  (message (edraw-msg "[Freehand Tool] Drag:Add path")))

(defcustom edraw-editor-tool-freehand-separate-smoothing-process nil
  "Non-nil means separating path object creation and smoothing processing.

Since path object creation and line smoothing are performed
separately, two pieces of undo data are also registered.

That option was added to develop the smoothing process and to
check the difference before and after smoothing."
  :group 'edraw-editor
  :type 'boolean)

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
        (let ((preview-path (edraw-create-shape-default
                             editor (edraw-svg-body editor) 'path)))
          (unwind-protect
              (progn
                ;; Add the first point of the path
                (push down-xy points)
                (edraw-add-anchor preview-path down-xy)

                ;; Add new points on dragging
                (edraw-track-dragging
                 down-event
                 (lambda (move-event)
                   (let ((move-xy
                          (edraw-mouse-event-to-xy-snapped
                           editor
                           (if edraw-editor-tool-freehand-dragged-outside-view
                               (edraw-convert-mouse-event-on-down-object
                                move-event
                                down-event)
                             move-event))))
                     ;;@todo realtime simplification & smoothing the path

                     ;; Clip
                     (when (eq edraw-editor-tool-freehand-dragged-outside-view
                               'clip)
                       (setq move-xy
                             (edraw-xy
                              (edraw-clamp
                               (edraw-x move-xy)
                               (edraw-scroll-visible-area-left editor)
                               (edraw-scroll-visible-area-right editor))
                              (edraw-clamp
                               (edraw-y move-xy)
                               (edraw-scroll-visible-area-top editor)
                               (edraw-scroll-visible-area-bottom editor)))))

                     ;; Add point
                     (unless (edraw-xy-equal-p move-xy last-xy)
                       (push move-xy points)
                       (edraw-add-anchor preview-path move-xy)
                       (setq last-xy move-xy))))
                 nil nil nil nil
                 (and edraw-editor-tool-freehand-dragged-outside-view t)))
            (edraw-remove preview-path))))

      ;; Create
      (when (cdr points) ;;(>= (length points) 2)
        (setq points (nreverse points))
        (when-let ((path-data
                    (if (or
                         (edraw-get-setting editor 'grid-visible) ;;@todo Checking while snapping should be a dedicated predicate.
                         edraw-editor-tool-freehand-separate-smoothing-process)
                        ;; If snapped to grid, don't smooth.
                        ;; or when separating smoothing process.
                        (edraw-editor-tool-freehand--make-poly-line points)
                      ;; Smooth
                      (edraw-editor-tool-freehand--make-smooth
                       points
                       (edraw-scroll-scale editor)))))
          ;; Add a new path shape
          (let ((path-obj
                 (edraw-create-shape-default ;;modify
                  editor (edraw-svg-body editor) 'path
                  'd path-data)))

            ;; Separated smoothing process
            (when edraw-editor-tool-freehand-separate-smoothing-process
              (when-let ((path-data-smoothed
                          (edraw-editor-tool-freehand--make-smooth
                           points
                           (edraw-scroll-scale editor))))
                (edraw-set-property path-obj 'd path-data-smoothed)))
            path-obj))))))

(defcustom edraw-editor-tool-freehand-smoothing-method 'bezier-fitting
  "The smoothing method used by the Freehand tool."
  :group 'edraw-editor
  :type '(choice
          (const :tag "Do not smooth" nil)
          (const :tag "Smooth each point individually" each-point)
          (const :tag "Approximate the whole with as few Bezier curves as possible" bezier-fitting)
          (function :tag "A function that creates path data")))

(defun edraw-editor-tool-freehand--make-smooth (points &optional scroll-scale)
  "Create path data from the list of POINTS.

POINTS is a list of coordinates (edraw-xy).

Returns the command string that can be specified in the d
attribute of the path element as a string."
  (pcase edraw-editor-tool-freehand-smoothing-method
    ('nil (edraw-editor-tool-freehand--make-poly-line points))
    ('each-point (edraw-editor-tool-freehand--smooth-each-point points))
    ('bezier-fitting (edraw-editor-tool-freehand--smooth-bezier-fitting
                      points
                      scroll-scale))
    ((and (pred functionp) fun) (funcall fun points))
    (_ (edraw-editor-tool-freehand--smooth-bezier-fitting
        points
        scroll-scale))))

(defun edraw-editor-tool-freehand--make-poly-line (points)
  (or
   (when points
     (let ((prev-xy (car points)))
       (concat
        (edraw-path-cmdstr-move (car points))
        (mapconcat (lambda (xy)
                     (prog1 (edraw-path-cmdstr-line xy prev-xy)
                       (setq prev-xy xy)))
                   (cdr points)))))
   ""))

(defun edraw-editor-tool-freehand--smooth-each-point (points)
  (let ((result nil)
        (prev-xy nil)
        (prev-backward-xy nil)
        (prev-forward-xy nil))
    (while points
      (let* ((curr-xy (car points))
             (next-xy (cadr points))
             (pn (edraw-path-anchor-make-smooth-xy curr-xy prev-xy next-xy))
             (curr-backward-xy (or (car pn) curr-xy))
             (curr-forward-xy (or (cdr pn) curr-xy)))
        (if prev-xy
            (setq result (concat result
                                 (edraw-path-cmdstr-curve curr-xy
                                                          curr-backward-xy
                                                          prev-forward-xy
                                                          prev-xy
                                                          prev-backward-xy)))
          (setq result (concat result
                               (edraw-path-cmdstr-move curr-xy))))
        (setq prev-xy curr-xy
              prev-forward-xy curr-forward-xy
              prev-backward-xy curr-backward-xy
              points (cdr points))))
    result))

(defcustom edraw-editor-tool-freehand-bezier-fitting-tolerance 1.0
  "The tolerance for how far the input point can deviate from the
Bezier curve when performing Bezier fitting.

Valid only when `edraw-editor-tool-freehand-smoothing-method' is
\\='bezier-fitting."
  :group 'edraw-editor
  :type 'number)

(define-obsolete-variable-alias
  'edraw-editor-tool-freehand--bezier-fitting-corner-angle-min
  'edraw-editor-tool-freehand-bezier-fitting-corner-angle-min
  "2024-06-12")
(defcustom edraw-editor-tool-freehand-bezier-fitting-corner-angle-min 45
  "The minimum angle at which an input point will be recognized as
a corner when the freehand tool performs Bezier fitting. The unit
is degrees.

Each point entered with a pointer device is considered a corner
if the angle between the vector from the previous point and the
vector to the next point is greater than or equal to this value.

At the same time, the distance to the previous and next points is
also taken into account, and the threshold can be set with
`edraw-editor-tool-freehand-bezier-fitting-corner-distance-min'.

This variable only makes sense when
`edraw-editor-tool-freehand-smoothing-method' is \\='bezier-fitting."
  :group 'edraw-editor
  :type 'number)

(define-obsolete-variable-alias
  'edraw-editor-tool-freehand--bezier-fitting-corner-distance-min
  'edraw-editor-tool-freehand-bezier-fitting-corner-distance-min
  "2024-06-12")
(defcustom edraw-editor-tool-freehand-bezier-fitting-corner-distance-min 5
  "The minimum distance at which an input point will be recognized
as a corner when the freehand tool performs Bezier fitting.

Each point entered with a pointer device is considered a corner
if the distance from the previous and next points is greater
than or equal to this value.

At the same time, the setting value of
`edraw-editor-tool-freehand-bezier-fitting-corner-distance-min' is
also taken into account.

This variable only makes sense when
`edraw-editor-tool-freehand-smoothing-method' is \\='bezier-fitting."
  :group 'edraw-editor
  :type 'number)

(defcustom edraw-editor-tool-freehand-bezier-fitting-max-div-distance 20
  "Each section of the input polyline will be divided so that it
 does not exceed this length.
If nil, no division will be performed."
  :group 'edraw-editor
  :type '(choice (number :tag "Length")
                 (const :tag "No division" nil)))

(defconst edraw-editor-tool-freehand--straight-line-tolerance 0.9)

(defun edraw-editor-tool-freehand--smooth-bezier-fitting (points
                                                          scroll-scale)
  (when edraw-editor-tool-freehand-bezier-fitting-max-div-distance
    (edraw-xy-points-divide-long-sections
     points
     (/ edraw-editor-tool-freehand-bezier-fitting-max-div-distance
        (float scroll-scale))))

  (when-let ((curves
              ;; Split POINTS at corners
              (edraw-xy-points-to-curves
               (edraw-xy-remove-consecutive-same-points points)
               edraw-editor-tool-freehand-bezier-fitting-corner-angle-min
               (/ edraw-editor-tool-freehand-bezier-fitting-corner-distance-min
                  (float scroll-scale))
               (/ edraw-editor-tool-freehand--straight-line-tolerance
                  (float scroll-scale)))))
    ;;(message "curves=%s" (length curves))
    ;; Create Bezier curves and concat them
    (mapconcat (lambda (curve)
                 (edraw-xy-points-to-smooth-path-data
                  curve
                  edraw-editor-tool-freehand-bezier-fitting-tolerance
                  (not (eq curve (car curves)))))
               curves
               "")))

(defun edraw-xy-points-straight-p (start-xy
                                   points end
                                   &optional
                                   end-v end-len
                                   perp-dist-tolerance)
  "Return non-nil if the points are arranged in order on a straight line.

Check that all points lie on a straight line from the point
START-XY to the vector END-V.

Also check that all points are lined up in order in the direction
of the vector END-V. Return nil if there is a point that returns
to the opposite direction.

The range to be checked is from POINTS to just before END. POINTS
is a list of `edraw-xy'. END is a cons cell reachable from POINTS.

PERP-DIST-TOLERANCE is the tolerance for how much deviation from
the line in the vertical direction is allowed.

If END-V is omitted, it is the car of END minus START-XY. If
END-LEN is omitted, it is the length of the vector END-V. If
these are calculated in advance, specifying them can eliminate
unnecessary calculations."
  (unless end-v (setq end-v (edraw-xy-sub (car end) start-xy)))
  (unless end-len (setq end-len (edraw-xy-length end-v)))
  (unless perp-dist-tolerance (setq perp-dist-tolerance 1.0))
  (when points
    (let* ((end-uv (if (< 1e-6 end-len) (edraw-xy-divn end-v end-len) nil))
           (it points)
           (prev-x 0))
      (when end-uv
        (while (and it
                    (not (eq it end))
                    (let* ((curr-xy (car it))
                           (curr-v (edraw-xy-sub curr-xy start-xy))
                           (x (edraw-xy-dot end-uv curr-v))
                           (y (abs (edraw-xy-perpdot end-uv curr-v))))
                      (when (and (<= prev-x x) ;; Moving forward
                                 (<= y perp-dist-tolerance)) ;; Deviation
                        (setq prev-x x)
                        (setq it (cdr it))
                        t))))
        (eq it end)))))
;; TEST: (let ((points '((10 . 20) (12 . 21) (18 . 20)))) (edraw-xy-points-straight-p (car points) (cdr points) (nthcdr 2 points))) => t
;; TEST: (let ((points '((10 . 20) (12 . 21) (8 . 20)))) (edraw-xy-points-straight-p (car points) (cdr points) (nthcdr 2 points))) => nil

(defun edraw-xy-points-straight-last (start-xy points min-distance
                                               perp-dist-tolerance)
  "Return last point on straight line from the starting point."
  (when (and start-xy points)
    (let ((it points)
          ;;(count 0)
          result-xy)
      (while (and it
                  (let* ((it-xy (car it))
                         (it-v (edraw-xy-sub it-xy start-xy))
                         (it-len (edraw-xy-length it-v)))
                    (when (edraw-xy-points-straight-p
                           start-xy points it
                           it-v it-len perp-dist-tolerance)
                      ;;(cl-incf count)
                      (setq result-xy (car it))
                      (if (<= min-distance it-len)
                          nil
                        (setq it (cdr it))
                        t)))))
      ;;(message "count=%s" count)
      result-xy)))
;; TEST: (let ((points '((10 . 20) (12 . 21) (18 . 20) (20 . 20.5)))) (edraw-xy-points-straight-last (car points) (cdr points) 5.0 1.0)) => (18 . 20)

(defun edraw-xy-points-to-curves (points min-angle
                                         min-distance
                                         &optional perp-dist-tolerance)
  "Group the list of POINTS by curve. In other words, split at the corners.

Returns a list of lists of points.

Divide a polyline consisting of POINTS at parts that can be
recognized as \"corners\".

MIN-ANGLE and MIN-DISTANCE are used to determine \"corner\".

A point is recognized as a corner if it is more than MIN-DISTANCE
away from the points before and after it, and the angle between
the two straight lines formed between the points before and after
it is more than MIN-ANGLE."
  (when points
    (let ((min-angle-rad (degrees-to-radians min-angle))
          (min-distance-sq (* min-distance min-distance))
          curves
          curr-curve
          (it points))
      ;; First point
      (push (car it) curr-curve)
      (setq it (cdr it))

      ;; Middle points
      (while (cdr it)
        (let* ((p0 (or (edraw-xy-points-straight-last (car it)
                                                      curr-curve
                                                      min-distance
                                                      perp-dist-tolerance)
                       (car curr-curve))) ;; Previous
               (p1 (car it)) ;; Current
               (p2 (or (edraw-xy-points-straight-last (car it)
                                                      (cdr it)
                                                      min-distance
                                                      perp-dist-tolerance)
                       (cadr it))) ;; Next
               (v01 (edraw-xy-sub p1 p0))
               (v12 (edraw-xy-sub p2 p1)))
          ;;(message "%s prev-len=%s next-len=%s angle=%s" p1 (edraw-xy-length v01) (edraw-xy-length v12) (radians-to-degrees (edraw-xy-angle v01 v12)))
          (when (and
                 (or (>= (edraw-xy-length-squared v01) min-distance-sq)
                     ;; If p0 is first point, accept even if short
                     (eq p0 (car points)))
                 (or (>= (edraw-xy-length-squared v12) min-distance-sq)
                     ;; If p2 is last point, accept even if short
                     (null (cddr it)))
                 (>= (abs (edraw-xy-angle v01 v12)) min-angle-rad))
            ;;(message "It's corner")
            (push (car it) curr-curve)
            (push (nreverse curr-curve) curves)
            (setq curr-curve nil))
          (push (car it) curr-curve)
          (setq it (cdr it))))

      ;; Last point
      (when it
        (push (car it) curr-curve))
      (push (nreverse curr-curve) curves)
      (nreverse curves))))
;; TEST: (edraw-xy-points-to-curves nil 45 5) => nil
;; TEST: (edraw-xy-points-to-curves '((10 . 20)) 45 5) => (((10 . 20)))
;; TEST: (edraw-xy-points-to-curves '((10 . 20) (100 . 30)) 45 5) => (((10 . 20) (100 . 30)))
;; TEST: (edraw-xy-points-to-curves '((10 . 20) (100 . 30) (200 . 40)) 45 5) => (((10 . 20) (100 . 30) (200 . 40)))
;; TEST: (edraw-xy-points-to-curves '((10 . 20) (100 . 30) (100 . 100)) 45 5) => (((10 . 20) (100 . 30)) ((100 . 30) (100 . 100)))


(defun edraw-xy-points-divide-long-sections (points max-dist)
  "Insert points so that each segment of the polyline represented
 by POINTS is no longer than MAX-DIST.
This function is destructive: the list POINTS is modified."
  (setq max-dist (float max-dist))
  (let ((it points))
    (while (cdr it)
      (let ((dist (edraw-xy-distance (car it) (cadr it))))
        (when (< max-dist dist)
          (let* ((ndiv (ceiling (/ dist max-dist)))
                 (p0 (car it))
                 (p1 (cadr it))
                 (dx (- (edraw-x p1) (edraw-x p0)))
                 (dy (- (edraw-y p1) (edraw-y p0))))
            (cl-loop for i from 1 to (1- ndiv)
                     for new-xy = (edraw-xy
                                   (+ (edraw-x p0) (/ (* dx i) (float ndiv)))
                                   (+ (edraw-y p0) (/ (* dy i) (float ndiv))))
                     for new-cell = (cons new-xy (cdr it))
                     do
                     (setcdr it new-cell)
                     (setq it new-cell)))))
      (setq it (cdr it))))
  points)
;; TEST: (edraw-xy-points-divide-long-sections (list '(1 . 2) '(10 . 2) '(100 . 2) '(200 . 2) '(250 . 2)) 40) => ((1 . 2) (10 . 2) (40.0 . 2.0) (70.0 . 2.0) (100 . 2) (133.33333333333334 . 2.0) (166.66666666666669 . 2.0) (200 . 2) (225.0 . 2.0) (250 . 2))



;;;;; Tool - Custom Shape Tool

(defclass edraw-editor-tool-custom-shape (edraw-editor-tool)
  ((on-picker-notify :initform nil)
   (picker-buffer :initform nil)
   (selected-shape-descriptor-list :initform nil)
   (selected-picker-entry-properties :initform nil)))

(cl-defmethod edraw-name ((_class (subclass edraw-editor-tool-custom-shape)))
  (edraw-msg "Custom Shape Tool"))

(cl-defmethod edraw-icon ((_class (subclass edraw-editor-tool-custom-shape)))
  (edraw-svg-group
   (edraw-svg-path
    "M14,4C16,2 18,0 20,2C22,4 18,6 20,8C22,10 26,8 26,12C26,14 22,12 20,14C18,16 24,20 20,22C16,24 16,16 12,16C8,16 4,20 4,16C4,14 6,12 8,10C10,8 4,6 6,4C8,2 12,6 14,4Z"
    :stroke-width 1 :stroke "#ccc" :fill "url(#icon-fg-gradient)")))

(cl-defmethod edraw-shape-type-to-create ((_class (subclass edraw-editor-tool-custom-shape)))
  ;;@todo Default values should be per tool, not per type
  'path) ;;Although not only path, default properties can be set only for path

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-custom-shape))
  ;;@todo Default values should be per tool, not per type
  'path) ;;Although not only path, default properties can be set only for path

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool-custom-shape))
  (message (edraw-msg "[Custom Shape Tool] Click:Add shape(original size), Drag:Add shape(specified size), S-Drag:Square")))

(cl-defmethod edraw-on-selected ((tool edraw-editor-tool-custom-shape)
                                 (_editor edraw-editor))
  (prog1 (cl-call-next-method)
    (edraw-connect-to-shape-picker tool)))

(cl-defmethod edraw-on-deselected ((tool edraw-editor-tool-custom-shape))
  (edraw-disconnect-from-shape-picker tool)
  (cl-call-next-method))

(cl-defmethod edraw-connect-to-shape-picker ((tool edraw-editor-tool-custom-shape))
  (let ((editor-buffer (current-buffer))) ;;@todo get from overlay?
    (with-slots (on-picker-notify picker-buffer editor) tool
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
        (setq picker-buffer (edraw-shape-picker-open (oref editor ui-state))) ;; Open common buffer
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
              (default-props (edraw-get-default-shape-properties editor 'path)))
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


;;;;; Tool - Generator Tool

(defclass edraw-editor-tool-generator (edraw-editor-tool)
  ()
  )

(cl-defmethod edraw-name ((_class (subclass edraw-editor-tool-generator)))
  (edraw-msg "Generator Tool"))

(cl-defmethod edraw-icon ((_class (subclass edraw-editor-tool-generator)))
  (edraw-svg-group
   (edraw-svg-path
    "M9,8.5C9.5,7.5 11,6 11.5,6C12,6 12.5,7 13,9L13.875,12.5C13.875,12.5 10.5,17.5 10,17.5C9.5,17.5 9.5,16.5 8.5,16.5C7.5,16.5 7,17.5 7,18.25C7,19.25 7.5,20 8.5,20C9.5,20 9.75,19.75 11,18C13.5,14.5 14.125,13.5 14.125,13.5L15,17C15.25,18 16,20 17,20C19,20 20.5,18 21.5,16.5C21.5,16.5 21,16 21,16C21,16 19,19 18,19C17,19 16.75,17 16.5,16C16.25,15 15.5,12 15.5,12C15.5,12 19.5,6.5 20,6.5C20.5,6.5 20.5,7.5 21.5,7.5C22.5,7.5 23,6.75 23,5.75C23,5 22.75,4 21.5,4C21,4 20.75,4.25 20,5C19,6 15.25,11 15.25,11C15.25,11 15,10 14.5,8C14,6 13,4 12,4C11,4 9,6.5 8,8C8,8 9,8.5 9,8.5Z" :stroke-width 1 :stroke "#ccc" :fill "url(#icon-fg-gradient)")))

(cl-defmethod edraw-shape-type-to-create ((_class (subclass edraw-editor-tool-generator)))
  'edraw-generator)

(cl-defmethod edraw-shape-type-to-create ((_tool edraw-editor-tool-generator))
  'edraw-generator)

(cl-defmethod edraw-print-help ((_tool edraw-editor-tool-generator))
  (message (edraw-msg "[Generator Tool] Click:Add generator shape")))

(cl-defmethod edraw-on-down-mouse-1 ((tool edraw-editor-tool-generator)
                                     down-event)
  (with-slots (editor) tool
    (edraw-put-generator-shape tool
                               (edraw-read-rectangle editor down-event t))))

(cl-defmethod edraw-put-generator-shape ((tool edraw-editor-tool-generator)
                                         rect)
  (with-slots (editor) tool
    (let* ((gen-type
            (x-popup-menu
             t
             (list
              (edraw-msg "Select generator type")
              (cons ""
                    (cl-loop for type in (edraw-shape-generator-types)
                             collect (cons type type))))))
           (generator-input-props
            (edraw-shape-generator-interactive gen-type editor rect 'no-func))
           (no-interactive-func-p (eq generator-input-props 'no-func))
           (generator-default-props
            (edraw-shape-generator-package-defaults gen-type)))

      ;; Default interactive input
      (when no-interactive-func-p
        (setq generator-input-props
              `((gen-source . ,(read-string
                                (format (edraw-msg "%s Source Code: ")
                                        (capitalize gen-type))))
                (transform . ,(edraw-svg-transform-from-matrix
                               (edraw-matrix-translate-xy
                                (edraw-rect-lt rect)))))))

      (when gen-type
        (edraw-deselect-all-shapes editor)
        (edraw-make-undo-group editor 'generator-tool-create
          (let ((shape (edraw-create-shape-without-default ;;notify modification
                        editor
                        (edraw-svg-body editor)
                        nil
                        'edraw-generator
                        (edraw-alist-append
                         `((gen-type . ,gen-type))
                         generator-input-props
                         (edraw-get-default-shape-properties editor
                                                             'edraw-generator)
                         generator-default-props))))
            ;; Update
            (edraw-regenerate shape)

            ;; Fit to RECT
            (when (and no-interactive-func-p
                       (not (edraw-rect-empty-p rect)))
              (let ((aabb (edraw-shape-aabb shape)))
                (edraw-transform-prop-multiply
                 shape
                 (edraw-matrix-fit-rect-to-rect aabb rect))))

            (edraw-select-shape editor shape)))))))


;;;;; Tool - After Class Definition

(defcustom edraw-editor-default-tool 'edraw-editor-tool-select
  "The first tool selected when opening the editor."
  :type `(choice ,@(mapcar (lambda (tool-class)
                             (list 'const
                                   ;; At this point, all tool classes
                                   ;; must be defined.
                                   :tag (edraw-name tool-class)
                                   tool-class))
                           edraw-editor-tool-list))
  :group 'edraw-editor)


;;;; Properties Holder

(cl-defmethod edraw-undo-to-data ((holder edraw-properties-holder) data)
  (while (not (let ((last-data (edraw-last-undo-data holder)))
                (or (null last-data)
                    (eq last-data data))))
    (edraw-undo holder)))

(cl-defmethod edraw-edit-properties ((holder edraw-properties-holder))
  (when-let ((editor (edraw-get-editor holder)))
    (edraw-editor-open-property-editor editor holder)))

(cl-defmethod edraw-edit-property-paint ((holder edraw-properties-holder)
                                         prop-name)
  (when-let ((editor (edraw-get-editor holder)))
    (let* ((old-value (edraw-get-property holder prop-name))
           (new-value
            ;; undo all changes in preview in case edraw-set-property
            ;; cannot revert property values to old-value.
            ;; edraw-multiple-shapes may not be reverted by
            ;; edraw-set-property (if multiple shapes have different
            ;; values).
            (edraw-editor-with-temp-modifications editor
              (unwind-protect
                  (edraw-color-picker-read-color
                   (format "%s: " prop-name)
                   (or old-value "")
                   '("" "none")
                   `((:color-name-scheme . web)
                     (:no-color . "none")
                     (:on-input-change
                      . ,(lambda (string color)
                           (when (or (member string '("" "none"))
                                     color)
                             ;;@todo suppress modified flag change and notification
                             (edraw-undo-all editor)
                             (edraw-set-property holder prop-name string))))
                     (:scale-direct . ,(oref editor image-scale))
                     (:recent-colors . ,(edraw-editor-recent-colors
                                         :ui-state (oref editor ui-state)))))
                ;; Restore value directly for HOLDER that don't support UNDO.
                ;; edraw-property-proxy-shape does not support UNDO.
                (edraw-undo-all editor)
                (edraw-set-property holder prop-name old-value)))))
      (when (string-empty-p new-value)
        (setq new-value nil))
      (when (not (equal new-value old-value))
        (edraw-set-property holder prop-name new-value)))))

(cl-defmethod edraw-edit-fill ((holder edraw-properties-holder))
  (edraw-edit-property-paint holder 'fill))

(cl-defmethod edraw-edit-stroke ((holder edraw-properties-holder))
  (edraw-edit-property-paint holder 'stroke))

(cl-defmethod edraw-set-marker-start-none ((holder edraw-properties-holder))
  (edraw-set-marker holder 'marker-start nil))
(cl-defmethod edraw-set-marker-start-arrow ((holder edraw-properties-holder))
  (edraw-set-marker holder 'marker-start "arrow"))
(cl-defmethod edraw-set-marker-start-circle ((holder edraw-properties-holder))
  (edraw-set-marker holder 'marker-start "circle"))
(cl-defmethod edraw-set-marker-end-none ((holder edraw-properties-holder))
  (edraw-set-marker holder 'marker-end nil))
(cl-defmethod edraw-set-marker-end-arrow ((holder edraw-properties-holder))
  (edraw-set-marker holder 'marker-end "arrow"))
(cl-defmethod edraw-set-marker-end-circle ((holder edraw-properties-holder))
  (edraw-set-marker holder 'marker-end "circle"))
(cl-defmethod edraw-set-marker ((holder edraw-properties-holder) prop-name marker-type)
  (edraw-set-property
   holder prop-name
   (edraw-get-default-marker-properties (edraw-get-editor holder) marker-type)))

(cl-defmethod edraw-set-marker-next ((holder edraw-properties-holder) prop-name)
  (edraw-set-property holder prop-name
                      (edraw-svg-marker-type-next
                       (edraw-svg-marker-type
                        (edraw-get-property holder prop-name)))))
(cl-defmethod edraw-set-marker-start-next ((holder edraw-properties-holder))
  (edraw-set-marker-next holder 'marker-start))
(cl-defmethod edraw-set-marker-end-next ((holder edraw-properties-holder))
  (edraw-set-marker-next holder 'marker-end))

(cl-defmethod edraw-has-property-p ((holder edraw-properties-holder) prop-name)
  "Return t if HOLDER holds the value of the property named PROP-NAME.

Return nil if no value is specified."
  (not (null (edraw-get-property holder prop-name))))

(cl-defmethod edraw-get-transform-method ((holder edraw-properties-holder))
  (when-let ((editor (edraw-get-editor holder)))
    (edraw-get-transform-method editor)))

;;;; Shape

;;;;; Shape Object Types & Creation

(defvar edraw-shape-type-alist nil
  ;; key : type-id(tag or classname)
  ;; value : alist
  ;; '((rect
  ;;    (:class . edraw-shape-rect))
  ;;   ...)
  "Alist of shape object types.

Use `edraw-shape-type-set' to change this.")

(defun edraw-shape-type-set (type shape-class)
  "Register a shape object type.

TYPE is a short symbol to identify the type of shape object.

By convention, if the shape object type corresponds to an SVG
element type, TYPE is the tag name, but TYPE is not necessarily
the tag name. Use `edraw-shape-svg-tag' to get the tag name.

SHAPE-CLASS is a class name symbol that inherits the `edraw-shape' class."
  (setf (alist-get type edraw-shape-type-alist)
        (list (cons :class shape-class))))

(defun edraw-shape-type-ids ()
  "Return all symbols representing the types of shapes registered in edraw.

A symbol may or may not be the same as an SVG tag name or class name."
  (mapcar #'car edraw-shape-type-alist))

(defun edraw-shape-type-valid-p (shape-type)
  "Return t if SHAPE-TYPE is valid.

Only shape types registered in `edraw-shape-type-alist' are valid."
  (when (and (symbolp shape-type)
             (assq shape-type edraw-shape-type-alist))
    t))

(defun edraw-shape-type-to-class (shape-type)
  "Return shape class name symbol from shape SHAPE-TYPE symbol."
  (when (symbolp shape-type)
    (alist-get :class (alist-get shape-type edraw-shape-type-alist))))

(defun edraw-shape-type-to-element (shape-type props-alist deftbl)
  "Create a new SVG element of SHAPE-TYPE."
  (let ((shape-class (edraw-shape-type-to-class shape-type)))
    (unless shape-class
      (error "Unsupported shape type %s" shape-type))
    (edraw-shape-create-element shape-class props-alist deftbl)))

(defun edraw-shape-type-from-element (element)
  "Return a symbol that represents the shape type of the SVG ELEMENT."
  (let ((shape-type
         (or
          ;;<?? data-edraw-type="type" ...>
          (when-let ((value (dom-attr element 'data-edraw-type)))
            (cond
             ((symbolp value) value)
             ((stringp value) (intern value))))
          ;;<type ...>
          (dom-tag element))))
    (when (edraw-shape-type-valid-p shape-type)
      ;; For safety, only return if the shape-type is a registered shape type.
      shape-type)))

(defun edraw-shape-type-create-shape-from-element (element editor)
  "Create a new shape object in EDITOR from ELEMENT."
  (when-let* ((shape-type (edraw-shape-type-from-element element))
              (shape-class (edraw-shape-type-to-class shape-type))
              (shape (make-instance shape-class
                                    :element element
                                    :editor editor)))
    ;; Attach the created SHAPE object to the ELEMENT.
    ;; :-edraw-shape is an attribute for internal use.
    ;; (See: `edraw-dom-attr-internal-p')
    (dom-set-attribute element :-edraw-shape shape)
    shape))


(defun edraw-shape-from-element-no-create (element)
  "Return the shape object already attached to the ELEMENT."
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
     (edraw-shape-type-create-shape-from-element element editor)
     ;; Error
     (if noerror-node-type
         nil
       (error "Unsupported SVG element %s"
              (edraw-svg-to-string element nil nil))))))


;;
;;

(defun edraw-create-shape-default (editor parent shape-type &rest props)
  "Create a new shape.

The shape type is specified by SHAPE-TYPE, and its initial
properties are specified by the property list PROPS. Properties
that are not specified will use the default value selected by the
EDITOR.

The newly created shape will be the last child of PARENT.

This function may add undo data and predefined elements (such as
markers) to the EDITOR."
  (edraw-create-shape-without-default
   editor parent nil
   shape-type
   ;; Complete property values with default values
   (edraw-alist-append
    (edraw-plist-to-alist props)
    ;; NOTE: Default properties depend on the selected tool.
    ;; Therefore, edraw-create-shape-default should not be used outside of
    ;; editor tools.
    (edraw-get-default-shape-properties editor shape-type))))

(defun edraw-create-shape-without-default (editor
                                           parent index shape-type props-alist)
  "Create a new shape.

The shape type is specified by SHAPE-TYPE and its initial
properties are specified by PROPS-ALIST. Properties that are not
specified use values determined by the shape type and are not
affected by the EDITOR state.

The newly created shape becomes the INDEXth child of PARENT. If
INDEX is nil, it will be the last child.

This function may add undo data and predefined elements (such as
markers) to the EDITOR."
  ;; When creating a new shape object, first create a DOM element, then
  ;; create a shape object for that element. This is the same behavior
  ;; when loading an SVG and is easier to maintain consistency.
  (edraw-create-shape-from-inserted-new-element
   editor
   (edraw-create-shape-svg-element shape-type props-alist
                                   (oref editor deftbl)
                                   parent index)))

(defun edraw-create-shape-from-inserted-new-element (editor element)
  (let ((shape (edraw-shape-from-element element editor)))
    (edraw-make-undo-group editor 'shape-create
      (edraw-push-undo editor 'shape-create (list 'edraw-remove shape))
      (edraw-on-shape-changed shape 'shape-create))
    shape))

(defun edraw-create-shape-svg-element (shape-type props-alist deftbl
                                                  parent index)
  "Create an SVG element of the shape specified by SHAPE-TYPE."
  ;; First, create an SVG element determined for each SHAPE-TYPE. In
  ;; the future, we may construct more complex elements (for example,
  ;; a g element with several child elements).
  (let ((element (edraw-shape-type-to-element shape-type props-alist deftbl)))
    ;; Then add the element to the PARENT.
    (when parent
      (if index
          (edraw-dom-insert-nth parent element index)
        (edraw-dom-append-child parent element)))
    element))

(cl-defmethod edraw-shape-create-element ((shape-class (subclass edraw-shape))
                                          props-alist deftbl)
  "Create a DOM element for the SHAPE-CLASS.

PROPS-ALIST is an alist of initial properties. These are not
necessarily the same as SVG attributes. The attributes are set
via the `edraw-svg-element-set-property' function.

DEFTBL is a table for collecting common definitions, and is
passed to `edraw-svg-element-set-property'."
  (let* ((tag (edraw-shape-svg-tag shape-class))
         (prop-info-list (edraw-get-property-info-list shape-class)))
    (let ((element (edraw-dom-element tag)))
      ;; Initialize attributes in the manner of the edraw-dom-svg.el library.
      (dolist (prop props-alist)
        (let ((prop-name (car prop))
              (value (cdr prop)))
          (edraw-svg-element-set-property element prop-name value deftbl
                                          prop-info-list)))
      (let ((shape-type (edraw-shape-type shape-class)))
        (unless (eq tag shape-type)
          (dom-set-attribute element 'data-edraw-type
                             (format "%s" shape-type))))

      element)))

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

(defclass edraw-shape (edraw-properties-holder)
  ((element :initarg :element)
   (editor :initarg :editor
           :type edraw-editor)
   (change-hook :initform (edraw-hook-make))
   (removed-p :initform nil)
   (point-connections :initform nil)
   (point-connection-referrers :initform nil)
   (temporary-state-alist :initform nil))
  :abstract t)

(defun edraw-shape-derived-p (obj)
  (and obj (cl-typep obj 'edraw-shape)))

(cl-defmethod edraw-property-editor-shape-p
  ((_target edraw-shape))
  t) ;;see: edraw-property-editor.el

(cl-defgeneric edraw-shape-type (shape) ;; Must be implement in derived classes
  "Return a symbol representing the type of SHAPE.")

(cl-defgeneric edraw-shape-svg-tag (class) ;; Must be implement in derived classes
  "Return the SVG tag generated by CLASS as a symbol.

Return nil if undefined.")

(cl-defgeneric edraw-shape-type-name (shape)
  "Return the name used to explain the SHAPE type to the user.

e.g. rect, ellipse, path, image, text, group, generator."
  (let ((class-name (symbol-name (eieio-object-class-name shape))))
    (if (string-match "\\`edraw-shape-\\(.+\\)\\'" class-name)
        (match-string 1 class-name)
      class-name)))

(cl-defmethod edraw-get-editor ((shape edraw-shape))
  (oref shape editor))

;;;;;; Internal

(cl-defmethod edraw-element ((shape edraw-shape))
  "Used only internally."
  (oref shape element))

(cl-defmethod edraw-parent-element ((shape edraw-shape))
  "Used only internally."
  (with-slots (editor) shape
    (edraw-dom-parent
     (edraw-svg-body editor)
     (edraw-element shape))))

;;;;;; SVG

(cl-defgeneric edraw-svg-string (obj)
  "Make SVG string from OBJ.")
(cl-defmethod edraw-svg-string ((shape edraw-shape))
  (edraw-svg-to-string (edraw-element shape) nil nil 0 t))

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
                   (let* ((prop-name (edraw-svg-prop-info-name prop-info))
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
    (when-let ((path-data
                (edraw-svg-element-to-path-data (edraw-element shape)))
               (shape-descriptor
                (edraw-shape-descriptor shape)))
      (edraw-shape-descriptor-set-type shape-descriptor 'path)
      (edraw-shape-descriptor-put-property shape-descriptor 'd
                                           (edraw-path-data-to-string
                                            path-data))
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
            ;;@todo This becomes a problem when adding complex
            ;;shape-types. Do not use edraw-svg-element-get-property
            ;;directly. You need a way to get the class from
            ;;shape-type and use a static method to get the
            ;;properties.
            (mapcar
             (lambda (prop-info)
               (let* ((prop-name (edraw-svg-prop-info-name prop-info))
                      (prop-value (edraw-svg-element-get-property
                                   element prop-name nil))) ;;deftbl=nil
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
  (when-let ((container (edraw-svg-decode-xml
                         (concat "<g>" svg-text "</g>") nil)))
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
  (let* ((shape-type (alist-get :type shape-descriptor))
         (props (alist-get :properties shape-descriptor))
         (children-descriptor (alist-get :children shape-descriptor))
         ;;@todo deftbl??? What happens when use marker attributes?
         (deftbl (edraw-svg-deftbl (edraw-dom-element 'defs))) ;;Dummy deftbl
         (element
          (edraw-create-shape-svg-element
           shape-type
           props
           deftbl
           nil ;;parent
           nil ;;index
           )))
    (when children-descriptor
      (dolist (child-descriptor children-descriptor)
        (edraw-dom-append-child
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

(cl-defmethod edraw-on-shape-changed ((shape edraw-shape) type &optional hint)
  ;;(message "on shape changed %s %s" (edraw-name shape) type)
  (with-slots (editor) shape
    (edraw-update-related-point-connections shape type)
    (edraw-notify-parent-of-change shape type)
    (edraw-notify-change-hook shape type hint)))

(cl-defmethod edraw-notify-parent-of-change ((shape edraw-shape) type)
  (if-let ((parent (edraw-parent shape)))
      (edraw-on-child-changed parent shape type)
    (edraw-on-document-changed (oref shape editor) 'shape)))

(cl-defmethod edraw-on-child-changed ((shape edraw-shape)
                                      (_child edraw-shape) _type)
  (edraw-on-shape-changed shape 'child-change))

(cl-defmethod edraw-notify-change-hook ((shape edraw-shape) type &optional hint
                                        &rest _unknown)
  (with-slots (editor change-hook) shape
    (edraw-hook-call change-hook shape type hint)))

(cl-defmethod edraw-add-change-hook ((shape edraw-shape) function &rest args)
  (with-slots (change-hook) shape
    (apply 'edraw-hook-add change-hook function args)))

(cl-defmethod edraw-remove-change-hook ((shape edraw-shape) function &rest args)
  (with-slots (change-hook) shape
    (apply 'edraw-hook-remove change-hook function args)))

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
        (edraw-dom-remove-node (edraw-parent-element shape) element)
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
          (edraw-dom-remove-node parent element)
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

(cl-defmethod edraw-pick-anchor-point ((shape edraw-shape) xy
                                       &optional pick-forced)
  (edraw-shape-point-find
   (edraw-get-anchor-points shape)
   xy
   pick-forced))

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

(cl-defmethod edraw-undo-block-begin ((shape edraw-shape))
  (edraw-undo-block-begin (oref shape editor)))

(cl-defmethod edraw-undo-block-end ((shape edraw-shape) backup)
  (edraw-undo-block-end (oref shape editor) backup))

(cl-defmethod edraw-undo-all ((shape edraw-shape))
  (edraw-undo-all (oref shape editor)))

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

(cl-defmethod edraw-get-deftbl ((shape edraw-shape))
  (oref (oref shape editor) deftbl))

(cl-defmethod edraw-get-summary ((shape edraw-shape))
  (edraw-svg-element-summary (edraw-element shape)))

(cl-defmethod edraw-get-property-info-list ((class (subclass edraw-shape)))
  (when-let ((tag (edraw-shape-svg-tag class)))
    (edraw-svg-tag-get-property-info-list tag)))

(cl-defmethod edraw-get-property-info-list ((shape edraw-shape))
  (edraw-svg-element-get-property-info-list (edraw-element shape)))

(cl-defmethod edraw-get-property ((shape edraw-shape) prop-name)
  (edraw-svg-element-get-property (edraw-element shape) prop-name
                                  (edraw-get-deftbl shape)
                                  (edraw-get-property-info-list shape)))

(cl-defmethod edraw-get-property-as-length ((shape edraw-shape) prop-name
                                            &optional default-value)
  ;;@todo Use edraw-svg-prop-to-number with property-info ?
  (or (edraw-svg-attr-length-to-number (edraw-get-property shape prop-name)
                                       (edraw-element shape)
                                       prop-name)
      (or default-value 0)))

(cl-defmethod edraw-set-properties ((shape edraw-shape) prop-list)
  "Returns t if the property is actually changed."
  (edraw-set-properties-internal shape prop-list nil))

(cl-defmethod edraw-set-properties-internal ((shape edraw-shape) prop-list
                                             old-prop-list)
  "Returns t if the property is actually changed."
  (let ((deftbl (edraw-get-deftbl shape))
        (prop-info-list (edraw-get-property-info-list shape)))
    (with-slots (element) shape
      (dolist (prop prop-list)
        (let* ((prop-name (car prop))
               (new-value (cdr prop))
               (old-value (edraw-svg-element-get-property element prop-name
                                                          deftbl
                                                          prop-info-list)))
          (unless (equal new-value old-value)
            ;;(message "%s: %s to %s" prop-name old-value new-value)
            (push (cons prop-name old-value) old-prop-list)
            (edraw-svg-element-set-property element prop-name new-value
                                            deftbl
                                            prop-info-list)))))
    (when old-prop-list
      (let ((editor (oref shape editor)))
        (edraw-make-undo-group editor 'shape-properties
          (edraw-push-undo
           editor
           'shape-properties
           (list #'edraw-set-properties shape old-prop-list))
          (edraw-on-shape-properties-changed shape old-prop-list)
          ;; NOTE: Other connected shapes may change.
          (edraw-on-shape-changed shape 'shape-properties old-prop-list)))
      t)))

(cl-defmethod edraw-on-shape-properties-changed ((_shape edraw-shape)
                                                 _old-prop-list)
  )

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

;;;;;; Temporary State

(defconst edraw-shape-temporary-state-default-alist
  '((visible . t)
    (pickable . t)))

(cl-defmethod edraw-clear-temporary-states ((shape edraw-shape))
  (oset shape temporary-state-alist nil)
  (edraw-invalidate-image (edraw-get-editor shape)))

(cl-defmethod edraw-has-temporary-states-p ((shape edraw-shape))
  ;; Find a property not equal to default value
  (seq-some (lambda (state)
              (let* ((state-name (car state))
                     (value (cdr state))
                     (default (alist-get
                               state-name
                               edraw-shape-temporary-state-default-alist)))
                (not (equal value default))))
            (oref shape temporary-state-alist)))

(cl-defmethod edraw-get-temporary-state ((shape edraw-shape) state-name)
  (cdr (or (assq state-name (oref shape temporary-state-alist))
           (assq state-name edraw-shape-temporary-state-default-alist))))

(cl-defmethod edraw-set-temporary-state ((shape edraw-shape)
                                         state-name value)
  (when (and state-name (symbolp state-name))
    (setf (alist-get state-name (oref shape temporary-state-alist))
          value)
    (edraw-invalidate-image (edraw-get-editor shape))))

(cl-defmethod edraw-visible-and-pickable-p ((shape edraw-shape))
  (and (edraw-visible-p shape)
       (edraw-pickable-p shape)))

(cl-defmethod edraw-visible-p ((shape edraw-shape))
  (edraw-get-temporary-state shape 'visible))

(cl-defmethod edraw-pickable-p ((shape edraw-shape))
  (edraw-get-temporary-state shape 'pickable))

(cl-defmethod edraw-set-visible ((shape edraw-shape) value)
  (edraw-set-temporary-state shape 'visible value))

(cl-defmethod edraw-set-pickable ((shape edraw-shape) value)
  (edraw-set-temporary-state shape 'pickable value))

(cl-defmethod edraw-toggle-visibility ((shape edraw-shape))
  (edraw-set-visible shape (not (edraw-visible-p shape))))

(cl-defmethod edraw-toggle-pickability ((shape edraw-shape))
  (edraw-set-pickable shape (not (edraw-pickable-p shape))))

;;;;;; Interactive Command

(cl-defmethod edraw-select ((shape edraw-shape))
  (with-slots (editor) shape
    (unless (let ((current-tool (edraw-selected-tool editor)))
              (and current-tool
                   (eq (edraw-tool-type current-tool) 'select)))
      (edraw-select-tool editor 'edraw-editor-tool-select))
    (edraw-select-shape editor shape)))

(cl-defmethod edraw-deselect ((shape edraw-shape))
  (with-slots (editor) shape
    (edraw-remove-shape-selection editor shape)))

;; I was wondering whether to name the method
;; `edraw-select-next-sibling', but since (`edraw-select-next-shape'
;; editor) already exists, I chose the same name.  Since there is
;; (`edraw-next-sibling' shape), would `edraw-select-next-sibling'
;; have been better?  If `-next-shape' selects descendants of a tree,
;; it needs to be clearly distinguished from `-next-sibling'.
(cl-defmethod edraw-select-next-shape ((shape edraw-shape))
  (when-let ((sibling (edraw-next-sibling shape)))
    (edraw-select sibling)))

(cl-defmethod edraw-select-previous-shape ((shape edraw-shape))
  (when-let ((sibling (edraw-previous-sibling shape)))
    (edraw-select sibling)))

(cl-defmethod edraw-matches-selected-p ((shape edraw-shape))
  "If SHAPE is selected and no other shapes are selected, return t."
  (with-slots (editor) shape
    (let ((selected-shapes (edraw-selected-shapes editor)))
      (and (eq (car selected-shapes) shape)
           (null (cdr selected-shapes))))))

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

(cl-defmethod edraw-popup-context-menu ((shape edraw-shape))
  (edraw-popup-menu nil (edraw-menu-shape shape) shape))

(cl-defmethod edraw-menu-shape ((shape edraw-shape))
  `(,(edraw-get-summary shape)
    ,(edraw-filter-menu-items
      shape 'shape
      (edraw-get-actions shape))))

(cl-defmethod edraw-get-actions ((shape edraw-shape))
  ;; NOTE: This method is not the same as `edraw-menu-items-shape-common'.
  ;; Items may be added in derived classes.
  (edraw-menu-items-shape-common shape))

(cl-defmethod edraw-filter-menu-items ((shape edraw-shape) menu-type items)
  (when-let ((editor (edraw-get-editor shape)))
    (edraw-filter-menu-items editor (list menu-type :target shape) items)))

(cl-defmethod edraw-get-actions--keys (obj command)
  (when (or (null obj)
            (edraw-matches-selected-p obj))
    (when-let ((binding (edraw-where-is-string command nil t)))
      (list :keys binding))))

(defun edraw-menu-items-shape-common--dummy (&rest _)
  "A command that does nothing and has no assigned key.
Alternative to `ignore'."
  (interactive))

(defun edraw-menu-items-shape-common--convert (menu-items shape)
  "Convert the menu item list used in `edraw-menu-items-shape-common' to
 that used in `edraw-popup-menu'."
  (let ((multiple-p (edraw-multiple-shapes-p shape))
        (selected-p (edraw-matches-selected-p shape)) ;;exactly match
        result)
    (dolist (item menu-items)
      (let* ((name (car item))
             (binding (or (cadr item) 'edraw-menu-items-shape-common--dummy))
             (props (cddr item))
             (cmd-for-selected (or (plist-get props :cmd-for-selected)
                                   'edraw-menu-items-shape-common--dummy)))
        (cond
         ;; Separator
         ((and (stringp name)
               (string-prefix-p "--" name)
               (null (cadr item)))
          (push item result))
         ;; Convert submenu recursively
         ((consp binding)
          (push (nconc (list name (edraw-menu-items-shape-common--convert
                                   binding shape))
                       props)
                result))
         ;; For selected multiple shapes
         ((and multiple-p selected-p)
          (push (nconc
                 ;; Use command for selected shapes
                 (list name cmd-for-selected)
                 props)
                result))
         ;; For multiple shapes including unselected
         ;; For a single specified shape
         ((and
           ;; Remove invalid function
           binding
           (not (eq binding
                    'edraw-menu-items-shape-common--dummy)))
          (push (nconc
                 (list name binding)
                 ;; Show key binding if SHAPE is the only selected shape.
                 (when selected-p
                   (edraw-get-actions--keys nil cmd-for-selected))
                 props)
                result)))))
    (nreverse result)))

(cl-defmethod edraw-menu-items-shape-common ((shape edraw-properties-holder))
  "Return a list of common menu items for `edraw-shape' or
`edraw-multiple-shapes'.

SHAPE must be a `edraw-shape' or `edraw-multiple-shapes' object.

If SHAPE is `edraw-multiple-shapes', the shapes it contains must
match all selected shapes in the editor."
  (let* ((shape-p (edraw-shape-derived-p shape))
         (selected-p (and shape-p (edraw-selected-p shape)))
         (multiple-p (edraw-multiple-shapes-p shape))
         (uniform-class (edraw-shape-uniform-class shape)))

    (edraw-menu-items-shape-common--convert
     `(((edraw-msg "Select") edraw-select
        :visible ,(and shape-p (not selected-p)))
       ((edraw-msg "Deselect") edraw-deselect
        :cmd-for-selected edraw-editor-toggle-selection-all
        :visible ,(if shape-p selected-p multiple-p))
       ((edraw-msg "Temporary State")
        (((edraw-msg "Invisible") edraw-toggle-visibility
          :cmd-for-selected edraw-editor-toggle-visibility-selected
          :button (:toggle . ,(not (edraw-visible-p shape))))
         ((edraw-msg "Pointer Input Disabled") edraw-toggle-pickability
          :cmd-for-selected edraw-editor-toggle-pickability-selected
          :button (:toggle . ,(not (edraw-pickable-p shape))))
         ((edraw-msg "Clear All States") edraw-clear-temporary-states
          :cmd-for-selected edraw-editor-clear-temporary-states-selected
          :enable ,(edraw-has-temporary-states-p shape))))
       ((edraw-msg "Properties...") edraw-edit-properties
        :cmd-for-selected edraw-editor-edit-properties-of-selected-shapes)
       ((edraw-msg "Set")
        (((edraw-msg "Fill...") edraw-edit-fill
          :cmd-for-selected edraw-editor-edit-fill-selected
          :visible ,(edraw-can-have-property-p shape 'fill))
         ((edraw-msg "Stroke...") edraw-edit-stroke
          :cmd-for-selected edraw-editor-edit-stroke-selected
          :visible ,(edraw-can-have-property-p shape 'stroke))
         ((edraw-msg "Href...") edraw-edit-href
          :cmd-for-selected edraw-editor-edit-href-selected
          :visible ,(edraw-can-have-property-p shape (edraw-svg-href-symbol)))
         ((edraw-msg "Font Size...") edraw-edit-font-size
          :cmd-for-selected edraw-editor-edit-font-size-selected
          :visible ,(edraw-can-have-property-p shape 'font-size))
         ;; Marker
         ((edraw-msg "Start Marker")
          (((edraw-msg "None") edraw-set-marker-start-none
            :cmd-for-selected edraw-editor-set-marker-start-none-selected
            :button (:toggle . ,(null (edraw-get-property shape 'marker-start))))
           ((edraw-msg "Arrow") edraw-set-marker-start-arrow
            :cmd-for-selected edraw-editor-set-marker-start-arrow-selected
            :button (:toggle . ,(equal (edraw-svg-marker-type
                                        (edraw-get-property shape 'marker-start))
                                       "arrow")))
           ((edraw-msg "Circle") edraw-set-marker-start-circle
            :cmd-for-selected edraw-editor-set-marker-start-circle-selected
            :button (:toggle . ,(equal (edraw-svg-marker-type
                                        (edraw-get-property shape 'marker-start))
                                       "circle")))
           ("--single-line")
           ((edraw-msg "Next Type") edraw-set-marker-start-next
            :cmd-for-selected edraw-editor-set-marker-start-next-selected))
          :visible ,(edraw-can-have-property-p shape 'marker-start))
         ((edraw-msg "End Marker")
          (((edraw-msg "None") edraw-set-marker-end-none
            :cmd-for-selected edraw-editor-set-marker-end-none-selected
            :button (:toggle . ,(null (edraw-get-property shape 'marker-end))))
           ((edraw-msg "Arrow") edraw-set-marker-end-arrow
            :cmd-for-selected edraw-editor-set-marker-end-arrow-selected
            :button (:toggle . ,(equal (edraw-svg-marker-type
                                        (edraw-get-property shape 'marker-end))
                                       "arrow")))
           ((edraw-msg "Circle") edraw-set-marker-end-circle
            :cmd-for-selected edraw-editor-set-marker-end-circle-selected
            :button (:toggle . ,(equal (edraw-svg-marker-type
                                        (edraw-get-property shape 'marker-end))
                                       "circle")))
           ("--single-line")
           ((edraw-msg "Next Type") edraw-set-marker-end-next
            :cmd-for-selected edraw-editor-set-marker-end-next-selected))
          :visible ,(edraw-can-have-property-p shape 'marker-end))))
       ((edraw-msg "Transform")
        (((edraw-msg "Transform...") edraw-transform-interactive
          :cmd-for-selected edraw-editor-transform-selected-interactive)
         ((edraw-msg "Translate...") edraw-translate
          :cmd-for-selected edraw-editor-translate-selected)
         ((edraw-msg "Scale...") edraw-scale
          :cmd-for-selected edraw-editor-scale-selected)
         ((edraw-msg "Rotate...") edraw-rotate
          :cmd-for-selected edraw-editor-rotate-selected)
         ((edraw-msg "Apply transform property to anchors")
          edraw-apply-transform-prop-to-anchor-points
          ;; @todo :cmd-for-selected ?
          :enable ,(and shape-p
                        (edraw-transform-prop-exists-p shape)
                        (not (edraw-matrix-identity-p
                              (edraw-transform-prop-get-matrix shape)))))
         ;; @todo Set for each shape?
         ,(edraw-transform-method-menu shape)))
       ((edraw-msg "Z-Order")
        (((edraw-msg "Bring to Front") edraw-bring-to-front
          :cmd-for-selected edraw-editor-bring-selected-to-front
          :enable ,(not (edraw-front-p shape)))
         ((edraw-msg "Bring Forward") edraw-bring-forward
          :cmd-for-selected edraw-editor-bring-selected-forward
          :enable ,(not (edraw-front-p shape)))
         ((edraw-msg "Send Backward") edraw-send-backward
          :cmd-for-selected edraw-editor-send-selected-backward
          :enable ,(not (edraw-back-p shape)))
         ((edraw-msg "Send to Back") edraw-send-to-back
          :cmd-for-selected edraw-editor-send-selected-to-back
          :enable ,(not (edraw-back-p shape)))))
       ((edraw-msg "Glue")
        (((edraw-msg "Glue to selected or overlapped shape") edraw-glue-to-selected-or-overlapped-shape
          ;; @todo :cmd-for-selected ?
          ;; @todo impl `edraw-get-point-connections' for multiple shapes
          :enable ,(and shape-p
                        (null (edraw-get-point-connections shape))))
         ((edraw-msg "Unglue All") edraw-unglue-all
          ;; @todo :cmd-for-selected ?
          ;; @todo impl `edraw-get-point-connections' for multiple shapes
          :enable ,(and shape-p
                        (not (null (edraw-get-point-connections shape))))))
        :visible ,shape-p)
       ;; Basic Edit
       ((edraw-msg "Delete...") edraw-delete-with-confirm
        :cmd-for-selected edraw-editor-delete-selected)
       ((edraw-msg "Duplicate") edraw-duplicate-and-select
        :cmd-for-selected edraw-editor-duplicate-selected-shapes)
       ((edraw-msg "Copy") edraw-copy
        :cmd-for-selected edraw-editor-copy-selected-shapes)
       ((edraw-msg "Cut") edraw-cut
        :cmd-for-selected edraw-editor-cut-selected-shapes)
       ;; Group
       ((edraw-msg "Group") edraw-group
        :cmd-for-selected edraw-editor-group-selected-shapes)
       ((edraw-msg "Ungroup") edraw-ungroup
        :cmd-for-selected edraw-editor-ungroup-selected-shapes
        :enable ,(and multiple-p
                      (edraw-group-p shape)))
       ;; Navigate
       ((edraw-msg "Select Next Above") edraw-select-next-shape
        :cmd-for-selected edraw-editor-select-next-shape
        :enable ,(not (edraw-front-p shape)))
       ((edraw-msg "Select Next Below") edraw-select-previous-shape
        :cmd-for-selected edraw-editor-select-previous-shape
        :enable ,(not (edraw-back-p shape)))
       ;; Path
       ((edraw-msg "Combine Paths") edraw-combine-paths
        :cmd-for-selected edraw-editor-combine-selected-paths
        :enable ,multiple-p
        :visible ,(eq uniform-class 'edraw-shape-path)))
     shape)))

(cl-defmethod edraw-property-editor-actions ((shape edraw-shape))
  `((push-button :notify ,(lambda (&rest _)
                            (edraw-popup-context-menu shape))
                 ,(edraw-msg "Actions"))))


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
  (let ((dst-shape
         ;; Determine the destination shape
         (edraw-glue-destination-of-selected-or-overlapped-shape shape)))
    (unless dst-shape
      (error (edraw-msg "No glue target")))
    (edraw-glue-to shape dst-shape)))

(cl-defmethod edraw-glue-to ((shape edraw-shape) (dst-shape edraw-shape))
  ;; Determine the destination shape
  (edraw-make-undo-group (oref shape editor) 'glue-shape-to-shape
    (let ((conn (edraw-point-connection
                 :src (edraw-point-connection-src-aabb
                       :shape shape :x-ratio 0.5 :y-ratio 0.5)
                 :dst (edraw-point-connection-dst-shape
                       :shape dst-shape))))
      ;; Update coordinates before adding CONN to SHAPE.
      ;;(edraw-update conn)
      ;; Add CONN to SHAPE.
      (edraw-add-point-connection shape conn t)
      ;; Do not update coordinates after adding CONN.
      ;; When undoing, CONN must be removed before undoing XY move.
      ;;(edraw-update-all-point-connections shape)
      )))

(cl-defmethod edraw-unglue-all ((shape edraw-shape))
  (edraw-make-undo-group (oref shape editor) 'unglue-all
    (edraw-remove-all-point-connections shape)))

(cl-defmethod edraw-point-connection-aabb ((shape edraw-shape))
  "Return AABB for point connection calculations.

Returns a bounding rectangle suitable for layout calculations.

The edraw-shape-text class returns a rectangle that does not
contain the descent of the last line. In the future this function
may be replaced by another mechanism."
  (edraw-shape-aabb shape))

;;;;; Shape - Rect Boundary

(defclass edraw-shape-with-rect-boundary (edraw-shape)
  ((anchor-points :initform nil)
   (p0p1)) ;;Note that p0(car p0p1) is not always in the upper left
  :abstract t)

;;(cl-defmethod edraw-get-rect-local ((shape edraw-shape-with-rect-boundary)))

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

(cl-defmethod edraw-get-anchor-point-by-ref-xy ((shape edraw-shape-with-rect-boundary) ref-x ref-y)
  (seq-find (lambda (spt)
              (and (eq (edraw-ref-x spt) ref-x)
                   (eq (edraw-ref-y spt) ref-y)))
            (oref shape anchor-points)))

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

(defconst edraw-shape-rect-shape-type 'rect)

(edraw-shape-type-set edraw-shape-rect-shape-type 'edraw-shape-rect)

(defclass edraw-shape-rect (edraw-shape-with-rect-boundary)
  ())

(cl-defmethod edraw-shape-type ((_shape edraw-shape-rect))
  edraw-shape-rect-shape-type)

(cl-defmethod edraw-shape-type ((_class (subclass edraw-shape-rect)))
  edraw-shape-rect-shape-type)

(cl-defmethod edraw-shape-svg-tag ((_class (subclass edraw-shape-rect)))
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

(defconst edraw-shape-ellipse-shape-type 'ellipse)

(edraw-shape-type-set edraw-shape-ellipse-shape-type 'edraw-shape-ellipse)

(defclass edraw-shape-ellipse (edraw-shape-with-rect-boundary)
  ())

(cl-defmethod edraw-shape-type ((_shape edraw-shape-ellipse))
  edraw-shape-ellipse-shape-type)

(cl-defmethod edraw-shape-type ((_class (subclass edraw-shape-ellipse)))
  edraw-shape-ellipse-shape-type)

(cl-defmethod edraw-shape-svg-tag ((_class (subclass edraw-shape-ellipse)))
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

(defconst edraw-shape-circle-shape-type 'circle)

(edraw-shape-type-set edraw-shape-circle-shape-type 'edraw-shape-circle)

(defclass edraw-shape-circle (edraw-shape-with-rect-boundary)
  ())

(cl-defmethod edraw-shape-type ((_shape edraw-shape-circle))
  edraw-shape-circle-shape-type)

(cl-defmethod edraw-shape-type ((_class (subclass edraw-shape-circle)))
  edraw-shape-circle-shape-type)

(cl-defmethod edraw-shape-svg-tag ((_class (subclass edraw-shape-circle)))
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

(defconst edraw-shape-text-shape-type 'text)

(edraw-shape-type-set edraw-shape-text-shape-type 'edraw-shape-text)

(defclass edraw-shape-text (edraw-shape)
  ((anchor-points)))

(cl-defmethod initialize-instance :after ((shape edraw-shape-text) &rest _args)
  (oset shape anchor-points (list (edraw-shape-point-text :shape shape))) ;; Check `edraw-get-anchor-point-count' when changing
  )

(cl-defmethod edraw-shape-type ((_shape edraw-shape-text))
  edraw-shape-text-shape-type)

(cl-defmethod edraw-shape-type ((_class (subclass edraw-shape-text)))
  edraw-shape-text-shape-type)

(cl-defmethod edraw-shape-svg-tag ((_class (subclass edraw-shape-text)))
  'text)

(cl-defmethod edraw-get-anchor-points ((shape edraw-shape-text))
  (oref shape anchor-points))

(cl-defmethod edraw-get-anchor-point-count ((_shape edraw-shape-text))
  1) ;; !!!!!

(cl-defmethod edraw-get-anchor-position ((shape edraw-shape-text))
  (with-slots (element) shape
    (cons
     (or (car (edraw-svg-attr-length-list element 'x)) 0)
     (or (car (edraw-svg-attr-length-list element 'y)) 0))))

(cl-defmethod edraw-set-anchor-position ((shape edraw-shape-text) xy)
  "Returns t if the property is actually changed."
  (with-slots (element) shape
    (unless (edraw-xy-equal-p xy (edraw-get-anchor-position shape))
      (edraw-set-properties
       shape
       (list
        (cons 'x (edraw-x xy))
        (cons 'y (edraw-y xy)))))))

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
    (let* ((xy (edraw-get-anchor-position shape))
           (new-xy (edraw-matrix-mul-mat-xy matrix xy)))

      (edraw-set-properties
       shape
       (list
        (cons 'x (edraw-x new-xy))
        (cons 'y (edraw-y new-xy)))))))

(cl-defmethod edraw-edit-font-size ((holder edraw-properties-holder))
  (when (edraw-can-have-property-p holder 'font-size)
    (let ((font-size (edraw-read-number-or-nil
                      (edraw-msg "Font Size: ")
                      (edraw-get-property holder 'font-size))))
      (edraw-set-property holder 'font-size font-size))))

(cl-defmethod edraw-point-connection-aabb ((shape edraw-shape-text))
  ;; Exclude descent of the last line.
  (let ((edraw-svg-text-contents-aabb--remove-last-descent t))
    (edraw-shape-aabb shape)))



;;;;; Shape - Image

(defconst edraw-shape-image-shape-type 'image)

(edraw-shape-type-set edraw-shape-image-shape-type 'edraw-shape-image)

(defclass edraw-shape-image (edraw-shape-with-rect-boundary)
  ())

(cl-defmethod edraw-shape-type ((_shape edraw-shape-image))
  edraw-shape-image-shape-type)

(cl-defmethod edraw-shape-type ((_class (subclass edraw-shape-image)))
  edraw-shape-image-shape-type)

(cl-defmethod edraw-shape-svg-tag ((_class (subclass edraw-shape-image)))
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

(cl-defmethod edraw-edit-href ((holder edraw-properties-holder))
  (when (edraw-can-have-property-p holder (edraw-svg-href-symbol))
    (let ((filename (read-file-name (edraw-msg "Image File: ")
                                    nil
                                    (edraw-get-property holder
                                                        (edraw-svg-href-symbol))
                                    t)))
      (edraw-set-property holder (edraw-svg-href-symbol)
                          (file-relative-name filename)))))


;;;;; Shape - Path

(defconst edraw-shape-path-shape-type 'path)

(edraw-shape-type-set edraw-shape-path-shape-type 'edraw-shape-path)

(defclass edraw-shape-path (edraw-shape)
  ((path-data)))

(cl-defmethod initialize-instance :after ((shape edraw-shape-path) &rest
                                          _args)
  (let ((d (dom-attr (oref shape element) 'd)))
    (oset shape path-data (or (and d
                                   (edraw-path-data-from-d d))
                              (edraw-path-data)))))

(cl-defmethod edraw-shape-type ((_shape edraw-shape-path))
  edraw-shape-path-shape-type)

(cl-defmethod edraw-shape-type ((_class (subclass edraw-shape-path)))
  edraw-shape-path-shape-type)

(cl-defmethod edraw-shape-svg-tag ((_class (subclass edraw-shape-path)))
  'path)

(cl-defmethod edraw-get-property ((shape edraw-shape-path) prop-name)
  (if (eq prop-name 'd)
      (edraw-path-data-to-string (oref shape path-data))
    (cl-call-next-method)))

(cl-defmethod edraw-set-properties ((shape edraw-shape-path) prop-list)
  (let (old-prop-list)
    (when-let ((d-cell (assq 'd prop-list)))
      (with-slots (path-data) shape
        (let ((new-d (or (cdr d-cell) ""))
              (old-d-attr (dom-attr (edraw-element shape) 'd))
              (old-d-prop (edraw-path-data-to-string path-data)))
          (unless (or (string= new-d old-d-attr)
                      (string= new-d old-d-prop))
            ;;(message "Change path data %s => %s" old-d-prop new-d)
            ;; Update path-data
            (edraw-path-data-subpath-swap path-data
                                          (edraw-path-data-from-d new-d))
            ;; Update d= attribute
            (edraw-update-path-d-attr shape)
            (push (cons 'd old-d-attr) old-prop-list))))
      (setf (alist-get 'd prop-list nil 'remove) nil))
    ;; other properties
    (edraw-set-properties-internal shape prop-list old-prop-list)))

(cl-defmethod edraw-get-actions ((shape edraw-shape-path))
  (let* ((items (copy-tree (cl-call-next-method)))
         (multi-subpaths-p (edraw-contains-multiple-subpaths-p shape)))
    (append
     items
     `(((edraw-msg "Close Path") edraw-close-path-shape
        :enable ,(and (not multi-subpaths-p)
                      (edraw-open-path-shape-p shape)))
       ((edraw-msg "Open Path") edraw-open-path-shape
        :enable ,(and (not multi-subpaths-p)
                      (edraw-closed-path-shape-p shape)))
       ((edraw-msg "Split Subpaths") edraw-split-subpaths
        :enable ,multi-subpaths-p)
       ((edraw-msg "Reverse Path Direction") edraw-reverse-path)
       ((edraw-msg "Make Smooth") edraw-make-smooth
        ;;@todo Add more conditions such as contains 3 or more anchor points
        ;;:enable
        )))))


(cl-defmethod edraw-transform-auto ((shape edraw-shape-path) matrix)
  (cond
   ;; `transform' property already used.
   ((edraw-transform-prop-exists-p shape)
    (edraw-transform-prop-multiply shape matrix))
   ;; Move anchor points.
   (t
    (edraw-transform-anchor-points-local shape matrix))))

(cl-defmethod edraw-transform-anchor-points-local ((shape edraw-shape-path)
                                                   matrix)
  (edraw-path-data-transform (oref shape path-data) matrix)
  (edraw-make-undo-group (oref shape editor) 'shape-path-transform
    (edraw-push-undo-properties shape 'shape-path-transform '(d))
    (edraw-update-path-d-attr shape)
    (edraw-on-shape-changed shape 'shape-transform)))

(cl-defmethod edraw-get-anchor-points ((shape edraw-shape-path))
  (let (points)
    (edraw-path-data-subpath-loop (oref shape path-data) subpath
      (edraw-path-subpath-anchor-loop subpath anchor
        (push (edraw-shape-point-path-anchor :shape shape :anchor anchor)
              points)))
    (nreverse points)))

(cl-defmethod edraw-get-anchor-point-count ((shape edraw-shape-path))
  (edraw-path-data-anchor-count (oref shape path-data)))

(defun edraw-shape-path-make-point-object (shape anchor-or-handle)
  (when anchor-or-handle
    (cond
     ((edraw-path-anchor-p anchor-or-handle)
      (edraw-shape-point-path-anchor :shape shape :anchor anchor-or-handle))
     ((edraw-path-handle-p anchor-or-handle)
      (edraw-shape-point-path-handle :shape shape :handle anchor-or-handle)))))

(defun edraw-shape-path-make-anchor-object (shape anchor)
  (when anchor
    (edraw-shape-point-path-anchor :shape shape :anchor anchor)))

(defun edraw-shape-path-point-index (anchor-or-handle)
  ;; Used by `edraw-get-nth-point'
  (cond
   ((edraw-path-anchor-p anchor-or-handle)
    (let* ((anchor anchor-or-handle)
           (index (edraw-path-anchor-index-in-data anchor)))
      (when index
        (+ (* index 3) 1))))
   ((edraw-path-handle-p anchor-or-handle)
    (let* ((handle anchor-or-handle)
           (index (edraw-path-anchor-index-in-data
                   (edraw-path-handle-parent handle))))
      (when index
        (+ (* index 3)
           (if (edraw-path-handle-forward-p handle) 2 0)))))))

(defun edraw-shape-path-point-nth (path-data index)
  ;; Used by `edraw-get-nth-point'
  (when (integerp index)
    (let* ((anchor-index (/ index 3))
           (sub-index (% index 3))
           (anchor (edraw-path-data-anchor-nth path-data anchor-index)))

      ;; (unless anchor
      ;;   (message "Warning: Point #%s not found (anchor #%s/%s)"
      ;;            index anchor-index
      ;;            (edraw-path-data-anchor-count path-data)))

      (when anchor
        (pcase sub-index
          (0 (edraw-path-anchor-backward-handle anchor))
          (1 anchor)
          (2 (edraw-path-anchor-forward-handle anchor)))))))

(cl-defmethod edraw-get-nth-point ((shape edraw-shape-path) index)
  ;; Corresponds to `edraw-index-in-path'
  (edraw-shape-path-make-point-object
   shape
   (edraw-shape-path-point-nth (oref shape path-data) index)))

(cl-defmethod edraw-get-nth-anchor-point ((shape edraw-shape-path) index)
  (let ((anchor (edraw-path-data-anchor-nth (oref shape path-data) index)))
    ;; (unless anchor
    ;;   (message "Warning: Anchor #%s not found (/ %s)"
    ;;            index (edraw-path-data-anchor-count (oref shape path-data))))
    (edraw-shape-path-make-anchor-object shape anchor)))

(cl-defmethod edraw-get-first-anchor-point ((shape edraw-shape-path))
  (edraw-shape-path-make-anchor-object
   shape
   ;; Make edraw-path-data-anchor-first ?
   (when-let ((subpath (edraw-path-data-first-or-nil (oref shape path-data))))
     (edraw-path-subpath-anchor-first-or-nil subpath))))

(cl-defmethod edraw-get-last-anchor-point ((shape edraw-shape-path))
  (edraw-shape-path-make-anchor-object
   shape
   ;; Make edraw-path-data-anchor-last ?
   (when-let ((subpath (edraw-path-data-last-or-nil (oref shape path-data))))
     (edraw-path-subpath-anchor-last-or-nil subpath))))

(cl-defmethod edraw-get-endpoints ((shape edraw-shape-path))
  "List all the endpoints contained in SHAPE.

Enumerates the start and end anchor points of all open subpaths.

Typically, two anchor points are listed for each open subpath,
but only one is listed for subpaths that have only one anchor
point.

Closed subpaths and handle points are ignored."
  (mapcar
   (lambda (anchor) (edraw-shape-path-make-anchor-object shape anchor))
   (edraw-path-data-endpoints (oref shape path-data))))

(cl-defmethod edraw-add-anchor ((shape edraw-shape-path)
                                      xy
                                      &optional
                                      backward-handle-xy-relative
                                      forward-handle-xy-relative)
  "Add a new anchor to the end of the path SHAPE.
XY are the coordinates of the anchor.

This function adds a new anchor to the end of the last subpath in
the path data. If the path data is empty, a new subpath is
created."
  (let ((anchor (edraw-path-data-add-new-anchor
                 (oref shape path-data)
                 xy
                 backward-handle-xy-relative
                 forward-handle-xy-relative)))
    (edraw-make-undo-group (oref shape editor) 'shape-path-add-anchor-last
      (edraw-push-undo
       (oref shape editor) 'shape-path-add-anchor-last
       (list 'edraw-remove-last-anchor shape))
      (edraw-update-path-d-attr shape)
      (edraw-on-shape-changed shape 'anchor-add))

    ;; Return a new shape point
    (edraw-shape-path-make-anchor-object shape anchor)))

(cl-defmethod edraw-remove-last-anchor ((shape edraw-shape-path))
  "Delete the last anchor of the path SHAPE.
If the last subpath becomes empty due to the deletion, also delete the subpath."
  (when-let ((subpath (edraw-path-data-last-or-nil (oref shape path-data)))
             (anchor (edraw-path-subpath-anchor-last-or-nil subpath)))
    (edraw-make-undo-group (oref shape editor) 'shape-path-remove-anchor-last
      (edraw-path-anchor-remove anchor)
      (when (edraw-path-subpath-empty-p subpath)
        (edraw-path-subpath-remove subpath))
      (edraw-push-undo
       (oref shape editor) 'shape-path-remove-anchor-last
       (list 'edraw-add-anchor shape
             (edraw-path-anchor-xy anchor)
             (when (edraw-path-anchor-has-backward-handle anchor)
               (edraw-path-anchor-backward-handle-xy-relative anchor))
             (when (edraw-path-anchor-has-forward-handle anchor)
               (edraw-path-anchor-forward-handle-xy-relative anchor))))
      (edraw-update-path-d-attr shape)
      (edraw-on-shape-changed shape 'anchor-remove))))

;; (cl-defmethod edraw-add-anchor-nth ((shape edraw-shape-path)
;;                                     subpath-index
;;                                     anchor-index-in-subpath
;;                                     xy
;;                                     &optional
;;                                     backward-handle-xy-relative
;;                                     forward-handle-xy-relative)
;;   )

;; (cl-defmethod edraw-remove-anchor-nth ((shape edraw-shape-path) index)
;;   )

(cl-defmethod edraw-move-nth-point ((shape edraw-shape-path) index xy)
  (when-let ((spt (edraw-get-nth-point shape index)))
    (edraw-move spt xy)))

(cl-defmethod edraw-move-nth-points ((shape edraw-shape-path) list-index-xy)
  (dolist (index-xy list-index-xy)
    (edraw-move-nth-point shape (car index-xy) (cdr index-xy))))

(cl-defmethod edraw-update-path-d-attr ((shape edraw-shape-path))
  (edraw-svg-set-attr-string (edraw-element shape)
                             'd
                             (edraw-path-data-to-string (oref shape path-data)))
  ;; The caller calls (edraw-on-shape-changed shape)
  )

(cl-defmethod edraw-on-shape-point-changed ((shape edraw-shape-path) type)
  (edraw-update-path-d-attr shape)
  (edraw-on-shape-changed shape type))

(cl-defmethod edraw-closed-path-shape-p ((shape edraw-shape-path))
  "Return non-nil if the SHAPE is a closed path.
Note: This method does not support multiple subpaths.
Return the status of the last subpath."
  (when-let ((subpath (edraw-path-data-last-or-nil (oref shape path-data))))
    (edraw-path-subpath-closed-p subpath)))

(cl-defmethod edraw-open-path-shape-p ((shape edraw-shape-path))
  "Return non-nil if the SHAPE is an open path.
Note: This method does not support multiple subpaths.
Return the status of the last subpath."
  (when-let ((subpath (edraw-path-data-last-or-nil (oref shape path-data))))
    (not (edraw-path-subpath-closed-p subpath))))

(cl-defmethod edraw-close-path-shape ((shape edraw-shape-path))
  "Close the SHAPE path.
Note: This method does not support multiple subpaths.
Close the last subpath if it is open."
  (when-let ((subpath (edraw-path-data-last-or-nil (oref shape path-data))))
    (when (edraw-path-subpath-open-p subpath)
      (edraw-path-subpath-close subpath)
      (edraw-make-undo-group (oref shape editor) 'shape-path-close
        (edraw-push-undo (oref shape editor) 'shape-path-close
                         (list 'edraw-open-path-shape shape))
        (edraw-update-path-d-attr shape)
        (edraw-on-shape-changed shape 'shape-close-path))
      t)))

(cl-defmethod edraw-open-path-shape ((shape edraw-shape-path))
  "Open the SHAPE path.
Note: This method does not support multiple subpaths.
Open the last subpath if it is closed."
  (when-let ((subpath (edraw-path-data-last-or-nil (oref shape path-data))))
    (when (edraw-path-subpath-closed-p subpath)
      (edraw-path-subpath-open subpath)
      (edraw-make-undo-group (oref shape editor) 'shape-path-close
        (edraw-push-undo (oref shape editor) 'shape-path-open
                         (list 'edraw-close-path-shape shape))
        (edraw-update-path-d-attr shape)
        (edraw-on-shape-changed shape 'shape-close-path))
      t)))

(cl-defmethod edraw-reverse-path ((shape edraw-shape-path))
  "Reverse the order of anchor points in the path.
The order of all subpaths, anchors, and handles within the SHAPE is reversed."
  (edraw-path-data-reverse (oref shape path-data))
  (edraw-make-undo-group (oref shape editor) 'shape-path-reverse
    (edraw-push-undo (oref shape editor) 'shape-path-reverse
                     (list 'edraw-reverse-path shape))
    (edraw-update-path-d-attr shape)
    (edraw-on-shape-changed shape 'shape-reverse-path))
  t)

(defun edraw-xy-points-to-smooth-path-data (xy-points
                                            &optional
                                            bezier-fitting-tolerance
                                            suppress-first-move-command)
  (let* ((points
          (edraw-xy-remove-consecutive-same-points xy-points))
         (points-vector (apply #'vector points))
         (size (length points-vector)))
    (when (>= size 2)
      (let* ((bezier-segments
              (edraw-fit-bezier-curve points-vector bezier-fitting-tolerance))
             (prev-segment nil)
             (d
              (concat
               (unless suppress-first-move-command
                 (edraw-path-cmdstr-move  (aref (car bezier-segments) 0)))
               (mapconcat
                (lambda (segment)
                  (prog1
                      (edraw-path-cmdstr-curve
                       (aref segment 3) (aref segment 2) (aref segment 1)
                       (aref segment 0)
                       (when prev-segment
                         (aref prev-segment 2)))
                    (setq prev-segment segment)))
                bezier-segments ""))))
        d))))

(cl-defmethod edraw-make-smooth ((path edraw-shape-path))
  (let ((path-data (oref path path-data))
        d)
    (edraw-path-data-subpath-loop path-data subpath
      (let (points)
        (edraw-path-subpath-anchor-loop subpath anchor
          (push (edraw-path-anchor-xy anchor) points))
        (when (cddr points) ;; 3 or more points
          (when (edraw-path-subpath-closed-p subpath)
            (push (edraw-path-anchor-xy
                   (edraw-path-subpath-anchor-first-or-nil subpath))
                  points))
          (when-let (subpath-d
                     (edraw-xy-points-to-smooth-path-data points)
                     ;; Emulate Freehand Tool
                     ;; (edraw-editor-tool-freehand--smooth-bezier-fitting
                     ;;  points
                     ;;  (edraw-scroll-scale (oref path editor)))
                     )
            ;;(message "subpath-d=%s" subpath-d)
            (setq d (concat d subpath-d
                            (when (edraw-path-subpath-closed-p subpath)
                              "Z")))))))
    (when d
      ;;(message "d=%s" d)
      (edraw-set-property path 'd d)
      t)))

(cl-defmethod edraw-combine-paths ((shapes list))
  "Combine path objects.

Only objects of class edraw-shape-path or its derived classes in
SHAPES will be processed; all others will be ignored.

All subpaths contained in the path data being processed will be
combined into a single path data as is.

A new path object will be created with the combined path data.

The new path object will have the properties of the first object in SHAPES.

All path objects being processed will be deleted."
  ;; Filter out objects that are not path or its derived classes.
  (let ((paths (seq-filter
                (lambda (obj) (cl-typep obj 'edraw-shape-path))
                shapes)))
    (when (cdr paths) ;; Two or more paths are required
      (let ((base-path (car paths))) ;;Use first one in PATHS as basis?
        (edraw-make-undo-group (edraw-get-editor base-path) 'combine-paths

          (let ((new-path (edraw-clone base-path)))

            (let ((new-path-data (edraw-path-data)))
              ;; Move all subpaths to the new-path-data
              (dolist (path paths)
                (edraw-path-data-insert-data-last new-path-data
                                                  (oref path path-data))
                ;; Restores path-data that became emptied during UNDO
                ;; @todo Avoid undo d=
                (edraw-push-undo-properties path 'combine-paths '(d))
                ;; Update empty d= (changes required to undo)
                (edraw-update-path-d-attr path)
                ;; Notify that all anchors have removed from the path
                (edraw-on-shape-changed path 'anchor-transfer-out))

              ;; Set combined path data to new-path
              (edraw-path-data-subpath-swap (oref new-path path-data)
                                            new-path-data)
              (edraw-update-path-d-attr new-path)
              (edraw-on-shape-changed new-path 'shape-init-path-data)

              ;; Remove old paths
              (dolist (path paths)
                (edraw-remove path))
              ;; Select new path
              (edraw-select new-path)))))
      t)))

(cl-defmethod edraw-contains-multiple-subpaths-p ((shape edraw-shape-path))
  "Return non-nil if SHAPE has two or more subpaths."
  (edraw-path-data-multiple-subpaths-p (oref shape path-data)))

(cl-defmethod edraw-split-subpaths ((shape edraw-shape-path))
  (when (edraw-contains-multiple-subpaths-p shape)
    (with-slots (path-data editor) shape
      ;; Deselect
      ;; (edraw-deselect-all-shapes (oref shape editor))
      (when (edraw-selected-p shape)
        (edraw-deselect shape))

      (edraw-make-undo-group (oref shape editor) 'split-subpaths
        ;; Restores path-data that became emptied during UNDO
        ;; @todo Avoid undo d=
        (edraw-push-undo-properties shape 'split-subpaths '(d))

        (let (subpath-list new-shape-list)
          ;; Collect subpaths
          (edraw-path-data-subpath-loop path-data subpath
            (push subpath subpath-list)) ;;Reversed order

          ;; Create new path shapes
          (dolist (subpath subpath-list)
            (let ((new-shape (edraw-clone shape)))
              (edraw-path-data-subpath-swap
               (oref new-shape path-data)
               (let ((new-data (edraw-path-data)))
                 (edraw-path-data-insert-subpath-last new-data subpath)
                 new-data))
              (edraw-update-path-d-attr new-shape)
              (edraw-on-shape-changed new-shape 'shape-init-path-data)
              (push new-shape new-shape-list)))

          ;; Update empty d= (changes required to undo)
          (edraw-update-path-d-attr shape)
          ;; Notify that all anchors have removed from the old path shape
          (edraw-on-shape-changed shape 'anchor-transfer-out)
          ;; Delete old path shape
          (edraw-remove shape)

          ;; Select
          ;; (dolist (new-shape new-shape-list)
          ;;   (edraw-add-shape-selection editor new-shape))

          t)))))



;;;;; Shape - Group

(defconst edraw-shape-group-shape-type 'g)

(edraw-shape-type-set edraw-shape-group-shape-type 'edraw-shape-group)

(defclass edraw-shape-group (edraw-shape-with-rect-boundary)
  ((child-change-notification-suppressed-p :type boolean :initform nil)))

(cl-defmethod edraw-shape-type ((_shape edraw-shape-group))
  edraw-shape-group-shape-type)

(cl-defmethod edraw-shape-type ((_class (subclass edraw-shape-group)))
  edraw-shape-group-shape-type)

(cl-defmethod edraw-shape-svg-tag ((_class (subclass edraw-shape-group)))
  'g)

(cl-defmethod edraw-on-child-changed ((shape edraw-shape-group)
                                      (_child edraw-shape) _type)
  ;;(message "on child changed group=%s child=%s type=%s suppress=%s" (edraw-name shape) (edraw-name _child) _type (oref shape child-change-notification-suppressed-p))
  (unless (oref shape child-change-notification-suppressed-p)
    ;; Update anchor points
    (edraw-update-p0p1-to-aabb shape)
    ;; Treat as a change in this group
    (edraw-on-shape-changed shape 'child-change)))

(cl-defmethod edraw-update-p0p1-to-aabb ((shape edraw-shape-group))
  "Update anchor points to fit descendants aabb."
  (let ((aabb (edraw-shape-aabb-local shape)))
    (if aabb
        (edraw-set-p0p1-without-notify shape
                                       (edraw-rect-left aabb)
                                       (edraw-rect-top aabb)
                                       (edraw-rect-right aabb)
                                       (edraw-rect-bottom aabb))
      (edraw-set-p0p1-without-notify shape 0 0 0 0))))

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

(cl-defmethod edraw-funcall-with-child-change-notification-suppressed
  ((group edraw-shape-group)
   change-type
   func &rest args)

  (let ((old-suppress-p
         (oref group child-change-notification-suppressed-p)))
    (oset group child-change-notification-suppressed-p t)
    (unwind-protect
        (apply func args)
      (oset group child-change-notification-suppressed-p old-suppress-p)))
  ;; Update anchor points
  (edraw-update-p0p1-to-aabb group)
  ;; Notify change
  (edraw-on-shape-changed group change-type))

(cl-defmethod edraw-transform-internal ((group edraw-shape-group) matrix
                                        transform-function
                                        change-type
                                        change-type-undo)
  ;; Transform child shapes.
  (with-slots (editor) group
    ;;@todo Check empty group

    (let ((undo-data-before-change (edraw-last-undo-data editor)))
      ;; Create one or zero undo data
      (edraw-make-undo-group editor change-type
        (edraw-funcall-with-child-change-notification-suppressed
         group
         change-type
         (lambda ()
           (dolist (child (edraw-children group))
             (funcall transform-function child matrix)))))

      ;; If new undo data exists
      (unless (eq (edraw-last-undo-data editor) undo-data-before-change)
        ;; Wrap undo data in suppressed funcall
        (with-slots (undo-list) editor
          ;; Rewrite the latest undo data
          (setcar undo-list
                  (edraw-undo-data
                   change-type-undo
                   (append
                    (list
                     #'edraw-funcall-with-child-change-notification-suppressed
                     group
                     change-type-undo)
                    ;; (func args...)
                    (edraw-undo-data-func-args (car undo-list)))))))))) ;;strip type

(cl-defmethod edraw-transform-local ((group edraw-shape-group) matrix)
  (edraw-transform-internal group matrix
                            #'edraw-transform
                            'group-transform-local
                            'group-transform-local-undo))

(cl-defmethod edraw-transform-anchor-points-local ((group edraw-shape-group) matrix)
  (edraw-transform-internal group matrix
                            #'edraw-transform-anchor-points
                            'group-transform-anchor-points-local
                            'group-transform-anchor-points-local-undo))

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

(cl-defmethod edraw-get-actions ((shape edraw-shape-group))
  (let* ((items (copy-tree (cl-call-next-method))))
    (append
     items
     `(((edraw-msg "Ungroup") edraw-ungroup-interactive
        ,@(edraw-get-actions--keys
           shape 'edraw-editor-ungroup-selected-shapes))))))

;;;;; Shape - Generator

;;;;;; Generator Functions

(defvar edraw-shape-generator-alist
  '(("latex" :prefix edraw-gen-latex :feature edraw-generator)
    ("grid" :prefix edraw-gen-grid :feature edraw-generator))
  "An association list whose keys are generator type strings and
values are property lists.

- :feature
  Library that should be loaded before calling the generator function.

- :prefix
  A symbol that prefixes the functions to be called.

  - (<prefix> <src> <keyword-argument>...)
    Generate a new element and return it.
    <keyword-argument> :
    - :options <alist>

  - (<prefix>-options-info)
    Return a list containing the names and types of options that
    can be set in the gen-options property.

  - (<prefix>-defaults)
    Return the default property values for the generator shape as an alist.

  - (<prefix>-interactive <editor>)
    Read the properties of the shape that will be created from the user.")

(defun edraw-shape-generator-type-set (type &rest plist)
  "Set generator type properties.

TYPE represents the type of generator. Specify the properties of
that type in the remaining argument PLIST.

The settings are recorded in the variable `edraw-shape-generator-alist'."
  (if-let ((cell (assoc type edraw-shape-generator-alist)))
      (setcdr cell (edraw-plist-append plist (cdr cell)))
    (push (cons type plist) edraw-shape-generator-alist))
  (edraw-shape-generator-prop-info-list--clear-cache))

(defun edraw-shape-generator-types ()
  (mapcar #'car edraw-shape-generator-alist))

(defun edraw-shape-generator-alist ()
  edraw-shape-generator-alist)

(defun edraw-shape-generator-type-from-element (element)
  (dom-attr element 'data-edraw-gen-type))

(defun edraw-shape-generator-prefix (type &optional noerror)
  (let* ((gen-info (alist-get type edraw-shape-generator-alist nil nil #'equal))
         (feature (plist-get gen-info :feature))
         (prefix (plist-get gen-info :prefix)))
    (unless noerror
      (unless type
        (error "Generator type not specified"))
      (unless gen-info
        (error "Generator type `%s' is not defined" type))
      (unless prefix
        (error "Generator type `%s' has no prefix specified" type)))
    (when feature
      (require feature nil noerror))
    prefix))

(defun edraw-shape-generator-execute (element)
  "Generate a shape by the shape generator ELEMENT."
  (let* ((prefix (edraw-shape-generator-prefix
                  (edraw-shape-generator-type-from-element element)))
         (gen-source (dom-attr element 'data-edraw-gen-source)))
    ;; The validity of gen-source is checked on the generator side. It might
    ;; be possible to generate something even if it is empty.
    (funcall prefix gen-source
             :options (edraw-shape-generator-options-get element))))

(defun edraw-shape-generator-options-get (element)
  "Get generation option alist from the shape generator ELEMENT."
  (let ((gen-options-attr (dom-attr element 'data-edraw-gen-options))
        (prop-info-list (edraw-shape-generator-options-info element)))
    (edraw-svg-prop-cssdecls-to-lisp-value-alist gen-options-attr
                                                 prop-info-list
                                                 element
                                                 'data-edraw-gen-options)))

(defun edraw-shape-generator-options-info (element)
  "Return generation option information list for the shape generator ELEMENT.

The format of the returned information is the same as `get-property-info-list',
which is a list of `edraw-svg-prop-info' objects."
  (edraw-shape-generator-call (edraw-shape-generator-type-from-element element)
                              nil
                              "-options-info"))

(defun edraw-shape-generator-package-defaults (type)
  (edraw-shape-generator-call type nil "-defaults"))

(defun edraw-shape-generator-interactive (type editor rect default)
  (edraw-shape-generator-call type default "-interactive" editor rect))

(defun edraw-shape-generator-safety (type)
  (edraw-shape-generator-call type nil "-safety"))

(defun edraw-shape-generator-call (type default function-name &rest args)
  (let ((prefix (edraw-shape-generator-prefix type t)))
    (if prefix
        (let ((fun (intern (format "%s%s" prefix function-name))))
          (if (fboundp fun)
              (apply fun args)
            default))
      default)))

;;;;;; Generator Shape

(defconst edraw-shape-generator-shape-type 'edraw-generator)

(edraw-shape-type-set edraw-shape-generator-shape-type 'edraw-shape-generator)

(defclass edraw-shape-generator (edraw-shape-group)
  ((prop-info-list-cache :initform nil)))

(cl-defmethod edraw-shape-type ((_class (subclass edraw-shape-generator)))
  edraw-shape-generator-shape-type)

(cl-defmethod edraw-shape-type ((_shape edraw-shape-generator))
  edraw-shape-generator-shape-type)

(cl-defmethod edraw-get-summary ((shape edraw-shape-generator))
  (format "generator (%s,%s)"
          (or (edraw-get-property shape 'gen-type) "")
          (truncate-string-to-width
           (replace-regexp-in-string
            "[\r\n\t\f]"
            " "
            (or (edraw-get-property shape 'gen-source) "")
            t t)
           20 nil nil "...")))

(defconst edraw-shape-generator-prop-info-list--cache nil)

(defun edraw-shape-generator-prop-info-list ()
  (or edraw-shape-generator-prop-info-list--cache
      (setq edraw-shape-generator-prop-info-list--cache
            (edraw-shape-generator-prop-info-list--make))))

(defun edraw-shape-generator-prop-info-list--clear-cache ()
  (setq edraw-shape-generator-prop-info-list--cache nil))

(defun edraw-shape-generator-prop-info-list--make ()
  (append
   (list
    ;; Add attributes:
    ;; - data-edraw-gen-type
    ;; - data-edraw-gen-source
    ;; - data-edraw-gen-options
    (edraw-svg-prop-info 'gen-type 'attr-data
                         (cons 'or (edraw-shape-generator-types)) nil)
    (edraw-svg-prop-info 'gen-source 'attr-data 'text nil)
    ;; Generally string, but for individual objects use
    ;; (cssdecls :prop-info-list ...)
    (edraw-svg-prop-info 'gen-options 'attr-data 'string nil))
   (edraw-svg-tag-get-property-info-list 'g)))

(cl-defmethod edraw-get-property-info-list ((_class
                                             (subclass edraw-shape-generator)))
  (edraw-shape-generator-prop-info-list))

(cl-defmethod edraw-get-property-info-list ((shape edraw-shape-generator))
  (with-slots (prop-info-list-cache) shape
    ;; Create prop-info-list-cache
    (unless prop-info-list-cache
      (setq prop-info-list-cache
            (cl-loop for prop-info in (edraw-shape-generator-prop-info-list)
                     ;; Replace type of `gen-options' property.
                     if (eq (edraw-svg-prop-info-name prop-info) 'gen-options)
                     collect (edraw-svg-prop-info
                              'gen-options
                              'attr-data
                              `(cssdecls
                                :prop-info-list
                                ;; Determine type of gen-options
                                ;; according to current gen-type.
                                ,(edraw-shape-generator-options-info
                                  (edraw-element shape)))
                              nil)
                     else
                     collect prop-info)))
    ;; Return prop-info-list-cache
    prop-info-list-cache))

(cl-defmethod edraw-on-shape-properties-changed ((shape edraw-shape-generator)
                                                 old-prop-list)
  ;; Clear prop-info-list-cache if gen-type is changed
  (when (assq 'gen-type old-prop-list)
    (with-slots (prop-info-list-cache) shape
      (setq prop-info-list-cache nil)))
  (cl-call-next-method))

(cl-defmethod edraw-property-editor-actions ((shape edraw-shape-generator))
  `(,@(cl-call-next-method)
    (push-button :notify ,(lambda (&rest _)
                            (edraw-regenerate-interactively shape))
                 ,(edraw-msg "Regenerate"))))

(cl-defmethod edraw-get-actions ((_shape edraw-shape-generator))
  (let* ((items (copy-tree (cl-call-next-method))))
    (append
     items
     `(((edraw-msg "Regenerate") edraw-regenerate-interactively
        ;; @todo Add key binding?
        ;; ,@(edraw-get-actions--keys
        ;;    shape 'edraw-editor-update-selected-shapes)
        )))))

(cl-defmethod edraw-regenerate ((shape edraw-shape-generator))
  (let ((new-element
         (condition-case err
             (edraw-shape-generator-execute (edraw-element shape))
           (error
            (message (edraw-msg "Generation error: %s") err)
            nil))))
    (unless new-element
      (setq new-element
            ;; Dummy
            (edraw-svg-rect 0 0 40 40 :stroke "red" :fill "#eee")))

    (with-slots (editor) shape
      (edraw-make-undo-group editor 'update-generator-shape
        ;; @todo I don't want to use edraw-shape objects for content

        ;; Remove old content
        ;;(edraw-dom-remove-all-children (edraw-element shape))
        (dolist (child (edraw-children shape))
          (edraw-remove child))

        ;; Add new content
        (edraw-dom-append-child (edraw-element shape) new-element)
        (edraw-create-shape-from-inserted-new-element editor new-element) ;; Notify
        ))))

(cl-defmethod edraw-regenerate-safely-p ((shape edraw-shape-generator))
  ;;@todo Skip confirmation for shapes that have been approved once (be careful of undo) or shapes created with the current editor.
  (memq (edraw-shape-generator-safety
         (edraw-shape-generator-type-from-element (edraw-element shape)))
        '(safe immediately-applicable)))

(cl-defmethod edraw-regenerate-interactively ((shape edraw-shape-generator))
  (when (or (edraw-regenerate-safely-p shape)
            (yes-or-no-p
             (edraw-msg "Evaluate this generator's code on your system?")))
    (edraw-regenerate shape)))

(cl-defmethod edraw-update-from-property-editor ((shape edraw-shape-generator))
  (edraw-regenerate-interactively shape))


;;;; Shape Point

;;
;; - Control points for changing shape
;; - By manipulating (e.g. moveing) a shape point, the related shape changes
;; - Usually either anchor point or handle point
;; - Shape point objects can be obtained from shape object
;;

;;;;; Shape Point - Point Set

(defun edraw-shape-point-find (point-list xy &optional pick-forced)
  (seq-find (lambda (spt) (edraw-hit-input-p spt xy pick-forced))
            point-list))

;;;;; Shape Point - Base Class

(defclass edraw-shape-point ()
  ()
  :abstract t)

(cl-defmethod edraw-previous-anchor-round ((_spt edraw-shape-point))
  nil)

(cl-defmethod edraw-next-anchor-round ((_spt edraw-shape-point))
  nil)

(cl-defmethod edraw-get-handle-points ((_spt edraw-shape-point))
  nil)

(cl-defmethod edraw-parent-shape ((_spt edraw-shape-point))
  nil)

(cl-defmethod edraw-get-actions ((_spt edraw-shape-point))
  nil)

(cl-defmethod edraw-filter-menu-items ((spt edraw-shape-point) menu-type items)
  (when-let ((shape (edraw-parent-shape spt))
             (editor (edraw-get-editor shape)))
    (edraw-filter-menu-items editor (list menu-type :target spt) items)))

(cl-defmethod edraw-menu-shape-point ((spt edraw-shape-point))
  (when-let ((actions (edraw-get-actions spt)))
    `(,(let* ((point-type (edraw-get-point-type spt))
              (point-type-name (pcase point-type
                                 ('anchor (edraw-msg "Anchor"))
                                 ('handle (edraw-msg "Handle"))
                                 (_ (capitalize (symbol-name point-type)))))
              (xy (edraw-get-xy-transformed spt)))
         (format "%s (%s, %s)"
                 point-type-name
                 (edraw-to-string (edraw-x xy))
                 (edraw-to-string (edraw-y xy))))
      ,(edraw-filter-menu-items spt 'shape-point actions))))

(cl-defmethod edraw-popup-context-menu ((spt edraw-shape-point))
  (when-let ((menu (edraw-menu-shape-point spt)))
    (edraw-popup-menu nil menu spt)
    ;; Return t if popuped
    t))

(cl-defmethod edraw-get-opposite-point ((_spt edraw-shape-point))
  "Return opposite point on rectangle boundary of SPT.

Return nil if SPT is not a point on rectangle boundary.

If SPT is a corner point, return the diagonal point.
If SPT is a midpoint of side, return the midpoint of the opposite side."
  nil)

(cl-defmethod edraw-get-opposite-point-vectors ((_spt edraw-shape-point))
  "Return two vectors from SPT on rectangle boundary to diagonal point.

The two vectors are along the vertical and horizontal directions
of the rectangle.

Adding two vectors to SPT leads to the opposite point."
  nil)

(cl-defgeneric edraw-valid-point-p (_spt)
  "Return non-nil if the SPT is valid.

Points (anchors, handles) in `edraw-shape-path' can become
invalid due to modification operations. A typical example is
removing a point, but operations that reduce points or move them
to another shape can also invalidate points
(`edraw-combine-paths', `edraw-split-subpaths', etc.).
Also, modifying the d attribute invalidates all points in the
shape.

Behavior of operations on invalid points is undefined and should
be avoided.

To detect invalidation of a point, use `edraw-valid-point-p' or
monitor changes with `edraw-add-change-hook'. edraw-editor
monitors selected anchors and handles for invalidation using
change hook."
  t)

(cl-defmethod edraw-anchor-p ((spt edraw-shape-point))
  (eq (edraw-get-point-type spt) 'anchor))

(cl-defmethod edraw-handle-p ((spt edraw-shape-point))
  (eq (edraw-get-point-type spt) 'handle))

(cl-defmethod edraw-hit-input-p ((spt edraw-shape-point) xy
                                 &optional pick-forced)
  "Returns non-nil, if the point SPT hits the pointer input(e.g. click) point XY."
  (let* ((shape (edraw-parent-shape spt))
         (editor (and shape (oref shape editor))))
    (when (or pick-forced
              (null shape)
              (edraw-visible-and-pickable-p shape))
      (let ((scale (if editor
                       (float (edraw-scroll-scale editor))
                     1.0))) ;;@todo Should I add the argument SCALE?

        (pcase (edraw-get-point-type spt)
          ('anchor (<= (edraw-xy-distance-l-inf (edraw-get-xy-transformed spt)
                                                xy) ;;square
                       (/ edraw-anchor-point-input-radius scale)))
          ('handle (<= (edraw-xy-distance-squared (edraw-get-xy-transformed spt)
                                                  xy) ;;circle
                       (let ((r (/ edraw-handle-point-input-radius scale)))
                         (* r r)))))))))

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

(cl-defmethod edraw-delete-point ((_spt edraw-shape-point))
  (message (edraw-msg "The operation is not supported on this object")))


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

(cl-defmethod edraw-get-opposite-point ((spt edraw-shape-point-rect-boundary))
  (let ((ref-x (edraw-ref-x spt))
        (ref-y (edraw-ref-y spt)))
    (edraw-get-anchor-point-by-ref-xy (oref spt shape)
                                      (and ref-x (logxor ref-x 1))
                                      (and ref-y (logxor ref-y 1)))))

(cl-defmethod edraw-get-opposite-point-vectors ((spt edraw-shape-point-rect-boundary))
  (let ((ref-x (edraw-ref-x spt))
        (ref-y (edraw-ref-y spt))
        (origin-xy (edraw-get-xy-transformed spt))) ;;@todo transformed?
    (cons
     (edraw-xy-sub
      (edraw-get-xy-transformed (edraw-get-anchor-point-by-ref-xy
                                 (oref spt shape)
                                 (and ref-x (logxor ref-x 1))
                                 ref-y))
      origin-xy)
     (edraw-xy-sub
      (edraw-get-xy-transformed (edraw-get-anchor-point-by-ref-xy
                                 (oref spt shape)
                                 ref-x
                                 (and ref-y (logxor ref-y 1))))
      origin-xy))))


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

(cl-defmethod edraw-delete-point ((spt edraw-shape-point-text))
  (edraw-remove (edraw-parent-shape spt)))

;;;;; Shape Point - Path Base

(defclass edraw-shape-point-path-base (edraw-shape-point)
  ((shape :initarg :shape :type edraw-shape-path)))

(cl-defmethod edraw-parent-shape ((spt edraw-shape-point-path-base))
  (oref spt shape))

(cl-defmethod edraw-push-undo-path-d-change
  ((spt edraw-shape-point-path-base) type)
  "Register undo data that restores the entire d property.

This is because undoing will revert the d attribute to its saved
value and rebuild the path-data, thereby invalidating all point
objects."
  (unless edraw-editor-inhibit-make-undo-data
    (with-slots (shape) spt
      (let* ((index (edraw-index-in-path spt))
             (type (intern (format "%s-p%s" type index))))
        ;; (message "type=%s len undo=%s" type
        ;;          (length (edraw-undo-list (oref shape editor))))
        (edraw-push-undo-properties shape type '(d))))))

(cl-defmethod edraw-on-path-point-move ((spt edraw-shape-point-path-base)
                                        type
                                        old-xy
                                        &optional
                                        opposite-index-old-xy
                                        notify-change-type)
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
        (edraw-on-shape-point-changed shape (or notify-change-type
                                                'point-move))))))


;;;;; Shape Point - Path Anchor

(defclass edraw-shape-point-path-anchor (edraw-shape-point-path-base)
  ((anchor :initarg :anchor :reader edraw-shape-point-path-anchor-obj)))

(cl-defmethod edraw-get-point-type ((_spt edraw-shape-point-path-anchor))
  'anchor)

(cl-defmethod edraw-valid-point-p ((spt edraw-shape-point-path-anchor))
  (when-let* ((subpath (edraw-path-anchor-parent-subpath (oref spt anchor)))
              (data (edraw-path-subpath-parent-data subpath)))
    (let ((shape (oref spt shape)))
      (and (eq data (oref shape path-data))
           (not (edraw-removed-p shape))))))

(cl-defmethod edraw-same-point-p ((spt1 edraw-shape-point-path-anchor) spt2)
  (and spt2
       (object-of-class-p spt2 'edraw-shape-point-path-anchor)
       (eq (oref spt1 shape) (oref spt2 shape))
       (eq (oref spt1 anchor) (oref spt2 anchor))))

(cl-defmethod edraw-same-subpath-p ((spt1 edraw-shape-point-path-anchor)
                                    (spt2 edraw-shape-point-path-anchor))
  "Return non-nil if the SPT1 and the SPT2 are on the same subpath."
  (when-let ((subpath1 (edraw-path-anchor-parent-subpath (oref spt1 anchor)))
             (subpath2 (edraw-path-anchor-parent-subpath (oref spt2 anchor))))
    (eq subpath1 subpath2)))


(cl-defmethod edraw-in-closed-subpath-p ((spt edraw-shape-point-path-anchor))
  "Returns t if the point SPT is part of a closed subpath in the path shape."
  (when-let ((subpath (edraw-path-anchor-parent-subpath (oref spt anchor))))
    (edraw-path-subpath-closed-p subpath)))

(cl-defmethod edraw-in-open-subpath-p ((spt edraw-shape-point-path-anchor))
  "Returns t if the point SPT is part of a closed subpath in the path shape."
  (when-let ((subpath (edraw-path-anchor-parent-subpath (oref spt anchor))))
    (edraw-path-subpath-open-p subpath)))

(cl-defmethod edraw-endpoint-p ((spt edraw-shape-point-path-anchor))
  "Return non-nil if the SPT is an end anchor point.
An endpoint means the first or last anchor point of an open subpath."
  (edraw-path-anchor-endpoint-p (oref spt anchor)))

(cl-defmethod edraw-first-end-p ((spt edraw-shape-point-path-anchor))
  "Return non-nil if SPT is the first end anchor.
Return nil whenever SPT is on a closed subpath.
If an SPT is the only anchor of an open subpath, it is both the
first and the last."
  (edraw-path-anchor-first-endpoint-p (oref spt anchor)))

(cl-defmethod edraw-last-end-p ((spt edraw-shape-point-path-anchor))
  "Return non-nil if SPT is the last end anchor.
Return nil whenever SPT is on a closed subpath.
If an SPT is the only anchor of an open subpath, it is both the
first and the last."
  (edraw-path-anchor-last-endpoint-p (oref spt anchor)))

(cl-defmethod edraw-first-anchor-p ((spt edraw-shape-point-path-anchor))
  "Return non-nil if the anchor SPT is the first of the subpath."
  (edraw-path-anchor-first-p (oref spt anchor)))

(cl-defmethod edraw-last-anchor-p ((spt edraw-shape-point-path-anchor))
  "Return non-nil if the anchor SPT is the last of the subpath."
  (edraw-path-anchor-last-p (oref spt anchor)))


(cl-defmethod edraw-previous-anchor-round
  ((spt edraw-shape-point-path-anchor))
  (with-slots (anchor shape) spt
    (when-let ((prev-anchor (edraw-path-anchor-prev-round anchor)))
      (edraw-shape-point-path-anchor :shape shape :anchor prev-anchor))))

(cl-defmethod edraw-next-anchor-round ((spt edraw-shape-point-path-anchor))
  (with-slots (anchor shape) spt
    (when-let ((next-anchor (edraw-path-anchor-next-round anchor)))
      (edraw-shape-point-path-anchor :shape shape :anchor next-anchor))))

(cl-defmethod edraw-anchor-index-in-path
  ((spt edraw-shape-point-path-anchor))
  (edraw-path-anchor-index-in-data (oref spt anchor)))

(cl-defmethod edraw-index-in-path ((spt edraw-shape-point-path-anchor))
  ;; Corresponds to `edraw-get-nth-point'.
  (edraw-shape-path-point-index (oref spt anchor)))

(cl-defmethod edraw-get-xy ((spt edraw-shape-point-path-anchor))
  (edraw-xy-clone (edraw-path-anchor-xy (oref spt anchor))))

(cl-defmethod edraw-move ((spt edraw-shape-point-path-anchor) &optional xy)
  (unless xy
    (setq xy (edraw-read-shape-point-xy spt)))
  (with-slots (anchor shape) spt
    (let ((old-xy (edraw-xy-clone (edraw-path-anchor-xy anchor))))
      (unless (edraw-xy-equal-p xy old-xy)
        (edraw-path-anchor-set-xy anchor xy) ;; Move with handles
        (edraw-on-path-point-move spt 'path-point-move old-xy)))))

(cl-defmethod edraw-get-handle-points ((spt edraw-shape-point-path-anchor))
  "Return the list of active handles of the anchor SPT."
  (with-slots (anchor shape) spt
    (delq nil
          (list
           (when (edraw-path-anchor-has-backward-handle anchor)
             (edraw-shape-point-path-handle
              :shape shape
              :handle (edraw-path-anchor-backward-handle anchor)))
           (when (edraw-path-anchor-has-forward-handle anchor)
             (edraw-shape-point-path-handle
              :shape shape
              :handle (edraw-path-anchor-forward-handle anchor)))))))

(cl-defmethod edraw-get-backward-handle ((spt edraw-shape-point-path-anchor))
  "Return the backward handle of the anchor SPT. Return even if not activated."
  (with-slots (anchor shape) spt
    (edraw-shape-point-path-handle
     :shape shape
     ;; If a handle is created at this timing, the handle remains
     ;; inactive. Basically, the handle is considered to have been
     ;; created with the anchor.
     :handle (edraw-path-anchor-backward-handle anchor))))

(cl-defmethod edraw-get-forward-handle ((spt edraw-shape-point-path-anchor))
  "Return the forward handle of the anchor SPT. Return even if not activated."
  (with-slots (anchor shape) spt
    (edraw-shape-point-path-handle
     :shape shape
     ;; If a handle is created at this timing, the handle remains
     ;; inactive. Basically, the handle is considered to have been
     ;; created with the anchor.
     :handle (edraw-path-anchor-forward-handle anchor))))

(cl-defmethod edraw-add-anchor-to-open-end
  ((spt edraw-shape-point-path-anchor) xy)
  "When SPT is an end anchor, insert a new anchor in the open (no
 anchor) direction next to it. That is, if SPT is the last
 anchor, add a new anchor after it, and if SPT is the first
 anchor, add a new anchor before it.

If it is both first and last, the last takes precedence.

If the SPT is on a closed subpath or in the middle of a subpath,
do nothing.

Set the new anchor's coordinates to XY.

Return the added anchor."
  (with-slots (anchor shape) spt
    (let ((new-anchor
           (cond
            ((edraw-last-end-p spt)
             (edraw-path-anchor-insert-after anchor (edraw-path-anchor xy)))
            ((edraw-first-end-p spt)
             (edraw-path-anchor-insert-before anchor (edraw-path-anchor xy))))))
      (when new-anchor
        ;;@todo Avoid using edraw-push-undo-path-d-change?
        (edraw-push-undo-path-d-change spt 'path-point-insert-to-open-end)
        (edraw-on-shape-point-changed shape 'anchor-insert)
        (edraw-shape-point-path-anchor :shape shape :anchor new-anchor)))))

(cl-defmethod edraw-get-actions ((spt edraw-shape-point-path-anchor))
  (with-slots (anchor) spt
    (let* ((backward-handle (edraw-path-anchor-backward-handle-or-nil anchor))
           (forward-handle (edraw-path-anchor-forward-handle-or-nil anchor))
           (glued-p (edraw-glued-p spt))
           (multi-subpaths-p (edraw-contains-multiple-subpaths-p
                              (edraw-parent-shape spt)))
           (splittable (not (edraw-endpoint-p spt))))
      `(((edraw-msg "Delete Point") edraw-delete-point)
        ((edraw-msg "Split Path at Point") edraw-split-path-at
         :enable ,splittable
         :visible ,(not multi-subpaths-p))
        ((edraw-msg "Split Subpath at Point") edraw-split-subpath-at
         :enable ,splittable
         :visible ,multi-subpaths-p)
        ((edraw-msg "Insert Point Before") edraw-insert-point-before
         :enable ,(not (null (edraw-path-anchor-prev-round anchor))))
        ((edraw-msg "Move by Coordinates...") edraw-move)
        ((edraw-msg "Make Smooth") edraw-make-smooth)
        ((edraw-msg "Make Corner") edraw-make-corner
         :enable ,(or backward-handle forward-handle))
        ((edraw-msg "Glue to selected or overlapped shape") edraw-glue-to-selected-or-overlapped-shape
         :visible ,(not glued-p)
         :enable ,(edraw-can-be-glued-to-selected-or-overlapped-shape spt))
        ((edraw-msg "Unglue") edraw-unglue
         :visible ,glued-p)))))

(cl-defmethod edraw-delete-point ((spt edraw-shape-point-path-anchor))
  (with-slots (anchor shape) spt
    (edraw-make-undo-group (oref shape editor) 'path-point-delete
      ;; Unglue if SPT is glued (Anchor only)
      (when (edraw-glued-p spt)
        (edraw-unglue spt))
      ;; Delete Point
      (when-let ((subpath (edraw-path-anchor-parent-subpath anchor)))
        ;; Remove anchor
        (edraw-path-anchor-remove anchor)
        ;; Remove empty subpath
        (when (edraw-path-subpath-empty-p subpath)
          (edraw-path-subpath-remove subpath))
        ;;@todo Avoid using edraw-push-undo-path-d-change?
        (edraw-push-undo-path-d-change spt 'path-point-delete)
        (edraw-on-shape-point-changed shape 'anchor-remove)
        t))))

(cl-defmethod edraw-insert-point-before ((spt edraw-shape-point-path-anchor))
  (with-slots (anchor shape) spt
    (when-let ((new-anchor (edraw-path-anchor-insert-midpoint-before anchor)))
      (edraw-make-undo-group (oref shape editor) 'path-point-insert-before
        ;;@todo Avoid using edraw-push-undo-path-d-change?
        (edraw-push-undo-path-d-change spt 'path-point-insert-before)
        (edraw-on-shape-point-changed shape 'anchor-insert)
        (edraw-shape-path-make-anchor-object shape new-anchor)))))

(cl-defmethod edraw-make-smooth ((spt edraw-shape-point-path-anchor))
  "Add handles to SPT anchor point."
  (with-slots (anchor shape) spt
    (let* ((curr-xy (edraw-path-anchor-xy anchor))
           (prev-anchor (edraw-path-anchor-prev-round anchor))
           (prev-xy (when prev-anchor (edraw-path-anchor-xy prev-anchor)))
           (next-anchor (edraw-path-anchor-next-round anchor))
           (next-xy (when next-anchor (edraw-path-anchor-xy next-anchor)))
           (pn (edraw-path-anchor-make-smooth-xy curr-xy prev-xy next-xy)))
      ;;(message "smooth prev=%s curr=%s next=%s" prev-xy curr-xy next-xy)
      (when (or (car pn) (cdr pn))
        (edraw-make-undo-group (oref shape editor) 'path-point-smooth
          (when (car pn)
            (edraw-move (edraw-get-backward-handle spt) (car pn)))
          (when (cdr pn)
            (edraw-move (edraw-get-forward-handle spt) (cdr pn))))
        t))))

(cl-defmethod edraw-make-corner ((spt edraw-shape-point-path-anchor))
  "Remove handles from SPT anchor point."
  (with-slots (anchor shape) spt
    (let ((backward-p (edraw-path-anchor-has-backward-handle anchor))
          (forward-p (edraw-path-anchor-has-forward-handle anchor)))
      (when (or backward-p forward-p)
        (edraw-make-undo-group (oref shape editor) 'path-point-corner
          (when backward-p
            (edraw-delete-point (edraw-get-backward-handle spt)))
          (when forward-p
            (edraw-delete-point (edraw-get-forward-handle spt))))
        t))))


(cl-defmethod edraw-split-subpath-at ((spt edraw-shape-point-path-anchor))
  "Split the subpath at the anchor SPT.

If the SPT is on an open subpath, split the subpath into two.
If it is on a closed subpath, reorder the subpath so that the SPT
is at the end of the subpath, then open the subpath.

This operation creates a new anchor that duplicates the SPT."
  (with-slots (anchor shape) spt
    (edraw-make-undo-group (oref shape editor) 'split-subpath-at-anchor
      (when (edraw-path-anchor-split anchor)
        (edraw-push-undo-properties shape 'split-path-at-anchor-d '(d))
        (edraw-update-path-d-attr shape)
        (edraw-on-shape-changed shape 'split-subpath-at-anchor)
        t))))

(cl-defmethod edraw-split-path-at ((spt edraw-shape-point-path-anchor))
  "Splits the path shape object at the anchor SPT."
  (with-slots (anchor shape) spt
    (edraw-make-undo-group (oref shape editor) 'split-path-at-anchor
      (when-let ((subpath (edraw-path-anchor-parent-subpath anchor))
                 (new-anchor (edraw-path-anchor-split anchor)))
        ;; Add new path shape
        (unless (eq (edraw-path-anchor-parent-subpath new-anchor) subpath)
          (let ((new-path-data (edraw-path-data))
                (new-subpath (edraw-path-anchor-parent-subpath new-anchor))
                (new-shape (edraw-clone shape)))
            (edraw-path-data-insert-subpath-last new-path-data new-subpath)
            (edraw-path-data-subpath-swap (oref new-shape path-data)
                                          new-path-data)
            (edraw-update-path-d-attr new-shape)
            (edraw-on-shape-changed new-shape 'split-path-at-anchor)))

        (edraw-push-undo-properties shape 'split-path-at-anchor-d '(d))
        (edraw-update-path-d-attr shape)
        (edraw-on-shape-changed shape 'split-path-at-anchor)
        t))))

(cl-defmethod edraw-connect ((SPT1 edraw-shape-point-path-anchor)
                             (SPT2 edraw-shape-point-path-anchor))
  "Connect anchor SPT1 to anchor SPT2.

If the connection is successful, SPT2 will be incorporated into
the parent shape of SPT1.

If successful, return the new SPT2. The previous SPT2 may become
invalid, so use this object instead."
  (unless (edraw-anchor-p SPT1)
    (error "Not anchor point: SPT1"))
  (unless (edraw-anchor-p SPT2)
    (error "Not anchor point: SPT2"))
  (unless (edraw-endpoint-p SPT1)
    (error "Not end anchor point: SPT1"))
  (unless (edraw-endpoint-p SPT2)
    (error "Not end anchor point: SPT2"))
  (when (edraw-same-point-p SPT1 SPT2)
    (error "The same points cannot be connected"))

  (let ((shape1 (oref SPT1 shape))
        (shape2 (oref SPT2 shape)) ;; Shape before connection
        (anchor1 (oref SPT1 anchor))
        (anchor2 (oref SPT2 anchor)))
    (edraw-make-undo-group (oref shape1 editor) 'connect-anchors
      (when (edraw-path-anchor-connect anchor1 anchor2)
        (if (eq shape1 shape2)
            ;; Same shape object
            (progn
              ;;@todo Avoid using edraw-push-undo-path-d-change?
              (edraw-push-undo-properties shape1 'connect-anchors '(d))
              (edraw-update-path-d-attr shape1)
              (edraw-on-shape-changed shape1 'connect-anchors)
              ;; Return the existing SPT2 object
              SPT2)
          ;; Different shape objects
          ;;(message "Connect different path shapes")
          ;;@todo Avoid using edraw-push-undo-path-d-change?
          (edraw-push-undo-properties shape1 'connect-anchors '(d))
          (edraw-push-undo-properties shape2 'connect-anchors '(d))
          (edraw-update-path-d-attr shape1)
          (edraw-update-path-d-attr shape2)
          (edraw-on-shape-changed shape1 'connect-anchors)
          (edraw-on-shape-changed shape2 'anchor-transfer-out)
          (when (edraw-path-data-no-anchor-p (oref shape2 path-data))
            (edraw-remove shape2))
          ;; Return new SPT2 object (anchor2 is now inside shape1)
          (edraw-shape-path-make-anchor-object shape1 anchor2))))))

(cl-defmethod edraw-glue-destination-of-selected-or-overlapped-shape
  ((spt edraw-shape-point-path-anchor))
  "Return the shape that is the glue destination."
  (with-slots (anchor shape) spt
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

(cl-defmethod edraw-can-be-glued-to-selected-or-overlapped-shape
  ((spt edraw-shape-point-path-anchor))
  (with-slots (anchor shape) spt
    (and (edraw-anchor-p spt)
         (not (edraw-glued-p spt))
         (edraw-glue-destination-of-selected-or-overlapped-shape spt))))

(cl-defmethod edraw-glue-to-selected-or-overlapped-shape
  ((spt edraw-shape-point-path-anchor))
  (when (edraw-anchor-p spt)
    (let ((dst-shape
           ;; Determine the destination shape
           (edraw-glue-destination-of-selected-or-overlapped-shape spt)))
      (unless dst-shape
        (error (edraw-msg "No glue target")))
      (edraw-glue-to spt dst-shape))))

(cl-defmethod edraw-glue-to ((spt edraw-shape-point-path-anchor)
                             (dst-shape edraw-shape))
  (when (edraw-anchor-p spt)
    (let* ((src-shape (oref spt shape))
           (num-anchors (edraw-get-anchor-point-count src-shape))
           (anchor-index (edraw-anchor-index-in-path spt)))
      (when anchor-index
        ;; Reverse anchor index
        (when (>= anchor-index (/ (1+ num-anchors) 2))
          (setq anchor-index (- anchor-index num-anchors)))
        ;;(message "Connect src=(%s %s) dst=%s" (edraw-name src-shape) anchor-index (edraw-name dst-shape))

        (edraw-make-undo-group (oref src-shape editor)
            'glue-to-selected-or-overlapped-shape
          (let ((conn (edraw-point-connection
                       :src (edraw-point-connection-src-anchor
                             :shape src-shape :index anchor-index)
                       :dst (edraw-point-connection-dst-shape
                             :shape dst-shape))))
            ;; Update XY before adding CONN to SRC-SHAPE.
            ;;(edraw-update conn)
            ;; Add CONN to SRC-SHAPE.
            (edraw-add-point-connection src-shape conn t)
            ;; Do not update XY after adding CONN.
            ;; When undoing, CONN must be removed before undoing XY move.
            ;;(edraw-update-all-point-connections src-shape)
            ))))))

(cl-defmethod edraw-unglue ((spt edraw-shape-point-path-anchor))
  (when (edraw-anchor-p spt)
    (with-slots (shape) spt
      (edraw-remove-point-connection
       shape
       (edraw-point-connection-src-anchor
        :shape shape
        :index (edraw-anchor-index-in-path spt))))))

(cl-defmethod edraw-glued-p ((spt edraw-shape-point-path-anchor))
  (when (edraw-anchor-p spt)
    (with-slots (shape) spt
      (not (null (edraw-find-point-connection
                  shape
                  (edraw-point-connection-src-anchor
                   :shape shape
                   :index (edraw-anchor-index-in-path spt))))))))


;;;;; Shape Point - Path Handle

(defclass edraw-shape-point-path-handle (edraw-shape-point-path-base)
  ((handle :initarg :handle :reader edraw-shape-point-path-handle-obj)))

(cl-defmethod edraw-get-point-type ((_spt edraw-shape-point-path-handle))
  'handle)

(cl-defmethod edraw-valid-point-p ((spt edraw-shape-point-path-handle))
  (when-let* ((anchor (edraw-path-handle-parent (oref spt handle)))
              (subpath (edraw-path-anchor-parent-subpath anchor))
              (data (edraw-path-subpath-parent-data subpath)))
    (let ((shape (oref spt shape)))
      (and (eq data (oref shape path-data))
           (not (edraw-removed-p shape))))))

(cl-defmethod edraw-active-handle-p ((spt edraw-shape-point-path-handle))
  "If SPT is an active handle, return a non-nil value.

Even if a handle is valid, it may not have any effect as a
 control point. Specifically, this is the case when its
 coordinates are in the same position as the anchor point. Such a
 state in which it has no effect is called inactive, and a state
 in which it has an effect is called active."
  (edraw-path-handle-active-p (oref spt handle)))

(cl-defmethod edraw-same-point-p ((spt1 edraw-shape-point-path-handle) spt2)
  (and spt2
       (object-of-class-p spt2 'edraw-shape-point-path-handle)
       (eq (oref spt1 shape) (oref spt2 shape))
       (eq (oref spt1 handle) (oref spt2 handle))))

(cl-defmethod edraw-parent-anchor ((spt edraw-shape-point-path-handle))
  (with-slots (handle shape) spt
    (when-let ((anchor (edraw-path-handle-parent handle)))
      (edraw-shape-point-path-anchor :shape shape :anchor anchor))))

(cl-defmethod edraw-index-in-path ((spt edraw-shape-point-path-handle))
  ;; Corresponds to `edraw-get-nth-point'.
  (edraw-shape-path-point-index (oref spt handle)))

(cl-defmethod edraw-get-xy ((spt edraw-shape-point-path-handle))
  (edraw-xy-clone (edraw-path-handle-xy (oref spt handle))))

(cl-defmethod edraw-set-xy-relative ((spt edraw-shape-point-path-handle) xy)
  (with-slots (handle shape) spt
    (let ((old-xy (edraw-xy-clone (edraw-path-handle-xy-relative handle))))
      (unless (edraw-xy-equal-p xy old-xy)
        (edraw-path-handle-set-xy-relative handle xy)

        ;; Push UNDO
        ;; (edraw-on-path-point-move spt 'path-point-move-relative old-xy)
        (let* ((index (edraw-index-in-path spt))
               (type (intern
                      (format "%s-p%s" 'path-handle-set-xy-relative index)))
               (editor (oref shape editor)))
          (edraw-make-undo-group (oref shape editor) type
            ;; Register undo data.
            ;; This function does not register the entire d property
            ;; as undo data.  Registering the d property as undo data
            ;; would cause all point objects to become invalid when
            ;; undoing, preventing them from being dragged. Undoing
            ;; while dragging is necessary for group transformations.
            (unless edraw-editor-inhibit-make-undo-data
              (edraw-push-undo
               editor type
               (list #'edraw-set-xy-relative shape index old-xy)))

            ;; Notify point move.
            (edraw-on-shape-point-changed shape 'set-xy-relative)))))))

(cl-defmethod edraw-move ((spt edraw-shape-point-path-handle) &optional xy)
  (unless xy
    (setq xy (edraw-read-shape-point-xy spt)))
  (with-slots (handle shape) spt
    (let ((old-xy (edraw-xy-clone (edraw-path-handle-xy handle))))
      (unless (edraw-xy-equal-p xy old-xy)
        (edraw-path-handle-set-xy handle xy)
        (edraw-on-path-point-move spt 'path-point-move old-xy)))))

(cl-defmethod edraw-get-opposite-handle-index-xy
  ((spt edraw-shape-point-path-handle))
  (when-let ((opposite-handle (edraw-path-handle-opposite-handle-or-nil
                               (oref spt handle)))
             (index (edraw-shape-path-point-index opposite-handle)))
    (cons
     index
     (edraw-xy-clone (edraw-path-handle-xy opposite-handle)))))

(cl-defmethod edraw-move-with-opposite-handle
  ((spt edraw-shape-point-path-handle) xy)
  (with-slots (handle shape) spt
    (let ((old-xy (edraw-xy-clone (edraw-path-handle-xy handle))))
      (unless (edraw-xy-equal-p xy old-xy)
        (let ((opposite-index-old-xy (edraw-get-opposite-handle-index-xy spt)))
          (edraw-path-handle-move-with-opposite-handle handle xy)
          (edraw-on-path-point-move
           spt
           'path-point-move-with-opposite-handle old-xy
           opposite-index-old-xy))))))

(cl-defmethod edraw-move-with-opposite-handle-on-transformed
  ((spt edraw-shape-point-path-handle) xy)
  (when-let ((shape (edraw-parent-shape spt))
             (inv-mat (edraw-transform-prop-get-inverse-matrix shape)))
    (edraw-move-with-opposite-handle
     spt (edraw-matrix-mul-mat-xy inv-mat xy))))

(cl-defmethod edraw-move-with-opposite-handle-symmetry
  ((spt edraw-shape-point-path-handle) xy)
  (with-slots (handle shape) spt
    (let ((old-xy (edraw-xy-clone (edraw-path-handle-xy handle))))
      (unless (edraw-xy-equal-p xy old-xy)
        (let ((opposite-index-old-xy (edraw-get-opposite-handle-index-xy spt)))
          (edraw-path-handle-move-with-opposite-handle-symmetry handle xy)
          (edraw-on-path-point-move
           spt
           'path-point-move-with-opposite-handle old-xy
           opposite-index-old-xy))))))

(cl-defmethod edraw-move-with-opposite-handle-symmetry-on-transformed
  ((spt edraw-shape-point-path-handle) xy)
  (when-let ((shape (edraw-parent-shape spt))
             (inv-mat (edraw-transform-prop-get-inverse-matrix shape)))
    (edraw-move-with-opposite-handle-symmetry
     spt (edraw-matrix-mul-mat-xy inv-mat xy))))

(cl-defmethod edraw-get-actions ((_spt edraw-shape-point-path-handle))
  `(((edraw-msg "Delete Point") edraw-delete-point)
    ((edraw-msg "Move by Coordinates...") edraw-move)
    ))

(cl-defmethod edraw-delete-point ((spt edraw-shape-point-path-handle))
  (with-slots (handle shape) spt
    (when (edraw-path-handle-active-p handle)
      (let ((old-xy (edraw-xy-clone (edraw-path-handle-xy handle))))
        (edraw-make-undo-group (oref shape editor) 'path-point-delete
          ;; Delete Point
          (edraw-path-handle-remove handle)
          (edraw-on-path-point-move spt 'path-point-delete old-xy
                                    nil 'handle-remove)
          t)))))



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

(cl-defmethod edraw-gap-distance ((src edraw-point-connection-src-anchor)
                                  dst-shape)
  "Return gap length between source and destination."
  (with-slots (shape index) src
    (or
     (when (or (= index 0) (= index -1))
       (let* ((prop-name (if (= index 0) 'marker-start 'marker-end))
              ;;(marker (edraw-get-property shape prop-name))
              (overhang (edraw-svg-marker-overhang (edraw-element shape)
                                                   prop-name
                                                   (edraw-get-deftbl shape))))
         (when overhang
           (let ((src-stroke (edraw-get-property shape 'stroke))
                 (dst-stroke (edraw-get-property dst-shape 'stroke)))
             (+
              (if (not (or (null src-stroke)
                           (equal src-stroke "")
                           (equal src-stroke "none")))
                  overhang
                0)
              (if (not (or (null dst-stroke)
                           (equal dst-stroke "")
                           (equal dst-stroke "none")))
                  (* 0.5
                     (edraw-get-property-as-length dst-shape 'stroke-width 0))
                0))))))
     0)))

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

(cl-defmethod edraw-gap-distance ((_src edraw-point-connection-src-attrs)
                                  _dst-shape)
  0)

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

(cl-defmethod edraw-gap-distance ((_src edraw-point-connection-src-aabb)
                                  _dst-shape)
  0)

(cl-defmethod edraw-get-xy ((src edraw-point-connection-src-aabb))
  "Return the current coordinates of the connection SRC."
  (with-slots (shape x-ratio y-ratio) src
    (when-let ((aabb (edraw-point-connection-aabb shape)))
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
  (edraw-rect-center (edraw-point-connection-aabb (oref dst shape))))

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
                 ;; Add gap
                 (let ((gap (edraw-gap-distance src (oref dst shape))))
                   (when (and gap (/= gap 0))
                     (setq xy (edraw-xy-add xy (edraw-xy-nmul (/ gap (sqrt len-sq)) dir)))))
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
                                          (conn edraw-point-connection)
                                          &optional update-conn)
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

    (when update-conn
      (edraw-update conn))

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


;;;; Multiple Shapes

(defclass edraw-multiple-shapes (edraw-properties-holder)
  ((shapes :type list :initarg :shapes)
   (editor :type edraw-editor :initarg :editor)
   (change-hook :initform (edraw-hook-make))
   (prop-info-list-cache :initform nil)))

;;;;; Multiple Shapes - Types

(cl-defgeneric edraw-shape-uniform-class (shapes)
  "When SHAPES consists of objects of a single class, return the
 class symbol.

Note that this does not return the common base class.")

(cl-defmethod edraw-shape-uniform-class ((shapes list))
  (when shapes
    (let* ((first-shape (car shapes))
           (first-class (and (eieio-object-p first-shape)
                             (eieio-object-class first-shape))))
      (cl-loop for shape in (cdr shapes)
               unless (and (eieio-object-p shape)
                           (eq (eieio-object-class shape) first-class))
               return nil
               finally return first-class))))

(cl-defmethod edraw-shape-uniform-class ((shape edraw-shape))
  (eieio-object-class shape))

(cl-defmethod edraw-shape-uniform-class ((obj edraw-multiple-shapes))
  (edraw-shape-uniform-class (oref obj shapes)))

;;;;; Multiple Shapes - Properties

(cl-defmethod edraw-get-editor ((shape edraw-multiple-shapes))
  (oref shape editor))

(cl-defmethod edraw-name ((obj edraw-multiple-shapes))
  (format (edraw-msg "%s shapes") (length (oref obj shapes))))

(cl-defmethod edraw-undo-block-begin ((shape edraw-multiple-shapes))
  (edraw-undo-block-begin (oref shape editor)))

(cl-defmethod edraw-undo-block-end ((shape edraw-multiple-shapes) backup)
  (edraw-undo-block-end (oref shape editor) backup))

(cl-defmethod edraw-undo-all ((shape edraw-multiple-shapes))
  (edraw-undo-all (oref shape editor)))

(cl-defmethod edraw-last-undo-data ((obj edraw-multiple-shapes))
  (edraw-last-undo-data (oref obj editor)))

(cl-defmethod edraw-undo ((obj edraw-multiple-shapes))
  (edraw-undo (oref obj editor)))

(cl-defmethod edraw-get-property-info-list ((obj edraw-multiple-shapes))
  (with-slots (prop-info-list-cache) obj
    (unless prop-info-list-cache
      (setq prop-info-list-cache
            (list
             (let ((info-list-list
                    (mapcar #'edraw-get-property-info-list (oref obj shapes))))
               (seq-reduce #'seq-intersection
                           (cdr info-list-list)
                           (car info-list-list))))))
    (car prop-info-list-cache)))

(cl-defmethod edraw-get-property ((obj edraw-multiple-shapes) prop-name)
  (with-slots (shapes) obj
    (when shapes
      (let ((value (edraw-get-property (car shapes) prop-name)))
        (if (seq-some (lambda (s)
                        (not (equal (edraw-get-property s prop-name) value)))
                      (cdr shapes))
            nil;;@todo represent invalid
          value)))))

(cl-defmethod edraw-set-properties ((obj edraw-multiple-shapes) prop-list)
  (edraw-make-undo-group (oref obj editor) 'shape-properties
    (dolist (shape (oref obj shapes))
      (edraw-set-properties shape prop-list))))

(cl-defmethod edraw-add-change-hook ((obj edraw-multiple-shapes)
                                     function &rest args)
  (with-slots (change-hook) obj
    ;; Add hook to SHAPES if the first observer is added
    (when (= (edraw-hook-length change-hook) 0)
      (dolist (shape (oref obj shapes))
        (edraw-add-change-hook shape #'edraw-on-target-changed obj)))
    ;; Add the hook to OBJ
    (apply 'edraw-hook-add change-hook function args)))

(cl-defmethod edraw-remove-change-hook ((obj edraw-multiple-shapes)
                                        function &rest args)
  (with-slots (change-hook) obj
    ;; Remove the hook from OBJ
    (apply 'edraw-hook-remove change-hook function args)
    ;; Remove hooks from SHAPES if no one is observing OBJ
    (when (= (edraw-hook-length change-hook) 0)
      (dolist (shape (oref obj shapes))
        (edraw-remove-change-hook shape #'edraw-on-target-changed obj)))))

(cl-defmethod edraw-on-target-changed ((obj edraw-multiple-shapes)
                                       _source type hint)
  (with-slots (change-hook) obj
    ;; Treat changes to SOURCE as changes to OBJ
    (edraw-hook-call change-hook obj type hint)));;@todo Convert type?

;;(cl-defmethod edraw-property-editor-shape-p (_obj) nil)
;;(cl-defmethod edraw-set-all-properties-as-default ((obj edraw-multiple-shapes)))

;;;;; Multiple Shapes - Selection

(cl-defmethod edraw-selected-all-p ((obj edraw-multiple-shapes))
  "Return non-nil if all the shapes contained in OBJ are selected."
  (seq-every-p #'edraw-selected-p (oref obj shapes)))

(cl-defmethod edraw-contains-selected-p ((obj edraw-multiple-shapes))
  "Return non-nil if OBJ contains one or more selected entities."
  (seq-some #'edraw-selected-p (oref obj shapes)))

(cl-defmethod edraw-matches-selected-p ((obj edraw-multiple-shapes))
  "Return non-nil if the shapes contained in OBJ exactly match
 the set of shapes currently selected by the editor."
  (seq-set-equal-p
   (oref obj shapes)
   (edraw-selected-shapes (oref obj editor))
   #'eq))

(cl-defmethod edraw-select ((obj edraw-multiple-shapes))
  "Select only the shapes contained in OBJ.

Deselect all shapes, then select the shapes contained in OBJ."
  (edraw-select-shapes (oref obj editor) (oref obj shapes)))

(cl-defmethod edraw-deselect ((obj edraw-multiple-shapes))
  "Remove shapes contained in OBJ from the selected shapes."
  (let ((editor (oref obj editor)))
    (dolist (shape (oref obj shapes))
      (edraw-remove-shape-selection editor shape))))

;;;;; Multiple Shapes - Temporary State

(cl-defmethod edraw-clear-temporary-states ((obj edraw-multiple-shapes))
  (dolist (shape (oref obj shapes))
    (edraw-clear-temporary-states shape)))

(cl-defmethod edraw-has-temporary-states-p ((obj edraw-multiple-shapes))
  (seq-some #'edraw-has-temporary-states-p (oref obj shapes)))

(cl-defmethod edraw-visible-p ((obj edraw-multiple-shapes))
  (seq-some #'edraw-visible-p (oref obj shapes)))

(cl-defmethod edraw-pickable-p ((obj edraw-multiple-shapes))
  (seq-some #'edraw-pickable-p (oref obj shapes)))

(cl-defmethod edraw-toggle-visibility ((obj edraw-multiple-shapes))
  (let ((new-value (not (edraw-visible-p obj))))
    (dolist (shape (oref obj shapes))
      (edraw-set-visible shape new-value))))

(cl-defmethod edraw-toggle-pickability ((obj edraw-multiple-shapes))
  (let ((new-value (not (edraw-pickable-p obj))))
    (dolist (shape (oref obj shapes))
      (edraw-set-pickable shape new-value))))

;;;;; Multiple Shapes - AABB

(cl-defmethod edraw-shape-aabb ((obj edraw-multiple-shapes))
  (edraw-shape-aabb (oref obj shapes)))

;;;;; Multiple Shapes - Menu

(cl-defmethod edraw-popup-context-menu ((obj edraw-multiple-shapes))
  (let ((edraw-current-editor (oref obj editor)))
    (edraw-popup-menu nil (edraw-menu-shape obj) obj)))

(cl-defmethod edraw-menu-shape ((obj edraw-multiple-shapes))
  `(,(edraw-name obj) ;;(edraw-get-summary obj)
    ,(edraw-filter-menu-items
      obj 'multiple-shapes
      (edraw-get-actions obj))))

(cl-defmethod edraw-get-actions ((obj edraw-multiple-shapes))
  (edraw-menu-items-shape-common obj))

(cl-defmethod edraw-filter-menu-items ((obj edraw-multiple-shapes) menu-type items)
  (when-let ((editor (edraw-get-editor obj)))
    (edraw-filter-menu-items editor (list menu-type :target obj) items)))

(cl-defmethod edraw-property-editor-actions ((obj edraw-multiple-shapes))
  `((push-button :notify ,(lambda (&rest _)
                            (edraw-popup-context-menu obj))
                 ,(edraw-msg "Actions"))))


;;;;; Multiple Shapes - Transform

(cl-defmethod edraw-translate ((obj edraw-multiple-shapes) &optional xy)
  (edraw-set-translate-params)
  (edraw-make-undo-group (oref obj editor) 'shapes-translate
    (dolist (shape (oref obj shapes))
      (edraw-translate shape xy))))

(cl-defmethod edraw-scale ((obj edraw-multiple-shapes) &optional origin-xy sx sy)
  (edraw-set-scale-params (edraw-shape-aabb obj))

  (edraw-transform
   obj
   (edraw-matrix-move-origin-xy (edraw-matrix-scale sx sy 1) origin-xy)))

(cl-defmethod edraw-rotate ((obj edraw-multiple-shapes) &optional origin-xy angle)
  (edraw-set-rotate-params (edraw-shape-aabb obj))

  (edraw-transform
   obj
   (edraw-matrix-move-origin-xy (edraw-matrix-rotate angle) origin-xy)))

(cl-defmethod edraw-transform ((obj edraw-multiple-shapes) matrix)
  (edraw-make-undo-group (oref obj editor) 'shapes-transform
    (dolist (shape (oref obj shapes))
      (edraw-transform shape matrix))))

;;;;; Multiple Shapes - Group

(cl-defmethod edraw-group ((obj edraw-multiple-shapes))
  (with-slots (editor shapes) obj
    (when shapes ;; Requires one or more shapes
      (edraw-make-undo-group editor 'shapes-group
        (let ((group (edraw-create-shape-default ;;@todo Use -without-default?
                      editor
                      ;; @todo Determined based on parent of shapes?
                      (edraw-svg-body editor)
                      'g)))
          (edraw-shape-group-add-children group (oref obj shapes)))))))

(cl-defmethod edraw-group-p ((obj edraw-multiple-shapes))
  "Return non-nil if OBJ contains one or more group elements."
  (seq-some #'edraw-shape-group-p (oref obj shapes)))

(cl-defmethod edraw-ungroup ((obj edraw-multiple-shapes)
                             &optional apply-transform-p)
  (edraw-make-undo-group (oref obj editor) 'shapes-ungroup
    (dolist (shape (oref obj shapes))
      (when (edraw-shape-group-p shape)
        (edraw-ungroup shape apply-transform-p)))))

(cl-defmethod edraw-ungroup-interactive ((obj edraw-multiple-shapes))
  (let* ((groups (seq-filter #'edraw-shape-group-p (oref obj shapes)))
         (apply-transform-p
          (and (seq-some (lambda (g)
                           (not (edraw-matrix-identity-p
                                 (edraw-transform-prop-get-matrix g))))
                         groups)
               (y-or-n-p
                (edraw-msg "Apply group's transform property to children?")))))
    (unless groups
      (error (edraw-msg "No group selected")))
    (edraw-ungroup obj apply-transform-p)))

;;;;; Multiple Shapes - Z Order

(cl-defmethod edraw-front-p ((obj edraw-multiple-shapes))
  "Return non-nil if all shapes contained in OBJ occupy the last
 part (frontmost part) of each parent."
  (let ((editor (oref obj editor))
        (parent-members-alist nil))
    (dolist (shape (oref obj shapes))
      (push shape (alist-get (edraw-parent shape) parent-members-alist)))
    (not
     (seq-some (lambda (parent-members)
                 (let* ((parent (car parent-members))
                        (members (cdr parent-members))
                        (children (if parent
                                      (edraw-children parent)
                                    ;; Nil means under document root
                                    (edraw-all-shapes editor)))
                        (len-members (length members))
                        (len-children (length children)))
                   (not (and (<= len-members len-children)
                             (seq-set-equal-p
                              members
                              (nthcdr (- len-children len-members) children)
                              #'eq)))))
               parent-members-alist))))

(cl-defmethod edraw-back-p ((obj edraw-multiple-shapes))
  "Return non-nil if all shapes contained in OBJ occupy the first
 part (backmost part) of each parent."
  (let ((editor (oref obj editor))
        (parent-members-alist nil))
    (dolist (shape (oref obj shapes))
      (push shape (alist-get (edraw-parent shape) parent-members-alist)))
    (not
     (seq-some (lambda (parent-members)
                 (let* ((parent (car parent-members))
                        (members (cdr parent-members))
                        (children (if parent
                                      (edraw-children parent)
                                    ;; Nil means under document root
                                    (edraw-all-shapes editor))))
                   (not (seq-set-equal-p
                         members
                         (seq-take children (length members))
                         #'eq))))
               parent-members-alist))))

(cl-defmethod edraw-shapes-back-to-front ((obj edraw-multiple-shapes))
  ;;@todo Is it okay to assume that all shapes have the same parent?
  ;; Or is it okay to use different parents to change the Z order?
  (mapcar #'cdr
          (sort
           (mapcar (lambda (shape)
                     (cons (edraw-node-position shape)
                           shape))
                   (oref obj shapes))
           (lambda (a b) (< (car a) (car b))))))

(cl-defmethod edraw-shapes-front-to-back ((obj edraw-multiple-shapes))
  (nreverse (edraw-shapes-back-to-front obj)))

(cl-defmethod edraw-bring-to-front ((obj edraw-multiple-shapes))
  (edraw-make-undo-group (oref obj editor) 'shapes-bring-to-front
    (dolist (shape (edraw-shapes-back-to-front obj))
      (edraw-bring-to-front shape))))

(cl-defmethod edraw-bring-forward ((obj edraw-multiple-shapes))
  (edraw-make-undo-group (oref obj editor) 'shapes-bring-forward
    (let ((shape-list (edraw-shapes-front-to-back obj)))
      (dolist (shape shape-list)
        (when-let ((next (edraw-next-sibling shape)))
          (unless (memq next shape-list) ;; No overtaking
            (edraw-bring-forward shape)))))))

(cl-defmethod edraw-send-backward ((obj edraw-multiple-shapes))
  (edraw-make-undo-group (oref obj editor) 'shapes-send-backward
    (let ((shape-list (edraw-shapes-back-to-front obj)))
      (dolist (shape shape-list)
        (when-let ((prev (edraw-previous-sibling shape)))
          (unless (memq prev shape-list) ;; No overtaking
            (edraw-send-backward shape)))))))

(cl-defmethod edraw-send-to-back ((obj edraw-multiple-shapes))
  (edraw-make-undo-group (oref obj editor) 'shapes-send-to-back
    (dolist (shape (edraw-shapes-front-to-back obj))
      (edraw-send-to-back shape))))

(cl-defmethod edraw-back-shape ((obj edraw-multiple-shapes))
  (when-let ((shapes (oref obj shapes)))
    (let* ((back-most-shape (car shapes))
           (back-most-index (edraw-node-position back-most-shape)))
      (dolist (shape (cdr shapes))
        (let ((index (edraw-node-position shape)))
          (when (< index back-most-index)
            (setq back-most-shape shape
                  back-most-index index))))
      back-most-shape)))

(cl-defmethod edraw-front-shape ((obj edraw-multiple-shapes))
  (when-let ((shapes (oref obj shapes)))
    (let* ((front-most-shape (car shapes))
           (front-most-index (edraw-node-position front-most-shape)))
      (dolist (shape (cdr shapes))
        (let ((index (edraw-node-position shape)))
          (when (> index front-most-index)
            (setq front-most-shape shape
                  front-most-index index))))
      front-most-shape)))

(cl-defmethod edraw-select-next-shape ((obj edraw-multiple-shapes))
  (when-let* ((front-most-shape (edraw-front-shape obj))
              (sibling (edraw-next-sibling front-most-shape)))
    (edraw-select sibling)))

(cl-defmethod edraw-select-previous-shape ((obj edraw-multiple-shapes))
  (when-let* ((back-most-shape (edraw-back-shape obj))
              (sibling (edraw-previous-sibling back-most-shape)))
    (edraw-select sibling)))

;;;;; Multiple Shapes - Path

(cl-defmethod edraw-combine-paths ((obj edraw-multiple-shapes))
  (edraw-combine-paths (oref obj shapes)))

;;;;; Multiple Shapes - Other Editing Commands

(cl-defmethod edraw-remove ((obj edraw-multiple-shapes))
  (edraw-make-undo-group (oref obj editor) 'shapes-remove
    (dolist (shape (oref obj shapes))
      (edraw-remove shape))))

(cl-defmethod edraw-delete-with-confirm ((obj edraw-multiple-shapes))
  (when (edraw-y-or-n-p (format "%s %s?"
                                (edraw-msg "Delete")
                                (edraw-name obj)))
    (edraw-remove obj)))

(cl-defmethod edraw-duplicate-and-select ((obj edraw-multiple-shapes))
  (edraw-select-shapes (oref obj editor)
                       (edraw-duplicate-shapes (oref obj shapes))))

(cl-defmethod edraw-copy ((obj edraw-multiple-shapes))
  (when-let ((shapes (edraw-shapes-back-to-front obj)))
    (edraw-clipboard-set
     'shape-descriptor-list
     (mapcar #'edraw-shape-descriptor shapes))))

(cl-defmethod edraw-cut ((obj edraw-multiple-shapes))
  (when (oref obj shapes)
    (edraw-copy obj)
    (edraw-deselect obj)
    (edraw-remove obj)))


;;;; Interactive Transform

(defclass edraw-shape-transformer ()
  ((target :initarg :target)
   (ui-svg) ;;SVG Element
   (original-aabb) ;;edraw-rect
   (scaled-aabb) ;;edraw-rect
   (corner-points) ;;list of (ref-x . ref-y)
   (side-points) ;;list of (ref-x . ref-y)
   (scaling-points) ;;list of (ref-x . ref-y)
   (origin-point) ;;(ref-x . ref-y)
   (rotation-angle :type number :initform 0) ;;degrees
   (translation-delta :initform (edraw-xy 0 0)) ;;edraw-xy
   (pointer-xy :initform nil)
   (pointer-type :initform nil)
   (pointer-angle :initform 0)
   ))

(defun edraw-shape-transformer-create (target)
  "Create a new edraw-shape-transformer object."
  (let ((transformer (edraw-shape-transformer :target target)))
    (with-slots (ui-svg
                 original-aabb scaled-aabb
                 corner-points side-points scaling-points
                 origin-point)
        transformer
      (setq original-aabb (edraw-shape-aabb target)
            scaled-aabb (edraw-rect-clone original-aabb)
            ;; NOTE: This part corresponds to the edraw-scaling-point-dir function.
            corner-points (list
                           (cons (car scaled-aabb) (car scaled-aabb)) ;;L-T
                           (cons (cdr scaled-aabb) (car scaled-aabb)) ;;R-T
                           (cons (cdr scaled-aabb) (cdr scaled-aabb)) ;;R-B
                           (cons (car scaled-aabb) (cdr scaled-aabb))) ;;L-B
            side-points (list
                         (cons (car scaled-aabb) (list 'center)) ;;Left
                         (cons (cdr scaled-aabb) (list 'center)) ;;Right
                         (cons (list 'center) (car scaled-aabb)) ;;Top
                         (cons (list 'center) (cdr scaled-aabb))) ;;Bottom
            scaling-points (append corner-points side-points)
            origin-point (cons (cons 'ratio 0.5) (cons 'ratio 0.5))
            ui-svg (edraw-svg-group
                    :parent (edraw-ui-foreground-svg (edraw-get-editor target))
                    :class "edraw-ui-transform"))
      transformer)))

(cl-defmethod edraw-scaling-point-dir ((transformer edraw-shape-transformer)
                                       ref)
  "Returns the position of REF in scaled-aabb.
REF is a point reference in scaling-points."
  (with-slots (scaled-aabb) transformer
    (cons
     (cond
      ((eq (car scaled-aabb) (car ref)) 'left)
      ((eq (cdr scaled-aabb) (car ref)) 'right)
      (t 'center))
     (cond
      ((eq (car scaled-aabb) (cdr ref)) 'top)
      ((eq (cdr scaled-aabb) (cdr ref)) 'bottom)
      (t 'center)))))

(cl-defmethod edraw-get-scaling-point-by-dir ((transformer
                                               edraw-shape-transformer)
                                              dir-x dir-y)
  (let ((dir (cons dir-x dir-y)))
    (seq-find (lambda (ref) (equal (edraw-scaling-point-dir transformer ref)
                                   dir))
              (oref transformer scaling-points))))

(cl-defmethod edraw-get-opposite-scaling-point-vectors
  ((transformer edraw-shape-transformer) ref)
  (let ((ref-xy (edraw-ref-xy-on-global transformer ref))
        (ref-dir (edraw-scaling-point-dir transformer ref)))
    (cons
     (edraw-xy-sub
      (edraw-ref-xy-on-global
       transformer
       (edraw-get-scaling-point-by-dir
        transformer
        (pcase (car ref-dir) ('left 'right) ('right 'left) (x x))
        (cdr ref-dir)))
      ref-xy)
     (edraw-xy-sub
      (edraw-ref-xy-on-global
       transformer
       (edraw-get-scaling-point-by-dir
        transformer
        (car ref-dir)
        (pcase (cdr ref-dir) ('top 'bottom) ('bottom 'top) (y y))))
      ref-xy))))

(cl-defmethod edraw-get-editor ((transformer edraw-shape-transformer))
  (edraw-get-editor (oref transformer target)))

;;;;; Interactive Transform - Point Reference

(cl-defmethod edraw-ref-xy ((transformer edraw-shape-transformer) ref)
  "Return coordinates referenced by REF."
  (with-slots (scaled-aabb) transformer
    (let ((ref-x (car ref))
          (ref-y (cdr ref)))
      (edraw-xy
       (pcase ref-x
         (`(center) (edraw-rect-cx scaled-aabb))
         (`(ratio . ,ratio) (+ (edraw-rect-left scaled-aabb)
                               (* ratio (edraw-rect-width scaled-aabb))))
         (`(global . ,xy)
          (edraw-x
           (edraw-matrix-mul-mat-xy
            (edraw-get-inverse-transform-matrix-without-scaling transformer)
            xy)))
         (_ (edraw-x ref-x)))
       (pcase ref-y
         (`(center) (edraw-rect-cy scaled-aabb))
         (`(ratio . ,ratio) (+ (edraw-rect-top scaled-aabb)
                               (* ratio (edraw-rect-height scaled-aabb))))
         (`(global . ,xy)
          (edraw-y
           (edraw-matrix-mul-mat-xy
            (edraw-get-inverse-transform-matrix-without-scaling transformer)
            xy)))
         (_ (edraw-y ref-y)))))))

(cl-defmethod edraw-ref-xy-set ((transformer edraw-shape-transformer)
                                ref
                                scaled-xy
                                global-xy)
  "Set coordinates referenced by REF to XY."
  (with-slots (scaled-aabb) transformer
    (let ((ref-x (car ref))
          (ref-y (cdr ref)))
      (pcase (car-safe ref-x)
        ('center) ;;Do not change
        ('ratio (setf (cdr ref-x) (if (/= (edraw-rect-width scaled-aabb) 0)
                                      (/ (- (edraw-x scaled-xy)
                                            (edraw-rect-left scaled-aabb))
                                         (edraw-rect-width scaled-aabb))
                                    0.5)))
        ('global (setf (cdr ref-x) (edraw-x global-xy)))
        (_ (setf (edraw-x ref-x) (edraw-x scaled-xy))))
      (pcase (car-safe ref-y)
        ('center) ;;Do not change
        ('ratio (setf (cdr ref-y) (if (/= (edraw-rect-height scaled-aabb) 0)
                                      (/ (- (edraw-y scaled-xy)
                                            (edraw-rect-top scaled-aabb))
                                         (edraw-rect-height scaled-aabb))
                                    0.5)))
        ('global (setf (cdr ref-y) (edraw-y global-xy)))
        (_ (setf (edraw-y ref-y) (edraw-y scaled-xy)))))))

(cl-defmethod edraw-ref-xy-transformed ((transformer edraw-shape-transformer)
                                        ref matrix)
  "Return coordinates referenced by REF converted by MATRIX."
  (edraw-matrix-mul-mat-xy
   matrix
   (edraw-ref-xy transformer ref)))

(cl-defmethod edraw-ref-xy-on-scaled ((transformer edraw-shape-transformer) ref)
  (edraw-ref-xy transformer ref))

(cl-defmethod edraw-ref-xy-on-global ((transformer edraw-shape-transformer) ref)
  (edraw-ref-xy-transformed
   transformer
   ref
   (edraw-get-transform-matrix-without-scaling transformer)))

(cl-defmethod edraw-ref-xy-on-view ((transformer edraw-shape-transformer) ref)
  (edraw-ref-xy-transformed
   transformer
   ref
   (edraw-get-view-matrix-without-scaling transformer)))

;;;;; Interactive Transform - Transform Matrix

;; Transform Order:
;; original-aabb
;; => scale (and translate) => scaled-aabb, corner-points, side-points
;; => rotate (rotation-angle)
;; => translate (translation-delta)
;; => view (scroll and zoom) transform

(cl-defmethod edraw-get-transform-matrix-without-scaling ((transformer edraw-shape-transformer))
  (with-slots (rotation-angle translation-delta) transformer
    (edraw-matrix-mul-mat-mat
     (edraw-matrix-translate-xy translation-delta)
     (edraw-matrix-rotate rotation-angle))))

(cl-defmethod edraw-get-transform-matrix ((transformer edraw-shape-transformer))
  (with-slots (original-aabb scaled-aabb) transformer
    (edraw-matrix-mul-mat-mat
     (edraw-get-transform-matrix-without-scaling transformer)
     (edraw-matrix-fit-rect-to-rect original-aabb scaled-aabb))))

(cl-defmethod edraw-get-view-matrix-without-scaling ((transformer edraw-shape-transformer))
  (edraw-matrix-mul-mat-mat
   (edraw-scroll-transform-matrix (edraw-get-editor transformer))
   (edraw-get-transform-matrix-without-scaling transformer)))

(cl-defmethod edraw-get-view-matrix ((transformer edraw-shape-transformer))
  (edraw-matrix-mul-mat-mat
   (edraw-scroll-transform-matrix (edraw-get-editor transformer))
   (edraw-get-transform-matrix transformer)))

(cl-defmethod edraw-get-inverse-transform-matrix-without-scaling ((transformer edraw-shape-transformer))
  (edraw-matrix-inverse
   (edraw-get-transform-matrix-without-scaling transformer)))

;;;;; Interactive Transform - UI SVG

(defconst edraw-transform-origin-radius 5)
(defconst edraw-transform-origin-input-radius 8)
(defconst edraw-transform-origin-outer-radius 10)

(defun edraw-svg-ui-transform-origin (parent xy)
  (let* ((ox (edraw-x xy))
         (oy (edraw-y xy))
         (or edraw-transform-origin-outer-radius))
    (edraw-svg-group
     :parent parent
     :class "edraw-ui-transform-origin"
     :children
     (list
      (edraw-svg-circle ox oy edraw-transform-origin-radius)
      (edraw-svg-line (- ox or) oy (+ ox or) oy)
      (edraw-svg-line ox (- oy or) ox (+ oy or))))))

(defconst edraw-transform-handle-radius edraw-anchor-point-radius)
(defconst edraw-transform-handle-input-radius (+ 3 edraw-transform-handle-radius))

(defun edraw-svg-ui-transform-handle (parent xy)
  (let ((r edraw-transform-handle-radius))
    (edraw-svg-rect (- (car xy) r) (- (cdr xy) r) (* 2 r) (* 2 r)
                    :parent parent
                    :class "edraw-ui-transform-handle")))

(cl-defmethod edraw-update-ui-svg ((transformer edraw-shape-transformer))
  (with-slots (ui-svg corner-points scaling-points origin-point) transformer
    (edraw-dom-remove-all-children ui-svg)

    ;; Origin
    (edraw-svg-ui-transform-origin
     ui-svg
     (edraw-ref-xy-on-view transformer origin-point))

    ;; Corners and midpoint of sides
    (dolist (ref scaling-points)
      (let ((xy (edraw-ref-xy-on-view transformer ref)))
        (edraw-svg-ui-transform-handle ui-svg xy)))

    ;; Boundary Box
    (edraw-svg-polygon (mapcar
                        (lambda (ref)
                          (edraw-ref-xy-on-view transformer ref))
                        corner-points)
                       :parent ui-svg
                       :class "edraw-ui-transform-boundary"))

  ;; Mouse Pointer
  (edraw-update-ui-pointer transformer))

(cl-defmethod edraw-remove-ui-svg ((transformer edraw-shape-transformer))
  (with-slots (ui-svg) transformer
    (edraw-dom-remove-node (edraw-ui-foreground-svg (edraw-get-editor
                                                     transformer))
                           ui-svg)))

(cl-defmethod edraw-update-ui-pointer ((transformer edraw-shape-transformer))
  (with-slots (ui-svg pointer-xy pointer-type pointer-angle) transformer
    (when (and pointer-xy pointer-type)

      (let ((xy
             (edraw-matrix-mul-mat-xy
              (edraw-scroll-transform-matrix (edraw-get-editor transformer))
              pointer-xy)))
        (edraw-svg-ui-transform-pointer
         ui-svg
         "edraw-ui-transform-pointer"
         (edraw-x xy) (edraw-y xy) pointer-type pointer-angle)))))

;;;;; Interactive Transform - Pseudo Mouse Pointer Shape

(defconst edraw-transform-pointer-size 20)

(defun edraw-svg-ui-transform-pointer (parent id x y type angle)
  (let* ((orig-size 440)
         (d (pcase type
              ('rotate "M320,20L220,120L300,120C300,220 220,300 120,300L120,220L20,320L120,420L120,340C240,340 340,240 340,120L420,120L320,20Z") ;;already rotated 45 degrees
              ('scale "M440,220L340,120L340,200L100,200L100,120L0,220L100,320L100,240L340,240L340,320L440,220Z")
              ('translate "M440,220L360,140L360,200L240,200L240,80L300,80L220,0L140,80L200,80L200,200L80,200L80,140L0,220L80,300L80,240L200,240L200,360L140,360L220,440L300,360L240,360L240,240L360,240L360,300L440,220Z")))
         (angle-base (if (eq type 'rotate) 45 0))
         (transform
          (format
           "translate(%s,%s) scale(%s) rotate(%s) translate(%s,%s)"
           x y
           (/ (float edraw-transform-pointer-size) orig-size)
           (- angle angle-base)
           (- (/ orig-size 2)) (- (/ orig-size 2)))))

    (when d
      (edraw-svg-path d
                      :parent parent
                      :id id
                      :fill "#000000" :stroke-width "1" :stroke "#ffffff"
                      :transform transform))))

;;;;; Interactive Transform - Get Transform Parameters

(cl-defmethod edraw-scale-x ((transformer edraw-shape-transformer))
  (let ((dst-size (edraw-rect-width (oref transformer scaled-aabb)))
        (src-size (edraw-rect-width (oref transformer original-aabb))))
    (if (= src-size 0)
        1.0
      (/ dst-size src-size))))

(cl-defmethod edraw-scale-y ((transformer edraw-shape-transformer))
  (let ((dst-size (edraw-rect-height (oref transformer scaled-aabb)))
        (src-size (edraw-rect-height (oref transformer original-aabb))))
    (if (= src-size 0)
        1.0
      (/ dst-size src-size))))

(cl-defmethod edraw-rotation-angle ((transformer edraw-shape-transformer))
  (oref transformer rotation-angle))

(cl-defmethod edraw-origin-x-string ((transformer edraw-shape-transformer))
  (let ((ref-x (car (oref transformer origin-point))))
    (pcase (car-safe ref-x)
      ('global (format "%.2f" (car (cdr ref-x))))
      ('ratio (format "%.2f%%" (* 100 (cdr ref-x))))
      ('center "center")
      (_ (format "(%.2f)" (car ref-x))))))

(cl-defmethod edraw-origin-y-string ((transformer edraw-shape-transformer))
  (let ((ref-y (car (oref transformer origin-point))))
    (pcase (car-safe ref-y)
      ('global (format "%.2f" (cdr (cdr ref-y))))
      ('ratio (format "%.2f%%" (* 100 (cdr ref-y))))
      ('center "center")
      (_ (format "(%.2f)" (cdr ref-y))))))

;;;;; Interactive Transform - Set Transform Parameters

(cl-defmethod edraw-on-transform-change ((transformer edraw-shape-transformer))
  (edraw-invalidate-image (edraw-get-editor transformer))
  (edraw-undo-all (edraw-get-editor transformer)) ;; Cancel previous changes
  (edraw-transform (oref transformer target)
                   (edraw-get-transform-matrix transformer)))

(cl-defmethod edraw-scale-interactive ((transformer edraw-shape-transformer))
  (let* ((scaled-w (edraw-rect-width (oref transformer scaled-aabb)))
         (scaled-h (edraw-rect-height (oref transformer scaled-aabb)))
         (sx (edraw-read-number-with-unit
              (edraw-msg "Scale X [px|%]: ") 1.0
              `(("%" . ,(lambda (n _def) (/ n 100.0)))
                ("px" . ,(lambda (n _def)
                           (if (zerop scaled-w)
                               1.0
                             (/ n (float scaled-w))))))))
         (sy (edraw-read-number-with-unit
              (edraw-msg "Scale Y [px|%]: ") sx
              `(("%" . ,(lambda (n _def) (/ n 100.0)))
                ("px" . ,(lambda (n _def)
                           (if (zerop scaled-h)
                               1.0
                             (/ n (float scaled-h)))))))))
    (edraw-scale transformer nil sx sy)))

(cl-defmethod edraw-scale ((transformer edraw-shape-transformer)
                           &optional origin-xy sx sy)
  (with-slots (scaled-aabb origin-point) transformer
    (unless origin-xy
      (setq origin-xy (edraw-ref-xy-on-scaled transformer origin-point)))
    (unless sx (setq sx 1))
    (unless sy (setq sy 1))

    (edraw-rect-scale scaled-aabb sx sy (edraw-x origin-xy) (edraw-y origin-xy))
    (edraw-on-transform-change transformer)))

(cl-defmethod edraw-set-scale-interactive ((transformer edraw-shape-transformer))
  (let* ((original-w (edraw-rect-width (oref transformer original-aabb)))
         (original-h (edraw-rect-height (oref transformer original-aabb)))
         (sx (edraw-read-number-with-unit
              (edraw-msg "Scale X [px|%]: ") (edraw-scale-x transformer)
              `(("%" . ,(lambda (n _def) (/ n 100.0)))
                ("px" . ,(lambda (n _def)
                           (if (zerop original-w)
                               1.0
                             (/ n (float original-w))))))))
         (sy (edraw-read-number-with-unit
              (edraw-msg "Scale Y [px|%]: ") (edraw-scale-y transformer)
              `(("%" . ,(lambda (n _def) (/ n 100.0)))
                ("px" . ,(lambda (n _def)
                           (if (zerop original-h)
                               1.0
                             (/ n (float original-h)))))))))
    (edraw-set-scale transformer nil sx sy)))

(cl-defmethod edraw-set-scale ((transformer edraw-shape-transformer)
                               &optional origin-xy sx sy)
  (with-slots (original-aabb scaled-aabb origin-point) transformer
    (unless origin-xy
      (setq origin-xy (edraw-ref-xy-on-scaled transformer origin-point)))
    (unless sx (setq sx 1))
    (unless sy (setq sy 1))

    (setq sx (/ sx (edraw-scale-x transformer)))
    (setq sy (/ sy (edraw-scale-y transformer)))

    (edraw-rect-scale scaled-aabb sx sy (edraw-x origin-xy) (edraw-y origin-xy))
    (edraw-on-transform-change transformer)))

(cl-defmethod edraw-rotate-interactive ((transformer edraw-shape-transformer))
  (edraw-rotate transformer
                nil
                (read-number (edraw-msg "Angle: ") 0)))

(cl-defmethod edraw-rotate ((transformer edraw-shape-transformer)
                            &optional origin-xy angle)
  (unless origin-xy
    (setq origin-xy
          (edraw-ref-xy-on-global transformer (oref transformer origin-point))))
  (unless angle
    (setq angle 0))

  (let ((old-rotation-angle (oref transformer rotation-angle))
        (old-translation-delta (oref transformer translation-delta)))
    (oset transformer rotation-angle
          (+ old-rotation-angle
             angle))
    (oset transformer translation-delta
          (edraw-xy-add origin-xy
                        (edraw-xy-rotate (edraw-xy-add (edraw-xy-neg
                                                        origin-xy)
                                                       old-translation-delta)
                                         angle))))
  (edraw-on-transform-change transformer))

(cl-defmethod edraw-set-rotation-angle-interactive ((transformer edraw-shape-transformer))
  (edraw-set-rotation-angle transformer
                            nil
                            (read-number (edraw-msg "Angle: ")
                                         (edraw-rotation-angle transformer))))

(cl-defmethod edraw-set-rotation-angle ((transformer edraw-shape-transformer)
                                        &optional origin-xy angle)
  (unless origin-xy
    (setq origin-xy
          (edraw-ref-xy-on-global transformer (oref transformer origin-point))))
  (unless angle
    (setq angle 0))

  (let* ((old-rotation-angle (oref transformer rotation-angle))
         (old-translation-delta (oref transformer translation-delta))
         (angle-delta (- angle old-rotation-angle)))
    (oset transformer rotation-angle angle)
    (oset transformer translation-delta
          (edraw-xy-add origin-xy
                        (edraw-xy-rotate (edraw-xy-add (edraw-xy-neg
                                                        origin-xy)
                                                       old-translation-delta)
                                         angle-delta))))
  (edraw-on-transform-change transformer))

(cl-defmethod edraw-translate-interactive ((transformer
                                            edraw-shape-transformer))
  (edraw-translate transformer
                   (edraw-xy
                    (read-number (edraw-msg "Delta X: ") 0)
                    (read-number (edraw-msg "Delta Y: ") 0))))

(cl-defmethod edraw-translate ((transformer edraw-shape-transformer) xy)
  (with-slots (translation-delta) transformer
    (setq translation-delta
          (edraw-xy-add translation-delta xy)))
  (edraw-on-transform-change transformer))

(cl-defmethod edraw-set-transform-origin-interactive ((transformer
                                                       edraw-shape-transformer))
  (let (ox oy)
    (while (null ox)
      (let ((input (read-string (edraw-msg "Origin X(left, center, right, <percentage>%, or <coordinate>): "))))
        (setq ox
              (cond
               ((string= input "left") (list 0.0))
               ((string= input "center") (list 0.5))
               ((string= input "right") (list 1.0))
               ((string-match "\\`\\s-*\\([-+]?[0-9]+\\(?:\\.[0-9]*\\)?\\)\\(%?\\)\\s-*\\'" input)
                (if (equal (match-string 2 input) "%")
                    (list (/ (string-to-number (match-string 1 input)) 100.0))
                  (string-to-number (match-string 1 input))))))))
    (while (null oy)
      (let ((input (read-string (edraw-msg "Origin Y(top, center, bottom, <percentage>%, or <coordinate>): "))))
        (setq oy
              (cond
               ((string= input "top") (list 0.0))
               ((string= input "center") (list 0.5))
               ((string= input "bottom") (list 1.0))
               ((string-match "\\`\\s-*\\([-+]?[0-9]+\\(?:\\.[0-9]*\\)?\\)\\(%?\\)\\s-*\\'" input)
                (if (equal (match-string 2 input) "%")
                    (list (/ (string-to-number (match-string 1 input))
                             100.0))
                  (string-to-number (match-string 1 input))))))))
    (edraw-set-transform-origin transformer ox oy)))

(cl-defmethod edraw-set-transform-origin ((transformer edraw-shape-transformer)
                                          ox oy)
  (with-slots (origin-point scaled-aabb) transformer
    (let ((oxy (edraw-xy (if (numberp ox) ox 0) (if (numberp oy) oy 0))))
      (setcar origin-point
              (if (numberp ox) (cons 'global oxy)
                (cons 'ratio (car ox))))
      (setcdr origin-point
              (if (numberp oy) (cons 'global oxy)
                (cons 'ratio (car oy)))))
    (edraw-invalidate-image (edraw-get-editor transformer))))

;;;;; Interactive Transform - Mouse Event Handler

(cl-defmethod edraw-on-mouse-down ((transformer edraw-shape-transformer)
                                   down-event)
  (or
   (edraw-set-transform-origin-by-dragging transformer down-event)
   (edraw-scale-by-dragging transformer down-event)
   (edraw-translate-by-dragging transformer down-event)
   (edraw-rotate-by-dragging transformer down-event)))

(cl-defmethod edraw-find-point ((transformer edraw-shape-transformer)
                                down-event
                                points
                                radius
                                dist-fun)
  (let* ((editor (edraw-get-editor transformer))
         (down-xy (edraw-mouse-event-to-xy-raw editor down-event))
         (input-radius (/ radius
                          (float (edraw-scroll-scale editor))))
         (ref (seq-find (lambda (ref)
                          (<=
                           (funcall (or dist-fun #'edraw-xy-distance-l-inf) ;;square
                                    (edraw-ref-xy-on-global transformer ref)
                                    down-xy)
                           input-radius))
                        points)))
    ref))

(cl-defmethod edraw-find-scaling-point ((transformer edraw-shape-transformer)
                                        down-event)
  (edraw-find-point transformer down-event
                    (oref transformer scaling-points)
                    edraw-transform-handle-input-radius
                    #'edraw-xy-distance-l-inf))

(cl-defmethod edraw-scale-by-dragging ((transformer edraw-shape-transformer)
                                       down-event)
  (let* ((editor (edraw-get-editor transformer))
         (ref (edraw-find-scaling-point transformer down-event)))
    (when ref
      (let* ((old-xy (edraw-ref-xy-on-global transformer ref))
             (matrix-inv (edraw-get-inverse-transform-matrix-without-scaling transformer))
             (shift-p (memq 'shift (event-modifiers down-event)))
             (opposite-vecs (when shift-p
                              (edraw-get-opposite-scaling-point-vectors
                               transformer ref))))
        ;;(message "old-xy=%s opposite-vecs=%s" old-xy opposite-vecs)
        (edraw-track-dragging
         down-event
         (lambda (move-event)
           (edraw-on-mouse-move transformer move-event 'scale ref)
           (let ((move-xy (edraw-mouse-event-to-xy-snapped editor move-event)))
             (when shift-p
               (setq move-xy
                     (if opposite-vecs
                         (edraw-xy-snap-to-rect-diagonal move-xy old-xy
                                                         (car opposite-vecs)
                                                         (cdr opposite-vecs))
                       (edraw-xy-snap-to-45deg move-xy old-xy))))
             (edraw-ref-xy-set
              transformer
              ref
              (edraw-matrix-mul-mat-xy matrix-inv move-xy)
              move-xy)
             (edraw-on-transform-change transformer)
             (message (edraw-msg "Scale %.2f%% %.2f%%")
                      (* 100.0 (edraw-scale-x transformer))
                      (* 100.0 (edraw-scale-y transformer)))
             ))))
      t)))

(cl-defmethod edraw-find-origin-point ((transformer edraw-shape-transformer)
                                       down-event)
  (edraw-find-point transformer down-event
                    (list (oref transformer origin-point))
                    edraw-transform-origin-input-radius
                    #'edraw-xy-distance))

(defconst edraw-transform-origin-snap-radius
  edraw-transform-origin-input-radius
  "Max snap distance.")

(cl-defmethod edraw-set-transform-origin-by-dragging ((transformer edraw-shape-transformer)
                                                      down-event)
  (let* ((editor (edraw-get-editor transformer))
         (ref (edraw-find-origin-point transformer down-event)))
    (when ref
      (let ((old-xy (edraw-ref-xy-on-scaled transformer ref))
            (shift-p (memq 'shift (event-modifiers down-event)))
            (snap-radius (/ edraw-transform-origin-snap-radius
                            (float (edraw-scroll-scale editor)))))
        (edraw-track-dragging
         down-event
         (lambda (move-event)
           (edraw-on-mouse-move transformer move-event 'origin)
           (let ((move-xy (edraw-mouse-event-to-xy-raw editor move-event)))
             (when shift-p
               (setq move-xy (edraw-xy-snap-to-45deg move-xy old-xy)))

             (if-let ((ratio-xy
                       (seq-find (lambda (ratio-xy)
                                   (< (edraw-xy-distance
                                       (edraw-ref-xy-on-global
                                        transformer
                                        (cons (cons 'ratio (car ratio-xy))
                                              (cons 'ratio (cdr ratio-xy))))
                                       move-xy)
                                      snap-radius))
                                 '((0.0 . 0.0) (0.5 . 0.0) (1.0 . 0.0)
                                   (0.0 . 0.5) (0.5 . 0.5) (1.0 . 0.5)
                                   (0.0 . 1.0) (0.5 . 1.0) (1.0 . 1.0)))))
                 ;; Relative
                 (edraw-set-transform-origin transformer
                                             (list (car ratio-xy))
                                             (list (cdr ratio-xy)))
               ;; Absolute
               (let ((move-xy-snapped (edraw-snap-xy editor move-xy)))
                 (edraw-set-transform-origin transformer
                                             (edraw-x move-xy-snapped)
                                             (edraw-y move-xy-snapped)))))))
        t))))

(cl-defmethod edraw-contains-point-p ((transformer edraw-shape-transformer)
                                      down-xy)
  (edraw-rect-contains-point-p
   (oref transformer scaled-aabb)
   (edraw-matrix-mul-mat-xy
    (edraw-get-inverse-transform-matrix-without-scaling transformer)
    down-xy)))

(cl-defmethod edraw-translate-by-dragging ((transformer edraw-shape-transformer)
                                           down-event)
  (let* ((editor (edraw-get-editor transformer))
         (down-xy (edraw-mouse-event-to-xy-raw editor down-event)))
    (when (edraw-contains-point-p transformer down-xy)
      (let ((down-xy-snapped (edraw-snap-xy editor down-xy))
            (old-translation-delta (oref transformer translation-delta))
            (shift-p (memq 'shift (event-modifiers down-event))))
        (edraw-track-dragging
         down-event
         (lambda (move-event)
           (edraw-on-mouse-move transformer move-event 'translate)
           (let* ((move-xy (edraw-mouse-event-to-xy-snapped editor move-event)))
             (when shift-p
               (setq move-xy (edraw-xy-snap-to-45deg move-xy down-xy-snapped)))
             (oset transformer translation-delta
                   (edraw-xy-add old-translation-delta
                                 (edraw-xy-sub move-xy down-xy-snapped)))
             (edraw-on-transform-change transformer)))))
      t)))

(cl-defmethod edraw-rotate-by-dragging ((transformer edraw-shape-transformer)
                                        down-event)
  (let* ((editor (edraw-get-editor transformer))
         (down-xy (edraw-mouse-event-to-xy-raw editor down-event))
         (origin-xy (edraw-ref-xy-on-global transformer
                                            (oref transformer origin-point)))
         (down-angle (radians-to-degrees
                      (edraw-xy-atan (edraw-xy-sub down-xy origin-xy))))
         (old-rotation-angle (oref transformer rotation-angle))
         (old-translation-delta (oref transformer translation-delta))
         (shift-p (memq 'shift (event-modifiers down-event))))
    (edraw-track-dragging
     down-event
     (lambda (move-event)
       (edraw-on-mouse-move transformer move-event 'rotate)
       (let* ((move-xy (edraw-mouse-event-to-xy-raw editor move-event))
              (move-angle (radians-to-degrees
                           (edraw-xy-atan (edraw-xy-sub move-xy origin-xy))))
              (angle-delta (- move-angle down-angle))
              ;; If shift is pressed, rotate by 15 degree increments.
              (angle-delta (if shift-p
                               (edraw-grid-round angle-delta 15)
                             angle-delta))
              (new-rotation-angle (+ old-rotation-angle angle-delta)))
         (message (edraw-msg "Rotate %.2fdeg") new-rotation-angle)
         (oset transformer rotation-angle new-rotation-angle)
         (oset transformer translation-delta
               (edraw-xy-add origin-xy
                             (edraw-xy-rotate (edraw-xy-add (edraw-xy-neg
                                                             origin-xy)
                                                            old-translation-delta)
                                              angle-delta)))
         (edraw-on-transform-change transformer))))
    t))

;;;;; Interactive Transform - Mouse Event Handler (Motion)

(cl-defmethod edraw-on-mouse-move ((transformer edraw-shape-transformer)
                                   move-event
                                   &optional type-spec dragging-ref)
  (let* ((editor (edraw-get-editor transformer))
         (ov (edraw-overlay editor))
         (posn (event-start move-event))
         (window (posn-window posn))
         (pos-or-area (nth 1 posn))

         (inside-p (and window (windowp window)
                        (eq (window-buffer window) (overlay-buffer ov))
                        (integerp pos-or-area)
                        (>= pos-or-area (overlay-start ov))
                        (< pos-or-area (overlay-end ov))))
         (move-xy (and inside-p
                       (edraw-mouse-event-to-xy-raw editor move-event)))

         (scale-ref nil)
         (type
          (and inside-p
               (cond
                (type-spec
                 (when (eq type-spec 'scale) (setq scale-ref dragging-ref))
                 type-spec)
                ((setq scale-ref
                       (edraw-find-scaling-point transformer move-event))
                 'scale)
                ((edraw-find-origin-point transformer move-event) 'origin)
                ((edraw-contains-point-p transformer move-xy) 'translate)
                (t 'rotate))))

         (angle
          (pcase type
            ('rotate
             ;; Direction from origin to pointer
             (radians-to-degrees
              (edraw-xy-atan
               (edraw-xy-sub
                move-xy
                (edraw-ref-xy-on-global transformer
                                        (oref transformer origin-point))))))
            ('scale
             (+
              (pcase (edraw-scaling-point-dir transformer scale-ref)
                (`(left . top) 45)
                (`(right . top) -45)
                (`(left . bottom) -45)
                (`(right . bottom) 45)
                (`(center . top) 90)
                (`(center . bottom) 90)
                (`(left . center) 0)
                (`(right . center) 0)
                (_ 0))
              (oref transformer rotation-angle)))
            (_ 0))))

    ;;(message "inside-p=%s pos-or-area=%s text-pos=%s" inside-p pos-or-area (nth 5 posn))

    (with-slots (pointer-xy pointer-type pointer-angle) transformer
      (unless (and (eq type pointer-type)
                   (equal move-xy pointer-xy)
                   (equal angle pointer-angle))
        (setq pointer-type type
              pointer-xy move-xy
              pointer-angle angle)
        (edraw-invalidate-image editor)))))

;;;;; Interactive Transform - Command

(defun edraw-transform-interactive (target)
  (let* ((transformer (edraw-shape-transformer-create target))
         (editor (edraw-get-editor target))
         result)

    ;; Hide selection UI and show transformation UI
    (oset editor selection-ui-visible nil)
    (edraw-invalidate-ui-parts editor 'selection-ui)
    (edraw-add-hook editor 'before-image-update
                    'edraw-update-ui-svg transformer)
    (edraw-invalidate-image editor)

    ;;@todo Implement using transient keymap or editing tool. However,
    ;;it is bad if it is transformed from another command before
    ;;committing the transformation.
    (edraw-editor-with-temp-modifications editor
      (unwind-protect
          (track-mouse
            (setq track-mouse 'dragging)
            (while (null result)
              (let ((event (read-event
                            (format (edraw-msg "q/R-Click:Cancel, RET/Dbl-Click:Commit,\ns:Scale(%.2f%% %.2f%%), r:Rotate(%.2fdeg), t:Translate,\no:Origin(%s %s), m:Transform Method(%s)")
                                    (* 100 (edraw-scale-x transformer))
                                    (* 100 (edraw-scale-y transformer))
                                    (edraw-rotation-angle transformer)
                                    (edraw-origin-x-string transformer)
                                    (edraw-origin-y-string transformer)
                                    (edraw-get-transform-method editor)
                                    ))))
                (cond
                 ;; Prefix C-c
                 ((eq event ?\C-c)
                  (pcase (read-event "C-c-")
                    ;; Cancel
                    (?\C-k (setq result 'cancel))
                    ;; Commit
                    (?\C-c (setq result 'ok))))
                 ;; Mouse Down
                 ((memq (car-safe event) '(S-down-mouse-1 down-mouse-1))
                  (edraw-on-mouse-down transformer event))
                 ;; Mouse Move
                 ((mouse-movement-p event)
                  (edraw-on-mouse-move transformer event))
                 ;; Cancel
                 ((or (eq (car-safe event) 'mouse-3)
                      (eq event ?q))
                  (setq result 'cancel))
                 ;; Commit
                 ((or (eq (car-safe event) 'double-down-mouse-1)
                      (eq event ?\C-m)
                      (eq event ?\C-j)
                      (eq event 'return))
                  (setq result 'ok))
                 ;; Set Transform Parameters
                 ((eq event ?s)
                  (edraw-set-scale-interactive transformer))
                 ((eq event ?S)
                  (edraw-scale-interactive transformer))
                 ((eq event ?r)
                  (edraw-set-rotation-angle-interactive transformer))
                 ((eq event ?R)
                  (edraw-rotate-interactive transformer))
                 ((eq event ?t)
                  (edraw-translate-interactive transformer))
                 ((eq event ?o)
                  (edraw-set-transform-origin-interactive transformer))
                 ((eq event ?m)
                  (when-let ((method
                              (pcase (read-char (edraw-msg "a:auto  t:transform property  p:anchor points"))
                                (?a 'auto)
                                (?t 'transform-property)
                                (?p 'anchor-points))))
                    (edraw-set-transform-method editor method)))
                 ;; ((memq (event-basic-type event) '(left up right down))
                 ;;  @todo translate by key
                 ;;  )
                 ;; Scroll
                 ((eq (car-safe event) 'down-mouse-2)
                  (edraw-editor-scroll-by-dragging event))
                 ((or (eq (car-safe event) mouse-wheel-down-event)
                      (eq (car-safe event) (intern (concat "C-" (symbol-name mouse-wheel-down-event)))))
                  (edraw-zoom-in editor))
                 ((or (eq (car-safe event) mouse-wheel-up-event)
                      (eq (car-safe event) (intern (concat "C-" (symbol-name mouse-wheel-up-event)))))
                  (edraw-zoom-out editor))
                 ((eq event ?0)
                  (edraw-reset-scroll-and-zoom editor))
                 ((eq event ?#)
                  (edraw-toggle-grid-visible editor))
                 ))))
        ;; Hide transformation UI and show selection UI
        (edraw-remove-hook editor 'before-image-update
                           'edraw-update-ui-svg transformer)
        (edraw-remove-ui-svg transformer)
        (edraw-invalidate-image editor)
        (oset editor selection-ui-visible t)
        (edraw-invalidate-ui-parts editor 'selection-ui)))
    (when (eq result 'ok)
      (edraw-transform target (edraw-get-transform-matrix transformer)))))



;;;; Preset

(defun edraw-preset--check-arg (type name)
  (unless (symbolp type)
    (signal 'wrong-type-argument (list (format "preset type is not a symbol: %s" type))))
  (unless (or (stringp name)
              (edraw-preset-name-special-p name))
    (signal 'wrong-type-argument (list (format "preset name is not string or special name: %s" name))))
  (when (and (stringp name) (string-empty-p name))
    (error "preset name is empty")))

(defun edraw-preset-save (ui-state type name data)
  (edraw-preset--check-arg type name)
  (let* ((type-alist (edraw-ui-state-get ui-state 'preset 'presets))
         (type-cell (or (assq type type-alist)
                        (let ((new-type-cell (cons type nil)))
                          ;; push back
                          (setq type-alist
                                (append type-alist (list new-type-cell)))
                          new-type-cell)))
         (name-alist (cdr type-cell))
         (name-cell (or (assoc name name-alist)
                        (let ((new-name-cell (cons name nil)))
                          (if (edraw-preset-name-special-p name)
                              ;; Insert
                              (let ((pos (or (cl-position-if
                                              (lambda (x)
                                                (edraw-preset-name-less-p
                                                 name (car x)))
                                              name-alist)
                                             0)))
                                (setq name-alist
                                      (append
                                       (seq-subseq name-alist 0 pos)
                                       (list new-name-cell)
                                       (seq-subseq name-alist pos))))
                            ;; Push back
                            (setq name-alist
                                  (append name-alist (list new-name-cell))))
                          (setcdr type-cell name-alist)
                          new-name-cell))))
    (setcdr name-cell data)
    (edraw-ui-state-set ui-state 'preset 'presets type-alist)
    (edraw-ui-state-save ui-state)))

(defun edraw-preset-load (ui-state type name)
  (edraw-preset--check-arg type name)
  (let ((type-alist (edraw-ui-state-get ui-state 'preset 'presets)))
    (alist-get name (alist-get type type-alist) nil nil #'equal)))

(defun edraw-preset-enum (ui-state type &optional pred)
  (edraw-preset--check-arg type "-")
  (let* ((type-alist (edraw-ui-state-get ui-state 'preset 'presets))
         (name-alist (alist-get type type-alist)))
    (if pred
        (seq-filter (lambda (name-data) (funcall pred (cdr name-data)))
                    name-alist)
      name-alist)))

(defun edraw-preset-enum-names (ui-state type &optional pred)
  (mapcar #'car (edraw-preset-enum ui-state type pred)))

(defun edraw-preset-delete (ui-state type name)
  (edraw-preset--check-arg type name)
  (let ((type-alist (edraw-ui-state-get ui-state 'preset 'presets)))
    (setf (alist-get name (alist-get type type-alist) nil t #'equal) nil)
    (edraw-ui-state-set ui-state 'preset 'presets type-alist)
    (edraw-ui-state-save ui-state)))

(defun edraw-preset-rename (ui-state type old-name new-name)
  (edraw-preset--check-arg type old-name)
  (edraw-preset--check-arg type new-name)
  (when (symbolp old-name)
    (error "Special presets cannot be renamed: %s" old-name))
  (let* ((type-alist (edraw-ui-state-get ui-state 'preset 'presets))
         (name-alist (alist-get type type-alist))
         (cell (assoc old-name name-alist)))
    (when cell
      (setcar cell new-name)
      (edraw-ui-state-set ui-state 'preset 'presets type-alist)
      (edraw-ui-state-save ui-state)
      t)))

(defun edraw-preset-clear (ui-state type)
  (edraw-preset--check-arg type "-")
  (let ((type-alist (edraw-ui-state-get ui-state 'preset 'presets)))
    (setf (alist-get type type-alist nil t) nil)
    (edraw-ui-state-set ui-state 'preset 'presets type-alist)
    (edraw-ui-state-save ui-state)))

;;;;; for Properties Holder

(cl-defmethod edraw-preset-properties ((holder edraw-properties-holder))
  ;;@todo Save only common properties.
  (edraw-get-all-properties holder))

(cl-defmethod edraw-preset-apply ((holder edraw-properties-holder) data
                                  &optional
                                  pred-for-prop-info)
  (let (;;(subtype (alist-get 'subtype data))
        (properties (alist-get 'properties data))
        (pred-for-prop-info
         (pcase pred-for-prop-info
           ('nil (lambda (_prop-info) t))
           ('all (lambda (_prop-info) t))
           ('not-required (lambda (prop-info)
                            (not (edraw-svg-prop-info-required-p prop-info))))
           ('not-geometry (lambda (prop-info)
                            (not (edraw-svg-prop-info-flag-p prop-info
                                                             'geometry))))
           (_ pred-for-prop-info))))
    ;; Filter properties
    (let ((prop-info-list (edraw-get-property-info-list holder)))
      (setq properties
            (seq-filter
             (lambda (prop)
               (let* ((prop-name (car prop))
                      (prop-info (seq-find
                                  (lambda (prop-info)
                                    (eq (edraw-svg-prop-info-name prop-info)
                                        prop-name))
                                  prop-info-list)))
                 (and
                  ;; Can be set for PROPERTIES-HOLDER.
                  prop-info
                  ;; pred
                  (funcall pred-for-prop-info prop-info)
                  ;; Not required property.
                  ;;(not (edraw-svg-prop-info-required-p prop-info))
                  )))
             properties)))
    ;; Apply properties
    (when properties
      (edraw-set-properties holder properties))))

(cl-defmethod edraw-preset-tool-type ((holder edraw-properties-holder))
  (when-let* ((editor (edraw-get-editor holder))
              (tool (edraw-selected-tool editor))
              (tool-shape-type (edraw-shape-type-to-create tool)))
    (when (and (eq (edraw-preset-type holder) 'shape)
               (eq (edraw-preset-subtype holder) tool-shape-type))
      (edraw-tool-class tool))))

;;;;; for edraw-shape

(cl-defmethod edraw-preset-type ((_shape edraw-shape))
  'shape)

(cl-defmethod edraw-preset-subtype ((shape edraw-shape))
  ;;@todo If the data includes only common properties, subtype should be allowed to be nil.
  (edraw-shape-type shape))

;;;;; for edraw-multiple-shapes

(cl-defmethod edraw-preset-type ((_shapes edraw-multiple-shapes))
  'shape)

(cl-defmethod edraw-preset-subtype ((_shapes edraw-multiple-shapes))
  ;;@todo If SHAPES consist of a single type, return the type as subtype.
  nil)

;;;;; for edraw-alist-properties-holder

;; See edraw-property-editor.el



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

(defun edraw-edit-svg (source
                       _source-type
                       &optional ov-beg ov-end on-finish writer
                       accepts-top-level-comments-p)
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
                    :document-writer-accepts-top-level-comments-p
                    accepts-top-level-comments-p
                    :menu-filter #'edraw-edit-svg--menu-filter
                    :keymap (edraw-edit-svg--make-keymap edraw-editor-map))))

      ;; Set info to the editor object
      (edraw-set-extra-prop editor 'edraw-edit-svg--original-buffer
                            original-buffer)
      (edraw-set-extra-prop editor 'edraw-edit-svg--new-buffer
                            new-buffer)

      ;; Add Finish Hook
      (when on-finish
        (edraw-define-hook-type editor 'edraw-edit-svg--finish)
        (edraw-add-hook
         editor 'edraw-edit-svg--finish
         (lambda (ok svg &rest args)
           (if (buffer-live-p original-buffer)
               (with-current-buffer original-buffer
                 (unless accepts-top-level-comments-p
                   ;; Discard top-level comments
                   (setq svg (car (edraw-dom-split-top-nodes svg))))
                 (apply on-finish ok svg args))
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
                       (edraw-document-svg editor t)))))

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
