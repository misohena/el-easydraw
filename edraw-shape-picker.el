;;; edraw-shape-picker.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

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

(require 'cl-lib)
(require 'eieio)
(require 'edraw-dom-svg)
(require 'edraw-util)

(autoload 'edraw-edit-svg "edraw")
(declare-function edraw-get-document-body "edraw")
(declare-function edraw-create-document-svg "edraw")
(autoload 'edraw-shape-descriptor-list-to-svg-string "edraw")

(autoload 'edraw-color-picker-read-color "edraw-color-picker")

;;;; Customize

(defgroup edraw-shape-picker nil
  "Emacs Easy Draw Shape Picker"
  :prefix "edraw-shape-picker-"
  :group 'edraw-editor)

(defcustom edraw-shape-picker-entries-file
  (locate-user-emacs-file "edraw-custom.eshapes")
  ""
  :type 'file
  :group 'edraw-shape-picker)

;; Thumbnail

(defcustom edraw-shape-picker-thumbnail-scaling nil
  "Scale thumbnails."
  :type 'boolean
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-width 50;;100
  "Width of a thumbnail."
  :type 'integer
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-height 50;;100
  "Height of a thumbnail."
  :type 'integer
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-max-width nil
  "Maximum width of a thumbnail."
  :type '(choice integer
                 (const nil :tag "Same as `edraw-shape-picker-thumbnail-width'"))
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-max-height nil
  "Maximum height of a thumbnail."
  :type '(choice integer
                 (const nil :tag "Same as `edraw-shape-picker-thumbnail-height'"))
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-padding 5
  "Space around a shape, inside of image."
  :type '(choice integer
                 (list (integer :tag "Left Right")
                       (integer :tag "Top Bottom"))
                 (list (integer :tag "Left")
                       (integer :tag "Top")
                       (integer :tag "Right")
                       (integer :tag "Bottom")))
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-background
  '(full (fill . "#fff"))
  "Thumbnail background"
  :type '(cons :tag "Background Spec"
          (choice (const full)
                  (const content))
          (repeat
           (cons :tag "SVG Attribute"
                 (symbol :tag "Attribute")
                 (string :tag "Value")))
          )
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-foreground-selected
  '(full (stroke . "rgba(0,100,255,0.75)") (stroke-width . "8") (fill . "none"))
  "Thumbnail background"
  :type '(cons :tag "Foreground Spec"
          (choice (const full)
                  (const content))
          (repeat
           (cons :tag "SVG Attribute"
                 (symbol :tag "Attribute")
                 (string :tag "Value")))
          )
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-margin
  '(1 . 2) ;; 2 means show active region
  "Thumbnail margin."
  :type '(choice (integer)
                 (cons
                  (integer :tag "X")
                  (integer :tag "Y")))
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-string-between-thumbnails
  nil
  ;; (propertize " "
  ;;             'display '(space :width (1))
  ;;             'pointer 'arrow)
  "String between thumbnails"
  :type 'string
  :group 'edraw-shape-picker)

;; Face

(defface edraw-shape-picker-heading
  '((t :background "#444" :foreground "#eee" :underline "#000" :extend t))
  "Face used for section headings."
  :group 'edraw-shape-picker)

;;

(defvar edraw-shape-picker-entries-default
  ;; (:section <root-props>... <entry>...)
  ;;
  ;; <entry> :
  ;;   (:section <section-prop>... <entry>...)
  ;;     see: `edraw-shape-picker-insert-section'
  ;;
  ;;   (:layout <layout-prop>... <entry>...)
  ;;     see: `edraw-shape-picker-insert-layout'
  ;;
  ;;   (:shape <shape-prop>... <shape-def>)
  ;;     see: `edraw-shape-picker-insert-shape'
  '(:section :name "Default Shape Set"
             (:section :name "Basic Shapes"
                       (:shape :name "Trigon(3)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-34.641,20L34.641,20z\" />")
                       (:shape :name "Tetragon(4)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-40,0L0,40L40,0z\" />")
                       (:shape :name "Pentagon(5)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-38.0423,-12.3607L-23.5114,32.3607L23.5114,32.3607L38.0423,-12.3607z\" />")
                       (:shape :name "Hexagon(6)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-34.641,-20L-34.641,20L0,40L34.641,20L34.641,-20z\" />")
                       (:shape :name "Heptagon(7)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-31.2733,-24.9396L-38.9971,8.90084L-17.3553,36.0388L17.3553,36.0388L38.9971,8.90084L31.2733,-24.9396z\" />")
                       (:shape :name "Octagon(8)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-28.2843,-28.2843L-40,0L-28.2843,28.2843L0,40L28.2843,28.2843L40,0L28.2843,-28.2843z\" />")
                       (:shape :name "Nonagon(9)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-25.7115,-30.6418L-39.3923,-6.94593L-34.641,20L-13.6808,37.5877L13.6808,37.5877L34.641,20L39.3923,-6.94593L25.7115,-30.6418z\" />")
                       (:shape :name "Decagon(10)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-23.5114,-32.3607L-38.0423,-12.3607L-38.0423,12.3607L-23.5114,32.3607L0,40L23.5114,32.3607L38.0423,12.3607L38.0423,-12.3607L23.5114,-32.3607z\" />")
                       (:shape :name "Star(5)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-8.98056,-12.3607L-38.0423,-12.3607L-14.5309,4.72136L-23.5114,32.3607L0,15.2786L23.5114,32.3607L14.5309,4.72136L38.0423,-12.3607L8.98056,-12.3607z\" />")
                       (:shape :name "Star(6)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-11.547,-20L-34.641,-20L-23.094,0L-34.641,20L-11.547,20L0,40L11.547,20L34.641,20L23.094,0L34.641,-20L11.547,-20z\" />")
                       (:shape :name "Star(7)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-12.0103,-24.9396L-31.2733,-24.9396L-26.9868,-6.15957L-38.9971,8.90084L-21.6418,17.2587L-17.3553,36.0388L0,27.6809L17.3553,36.0388L21.6418,17.2587L38.9971,8.90084L26.9868,-6.15957L31.2733,-24.9396L12.0103,-24.9396z\" />")
                       (:shape :name "Star(8)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-11.7157,-28.2843L-28.2843,-28.2843L-28.2843,-11.7157L-40,0L-28.2843,11.7157L-28.2843,28.2843L-11.7157,28.2843L0,40L11.7157,28.2843L28.2843,28.2843L28.2843,11.7157L40,0L28.2843,-11.7157L28.2843,-28.2843L11.7157,-28.2843z\" />")
                       (:shape :name "Star(9)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-11.1527,-30.6418L-25.7115,-30.6418L-28.2396,-16.3041L-39.3923,-6.94593L-32.1129,5.66237L-34.641,20L-20.9602,24.9794L-13.6808,37.5877L0,32.6083L13.6808,37.5877L20.9602,24.9794L34.641,20L32.1129,5.66237L39.3923,-6.94593L28.2396,-16.3041L25.7115,-30.6418L11.1527,-30.6418z\" />")
                       (:shape :name "Star(10)" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-10.5146,-32.3607L-23.5114,-32.3607L-27.5276,-20L-38.0423,-12.3607L-34.026,0L-38.0423,12.3607L-27.5276,20L-23.5114,32.3607L-10.5146,32.3607L0,40L10.5146,32.3607L23.5114,32.3607L27.5276,20L38.0423,12.3607L34.026,0L38.0423,-12.3607L27.5276,-20L23.5114,-32.3607L10.5146,-32.3607z\" />"))
             (:section :name "Arrows"
                       (:shape :name "Up Arrow" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L-40,40L-20,40L-20,80L20,80L20,40L40,40L0,0Z\" />")
                       (:shape :name "Right Arrow" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L-40,-40L-40,-20L-80,-20L-80,20L-40,20L-40,40L0,0Z\" />")
                       (:shape :name "Down Arrow" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L-40,-40L-20,-40L-20,-80L20,-80L20,-40L40,-40L0,0Z\" />")
                       (:shape :name "Left Arrow" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L40,-40L40,-20L80,-20L80,20L40,20L40,40L0,0Z\" />")
                       (:shape :name "Up Arrow" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L20,40L5,30L5,80L-5,80L-5,30L-20,40L0,0Z\" />")
                       (:shape :name "Left Arrow" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L-40,20L-30,5L-80,5L-80,-5L-30,-5L-40,-20L0,0Z\" />")
                       (:shape :name "Left Arrow" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L-20,-40L-5,-30L-5,-80L5,-80L5,-30L20,-40L0,0Z\" />")
                       (:shape :name "Left Arrow" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L40,-20L30,-5L80,-5L80,5L30,5L40,20L0,0Z\" />"))
             (:section :name "Unknown Symbols"
                       (:shape :name "Uzu" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M3,9C8,11 17,4 17,-2C17,-10 11,-16 1,-16C-11,-16 -18,-6.049334505329199 -18,4C-18,17 -8,29 7,29C25,29 37,12.868873853548592 37,0C37,-20 22,-35.226 2,-36C-20,-37 -39.0439523289494,-19.077001092062517 -40,1C-41,22 -32,33 -18,43\" />")
                       (:shape :name "Star" "<path stroke=\"#888\" stroke-width=\"4\" d=\"M0,-40L-12.9313,-17.7984L-38.0423,-12.3607L-20.9232,6.79837L-23.5114,32.3607L0,22L23.5114,32.3607L20.9232,6.79837L38.0423,-12.3607L12.9313,-17.7984z\" />")
                       (:shape :name "Wave" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M-27,-40.5C-18,-31.5 -18,-22.5 -27,-13.5C-33.3639,-7.1352 -33.3639,7.1352 -27,13.5C-18,22.5 -18,31.5 -27,40.5\" /><path d=\"M0,-40.5C9,-31.5 9,-22.5 0,-13.5C-6.3639,-7.1352 -6.3639,7.1352 0,13.5C9,22.5 9,31.5 0,40.5\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M27,-40.5C36,-31.5 36,-22.5 27,-13.5C20.6352,-7.1352 20.6352,7.1352 27,13.5C36,22.5 36,31.5 27,40.5\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" />")
                       (:shape :name "Plus" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M-10,-10L-10,-40L10,-40L10,-10L40,-10L40,10L10,10L10,40L-10,40L-10,10L-40,10L-40,-10L-10,-10Z\" />")
                       (:shape :name "Onsen" "<path d=\"M20,-20C26.666666,-13.333333 26.666666,-6.666666 20,0.0C13.333333,6.666666 13.333333,13.333333 20,20\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M-20,-20C-13.333333,-13.333333 -13.333333,-6.666666 -20,0.0C-26.666666,6.666666 -26.666666,13.333333 -20,20\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M0.0,-26.666666C6.666666,-20 6.666666,-6.666666 0.0,0.0C-6.666666,6.666666 -6.666666,20 0.0,26.666666\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M26.666666,6.666666C33.333333,10 40,13.333333 40,20C40,28 20,33.333333 0.0,33.333333C-20,33.333333 -40,28 -40,20C-40,13.333333 -33.333333,10 -26.666666,6.666666\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" />"))
             (:section :name "Weather"
                       (:shape :name "Sunny" "<g><ellipse cx=\"0\" cy=\"0\" rx=\"20\" ry=\"20\" fill=\"#888\" stroke=\"none\" /><path d=\"M-30,0L-40,0\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M0,-30L0,-40\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M30,0L40,0\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M0,30L0,40\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M-21.213203435596427,21.213203435596427L-28.284271247461895,28.284271247461902\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M21.213203435596427,21.213203435596427L28.284271247461902,28.284271247461888\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M21.213203435596427,-21.213203435596434L28.284271247461902,-28.28427124746191\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /><path d=\"M-21.213203435596427,-21.213203435596427L-28.284271247461902,-28.284271247461902\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" /></g>")
                       (:shape :name "Cloudy" "<path d=\"M-20,-10C-20,-20 -10,-30 0,-30C10,-30 20,-20 20,-10C30,-10 40,0 40,10C40,20 30,30 20,30C10,30 -10,30 -20,30C-30,30 -40,20 -40,10C-40,0 -30,-10 -20,-10Z\" fill=\"#888\" stroke=\"none\" />")
                       (:shape :name "Rainy" "<path d=\"M-20,-25C-20,-30 -10,-35 0,-35C10,-35 20,-30 20,-25C30,-25 40,-20 40,-15C40,-10 30,-5 20,-5C10,-5 -10,-5 -20,-5C-30,-5 -40,-10 -40,-15C-40,-20 -30,-25 -20,-25Z\" fill=\"#888\" stroke=\"none\" /><path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M-20,0L-30,40\" stroke-dasharray=\"8 3\" /><path d=\"M-10,0L-20,40\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" stroke-dasharray=\"8 3\" /><path d=\"M0,0L-10,40\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" stroke-dasharray=\"8 3\" /><path d=\"M10,0L0,40\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" stroke-dasharray=\"8 3\" /><path d=\"M20,0L10,40\" fill=\"none\" stroke=\"#888\" stroke-width=\"4\" stroke-dasharray=\"8 3\" />")
                       (:shape :name "Lightning" "<path stroke-width=\"4\" d=\"M10,-40L-15,0L0,5L-10,40L15,0L0,-5L10,-40Z\" fill=\"#888\" stroke=\"none\" />"))
             (:section :name "Game"
                       (:shape :name "Spade" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M-14.285714285694993,40C-5.714285714260001,28.57142857148 -5.714285714260001,22.857142857190013 -5.714285714260001,11.428571428610013C-11.42857142855,17.1428571429 -17.14285714284,20 -22.85714285713,20C-34.28571428571,20 -40,11.428571428610013 -40,2.857142857175006C-40,-10.71428571426 -13.571428571404994,-25 0,-40C13.571428571465006,-25 40,-10.71428571426 40,2.857142857175006C40,11.428571428610013 34.28571428577001,20 22.857142857190013,20C17.1428571429,20 11.428571428610013,17.1428571429 5.714285714319999,11.428571428610013C5.714285714319999,22.857142857190013 5.714285714319999,28.57142857148 14.285714285755006,40C2.857142857175006,40 -2.8571428571149937,40 -14.285714285694993,40Z\" />")
                       (:shape :name "Heart" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,35C-20,15 -40,5 -40,-15C-40,-25 -30,-35 -20,-35C-15,-35 -10,-35 0,-25C10,-35 15,-35 20,-35C30,-35 40,-25 40,-15C40,5 20,15 0,35Z\" />")
                       (:shape :name "Diamond" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,-40C-10,-20 -20,-10 -30,0C-20,10 -10,20 0,40C10,20 20,10 30,0C20,-10 10,-20 0,-40Z\" />")
                       (:shape :name "Club" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M-14.285714285694997,40C-5.714285714259994,28.571428571480013 -5.714285714259994,22.85714285719 -5.714285714259994,11.42857142861C-11.428571428549997,17.1428571429 -17.142857142839997,20 -22.857142857129997,20C-34.28571428571,20 -40,11.42857142861 -40,2.857142857175006C-40,-5.714285714259994 -31.428571428565,-14.285714285694997 -22.857142857129997,-14.285714285694997C-17.142857142839997,-14.285714285694997 -14.285714285694997,-11.428571428549997 -11.428571428549997,-8.571428571404997C-17.142857142839997,-14.285714285694997 -17.142857142839997,-14.285714285694997 -17.142857142839997,-22.857142857129997C-17.142857142839997,-31.428571428565 -8.571428571404997,-40 0,-40C8.571428571465006,-40 17.14285714290,-31.428571428565 17.14285714290,-22.857142857129997C17.14285714290,-14.285714285694997 14.285714285755006,-11.428571428549997 11.42857142861,-8.571428571404997C17.14285714290,-14.285714285694997 17.14285714290,-14.285714285694997 22.85714285719,-14.285714285694997C31.428571428625006,-14.285714285694997 40,-5.714285714259994 40,2.857142857175006C40,11.42857142861 34.28571428577001,20 22.85714285719,20C17.14285714290,20 11.42857142861,17.1428571429 5.71428571432,11.42857142861C5.71428571432,22.85714285719 5.71428571432,28.571428571480013 14.285714285755006,40C2.857142857175006,40 -2.8571428571149937,40 -14.285714285694997,40Z\" />"))
             (:section :name "Custom"
                       (:shape :name "fujisan" :background
                               (full
                                (fill . "#333"))
                               :width 100 "<path stroke=\"#888\" stroke-width=\"4\" d=\"M-104,40C-60,1 -37,-28 -23,-57C-23,-57 9,-55 26,-57C44,-28 99,34 107,40C85,39 -81,41 -104,40Z\" fill=\"#8c94b1\" /><path d=\"M-43,-22C-36,-34 -37,-28 -23,-57C-23,-57 9,-55 26,-57C44,-28 44,-27 52,-21C51.75,-21.0625 29,-37 29,-37L19,-22C19,-22 4,-40 4,-40C4,-40 -10,-21 -10,-21C-10,-21 -21,-37 -21,-37C-21,-37 -39.5,-22.5 -43,-22Z\" stroke=\"#888\" stroke-width=\"4\" fill=\"#eeeeee\" />")
                       (:shape :name "Bou-ningen" "<path stroke=\"#888\" stroke-width=\"4\" fill=\"none\" d=\"M0,-8C-9.2,-8 -16,-14.8 -16,-24C-16,-33.2 -9.2,-40 0,-40C9.2,-40 16,-33.2 16,-24C16,-14.8 9.2,-8 0,-8C0,-8 2,-8 2,-8L24,12L22.8,13.6C22.8,13.6 0.82,-6.4 0.82,-6.4C0.82,-6.4 0.82,14.8 0.82,14.8L24,38L22.8,39.6L0,16.8L-22.8,39.6L-24,38C-24,38 -0.82,14.8 -0.82,14.8C-0.82,14.8 -0.82,-6.4 -0.82,-6.4L-22.8,13.6L-24,12L-2,-8C-2,-8 0,-8 0,-8Z\" />"))))


;;;; Setup

(defconst edraw-shape-picker-buffer-name "*Easy Draw Shape Picker*")

(defvar-local edraw-shape-picker-opened-by-user nil)
(defvar-local edraw-shape-picker-initial-window nil)

(defun edraw-shape-picker ()
  (interactive)
  (edraw-shape-picker-open)
  (setq-local edraw-shape-picker-opened-by-user t))

(defun edraw-shape-picker-open ()
  (edraw-shape-picker-pop-to-buffer
   (cond
    ;; org format
    ((and edraw-shape-picker-entries-file
          (equal (file-name-extension edraw-shape-picker-entries-file) "org"))
     (find-file-noselect edraw-shape-picker-entries-file)
     ;;@todo run edraw-shape-picker-org-mode minor mode
     )
    ;; .eshapes format
    (t
     (edraw-shape-picker-get-buffer-file-mode edraw-shape-picker-entries-file)))))
;;(edraw-shape-picker-open)

(defun edraw-shape-picker-get-buffer-file-mode (file)
  (if file
      (or
       ;; Existing buffer
       (find-buffer-visiting file)
       ;; Existing file
       (when (file-exists-p file)
         (let ((buffer (find-file-noselect file)))
           (with-current-buffer buffer
             (edraw-shape-picker-file-mode)
             buffer)))
       ;; New buffer
       (edraw-shape-picker-create-buffer-file-mode file))
    ;; New buffer (not yet associated with a file)
    (edraw-shape-picker-create-buffer-file-mode nil)))

(defun edraw-shape-picker-create-buffer-file-mode (file)
  (let ((buffer (generate-new-buffer edraw-shape-picker-buffer-name)))
    (with-current-buffer buffer
      (with-silent-modifications
        (edraw-shape-picker-write-entries buffer edraw-shape-picker-entries-default)
        (when file
          (set-visited-file-name file)))
      (edraw-shape-picker-file-mode))
    buffer))

(defun edraw-shape-picker-pop-to-buffer (buffer)
  (pop-to-buffer buffer)
  (setq-local edraw-shape-picker-initial-window (selected-window))
  buffer)

;; For read-only buffers never associated with a file.
;; Use edraw-shape-picker-ui-mode (not -file-mode).

(defun edraw-shape-picker-open-neverfile (&optional buffer-name)
  (edraw-shape-picker-pop-to-buffer
   (edraw-shape-picker-get-buffer-neverfile buffer-name)))

(defun edraw-shape-picker-get-buffer-neverfile (&optional buffer-name)
  (unless buffer-name
    (setq buffer-name edraw-shape-picker-buffer-name))
  (or
   ;; Existing buffer
   (get-buffer buffer-name)
   ;; New buffer
   (edraw-shape-picker-create-buffer-neverfile buffer-name)))

(defun edraw-shape-picker-create-buffer-neverfile (buffer-name)
  (unless buffer-name
    (setq buffer-name edraw-shape-picker-buffer-name))
  (let ((buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (edraw-shape-picker-set-local-entries
       edraw-shape-picker-entries-default t)
      (edraw-shape-picker-make-buffer-contents) ;;silent modification
      (edraw-shape-picker-ui-mode))
    buffer))

;;

(defun edraw-shape-picker-connect (buffer on-notify)
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'edraw-shape-picker-notification-hook on-notify nil t)))

(defun edraw-shape-picker-disconnect (buffer on-notify)
  (with-current-buffer (or buffer (current-buffer))
    (remove-hook 'edraw-shape-picker-notification-hook on-notify t)
    ;; Close automatically
    (when (and (edraw-shape-picker-notification-hook-empty-p)
               (not edraw-shape-picker-opened-by-user))
      (edraw-shape-picker-close buffer))))

(defun edraw-shape-picker-close (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (when (buffer-live-p buffer)
    ;; Kill Window
    (with-current-buffer buffer
      (if (and edraw-shape-picker-initial-window
               (window-live-p edraw-shape-picker-initial-window))
          (quit-restore-window edraw-shape-picker-initial-window 'bury)
        (when-let ((window (get-buffer-window buffer)))
          (when (window-parent window)
            (delete-window window))))
      (setq-local edraw-shape-picker-initial-window nil))
    ;; Kill buffer?
    (if (buffer-modified-p buffer)
        (message (edraw-msg "Custom shapes have unsaved changes."))
      (kill-buffer buffer))))

;;;; Picker Mode (Picker Buffer Control)
;;;;; Key Map
(defvar edraw-shape-picker-ui-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "q" #'edraw-shape-picker-quit)
    (define-key km "g" #'edraw-shape-picker-refresh)
    (define-key km [mouse-2] #'edraw-shape-picker-quit)
    (define-key km [mouse-3] #'edraw-shape-picker-open-context-menu-at)
    (define-key km "ii" #'edraw-shape-picker-insert-new-shape-at)
    (define-key km "is" #'edraw-shape-picker-insert-new-section-at)
    (define-key km "ix" #'edraw-shape-picker-import-section-at)
    (define-key km "y" #'edraw-shape-picker-paste-entry-at)
    (define-key km "R" #'edraw-shape-picker-rename-entry-at)
    (define-key km "D" #'edraw-shape-picker-delete-entry-at)
    (define-key km "w" #'edraw-shape-picker-copy-entry-at)
    (define-key km "W" #'edraw-shape-picker-cut-entry-at)
    (define-key km [remap delete-char] #'edraw-shape-picker-delete-entry-at)
    (define-key km [remap delete-backward-char] #'edraw-shape-picker-delete-backward-entry-at)
    (define-key km [remap kill-ring-save] #'edraw-shape-picker-copy-entries-region)
    (define-key km [remap kill-region] #'edraw-shape-picker-cut-entries-region)
    (define-key km [remap yank] #'edraw-shape-picker-paste-entry-at)
    (define-key km (kbd "M-<right>") #'edraw-shape-picker-move-entry-forward)
    (define-key km (kbd "M-<left>") #'edraw-shape-picker-move-entry-backward)
    (define-key km (kbd "M-<down>") #'edraw-shape-picker-move-entry-forward-same-level)
    (define-key km (kbd "M-<up>") #'edraw-shape-picker-move-entry-backward-same-level)
    (define-key km "p" #'edraw-shape-picker-set-entry-property-at)
    ;; File
    (define-key km "fi" #'edraw-shape-picker-import-section-at)
    (define-key km "fx" #'edraw-shape-picker-export-section-at)
    (define-key km "fR" #'edraw-shape-picker-reset-entries-to-default)
    km))

(defvar edraw-shape-picker-thumbnail-map
  (let ((km (make-sparse-keymap)))
    (define-key km [mouse-1] #'edraw-shape-picker-select-shape-at)
    (define-key km (kbd "RET") #'edraw-shape-picker-select-shape-at)
    (define-key km "e" #'edraw-shape-picker-edit-shape-at)
    km))

;;;;; Variables

(defvar-local edraw-shape-picker-entries nil)
(defvar-local edraw-shape-picker-notification-hook nil)
(defvar-local edraw-shape-picker-selected-shape-entry nil)
(defvar-local edraw-shape-picker-inhibit-refresh nil)
(defvar-local edraw-shape-picker-modified-entries nil)

;;;;; Mode

(define-derived-mode edraw-shape-picker-ui-mode nil "EShapes UI"
  "Major mode for edraw shape picker UI.

\\{edraw-shape-picker-ui-mode-map}

\\{edraw-shape-picker-thumbnail-map}"
  (setq-local line-move-visual t
              line-spacing 0))

(defun edraw-shape-picker-set-local-entries (entries copy)
  (setq-local edraw-shape-picker-entries
              (if copy
                  (copy-tree entries)
                entries)))

;;;;; Notification

(defun edraw-shape-picker-notification-hook-empty-p ()
  (null edraw-shape-picker-notification-hook))

(defun edraw-shape-picker-notify (type &rest args)
  (apply #'run-hook-with-args
         'edraw-shape-picker-notification-hook
         type
         args))

;;;;; Display

(defun edraw-shape-picker-refresh ()
  (interactive)
  (edraw-shape-picker-make-buffer-contents))

(defun edraw-shape-picker-undo-combined (bul)
  (let ((edraw-shape-picker-inhibit-refresh t))
    (dolist (u bul)
      (when (eq (car u) 'apply)
        (apply (cadr u)
               (cddr u)))))
  (edraw-shape-picker-refresh-on-entry-modified))

(defmacro edraw-shape-picker-combine-refresh (&rest body)
  (let ((actions (gensym 'actions-)))
    `(let* ((,actions
             (let ((edraw-shape-picker-inhibit-refresh t)
                   (buffer-undo-list nil))
               ,@body
               buffer-undo-list))
            (last-action (car (last ,actions))))
       ;; Refresh
       (edraw-shape-picker-refresh-on-entry-modified)
       ;; Move last time-flag to original undo list
       (when (eq (car last-action) t) ;; (t . time-flag)
         (push last-action
               buffer-undo-list)
         (setq ,actions (nbutlast ,actions))) ;;remove last
       ;; Push combined actions to undo list
       (when ,actions
         (push (list 'apply #'edraw-shape-picker-undo-combined ,actions)
               buffer-undo-list)))))

;;;;; Mouse Support

(defun edraw-shape-picker-interactive-point-buffer ()
  (let ((event last-input-event))
    (if (and (consp event)
             (symbolp (car event))
             (consp (cdr event))) ;;Click Event?
        (let* ((click-start (event-start event))
               (click-window (posn-window click-start))
               (click-buffer (window-buffer click-window))
               (click-point (posn-point click-start)))
          (list click-point click-buffer))
      (list (point)))))

;;;;; Menu

(defun edraw-shape-picker-open-context-menu-at (pos &optional buffer)
  (interactive (edraw-shape-picker-interactive-point-buffer))
  (with-current-buffer (or buffer (current-buffer))
    (if (and (region-active-p)
             (<= (region-beginning) pos)
             (< pos (region-end)))
        (edraw-popup-menu-call-interactively
         "Region"
         (edraw-shape-picker-region-actions))
      (goto-char pos)
      (if-let ((entry (edraw-shape-picker-entry-at pos)))
          (edraw-popup-menu-call-interactively
           (edraw-shape-picker-entry-name-for-msg entry)
           (edraw-shape-picker-entry-actions entry))
        (edraw-popup-menu-call-interactively
         "Shape Picker"
         (edraw-shape-picker-buffer-actions))))))

(defun edraw-shape-picker-region-actions ()
  `((,(edraw-msg "Copy") edraw-shape-picker-copy-entries-region)
    (,(edraw-msg "Cut") edraw-shape-picker-cut-entries-region)))

(defun edraw-shape-picker-buffer-actions ()
  `((,(edraw-msg "Close") edraw-shape-picker-quit)
    (,(edraw-msg "Save") save-buffer
     :enable ,(buffer-modified-p))
    (,(edraw-msg "Undo") undo)
    (,(edraw-msg "Insert New Shape") edraw-shape-picker-insert-new-shape-at)
    (,(edraw-msg "Insert New Section") edraw-shape-picker-insert-new-section-at)
    (,(edraw-msg "Import Section") edraw-shape-picker-import-section-at)
    (,(edraw-msg "Reset to Default") edraw-shape-picker-reset-entries-to-default)
    (,(edraw-msg "Paste") edraw-shape-picker-paste-entry-at
     :enable ,(edraw-shape-picker-clipboard-not-empty-p))))

(defun edraw-shape-picker-entry-actions (entry)
  (pcase (edraw-shape-picker-entry-type entry)
    (:section
     `((,(edraw-msg "Move Forward Same Level") edraw-shape-picker-move-entry-forward-same-level)
       (,(edraw-msg "Move Backward Same Level") edraw-shape-picker-move-entry-backward-same-level)
       (,(edraw-msg "Move Forward") edraw-shape-picker-move-entry-forward)
       (,(edraw-msg "Move Backward") edraw-shape-picker-move-entry-backward)
       (,(edraw-msg "Rename") edraw-shape-picker-rename-entry-at)
       (,(edraw-msg "Set Property") edraw-shape-picker-set-entry-property-at)
       (,(edraw-msg "Insert New Shape") edraw-shape-picker-insert-new-shape-at)
       (,(edraw-msg "Insert New Section") edraw-shape-picker-insert-new-section-at)
       (,(edraw-msg "Import Section") edraw-shape-picker-import-section-at)
       (,(edraw-msg "Export Section") edraw-shape-picker-export-section-at)
       (,(edraw-msg "Delete") edraw-shape-picker-delete-entry-at)
       (,(edraw-msg "Copy") edraw-shape-picker-copy-entry-at)
       (,(edraw-msg "Cut") edraw-shape-picker-cut-entry-at)
       (,(edraw-msg "Paste") edraw-shape-picker-paste-entry-at
        :enable ,(edraw-shape-picker-clipboard-not-empty-p))))
    (:shape
     `((,(edraw-msg "Select") edraw-shape-picker-select-shape-at)
       (,(edraw-msg "Move Forward") edraw-shape-picker-move-entry-forward)
       (,(edraw-msg "Move Backward") edraw-shape-picker-move-entry-backward)
       (,(edraw-msg "Edit") edraw-shape-picker-edit-shape-at)
       (,(edraw-msg "Rename") edraw-shape-picker-rename-entry-at)
       (,(edraw-msg "Set Property") edraw-shape-picker-set-entry-property-at)
       (,(edraw-msg "Insert New Shape Before") edraw-shape-picker-insert-new-shape-at)
       (,(edraw-msg "Insert New Section Before") edraw-shape-picker-insert-new-section-at)
       (,(edraw-msg "Import Section Before") edraw-shape-picker-import-section-at)
       (,(edraw-msg "Delete") edraw-shape-picker-delete-entry-at)
       (,(edraw-msg "Copy") edraw-shape-picker-copy-entry-at)
       (,(edraw-msg "Cut") edraw-shape-picker-cut-entry-at)
       (,(edraw-msg "Paste Before") edraw-shape-picker-paste-entry-at
        :enable ,(edraw-shape-picker-clipboard-not-empty-p))))))

;;;;; Quit

(defun edraw-shape-picker-quit (&optional _pos buffer)
  (interactive (edraw-shape-picker-interactive-point-buffer))
  (edraw-shape-picker-close buffer))

;;;;; Edit Entry Properties

(defun edraw-shape-picker-rename-entry-at (pos)
  (interactive "d")
  (when-let ((entry (edraw-shape-picker-entry-at pos)))
    (when (edraw-shape-picker-entry-can-have-name-p entry)
      ;; :section, :shape
      (let* ((old-name
              (or (edraw-shape-picker-entry-prop-get entry :name)
                  ""))
             (new-name
              (read-string (edraw-msg "Input name: ") old-name old-name)))
        (edraw-shape-picker-entry-prop-put entry :name new-name)))))

(defconst edraw-shape-picker-entry-prop-defs
  '((:section
     (name string)
     (thumbnail-width integer)
     (thumbnail-height integer)
     (thumbnail-max-width integer)
     (thumbnail-max-height integer)
     (thumbnail-background cover)
     (thumbnail-foreground-selected cover)
     (thumbnail-padding integer) ;;@todo or (x y) or (left top right bottom)
     (thumbnail-margin integer)) ;;@todo or (x y)
    (:layout
     (thumbnail-width integer)
     (thumbnail-height integer)
     (thumbnail-max-width integer)
     (thumbnail-max-height integer)
     (thumbnail-background cover)
     (thumbnail-foreground-selected cover)
     (thumbnail-padding integer) ;;@todo or (x y) or (left top right bottom)
     (thumbnail-margin integer)) ;;@todo or (x y)
    (:shape
     (name string)
     (width integer)
     (height integer)
     (max-width integer)
     (max-height integer)
     (background cover)
     (foreground-selected cover)
     (padding integer) ;;@todo or (x y) or (left top right bottom)
     (margin integer)))) ;;@todo or (x y)


(defun edraw-shape-picker-set-entry-property-at (pos)
  (interactive "d")
  (when-let ((entry (edraw-shape-picker-entry-at pos)))
    (let* ((prop-defs (alist-get (edraw-shape-picker-entry-type entry)
                                 edraw-shape-picker-entry-prop-defs))
           (prop-name (completing-read (edraw-msg "Property: ")
                                       (mapcar #'car prop-defs)))
           (prop-sym (intern (concat ":" prop-name)))
           (prop-def (alist-get (intern prop-name) prop-defs))
           (prop-type (nth 0 prop-def))
           (prompt (format "%s(%s or empty): " prop-name prop-type))
           (curr-val (edraw-shape-picker-entry-prop-get entry prop-sym))
           (new-val (pcase prop-type
                      ('string
                       (let ((str (read-string prompt (or curr-val ""))))
                         (if (string-empty-p str) nil str)))
                      ('integer
                       (edraw-read-integer-or-nil prompt curr-val))
                      ('cover
                       ;; see: `edraw-svg-shape-thumbnail-cover'
                       (let* ((fill (edraw-color-picker-read-color
                                     "Fill Color: "
                                     (alist-get 'fill (cdr curr-val) "")
                                     '("none" "")))
                              (fill (if (string-empty-p fill) nil fill))
                              (stroke (edraw-color-picker-read-color
                                       "Stroke Color: "
                                       (alist-get 'stroke (cdr curr-val) "")
                                       '("none" "")))
                              (stroke (if (string-empty-p stroke) nil stroke))
                              (stroke-width (when stroke
                                              (edraw-read-integer-or-nil
                                               "Stroke Width: "
                                               (alist-get 'stroke-width
                                                          (cdr curr-val) "")))))
                         (if (or fill stroke stroke-width)
                             (nconc
                              (list 'full)
                              (when fill
                                (list (cons 'fill fill)))
                              (when stroke
                                (list (cons 'stroke stroke)))
                              (when stroke-width
                                (list (cons 'stroke-width
                                            (format "%s" stroke-width)))))
                           nil))))))
      (unless (equal new-val curr-val)
        (if new-val
            (edraw-shape-picker-entry-prop-put entry prop-sym new-val)
          (edraw-shape-picker-entry-prop-remove entry prop-sym))))))

;;;;; Edit Shape

(defun edraw-shape-picker-edit-shape-at (pos)
  (interactive "d")

  (when-let ((entry (edraw-shape-picker-shape-entry-at pos)))
    (edraw-edit-svg
     ;; Source
     (edraw-create-document-svg
      nil nil nil
      (edraw-shape-picker-shape-to-svg-node-list
       (edraw-shape-picker-shape-entry-shape-get entry)))
     'edraw-svg
     ;; Overlay Range
     nil nil
     ;; On Finish
     (lambda (ok svg)
       ;; Called on the current buffer
       (when ok
         (edraw-shape-picker-edit-shape--save entry svg))))))

(defun edraw-shape-picker-edit-shape--save (entry svg)
  (edraw-shape-picker-shape-entry-shape-set
   entry
   ;;@todo add defs if using markers
   ;; SVG string
   (edraw-shape-picker-editor-svg-to-string svg)))

(defun edraw-shape-picker-editor-svg-to-string (svg)
  "Convert edraw-editor's SVG to SVG string."
  (mapconcat
   (lambda (node) (edraw-svg-encode node nil nil))
   ;; Under body nodes
   (dom-children (edraw-get-document-body svg))
   ""))

;;;;; Insert

(defun edraw-shape-picker-insert-new-shape-at (pos)
  (interactive "d")

  (if-let ((parent-index (edraw-shape-picker-entry-insertion-point-at pos)))
      (progn
        (edraw-edit-svg
         ;; Source
         (edraw-create-document-svg nil nil nil nil) ;;empty
         'edraw-svg
         ;; Overlay Range
         nil nil
         ;; On Finish
         (lambda (ok svg)
           ;; Called on the current buffer
           (when ok
             (edraw-shape-picker-insert-new-shape--save parent-index svg)))))
    (message (edraw-msg "Failed to find insertion point"))))

(defun edraw-shape-picker-insert-new-shape--save (parent-index svg)
  ;;@todo check parent-index exists
  (let ((svg-str (edraw-shape-picker-editor-svg-to-string svg)))
    (if (string-empty-p svg-str)
        (message (edraw-msg "Empty shapes cannot be registered"))
      (edraw-shape-picker-entry-insert
       (car parent-index) (cdr parent-index)
       ;;@todo Make shape constructor function
       (list :shape
             :name (read-string "Shape name: ")
             svg-str)))))

(defun edraw-shape-picker-insert-new-section-at (pos)
  (interactive "d")

  (if-let ((parent-index (edraw-shape-picker-entry-insertion-point-at pos)))
      (edraw-shape-picker-entry-insert
       (car parent-index) (cdr parent-index)
       ;;@todo Make shape constructor function
       (list :section
             :name (read-string "Section name: ")))
    (message (edraw-msg "Failed to find insertion point"))))

;;;;; Edit Structure

(defun edraw-shape-picker-delete-entry-common (entry)
  (when (eq entry edraw-shape-picker-entries)
    (error (edraw-msg "The root entry cannot be deleted")))

  ;; Deselect
  (edraw-shape-picker-ensure-deselect-shape entry)

  ;; Remove entry or parent?
  (let ((parent (edraw-shape-picker-entry-parent entry)))
    (if (and parent
             (eq (edraw-shape-picker-entry-type parent) :layout)
             (= (length (edraw-shape-picker-entry-child-entries parent)) 1))
        ;; Remove empty layout
        (unless (edraw-shape-picker-entry-remove parent)
          (error (edraw-msg "Failed to delete entry")))
      ;; Remove entry
      (unless (edraw-shape-picker-entry-remove entry)
        (error (edraw-msg "Failed to delete entry"))))
    entry))

(defun edraw-shape-picker-delete-entry-at (pos)
  (interactive "d")
  (if-let ((entry (edraw-shape-picker-entry-at pos)))
      (edraw-shape-picker-delete-entry-common entry)
    (message (edraw-msg "No entries at point"))))

(defun edraw-shape-picker-delete-backward-entry-at (pos)
  (interactive "d")
  (when-let ((entry-pos (edraw-shape-picker-search-backward-entry-pos pos nil))
             (entry (car entry-pos))
             (range (edraw-shape-picker-lookup-text-entry-range (cdr entry-pos))))
    (goto-char (car range))
    (edraw-shape-picker-delete-entry-common entry)))

;;;;; Kill/Yank Entry

(defun edraw-shape-picker-clipboard-set (entries)
  ;; Completely clone entries
  (edraw-clipboard-set 'shape-picker-entry-list (copy-tree entries)))

(defun edraw-shape-picker-clipboard-not-empty-p ()
  (not (null
        (memq (edraw-clipboard-type)
              '(shape-picker-entry-list
                shape-descriptor-list)))))

(defun edraw-shape-picker-clipboard-get ()
  (copy-tree ;; Completely clone entries
   (pcase (edraw-clipboard-type)
     ('shape-picker-entry-list
      (edraw-clipboard-data))
     ('shape-descriptor-list
      (let ((svg-str (edraw-shape-descriptor-list-to-svg-string
                      (edraw-clipboard-data))))
        (when (and svg-str (not (string-empty-p svg-str)))
          (list
           ;;@todo Make shape constructor function
           (list
            :shape
            :name (read-string (edraw-msg "Shape name: "))
            svg-str))))))))

(defun edraw-shape-picker-copy-entry-at (pos)
  (interactive "d")
  (when-let ((entry (edraw-shape-picker-entry-at pos)))
    (deactivate-mark)
    (edraw-shape-picker-clipboard-set (list entry))
    (message (edraw-msg "Copied %s")
             (edraw-shape-picker-entry-name-for-msg entry))
    entry))

(defun edraw-shape-picker-cut-entry-at (pos)
  (interactive "d")
  (when-let ((entry (edraw-shape-picker-entry-at pos)))
    (deactivate-mark)
    (when (edraw-shape-picker-delete-entry-common entry)
      (edraw-shape-picker-clipboard-set (list entry))
      (message (edraw-msg "Cut %s")
               (edraw-shape-picker-entry-name-for-msg entry))
      (when (<= pos (point-max))
        (goto-char pos)))
    entry))

(defun edraw-shape-picker-paste-entry-at (pos)
  (interactive "d")
  (when-let ((entries (edraw-shape-picker-clipboard-get)))
    (edraw-shape-picker-entries-insert-at entries pos)))

;; Region

(defun edraw-shape-picker-copy-entries-region (beg end)
  (interactive "r")

  (when-let ((entries (edraw-shape-picker-entries-in-region beg end)))
    (edraw-shape-picker-clipboard-set entries)
    (deactivate-mark)
    (message (edraw-msg "Copied %s entries")
             (length entries))))

(defun edraw-shape-picker-cut-entries-region (beg end)
  (interactive "r")

  (when-let ((entries (edraw-shape-picker-entries-in-region beg end)))
    (when (memq edraw-shape-picker-entries entries)
      (error (edraw-msg "Unable to cut root entry")))
    (edraw-shape-picker-clipboard-set entries)
    (edraw-shape-picker-combine-refresh
     (dolist (entry entries)
       (edraw-shape-picker-entry-remove entry)))
    (deactivate-mark)
    (message (edraw-msg "Cut %s entries")
             (length entries))
    (when (<= beg (point-max))
      (goto-char beg))))

;;;;; Move Entry

(defun edraw-shape-picker-move-entry-forward (pos &optional
                                                  no-enter-p no-leave-p)
  (interactive "d")
  (when-let ((entry (edraw-shape-picker-entry-at pos)))
    (let ((ins-point
           ;; Find next insertion point
           (if-let ((parent-index (edraw-shape-picker-entry-parent-index entry))
                    (parent (car parent-index))
                    (index (cdr parent-index)))
               (if (< index
                      (1- (length (edraw-shape-picker-entry-child-entries parent))))
                   ;; ENTRY is not the last child of PARENT
                   (let ((next-entry
                          (nth
                           (1+ index)
                           (edraw-shape-picker-entry-child-entries parent))))
                     (if (and (not no-enter-p)
                              (edraw-shape-picker-entry-container-p next-entry))
                         ;; NEXT-ENTRY is a container type.
                         ;; ENTRY becomes the first child of NEXT-ENTRY
                         (cons next-entry 0)
                       ;; Skip NEXT-ENTRY
                       (cons parent (1+ index))))
                 ;; ENTRY is the last child of PARENT
                 (if no-leave-p
                     nil
                   (if-let ((grandparent-index
                             (edraw-shape-picker-entry-parent-index parent)))
                       ;; ENTRY becomes next sibling of PARENT
                       (cons
                        (car grandparent-index)
                        (1+ (cdr grandparent-index)))
                     ;; ENTRY is the last child of ROOT
                     nil)))
             ;; ENTRY is the ROOT
             nil)))

      (when ins-point
        (message "Under %s"
                 (edraw-shape-picker-entry-name-for-msg (car ins-point)))
        (edraw-shape-picker-combine-refresh
         (edraw-shape-picker-entry-remove entry)
         (edraw-shape-picker-entry-insert (car ins-point) (cdr ins-point)
                                          entry))
        (goto-char (car (edraw-shape-picker-find-entry-text entry)))))))

(defun edraw-shape-picker-move-entry-backward (pos &optional
                                                   no-enter-p no-leave-p)
  (interactive "d")
  (when-let ((entry (edraw-shape-picker-entry-at pos)))
    (let ((ins-point
           ;; Find previous insertion point
           (if-let ((parent-index (edraw-shape-picker-entry-parent-index entry))
                    (parent (car parent-index))
                    (index (cdr parent-index)))
               (if (> index 0)
                   ;; ENTRY is not the first child of PARENT
                   (let ((prev-entry
                          (nth
                           (1- index)
                           (edraw-shape-picker-entry-child-entries parent))))
                     (if (and (not no-enter-p)
                              (edraw-shape-picker-entry-container-p prev-entry))
                         ;; PREV-ENTRY is a container type.
                         ;; ENTRY becomes the last child of PREV-ENTRY
                         (cons prev-entry
                               (length (edraw-shape-picker-entry-child-entries
                                        prev-entry)))
                       ;; Skip PREV-ENTRY
                       (cons parent (1- index))))
                 ;; ENTRY is the first child of PARENT
                 (if no-leave-p
                     nil
                   (if-let ((grandparent-index
                             (edraw-shape-picker-entry-parent-index parent)))
                       ;; ENTRY becomes previous sibling of PARENT
                       (cons
                        (car grandparent-index)
                        (cdr grandparent-index))
                     ;; ENTRY is the first child of ROOT
                     nil)))
             ;; ENTRY is the ROOT
             nil)))

      (when ins-point
        (message "Under %s"
                 (edraw-shape-picker-entry-name-for-msg (car ins-point)))
        (edraw-shape-picker-combine-refresh
         (edraw-shape-picker-entry-remove entry)
         (edraw-shape-picker-entry-insert (car ins-point) (cdr ins-point)
                                          entry))
        (goto-char (car (edraw-shape-picker-find-entry-text entry)))))))

(defun edraw-shape-picker-move-entry-forward-same-level (pos)
  (interactive "d")
  (edraw-shape-picker-move-entry-forward pos t t))

(defun edraw-shape-picker-move-entry-backward-same-level (pos)
  (interactive "d")
  (edraw-shape-picker-move-entry-backward pos t t))


;;;;; Import/Export

(defun edraw-shape-picker-export-section-at (pos)
  (interactive "d")
  (when-let ((entry (edraw-shape-picker-entry-at pos)))
    (when (eq (edraw-shape-picker-entry-type entry) :section)
      (let* ((name (or (edraw-shape-picker-entry-prop-get entry :name) ""))
             (filename-initial
              (concat
               (replace-regexp-in-string "[ \t\n'`\"/:]" "-" (downcase name))
               ".eshapes"))
             (filename
              (read-file-name "Export filename: " nil nil nil filename-initial)))
        (when (or (not (file-exists-p filename))
                  (yes-or-no-p "File already exists. Overwrite?"))
          (edraw-shape-picker-save-entries filename entry))))))

(defun edraw-shape-picker-import-section-at (pos)
  (interactive "d")
  (if-let ((parent-index (edraw-shape-picker-entry-insertion-point-at pos)))
      (let* ((filename (read-file-name "Import .eshapes file: " nil nil t))
             (entry (edraw-shape-picker-load-entries filename)))

        (edraw-shape-picker-entry-insert
         (car parent-index) (cdr parent-index)
         entry))
    (message (edraw-msg "Failed to find insertion point"))))

;;;;; Reset

(defun edraw-shape-picker-reset-entries-to-default ()
  (interactive)
  (edraw-shape-picker-entry-reset-root
   (copy-tree edraw-shape-picker-entries-default)))



;;;; Entry

;;;;; Entry Common

;;;;;; Entry Type

(defun edraw-shape-picker-entry-container-p (entry)
  "Return non-nil if ENTRY can contain child entries."
  (memq (edraw-shape-picker-entry-type entry) '(:section :layout)))

(defun edraw-shape-picker-entry-can-have-name-p (entry)
  (memq (edraw-shape-picker-entry-type entry) '(:section :shape)))

;;;;;; Entry Name

(defun edraw-shape-picker-entry-name-for-msg (entry)
  (let ((type-name
         (substring (symbol-name (edraw-shape-picker-entry-type entry)) 1))
        (entry-name
         (or (and (edraw-shape-picker-entry-can-have-name-p entry)
                  (edraw-shape-picker-entry-prop-get entry :name))
             "<no name>")))
    (format "`%s' %s" entry-name type-name)))

;;;;;; Entry Modification

(defun edraw-shape-picker-on-entry-modified (&rest entries)
  ;;@todo Do not update if entry has not been added yet
  (unless (buffer-modified-p)
    (set-buffer-modified-p t)
    (push (cons t (visited-file-modtime)) buffer-undo-list))

  ;; Update
  (setq edraw-shape-picker-modified-entries
        (append
         edraw-shape-picker-modified-entries
         entries))

  (unless edraw-shape-picker-inhibit-refresh
    (edraw-shape-picker-refresh-on-entry-modified)))

(defun edraw-shape-picker-refresh-on-entry-modified ()
  ;;@todo Update only the entry?
  ;;@todo keep point exact
  (when edraw-shape-picker-modified-entries
    (setq edraw-shape-picker-modified-entries nil)
    (let ((point (point)))
      (edraw-shape-picker-refresh)
      (when (<= (point-min) point (point-max))
        (goto-char point)))))

;;;;;; Entry Basic Accessors

;; (:<type> <:keywordN> <valueN> ... <contents>...)

(defun edraw-shape-picker-entry-type (entry)
  (and (consp entry)
       (keywordp (car entry))
       (car entry)))

(defun edraw-shape-picker-entry-prev-contents (entry)
  "Returns the previous link (cons cell) of the ENTRY's contents list."
  (let ((prev entry))
    (while (and (cddr prev)
                (keywordp (cadr prev)))
      (setq prev (cddr prev)))
    prev))
;;TEST: (edraw-shape-picker-entry-prev-contents '(:tag :p1 11 :p2 22 "DAT1" "DAT2")) => (22 "DAT1" "DAT2")
;;TEST: (edraw-shape-picker-entry-prev-contents '(:tag)) => (:tag)
;;TEST: (edraw-shape-picker-entry-prev-contents '(:tag "DAT1")) => (:tag "DAT1")

(defun edraw-shape-picker-entry-contents-get (entry)
  (cdr (edraw-shape-picker-entry-prev-contents entry)))
;;TEST: (edraw-shape-picker-entry-contents-get '(:tag :p1 11 :p2 22 "DAT1" "DAT2")) => ("DAT1" "DAT2")

(defun edraw-shape-picker-entry-contents-set (entry list)
  (let* ((prev (edraw-shape-picker-entry-prev-contents entry))
         (old-list (cdr prev)))
    (setcdr prev list)
    ;;@todo check attached to buffer
    (edraw-shape-picker-on-entry-modified entry)
    (push (list 'apply #'edraw-shape-picker-entry-contents-set entry old-list) buffer-undo-list)))

(defun edraw-shape-picker-entry-contents-first-get (entry)
  (car (edraw-shape-picker-entry-contents-get entry)))
;;TEST: (edraw-shape-picker-entry-contents-first-get '(:tag :p1 11 :p2 22)) => nil
;;TEST: (edraw-shape-picker-entry-contents-first-get '(:tag :p1 11 :p2 22 "DAT1" "DAT2")) => "DAT1"

(defun edraw-shape-picker-entry-contents-first-set (entry val)
  (let ((prev (edraw-shape-picker-entry-prev-contents entry)))
    (if (cdr prev)
        ;; Change
        (let ((old-val (cadr prev)))
          (setcar (cdr prev) val)
          ;;@todo check attached to buffer
          (edraw-shape-picker-on-entry-modified entry)
          (push (list 'apply #'edraw-shape-picker-entry-contents-first-set entry old-val) buffer-undo-list))
      ;; Add
      (setcdr prev (cons val nil))
      ;;@todo check attached to buffer
      (edraw-shape-picker-on-entry-modified entry)
      (push (list 'apply #'edraw-shape-picker-entry-contents-set entry nil) buffer-undo-list)))
  entry)

(defun edraw-shape-picker-entry-props (entry)
  "Extract the property list part of the ENTRY."
  (cl-loop for curr on (cdr entry) by #'cddr
           while (and (cdr entry)
                      (keywordp (car curr)))
           nconc (list (car curr) (cadr curr))))

(defun edraw-shape-picker-entry-prop-memq (entry prop)
  (let ((curr (cdr entry)))
    (while (and (cdr curr)
                (keywordp (car curr))
                (not (eq (car curr) prop)))
      (setq curr (cddr curr)))
    (when (eq (car curr) prop)
      curr)))

(defun edraw-shape-picker-entry-prop-get (entry prop)
  (cadr (edraw-shape-picker-entry-prop-memq entry prop)))

(defun edraw-shape-picker-entry-prop-put (entry prop val)
  (let ((prev entry)
        (curr (cdr entry)))
    (while (and (cdr curr)
                (keywordp (car curr))
                (not (eq (car curr) prop)))
      (setq prev (cdr curr)
            curr (cddr curr)))
    (if (eq (car curr) prop)
        (let ((old-val (cadr curr)))
          ;; Change Value
          (setcar (cdr curr) val)
          ;;@todo check attached to buffer
          (edraw-shape-picker-on-entry-modified entry)
          (push (list 'apply #'edraw-shape-picker-entry-prop-put entry prop old-val) buffer-undo-list))
      ;; Add new
      (setcdr prev (cons prop (cons val (cdr prev))))
      ;;@todo check attached to buffer
      (edraw-shape-picker-on-entry-modified entry)
      (push (list 'apply #'edraw-shape-picker-entry-prop-remove entry prop) buffer-undo-list)))
  entry)

(defun edraw-shape-picker-entry-prop-remove (entry prop)
  (let ((prev entry)
        (curr (cdr entry)))
    (while (and (cdr curr)
                (keywordp (car curr))
                (not (eq (car curr) prop)))
      (setq prev (cdr curr)
            curr (cddr curr)))
    (when (eq (car curr) prop)
      ;; Remove
      (let ((old-val (cadr curr)))
        (setcdr prev (cddr curr))
        ;;@todo check attached to buffer
        (edraw-shape-picker-on-entry-modified entry)
        (push (list 'apply #'edraw-shape-picker-entry-prop-put entry prop old-val) buffer-undo-list)))) ;;@todo preserve property order?
  entry)

;;;;;; Entry Tree

(defun edraw-shape-picker-entry-reset-root (new-root-entry)
  (when (eq (edraw-shape-picker-entry-type new-root-entry) :section)
    (let ((old-root-entry edraw-shape-picker-entries))
      (setq-local edraw-shape-picker-entries new-root-entry)
      (edraw-shape-picker-on-entry-modified new-root-entry)
      (push (list 'apply #'edraw-shape-picker-entry-reset-root old-root-entry)
            buffer-undo-list))))

(defun edraw-shape-picker-entry-child-entries (entry)
  (when (edraw-shape-picker-entry-container-p entry)
    ;; Only :section, :layout
    (edraw-shape-picker-entry-contents-get entry)))

(defun edraw-shape-picker-entry-parent (target-entry &optional root)
  (unless root (setq root edraw-shape-picker-entries))
  (let ((children (edraw-shape-picker-entry-child-entries root)))
    (if (memq target-entry children)
        root
      (cl-loop for child in children
               for found = (edraw-shape-picker-entry-parent target-entry child)
               when found
               return found))))

(defun edraw-shape-picker-entry-parent-index (target-entry &optional root)
  "Return ENTRY's parent and index.

(parent . index)"
  (unless root (setq root edraw-shape-picker-entries))
  (cl-loop for index from 0
           for child in (edraw-shape-picker-entry-child-entries root)
           when (eq child target-entry) return (cons root index)
           for found = (edraw-shape-picker-entry-parent-index target-entry
                                                              child)
           when found return found))

(defun edraw-shape-picker-entry-path (target-entry &optional root)
  "Return path from ROOT to TARGET-ENTRY.

((root . index) (ancestor . index) ... (parent . index))."
  (unless root (setq root edraw-shape-picker-entries))
  (cl-loop for index from 0
           for child in (edraw-shape-picker-entry-child-entries root)
           when (eq child target-entry) return (cons (cons root index) nil)
           for found = (edraw-shape-picker-entry-path target-entry child)
           when found return (cons (cons root index) found)))

(defun edraw-shape-picker-entry-remove-child (parent child)
  (when (and parent child
             (edraw-shape-picker-entry-container-p parent))
    (let ((prev (edraw-shape-picker-entry-prev-contents parent))
          (index 0))
      (while (and (cdr prev)
                  (not (eq (cadr prev) child)))
        (setq prev (cdr prev)
              index (1+ index)))
      (when (eq (cadr prev) child)
        (setcdr prev (cddr prev))
        ;;@todo check attached to buffer
        (edraw-shape-picker-on-entry-modified parent)
        (push (list 'apply #'edraw-shape-picker-entry-insert parent index child) buffer-undo-list)
        ;; Return removed entry
        child))))

(defun edraw-shape-picker-entry-remove (entry &optional root)
  (unless root (setq root edraw-shape-picker-entries))
  (edraw-shape-picker-entry-remove-child
   (edraw-shape-picker-entry-parent entry root)
   entry))

(defun edraw-shape-picker-entry-remove-children (parent index count)
  (when (and parent
             (> count 0)
             (edraw-shape-picker-entry-container-p parent))
    (let* ((prev (nthcdr index (edraw-shape-picker-entry-prev-contents parent))))
      (when prev
        (let ((removed-children (cdr prev))
              (prev-last-removed-child (nthcdr count prev)))
          (setcdr prev (cdr prev-last-removed-child))
          (setcdr prev-last-removed-child nil)
          ;;@todo check attached to buffer
          (edraw-shape-picker-on-entry-modified parent)
          (push (list 'apply #'edraw-shape-picker-entries-insert parent index removed-children) buffer-undo-list)
          ;; Return removed entries
          removed-children)))))

(defun edraw-shape-picker-entry-insert (parent index child)
  "The CHILD becomes the PARENT's INDEX-th child."
  ;;@todo check CHILD already has parent?
  (when (and parent child
             (edraw-shape-picker-entry-container-p parent))
    (let ((prev (nthcdr index (edraw-shape-picker-entry-prev-contents parent))))
      (setcdr prev (cons child (cdr prev)))
      ;;@todo check attached to buffer
      (edraw-shape-picker-on-entry-modified parent)
      (push (list 'apply #'edraw-shape-picker-entry-remove-child parent child) buffer-undo-list)
      ;; Return inserted entry
      child)))

(defun edraw-shape-picker-entries-insert (parent index children)
  "The first of CHILDREN becomes the PARENT's INDEX-th child.

CHILDREN list is not copied. At the end of Children, the
subsequent entry will be connected."
  ;;@todo check CHILDREN already has parent?
  (when (and parent children
             (edraw-shape-picker-entry-container-p parent))
    (let ((prev (nthcdr index (edraw-shape-picker-entry-prev-contents parent)))
          (count (length children)))
      (setcdr (last children) (cdr prev))
      (setcdr prev children)
      ;;@todo check attached to buffer
      (edraw-shape-picker-on-entry-modified parent)
      (push (list 'apply #'edraw-shape-picker-entry-remove-children parent index count) buffer-undo-list)
      ;; Return inserted entry
      children)))

(defun edraw-shape-picker-entries-insert-at (entries pos)
  (if-let ((parent-index (edraw-shape-picker-entry-insertion-point-at pos)))
      (edraw-shape-picker-entries-insert (car parent-index) (cdr parent-index)
                                         entries)
    (message (edraw-msg "Failed to find insertion point"))))

;;

(defun edraw-shape-picker-entry-next-sibling-or-upper (entry &optional root)
  "Return ENTRY's next sibling or ancestor's next sibling.

ROOT is the top level entry of the tree containing ENTRY."
  (when entry
    (cl-loop for p-i = (edraw-shape-picker-entry-parent-index entry root)
             while p-i
             for parent = (car p-i)
             for index = (cdr p-i)
             for next-sibling = (nth (1+ index)
                                     (edraw-shape-picker-entry-child-entries
                                      parent))
             when next-sibling return next-sibling
             do (setq entry parent))))

(defun edraw-shape-picker-entry-next (entry &optional root)
  "Return next entry of ENTRY.

ROOT is the top level entry of the tree containing ENTRY."
  (when entry
    (or
     ;; First child
     (car (edraw-shape-picker-entry-child-entries entry))
     ;; Next siblig
     (edraw-shape-picker-entry-next-sibling-or-upper entry root))))


;;;;; Section

;;;;; Layout

;;;;; Shape

(defun edraw-shape-picker-shape-entry-name-get (entry)
  "Return the name of the shape ENTRY."
  (edraw-shape-picker-entry-prop-get entry :name))

(defun edraw-shape-picker-shape-entry-shape-get (entry)
  "Return the shape definition of the shape ENTRY."
  (edraw-shape-picker-entry-contents-first-get entry))

(defun edraw-shape-picker-shape-entry-shape-set (entry shape)
  "Change the SHAPE definition of the shape ENTRY."
  (edraw-shape-picker-entry-contents-first-set entry shape))

(defconst edraw-shape-picker-thumbnail-param-names
  '(width height max-width max-height
          padding background foreground-selected margin))

(defun edraw-shape-picker-shape-entry-thumbnail-params (entry)
  (let (;; from custom variables
        (params (mapcar
                 (lambda (name)
                   (cons
                    name
                    (symbol-value
                     (intern (format "edraw-shape-picker-thumbnail-%s" name)))))
                 edraw-shape-picker-thumbnail-param-names)))

    ;; from ancestors
    (dolist (ancestor (mapcar #'car ;;discard index
                              (edraw-shape-picker-entry-path entry)))
      (dolist (name edraw-shape-picker-thumbnail-param-names)
        (when-let ((prop (edraw-shape-picker-entry-prop-memq
                          ancestor
                          (intern (format ":thumbnail-%s" name)))))
          (setf (alist-get name params) (cadr prop)))))

    ;; from shape entry
    (dolist (name edraw-shape-picker-thumbnail-param-names)
      (when-let ((prop (edraw-shape-picker-entry-prop-memq
                        entry
                        (intern (format ":%s" name)))))
        (setf (alist-get name params) (cadr prop))))

    params))

;;;; Thumbnail Image

(defun edraw-shape-picker-shape-to-svg-node-list (shape)
  (cond
   ((stringp shape)
    (dom-children (edraw-svg-decode (concat "<g>" shape "</g>") nil)))
   ;;@todo Convert shape-descriptor to svg
   ((edraw-dom-element-p shape) (list shape))))

(defun edraw-shape-picker-create-thumbnail-image (entry &optional selected)
  (let* ((params (edraw-shape-picker-shape-entry-thumbnail-params entry))
         (shape (edraw-shape-picker-shape-entry-shape-get entry))
         (svg-node-list (edraw-shape-picker-shape-to-svg-node-list shape)))
    (when svg-node-list
      (svg-image (edraw-svg-shape-thumbnail
                  ;; to single element
                  (if (<= (length svg-node-list) 1)
                      (car svg-node-list)
                    (apply #'dom-node 'g nil svg-node-list))
                  (alist-get 'width params)
                  (alist-get 'height params)
                  (alist-get 'padding params)
                  (alist-get 'background params)
                  (when selected
                    (alist-get 'foreground-selected params))
                  (alist-get 'max-width params)
                  (alist-get 'max-height params))
                 :ascent 'center
                 :margin (alist-get 'margin params)))))

;;;; Shape Selection

(defun edraw-shape-picker-selected-shape-entry ()
  edraw-shape-picker-selected-shape-entry)

(defun edraw-shape-picker-selected-shape ()
  (when edraw-shape-picker-selected-shape-entry
    (edraw-shape-picker-shape-entry-shape-get
     edraw-shape-picker-selected-shape-entry)))

(defun edraw-shape-picker-ensure-deselect-shape (shape)
  (when (eq edraw-shape-picker-selected-shape-entry shape)
    (edraw-shape-picker-deselect-shape)))

(defun edraw-shape-picker-deselect-shape ()
  (when edraw-shape-picker-selected-shape-entry
    ;; Update Image
    (when-let ((range (edraw-shape-picker-find-entry-text
                       edraw-shape-picker-selected-shape-entry)))
      (with-silent-modifications
        (put-text-property (car range) (cdr range)
                           'display
                           (edraw-shape-picker-create-thumbnail-image
                            edraw-shape-picker-selected-shape-entry
                            nil))))
    ;; Change Variables
    (setq-local edraw-shape-picker-selected-shape-entry nil)
    ;; Notify Changes
    (edraw-shape-picker-notify 'deselect)))

(defun edraw-shape-picker-select-shape-at (pos &optional buffer)
  (interactive (edraw-shape-picker-interactive-point-buffer))
  (with-current-buffer (or buffer (current-buffer))
    (when-let ((entry (edraw-shape-picker-entry-at pos))
               (range (edraw-shape-picker-lookup-text-entry-range pos))
               (beg (car range))
               (end (cdr range)))
      (edraw-shape-picker-deselect-shape)
      ;; Update Image
      (with-silent-modifications
        (put-text-property (car range) (cdr range)
                           'display
                           (edraw-shape-picker-create-thumbnail-image entry t)))
      ;; Change Variables
      (setq-local edraw-shape-picker-selected-shape-entry entry)
      ;; Message
      (message
       (edraw-msg "Select %s")
       (or (edraw-shape-picker-shape-entry-name-get entry)
           (edraw-msg "<no name>")))
      ;; Notify Changes
      (edraw-shape-picker-notify
       'select
       (edraw-shape-picker-shape-entry-shape-get entry)))))


;;;; Buffer Contents

(defun edraw-shape-picker-insert-text (str)
  (insert (propertize str
                      'read-only t
                      'front-sticky t)))

;;;;; Create

(defvar-local edraw-shape-picker-section-level 0)

(defun edraw-shape-picker-make-buffer-contents ()
  (let ((edraw-shape-picker-section-level 0))
    (with-silent-modifications
      (erase-buffer)
      (edraw-shape-picker-insert-section edraw-shape-picker-entries)
      (unless (bolp)
        (edraw-shape-picker-insert-text "\n"))
      (goto-char (point-min)))))

(defun edraw-shape-picker-insert-entries (entries)
  (dolist (entry entries)
    (edraw-shape-picker-insert-entry entry)))

(defun edraw-shape-picker-insert-entry (entry)
  (pcase (edraw-shape-picker-entry-type entry)
    (:section (edraw-shape-picker-insert-section entry))
    (:layout  (edraw-shape-picker-insert-layout entry))
    (:shape   (edraw-shape-picker-insert-shape entry))))

(defun edraw-shape-picker-insert-section (entry)
  ;; (:section
  ;;     :name <string>
  ;;     <entry>...)
  (unless (bolp)
    (edraw-shape-picker-insert-text "\n"))
  (let ((edraw-shape-picker-section-level
         (1+ edraw-shape-picker-section-level))
        (name (edraw-shape-picker-entry-prop-get entry :name)))
    (edraw-shape-picker-insert-text
     (propertize
      (concat
       ;; *
       (propertize
        (make-string edraw-shape-picker-section-level ?*)
        'display (concat (make-string (1- edraw-shape-picker-section-level) ? )
                         "\u29bf"))
       ;;  <name>\n
       (propertize
        (concat (or (concat " " name) "") "\n")))
      'edraw-shape-picker-entry entry
      'font-lock-face 'edraw-shape-picker-heading
      'pointer 'arrow))

    (edraw-shape-picker-insert-entries
     (edraw-shape-picker-entry-contents-get entry)))

  (unless (bolp)
    (edraw-shape-picker-insert-text "\n")))

(defun edraw-shape-picker-insert-layout (entry)
  ;; (:layout
  ;;     :thumbnail-width <integer>
  ;;     :thumbnail-height <integer>
  ;;     :thumbnail-max-width <integer>
  ;;     :thumbnail-max-height <integer>
  ;;     :thumbnail-padding <padding-spec>
  ;;     :thumbnail-background <svg-attrs-spec>
  ;;     :thumbnail-foreground-selected <svg-attrs-spec>
  ;;     :thumbnail-margin <margin-spec>
  ;;     <entry>...)
  ;;
  ;; see: `edraw-shape-picker-thumbnail-*' variables.
  (edraw-shape-picker-insert-entries
   (edraw-shape-picker-entry-contents-get entry)))

(defun edraw-shape-picker-insert-shape (entry)
  ;; (:shape
  ;;     :width <integer>
  ;;     :height <integer>
  ;;     :max-width <integer>
  ;;     :max-height <integer>
  ;;     :padding <padding-spec>
  ;;     :background <svg-attrs-spec>
  ;;     :foreground-selected <svg-attrs-spec>
  ;;     :margin <margin-spec>
  ;;     <shape-def>)
  ;;
  ;; see: `edraw-shape-picker-thumbnail-*' variables.
  ;;
  ;; <shape-def> :
  ;;     <svg-string>
  ;;     <svg-node-list>
  ;;     ;;<shape-descriptor-list>

  ;; Space
  (when (and edraw-shape-picker-string-between-thumbnails
             (not (bolp)))
    (edraw-shape-picker-insert-text edraw-shape-picker-string-between-thumbnails))

  ;; Thumbnail
  (let ((name (edraw-shape-picker-shape-entry-name-get entry)))
    (edraw-shape-picker-insert-text
     (propertize
      (format " [%s] " (or name ""))
      'display (edraw-shape-picker-create-thumbnail-image
                entry
                ;; selected?
                (eq (edraw-shape-picker-selected-shape-entry) entry))
      'rear-nonsticky t
      'keymap edraw-shape-picker-thumbnail-map
      'pointer 'hand
      'edraw-shape-picker-entry entry
      'help-echo name
      ))))

;;;;; Detect Entry

(defun edraw-shape-picker-entry-at (pos)
  (get-text-property pos 'edraw-shape-picker-entry))

(defun edraw-shape-picker-lookup-text-prop-range (pos prop val)
  (cons
   (if (or (= pos (point-min))
           (not (eq (get-text-property (1- pos) prop)
                    val)))
       pos
     (previous-single-property-change pos prop nil (point-min)))
   (next-single-property-change pos prop)))

(defun edraw-shape-picker-lookup-text-entry-range (pos)
  (when-let ((entry (get-text-property pos 'edraw-shape-picker-entry)))
    (edraw-shape-picker-lookup-text-prop-range pos 'edraw-shape-picker-entry
                                               entry)))

(defun edraw-shape-picker-shape-entry-at (pos)
  (when-let ((entry (edraw-shape-picker-entry-at pos)))
    (when (eq (edraw-shape-picker-entry-type entry) :shape)
      entry)))

(defun edraw-shape-picker-find-entry-text (entry)
  (save-excursion
    (goto-char (point-min))
    (when-let ((match-data (text-property-search-forward
                            'edraw-shape-picker-entry entry #'eq)))
      (cons
       (prop-match-beginning match-data)
       (prop-match-end match-data)))))

(defun edraw-shape-picker-entry-insertion-point-at (pos)
  "Detect entry insertion point at POS."
  (when-let ((entry-pos (edraw-shape-picker-search-backward-entry-pos pos t))) ;;include POS
    (let* ((entry (car entry-pos))
           (pos (cdr entry-pos))
           (range (edraw-shape-picker-lookup-text-entry-range pos))
           (parent-index (edraw-shape-picker-entry-parent-index entry))
           (parent (car parent-index))
           (index (cdr parent-index)))
      ;; POS points to ENTRY
      (if (= (car range) pos)
          ;; Insert before ENTRY
          ;; |* SECTION
          ;; [shape][shape]|[shape]
          parent-index ;;null if ENTRY is root
        (pcase (edraw-shape-picker-entry-type entry)
          (:section
           ;; Insert as first child of ENTRY
           ;; * SECTION|
           ;; * SECTION\n|\n
           (cons entry 0))
          (:shape
           ;; after the entry
           ;; [shape][shape][SH|APE]
           ;; [shape][shape][SHAPE]|
           ;; [shape][SHAPE] | [shape]
           (cons parent (1+ index)))))))) ;;assert (not (null parent))

(defun edraw-shape-picker-search-backward-entry-pos (pos include-pos-p)
  (unless include-pos-p
    (setq pos (1- pos)))
  (let (entry)
    (while (and (>= pos (point-min))
                (null
                 (setq entry
                       (get-text-property pos 'edraw-shape-picker-entry))))
      (setq pos (1- pos))) ;;@todo use previous-property-change?
    (when entry
      (cons entry pos))))

(defun edraw-shape-picker-entries-in-region (beg end &optional root)
  (when (< beg end)
    ;; First, identify the first and last entries
    (let* ((first-entry
            (let ((entry
                   (or
                    ;; Pointed
                    (get-text-property beg 'edraw-shape-picker-entry)
                    ;; After
                    (let ((pos (next-single-property-change
                                beg 'edraw-shape-picker-entry nil end)))
                      (when (< pos end)
                        (get-text-property pos 'edraw-shape-picker-entry))))))
              ;; Include layout container
              ;; (:section  ENTRY=>(:layout (:layout (:layout <ENTRY> ...)))
              (while (let* ((parent-index
                             (edraw-shape-picker-entry-parent-index entry root))
                            (parent (car parent-index))
                            (index (cdr parent-index)))
                       (when (and parent
                                  (= index 0)
                                  (eq (edraw-shape-picker-entry-type parent)
                                      :layout))
                         (setq entry (car parent-index))
                         t)))
              entry))
           (last-entry
            (or
             (get-text-property (1- end) 'edraw-shape-picker-entry)
             (let ((pos (previous-single-property-change
                         end 'edraw-shape-picker-entry nil beg)))
               (when (> pos beg)
                 (get-text-property (1- pos) 'edraw-shape-picker-entry))))))

      ;; Extracts entries whose entire subtree, including descendants,
      ;; is contained within the region.
      ;;
      ;; Even if the section heading is included in the region, if
      ;; there are descendants that are not included in the region,
      ;; that section is not extracted.
      ;; Cutting such a section will cause the remaining descendants
      ;; to have no parents. On the other hand, if such descendants
      ;; are also cut at once, it will be cut extra large than the
      ;; region.
      ;;
      ;; The result will be multiple subtrees.
      (when (and first-entry last-entry)
        (let ((last-entry-path (edraw-shape-picker-entry-path last-entry root))
              (entry first-entry)
              entries
              (quit nil))
          (while (and entry (not quit))
            (when (eq entry last-entry)
              (setq quit t))
            (if ;; ENTRY tree is completely containd within the region?
                (or
                 (not (edraw-shape-picker-entry-container-p entry))
                 ;; ENTRY not contains LAST-ENTRY
                 ;; (ENTRY not on path from LAST-ENTRY to the root)
                 (not (seq-find (lambda (p-i) (eq (car p-i) entry)) last-entry-path))
                 ;; The last entry of ENTRY tree equals LAST-ENTRY
                 (setq quit (eq
                             (let ((last-in-entry entry)
                                   children)
                               (while (setq children
                                            (edraw-shape-picker-entry-child-entries last-in-entry))
                                 (setq last-in-entry (car (last children))))
                               last-in-entry)
                             last-entry)))
                ;; Accept ENTRY and go next sibling or ancestor sibling
                ;; Skip children
                (progn
                  (push entry entries)
                  (unless quit
                    (setq entry
                          (edraw-shape-picker-entry-next-sibling-or-upper entry))))
              ;; Reject ENTRY and go first child or next sibling or ancestor sibling
              (unless quit
                (setq entry (edraw-shape-picker-entry-next entry)))))
          (nreverse entries))))))


;;;; File I/O

(defun edraw-shape-picker-write-entries (buffer entries)
  (with-current-buffer (or buffer (current-buffer))
    (insert ";; Custom Shape Definition for edraw-shape-picker\n")
    (pp (list
         (cons :format 'edraw-shape-picker-entries)
         (cons :version 1)
         (cons :entries entries))
        (current-buffer))))

(defun edraw-shape-picker-save-entries (file entries)
  (with-temp-file file
    (edraw-shape-picker-write-entries (current-buffer) entries)))
;;TEST (edraw-shape-picker-save-entries edraw-shape-picker-entries-file edraw-shape-picker-entries)

(defun edraw-shape-picker-read-entries (buffer)
  (let* ((sexp (read (or buffer (current-buffer))))
         (format (alist-get :format sexp))
         (version (alist-get :version sexp))
         (entries (alist-get :entries sexp)))
    (unless (eq format 'edraw-shape-picker-entries)
      (error "Invalid file format: %s" format))
    (unless (eq version 1)
      (error "Invalid file version: %s" version))
    (unless (eq (edraw-shape-picker-entry-type entries) :section)
      (error "Invalid top level entry type"))
    entries))

(defun edraw-shape-picker-load-entries (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (edraw-shape-picker-read-entries (current-buffer))))
;;TEST (edraw-shape-picker-load-entries edraw-shape-picker-entries-file)


;;;; Picker File Mode

(defvar edraw-shape-picker-file-mode-map
  (let ((km (make-sparse-keymap)))
    km))

(defun edraw-shape-picker-file-mode ()
  "Major mode for visually editing .eshapes files.

Like `hexl-mode', open a .eshapes file and then launch this
mode to use it. Run `edraw-shape-picker-file-mode-exit' to return
to the original mode.

The following commands are available in buffer:

\\{edraw-shape-picker-ui-mode-map}

The following commands are available on thumbnails:

\\{edraw-shape-picker-thumbnail-map}"
  (interactive)

  (delay-mode-hooks
    (unless (eq major-mode 'edraw-shape-picker-file-mode)
      (major-mode-suspend)

      ;; Visualize
      (let ((entries (progn
                       ;; Parse buffer
                       (goto-char (point-min))
                       (edraw-shape-picker-read-entries (current-buffer))))
            (modified (buffer-modified-p)))
        ;; When parsing is successful

        ;; Call parent mode (and kill all local variables)
        (edraw-shape-picker-ui-mode)

        ;; Discard undo list
        (setq buffer-undo-list nil)

        ;; Disable auto save
        (auto-save-mode -1)

        ;; Refresh buffer contents
        (edraw-shape-picker-set-local-entries entries nil)
        (edraw-shape-picker-make-buffer-contents) ;;silent modification

        (restore-buffer-modified-p modified))

      ;;  Key maps
      (set-keymap-parent edraw-shape-picker-file-mode-map (current-local-map))
      (use-local-map edraw-shape-picker-file-mode-map)
      ;;  Mode
      (setq-local mode-name "EShapes")
      (setq-local major-mode 'edraw-shape-picker-file-mode)
      ;;  Hooks
      (add-hook 'write-contents-functions #'edraw-shape-picker-file-mode--save-buffer nil t)
      (add-hook 'change-major-mode-hook #'edraw-shape-picker-file-mode--maybe-devisualize nil t)
      (setq-local revert-buffer-function #'edraw-shape-picker-file-mode--revert-buffer-function)
      ))
  (run-mode-hooks 'edraw-shape-picker-file-mode-hook))

(defun edraw-shape-picker-file-mode--exit ()
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (edraw-shape-picker-file-mode--devisualize)
    (restore-buffer-modified-p modified))

  (remove-hook 'write-contents-functions #'edraw-shape-picker-file-mode--save-buffer t)
  (remove-hook 'change-major-mode-hook #'edraw-shape-picker-file-mode--maybe-devisualize t)
  (setq-local revert-buffer-function nil))

(defun edraw-shape-picker-file-mode-exit ()
  (interactive)
  (edraw-shape-picker-file-mode--exit)
  (major-mode-restore))

(defun edraw-shape-picker-file-mode--maybe-devisualize ()
  (when (y-or-n-p (edraw-msg "Convert contents back to text format? "))
    (edraw-shape-picker-file-mode--exit)))

(defun edraw-shape-picker-file-mode--revert-buffer-function (_ignore-auto _noconfirm)
  ;; see: hexl-revert-buffer-function
  (let (revert-buffer-function)
    (revert-buffer nil nil t)
    (remove-hook 'change-major-mode-hook #'edraw-shape-picker-file-mode--maybe-devisualize t)
    (setq major-mode 'fundamental-mode)
    (edraw-shape-picker-file-mode)))

(defvar edraw-shape-picker-file-mode--in-save-buffer nil)

(defun edraw-shape-picker-file-mode--save-buffer ()
  (cond
   (edraw-shape-picker-file-mode--in-save-buffer
    nil) ;; Ignore
   ((not (buffer-modified-p))
    (message "(No changes need to be saved)")
    t) ;; Saved
   (t
    ;; Backup current buffer to temp buffer
    (let* ((buffer (current-buffer))
           (buffer-start (point-min))
           (buffer-end (point-max))
           (buffer-len (- buffer-end buffer-start))
           (inhibit-read-only t))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer))
              (temp-buffer-start (point))
              (temp-buffer-end (+ (point) buffer-len)))
          (insert-buffer-substring buffer buffer-start buffer-end)
          ;; Save devisualized buffer
          (with-current-buffer buffer
            (edraw-shape-picker-file-mode--devisualize)
            (let ((edraw-shape-picker-file-mode--in-save-buffer t))
              (save-buffer)) ;;Recursive!!
            ;; Restore(Visualize)
            (let ((modified (buffer-modified-p)))
              (delete-region (point-min) (point-max))
              (insert-buffer-substring temp-buffer temp-buffer-start temp-buffer-end)
              (restore-buffer-modified-p modified))))))
    t))) ;; Saved

(defun edraw-shape-picker-file-mode--devisualize ()
  (delete-region (point-min) (point-max))
  (edraw-shape-picker-write-entries (current-buffer) edraw-shape-picker-entries))


(provide 'edraw-shape-picker)
;;; edraw-shape-picker.el ends here
