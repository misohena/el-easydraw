;;; edraw-color.el --- Color Library                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

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

(require 'seq)
(require 'subr-x)
(require 'eieio)
(require 'color)

(defclass edraw-color ()
  ((r :initarg :r :accessor edraw-color-r)
   (g :initarg :g :accessor edraw-color-g)
   (b :initarg :b :accessor edraw-color-b)
   (a :initarg :a :accessor edraw-color-a :initform 1.0)))

(defun edraw-color-ensure (color)
  (cond
   ((cl-typep color 'edraw-color) color)
   ((stringp color)
    (let ((result (edraw-color-from-string color)))
      (unless result
        (error "Invalid color `%s'" color))
      result))
   (t (error "Invalid color `%s'" color))))

(defun edraw-color-rgb (r g b &optional a)
  (edraw-color
   :r (/ r 255.0)
   :g (/ g 255.0)
   :b (/ b 255.0)
   :a (or a 1.0)))

(defun edraw-color-f (r g b &optional a)
  (edraw-color
   :r (float r)
   :g (float g)
   :b (float b)
   :a (float (or a 1.0))))

(defun edraw-color-from-hsl (h s l &optional a)
  "Create an edraw-color object from HSL color components.

  H : Hue (degrees)
  S : Saturation (0.0 to 1.0)
  L : Lightness (0.0 to 1.0)
  A : Opacity (0.0 to 1.0 : default 1.0)"
  ;; Same as (color-hsl-to-rgb (/ h 360.0) s l)
  (setq h (mod h 360.0)) ;; 0 <= h < 360
  (let* ((c (- 1.0 (abs (* 2 (- l 0.5)))))
         (sc (* s c))
         (m (- l (* 0.5 sc))))
    (edraw-color-f
     (+ m (* sc (edraw-color-hue-red-level--no-mod h)))
     (+ m (* sc (edraw-color-hue-red-level--no-mod (- h 120.0))))
     (+ m (* sc (edraw-color-hue-red-level--no-mod (+ h 120.0))))
     a)))

(defun edraw-color-from-hsv (h s v &optional a)
  "Create an edraw-color object from HSV (HSB) color components.

  H : Hue (degrees)
  S : Saturation (0.0 to 1.0)
  V : Value/Brightness (0.0 to 1.0)
  A : Opacity (0.0 to 1.0 : default 1.0)"
  (setq h (mod h 360.0)) ;; 0 <= h < 360
  (let* ((is (- 1.0 s))
         (vis (* v is))
         (vs (* v s)))
    ;; v * ((1-s)*[1,1,1] + s*edraw-color-from-hue(h))
    (edraw-color-f
     (+ vis (* vs (edraw-color-hue-red-level--no-mod h)))
     (+ vis (* vs (edraw-color-hue-red-level--no-mod (- h 120.0))))
     (+ vis (* vs (edraw-color-hue-red-level--no-mod (+ h 120.0))))
     a)))
;; (edraw-to-string (edraw-color-from-hsv 0 0 0)) => "#000000"
;; (edraw-to-string (edraw-color-from-hsv 0 1 1)) => "#ff0000"

(defun edraw-color-from-hue (hue)
  "Return the color corresponding to HUE (degrees)."
  (setq hue (mod hue 360.0)) ;; 0 <= h < 360
  (edraw-color-f
   (edraw-color-hue-red-level--no-mod hue)
   (edraw-color-hue-red-level--no-mod (- hue 120.0))
   (edraw-color-hue-red-level--no-mod (+ hue 120.0))))
;; TEST: (edraw-to-string (edraw-color-from-hue -60)) => "#ff00ff"
;; TEST: (edraw-to-string (edraw-color-from-hue -30)) => "#ff0080"
;; TEST: (edraw-to-string (edraw-color-from-hue 0)) => "#ff0000"
;; TEST: (edraw-to-string (edraw-color-from-hue 30)) => "#ff8000"
;; TEST: (edraw-to-string (edraw-color-from-hue 60)) => "#ffff00"
;; TEST: (edraw-to-string (edraw-color-from-hue 120)) => "#00ff00"
;; TEST: (edraw-to-string (edraw-color-from-hue 180)) => "#00ffff"
;; TEST: (edraw-to-string (edraw-color-from-hue 240)) => "#0000ff"
;; TEST: (edraw-to-string (edraw-color-from-hue 300)) => "#ff00ff"
;; TEST: (edraw-to-string (edraw-color-from-hue 360)) => "#ff0000"

(defun edraw-color-hue-red-level (hue)
  "Return the intensity of red corresponding to HUE (degrees), between
0.0 and 1.0."
  (edraw-color-hue-red-level--no-mod (mod hue 360.0)))

(defun edraw-color-hue-red-level--no-mod (hue)
  "Return the intensity of red corresponding to HUE (degrees), between
0.0 and 1.0.
HUE must be between -180.0 and 540.0."
  ;; -180+0 <= hue <= 360+180 to -180 <= hue <= 180
  (when (> hue 180.0) (setq hue (- hue 360.0)))
  (when (< hue 0.0) (setq hue (- hue))) ;; abs(hue)
  ;; 0 <= hue <= 180
  (cond
   ((<= hue 60.0) 1.0)
   ((< hue 120.0) (/ (- 120.0 hue) 60.0))
   (t 0.0)))

(defun edraw-color-equal-p (a b)
  (equal a b))

(cl-defmethod edraw-change-r ((color edraw-color) r)
  (edraw-color
   :r (float r)
   :g (oref color g)
   :b (oref color b)
   :a (oref color a)))

(cl-defmethod edraw-change-g ((color edraw-color) g)
  (edraw-color
   :r (oref color r)
   :g (float g)
   :b (oref color b)
   :a (oref color a)))

(cl-defmethod edraw-change-b ((color edraw-color) b)
  (edraw-color
   :r (oref color r)
   :g (oref color g)
   :b (float b)
   :a (oref color a)))

(cl-defmethod edraw-change-a ((color edraw-color) a)
  (edraw-color
   :r (oref color r)
   :g (oref color g)
   :b (oref color b)
   :a (float a)))

(cl-defmethod edraw-hue ((color edraw-color))
  (with-slots (r g b) color
    (let ((d (- (max r g b) (min r g b))))
      (mod
       (cond
        ((= d 0)
         0)
        ((and (>= r b) (>= r g))
         (+ (/ (* 60.0 (- g b)) d) 0))
        ((and (>= g r) (>= g b))
         (+ (/ (* 60.0 (- b r)) d) 120))
        ((and (>= b r) (>= b g))
         (+ (/ (* 60.0 (- r g)) d) 240))
        (t 0))
       360.0))))

(cl-defmethod edraw-saturation ((color edraw-color))
  (with-slots (r g b) color
    (let ((max (max r g b))
          (min (min r g b)))
      (if (= max 0)
          0
        (/ (- max min) max)))))

(cl-defmethod edraw-brightness ((color edraw-color))
  (with-slots (r g b) color
    (max r g b)))

(cl-defmethod edraw-relative-luminance ((color edraw-color))
  "Return the relative luminance of COLOR.
See https://en.wikipedia.org/wiki/Relative_luminance"
  (with-slots (r g b) color
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(cl-defmethod edraw-length-rgb ((color edraw-color))
  (with-slots (r g b) color
    (sqrt (+ (* r r) (* g g) (* b b)))))

(cl-defmethod edraw-blend-normal ((c0 edraw-color) (c1 edraw-color))
  (let* ((r0 (oref c0 r)) (g0 (oref c0 g)) (b0 (oref c0 b)) (a0 (oref c0 a))
         (r1 (oref c1 r)) (g1 (oref c1 g)) (b1 (oref c1 b)) (a1 (oref c1 a))
         (a (- 1.0 (* (- 1.0 a0) (- 1.0 a1)))))
      (if (= a 0)
          (edraw-color :r 0.0 :g 0.0 :b 0.0 :a 0.0)
        (edraw-color
         :r (/ (+ (* a1 r1) (* (- 1.0 a1) a0 r0)) a)
         :g (/ (+ (* a1 g1) (* (- 1.0 a1) a0 g0)) a)
         :b (/ (+ (* a1 b1) (* (- 1.0 a1) a0 b0)) a)
         :a a))))

(cl-defmethod edraw-interpolate ((c0 edraw-color) (c1 edraw-color) alpha)
  (cond
   ((<= alpha 0) c0)
   ((>= alpha 1) c1)
   (t
    (let* ((r0 (oref c0 r)) (g0 (oref c0 g)) (b0 (oref c0 b)) (a0 (oref c0 a))
           (r1 (oref c1 r)) (g1 (oref c1 g)) (b1 (oref c1 b)) (a1 (oref c1 a))
           (a (+ (* (- 1.0 alpha) a0) (* (float alpha) a1))))
      (if (= a 0)
          (edraw-color :r 0.0 :g 0.0 :b 0.0 :a 0.0)
        (edraw-color
         :r (/ (+ (* (- 1.0 alpha) r0 a0) (* (float alpha) r1 a1)) a)
         :g (/ (+ (* (- 1.0 alpha) g0 a0) (* (float alpha) g1 a1)) a)
         :b (/ (+ (* (- 1.0 alpha) b0 a0) (* (float alpha) b1 a1)) a)
         :a a))))))

(defun edraw-color-from-1d-gradient (offset colors)
  (let* ((num-intervals (1- (length colors)))
         (index (min (floor (* offset num-intervals)) (1- num-intervals)))
         (interval-start (/ (float index) num-intervals))
         (alpha (max 0.0 (min 1.0 (* (- offset interval-start) num-intervals)))))
    (edraw-interpolate (elt colors index) (elt colors (1+ index)) alpha)))

(defun edraw-color-from-2d-gradient (offset-x offset-y colors-h colors-v)
  (let ((color-x (edraw-color-from-1d-gradient offset-x colors-h))
        (color-y (edraw-color-from-1d-gradient offset-y colors-v)))
    (edraw-blend-normal color-x color-y)))

;;;; To String

(defvar edraw-color-string-float-format "%s")

(defun edraw-color-string-format-float (value)
  (cond
   ((functionp edraw-color-string-float-format)
    (funcall edraw-color-string-float-format value))
   ((stringp edraw-color-string-float-format)
    (format edraw-color-string-float-format value))
   ((integerp edraw-color-string-float-format)
    (string-trim-right
     (format (format "%%.%sf" edraw-color-string-float-format) value)
     "\\.?0+"))
   (t (format "%s" value))))

(cl-defmethod edraw-to-string ((color edraw-color))
  (if (/= (oref color a) 1.0)
      (edraw-to-string-rgba color)
    (edraw-to-string-hex color)))

(cl-defmethod edraw-to-string-rgba ((color edraw-color))
  (with-slots (r g b a) color
    (format "rgba(%d,%d,%d,%s)"
            (round (* 255 r))
            (round (* 255 g))
            (round (* 255 b))
            (edraw-color-string-format-float a))))

(cl-defmethod edraw-to-string-rgba-or-rgb ((color edraw-color))
  (with-slots (r g b a) color
    (if (= a 1)
        (format "rgb(%d,%d,%d)"
                (round (* 255 r))
                (round (* 255 g))
                (round (* 255 b)))
      (edraw-to-string-rgba color))))

(cl-defmethod edraw-to-string-hex ((color edraw-color))
  (with-slots (r g b a) color
    (if (= a 1.0)
        (format "#%02x%02x%02x"
                (round (* 255 r))
                (round (* 255 g))
                (round (* 255 b)))
      (format "#%02x%02x%02x%02x"
              (round (* 255 r))
              (round (* 255 g))
              (round (* 255 b))
              (round (* 255 a))))))

;;;; From String

(defun edraw-color-from-hex-string (str)
  (let ((slen (length str)))
    (when (and (or (= slen 4) (= slen 5) (= slen 7) (= slen 9))
               (string-match-p "\\`#[0-9a-fA-F]\\{3,8\\}\\'" str))
      (let* ((n-dig (if (<= (1- slen) 4) 1 2))
             (n-val (/ (1- slen) n-dig))
             (nums (cl-loop
                    for i from 1 to (* n-val n-dig) by n-dig
                    collect
                    (let ((n (string-to-number
                              (substring str i (+ i n-dig)) 16)))
                      (when (= n-dig 1)
                        (setq n (+ (* n 16) n)))
                      (/ n 255.0)))))
        (edraw-color
         :r (nth 0 nums)
         :g (nth 1 nums)
         :b (nth 2 nums)
         :a (or (nth 3 nums) 1.0))))))

(defun edraw-color-from-rgb-string (str)
  (when (string-match "\\` *rgba? *(\\([^)]+\\)) *\\'" str)
    (let* ((strs (split-string (match-string 1 str) " *[ ,/] *" nil))
           (strs-len (length strs)))
      (when (or (= strs-len 3) (= strs-len 4))
        (let ((nums
               (cl-loop for i from 0 to 3
                        for s in strs
                        collect
                        (when (string-match "\\`\\([-+]?\\(?:\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\)\\|\\(?:\\.[0-9]+\\)\\)\\(?:[eE][-+]?[0-9]+\\)?\\)\\(%\\)?\\'" s)
                          (let ((n (string-to-number (match-string 1 s))))
                            (max 0.0
                                 (min 1.0
                                      (if (match-string 2 s)
                                          (/ n 100.0)
                                        (if (< i 3) (/ n 255.0) n)))))))))
          (unless (memq nil nums)
            (edraw-color
             :r (nth 0 nums)
             :g (nth 1 nums)
             :b (nth 2 nums)
             :a (or (nth 3 nums) 1.0))))))))

(defconst edraw-color-string-patterns
  '(("#[0-9a-fA-F]*" hex edraw-color-from-hex-string edraw-to-string-hex)
    ("rgba? *([^)]*)" rgb edraw-color-from-rgb-string edraw-to-string-rgba-or-rgb)))

(defconst edraw-color-string-patterns-re
  (mapconcat (lambda (re) (concat "\\(" (car re) "\\)"))
             edraw-color-string-patterns
             "\\|"))

(defvar edraw-color-name-scheme 'web)

(defun edraw-color-from-string (str)
  (or
   (when (string-match edraw-color-string-patterns-re str)
     (funcall
      (caddr
       (nth
        (/ (seq-position (cddr (match-data)) t (lambda (a b) (and a b))) 2)
        edraw-color-string-patterns))
      str))
   (pcase edraw-color-name-scheme
     ('web (edraw-color-from-web-keyword-string str))
     ('emacs (edraw-color-from-emacs-color-name str))
     (_ (edraw-color-from-emacs-color-name str)))))

;;;; Emacs Color Names

(defun edraw-color-from-emacs-color-name (str)
  (when-let ((rgb (color-name-to-rgb str)))
    (edraw-color-f
     (nth 0 rgb)
     (nth 1 rgb)
     (nth 2 rgb)
     1.0)))

(cl-defmethod edraw-to-string-emacs-color-name ((color edraw-color))
  "Convert COLOR to Emacs color name.
Since there are many colors with the same name, COLOR does not
 always return to the original color name."
  (when (= (edraw-color-a color) 1.0)
    (let ((r (edraw-color-r color))
          (g (edraw-color-g color))
          (b (edraw-color-b color))
          (eps (/ 1.0 255 2)))
      (seq-find (lambda (name)
                  (let ((named-color (color-name-to-rgb name)))
                    (and (<= (abs (- (car named-color) r)) eps)
                         (<= (abs (- (cadr named-color) g)) eps)
                         (<= (abs (- (caddr named-color) b)) eps))))
                (defined-colors)))))

;;;; Web Keywords

(defconst edraw-color-web-keywords
  '(("black" . "#000000")
    ("silver" . "#c0c0c0")
    ("gray" . "#808080")
    ("white" . "#ffffff")
    ("maroon" . "#800000")
    ("red" . "#ff0000")
    ("purple" . "#800080")
    ("fuchsia" . "#ff00ff")
    ("green" . "#008000")
    ("lime" . "#00ff00")
    ("olive" . "#808000")
    ("yellow" . "#ffff00")
    ("navy" . "#000080")
    ("blue" . "#0000ff")
    ("teal" . "#008080")
    ("aqua" . "#00ffff")
    ("orange" . "#ffa500")
    ("aliceblue" . "#f0f8ff")
    ("antiquewhite" . "#faebd7")
    ("aquamarine" . "#7fffd4")
    ("azure" . "#f0ffff")
    ("beige" . "#f5f5dc")
    ("bisque" . "#ffe4c4")
    ("blanchedalmond" . "#ffebcd")
    ("blueviolet" . "#8a2be2")
    ("brown" . "#a52a2a")
    ("burlywood" . "#deb887")
    ("cadetblue" . "#5f9ea0")
    ("chartreuse" . "#7fff00")
    ("chocolate" . "#d2691e")
    ("coral" . "#ff7f50")
    ("cornflowerblue" . "#6495ed")
    ("cornsilk" . "#fff8dc")
    ("crimson" . "#dc143c")
    ("cyan" . "#00ffff")
    ("aqua" . "#00ffff")
    ("darkblue" . "#00008b")
    ("darkcyan" . "#008b8b")
    ("darkgoldenrod" . "#b8860b")
    ("darkgray" . "#a9a9a9")
    ("darkgreen" . "#006400")
    ("darkgrey" . "#a9a9a9")
    ("darkkhaki" . "#bdb76b")
    ("darkmagenta" . "#8b008b")
    ("darkolivegreen" . "#556b2f")
    ("darkorange" . "#ff8c00")
    ("darkorchid" . "#9932cc")
    ("darkred" . "#8b0000")
    ("darksalmon" . "#e9967a")
    ("darkseagreen" . "#8fbc8f")
    ("darkslateblue" . "#483d8b")
    ("darkslategray" . "#2f4f4f")
    ("darkslategrey" . "#2f4f4f")
    ("darkturquoise" . "#00ced1")
    ("darkviolet" . "#9400d3")
    ("deeppink" . "#ff1493")
    ("deepskyblue" . "#00bfff")
    ("dimgray" . "#696969")
    ("dimgrey" . "#696969")
    ("dodgerblue" . "#1e90ff")
    ("firebrick" . "#b22222")
    ("floralwhite" . "#fffaf0")
    ("forestgreen" . "#228b22")
    ("gainsboro" . "#dcdcdc")
    ("ghostwhite" . "#f8f8ff")
    ("gold" . "#ffd700")
    ("goldenrod" . "#daa520")
    ("greenyellow" . "#adff2f")
    ("grey" . "#808080")
    ("honeydew" . "#f0fff0")
    ("hotpink" . "#ff69b4")
    ("indianred" . "#cd5c5c")
    ("indigo" . "#4b0082")
    ("ivory" . "#fffff0")
    ("khaki" . "#f0e68c")
    ("lavender" . "#e6e6fa")
    ("lavenderblush" . "#fff0f5")
    ("lawngreen" . "#7cfc00")
    ("lemonchiffon" . "#fffacd")
    ("lightblue" . "#add8e6")
    ("lightcoral" . "#f08080")
    ("lightcyan" . "#e0ffff")
    ("lightgoldenrodyellow" . "#fafad2")
    ("lightgray" . "#d3d3d3")
    ("lightgreen" . "#90ee90")
    ("lightgrey" . "#d3d3d3")
    ("lightpink" . "#ffb6c1")
    ("lightsalmon" . "#ffa07a")
    ("lightseagreen" . "#20b2aa")
    ("lightskyblue" . "#87cefa")
    ("lightslategray" . "#778899")
    ("lightslategrey" . "#778899")
    ("lightsteelblue" . "#b0c4de")
    ("lightyellow" . "#ffffe0")
    ("limegreen" . "#32cd32")
    ("linen" . "#faf0e6")
    ("magenta" . "#ff00ff")
    ("fuchsia" . "#ff00ff")
    ("mediumaquamarine" . "#66cdaa")
    ("mediumblue" . "#0000cd")
    ("mediumorchid" . "#ba55d3")
    ("mediumpurple" . "#9370db")
    ("mediumseagreen" . "#3cb371")
    ("mediumslateblue" . "#7b68ee")
    ("mediumspringgreen" . "#00fa9a")
    ("mediumturquoise" . "#48d1cc")
    ("mediumvioletred" . "#c71585")
    ("midnightblue" . "#191970")
    ("mintcream" . "#f5fffa")
    ("mistyrose" . "#ffe4e1")
    ("moccasin" . "#ffe4b5")
    ("navajowhite" . "#ffdead")
    ("oldlace" . "#fdf5e6")
    ("olivedrab" . "#6b8e23")
    ("orangered" . "#ff4500")
    ("orchid" . "#da70d6")
    ("palegoldenrod" . "#eee8aa")
    ("palegreen" . "#98fb98")
    ("paleturquoise" . "#afeeee")
    ("palevioletred" . "#db7093")
    ("papayawhip" . "#ffefd5")
    ("peachpuff" . "#ffdab9")
    ("peru" . "#cd853f")
    ("pink" . "#ffc0cb")
    ("plum" . "#dda0dd")
    ("powderblue" . "#b0e0e6")
    ("rosybrown" . "#bc8f8f")
    ("royalblue" . "#4169e1")
    ("saddlebrown" . "#8b4513")
    ("salmon" . "#fa8072")
    ("sandybrown" . "#f4a460")
    ("seagreen" . "#2e8b57")
    ("seashell" . "#fff5ee")
    ("sienna" . "#a0522d")
    ("skyblue" . "#87ceeb")
    ("slateblue" . "#6a5acd")
    ("slategray" . "#708090")
    ("slategrey" . "#708090")
    ("snow" . "#fffafa")
    ("springgreen" . "#00ff7f")
    ("steelblue" . "#4682b4")
    ("tan" . "#d2b48c")
    ("thistle" . "#d8bfd8")
    ("tomato" . "#ff6347")
    ("turquoise" . "#40e0d0")
    ("violet" . "#ee82ee")
    ("wheat" . "#f5deb3")
    ("whitesmoke" . "#f5f5f5")
    ("yellowgreen" . "#9acd32")
    ("rebeccapurple" . "#663399")
    ("transparent" . "#00000000")))

(defun edraw-color-from-web-keyword-string (str)
  (when-let ((color (alist-get str edraw-color-web-keywords nil nil 'string=)))
    (edraw-color-from-hex-string color)))

(cl-defmethod edraw-to-string-web-keyword ((color edraw-color))
  (if (= (edraw-color-a color) 0)
      "transparent" ;; Ignore RGB
    (let ((hex (edraw-to-string-hex color)))
      (car (seq-find (lambda (cell) (string= (cdr cell) hex))
                     edraw-color-web-keywords)))))


(provide 'edraw-color)
;;; edraw-color.el ends here
