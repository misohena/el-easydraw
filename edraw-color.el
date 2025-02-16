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


;;;; `edraw-color' class

(defclass edraw-color ()
  ((r :initarg :r :accessor edraw-color-r)
   (g :initarg :g :accessor edraw-color-g)
   (b :initarg :b :accessor edraw-color-b)
   (a :initarg :a :accessor edraw-color-a :initform 1.0)))

;;;;; Construction

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

(cl-defmethod edraw-replace-r ((color edraw-color) r)
  "Create a new `edraw-color' object by replacing the r of COLOR with R."
  (edraw-color
   :r (float r)
   :g (oref color g)
   :b (oref color b)
   :a (oref color a)))

(cl-defmethod edraw-replace-g ((color edraw-color) g)
  "Create a new `edraw-color' object by replacing the g of COLOR with G."
  (edraw-color
   :r (oref color r)
   :g (float g)
   :b (oref color b)
   :a (oref color a)))

(cl-defmethod edraw-replace-b ((color edraw-color) b)
  "Create a new `edraw-color' object by replacing the b of COLOR with B."
  (edraw-color
   :r (oref color r)
   :g (oref color g)
   :b (float b)
   :a (oref color a)))

(cl-defmethod edraw-replace-a ((color edraw-color) a)
  "Create a new `edraw-color' object by replacing the a of COLOR with A."
  (edraw-color
   :r (oref color r)
   :g (oref color g)
   :b (oref color b)
   :a (float a)))

;;;;; Converting from other color systems

(defun edraw-color-from-hsl (h s l &optional a)
  "Create an `edraw-color' object from HSL color components.

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
  "Create an `edraw-color' object from HSV (HSB) color components.

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
  "Create an `edraw-color' object corresponding to HUE (degrees)."
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

(defun edraw-color-from-hwb (h w b &optional a)
  "Create an `edraw-color' object from HWB color components.

  H : Hue (degrees)
  W : Whiteness (0.0 to 1.0)
  B : Blackness (0.0 to 1.0)
  A : Opacity (0.0 to 1.0 : default 1.0)

See URL `https://en.wikipedia.org/wiki/HWB_color_model'."
  ;; https://www.w3.org/TR/css-color-4/#hwb-to-rgb
  (if (>= (+ w b) 1)
      (let ((gray (/ w (+ w b))))
        (edraw-color-f gray gray gray a))
    (setq h (mod h 360.0)) ;; 0 <= h < 360
    (let ((c (- 1.0 w b)))
      (edraw-color-f
       (+ w (* c (edraw-color-hue-red-level--no-mod h)))
       (+ w (* c (edraw-color-hue-red-level--no-mod (- h 120.0))))
       (+ w (* c (edraw-color-hue-red-level--no-mod (+ h 120.0))))
       a))))

(defun edraw-color-from-lab (l a b &optional alpha)
  "Create an `edraw-color' object from Lab color components.

  L : CIE Lightness (0.0 to 100.0)
  A : a-coordinate (There is no limit, but it generally does not exceed ±160)
  B : b-coordinate (There is no limit, but it generally does not exceed ±160)
  ALPHA : Opacity (0.0 to 1.0 : default 1.0)

See URL `https://en.wikipedia.org/wiki/CIELAB_color_space' and
URL `https://www.w3.org/TR/css-color-4/#lab-colors'."
  (let ((rgb (color-lab-to-srgb l a b)))
    (edraw-color-f
     (max 0.0 (min 1.0 (car rgb)))
     (max 0.0 (min 1.0 (cadr rgb)))
     (max 0.0 (min 1.0 (caddr rgb)))
     alpha)))

(defun edraw-color-from-lch (l c h &optional alpha)
  "Create an `edraw-color' object from LCH color components.

  L : CIE Lightness (0.0 to 100.0)
  C : Chroma (0.0 or more. There is no upper limit, but it generally does
      not exceed 230.)
  H : Hue (degrees)
  ALPHA : Opacity (0.0 to 1.0 : default 1.0)

See URL `https://en.wikipedia.org/wiki/CIELAB_color_space' and
URL `https://www.w3.org/TR/css-color-4/#lab-colors'."
  (let ((rgb (apply #'color-lab-to-srgb
                    (color-lch-to-lab l c (/ (* float-pi h) 180.0)))))
    (edraw-color-f
     (max 0.0 (min 1.0 (car rgb)))
     (max 0.0 (min 1.0 (cadr rgb)))
     (max 0.0 (min 1.0 (caddr rgb)))
     alpha)))

(defun edraw-color-from-oklab (l a b &optional alpha)
  "Create an `edraw-color' object from OkLab color components.

  L : Oklab Lightness (0.0 to 1.0)
  A : a-coordinate (There is no limit, but it generally does not exceed ±0.5)
  B : b-coordinate (There is no limit, but it generally does not exceed ±0.5)
  ALPHA : Opacity (0.0 to 1.0 : default 1.0)

See URL `https://en.wikipedia.org/wiki/Oklab_color_space' and
URL `https://www.w3.org/TR/css-color-4/#specifying-oklab-oklch'."
  (let ((rgb (color-oklab-to-srgb l a b)))
    (edraw-color-f
     (max 0.0 (min 1.0 (car rgb)))
     (max 0.0 (min 1.0 (cadr rgb)))
     (max 0.0 (min 1.0 (caddr rgb)))
     alpha)))

(defun edraw-color-from-oklch (l c h &optional alpha)
  "Create an `edraw-color' object from OkLCH color components.

  L : Oklab Lightness (0.0 to 1.0)
  C : Chroma (0.0 or more. There is no upper limit, but it generally does
      not exceed 0.5.)
  H : Hue (degrees)
  ALPHA : Opacity (0.0 to 1.0 : default 1.0)

See URL `https://en.wikipedia.org/wiki/Oklab_color_space' and
URL `https://www.w3.org/TR/css-color-4/#specifying-oklab-oklch'."
  (let ((rgb (apply #'color-oklab-to-srgb
                    (color-lch-to-lab l c (/ (* float-pi h) 180.0)))))
    (edraw-color-f
     (max 0.0 (min 1.0 (car rgb)))
     (max 0.0 (min 1.0 (cadr rgb)))
     (max 0.0 (min 1.0 (caddr rgb)))
     alpha)))

;;;;; Converting to other color systems

(defun edraw-color-to-rgb-list (color)
  "Create a list (R G B) from COLOR."
  (list (edraw-color-r color)
        (edraw-color-g color)
        (edraw-color-b color)))

(defun edraw-color-to-rgba-list (color &optional allow-omit-alpha)
  "Create a list (R G B [A]) from COLOR.
When ALLOW-OMIT-ALPHA is non-nil, the A part is omitted when it is 1.0
or greater (returning a list of 3 elements of only RGB)."
  (nconc
   (list (edraw-color-r color)
         (edraw-color-g color)
         (edraw-color-b color))
   (unless (and allow-omit-alpha
                ;; No need for alpha
                (>= (edraw-color-a color) 1.0))
     (list (edraw-color-a color)))))

(defun edraw-color-to-hsl-list (color)
  "Create a list (H S L) from COLOR."
  (let ((result (color-rgb-to-hsl (edraw-color-r color)
                                  (edraw-color-g color)
                                  (edraw-color-b color))))
    (setcar result (* 360.0 (car result)))
    result))
;; TEST: (edraw-color-to-hsl-list (edraw-color-f 0 0.5 0 0.5)) => (120.0 1.0 0.25)

(defun edraw-color-to-hsla-list (color &optional allow-omit-alpha)
  "Create a list (H S L [A]) from COLOR.
When ALLOW-OMIT-ALPHA is non-nil, the A part is omitted when it is 1.0
or greater (returning a list of 3 elements of only RGB)."
  (let ((result (edraw-color-to-hsl-list color)))
    (unless (and allow-omit-alpha
                 ;; No need for alpha
                 (>= (edraw-color-a color) 1.0))
      (setq result (nconc result (list (edraw-color-a color)))))
    result))
;; TEST: (edraw-color-to-hsla-list (edraw-color-f 0 0.5 0)) => (120.0 1.0 0.25 1.0)
;; TEST: (edraw-color-to-hsla-list (edraw-color-f 0 0.5 0 0.5)) => (120.0 1.0 0.25 0.5)
;; TEST: (edraw-color-to-hsla-list (edraw-color-f 0 0.5 0 0.5) t) => (120.0 1.0 0.25 0.5)
;; TEST: (edraw-color-to-hsla-list (edraw-color-f 0 0.5 0) t) => (120.0 1.0 0.25)

(defun edraw-color-to-hwb-list (color)
  "Create a list (H W B) from COLOR."
  ;; https://www.w3.org/TR/css-color-4/#rgb-to-hwb
  (let* ((hsl (edraw-color-to-hsl-list color))
         (white (edraw-min-rgb color))
         (black (- 1.0 (edraw-max-rgb color))))
    (list (car hsl) white black)))

(defun edraw-color-to-hwba-list (color &optional allow-omit-alpha)
  "Create a list (H W B [A]) from COLOR.
When ALLOW-OMIT-ALPHA is non-nil, the A part is omitted when it is 1.0
or greater (returning a list of 3 elements of only RGB)."
  (let ((result (edraw-color-to-hwb-list color)))
    (unless (and allow-omit-alpha
                 ;; No need for alpha
                 (>= (edraw-color-a color) 1.0))
      (setq result (nconc result (list (edraw-color-a color)))))
    result))
;; TEST: (edraw-color-to-hwba-list (edraw-color-f 0 0 0)) => (0.0 0.0 1.0 1.0)
;; TEST: (edraw-color-to-hwba-list (edraw-color-f 1 1 1)) => (0.0 1.0 0.0 1.0)
;; TEST: (edraw-color-to-hwba-list (edraw-color-f 0 1 0)) => (120.0 0.0 0.0 1.0)
;; TEST: (edraw-color-to-hwba-list (edraw-color-f 0.8 1 0.8)) => (120.0 0.8 0.0 1.0)
;; TEST: (edraw-color-to-hwba-list (edraw-color-f 0.3 0.5 0.3 0.75)) => (120.0 0.3 0.5 0.75)

(defun edraw-color-to-lab-list (color)
  "Create a list (L a b) from COLOR."
  (color-srgb-to-lab (oref color r) (oref color g) (oref color b)))

(defun edraw-color-to-lch-list (color)
  "Create a list (L C H) from COLOR."
  (let ((result (apply #'color-lab-to-lch
                       (color-srgb-to-lab
                        (oref color r) (oref color g) (oref color b)))))
    (setcar (cddr result) (mod (/ (* 180.0 (caddr result)) float-pi) 360.0))
    result))

(defun edraw-color-to-oklab-list (color)
  "Create a list OkLab's (L a b) from COLOR."
  (color-srgb-to-oklab (oref color r) (oref color g) (oref color b)))

(defun edraw-color-to-oklch-list (color)
  "Create a list OkLab's (L C H) from COLOR."
  (let ((result (apply #'color-lab-to-lch
                       (color-srgb-to-oklab
                        (oref color r) (oref color g) (oref color b)))))
    (setcar (cddr result) (mod (/ (* 180.0 (caddr result)) float-pi) 360.0))
    result))

;;;;; Comparison

(defun edraw-color-equal-p (a b)
  (equal a b))

(cl-defmethod edraw-opaque-p ((color edraw-color))
  "Return t if COLOR is opaque.
A color is considered opaque if its alpha component is 1 or greater."
  (>= (oref color a) 1.0))

(cl-defmethod edraw-transparent-p ((color edraw-color))
  "Return t if COLOR is transparent.
A color is considered transparent if its alpha component is 0 or less."
  (<= (oref color a) 0.0))

;;;;; Retrieving attributes

(cl-defmethod edraw-hue ((color edraw-color))
  (let* ((r (oref color r)) (g (oref color g)) (b (oref color b))
         (d (- (max r g b) (min r g b))))
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
     360.0)))

(cl-defmethod edraw-saturation ((color edraw-color))
  (let ((r (oref color r)) (g (oref color g)) (b (oref color b)))
    (let ((max (max r g b))
          (min (min r g b)))
      (if (= max 0)
          0
        (/ (- max min) max)))))

(cl-defmethod edraw-brightness ((color edraw-color))
  (edraw-max-rgb color))

(cl-defmethod edraw-relative-luminance ((color edraw-color))
  "Return the relative luminance of COLOR.
See https://en.wikipedia.org/wiki/Relative_luminance"
  (with-slots (r g b) color
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(cl-defmethod edraw-max-rgb ((color edraw-color))
  (max (oref color r) (oref color g) (oref color b)))

(cl-defmethod edraw-min-rgb ((color edraw-color))
  (min (oref color r) (oref color g) (oref color b)))

(cl-defmethod edraw-length-rgb ((color edraw-color))
  (let ((r (oref color r)) (g (oref color g)) (b (oref color b)))
    (sqrt (+ (* r r) (* g g) (* b b)))))

;;;;; Blending

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

;;;;; Converting to string

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

;;;;; Converting from string

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
     ('web (edraw-color-from-css-color-name str))
     ('emacs (edraw-color-from-emacs-color-name str))
     (_ (edraw-color-from-emacs-color-name str)))))


;;;; Color syntax

;;;;; Common

(defmacro edraw-color-info-color (color-info) `(car ,color-info))
(defmacro edraw-color-info-props (color-info) `(cdr ,color-info))

(defun edraw-color-number-to-string (n &optional decimal-places)
  (unless decimal-places (setq decimal-places 6))
  (if (floatp n)
      ;; Float
      (if (<= decimal-places 0)
          (format "%d" (round n))
        (let ((ni (ffloor n)))
          (if (= ni n)
              (format "%d" ni) ;;Remove .0 and Avoid -0
            (let* ((str (format (format "%%.%df" decimal-places) n))
                   (pos (string-match-p "\\(?:\\`-0\\.\\|\\.\\)?0+\\'" str)))
              (if pos
                  (if (= pos 0)
                      "0" ;; "-0.000000"
                    (substring str 0 pos))
                str)))))
    ;; Integer? or error
    (number-to-string n)))
;; TEST: (edraw-color-number-to-string 123) => "123"
;; TEST: (edraw-color-number-to-string -123.0) => "-123"
;; TEST: (edraw-color-number-to-string -0.0) => "0"
;; TEST: (edraw-color-number-to-string -0.00000000001) => "0"
;; TEST: (edraw-color-number-to-string pi) => "3.141593"
;; TEST: (edraw-color-number-to-string 1.2300001) => "1.23"
;; TEST: (edraw-color-number-to-string pi 3) => "3.142"
;; TEST: (edraw-color-number-to-string pi 0) => "3"
;; TEST: (edraw-color-number-to-string pi -1) => "3"
;; TEST: (edraw-color-number-to-string 120.1 0) => "120"

(defun edraw-color-parse-hex-color (hex-string
                                    &optional begin end no-props)
  "Parse HEX-STRING from BEGIN to END.

HEX-STRING must be filled with hexadecimal digits from BEGIN to END of
the string. The behavior is undefined if any digits other than
hexadecimal digits are included.

Supported formats:
  RGB
  RGBA (CSS)
  RRGGBB
  RRGGBBAA (CSS)
  RRRGGGBBB (Emacs)
  RRRRGGGGBBBB (Emacs)

Signals an error if something unexpected occurs and never returns nil."
  (unless begin (setq begin 0))
  (unless end (setq end (length hex-string)))
  (let* ((len (- end begin))
         (num-components (cond
                          ;; 3 <= len <= 12, (len mod 3)=>3, (len mod 4)=>4
                          ((memq len '(3 6 9 12)) 3) ;; 12 => 3 not 4
                          ((memq len '(4 8)) 4)
                          (t (error "Invalid hex-string length %d" len))))
         (digits-per-component (/ len num-components))
         (max-value (float (1- (ash 1 (* 4 digits-per-component)))))
         (components (cl-loop for i from begin below end
                              by digits-per-component
                              collect
                              (/
                               (float
                                ;; @todo Check if filled with xdigit?
                                (string-to-number
                                 (substring hex-string
                                            i
                                            (+ i digits-per-component))
                                 16))
                               max-value))))
    (if no-props
        (apply #'edraw-color-f components)
      (list
       (apply #'edraw-color-f components)
       :num-components num-components
       :hex-digits-per-component digits-per-component))))
;; TEST: (edraw-color-parse-hex-color "#40008000c000" 1) => (#s(edraw-color 0.2500038147554742 0.5000076295109483 0.7500114442664225 1.0) :num-components 3 :hex-digits-per-component 4)

(defun edraw-color-make-hex-color (color
                                   &optional
                                   prefix disable-alpha
                                   digits-per-component
                                   uppercase)
  (unless digits-per-component (setq digits-per-component 2))
  (concat
   prefix
   (cl-loop
    with max-value = (float (1- (ash 1 (* 4 digits-per-component))))
    with fmt = (format "%%0%d%c" digits-per-component (if uppercase ?X ?x))
    for value
    in (if disable-alpha
           (edraw-color-to-rgb-list color)
         (edraw-color-to-rgba-list color t))
    concat (format fmt (round (* max-value (max 0.0 (min 1.0 value))))))))
;; TEST: (edraw-color-make-hex-color (edraw-color-f 0.25 0.5 0.75 0.33) "#" t 4 t) => "#40008000BFFF"
;; TEST: (edraw-color-make-hex-color (edraw-color-f 0.25 0.5 0.75 0.33) "#") => "#4080bf54"
;; TEST: (edraw-color-make-hex-color (edraw-color-f 0.25 0.5 0.75) "#") => "#4080bf"

;;;;; Emacs color syntax system

;;;;;; Color names

(defun edraw-color-from-emacs-color-name (name)
  "Convert a Emacs color name to an `edraw-color' object."
  (when-let* ((rgb (color-name-to-rgb name)))
    (edraw-color-f
     (nth 0 rgb)
     (nth 1 rgb)
     (nth 2 rgb)
     1.0)))

(defun edraw-color-to-emacs-color-name (color)
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

(cl-defmethod edraw-to-emacs-color-name ((color edraw-color))
  "Convert COLOR to Emacs color name.
Since there are many colors with the same name, COLOR does not
 always return to the original color name."
  (edraw-color-to-emacs-color-name color))

;;;;;; Parsing

(defvar edraw-color-emacs-regexp--cache nil
  "A regular expression cache used by `edraw-color-emacs-regexp'.")

(defun edraw-color-emacs-regexp-clear-cache ()
  "Clear the regular expression cache used by `edraw-color-emacs-regexp'."
  (setq edraw-color-emacs-regexp--cache nil))

(defun edraw-color-emacs-regexp ()
  "Return a regexp that may match a Emacs color text.

Note that a match does not necessarily mean that the matched text is valid.

After matching this regular expression, call
`edraw-color-emacs-matched-color-info' to get detailed information about
the matched color and writing style."
  (or edraw-color-emacs-regexp--cache
      (setq edraw-color-emacs-regexp--cache
            (concat
             "\\(?:"
             (rx (or (group-n 1 (seq "#" (or (= 12 xdigit) (= 9 xdigit)
                                             (= 6 xdigit) (= 3 xdigit))))
                     (group-n 2 (regexp (regexp-opt (defined-colors))))))
             "\\)"))))

(defun edraw-color-emacs-matched-color-info (&optional string)
  "Create color information from the current match data.

When called after a successful search using the regular expression
returned by `edraw-emacs-css-regexp', this function returns a cons cell
containing the color value and syntax information from the match data.

The car of the returned value is an `edraw-color' object. The cdr is a
property list containing the information obtained by syntax parsing.

Signals an error if there is a syntax or other problem, never returns nil."
  (let ((beg (match-beginning 0))
        (end (match-end 0))
        (color-info
         (cond
          ((match-beginning 1)
           (nconc
            (edraw-color-parse-hex-color
             (match-string-no-properties 1 string) 1)
            (list :syntax 'emacs-hex-color)))
          ((match-beginning 2)
           (list
            (or
             (edraw-color-from-emacs-color-name
              (match-string-no-properties 2 string))
             (error "Invalid color name"))
            :syntax 'emacs-color-name))
          (t (error "Invalid match data")))))
    (nconc
     color-info
     (list :begin beg :end end :syntax-system 'emacs))))

;;;;;; Serialization

(defun edraw-color-emacs-serialize (color &optional options)
  "Convert COLOR to a string in Emacs color syntax."
  (or (and (not (plist-get options :disable-color-names))
           (edraw-color-to-emacs-color-name color))
      (edraw-color-make-hex-color
       color "#" t
       (max
        (or (plist-get options :hex-digits-per-component) 2)
        (or (plist-get options :hex-min-digits-per-component) 0))
       (plist-get options :hex-upcase))))
;; TEST: (edraw-color-emacs-serialize (edraw-color-f 0.5 0.5 0.5)) => "gray50"
;; TEST: (edraw-color-emacs-serialize (edraw-color-f 0.25 0.5 0.75 0.5)) => "#4080bf"


;;;;; CSS color syntax system

;; @todo Unify edraw-css- functions in edraw-dom-svg.el. Create edraw-css.el ?

;;;;;; Color names

(defconst edraw-color-css-color-names
  '(("aliceblue" . "#f0f8ff")
    ("antiquewhite" . "#faebd7")
    ("aqua" . "#00ffff")
    ("aquamarine" . "#7fffd4")
    ("azure" . "#f0ffff")
    ("beige" . "#f5f5dc")
    ("bisque" . "#ffe4c4")
    ("black" . "#000000")
    ("blanchedalmond" . "#ffebcd")
    ("blue" . "#0000ff")
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
    ("fuchsia" . "#ff00ff")
    ("gainsboro" . "#dcdcdc")
    ("ghostwhite" . "#f8f8ff")
    ("gold" . "#ffd700")
    ("goldenrod" . "#daa520")
    ("gray" . "#808080")
    ("green" . "#008000")
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
    ("lime" . "#00ff00")
    ("limegreen" . "#32cd32")
    ("linen" . "#faf0e6")
    ("magenta" . "#ff00ff")
    ("maroon" . "#800000")
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
    ("navy" . "#000080")
    ("oldlace" . "#fdf5e6")
    ("olive" . "#808000")
    ("olivedrab" . "#6b8e23")
    ("orange" . "#ffa500")
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
    ("purple" . "#800080")
    ("rebeccapurple" . "#663399")
    ("red" . "#ff0000")
    ("rosybrown" . "#bc8f8f")
    ("royalblue" . "#4169e1")
    ("saddlebrown" . "#8b4513")
    ("salmon" . "#fa8072")
    ("sandybrown" . "#f4a460")
    ("seagreen" . "#2e8b57")
    ("seashell" . "#fff5ee")
    ("sienna" . "#a0522d")
    ("silver" . "#c0c0c0")
    ("skyblue" . "#87ceeb")
    ("slateblue" . "#6a5acd")
    ("slategray" . "#708090")
    ("slategrey" . "#708090")
    ("snow" . "#fffafa")
    ("springgreen" . "#00ff7f")
    ("steelblue" . "#4682b4")
    ("tan" . "#d2b48c")
    ("teal" . "#008080")
    ("thistle" . "#d8bfd8")
    ("tomato" . "#ff6347")
    ("turquoise" . "#40e0d0")
    ("violet" . "#ee82ee")
    ("wheat" . "#f5deb3")
    ("white" . "#ffffff")
    ("whitesmoke" . "#f5f5f5")
    ("yellow" . "#ffff00")
    ("yellowgreen" . "#9acd32")
    ;; 148 color names defined in CSS Color Module Level 4
    ;; Special Color
    ("transparent" . "#00000000"))
  "An alist of CSS color names and their associated hex color values.")

(defun edraw-color-css-color-name-to-hex-color (name)
  "Convert a CSS color name to a HEX color."
  (alist-get name edraw-color-css-color-names nil nil 'string=))

(defun edraw-color-from-css-color-name (name)
  "Convert a CSS color name to an `edraw-color' object."
  (when-let* ((hex
               (alist-get name edraw-color-css-color-names nil nil 'string=)))
    (edraw-color-from-hex-string hex)))

(defun edraw-color-to-css-color-name (color)
  "Convert COLOR, which is an `edraw-color' object, to a CSS color name."
  (cond
   ;; Transparent Black
   ((edraw-color-equal-p color (edraw-color-f 0 0 0 0))
    "transparent")
   ;; Opaque
   ((>= (edraw-color-a color) 1.0)
    (car (rassoc (edraw-to-string-hex color) edraw-color-css-color-names)))))
;; TEST: (edraw-color-to-css-color-name (edraw-color-f 1 1 1 0)) => nil
;; TEST: (edraw-color-to-css-color-name (edraw-color-f 0 0 0 0)) => "transparent"
;; TEST: (edraw-color-to-css-color-name (edraw-color-f 0 0.5 0)) => "green"
;; TEST: (edraw-color-to-css-color-name (edraw-color-f 0 0.5 0 0.5)) => nil

(cl-defmethod edraw-to-css-color-name ((color edraw-color))
  "Convert COLOR, which is an `edraw-color' object, to a CSS color name."
  (edraw-color-to-css-color-name color))

;;;;;; Parsing

;; Note: If you modify these constants,
;; call the `edraw-color-css-regexp-clear-cache' function
;; to clear the regular expression cache.

(defconst edraw-color-css-functions
  ;; https://www.w3.org/TR/css-color-4/#typedef-color-function
  '(("rgb"
     :parser edraw-color-css-fun-rgb :serializer edraw-color-css-make-rgb)
    ("rgba"
     :parser edraw-color-css-fun-rgb :serializer edraw-color-css-make-rgb)
    ("hsl"
     :parser edraw-color-css-fun-hsl :serializer edraw-color-css-make-hsl)
    ("hsla"
     :parser edraw-color-css-fun-hsl :serializer edraw-color-css-make-hsl)
    ("hwb"
     :parser edraw-color-css-fun-hwb :serializer edraw-color-css-make-hwb)
    ("lab"
     :parser edraw-color-css-fun-lab :serializer edraw-color-css-make-lab)
    ("lch"
     :parser edraw-color-css-fun-lch :serializer edraw-color-css-make-lch)
    ("oklab"
     :parser edraw-color-css-fun-oklab :serializer edraw-color-css-make-oklab)
    ("oklch"
     :parser edraw-color-css-fun-oklch :serializer edraw-color-css-make-oklch)
    ;; @todo impl color() function
    ("color")))

(defconst edraw-color-css-re-hex-color
  (rx "#" (or (= 8 xdigit) (= 6 xdigit) (= 4 xdigit) (= 3 xdigit))))

(defconst edraw-color-css-re-number
  (concat "\\(?:"
          "[-+]?"
          "\\(?:[0-9]+\\(?:\\.[0-9]+\\)?\\|\\.[0-9]+\\)" ;; "12." is invalid
          "\\(?:[eE][-+]?[0-9]+\\)?"
          "\\)"))

(defconst edraw-color-css-re-ident
  (concat
   "\\(?:--\\|-?\\(?:[_a-zA-Z]\\|[[:nonascii:]]\\)\\)"
   "\\(?:[-_a-zA-Z0-9]\\|[[:nonascii:]]\\)*")) ;; without escape

(defconst edraw-color-css-re-argument
  ;; number | angle | percentage | ident (include none)
  (rx (or (seq (regexp edraw-color-css-re-number)
               (? (or "%" "deg" "grad" "rad" "turn")))
          (regexp edraw-color-css-re-ident))))

(defconst edraw-color-css-re-argument-parse
  (rx
   (or
    (group ;; 1: argument
     (? (group (+ space))) ;; 2: pre-spaces
     (or (seq
          (group (regexp edraw-color-css-re-number)) ;; 3: number
          (? (group (or "%" "deg" "grad" "rad" "turn")))) ;; 4: unit
         (group (regexp edraw-color-css-re-ident))) ;; 5: ident
     (? (group (+ space))) ;; 6: post-spaces
     (? (group (or "," "/")))) ;; 7: next separator
    ;; This regular expression always matches.
    "A regular expression to parse the argument part of the <color-function>
syntax.")))

(defconst edraw-color-css-system-colors
  ;; https://www.w3.org/TR/css-color-4/#css-system-colors
  '("accentcolor" "accentcolortext" "activetext" "buttonborder" "buttonface"
    "buttontext" "canvas" "canvastext" "field" "fieldtext"
    "graytext" "highlight" "highlighttext" "linktext" "mark"
    "marktext" "selecteditem" "selecteditemtext" "visitedtext")
  "A list of system color names.")

(defvar edraw-color-css-regexp--cache nil
  "A regular expression cache used by `edraw-color-css-regexp'.")

(defun edraw-color-css-regexp-clear-cache ()
  "Clear the regular expression cache used by `edraw-color-css-regexp'."
  (setq edraw-color-css-regexp--cache nil))

(defun edraw-color-css-regexp ()
  "Return a regexp that may match a CSS color text.

Note that a match does not necessarily mean that the matched text is valid.

After matching this regular expression, call
`edraw-color-css-matched-color-info' to get detailed information about the
matched color and writing style."
  (or edraw-color-css-regexp--cache
      (setq edraw-color-css-regexp--cache
            (concat
             "\\(?:"
             (rx
              (or
               ;; See: https://www.w3.org/TR/css-color-4/#color-syntax
               ;; <hex-color>
               (group-n 1 (regexp edraw-color-css-re-hex-color))
               ;; <named-color> | transparent | currentcolor | <system-color>
               (group-n 2 (regexp
                           (regexp-opt
                            (nconc
                             (list "transparent" "currentcolor")
                             (mapcar #'car edraw-color-css-color-names)
                             edraw-color-css-system-colors))))
               ;; <color-function>
               (seq
                ;; Function Name
                (group-n 3 (regexp (regexp-opt
                                    (mapcar #'car edraw-color-css-functions))))
                "("
                ;; Argument List
                (group-n 4
                  (* space)
                  (regexp edraw-color-css-re-argument)
                  (or
                   ;; legacy syntax
                   (** 2 3 (seq (* space)
                                ","
                                (* space)
                                (regexp edraw-color-css-re-argument)))
                   ;; modern syntax
                   (seq (* (seq (+ space)
                                (regexp edraw-color-css-re-argument)))
                        (? (seq (* space)
                                "/"
                                (* space)
                                (regexp edraw-color-css-re-argument)))))
                  (* space))
                ")")))
             "\\)"))))

(defun edraw-color-css-matched-color-info (&optional string)
  "Create color information from the current match data.

When called after a successful search using the regular expression
returned by `edraw-color-css-regexp', this function returns a cons cell
containing the color value and syntax information from the match data.

The car of the returned value is an `edraw-color' object. The cdr is a
property list containing the information obtained by syntax parsing.

Signals an error if there is a syntax or other problem, never returns nil."
  (let ((beg (match-beginning 0))
        (end (match-end 0))
        (color-info
         (cond
          ((match-beginning 1)
           (nconc
            (edraw-color-parse-hex-color
             (match-string-no-properties 1 string) 1)
            (list :syntax 'css-hex-color)))
          ((match-beginning 2)
           (let ((color (edraw-color-from-css-color-name
                         (match-string-no-properties 2 string))))
             (list
              (or color
                  ;; <system-color> and currentColor
                  (edraw-color-f 0 0 0 1))
              ;; Note: Strictly speaking, transparent, currentColor,
              ;; and <system-color> are not included in named-color in
              ;; CSS Color Module Level 4.
              :syntax 'css-named-color)))
          ((match-beginning 3)
           (or (edraw-color-css-call-color-function
                (match-string-no-properties 3 string)
                (match-string-no-properties 4 string))
               ;; Just to be sure
               (error "Failed to edraw-color-css-call-function")))
          (t (error "Invalid match data")))))
    (nconc
     color-info
     (list :begin beg :end end :syntax-system 'css))))

(defun edraw-color-css-call-color-function (fname arglist-str)
  (let ((parser (plist-get
                 (alist-get fname edraw-color-css-functions nil nil #'string=)
                 :parser)))
    (unless parser
      (error "Unsupported color function: %s" fname))
    (funcall parser fname arglist-str)))

(defun edraw-color-css-parse-arglist (arglist-str
                                      &optional
                                      allow-legacy-syntax
                                      allowed-idents
                                      min-components-without-alpha
                                      max-components-without-alpha
                                      component-props)
  (let ((len (length arglist-str))
        (pos 0)
        (prev-separator nil)
        (prev-post-spaces " ")
        (syntax-generation 'unknown)
        (args nil))
    (while (and
            (< pos len)
            (progn
              ;; number | angle | percentage | ident
              (string-match edraw-color-css-re-argument-parse arglist-str pos)
              (match-beginning 1)))
      (setq pos (match-end 0))
      (when (and (null prev-separator)
                 (null prev-post-spaces)) ;; e.g."none255"
        (error "Syntax error: %s (No argument separator)" arglist-str))
      (let* ((pre-spaces (match-string 2 arglist-str))
             (number-str (match-string 3 arglist-str))
             (unit (match-string 4 arglist-str))
             (ident (match-string 5 arglist-str))
             (post-spaces (match-string 6 arglist-str))
             (next-separator (match-string 7 arglist-str))
             (next-legacy-p (equal next-separator ","))
             (eos-p (= pos len))
             (component-prop (car component-props)))

        ;; Check syntax
        (unless eos-p
          (when (and (not allow-legacy-syntax) next-legacy-p)
            (error "Legacy syntax is not allowed"))
          (if (eq syntax-generation 'unknown)
              (setq syntax-generation (if next-legacy-p 'legacy 'modern))
            (when (not (eq (eq syntax-generation 'legacy) next-legacy-p))
              (error "Syntax generation mismatch")))
          (when (and (eq syntax-generation 'legacy)
                     ident)
            (error "Identifier is not allowed in legacy syntax"))
          (when (equal prev-separator "/")
            (error "`/' is only allowed before the last component")))
        (when (and ident
                   allowed-idents
                   (not (member ident allowed-idents)))
          (error "Identifier is not allowed: %s" ident))
        (when-let* ((allowed-units (plist-get component-prop :allowed-units)))
          (unless (member unit allowed-units)
            (error "Unit is not allowed: %s" unit)))

        (push (list (cond
                     (number-str
                      (let ((n (float (string-to-number number-str))))
                        (pcase unit
                          ("deg" n)
                          ("rad" (/ (* 180.0 n) float-pi))
                          ("grad" (/ (* n 360.0) 400.0))
                          ("turn" (* n 360.0))
                          ("%" (/
                                (* n
                                   (or (plist-get component-prop :percent-ref)
                                       1.0))
                                100.0))
                          ('nil (/ n
                                   (or (plist-get component-prop :number-ref)
                                       1.0)))
                          (_ (error "Unknown unit: %s" unit)))))
                     (ident
                      (if (string= ident "none")
                          0
                        (intern ident))))
                    :pre-separator prev-separator ;; See `edraw-color-css-args-legacy-p' if change
                    :pre-spaces pre-spaces
                    :number-str number-str
                    :unit unit
                    :ident ident
                    :post-spaces post-spaces)
              args)
        (setq prev-separator next-separator
              prev-post-spaces post-spaces
              component-props (cdr component-props)))) ;; End of loop

    (when prev-separator
      (error "Syntax error: %s (ends with a separator)" arglist-str))
    (when (/= pos len)
      (error "Syntax error: %s" arglist-str))

    (let ((num-components
           (if (equal (plist-get (cdr (car args)) :pre-separator) "/")
               (1- (length args))
             (length args))))
      (when (and min-components-without-alpha
                 (< num-components min-components-without-alpha))
        (error "Not enough components"))
      (when (and max-components-without-alpha
                 (> num-components (if (eq syntax-generation 'legacy)
                                       (1+ max-components-without-alpha)
                                     max-components-without-alpha)))
        (error "Too many components")))

    ;; Return reversed args
    (nreverse args)))
;; EXAMPLE: (edraw-color-css-parse-arglist " 64deg , 128 , 75% , 0.5 " t)
;; EXAMPLE: (edraw-color-css-parse-arglist "64deg,128,192,0.5" t)
;; EXAMPLE: (edraw-color-css-parse-arglist "64deg  128  192 / 0.5")
;; EXAMPLE: (edraw-color-css-parse-arglist "64deg  128  192 / 0.5" t nil 3 3)

(defun edraw-color-css-fun-rgb (fname arglist-str)
  (let ((args (edraw-color-css-parse-arglist
               arglist-str t '("none") 3 3
               '((:allowed-units ("%" nil) :number-ref 255.0)
                 (:allowed-units ("%" nil) :number-ref 255.0)
                 (:allowed-units ("%" nil) :number-ref 255.0)
                 (:allowed-units ("%" nil) :number-ref 1.0)))))
    (list
     (edraw-color-f
      (max 0.0 (min 1.0 (car (nth 0 args))))
      (max 0.0 (min 1.0 (car (nth 1 args))))
      (max 0.0 (min 1.0 (car (nth 2 args))))
      (max 0.0 (min 1.0 (or (car (nth 3 args)) 1.0))))
     :css-fname fname
     :css-args args
     :syntax 'css-color-function)))
;; TEST: (car (edraw-color-css-fun-rgb "rgb" "25%,50%,75%,0.5")) => #s(edraw-color 0.25 0.5 0.75 0.5)

(defun edraw-color-css-fun-hsl (fname arglist-str)
  (let ((args (edraw-color-css-parse-arglist
               arglist-str t '("none") 3 3
               '((:allowed-units ("deg" "grad" "rad" "turn"  nil))
                 (:allowed-units ("%" nil) :number-ref 100.0)
                 (:allowed-units ("%" nil) :number-ref 100.0)
                 (:allowed-units ("%" nil) :number-ref 1.0)))))
    (list
     (edraw-color-from-hsl
      (car (nth 0 args))
      (max 0.0 (min 1.0 (car (nth 1 args))))
      (max 0.0 (min 1.0 (car (nth 2 args))))
      (max 0.0 (min 1.0 (or (car (nth 3 args)) 1.0))))
     :css-fname fname
     :css-args args
     :syntax 'css-color-function)))
;; TEST: (car (edraw-color-css-fun-hsl "hsl" "120deg,100,50,0.5")) => #s(edraw-color 0.0 1.0 0.0 0.5)

(defun edraw-color-css-fun-hwb (fname arglist-str)
  (let ((args (edraw-color-css-parse-arglist
               arglist-str t '("none") 3 3
               '((:allowed-units ("deg" "grad" "rad" "turn"  nil))
                 (:allowed-units ("%" nil) :number-ref 100.0)
                 (:allowed-units ("%" nil) :number-ref 100.0)
                 (:allowed-units ("%" nil) :number-ref 1.0)))))
    (list
     (edraw-color-from-hwb
      (car (nth 0 args))
      (max 0.0 (min 1.0 (car (nth 1 args))))
      (max 0.0 (min 1.0 (car (nth 2 args))))
      (max 0.0 (min 1.0 (or (car (nth 3 args)) 1.0))))
     :css-fname fname
     :css-args args
     :syntax 'css-color-function)))

(defun edraw-color-css-fun-lab (fname arglist-str)
  (let ((args (edraw-color-css-parse-arglist
               arglist-str t '("none") 3 3
               '((:allowed-units ("%" nil) :percent-ref 100.0)
                 (:allowed-units ("%" nil) :percent-ref 125.0)
                 (:allowed-units ("%" nil) :percent-ref 125.0)
                 (:allowed-units ("%" nil) :number-ref 1.0)))))
    (list
     (edraw-color-from-lab
      (max 0.0 (min 100.0 (car (nth 0 args))))
      (car (nth 1 args))
      (car (nth 2 args))
      (max 0.0 (min 1.0 (or (car (nth 3 args)) 1.0))))
     :css-fname fname
     :css-args args
     :syntax 'css-color-function)))

(defun edraw-color-css-fun-lch (fname arglist-str)
  (let ((args (edraw-color-css-parse-arglist
               arglist-str t '("none") 3 3
               '((:allowed-units ("%" nil) :percent-ref 100.0)
                 (:allowed-units ("%" nil) :percent-ref 150.0)
                 (:allowed-units ("deg" "grad" "rad" "turn"  nil))
                 (:allowed-units ("%" nil) :number-ref 1.0)))))
    (list
     (edraw-color-from-lch
      (max 0.0 (min 100.0 (car (nth 0 args))))
      (max 0.0 (car (nth 1 args)))
      (car (nth 2 args))
      (max 0.0 (min 1.0 (or (car (nth 3 args)) 1.0))))
     :css-fname fname
     :css-args args
     :syntax 'css-color-function)))

(defun edraw-color-css-fun-oklab (fname arglist-str)
  (let ((args (edraw-color-css-parse-arglist
               arglist-str t '("none") 3 3
               '((:allowed-units ("%" nil))
                 (:allowed-units ("%" nil) :percent-ref 0.4)
                 (:allowed-units ("%" nil) :percent-ref 0.4)
                 (:allowed-units ("%" nil) :number-ref 1.0)))))
    (list
     (edraw-color-from-oklab
      (max 0.0 (min 1.0 (car (nth 0 args))))
      (car (nth 1 args))
      (car (nth 2 args))
      (max 0.0 (min 1.0 (or (car (nth 3 args)) 1.0))))
     :css-fname fname
     :css-args args
     :syntax 'css-color-function)))

(defun edraw-color-css-fun-oklch (fname arglist-str)
  (let ((args (edraw-color-css-parse-arglist
               arglist-str t '("none") 3 3
               '((:allowed-units ("%" nil))
                 (:allowed-units ("%" nil) :percent-ref 0.4)
                 (:allowed-units ("deg" "grad" "rad" "turn"  nil))
                 (:allowed-units ("%" nil) :number-ref 1.0)))))
    (list
     (edraw-color-from-oklch
      (max 0.0 (min 1.0 (car (nth 0 args))))
      (max 0.0 (car (nth 1 args)))
      (car (nth 2 args))
      (max 0.0 (min 1.0 (or (car (nth 3 args)) 1.0))))
     :css-fname fname
     :css-args args
     :syntax 'css-color-function)))

;;;;;; Serialization

(defun edraw-color-css-make-hex-color (color &optional options)
  (edraw-color-make-hex-color
   color "#" nil
   (max
    (or (plist-get options :hex-digits-per-component) 2)
    (or (plist-get options :hex-min-digits-per-component) 0))
   (plist-get options :hex-upcase)))


(defun edraw-color-css-args-legacy-p (args)
  (equal (plist-get (cdr (nth 1 args)) :pre-separator) ","))

(defun edraw-color-css-options-modern-p (options)
  (or (plist-get options :css-use-modern-syntax)
      (let ((args (plist-get options :css-args)))
        (if args
            (not (edraw-color-css-args-legacy-p args))
          (plist-get options :css-default-modern-syntax)))))

(defun edraw-color-css-make-alpha-number (value unit options)
  (setq value (max 0.0 (min 1.0 value)))
  (concat
   (pcase unit
     ('nil (edraw-color-number-to-string
            value
            (or (plist-get options :css-decimal-places-alpha) 3)))
     ("%" (edraw-color-number-to-string (* value 100.0) 1))
     (_ (error "Unexpected unit: %s" unit)))
   unit))
;; TEST: (edraw-color-css-make-alpha-number (/ 237 255.0) nil nil) => "0.929"

(defun edraw-color-css-make-number-unit (value unit &optional
                                               number-ref
                                               percent-ref
                                               decimal-places-for-number
                                               options)
  (concat
   (pcase unit
     ('nil (edraw-color-number-to-string
            (* (or number-ref 1.0) value)
            decimal-places-for-number))
     ("%" (edraw-color-number-to-string
           (/ (* 100.0 value) (or percent-ref 1.0))
           (or (plist-get options :decimal-places-percent) 1))) ;; 100
     ("deg" (edraw-color-number-to-string
             value
             (or (plist-get options :decimal-places-deg) 0))) ;; 360
     ("rad" (edraw-color-number-to-string
             (/ (* float-pi value) 180.0)
             (or (plist-get options :decimal-places-rad) 2))) ;; 6.28
     ("grad" (edraw-color-number-to-string
              (/ (* 400.0 value) 360.0)
              (or (plist-get options :decimal-places-grad) 0))) ;; 400
     ("turn" (edraw-color-number-to-string
              (/ value 360.0)
              (or (plist-get options :decimal-places-turn) 3))) ;; 1
     (_ (error "Unknown unit: %s" unit)))
   unit))
;; TEST: (edraw-color-css-make-number-unit 0.25 nil 255.0) => "63.75"
;; TEST: (edraw-color-css-make-number-unit 0.001 "%" 255.0) => "0.1%"
;; TEST: (edraw-color-css-make-number-unit 120.1 "deg" 255.0) => "120deg"
;; TEST: (edraw-color-css-make-number-unit 120.1 "rad" 255.0) => "2.1rad"
;; TEST: (edraw-color-css-make-number-unit 120.1 "grad" 255.0) => "133grad"
;; TEST: (edraw-color-css-make-number-unit 120.1 "turn" 255.0) => "0.334turn"

(defun edraw-color-css-make-slash (options)
  (concat (plist-get options :css-spaces-before-slash)
          "/"
          (plist-get options :css-spaces-after-slash)))

(defun edraw-color-css-make-comma (options)
  (concat (plist-get options :css-spaces-before-comma)
          ","
          (plist-get options :css-spaces-after-comma)))

(defun edraw-color-css-make-alpha (alpha modern arg-props options)
  (when (< alpha 1.0)
    (concat
     (if modern
         (edraw-color-css-make-slash options)
       (edraw-color-css-make-comma options))
     (let ((unit (or (plist-get arg-props :unit)
                     (plist-get options :css-default-alpha-unit))))
       (concat
        (plist-get arg-props :pre-spaces)
        (edraw-color-css-make-alpha-number alpha unit options)
        (plist-get arg-props :post-spaces))))))

(defun edraw-color-css-make-color-function-impl
    (color options target-fname color-to-value-list-function arg-syntax-list
           &optional legacy-p unify-unit-p)
  ;; Legacy:
  ;;   rgb[a](r , g , b [, a])
  ;;   hsl[a](h , s , l [, a])
  ;; Modern:
  ;;   rgb[a](r g b [/ a])
  ;;   hsl[a](h s l [/ a])
  ;;   hwb(h w b [/ a])
  ;;   lab(l a b [/ a])
  ;;   lch(l c h [/ a])
  ;;   oklab(l a b [/ a])
  ;;   oklch(l c h [/ a])
  (let* ((has-alpha (< (edraw-color-a color) 1.0))
         (args (plist-get options :css-args))
         (args-for-this-fun-p
          (when-let* ((fname (plist-get options :css-fname)))
            (if (consp target-fname)
                (member (downcase fname) target-fname)
              (equal (downcase fname) target-fname)))))
    (concat
     (if (consp target-fname)
         (if legacy-p
             ;; ("rgb" "rgba")
             (if has-alpha (cadr target-fname) (car target-fname))
           (car target-fname))
       target-fname) ;; @todo Keep upcase?
     "("
     (cl-loop with comma = (when legacy-p (edraw-color-css-make-comma options))
              with unified-unit = (when unify-unit-p
                                    (or (when args-for-this-fun-p
                                          (plist-get (cdr (car args)) :unit))
                                        (plist-get
                                         options
                                         (plist-get (car arg-syntax-list)
                                                    :default-unit-keyword))))
              for i from 0
              for value in (funcall color-to-value-list-function color)
              for arg-syntax = (pop arg-syntax-list)
              for arg-props = (cdr (pop args))
              for unit = (if unify-unit-p
                             unified-unit
                           ;; Modern syntax allows different units for each arg
                           (or (plist-get arg-syntax :unit) ;; forced unit
                               (when args-for-this-fun-p
                                 (plist-get arg-props :unit))
                               (plist-get
                                options
                                (plist-get arg-syntax :default-unit-keyword))))
              concat
              (concat
               ;; Argument separator for legacy syntax
               (when (and legacy-p (> i 0)) comma)
               ;; Whitespaces before argument
               (plist-get arg-props :pre-spaces)
               ;; Argument
               (edraw-color-css-make-number-unit
                value unit
                (plist-get arg-syntax :number-ref)
                (plist-get arg-syntax :percent-ref)
                (plist-get arg-syntax :decimal-places-number))
               ;; Whitespaces after argument
               (or (plist-get arg-props :post-spaces)
                   ;; Argument separator for modern syntax
                   (when (and (not legacy-p) (< i 2)) " "))))
     ;; Alpha
     (when has-alpha
       (edraw-color-css-make-alpha (edraw-color-a color)
                                   (not legacy-p)
                                   (when args-for-this-fun-p (cdr (pop args)))
                                   options))
     ")")))

(defun edraw-color-css-make-rgb (color &optional options)
  ;; RGB ( https://www.w3.org/TR/css-color-4/#rgb-functions )
  (if (edraw-color-css-options-modern-p options)
      ;; <modern-rgb-syntax>
      ;; R: <number>|<percentage>|none  (0%~100% | 0~255) => 0~1
      ;; G: <number>|<percentage>|none  (0%~100% | 0~255) => 0~1
      ;; B: <number>|<percentage>|none  (0%~100% | 0~255) => 0~1
      (let ((arg-syntax
             `(:default-unit-keyword
               :css-default-rgb-unit
               :number-ref 255.0 :percent-ref 1.0
               :decimal-places-number
               ,(or (plist-get options :decimal-places-rgb-legacy) 0))))
        (edraw-color-css-make-color-function-impl
         color options '("rgb" "rgba") ;; Always use rgb
         #'edraw-color-to-rgb-list
         `(,arg-syntax ,arg-syntax ,arg-syntax)))
    ;; <legacy-rgb-syntax>
    ;;  : rgb(<number>    , <number>    , <number>     [, <alpha-value> ]?)
    ;;  | rgb(<percentage>, <percentage>, <percentage> [, <alpha-value> ]?)
    (let ((arg-syntax
           `(:default-unit-keyword
             :css-default-rgb-unit
             :number-ref 255.0 :percent-ref 1.0
             :decimal-places-number
             ,(or (plist-get options :decimal-places-rgb) 0))))
      (edraw-color-css-make-color-function-impl
       color options '("rgb" "rgba")
       #'edraw-color-to-rgb-list
       `(,arg-syntax ,arg-syntax ,arg-syntax)
       t t))))
;; TEST: (edraw-color-css-make-rgb (edraw-color-f 0.25 0.5 0.75 0.3)) => "rgba(64,128,191,0.3)"
;; TEST: (edraw-color-css-make-rgb (edraw-color-f 0.25 0.5 0.75 0.3) '(:css-use-modern-syntax t)) => "rgb(64 128 191/0.3)"

(defun edraw-color-css-make-hsl (color &optional options)
  ;; HSL ( https://www.w3.org/TR/css-color-4/#the-hsl-notation )
  (if (edraw-color-css-options-modern-p options)
      ;; <modern-hsl-syntax>
      ;; H: <hue>|none
      ;; S: <percentage>|<number>|none  (0%~100% | 0~100) => 0~1
      ;; L: <percentage>|<number>|none  (0%~100% | 0~100) => 0~1
      (edraw-color-css-make-color-function-impl
       color options '("hsl" "hsla") ;; Always use hsl
       #'edraw-color-to-hsl-list
       `((:default-unit-keyword
          :css-default-hue-unit
          :decimal-places-number 0)
         (:default-unit-keyword
          :css-default-saturation-unit
          :number-ref 100.0 :percent-ref 1.0
          :decimal-places-number 1)
         (:default-unit-keyword
          :css-default-lightness-unit
          :number-ref 100.0 :percent-ref 1.0
          :decimal-places-number 1)))
    ;; <legacy-hsl-syntax>
    ;;   : hsl( <hue> , <percentage> , <percentage> [ , <alpha-value> ]? )
    (edraw-color-css-make-color-function-impl
     color options '("hsl" "hsla")
     #'edraw-color-to-hsl-list
     '((:default-unit-keyword
        :css-default-hue-unit
        :decimal-places-number 0)
       (:unit "%")
       (:unit "%"))
     t)))
;; TEST: (edraw-color-css-make-hsl (edraw-color-f 0 0.5 0 0.3)) => "hsla(120,100%,25%,0.3)"

(defun edraw-color-css-make-hwb (color &optional options)
  ;; HWB ( https://www.w3.org/TR/css-color-4/#the-hwb-notation )
  ;;   H: <hue>|none
  ;;   W: <percentage>|<number>|none  (0%~100% | 0~100) => 0~1
  ;;   B: <percentage>|<number>|none  (0%~100% | 0~100) => 0~1
  ;; <hue> : <number> | <angle>
  (edraw-color-css-make-color-function-impl
   color options "hwb" #'edraw-color-to-hwb-list
   '((:default-unit-keyword
      :css-default-hue-unit
      :decimal-places-number 0)
     (:default-unit-keyword
      :css-default-lightness-unit
      :number-ref 100.0
      :decimal-places-number 1)
     (:default-unit-keyword
      :css-default-lightness-unit
      :number-ref 100.0
      :decimal-places-number 1))))

(defun edraw-color-css-make-lab (color &optional options)
  ;; Lab ( https://www.w3.org/TR/css-color-4/#specifying-lab-lch )
  ;;  L: <percentage>|<number>|none  (0~100% | 0~100) => 0~100
  ;;  a: <percentage>|<number>|none  (~-100%~100%~ | ~-125~125~) => ~-125~125~)
  ;;  b: <percentage>|<number>|none  (~-100%~100%~ | ~-125~125~) => ~-125~125~)
  (edraw-color-css-make-color-function-impl
   color options "lab" #'edraw-color-to-lab-list
   '((:default-unit-keyword
      :css-default-lab-luminance-unit
      :number-ref 1.0 :percent-ref 100.0
      :decimal-places-number 2)
     (:default-unit-keyword
      :css-default-lab-coordinate-unit
      :number-ref 1.0 :percent-ref 100.0
      :decimal-places-number 2)
     (:default-unit-keyword
      :css-default-lab-coordinate-unit
      :number-ref 1.0 :percent-ref 125.0
      :decimal-places-number 2))))

(defun edraw-color-css-make-lch (color &optional options)
  ;; LCH ( https://www.w3.org/TR/css-color-4/#specifying-lab-lch )
  ;;  L: <percentage>|<number>| none  (0%~100% | 0~100) => 0~100
  ;;  C: <percentage>|<number>| none  (0%~100%~ | 0~150~) => 0~150~
  ;;  H: <hue>|none
  (edraw-color-css-make-color-function-impl
   color options "lch" #'edraw-color-to-lch-list
   '((:default-unit-keyword
      :css-default-lab-luminance-unit
      :number-ref 1.0 :percent-ref 100.0
      :decimal-places-number 2)
     (:default-unit-keyword
      :css-default-lab-chroma-unit
      :number-ref 1.0 :percent-ref 150.0
      :decimal-places-number 2)
     (:default-unit-keyword
      :css-default-hue-unit
      :decimal-places-number 0))))

(defun edraw-color-css-make-oklab (color &optional options)
  ;; Lab ( https://www.w3.org/TR/css-color-4/#specifying-oklab-oklch )
  ;;  L: <percentage>|<number>|none  (0~100% | 0~1) => 0~1
  ;;  a: <percentage>|<number>|none  (~-100%~100%~ | ~-0.4~0.4~) => ~-0.4~0.4~)
  ;;  b: <percentage>|<number>|none  (~-100%~100%~ | ~-0.4~0.4~) => ~-0.4~0.4~)
  (edraw-color-css-make-color-function-impl
   color options "oklab" #'edraw-color-to-oklab-list
   '((:default-unit-keyword
      :css-default-oklab-luminance-unit
      :decimal-places-number 4)
     (:default-unit-keyword
      :css-default-oklab-coordinate-unit
      :number-ref 1.0 :percent-ref 0.4
      :decimal-places-number 4)
     (:default-unit-keyword
      :css-default-oklab-coordinate-unit
      :number-ref 1.0 :percent-ref 0.4
      :decimal-places-number 4))))

(defun edraw-color-css-make-oklch (color &optional options)
  ;; LCH ( https://www.w3.org/TR/css-color-4/#specifying-oklab-oklch )
  ;;  L: <percentage>|<number>| none  (0%~100% | 0~1) => 0~1
  ;;  C: <percentage>|<number>| none  (0%~100%~ | 0~0.4~) => 0~0.4~
  ;;  H: <hue>|none
  (edraw-color-css-make-color-function-impl
   color options "oklch" #'edraw-color-to-oklch-list
   '((:default-unit-keyword
      :css-default-oklab-luminance-unit
      :decimal-places-number 5)
     (:default-unit-keyword
      :css-default-oklab-chroma-unit
      :number-ref 1.0 :percent-ref 0.4
      :decimal-places-number 5)
     (:default-unit-keyword
      :css-default-hue-unit
      :decimal-places-number 0))))

(defun edraw-color-css-make-color-function (color &optional options)
  (let* ((fname (or (plist-get options :css-fname)
                    ;; @todo Ensure string?
                    (plist-get options :css-default-color-function)))
         (serializer
          (plist-get
           (alist-get fname edraw-color-css-functions nil nil #'string=)
           :serializer)))
    (if serializer
        (funcall serializer color options)
      ;; Fallback
      (edraw-color-css-make-rgb color options))))

(defun edraw-color-css-make-named-color (color &optional _options)
  (edraw-color-to-css-color-name color))

(defun edraw-color-css-serialize (color &optional options)
  "Convert COLOR to a string in CSS color syntax."

  (let ((syntax (plist-get options :syntax)))
    (or
     ;; Try converting to color name
     (when (and (not (plist-get options :disable-color-names))
                (or (eq syntax 'css-named-color)
                    (plist-get options :prefer-color-names))) ;;@todo Add :css-?
       (edraw-color-css-make-named-color color options))
     ;; Unable or unwilling to represent as a color name
     (pcase syntax
       ('css-hex-color
        (edraw-color-css-make-hex-color color options))
       ('css-color-function
        (edraw-color-css-make-color-function color options))
       (_ ;; Unspecified or named-color
        (pcase (plist-get options :css-default-color-syntax)
          ('hex-color
           (edraw-color-css-make-hex-color color options))
          ('color-function
           (edraw-color-css-make-color-function color options))
          (_
           (edraw-color-css-make-hex-color color options))))))))
;; TEST: (edraw-color-css-serialize (edraw-color-f 0.25 0.5 0.75 0.3)) => "#4080bf4c"

;;;;; LaTeX color syntax system

;; @todo impl

;;;;; Syntax systems

(defconst edraw-color-syntax-systems
  '((emacs
     :regexp edraw-color-emacs-regexp
     :regexp-clear-cache edraw-color-emacs-regexp-clear-cache
     :matched-color-info edraw-color-emacs-matched-color-info
     :serializer edraw-color-emacs-serialize)
    (css
     :regexp edraw-color-css-regexp
     :regexp-clear-cache edraw-color-css-regexp-clear-cache
     :matched-color-info edraw-color-css-matched-color-info
     :serializer edraw-color-css-serialize))
  "An alist of symbols representing syntax systems and plists that hold
information about them.")

(defun edraw-color-syntax-info (syntax-system)
  (alist-get (or syntax-system (edraw-color-syntax-system-default))
             edraw-color-syntax-systems))

(defun edraw-color-syntax-property (syntax-system key)
  (plist-get (edraw-color-syntax-info syntax-system) key))

(defun edraw-color-syntax-property-non-nil (syntax-system key)
  (or
   (plist-get (edraw-color-syntax-info syntax-system) key)
   (error "Failed to get syntax systam property %s of %s" key syntax-system)))

(defun edraw-color-syntax-regexp-clear-cache-all ()
  "Clear all regular expression caches."
  (interactive)
  (dolist (type-props edraw-color-syntax-systems)
    (when-let* ((fun (plist-get (cdr type-props) :regexp-clear-cache)))
      (funcall fun))))

(defun edraw-color-syntax-regexp (syntax-system)
  "Return a regexp that may match a color text in SYNTAX-SYSTEM.

Note that a match does not necessarily mean that the matched text is valid.

After matching this regular expression, call
`edraw-color-syntax-matched-color-info' to get detailed information
about the matched color and writing style."
  (funcall (edraw-color-syntax-property-non-nil syntax-system :regexp)))

(defun edraw-color-syntax-matched-color-info (syntax-system &optional string)
  "Create color information from the current match data.

When called after a successful search using the regular expression
returned by `edraw-color-syntax-regexp', this function returns a cons
cell containing the color value and syntax information from the match
data.

The car of the returned value is an `edraw-color' object. The cdr is a
property list containing the information obtained by syntax parsing.

The properties always include:

- :begin : The position in the buffer where the color text begins.
- :end : The position in the buffer where the color text ends.
- :syntax : A symbol representing the type of syntax that was matched.
- :syntax-system : The syntax system symbol.

The rest depend on the value of the :syntax property."
  (funcall
   (edraw-color-syntax-property-non-nil syntax-system :matched-color-info)
   string))

(defun edraw-color-syntax-serialize (color options &optional syntax-system)
  "Convert COLOR to a string in specified SYNTAX-SYSTEM.

COLOR is an `edraw-color' object.

OPTIONS is a plist that holds various information for controlling the
string format. By including the properties returned by
`edraw-color-syntax-matched-color-info' in this plist, a string can be
created that closely preserves the original format whenever possible.
Additionally, various options can be specified, such as preferred syntax
and number precision.

When SYNTAX-SYSTEM is nil, it is inferred by the
`edraw-color-syntax-system-default' function."
  (funcall
   (edraw-color-syntax-property-non-nil syntax-system :serializer)
   color options))

;;;;; Syntax system detection

(defvar-local edraw-color-syntax-system-of-buffer nil
  "The syntax system of the buffer.

nil means unknown, and `edraw-color-syntax-systems-by-major-mode' is used.
See `edraw-color-syntax-system-default'.")

(defvar edraw-color-syntax-systems-by-major-mode
  '((Custom-mode . emacs)
    (css-mode . css)
    (html-mode . css)
    (mhtml-mode . css)
    (js-mode . css)
    (web-mode . css)
    (nxml-mode . css))
  "An alist of syntax systems for each major mode.
See `edraw-color-syntax-system-default'.")

(defun edraw-color-syntax-system-default (&optional default-syntax-system)
  "Return the default syntax system for the current buffer.

The order of precedence is:
1. `edraw-color-syntax-system-of-buffer' variable
2. `edraw-color-syntax-systems-by-major-mode' variable
3. DEFAULT-SYNTAX-SYSTEM argument
4. \\='css"
  (or
   edraw-color-syntax-system-of-buffer
   (alist-get major-mode edraw-color-syntax-systems-by-major-mode)
   default-syntax-system
   'css))

;;;;; Searching and parsing color text

(defun edraw-color--in-regexp (pos regexp &optional include-end)
  "Return non-nil if POS is within the match range of REGEXP.

When this function returns non-nil, the match data is set. This match
data corresponds to a match where POS is included within the range of
REGEXP. POS is between (match-beginning 0) and (match-end 0).

If INCLUDE-END is non-nil, a match whose end is the same as POS is
considered successful, except that a match whose start is the same as
POS is preferred."

  (save-excursion
    (goto-char pos)
    (let ((eol (pos-eol))
          (match-data nil))
      (forward-line 0)
      (while (and (re-search-forward regexp eol t)
                  (let ((beg (match-beginning 0))
                        (end (match-end 0)))
                    (when (<= beg pos)
                      (cond
                       ((and (= pos end) include-end)
                        (setq match-data (match-data))) ;; Save candidate
                       ((< pos end)
                        (setq match-data 'current))))
                    (<= end pos))))
      (when (and match-data (not (eq match-data 'current)))
        (set-match-data match-data t))
      (not (null match-data)))))

(defun edraw-color-info-at (pos &optional syntax-system include-end)
  "Return the color information of the color text written at the current
point.

The color text must be written in the syntax specified by SYNTAX-SYSTEM.
When SYNTAX-SYSTEM is nil, it is inferred by the
`edraw-color-syntax-system-default' function.

The range of the color text must include the current point.

If INCLUDE-END is non-nil, the color text is considered to include the
current point even if it ends at the same position as the current
point. However, if there is another color text that starts at the
current point, that takes precedence."
  (when (edraw-color--in-regexp pos
                                (edraw-color-syntax-regexp syntax-system)
                                include-end)
    (ignore-errors
      (edraw-color-syntax-matched-color-info syntax-system))))

(defun edraw-color-info-looking-at (&optional syntax-system)
  "Return the color information of the color text written immediately after
the current point.

The color text must be written in the syntax specified by SYNTAX-SYSTEM.
When SYNTAX-SYSTEM is nil, it is inferred by the
`edraw-color-syntax-system-default' function.

The color text must start at the current point. There may be no
whitespace or other extraneous characters between the point and the
color text."
  (when (looking-at (edraw-color-syntax-regexp syntax-system))
    (ignore-errors
      (edraw-color-syntax-matched-color-info syntax-system))))

(defun edraw-color-info-from-string (string
                                     &optional position syntax-system
                                     trailing-regexp)
  "Return the color information of the color text written at the POSITION
in STRING.

The color text must be written in the syntax specified by SYNTAX-SYSTEM.
When SYNTAX-SYSTEM is nil, it is inferred by the
`edraw-color-syntax-system-default' function.

The color text must start at the POSITION in STRING. Leading blank
characters are ignored, but nothing else is ignored. In other
words, (edraw-color-info-from-string \" #112233\" 0) matches,
but (edraw-color-info-from-string \"dustdata #112233\" 0) does not
match. Does not check that the string ends with a delimiter. In other
words, if (edraw-color-info-from-string \"#1122334455\" 0 \\='css) is
evaluated, it will match the \"#11223344\" part. If POSITION is nil, 0
is assumed to be specified."
  (when (and (string-match (concat
                            "\\(?:\\(?:[ \t\n\r\f]*\\(?:"
                            (edraw-color-syntax-regexp syntax-system)
                            "\\)"
                            (pcase trailing-regexp
                              ('eos "\\'")
                              ('ws-eos "[ \t\n\r\f]*\\'")
                              ((pred stringp) trailing-regexp))
                            "\\)\\|\\)")
                           string position)
             (< (match-beginning 0) (match-end 0)))
    (ignore-errors
      (edraw-color-syntax-matched-color-info syntax-system string))))
;; TEST: (car (edraw-color-info-from-string "Dust   rgba(64 128 192 / 0.5)dust" 4 'css)) => #s(edraw-color 0.25098039215686274 0.5019607843137255 0.7529411764705882 0.5)
;; TEST: (car (edraw-color-info-from-string "rgba(64 128 192 / 0.5)" 0 'css)) => #s(edraw-color 0.25098039215686274 0.5019607843137255 0.7529411764705882 0.5)
;; TEST: (edraw-color-info-from-string "Dust   #44448888cccc dust" 4 'emacs) => (#s(edraw-color 0.26666666666666666 0.5333333333333333 0.8 1.0) :num-components 3 :hex-digits-per-component 4 :syntax emacs-hex-color :begin 4 :end 20 :syntax-system emacs)
;; TEST: (edraw-color-info-from-string "Dust   #44448888cccc dust" 4 'css) => (#s(edraw-color 0.26666666666666666 0.26666666666666666 0.5333333333333333 0.5333333333333333) :num-components 4 :hex-digits-per-component 2 :syntax css-hex-color :begin 4 :end 16 :syntax-system css)
;; TEST: (car (edraw-color-info-from-string "Dust   hsl( 30 50 50/0.5)dust" 4 'css)) => #s(edraw-color 0.75 0.5 0.25 0.5)
;; TEST: (car (edraw-color-info-from-string "Dust   hsl( 30 50 50)   " 4 'css " *\\'")) => #s(edraw-color 0.75 0.5 0.25 1.0)
;; TEST: (edraw-color-info-from-string "Dust   hsl( 30 50 50) dust " 4 'css " *\\'") => nil


(provide 'edraw-color)
;;; edraw-color.el ends here
