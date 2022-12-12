;;; edraw-math.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Graphics, Drawing, SVG

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

(require 'cl-generic)

;;;; Number

(cl-defmethod edraw-clamp ((n number) min-value max-value)
  (cond
   ((and min-value (< n min-value)) min-value)
   ((and max-value (> n max-value)) max-value)
   (t n)))

(defun edraw-quadratic-roots (a b c)
  "Return real roots of quadratic equation."
  (if (= a 0)
      (if (= b 0)
          nil
        (list (/ (float (- c)) b)))
    (let ((d (- (* (float b) b) (* 4.0 a c))))
      (if (> d 0)
          (let ((sqrt-d (sqrt d)))
            (list
             (/ (+ (- b) sqrt-d) (* 2.0 a))
             (/ (- (- b) sqrt-d) (* 2.0 a))))
        (if (= d 0)
            (list (/ (- b) (* 2.0 a)))
          nil)))))

(defun edraw-min-max-update (x-min-max n)
  (if x-min-max
      (cond
       ((< n (car x-min-max)) (cons n (cdr x-min-max)))
       ((> n (cdr x-min-max)) (cons (car x-min-max) n))
       (t x-min-max))
    (cons n n)))

;;;; Vector

(defmacro edraw-xy (x y) `(cons ,x ,y))
(defmacro edraw-x (xy) `(car ,xy))
(defmacro edraw-y (xy) `(cdr ,xy))

(defsubst edraw-xy-zero-p (a)
  (and (= (car a) 0)
       (= (cdr a) 0)))

(defsubst edraw-xy-equal-p (a b)
  (equal a b))

(defsubst edraw-xy-assign (a b)
  (setcar a (car b))
  (setcdr a (cdr b)))

(defsubst edraw-xy-clone (cell)
  (cons (car cell) (cdr cell)))

(defsubst edraw-xy-add (a b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

(defsubst edraw-xy-sub (a b)
  (cons (- (car a) (car b))
        (- (cdr a) (cdr b))))

(defsubst edraw-xy-nmul (n xy)
  (cons (* n (car xy))
        (* n (cdr xy))))

(defsubst edraw-xy-divn (xy n)
  (cons (/ (car xy) n)
        (/ (cdr xy) n)))

(defun edraw-xy-length (xy)
  (let ((x (car xy))
        (y (cdr xy)))
    (sqrt (+ (* x x) (* y y)))))

(defun edraw-xy-length-squared (xy)
  (let ((x (car xy))
        (y (cdr xy)))
    (+ (* x x) (* y y))))

(defun edraw-xy-normalize (xy)
  (let ((len (edraw-xy-length xy)))
    (edraw-xy-divn xy len)))

(defun edraw-xy-dot (a b)
  "Return |A|*|B|*cos(the angle between A and B)."
  (+ (* (car a) (car b))
     (* (cdr a) (cdr b))))

(defun edraw-xy-perpdot (a b)
  "Return |A|*|B|*sin(the angle from A to B)."
  (- (* (car a) (cdr b))
     (* (cdr a) (car b))))

(defun edraw-xy-rot90 (xy)
  (cons (- (cdr xy)) (car xy)))

(defun edraw-xy-rot180 (xy)
  (cons (- (car xy)) (- (cdr xy))))

(defun edraw-xy-rot270 (xy)
  (cons (cdr xy) (- (car xy))))

(defun edraw-xy-list-equal-all-p (xy-list)
  (let ((first (car xy-list))
        (rest (cdr xy-list)))
    (while (and rest (edraw-xy-equal-p first (car rest)))
      (setq rest (cdr rest)))
    (null rest)))

;;;; Point

(defsubst edraw-xy-midpoint-float (a b)
  (cons (* 0.5 (+ (car a) (car b)))
        (* 0.5 (+ (cdr a) (cdr b)))))

(defun edraw-xy-distance (a b)
  "Return euclidean distance between A and B."
  (let ((dx (- (car a) (car b)))
        (dy (- (cdr a) (cdr b))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun edraw-xy-distance-squared (a b)
  "Return squared euclidean distance between A and B."
  (let ((dx (- (car a) (car b)))
        (dy (- (cdr a) (cdr b))))
    (+ (* dx dx) (* dy dy))))

(defun edraw-xy-distance-l-inf (a b)
  "Return Chebyshev distance between A and B."
  (max (abs (- (car a) (car b)))
       (abs (- (cdr a) (cdr b)))))

(defun edraw-in-circle-p (xy center radius)
  (<= (edraw-xy-distance-squared xy center) (* radius radius)))

(defun edraw-in-square-p (xy center radius)
  (<= (edraw-xy-distance-l-inf xy center) radius))

(defsubst edraw-xy-empty-aabb-p (a b)
  (or (= (car a) (car b))
      (= (cdr a) (cdr b))))

;;;; Rectangle

(defmacro edraw-rect-left (rect) `(caar ,rect))
(defmacro edraw-rect-top (rect) `(cdar ,rect))
(defmacro edraw-rect-right (rect) `(cadr ,rect))
(defmacro edraw-rect-bottom (rect) `(cddr ,rect))
(defmacro edraw-rect-width (rect) `(- (cadr ,rect) (caar ,rect)))
(defmacro edraw-rect-height (rect) `(- (cddr ,rect) (cdar ,rect)))
(defmacro edraw-rect-xy0 (rect) `(car ,rect))
(defmacro edraw-rect-xy1 (rect) `(cdr ,rect))
(defmacro edraw-rect-lt (rect) `(edraw-rect-xy0 ,rect))
(defmacro edraw-rect-rb (rect) `(edraw-rect-xy1 ,rect))

(defsubst edraw-rect (x0 y0 x1 y1)
  (cons
   (cons x0 y0)
   (cons x1 y1)))

(defsubst edraw-rect-xywh (x y w h)
  (cons
   (cons x y)
   (cons (+ x w) (+ y h))))

(defsubst edraw-rect-pp (p0 p1)
  (cons
   (edraw-xy-clone p0)
   (edraw-xy-clone p1)))

(defun edraw-rect-clone (rect)
  (when rect
    (edraw-rect
     (edraw-rect-left rect)
     (edraw-rect-top rect)
     (edraw-rect-right rect)
     (edraw-rect-bottom rect))))

(defun edraw-aabb (&rest points)
  (when points
    (let ((x0 (car (car points)))
          (y0 (cdr (car points)))
          (x1 (car (car points)))
          (y1 (cdr (car points))))
      (dolist (xy (cdr points))
        (let ((x (car xy))
              (y (cdr xy)))
          (if (< x x0) (setq x0 x))
          (if (< y y0) (setq y0 y))
          (if (> x x1) (setq x1 x))
          (if (> y y1) (setq y1 y))))
      (edraw-rect x0 y0 x1 y1))))

(defun edraw-square (center radius)
  (edraw-rect
   (- (car center) radius)
   (- (cdr center) radius)
   (+ (car center) radius)
   (+ (cdr center) radius)))

(defun edraw-rect-contains-point-p (rect xy)
  (let ((x (car xy))
        (y (cdr xy)))
    (and (<= (caar rect) x) (< x (cadr rect))
         (<= (cdar rect) y) (< y (cddr rect)))))

(defun edraw-rect-intersects-rect-p (rect1 rect2)
  (if (or (edraw-rect-empty-p rect1)
          (edraw-rect-empty-p rect2))
      nil
    (and
     (< (caar rect1) (cadr rect2))
     (> (cadr rect1) (caar rect2))
     (< (cdar rect1) (cddr rect2))
     (> (cddr rect1) (cdar rect2)))))

(defun edraw-rect-center (rect)
  (cons
   (* 0.5 (+ (caar rect) (cadr rect)))
   (* 0.5 (+ (cdar rect) (cddr rect)))))


(defun edraw-rect-empty-p (rect)
  (not (and rect
            (< (edraw-rect-left rect) (edraw-rect-right rect))
            (< (edraw-rect-top rect) (edraw-rect-bottom rect)))))

(defun edraw-rect-union (r1 r2)
  (cond
   ((edraw-rect-empty-p r1) (edraw-rect-clone r2))
   ((edraw-rect-empty-p r2) (edraw-rect-clone r1))
   (t
    (edraw-rect
     (min (edraw-rect-left r1) (edraw-rect-left r2))
     (min (edraw-rect-top r1) (edraw-rect-top r2))
     (max (edraw-rect-right r1) (edraw-rect-right r2))
     (max (edraw-rect-bottom r1) (edraw-rect-bottom r2))))))

;;;; Matrix

(defun edraw-matrix (&optional init)
  (unless init (setq init [1 0 0 1 0 0]))
  (cond
   ((or (listp init) (vectorp init))
    (cond
     ((= (length init) 6)
      (vector
       (elt init 0) (elt init 1) 0 0
       (elt init 2) (elt init 3) 0 0
       0 0 1 0
       (elt init 4) (elt init 5) 0 1))
     ((= (length init) 16)
      (apply 'vector (mapcar 'identity init)))
     (t (error "Invalid init argument"))))
   ;;@todo support string
   (t (error "Invalid init argument"))))

(defun edraw-matrix-move-origin (mat ox oy oz)
  (if (or (/= ox 0) (/= oy 0)(/= oz 0))
      (edraw-matrix-mul-mat-mat
       (edraw-matrix-mul-mat-mat
        (edraw-matrix-translate ox oy oz)
        mat)
       (edraw-matrix-translate (- ox) (- oy) (- oz)))
    mat))

(defun edraw-matrix-move-origin-xy (mat xy)
  (if xy
      (edraw-matrix-move-origin mat (edraw-x xy) (edraw-y xy) 0)
    mat))

(defun edraw-matrix-translate (dx dy dz)
  (edraw-matrix (vector 1 0 0 0  0 1 0 0  0 0 1 0  dx dy dz 1)))

(defun edraw-matrix-scale (sx sy sz)
  (edraw-matrix (vector sx 0 0 0  0 sy 0 0  0 0 sz 0  0 0 0 1)))


(defun edraw-matrix-rotate (deg)
  (let ((d (mod deg 360)))
    (cond
     ((= d 0) (edraw-matrix (vector 1 0 0 1 0 0)))
     ((= d 90) (edraw-matrix (vector 0 1 -1 0 0 0)))
     ((= d 180) (edraw-matrix (vector -1 0 0 -1 0 0)))
     ((= d 270) (edraw-matrix (vector 0 -1 1 0 0 0)))
     (t
      (let* ((rad (degrees-to-radians deg))
             (c (cos rad))
             (s (sin rad)))
        (edraw-matrix (vector c s (- s) c 0 0)))))))

(defun edraw-matrix-rotate-x (deg)
  (let* ((rad (degrees-to-radians deg))
         (c (cos rad))
         (s (sin rad)))
    (edraw-matrix (vector 1 0 0 0  0 c s 0  0 (- s) c 0  0 0 0 1))))

(defun edraw-matrix-rotate-y (deg)
  (let* ((rad (degrees-to-radians deg))
         (c (cos rad))
         (s (sin rad)))
    (edraw-matrix (vector c 0 (- s) 0  0 1 0 0  s 0 c 0  0 0 0 1))))

(defun edraw-matrix-rotate-z (deg)
  (let* ((rad (degrees-to-radians deg))
         (c (cos rad))
         (s (sin rad)))
    (edraw-matrix (vector c s 0 0  (- s) c 0 0  0 0 1 0  0 0 0 1))))

(defun edraw-matrix-skew (ax-deg ay-deg)
  (let* ((ax-rad (degrees-to-radians ax-deg))
         (ay-rad (degrees-to-radians ay-deg))
         (tx (tan ax-rad))
         (ty (tan ay-rad)))
    (edraw-matrix (vector 1 ty tx 1 0 0))))

(defmacro edraw-matrix-at (mat index)
  `(aref ,mat ,index))

(defmacro edraw-matrix-let-elements (mat-symbol var-prefix &rest body)
  (declare (indent 2))
  `(let (,@(cl-loop for i from 0 to 15
                    collect
                    (list
                     (intern
                      (format "%s%d%d" var-prefix (1+ (/ i 4)) (1+ (% i 4))))
                     `(aref ,mat-symbol ,i))))
     ,@body))

(defun edraw-matrix-identity-p (mat)
  (or
   (null mat)
   (edraw-matrix-let-elements mat m
     (and
      (= 1 m11 m22 m33 m44)
      (= 0 m12 m13 m14 m21 m23 m24 m31 m32 m34 m41 m42 m43)))))

(defun edraw-matrix-translate-only-p (mat)
  (or
   (null mat)
   (edraw-matrix-let-elements mat m
     (ignore m41 m42 m43);;suppress warnings
     (and
      (= 1 m11 m22 m33 m44)
      (= 0 m12 m13 m14 m21 m23 m24 m31 m32 m34)))))

(defun edraw-matrix-mul-dispatch (a b)
  (let (lb)
    (cond
     ((= (length a) 16)
      (cond
       ((and (consp b) (numberp (car b)) (numberp (cdr b)))
        (edraw-matrix-mul-mat-xy a b))
       ((= (setq lb (length b)) 16) (edraw-matrix-mul-mat-mat a b))
       ((= lb 4) (edraw-matrix-mul-mat-vec4 a b))
       ((= lb 3) (edraw-matrix-mul-mat-vec3 a b))
       ((= lb 2) (edraw-matrix-mul-mat-vec2 a b))
       (t (error "no applicable method matrix-mul %s %s" a b))))
     (t (error "no applicable method matrix-mul %s %s" a b)))))

(defmacro edraw-matrix-mul (&rest args)
  (pcase (length args)
    (0 nil)
    (1 (car args))
    (2 `(edraw-matrix-mul-dispatch ,(car args) ,(cadr args)))
    (_ `(edraw-matrix-mul
         (edraw-matrix-mul-dispatch ,(car args) ,(cadr args))
         ,@(cddr args)))))

(defmacro edraw-matrix-mul-element (lhs rhs col row)
  `(+ (* (aref ,lhs ,(+ row 0)) (aref ,rhs ,(+ (* col 4) 0)))
      (* (aref ,lhs ,(+ row 4)) (aref ,rhs ,(+ (* col 4) 1)))
      (* (aref ,lhs ,(+ row 8)) (aref ,rhs ,(+ (* col 4) 2)))
      (* (aref ,lhs ,(+ row 12)) (aref ,rhs ,(+ (* col 4) 3)))))

(defun edraw-matrix-mul-mat-mat (a b)
  (if a
      (if b
          (vector
           (edraw-matrix-mul-element a b 0 0)
           (edraw-matrix-mul-element a b 0 1)
           (edraw-matrix-mul-element a b 0 2)
           (edraw-matrix-mul-element a b 0 3)
           (edraw-matrix-mul-element a b 1 0)
           (edraw-matrix-mul-element a b 1 1)
           (edraw-matrix-mul-element a b 1 2)
           (edraw-matrix-mul-element a b 1 3)
           (edraw-matrix-mul-element a b 2 0)
           (edraw-matrix-mul-element a b 2 1)
           (edraw-matrix-mul-element a b 2 2)
           (edraw-matrix-mul-element a b 2 3)
           (edraw-matrix-mul-element a b 3 0)
           (edraw-matrix-mul-element a b 3 1)
           (edraw-matrix-mul-element a b 3 2)
           (edraw-matrix-mul-element a b 3 3))
        (edraw-matrix a));;clone
    (if b
        (edraw-matrix b);;clone
      nil)))

(defun edraw-matrix-mul-mat-vec4 (a b)
  (if b
      (if a
          (vector
           (edraw-matrix-mul-element a b 0 0)
           (edraw-matrix-mul-element a b 0 1)
           (edraw-matrix-mul-element a b 0 2)
           (edraw-matrix-mul-element a b 0 3))
        (copy-sequence b))))

(defun edraw-matrix-mul-mat-vec3 (a b)
  (if b
      (if a
          (let* ((p (edraw-matrix-mul-mat-vec4 a (vector (aref b 0) (aref b 1) (aref b 2) 1.0)))
                 (w (aref p 3)))
            (vector
             (/ (aref p 0) w)
             (/ (aref p 1) w)
             (/ (aref p 2) w)))
        (copy-sequence b))))

(defun edraw-matrix-mul-mat-vec2 (a b)
  (if b
      (if a
          (let ((x (+ (* (aref a 0) (aref b 0)) (* (aref a 4) (aref b 1)) (aref a 12)))
                (y (+ (* (aref a 1) (aref b 0)) (* (aref a 5) (aref b 1)) (aref a 13)))
                (w (+ (* (aref a 3) (aref b 0)) (* (aref a 7) (aref b 1)) (aref a 15))))
            (cons
             (/ x w)
             (/ y w)))
        (copy-sequence b))))

(defun edraw-matrix-mul-mat-xy (a b &optional dst)
  (if b
      (if a
          (let ((x (+ (* (aref a 0) (car b)) (* (aref a 4) (cdr b)) (aref a 12)))
                (y (+ (* (aref a 1) (car b)) (* (aref a 5) (cdr b)) (aref a 13)))
                (w (+ (* (aref a 3) (car b)) (* (aref a 7) (cdr b)) (aref a 15))))
            (if dst
                (progn
                  (setcar dst (/ x w))
                  (setcdr dst (/ y w))
                  dst)
              (cons
               (/ x w)
               (/ y w))))
        (if dst
            (progn
              (setcar dst (car b))
              (setcdr dst (cdr b))
              dst)
          (cons
           (car b)
           (cdr b))))))

(defun edraw-matrix-determinant (mat)
  (edraw-matrix-let-elements mat m
    (+
     (* (- (* m33 m44) (* m43 m34)) (- (* m11 m22) (* m21 m12)))
     (* (- (* m43 m14) (* m13 m44)) (- (* m31 m22) (* m21 m32)))
     (* (- (* m23 m44) (* m43 m24)) (- (* m31 m12) (* m11 m32)))
     (* (- (* m23 m34) (* m33 m24)) (- (* m11 m42) (* m41 m12)))
     (* (- (* m13 m34) (* m33 m14)) (- (* m41 m22) (* m21 m42)))
     (* (- (* m13 m24) (* m23 m14)) (- (* m31 m42) (* m41 m32))))))

(defun edraw-matrix-inverse (mat)
  (edraw-matrix-let-elements mat m
    ;; https://www.geometrictools.com/Documentation/LaplaceExpansionTheorem.pdf
    (let* ((s1 (- (* m11 m22) (* m12 m21)))
           (s2 (- (* m11 m32) (* m12 m31)))
           (s3 (- (* m11 m42) (* m12 m41)))
           (s4 (- (* m21 m32) (* m22 m31)))
           (s5 (- (* m21 m42) (* m22 m41)))
           (s6 (- (* m31 m42) (* m32 m41)))

           (c6 (- (* m33 m44) (* m34 m43)))
           (c5 (- (* m23 m44) (* m24 m43)))
           (c4 (- (* m23 m34) (* m24 m33)))
           (c3 (- (* m13 m44) (* m14 m43)))
           (c2 (- (* m13 m34) (* m14 m33)))
           (c1 (- (* m13 m24) (* m14 m23)))
           (det (+ (* s1 c6)
                   (- (* s2 c5))
                   (* s3 c4)
                   (* s4 c3)
                   (- (* s5 c2))
                   (* s6 c1))))
      (when (/= det 0.0)
        (let ((idet (/ 1.0 det)))
          (edraw-matrix
           (vector
            (* idet (+    (* m22 c6) (- (* m32 c5))   (* m42 c4)))
            (* idet (+ (- (* m12 c6))   (* m32 c3) (- (* m42 c2))))
            (* idet (+    (* m12 c5) (- (* m22 c3))   (* m42 c1)))
            (* idet (+ (- (* m12 c4))   (* m22 c2) (- (* m32 c1))))

            (* idet (+ (- (* m21 c6))   (* m31 c5) (- (* m41 c4))))
            (* idet (+    (* m11 c6) (- (* m31 c3))   (* m41 c2)))
            (* idet (+ (- (* m11 c5))   (* m21 c3) (- (* m41 c1))))
            (* idet (+    (* m11 c4) (- (* m21 c2))   (* m31 c1)))

            (* idet (+    (* m24 s6) (- (* m34 s5))   (* m44 s4)))
            (* idet (+ (- (* m14 s6))   (* m34 s3) (- (* m44 s2))))
            (* idet (+    (* m14 s5) (- (* m24 s3))   (* m44 s1)))
            (* idet (+ (- (* m14 s4))   (* m24 s2) (- (* m34 s1))))

            (* idet (+ (- (* m23 s6))   (* m33 s5) (- (* m43 s4))))
            (* idet (+    (* m13 s6) (- (* m33 s3))   (* m43 s2)))
            (* idet (+ (- (* m13 s5))   (* m23 s3) (- (* m43 s1))))
            (* idet (+    (* m13 s4) (- (* m23 s2))   (* m33 s1))))))))))

;;TEST: (edraw-matrix-inverse (edraw-matrix-translate 10 20 30)) => [1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 -10.0 -20.0 -30.0 1.0]
;;TEST: (edraw-matrix-inverse (edraw-matrix-scale 2 4 8)) => [0.5 0.0 0.0 0.0 0.0 0.25 0.0 0.0 0.0 0.0 0.125 0.0 0.0 0.0 0.0 1.0]
;;TEST: (edraw-matrix-inverse (edraw-matrix-rotate 45)) => [0.7071067811865476 -0.7071067811865475 0.0 0.0 0.7071067811865475 0.7071067811865476 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0]

(defun edraw-matrix-translate-add (mat x y &optional z)
  (when (numberp x) (aset mat 12 (+ (aref mat 12) x)))
  (when (numberp y) (aset mat 13 (+ (aref mat 13) y)))
  (when (numberp z) (aset mat 14 (+ (aref mat 14) z)))
  mat)



;;;; Bezier Curve

(defun edraw-quadratic-bezier-curve-to-cubic (p0 p1 p2)
  (cons
   ;; p0 + 2/3*(p1-p0)
   (edraw-xy-add
    p0
    (edraw-xy-divn
     (edraw-xy-nmul 2.0 (edraw-xy-sub p1 p0)) 3))
   ;; p2 + 2/3*(p1-p2)
   (edraw-xy-add
    p2
    (edraw-xy-divn
     (edraw-xy-nmul 2.0 (edraw-xy-sub p1 p2)) 3))))

(defun edraw-cubic-bezier (x0 x1 x2 x3 T)
  "Return (1-T)^3*x0 + 3*(1-T)^2*T + 3(1-T)*T^2*x2 + T^3*x3."
  (let* ((u (- 1 T))
         (uu (* u u))
         (TT (* T T)))
    (+
     (* u uu x0)
     (* T uu x1 3)
     (* u TT x2 3)
     (* T TT x3))))

(defun edraw-cubic-bezier-min-max (x0 x1 x2 x3)
  "Return minimum and maximum value of (edraw-cubic-bezier x0 xy x2 x3 t).

t ranges from0 to 1."
  (when (> x0 x3)
    (setq x0 (prog1 x3 (setq x3 x0)))
    (setq x1 (prog1 x2 (setq x2 x1))))
  (if (and (<= x0 x1 x3)
           (<= x0 x2 x3))
      (cons x0 x3)
    ;; x = x0(1-t)^3 + 3x1t(1-t)^2 + 3x2t^2(1-t) + x3t^3
    ;; dx/dt = (3x3 - 9x2 + 9x1 - 3x0)t^2 + (6x0 - 12x1 + 6x2)t + 3(x1-x0)
    ;;       = 0
    ;; t =
    (let* ((ts (edraw-quadratic-roots
                (* 3 (+ x3 (* 3 (+ (- x2) x1)) (- x0)))
                (* 6 (+ x0 (* -2 x1) x2))
                (* 3 (- x1 x0))))
           (xs (mapcar (lambda (T)
                         (if (< 0 T 1)
                             (edraw-cubic-bezier x0 x1 x2 x3 T)
                           x0))
                       ts)))
      (cons
       (apply #'min x0 xs)
       (apply #'max x3 xs)))))

;;TEST: (edraw-cubic-bezier-min-max 10 20 30 40) => (10 . 40)
;;TEST: (edraw-cubic-bezier-min-max 20 10 30 40) => (17.274575140626318 . 40)
;;TEST: (edraw-cubic-bezier-min-max 20 50 30 40) => (20 . 40)
;;TEST: (edraw-cubic-bezier-min-max 20 60 30 40) => (20 . 41.51741155165272)
;;TEST: (edraw-cubic-bezier-min-max 50 90 0 40) => (29.495658176348943 . 60.50434182365106)
;;TEST: (edraw-cubic-bezier-min-max 50 90 0 40) => (29.495658176348943 . 60.50434182365106)
;;TEST: (edraw-cubic-bezier-min-max 50 90 10 50) => (38.45299461620749 . 61.54700538379252)
;;TEST: (edraw-cubic-bezier-min-max 50 50 10 50) => (32.22222222222223 . 50)
;;TEST: (edraw-cubic-bezier-min-max 20 0 0 20)

(defun edraw-cubic-bezier-min-max-update (x-min-max x0 x1 x2 x3)
  (if x-min-max
      (let ((x-min (car x-min-max))
            (x-max (cdr x-min-max)))
        (if (and (<= x-min x0 x-max)
                 (<= x-min x1 x-max)
                 (<= x-min x2 x-max)
                 (<= x-min x3 x-max))
            x-min-max
          (let ((mm (edraw-cubic-bezier-min-max x0 x1 x2 x3)))
            (cons
             (min (car mm) x-min)
             (max (cdr mm) x-max)))))
    (edraw-cubic-bezier-min-max x0 x1 x2 x3)))

(provide 'edraw-math)
;;; edraw-math.el ends here
