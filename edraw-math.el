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

(require 'cl-lib)
(require 'cl-generic)

(defmacro edraw-math-debug-log (_format-string &rest _args)
  ;;`(princ (concat (format ,format-string ,@args) "\n"))
  nil)

;;;; Number

(cl-defmethod edraw-to-string ((n number))
  (if (floatp n)
      ;; Float
      (let ((ni (ffloor n)))
        (if (= ni n)
            (format "%d" ni) ;;Remove .0
          (format "%s" n))) ;;@todo Allow setting the number of digits
    ;; Integer?
    (format "%d" n)))

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

(defun edraw-grid-floor (n interval)
  (- n (mod n interval)))

(defun edraw-grid-ceil (n interval)
  (let ((m (mod n interval)))
    (if (= m 0)
        n
      (+ (- n m) interval))))

(defun edraw-grid-round (n interval)
  (let ((m (mod n interval)))
    (if (>= (* m 2) interval)
        (+ (- n m) interval)
      (- n m))))

(defsubst edraw-sign (n)
  (if (< n 0) -1
    (if (> n 0) 1
      n))) ;; 0 or NaN

(defsubst edraw-sign-mul (n x)
  (if (< n 0) (- x)
    (if (> n 0) x
      n))) ;; 0 or NaN



;;;; Vector

(defmacro edraw-xy (x y) `(cons ,x ,y))
(defmacro edraw-x (xy) `(car ,xy))
(defmacro edraw-y (xy) `(cdr ,xy))

(defsubst edraw-xy-zero-p (a)
  (and (= (car a) 0)
       (= (cdr a) 0)))

(defsubst edraw-xy-small-p (a epsilon)
  (and (< (abs (car a)) epsilon)
       (< (abs (cdr a)) epsilon)))

(defsubst edraw-xy-equal-p (a b)
  (equal a b))

(defsubst edraw-xy-assign (a b)
  (setcar a (car b))
  (setcdr a (cdr b)))

(defsubst edraw-xy-clone (cell)
  (cons (car cell) (cdr cell)))

(defsubst edraw-xy-round (cell)
  (cons (round (car cell))
        (round (cdr cell))))

(defsubst edraw-xy-floor (cell)
  (cons (floor (car cell))
        (floor (cdr cell))))

(defsubst edraw-xy-ceil (cell)
  (cons (ceiling (car cell))
        (ceiling (cdr cell))))

(defmacro edraw-xy-add (&rest xys)
  (let* ((vars (mapcar (lambda (_) (gensym "xy")) xys)))
    `(let ,(cl-loop for var in vars
                    for exp in xys
                    unless (eq var exp)
                    collect `(,var ,exp))
       (cons
        (+ ,@(cl-loop for var in vars collect `(car ,var)))
        (+ ,@(cl-loop for var in vars collect `(cdr ,var)))))))

(defmacro edraw-xy-sub (&rest xys)
  (let* ((vars (mapcar (lambda (_) (gensym "xy")) xys)))
    `(let ,(cl-loop for var in vars
                    for exp in xys
                    unless (eq var exp)
                    collect `(,var ,exp))
       (cons
        (- ,@(cl-loop for var in vars collect `(car ,var)))
        (- ,@(cl-loop for var in vars collect `(cdr ,var)))))))

(defsubst edraw-xy-neg (xy)
  (cons (- (car xy)) (- (cdr xy))))

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

(defun edraw-xy-proj (v onto)
  "Return vector projection of V onto ONTO."
  (if (edraw-xy-zero-p onto)
      (edraw-xy 0 0)
    (let ((d (edraw-xy-dot onto v))
          (s (edraw-xy-length-squared onto)))
      (edraw-xy (/ (* (edraw-x onto) d) s)
                (/ (* (edraw-y onto) d) s)))))

(defun edraw-xy-proj-on-line (p line-p0 line-p1)
  "Return point projection of P onto line LINE-P0 to LINE-P1."
  (edraw-xy-add
   line-p0
   (edraw-xy-proj (edraw-xy-sub p line-p0) (edraw-xy-sub line-p1 line-p0))))

(defun edraw-xy-atan (xy)
  (atan (cdr xy) (car xy)))

(defun edraw-xy-angle (a b)
  "Return the angle between the two vectors A and B.

Let A and B both be vectors from the origin, and return the angle
from A to B. If the rotation direction from A to B matches the
rotation direction from the positive X-axis direction to the
positive Y-axis direction, a positive angle is returned. The
angle is returned between -pi and pi."
  (atan (edraw-xy-perpdot a b) (edraw-xy-dot a b)))

(defun edraw-xy-rot90 (xy)
  (cons (- (cdr xy)) (car xy)))

(defun edraw-xy-rot180 (xy)
  (cons (- (car xy)) (- (cdr xy))))

(defun edraw-xy-rot270 (xy)
  (cons (cdr xy) (- (car xy))))

(defun edraw-xy-rotate (xy deg)
  ;; Specify the angle in units of degrees in order to perform
  ;; rotation in units of 90 degrees without error.
  (let ((d (mod deg 360)))
    (cond
     ((= d 0) (edraw-xy-clone xy))
     ((= d 90) (edraw-xy-rot90 xy))
     ((= d 180) (edraw-xy-rot180 xy))
     ((= d 270) (edraw-xy-rot270 xy))
     (t
      (let* ((rad (degrees-to-radians deg))
             (c (cos rad))
             (s (sin rad))
             (x (car xy))
             (y (cdr xy)))
        (edraw-xy
         (- (* x c) (* y s))
         (+ (* x s) (* y c))))))))

(defun edraw-xy-interpolate (a b alpha)
  (let ((ra (- 1.0 alpha)))
    (cons (+ (* ra (car a)) (* alpha (car b)))
          (+ (* ra (cdr a)) (* alpha (cdr b))))))

(defun edraw-xy-interpolate-bezier2 (p0 p1 p2 T)
  (edraw-xy
   (edraw-quadratic-bezier (edraw-x p0) (edraw-x p1) (edraw-x p2) T)
   (edraw-quadratic-bezier (edraw-y p0) (edraw-y p1) (edraw-y p2) T)))

(defun edraw-xy-interpolate-bezier3 (p0 p1 p2 p3 T)
  (edraw-xy
   (edraw-cubic-bezier (edraw-x p0) (edraw-x p1) (edraw-x p2) (edraw-x p3) T)
   (edraw-cubic-bezier (edraw-y p0) (edraw-y p1) (edraw-y p2) (edraw-y p3) T)))

(defun edraw-xy-list-equal-all-p (xy-list)
  (let ((first (car xy-list))
        (rest (cdr xy-list)))
    (while (and rest (edraw-xy-equal-p first (car rest)))
      (setq rest (cdr rest)))
    (null rest)))

(defun edraw-xy-remove-consecutive-same-points (points)
  (cl-loop with prev-xy = nil
           for curr-xy in points
           unless (and prev-xy (edraw-xy-equal-p curr-xy prev-xy))
           collect curr-xy
           do (setq prev-xy curr-xy)))

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

(defun edraw-xy-snap-to-45deg (xy &optional origin-xy)
  (let* ((tan22.5 0.41421356237309503) ;;(tan (degrees-to-radians 22.5))
         (ox (if origin-xy (edraw-x origin-xy) 0))
         (oy (if origin-xy (edraw-y origin-xy) 0))
         (x (- (edraw-x xy) ox))
         (y (- (edraw-y xy) oy)))
    (cond
     ((< (abs y) (* (abs x) tan22.5))
      (edraw-xy (+ ox x) oy))
     ((< (abs x) (* (abs y) tan22.5))
      (edraw-xy ox (+ oy y)))
     (t
      (let ((r (* (+ (abs x) (abs y)) 0.5)))
        (edraw-xy
         (+ ox (edraw-sign-mul x r))
         (+ oy (edraw-sign-mul y r))))))))

(defun edraw-xy-snap-to-square (xy &optional origin-xy)
  (let* ((ox (if origin-xy (edraw-x origin-xy) 0))
         (oy (if origin-xy (edraw-y origin-xy) 0))
         (x (edraw-x xy))
         (y (edraw-y xy))
         (dx (- x ox))
         (dy (- y oy))
         (adx (abs dx))
         (ady (abs dy)))
    (if (< adx ady)
        (edraw-xy (+ ox (if (< dx 0) (- ady) ady)) y)
      (edraw-xy x (+ oy (if (< dy 0) (- adx) adx) )))))

(defun edraw-xy-snap-to-rect-diagonal (pm pc ax ay)
  "Snap coordinates PM to diagonal of rectangle represented by PC, AX, AY.

The rectangle may be transformed.

PM = Mouse coordinates of drag destination
PC = Coordinates of corner of rectangle being dragged
AX = Vector from PC to an adjacent corner
AY = Vector from PC to another adjacent corner

PO +----+--   (PO(Point of Opposite Corner)=PC+AX+AY)
   |\\   | ^
   | \\  | AY
   |  \\ |
   |   \\|
   +----+ PC
   |<AX  \\
          \\
    result * <---* PM (1)
           ^\\
           | \\
    PM (2) *"
  (let* ((vco (edraw-xy-add ax ay))
         (vcm (edraw-xy-sub pm pc)))
    (if (or (edraw-xy-zero-p vco) ;; Empty rectangle
            (edraw-xy-zero-p vcm)) ;; Not moved (PM=PC)
        ;; Free to move
        pm
      (let* (;; Use ax or ay?
             (dir (if (= (edraw-sign (edraw-xy-perpdot vco vcm))
                         (edraw-sign (edraw-xy-perpdot vco ay)))
                      (if (edraw-xy-zero-p ax) ay ax)
                    (if (edraw-xy-zero-p ay) ax ay)))
             ;; Intersection (pm -> dir) and (pc -> vco)
             (t-numer (edraw-xy-perpdot vco vcm))
             (t-denom (edraw-xy-perpdot dir vco)))
        (if (= t-denom 0)
            pm
          (edraw-xy-add
           pm
           (edraw-xy-divn (edraw-xy-nmul t-numer dir) (float t-denom))))))))
;; TEST: (edraw-xy-snap-to-diagonal (edraw-xy 200 100) (edraw-xy 100 100) (edraw-xy -50 0) (edraw-xy 0 -50)) => (100.0 . 100.0)
;; TEST: (edraw-xy-snap-to-diagonal (edraw-xy 300 100) (edraw-xy 200 100) (edraw-xy -50 -50) (edraw-xy 50 -50)) => (200.0 . 0.0)

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
(defmacro edraw-rect-lt-xy (rect) `(edraw-xy (edraw-rect-left ,rect)
                                             (edraw-rect-top ,rect)))
(defmacro edraw-rect-lb-xy (rect) `(edraw-xy (edraw-rect-left ,rect)
                                             (edraw-rect-bottom ,rect)))
(defmacro edraw-rect-rt-xy (rect) `(edraw-xy (edraw-rect-right ,rect)
                                             (edraw-rect-top ,rect)))
(defmacro edraw-rect-rb-xy (rect) `(edraw-xy (edraw-rect-right ,rect)
                                             (edraw-rect-bottom ,rect)))
(defun edraw-rect-corner-points (rect)
  (list (edraw-rect-lt-xy rect)
        (edraw-rect-rt-xy rect)
        (edraw-rect-rb-xy rect)
        (edraw-rect-lb-xy rect)))

(defun edraw-rect-set-ltrb (rect x0 y0 x1 y1)
  (setf (caar rect) x0)
  (setf (cdar rect) y0)
  (setf (cadr rect) x1)
  (setf (cddr rect) y1)
  rect)

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

(defsubst edraw-rect-cx (rect)
  (* 0.5 (+ (caar rect) (cadr rect))))

(defsubst edraw-rect-cy (rect)
  (* 0.5 (+ (cdar rect) (cddr rect))))

(defun edraw-rect-center (rect)
  (cons
   (edraw-rect-cx rect)
   (edraw-rect-cy rect)))


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

(defun edraw-rect-scale (rect sx sy &optional ox oy)
  (unless ox (setq ox 0))
  (unless oy (setq oy 0))
  (edraw-rect-set-ltrb
   rect
   (+ (* (- (edraw-rect-left rect) ox) sx) ox)
   (+ (* (- (edraw-rect-top rect) oy) sy) oy)
   (+ (* (- (edraw-rect-right rect) ox) sx) ox)
   (+ (* (- (edraw-rect-bottom rect) oy) sy) oy)))

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

(defun edraw-matrix-translate-xy (xy)
  (edraw-matrix-translate (edraw-x xy) (edraw-y xy) 0))

(defun edraw-matrix-scale (sx sy sz)
  (edraw-matrix (vector sx 0 0 0  0 sy 0 0  0 0 sz 0  0 0 0 1)))


(defun edraw-matrix-rotate (deg)
  ;; Specify the angle in units of degrees in order to perform
  ;; rotation in units of 90 degrees without error.
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

(defun edraw-matrix-fit-rect-to-rect (src-rect dst-rect)
  (edraw-matrix-mul-mat-mat
   (edraw-matrix-mul-mat-mat
    (edraw-matrix-translate
     (edraw-rect-left dst-rect)
     (edraw-rect-top dst-rect)
     0)
    (edraw-matrix-scale
     (if (= (edraw-rect-width src-rect) 0)
         1 ;;Keep
       (/ (float (edraw-rect-width dst-rect)) (edraw-rect-width src-rect)))
     (if (= (edraw-rect-height src-rect) 0)
         1 ;;Keep
       (/ (float (edraw-rect-height dst-rect)) (edraw-rect-height src-rect)))
     1))
   (edraw-matrix-translate
    (- (edraw-rect-left src-rect))
    (- (edraw-rect-top src-rect))
    0)))

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

(defun edraw-matrix-translation-only-p (mat)
  "Return t if the matrix MAT only represents translations.

Return t for identity matrix or nil."
  (or
   (null mat)
   (edraw-matrix-let-elements mat m
     (ignore m41 m42 m43);;suppress warnings
     (and
      (= 1 m11 m22 m33 m44)
      (= 0 m12 m13 m14 m21 m23 m24 m31 m32 m34)))))

(defun edraw-matrix-scaling-and-translation-only-p (mat)
  "Return t if the matrix MAT only represents scaling and translation.

Return t for identity matrix or nil.
Return nil if rotation components of the matrix is non-zero."
  (or
   (null mat)
   (edraw-matrix-let-elements mat m
     (ignore m11 m22 m33 m44 m41 m42 m43);;suppress warnings
     (= 0 m12 m13 m14 m21 m23 m24 m31 m32 m34))))

(defun edraw-matrix-contains-rotation-p (mat)
  "Return t if the matrix MAT contains rotations (including skew)."
  (not (edraw-matrix-scaling-and-translation-only-p mat)))

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

(defun edraw-quadratic-bezier (x0 x1 x2 T)
  "Return (1-T)^2*x0 + 2*(1-T)*T*x1 + T^2*x2."
  (let* ((u (- 1 T)))
    (+
     (* u u x0)
     (* u T x1 2)
     (* T T x2))))

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
  "Return (1-T)^3*x0 + 3*(1-T)^2*T*x1 + 3(1-T)*T^2*x2 + T^3*x3."
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

;;;; Bezier Curve Fitting

;; Emacs Lisp implementation of
;; Algorithm for Automatically Fitting Digitized Curves
;; by Philip J. Schneider
;; "Graphics Gems", Academic Press, 1990

(defconst edraw-bcf-max-iterations 4)

(defconst edraw-bcf-default-error 2.0)

(defun edraw-fit-bezier-curve (points &optional error)
  (unless error (setq error edraw-bcf-default-error))

  (cond
   ((listp points)
    (setq points
          (apply #'vector (edraw-xy-remove-consecutive-same-points points))))
   ((vectorp points)
    ;;@todo Remove consecutive same points.
    )
   (t
    (error "points must be a list or vector")))

  (edraw-math-debug-log "Enter fit-bezier-curve points=%s" points)
  (let ((begin 0)
        (end (length points)))
    (when (>= (- end begin) 2)
      (edraw-bcf-fit-cubic points begin end
                           (edraw-bcf-left-tangent points begin)
                           (edraw-bcf-right-tangent points (1- end))
                           error))))

(defun edraw-bcf-fit-cubic (points begin end tan-first tan-last error)
  (edraw-math-debug-log "Enter fit-cubic begin=%s end=%s" begin end)
  (let ((size (- end begin)))
    (cond
     ((< size 2)
      (edraw-math-debug-log "Emit nil (size<2)")
      nil)
     ((= size 2)
      (let* ((p0 (aref points begin))
             (p3 (aref points (1- end)))
             (dist (/ (edraw-xy-distance p0 p3) 3.0))
             (bseg (vector p0
                           (edraw-xy-add p0 (edraw-xy-nmul dist tan-first))
                           (edraw-xy-add p3 (edraw-xy-nmul dist tan-last))
                           p3)))
        (edraw-math-debug-log "Emit %s (size=2)" bseg)
        (list bseg)))
     ;; If there are 3 points, it may generate an incorrect bezier segment.
     ;; So always split.
     ;; ((= size 3)
     ;;  (let* ((split-point (+ begin 1))
     ;;         (tan-sp (edraw-bcf-middle-tangent points split-point)))
     ;;    (nconc
     ;;     (edraw-bcf-fit-cubic points begin (1+ split-point)
     ;;                          tan-first tan-sp
     ;;                          error)
     ;;     (edraw-bcf-fit-cubic points split-point end
     ;;                          (edraw-xy-neg tan-sp) tan-last
     ;;                          error))))
     (t
      (let* ((u-params (edraw-bcf-chord-length-parameterize points begin end))
             (bseg (edraw-bcf-generate-bezier points begin end u-params
                                              tan-first tan-last))
             (mesp (edraw-bcf-compute-max-error points begin end bseg u-params))
             (max-error (car mesp))
             (split-point (cdr mesp))
             (iteration-error (max (* 4.0 error) (* error error))))
        (edraw-math-debug-log "max-error=%s split-point=%s" max-error split-point)

        (cond
         ((< max-error error)
          (edraw-math-debug-log "Emit %s (A)" bseg)
          (list bseg))

         ;; Improve u-params
         ((and
           (< max-error iteration-error)
           (cl-loop for i from 0 to (1- edraw-bcf-max-iterations)
                    do (edraw-bcf-reparameterize points begin end
                                                 ;;Overwrite u-params
                                                 u-params bseg)
                    do (setq bseg (edraw-bcf-generate-bezier
                                   points begin end u-params
                                   tan-first tan-last)
                             mesp (edraw-bcf-compute-max-error
                                   points begin end bseg u-params)
                             max-error (car mesp)
                             split-point (cdr mesp))
                    when (< max-error error) return t))
          (edraw-math-debug-log "Emit %s (B)" bseg)
          (list bseg))

         ;; Failed to fit one bezier segment. Split into two segments.
         (t
          (let ((tan-sp (edraw-bcf-middle-tangent points split-point)))
            (nconc
             (edraw-bcf-fit-cubic points begin (1+ split-point)
                                  tan-first tan-sp
                                  error)
             (edraw-bcf-fit-cubic points split-point end
                                  (edraw-xy-neg tan-sp) tan-last
                                  error))))))))))

(defconst edraw-bcf-generate-bezier-length-limit 2.0)

(defun edraw-bcf-generate-bezier (points begin end u-params tan-first tan-last)
  (edraw-math-debug-log
   "Enter generate-bezier begin=%s end=%s tan-first=%s tan-last=%s"
   begin end tan-first tan-last)
  (edraw-math-debug-log "u-params=%s" u-params)

  (let* ((size (- end begin))
         (c00 0.0)
         (c01 0.0)
         (c10 0.0)
         (c11 0.0)
         (x0 0.0)
         (x1 0.0)
         (p0 (aref points begin))
         (p3 (aref points (1- end))))

    (dotimes (i size)
      (let* ((u (aref u-params i))
             (r (- 1.0 u))
             (3ur (* 3.0 u r))
             (b0 (* r r r))
             (b1 (* 3ur r))
             (b2 (* 3ur u))
             (b3 (* u u u))
             (a0 (edraw-xy-nmul b1 tan-first))
             (a1 (edraw-xy-nmul b2 tan-last))
             (tmp (edraw-xy-sub
                   (aref points (+ begin i))
                   (edraw-xy-add
                    (edraw-xy-nmul (+ b0 b1) p0)
                    (edraw-xy-nmul (+ b2 b3) p3)))))
        (cl-incf c00 (edraw-xy-dot a0 a0))
        (cl-incf c01 (edraw-xy-dot a0 a1))
        ;;(cl-incf c10 (edraw-xy-dot a1 a0))
        (cl-incf c11 (edraw-xy-dot a1 a1))
        (cl-incf x0 (edraw-xy-dot a0 tmp))
        (cl-incf x1 (edraw-xy-dot a1 tmp))))
    (setq c10 c01)

    (let* ((det-c0-c1 (- (* c00 c11) (* c10 c01)))
           (det-c0-x (- (* c00 x1) (* c10 x0)))
           (det-x-c1 (- (* x0 c11) (* x1 c01)))
           (alpha-l (if (< (abs det-c0-c1) 1e-6)
                        0.0 (/ (float det-x-c1) det-c0-c1)))
           (alpha-r (if (< (abs det-c0-c1) 1e-6)
                        0.0 (/ (float det-c0-x) det-c0-c1)))
           (seg-length (edraw-xy-distance p0 p3))
           (epsilon (* 1e-6 seg-length)))

      (edraw-math-debug-log
       "detc0c1=%s detc0x=%s detxc1=%s alpha-l=%s alpha-r=%s seg-length=%s"
       det-c0-c1 det-c0-x det-x-c1 alpha-l alpha-r seg-length)

      ;; (if (< (abs det-c0-c1) 1e-6)
      ;;     (let ((c0 (+ c00 c01))
      ;;           (c1 (+ c10 c11)))
      ;;       (if (>= (abs c0) 1e-6)
      ;;           (setq alpha-l (/ x0 c0)
      ;;                 alpha-r alpha-l)
      ;;         (if (>= (abs c1) 1e-6)
      ;;             (setq alpha-l (/ x1 c1)
      ;;                   alpha-r alpha-l)))))

      (if (or (< alpha-l epsilon)
              (< alpha-r epsilon)
              ;; Sometimes the computed control points are very far
              ;; away, so reject it.
              ;; Occurs when the direction of the tangent line is
              ;; difficult to pass through the target point.
              (> (+ alpha-l alpha-r)
                 (* edraw-bcf-generate-bezier-length-limit seg-length)))
          (let ((dist (/ seg-length 3.0)))
            (edraw-math-debug-log "dist=%s" dist)
            (vector
             p0
             (edraw-xy-add p0 (edraw-xy-nmul dist tan-first))
             (edraw-xy-add p3 (edraw-xy-nmul dist tan-last))
             p3))
        (vector
         p0
         (edraw-xy-add p0 (edraw-xy-nmul alpha-l tan-first))
         (edraw-xy-add p3 (edraw-xy-nmul alpha-r tan-last))
         p3)))))

(defun edraw-bcf-reparameterize (points begin end u-params bseg)
  (edraw-math-debug-log "Enter reparameterize")
  (dotimes (i (- end begin))
    (aset u-params i ;;Overwrite
          (edraw-bcf-find-root bseg
                               (aref points (+ begin i))
                               (aref u-params i))))
  u-params)

(defun edraw-bcf-find-root (q p u)
  (edraw-math-debug-log "Enter find-root")
  (let* ((q1-0 (edraw-xy-nmul 3.0 (edraw-xy-sub (aref q 1) (aref q 0))))
         (q1-1 (edraw-xy-nmul 3.0 (edraw-xy-sub (aref q 2) (aref q 1))))
         (q1-2 (edraw-xy-nmul 3.0 (edraw-xy-sub (aref q 3) (aref q 2))))
         (q2-0 (edraw-xy-nmul 2.0 (edraw-xy-sub q1-1 q1-0)))
         (q2-1 (edraw-xy-nmul 2.0 (edraw-xy-sub q1-2 q1-1)))
         (qu (edraw-xy-interpolate-bezier3
              (aref q 0) (aref q 1) (aref q 2) (aref q 3) u))
         (q1u (edraw-xy-interpolate-bezier2 q1-0 q1-1 q1-2 u))
         (q2u (edraw-xy-interpolate q2-0 q2-1 u))
         (numer (edraw-xy-dot (edraw-xy-sub qu p) q1u))
         (denom (+ (edraw-xy-dot q1u q1u)
                   (edraw-xy-dot (edraw-xy-sub qu p) q2u))))
    (if (= denom 0)
        u
      (- u (/ (float numer) denom)))))

(defun edraw-bcf-chord-length-parameterize (points begin end)
  "Return an array of cumulative distances from the beginning point."
  (let* ((size (- end begin))
         ;; assert size >= 2
         (u (make-vector size nil))
         (len 0.0))
    (aset u 0 0.0)
    ;; Accumulate length.
    (cl-loop with last-xy = (aref points begin)
             for i from 1 to (1- size)
             for curr-xy = (aref points (+ begin i))
             do (progn
                  (cl-incf len (edraw-xy-distance last-xy curr-xy))
                  (aset u i len)
                  (setq last-xy curr-xy)))
    ;; Correct values to be in range 0 to 1.
    (cl-loop for i from 1 to (1- size)
             ;; assert length > 0, because the same points were previously removed.
             do (aset u i (/ (aref u i) len)))
    u))

(defun edraw-bcf-compute-max-error (points begin end bseg u-params)
  (let* ((size (- end begin))
         (split-i (/ size 2))
         (max-dist 0.0))
    (cl-loop for i from 1 to (1- size)
             for dist = (edraw-xy-distance-squared
                         (edraw-xy-interpolate-bezier3
                          (aref bseg 0) (aref bseg 1)
                          (aref bseg 2) (aref bseg 3)
                          (aref u-params i))
                         (aref points (+ begin i)))
             when (>= dist max-dist)
             do (setq max-dist dist
                      split-i i))
    (cons max-dist (+ begin split-i))))


(defun edraw-bcf-left-tangent (points index)
  (edraw-xy-normalize
   ;; assert length > 0, because the same points were previously removed.
   (edraw-xy-sub (aref points (1+ index)) (aref points index))))

(defun edraw-bcf-right-tangent (points index)
  (edraw-xy-normalize
   ;; assert length > 0, because the same points were previously removed.
   (edraw-xy-sub (aref points (1- index)) (aref points index))))

(defconst edraw-bcf-use-rot90-on-middle-tangent nil)

(defun edraw-bcf-middle-tangent (points index)
  (let* ((v1 (edraw-xy-sub (aref points (1- index))
                           (aref points index)))
         (v2 (edraw-xy-sub (aref points index)
                           (aref points (1+ index))))
         (v (edraw-xy-midpoint-float v1 v2)))
    (if (edraw-xy-small-p v 1e-6)
        ;;(warn "Normalize zero vector")
        ;; length=0, 180-deg turn and same length!!!
        (if (and edraw-bcf-use-rot90-on-middle-tangent
                 (not (edraw-xy-small-p v1 1e-6)))
            ;; Rotate 90-deg
            (edraw-xy-normalize (edraw-xy-rot90 v1))
          (edraw-xy 0.0 0.0))
      (edraw-xy-normalize v))))


(provide 'edraw-math)
;;; edraw-math.el ends here
