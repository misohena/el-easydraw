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

;;;; Vector

(defmacro edraw-xy (x y) `(cons ,x ,y))
(defmacro edraw-x (xy) `(car ,xy))
(defmacro edraw-y (xy) `(cdr ,xy))

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

(defsubst edraw-rect (x0 y0 x1 y1)
  (cons
   (cons x0 y0)
   (cons x1 y1)))

(defsubst edraw-rect-xywh (x y w h)
  (cons
   (cons x y)
   (cons (+ x w) (+ y h))))

(defun edraw-rect-pp (p0 p1)
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
       (elt 0 init) (elt 1 init) 0 0
       (elt 2 init) (elt 3 init) 0 0
       0 0 1 0
       (elt 4 init) (elt 5 init) 0 1))
     ((= (length init) 16)
      (apply 'vector (mapcar 'identity init)))
     (t (error "Invalid init argument"))))
   ;;@todo support string
   (t (error "Invalid init argument"))))

(defun edraw-matrix-translate (dx dy dz)
  (edraw-matrix (vector 1 0 0 dx  0 1 0 dy  0 0 1 dz  0 0 0 1)))

(defun edraw-matrix-scale (sx sy sz)
  (edraw-matrix (vector sx 0 0 0  0 sy 0 0  0 0 sz 0  0 0 0 1)))

(defun edraw-matrix-mul (a b)
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

(defmacro edraw-matrix-mul-element (lhs rhs col row)
  `(+ (* (aref ,lhs ,(+ row 0)) (aref ,rhs ,(+ (* col 4) 0)))
      (* (aref ,lhs ,(+ row 4)) (aref ,rhs ,(+ (* col 4) 1)))
      (* (aref ,lhs ,(+ row 8)) (aref ,rhs ,(+ (* col 4) 2)))
      (* (aref ,lhs ,(+ row 12)) (aref ,rhs ,(+ (* col 4) 3)))))

(defun edraw-matrix-mul-mat-mat (a b)
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
   (edraw-matrix-mul-element a b 3 3)))

(defun edraw-matrix-mul-mat-vec4 (a b)
  (vector
   (edraw-matrix-mul-element a b 0 0)
   (edraw-matrix-mul-element a b 0 1)
   (edraw-matrix-mul-element a b 0 2)
   (edraw-matrix-mul-element a b 0 3)))

(defun edraw-matrix-mul-mat-vec3 (a b)
  (let* ((p (edraw-matrix-mul-mat-vec4 a (vector (aref b 0) (aref b 1) (aref b 2) 1.0)))
         (w (aref p 3)))
    (vector
     (/ (aref p 0) w)
     (/ (aref p 0) w)
     (/ (aref p 0) w))))

(defun edraw-matrix-mul-mat-vec2 (a b)
  (let ((x (+ (* (aref a 0) (aref b 0)) (* (aref a 4) (aref b 1)) (aref a 12)))
        (y (+ (* (aref a 1) (aref b 0)) (* (aref a 5) (aref b 1)) (aref a 13)))
        (w (+ (* (aref a 3) (aref b 0)) (* (aref a 7) (aref b 1)) (aref a 15))))
    (cons
     (/ x w)
     (/ y w))))

(defun edraw-matrix-mul-mat-xy (a b)
  (let ((x (+ (* (aref a 0) (car b)) (* (aref a 4) (cdr b)) (aref a 12)))
        (y (+ (* (aref a 1) (car b)) (* (aref a 5) (cdr b)) (aref a 13)))
        (w (+ (* (aref a 3) (car b)) (* (aref a 7) (cdr b)) (aref a 15))))
    (cons
     (/ x w)
     (/ y w))))



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

(provide 'edraw-math)
;;; edraw-math.el ends here
