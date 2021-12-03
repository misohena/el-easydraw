;;; edraw-path.el ---                             -*- lexical-binding: t; -*-

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

(require 'edraw-math)



;;;;
;;;; Path Command Manipulation
;;;;

;;;;; edraw-path-cmdlist

;;
;; - A container that holds edraw-path-cmd objects.
;; - Corresponds to the content of d= attribute of <path> element.
;; - Doubly-linked list of command.
;;

(defun edraw-path-cmdlist ()
  "Create a new path command list object."
  (edraw-path-cmd-make-terminator))

;;;;;; cmdlist - Container Operations

(defun edraw-path-cmdlist-empty-p (cmdlist)
  (eq (edraw-path-cmdlist-begin cmdlist)
      (edraw-path-cmdlist-end cmdlist)))

(defun edraw-path-cmdlist-front (cmdlist)
  (unless (edraw-path-cmdlist-empty-p cmdlist)
    (edraw-path-cmdlist-begin cmdlist)))

(defun edraw-path-cmdlist-back (cmdlist)
  (unless (edraw-path-cmdlist-empty-p cmdlist)
    (edraw-path-cmd--prev
     (edraw-path-cmdlist-end cmdlist))))

(defun edraw-path-cmdlist-begin (cmdlist)
  (edraw-path-cmd--next cmdlist))

(defun edraw-path-cmdlist-end (cmdlist)
  cmdlist)

(defun edraw-path-cmdlist-swap (a b)
  (let ((a-first (edraw-path-cmdlist-front a))
        (a-last (edraw-path-cmdlist-back a))
        (b-first (edraw-path-cmdlist-front b))
        (b-last (edraw-path-cmdlist-back b)))
    (when a-first
      (edraw-path-cmd-remove-range a-first a-last)
      (edraw-path-cmd-insert-range-before (edraw-path-cmdlist-end b)
                                          a-first a-last))
    (when b-first
      (edraw-path-cmd-remove-range b-first b-last)
      (edraw-path-cmd-insert-range-before (edraw-path-cmdlist-end a)
                                          b-first b-last))))


(defmacro edraw-path-cmdlist-loop (cmdlist var &rest body)
  (declare (indent 2))
  (let ((cmds (gensym))
        (it (gensym))
        (end (gensym)))
    `(let* ((,cmds ,cmdlist)
            (,it (edraw-path-cmdlist-begin ,cmds))
            (,end (edraw-path-cmdlist-end ,cmds))
            ,var)
       (while (not (eq ,it ,end))
         (setq ,var ,it)
         ,@body
         (setq ,it (edraw-path-cmd--next ,it))))))

(defun edraw-path-cmdlist-clear (cmdlist)
  (edraw-path-cmd-terminate cmdlist))

(defun edraw-path-cmdlist-push-back (cmdlist cmd)
  (edraw-path-cmd-insert-before
   (edraw-path-cmdlist-end cmdlist)
   cmd))

(defun edraw-path-cmdlist-push-back-new (cmdlist type &rest ppoints)
  (let ((cmd (apply 'edraw-path-cmd-from-ppoints type ppoints)))
    (edraw-path-cmdlist-push-back cmdlist cmd)
    cmd))

(defun edraw-path-cmdlist-push-back-overwrite (cmdlist cmd type &rest ppoints)
  (edraw-path-cmd-remove cmd)
  (apply 'edraw-path-cmd-overwrite-from-ppoints cmd type ppoints)
  (edraw-path-cmdlist-push-back cmdlist cmd)
  cmd)

;;;;;; cmdlist - Search

(defun edraw-path-cmdlist-pick-point (cmdlist xy anchor-radius handle-radius)
  ;; Search from back to front. To find the handle added later first.
  (let ((cmd (edraw-path-cmdlist-back cmdlist))
        picked-point)
    (while (and cmd
                (null picked-point))
      (setq picked-point
            (edraw-path-cmd-pick-point cmd xy anchor-radius handle-radius))
      (setq cmd (edraw-path-cmd-prev cmd)))
    picked-point))

;;;;;; cmdlist - Path Operations

(defun edraw-path-cmdlist-translate (cmdlist delta-xy)
  (when (and delta-xy
             (or
              (/= (car delta-xy) 0)
              (/= (cdr delta-xy) 0)))
    (edraw-path-cmdlist-loop cmdlist cmd
      (edraw-path-cmd-translate cmd delta-xy))))

(defun edraw-path-cmdlist-transform (cmdlist matrix)
  (unless (edraw-matrix-identity-p matrix)
    (edraw-path-cmdlist-loop cmdlist cmd
      (edraw-path-cmd-transform cmd matrix))))

(defun edraw-path-cmdlist-closed-p (cmdlist)
  (edraw-path-cmd-is-type-p (edraw-path-cmdlist-back cmdlist) 'Z))

(defun edraw-path-cmdlist-closable-p (cmdlist)
  ;;@todo if there is no M command in cmdlist? (broken path data)
  ;; type is not M or Z
  (edraw-path-cmd-is-type-p (edraw-path-cmdlist-back cmdlist)
                            'C 'L '-forward-handle-point))

(defun edraw-path-cmdlist-open-path (cmdlist)
  (when-let ((last-cmd (edraw-path-cmdlist-back cmdlist)))
    (when (edraw-path-cmd-is-Z last-cmd)
      (when (edraw-path-cmd-closing-segment-p (edraw-path-cmd-prev last-cmd))
        (edraw-path-cmd-remove (edraw-path-cmd-prev last-cmd)))
      (edraw-path-cmd-remove last-cmd)
      t)))

(defun edraw-path-cmdlist-close-path (cmdlist &optional close-straight-p)
  (when-let ((last-cmd (edraw-path-cmdlist-back cmdlist))
             (m-cmd (edraw-path-cmd-prev-M last-cmd))
             (initial-xy (edraw-path-cmd-anchor-point-xy m-cmd 'fast)))
    (when (edraw-path-cmd-is-type-p last-cmd 'C 'L '-forward-handle-point)
      ;; Make sure there is a closing segment.
      (cond
       ;; '-forward-handle-point
       ;; Replace -forward-handle-point with C command that curve to M
       ((and (edraw-path-cmd-is-type-p last-cmd '-forward-handle-point)
             (not close-straight-p))
        (edraw-path-cmd-overwrite-from-ppoints
         last-cmd 'C
         (edraw-path-cmd-arg-pt last-cmd 0)
         (edraw-path-point 'handle last-cmd 1 initial-xy)
         (edraw-path-point 'anchor last-cmd 2 initial-xy)))

       ;; 'C or 'L or ('-forward-handle-point and close-straight-p)
       ;; Add a line segment from last point to initial point
       ((not (edraw-xy-equal-p (edraw-path-cmd-anchor-point-xy last-cmd 'fast)
                               initial-xy))
        (setq last-cmd
              (edraw-path-cmdlist-push-back
               cmdlist (edraw-path-cmd 'L initial-xy)))))

      ;; Add a handle point at the reflection of forward handle point
      ;; on initial anchor point.
      (unless close-straight-p
        (when-let ((prev-xy (edraw-path-cmd-prev-anchor-point-xy last-cmd))
                   (forward-handle (edraw-path-anchor-forward-handle
                                    (edraw-path-cmd-anchor-point m-cmd 'fast))))
          (let* ((vha (edraw-xy-sub ;;handle to anchor
                       initial-xy
                       (edraw-path-point-xy forward-handle)))
                 (vha-len (edraw-xy-length vha))
                 (last-dist (edraw-xy-distance initial-xy prev-xy))
                 (new-handle-len (min (* 0.4 last-dist) vha-len)))
            (when (> new-handle-len 0.1)
              (edraw-path-cmd-L-to-C last-cmd)
              (edraw-path-point-move
               (edraw-path-cmd-arg-pt last-cmd 1)
               (edraw-xy-add
                initial-xy
                (edraw-xy-nmul (/ new-handle-len vha-len) vha)))))))

      ;; Add Z
      (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'Z))

      t)))

(defun edraw-path-cmdlist-reverse (cmdlist)
  ;; Check begin with M.
  (unless (or (edraw-path-cmdlist-empty-p cmdlist)
              (edraw-path-cmd-is-M (edraw-path-cmdlist-front cmdlist)))
    ;; https://www.w3.org/TR/SVG11/paths.html#PathDataMovetoCommands
    (error "A path data segment must begin with a M"))

  (let ((tmp-cmdlist (edraw-path-cmdlist))
        in-closed-subpath-p
        prev-type
        prev-forward-handle
        curr-backward-handle
        (curr (edraw-path-cmdlist-back cmdlist))
        (prev-m (edraw-path-cmd-prev-M (edraw-path-cmdlist-end cmdlist))))
    (while curr
      ;; Update prev-m
      (when (eq curr prev-m)
        (setq prev-m (edraw-path-cmd-prev-M curr)))

      (let ((next (edraw-path-cmd-prev curr)))
        (edraw-path-cmd-remove curr)

        (if (edraw-path-cmd-is-fhp curr)
            ;; Ignore fhp (Keep prev)
            nil

          (let* ((curr-type (edraw-path-cmd-type curr))
                 (curr-anchor (pcase curr-type
                                ('C (edraw-path-cmd-arg-pt curr 2))
                                ((or 'L 'M) (edraw-path-cmd-arg-pt curr 0))))
                 (next-backward-handle (if (eq curr-type 'C)
                                           (edraw-path-cmd-arg-pt curr 0)))
                 (curr-forward-handle (if (eq curr-type 'C)
                                          (edraw-path-cmd-arg-pt curr 1))))
            ;;(message "curr-type=%s" curr-type)

            (pcase curr-type
              ;; curr <- prev
              ;; Z <- nil (not in-closed-subpath-p) => M(in-closed-subpath-p=t)
              ;; Z <- M   (not in-closed-subpath-p) => M(in-closed-subpath-p=t)
              ;; Z <- Z   (in-closed-subpath-p)     => Z
              ;; Z <- LorC in-closed-subpath-p      => Z LorC
              ;; Z <- LorC not in-closed-subpath-p  => M(in-closed-subpath-p=t) LorC
              ('Z
               ;; assert (not (null prev-m)) ;;because cmdlist always starts with M
               (let ((prev-m-xy (edraw-path-cmd-arg-xy prev-m 0)))
                 ;; Make segment to Z
                 (pcase prev-type
                   ('L
                    (edraw-path-cmdlist-push-back-new
                     tmp-cmdlist 'L (edraw-path-point 'anchor nil 0 prev-m-xy)))
                   ('C
                    (edraw-path-cmdlist-push-back-new
                     tmp-cmdlist 'C prev-forward-handle curr-backward-handle (edraw-path-point 'anchor nil 2 prev-m-xy))))
                 ;; Make M or Z
                 (if in-closed-subpath-p
                     (edraw-path-cmdlist-push-back-new tmp-cmdlist 'Z)
                   (edraw-path-cmdlist-push-back-new tmp-cmdlist 'M (edraw-path-cmd-arg-pt prev-m 0)) ;; move M's anchor point to here
                   (setq in-closed-subpath-p t))))

              ;; M <- nil (not in-closed-subpath-p) => M
              ;; M <- M   (not in-closed-subpath-p) => M
              ;; M <- Z   (in-closed-subpath-p)     => Z (in-closed-subpath-p=nil)
              ;; M <= LorC in-closed-subpath-p      => Z LorC (in-closed-subpath-p=nil)
              ;; M <= LorC not in-closed-subpath-p  => LorC
              ('M
               (pcase prev-type
                 ;; Make segment to M
                 ('L
                  (edraw-path-cmdlist-push-back-overwrite
                   tmp-cmdlist curr 'L
                   (if in-closed-subpath-p
                       ;; don't use moved M's anchor point
                       ;; create a point of closing segment
                       (edraw-path-point 'anchor curr 0 (edraw-path-point-xy curr-anchor))
                     curr-anchor)))
                 ('C
                  (edraw-path-cmdlist-push-back-overwrite
                   tmp-cmdlist curr 'C
                   prev-forward-handle curr-backward-handle
                   (if in-closed-subpath-p
                       ;; don't use moved M's anchor point
                       ;; create a point of closing segment
                       (edraw-path-point 'anchor curr 0 (edraw-path-point-xy curr-anchor))
                     curr-anchor)))
                 ;; Keep single M
                 ((or 'M 'nil)
                  (edraw-path-cmdlist-push-back tmp-cmdlist curr)))
               ;; Close path
               (when in-closed-subpath-p
                 (edraw-path-cmdlist-push-back-new tmp-cmdlist 'Z)
                 (setq in-closed-subpath-p nil)))

              ;; LorC <- nil => M
              ;; LorC <- M => M
              ;; LorC(=closing) <- Z => do nothing
              ;; LorC(/=closing) <- Z => L
              ;; LorC <- LorC => LorC
              ((or 'L 'C)
               (pcase prev-type
                 ((or 'M 'nil)
                  (edraw-path-cmdlist-push-back-overwrite tmp-cmdlist curr 'M
                                                          curr-anchor))
                 ('L
                  (edraw-path-cmdlist-push-back-overwrite tmp-cmdlist curr 'L
                                                          curr-anchor))
                 ('C
                  (edraw-path-cmdlist-push-back-overwrite tmp-cmdlist curr 'C
                                                          prev-forward-handle
                                                          curr-backward-handle
                                                          curr-anchor))
                 ('Z (unless (edraw-xy-equal-p
                              (edraw-path-cmd-arg-xy prev-m 0)
                              (edraw-path-point-xy curr-anchor))
                       ;; not closing segmeht
                       (edraw-path-cmdlist-push-back-new tmp-cmdlist 'L
                                                         curr-anchor))))))

            (setq prev-type curr-type
                  prev-forward-handle curr-forward-handle
                  curr-backward-handle next-backward-handle)))
        (setq curr next)))

    ;; assert (not in-closed-subpath-p) ;;because cmdlist always starts with M
    ;; assert (edraw-path-cmdlist-empty-p cmdlist))

    ;; Move contents of tmp-cmdlist to cmdlist
    (edraw-path-cmd-insert-range-before
     cmdlist
     (edraw-path-cmdlist-begin tmp-cmdlist)
     (edraw-path-cmdlist-back tmp-cmdlist))
    cmdlist))

;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-reverse (edraw-path-cmdlist-from-d ""))) => ""
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-reverse (edraw-path-cmdlist-from-d "M 1 2"))) => "M1,2"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-reverse (edraw-path-cmdlist-from-d "M 1 2 L 3 4"))) => "M3,4L1,2"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-reverse (edraw-path-cmdlist-from-d "M 1 2 L 3 4 C 5 6 7 8 9 10"))) => "M9,10C7,8 5,6 3,4L1,2"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-reverse (edraw-path-cmdlist-from-d "M 1 2 L 3 4 C 5 6 7 8 9 10 Z L 11 12 L13 14 Z"))) => "M1,2L13,14L11,12L1,2ZL9,10C7,8 5,6 3,4L1,2Z"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-reverse (edraw-path-cmdlist-from-d "M 1 2 L 3 4 M 5 6 C 7 8 9 10 11 12 Z L 11 12 L13 14 Z"))) => "M5,6L13,14L11,12L5,6ZL11,12C9,10 7,8 5,6ZM3,4L1,2"

(defun edraw-path-cmdlist-insert-cmdlist-front (dst-cmdlist src-cmdlist)
  (when (eq dst-cmdlist src-cmdlist)
    (error "Same objects"))

  (unless (edraw-path-cmdlist-empty-p src-cmdlist)
    (let ((first (edraw-path-cmdlist-front src-cmdlist))
          (last (edraw-path-cmdlist-back src-cmdlist)))
      (edraw-path-cmd-remove-range first last)
      (edraw-path-cmd-insert-range-after (edraw-path-cmdlist-end dst-cmdlist) first last)))

  dst-cmdlist)
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-insert-cmdlist-front (edraw-path-cmdlist-from-d "L10 11L12 13") (edraw-path-cmdlist-from-d "M1 2L3 4"))) => "M1,2L3,4L10,11L12,13"

(defun edraw-path-cmdlist-connect-cmdlist-front (dst-cmdlist src-cmdlist)
  (when (eq dst-cmdlist src-cmdlist)
    (error "Same objects"))

  (when (and (not (edraw-path-cmdlist-empty-p src-cmdlist))
             (not (edraw-path-cmdlist-empty-p dst-cmdlist)))
    ;; last of src              first of dst
    ;; ..                    -> not M        : Discard dst cmdlist(Invalid path)
    ;; ..Z                   -> M            : Keep both
    ;; ..M   (Same Point)    -> M(Closed)    : Keep both
    ;; ..M   (Same Point)    -> M(Not Closed): Replace dst M with L
    ;; ..LorC(Same Point)    -> M(Closed)    : Keep both
    ;; ..LorC(Same Point)    -> M(Not Closed): Replace dst M with L
    ;; ..M   (Not Same Point)-> M(Closed)    : Keep both
    ;; ..M   (Not Same Point)-> M(Not Closed): Replace dst M with L
    ;; ..LorC(Not Same Point)-> M(Closed)    : Keep both
    ;; ..LorC(Not Same Point)-> M(Not Closed): Replace dst M with L
    ;; ..fhp                 -> M(Closed)    : Discard src fhp
    ;; ..fhp                 -> M(Not Closed): Replace dst M with C, discard fhp
    (let* ((src-last (edraw-path-cmdlist-back src-cmdlist))
           (dst-first (edraw-path-cmdlist-front dst-cmdlist))
           (dst-first-xy (edraw-path-cmd-anchor-point-xy dst-first 'fast))
           (dst-closed-p (not (null (edraw-path-cmd-Zs-from-M dst-first)))))
      (if (not (edraw-path-cmd-is-M dst-first))
          ;; Discard invalid path data
          (edraw-path-cmdlist-clear dst-cmdlist)
        ;; Connect src-last to dst-first
        (when (not dst-closed-p)
          (pcase (edraw-path-cmd-type src-last)
            ((or 'M 'L 'C)
             ;; Replace dst M with L
             (edraw-path-cmd-overwrite-from-ppoints
              dst-first 'L (edraw-path-cmd-arg-pt dst-first 0)))
            ('-forward-handle-point
             ;; Replace dst M with C
             (edraw-path-cmd-overwrite-from-ppoints
              dst-first 'C
              (edraw-path-cmd-arg-pt src-last 0)
              (edraw-path-point 'handle dst-first 1 dst-first-xy)
              (edraw-path-cmd-arg-pt dst-first 0)))))
        ;; Discard src fhp
        (when (edraw-path-cmd-is-fhp src-last)
          (edraw-path-cmd-remove src-last)))))

  (edraw-path-cmdlist-insert-cmdlist-front dst-cmdlist src-cmdlist))
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-connect-cmdlist-front (edraw-path-cmdlist-from-d "M10 11L12 13") (edraw-path-cmdlist-from-d "M1 2L3 4"))) => "M1,2L3,4L10,11L12,13"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-connect-cmdlist-front (edraw-path-cmdlist-from-d "M10 11L12 13") (edraw-path-cmdlist-from-d "M1 2L3 4L10 11"))) => "M1,2L3,4L10,11L10,11L12,13"

(defun edraw-path-cmdlist-split-subpaths (cmdlist)
  "Separate all subpaths contained in CMDLIST into new cmdlists.

Returns list of edraw-path-cmdlist object.

Move existing cmd and point objects to the new cmdlists.  In
addition, M commands may be added as needed (e.g. M1,2L3,4ZL5,6).
The CMDLIST will be empty after calling this function. "
  (let ((cmd (edraw-path-cmdlist-front cmdlist))
        prev-cmd
        prev-m
        subpath-begin
        cmdlist-list)
    (cl-labels ((emit-subpath
                 (last)
                 (when subpath-begin
                   (let ((new-cmdlist (edraw-path-cmdlist)))
                     (edraw-path-cmd-remove-range subpath-begin last)
                     (edraw-path-cmd-insert-range-before
                      (edraw-path-cmdlist-end new-cmdlist) subpath-begin last)
                     (push new-cmdlist cmdlist-list)))))
      (while cmd
        (cond
         ((edraw-path-cmd-is-M cmd)
          (emit-subpath prev-cmd)
          (setq prev-m cmd)
          (setq subpath-begin cmd))

         ((edraw-path-cmd-is-Z cmd)
          (unless (or (null (edraw-path-cmd-next cmd))
                      (edraw-path-cmd-is-M (edraw-path-cmd-next cmd)))
            (let ((next-cmd
                   (edraw-path-cmd-insert-after
                    cmd
                    (edraw-path-cmd 'M (edraw-path-cmd-arg-xy prev-m 0)))))
              (emit-subpath cmd)
              (setq subpath-begin next-cmd)
              (setq cmd next-cmd)))))

        (setq prev-cmd cmd)
        (setq cmd (edraw-path-cmd-next cmd)))

      (emit-subpath prev-cmd)

      (nreverse cmdlist-list))))
;; TEST: (mapcar 'edraw-path-cmdlist-to-string (edraw-path-cmdlist-split-subpaths (edraw-path-cmdlist-from-d ""))) => nil
;; TEST: (mapcar 'edraw-path-cmdlist-to-string (edraw-path-cmdlist-split-subpaths (edraw-path-cmdlist-from-d "M1,2"))) => ("M1,2")
;; TEST: (mapcar 'edraw-path-cmdlist-to-string (edraw-path-cmdlist-split-subpaths (edraw-path-cmdlist-from-d "M1,2L3,4ZL5,6L7,8ZL9,10L11,12"))) => ("M1,2L3,4Z" "M1,2L5,6L7,8Z" "M1,2L9,10L11,12")


(defun edraw-path-cmdlist-aabb (cmdlist)
  "Return a axis aligned bounding box of CMDLIST."
  (let ((initial-point nil) ;;last M point
        (current-point nil)
        x-min-max
        y-min-max)
    (edraw-path-cmdlist-loop cmdlist cmd
      (pcase (edraw-path-cmd-type cmd)
        ('M
         (setq current-point
               (setq initial-point (edraw-path-cmd-arg-xy cmd 0))))
        ('L
         (when current-point
           (let ((p0 current-point)
                 (p1 (edraw-path-cmd-arg-xy cmd 0)))
             (setq x-min-max (edraw-min-max-update x-min-max (car p0)))
             (setq x-min-max (edraw-min-max-update x-min-max (car p1)))
             (setq y-min-max (edraw-min-max-update y-min-max (cdr p0)))
             (setq y-min-max (edraw-min-max-update y-min-max (cdr p1)))
             (setq current-point p1))))
        ('C
         (when current-point
           (let ((p0 current-point)
                 (p1 (edraw-path-cmd-arg-xy cmd 0))
                 (p2 (edraw-path-cmd-arg-xy cmd 1))
                 (p3 (edraw-path-cmd-arg-xy cmd 2)))
             (setq x-min-max (edraw-cubic-bezier-min-max-update x-min-max (car p0) (car p1) (car p2) (car p3)))
             (setq y-min-max (edraw-cubic-bezier-min-max-update y-min-max (cdr p0) (cdr p1) (cdr p2) (cdr p3)))
             (setq current-point p3))))
        ('Z
         (setq current-point initial-point))))
    (edraw-rect
     (car x-min-max)
     (car y-min-max)
     (cdr x-min-max)
     (cdr y-min-max))))

;;TEST: (edraw-path-cmdlist-aabb (edraw-path-cmdlist-from-d "M10,20 L30,40 Z C20,0 80,0 100,20")) => ((10 . 5.0) 100 . 40)



;;;;;; cmdlist - Point

(defun edraw-path-cmdlist-nth-point (cmdlist index)
  (let ((cmd (edraw-path-cmdlist-front cmdlist))
        args-size)
    (while (and cmd
                (>= index (setq args-size (edraw-path-cmd-args-size cmd))))
      (setq index (- index args-size))
      (setq cmd (edraw-path-cmd-next cmd)))
    (when cmd
      (edraw-path-cmd-arg-pt cmd index))))
;; TEST: (edraw-path-point-xy (edraw-path-cmdlist-nth-point (edraw-path-cmdlist-from-d "M1,2L3,4ZC5,6 7,8 9,10") 4)) => (9 . 10)

;;;;;; cmdlist - Anchor Point

(defun edraw-path-cmdlist-anchor-points (cmdlist)
  (let (points)
    (edraw-path-cmdlist-loop cmdlist cmd
      (when-let ((anchor (edraw-path-cmd-anchor-point cmd nil)))
        (push anchor points)))
    (nreverse points)))

(defun edraw-path-cmdlist-add-anchor-point (cmdlist xy)
  (let* ((last-cmd (edraw-path-cmdlist-back cmdlist))
         (new-cmd
          (cond
           ;; Beginning of path
           ((null last-cmd)
            (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'M xy)))

           ;; Replace -forward-handle-point with C
           ((eq (edraw-path-cmd-type last-cmd) '-forward-handle-point)
            (edraw-path-cmd-overwrite-from-ppoints
             last-cmd 'C
             (edraw-path-cmd-arg-pt last-cmd 0)
             (edraw-path-point 'handle last-cmd 1 xy) ;;new handle
             (edraw-path-point 'anchor last-cmd 2 xy)) ;;new anchor
            last-cmd)

           ;; Add L
           (t (edraw-path-cmdlist-push-back cmdlist (edraw-path-cmd 'L xy))))))
    ;; Return anchor point
    (edraw-path-cmd-anchor-point new-cmd 'fast)))

(defun edraw-path-cmdlist-first-anchor-point (cmdlist)
  (when-let ((first-cmd (edraw-path-cmdlist-front cmdlist)))
    ;;@todo if the first cmd is not an M command ? (broken path data)
    (edraw-path-cmd-anchor-point first-cmd 'fast)))

(defun edraw-path-cmdlist-last-anchor-point (cmdlist)
  (unless (edraw-path-cmdlist-empty-p cmdlist)
    (edraw-path-cmd-prev-anchor-point
     (edraw-path-cmdlist-end cmdlist))))

;;;;;; cmdlist - String Conversion

(defun edraw-path-cmdlist-to-string (cmdlist)
  "Return string as d= attribute format."
  (let ((str ""))
    (edraw-path-cmdlist-loop cmdlist cmd
      (setq str (concat str (edraw-path-cmd-to-string cmd))))
    str))

(defun edraw-path-cmdlist-from-d (d)
  "Convert path data attribute(<path d=D>) to edraw-path-cmdlist object."
  ;; ref: https://www.w3.org/TR/SVG11/paths.html#PathData
  (let ((cmdlist (edraw-path-cmdlist))
        initial-xy
        (current-xy (cons 0 0)))
    (cl-flet* (;; Take NUM arguments from CMD-ARGS.
               (get-args
                (num cmd-args)
                (let ((args-head (cdr cmd-args))
                      (args-last-cell (nthcdr num cmd-args)))
                  (when (null args-last-cell)
                    (error "Too short path command argument %s" cmd-args))
                  (setcdr cmd-args (cdr args-last-cell))
                  (setcdr args-last-cell nil)
                  args-head))
               ;; Take 2 (or 1) arguments from CMD-ARGS and return a
               ;; coordinate-pair.
               (get-xy
                (cmd-args &optional point-type relative-p x-or-y)
                (let* ((nums (if x-or-y
                                 (get-args 1 cmd-args)
                               (get-args 2 cmd-args)))
                       (xy (pcase x-or-y
                             ('x (cons (car nums)
                                       (if relative-p 0 (cdr current-xy))))
                             ('y (cons (if relative-p 0 (car current-xy))
                                       (car nums)))
                             (_ (cons (car nums) (cadr nums))))))
                  ;; Convert xy to relative coordinates.
                  (when relative-p
                    (setq xy (edraw-xy-add current-xy xy)))
                  ;; Record the initial or current point.
                  (pcase point-type
                    ('initial (setq initial-xy xy)
                              (setq current-xy xy))
                    ('current (setq current-xy xy)))
                  xy))
               ;; Push CMD to the end of CMDLIST
               (push-cmd
                (cmd)
                (edraw-path-cmdlist-push-back cmdlist cmd)))
      (dolist (cmd-args (edraw-path-d-parse d))
        (let* ((cmd-type (car cmd-args))
               (cmd-type-char (elt (symbol-name cmd-type) 0))
               (rel-p (<= ?a cmd-type-char ?z))) ;;lowercase-p
          (pcase cmd-type
            ((or 'M 'm)
             (push-cmd (edraw-path-cmd 'M (get-xy cmd-args 'initial rel-p)))
             (while (cdr cmd-args) ;; lineto-argument-sequence
               (push-cmd (edraw-path-cmd 'L (get-xy cmd-args 'current rel-p)))))
            ((or 'Z 'z)
             (push-cmd (edraw-path-cmd 'Z))
             (setq current-xy initial-xy))
            ((or 'L 'l)
             (while (cdr cmd-args) ;; lineto-argument-sequence
               (push-cmd (edraw-path-cmd 'L (get-xy cmd-args 'current rel-p)))))
            ((or 'H 'h)
             (while (cdr cmd-args) ;; horizontal-lineto-argument-sequence
               (push-cmd (edraw-path-cmd 'L (get-xy cmd-args 'current rel-p
                                                    'x)))))
            ((or 'V 'v)
             (while (cdr cmd-args) ;; vertical-lineto-argument-sequence
               (push-cmd (edraw-path-cmd 'L (get-xy cmd-args 'current rel-p
                                                    'y)))))
            ((or 'C 'c)
             (while (cdr cmd-args) ;; curveto-argument-sequence
               (let ((p1 (get-xy cmd-args nil rel-p))
                     (p2 (get-xy cmd-args nil rel-p))
                     (p (get-xy cmd-args 'current rel-p)))
                 (push-cmd (edraw-path-cmd 'C p1 p2 p)))))
            ((or 'S 's)
             (while (cdr cmd-args) ;; smooth-curveto-argument-sequence
               (let ((p1
                      ;; the reflection of the second control point on
                      ;; the previous command relative to the current point.
                      (let ((prev-cmd (edraw-path-cmdlist-back cmdlist)))
                        (if (and prev-cmd
                                 (eq (edraw-path-cmd-type prev-cmd) 'C))
                            (edraw-xy-sub
                             (edraw-xy-nmul 2 current-xy)
                             (edraw-path-cmd-arg-xy prev-cmd 1))
                          (edraw-xy-clone current-xy))))
                     (p2 (get-xy cmd-args nil rel-p))
                     (p (get-xy cmd-args 'current rel-p)))
                 (push-cmd (edraw-path-cmd 'C p1 p2 p)))))
            ((or 'Q 'q)
             (while (cdr cmd-args) ;; quadratic-bezier-curveto-argument-sequence
               (let* ((p0 current-xy)
                      (qp1 (get-xy cmd-args nil rel-p))
                      (p (get-xy cmd-args 'current rel-p))
                      (cp (edraw-quadratic-bezier-curve-to-cubic p0 qp1 p)))
                 (push-cmd (edraw-path-cmd 'C (car cp) (cdr cp) p)))))
            ((or 'T 't)
             (while (cdr cmd-args) ;; smooth-quadratic-bezier-curveto-argument-sequence
               (let* ((p0 current-xy)
                      (qp1
                       ;; the reflection of the control point on the
                       ;; previous command relative to the current point.
                       (let ((prev-cmd (edraw-path-cmdlist-back cmdlist)))
                         (if (and prev-cmd
                                  (eq (edraw-path-cmd-type prev-cmd) 'Q))
                             (edraw-xy-sub
                              (edraw-xy-nmul 2 current-xy)
                              (edraw-path-cmd-arg-xy prev-cmd 0))
                           (edraw-xy-clone current-xy))))
                      (p (get-xy cmd-args 'current rel-p))
                      (cp (edraw-quadratic-bezier-curve-to-cubic p0 qp1 p)))
                 (push-cmd (edraw-path-cmd 'C (car cp) (cdr cp) p)))))
            ;;@todo support A command
            ;; ((or 'A 'a) )
            (_ (error "Unsupported path command found: %s in %s" cmd-args d))
            ))))
    cmdlist))
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-from-d "M 10 10.1 L 20.2 20e1 .3 .3e-1 Z")) => "M10,10.1L20.2,200.0L0.3,0.03Z"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-from-d "m 10 10.1 l 10.2 189.9 -19.9 -199.97 z")) => "M10,10.1L20.2,200.0L0.3000000000000007,0.030000000000001137Z"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-from-d "M 10 20 h 30 40 -50 v 60 -70")) => "M10,20L40,20L80,20L30,20L30,80L30,10"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-from-d "M 10 20 H 30 40 -50 V 60 -70")) => "M10,20L30,20L40,20L-50,20L-50,60L-50,-70"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-from-d "M 10 20 C 30 40 50 60 70 85 S 150 160 170 180")) => "M10,20C30,40 50,60 70,85C90,110 150,160 170,180"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-from-d "M 65 50 Q 130 85 65 120")) => "M65,50C108.33333333333334,73.33333333333333 108.33333333333334,96.66666666666667 65,120"
;; TEST: (edraw-path-cmdlist-to-string (edraw-path-cmdlist-from-d "M 100 100 c 50,50 100,50 150,0")) => "M100,100C150,150 200,150 250,100"



;;;;; edraw-path-cmd

;;
;; - Corresponds to each command in the d= attribute of the <path> element.
;; - The command types and argument formats are as follows:
;;   - type=M args=((anchor-x . anchor-y))
;;   - type=L args=((anchor-x . anchor-y))
;;   - type=C args=((handle-x1 . handle-y1) (handle-x2 . handle-y2) (anchor-x . anchor-y))
;;   - type=Z args=nil
;;   - type=-forward-handle-point args=((handle-x . handle-y))
;; - Other commands are converted to L or C by edraw-path-cmdlist-from-d.
;; - L and C (or sometimes Z) represent a segment.
;; - This is a doubly-linked list, so you can retrieve the previous and next
;;   commands.
;;

(gv-define-simple-setter edraw-path-cmd--prev edraw-path-cmd--prev-set)
(gv-define-simple-setter edraw-path-cmd--next edraw-path-cmd--next-set)
(gv-define-simple-setter edraw-path-cmd-type edraw-path-cmd-type-set)
(gv-define-simple-setter edraw-path-cmd--args edraw-path-cmd--args-set)

;;;;;; cmd - Constructor

(defun edraw-path-cmd (type &rest args)
  (let ((cmd (vector nil nil type nil)))
    (setf (edraw-path-cmd--args cmd)
          (pcase type
            ('M
             (list (edraw-path-point 'anchor cmd 0 (elt args 0))))
            ('L
             (list (edraw-path-point 'anchor cmd 0 (elt args 0))))
            ('C
             (list (edraw-path-point 'handle cmd 0 (elt args 0))
                   (edraw-path-point 'handle cmd 1 (elt args 1))
                   (edraw-path-point 'anchor cmd 2 (elt args 2))))
            ('-forward-handle-point
             (list (edraw-path-point 'handle cmd 0 (elt args 0))))))
    cmd))

(defun edraw-path-cmd-from-ppoints (type &rest ppoints)
  (let ((cmd (vector nil nil type nil)))
    (edraw-path-cmd--accept-ppoints cmd ppoints)
    cmd))

(defun edraw-path-cmd--accept-ppoints (cmd ppoints)
  (setf (edraw-path-cmd--args cmd) ppoints)
  (cl-loop for arg-index from 0
           for ppoint in ppoints
           do (edraw-path-point-transfer ppoint cmd arg-index)))

(defun edraw-path-cmd-make-terminator ()
  (let ((terminator (edraw-path-cmd nil)))
    (edraw-path-cmd-terminate terminator)
    terminator))

;;;;;; cmd - Accessors

(defun edraw-path-cmd--prev (cmd) (aref cmd 0))
(defun edraw-path-cmd--next (cmd) (aref cmd 1))
(defun edraw-path-cmd-type (cmd) (aref cmd 2))
(defun edraw-path-cmd--args (cmd) (aref cmd 3))
(defun edraw-path-cmd--prev-set (cmd prev) (aset cmd 0 prev))
(defun edraw-path-cmd--next-set (cmd next) (aset cmd 1 next))
(defun edraw-path-cmd-type-set (cmd type) (aset cmd 2 type))
(defun edraw-path-cmd--args-set (cmd args) (aset cmd 3 args))

(defun edraw-path-cmd-args-size (cmd)
  (pcase (edraw-path-cmd-type cmd)
    ('Z 0)
    ('M 1)
    ('L 1)
    ('C 3)
    ('-forward-handle-point 1)
    (_ 0)))

(defun edraw-path-cmd-arg-pt (cmd n)
  (nth n (edraw-path-cmd--args cmd)))

(defun edraw-path-cmd-arg-xy (cmd n)
  (edraw-path-point-xy (nth n (edraw-path-cmd--args cmd))))

(defun edraw-path-cmd-is-type-p (cmd &rest types)
  (and cmd
       (memq (edraw-path-cmd-type cmd) types)))
(defun edraw-path-cmd-is-M (cmd) (and cmd (eq (edraw-path-cmd-type cmd) 'M)))
(defun edraw-path-cmd-is-L (cmd) (and cmd (eq (edraw-path-cmd-type cmd) 'L)))
(defun edraw-path-cmd-is-C (cmd) (and cmd (eq (edraw-path-cmd-type cmd) 'C)))
(defun edraw-path-cmd-is-Z (cmd) (and cmd (eq (edraw-path-cmd-type cmd) 'Z)))
(defun edraw-path-cmd-is-fhp (cmd) (and cmd (eq (edraw-path-cmd-type cmd)
                                                '-forward-handle-point)))
(defun edraw-path-cmd-is-L-or-C (cmd)
  (and cmd
       (let ((type (edraw-path-cmd-type cmd)))
         (or (eq type 'L) (eq type 'C)))))

;;;;;; cmd - Command Mutation

(defun edraw-path-cmd-overwrite-from-ppoints (cmd type &rest ppoints)
  (setf (edraw-path-cmd-type cmd) type)
  (edraw-path-cmd--accept-ppoints cmd ppoints))

(defun edraw-path-cmd-L-to-C (cmd)
  "Mutate from L command to C command.

Change a straight segment to a curved segment.

The anchor point object in the C command is still the same as the one in the L command, and two new handle point objects will be created.
"
  (pcase (edraw-path-cmd-type cmd)
    ('C
     cmd)
    ('L
     (let ((prev-anchor-xy (edraw-path-cmd-prev-anchor-point-xy cmd))
           (curr-anchor (edraw-path-cmd-anchor-point cmd t)))
       (unless prev-anchor-xy
         (error "No previous anchor"))
       (edraw-path-cmd-overwrite-from-ppoints
        cmd 'C
        (edraw-path-point 'handle cmd 0 prev-anchor-xy)
        (edraw-path-point 'handle cmd 1 (edraw-path-point-xy curr-anchor))
        curr-anchor)
       cmd))
    (type
     (error "cmd type is not L (%s)" type))))

;;;;;; cmd - String Conversion

(defun edraw-path-cmd-to-string (cmd)
  (let* ((type (edraw-path-cmd-type cmd))
         (args (edraw-path-cmd--args cmd))
         (type-name (symbol-name type)))
    (cond
     ;; Discard -forward-handle-point, etc...
     ((= (elt type-name 0) ?-)
      "")
     ;; [M|Z|L|C] (x . y)...
     ;; @todo Convert to H, V, S if possible
     (t
      (concat
       type-name
       (if args
           (concat
            ;;" "
            (mapconcat (lambda (arg)
                         (let ((xy (edraw-path-point-xy arg)))
                           (format "%s,%s" (car xy) (cdr xy))))
                       args " "))))))))

;;;;;; cmd - Relationship Between Commands

(defun edraw-path-cmd-prev (cmd)
  "Return the previous command of CMD. If CMD is the first command, return nil."
  (when-let ((prev (edraw-path-cmd--prev cmd)))
    (unless (edraw-path-cmd-terminator-p prev)
      prev)))
(defun edraw-path-cmd-next (cmd)
  "Return the next command of CMD. If CMD is the last command, return nil."
  (when-let ((next (edraw-path-cmd--next cmd)))
    (unless (edraw-path-cmd-terminator-p next)
      next)))

(defun edraw-path-cmd-insert-before (this-cmd new-cmd)
  (when (and this-cmd new-cmd)
    (when-let ((prev-cmd (edraw-path-cmd--prev this-cmd)))
      (setf (edraw-path-cmd--next prev-cmd) new-cmd)
      (setf (edraw-path-cmd--prev new-cmd) prev-cmd))
    (setf (edraw-path-cmd--next new-cmd) this-cmd)
    (setf (edraw-path-cmd--prev this-cmd) new-cmd))
  new-cmd)

(defun edraw-path-cmd-insert-after (this-cmd new-cmd)
  (when (and this-cmd new-cmd)
    (when-let ((next-cmd (edraw-path-cmd--next this-cmd)))
      (setf (edraw-path-cmd--prev next-cmd) new-cmd)
      (setf (edraw-path-cmd--next new-cmd) next-cmd))
    (setf (edraw-path-cmd--prev new-cmd) this-cmd)
    (setf (edraw-path-cmd--next this-cmd) new-cmd))
  new-cmd)

(defun edraw-path-cmd-remove (this-cmd)
  (when (and this-cmd
             (not (edraw-path-cmd-terminator-p this-cmd)))
    (let ((prev-cmd (edraw-path-cmd--prev this-cmd))
          (next-cmd (edraw-path-cmd--next this-cmd)))
      (when prev-cmd
        (setf (edraw-path-cmd--next prev-cmd) next-cmd)
        (setf (edraw-path-cmd--prev this-cmd) nil))
      (when next-cmd
        (setf (edraw-path-cmd--prev next-cmd) prev-cmd)
        (setf (edraw-path-cmd--next this-cmd) nil)))))

(defun edraw-path-cmd-remove-range (first last)
  (when (and first last)
    (let ((prev-cmd (edraw-path-cmd--prev first))
          (next-cmd (edraw-path-cmd--next last)))
      (when prev-cmd
        (setf (edraw-path-cmd--next prev-cmd) next-cmd))
      (when next-cmd
        (setf (edraw-path-cmd--prev next-cmd) prev-cmd))
      (setf (edraw-path-cmd--prev first) nil)
      (setf (edraw-path-cmd--next last) nil))))

(defun edraw-path-cmd-insert-range-after (cmd first last)
  (when (and cmd first last)
    (let ((next-cmd (edraw-path-cmd--next cmd)))
      (setf (edraw-path-cmd--next cmd) first)
      (setf (edraw-path-cmd--prev first) cmd)
      (when next-cmd
        (setf (edraw-path-cmd--prev next-cmd) last))
      (setf (edraw-path-cmd--next last) next-cmd))))

(defun edraw-path-cmd-insert-range-before (cmd first last)
  (when (and cmd first last)
    (let ((prev-cmd (edraw-path-cmd--prev cmd)))
      (when prev-cmd
        (setf (edraw-path-cmd--next prev-cmd) first))
      (setf (edraw-path-cmd--prev first) prev-cmd)
      (setf (edraw-path-cmd--prev cmd) last)
      (setf (edraw-path-cmd--next last) cmd))))

(defun edraw-path-cmd-terminate (cmd)
  (setf (edraw-path-cmd--prev cmd) cmd)
  (setf (edraw-path-cmd--next cmd) cmd))

(defun edraw-path-cmd-terminator-p (cmd)
  (null (edraw-path-cmd-type cmd)))

(defun edraw-path-cmd-front-p (cmd)
  (null (edraw-path-cmd-prev cmd))) ;; cmd--prev is nil or terminator

(defun edraw-path-cmd-back-p (cmd)
  (null (edraw-path-cmd-next cmd))) ;; cmd--next is nil or terminator

(defun edraw-path-cmd-subpaths-from-M (move-cmd)
  "Return a list of subpath range (first . last) of path that
beginning from MOVE-CMD.

Each range does not include the first M command.

The last range may not end with the Z command. The rest of the
range always ends with the Z command."
  (when (and move-cmd
             (not (edraw-path-cmd-terminator-p move-cmd))
             (edraw-path-cmd-is-M move-cmd))
    (let ((cmd move-cmd)
          (subpath-first (edraw-path-cmd-next move-cmd))
          subpath-list)
      (while (let ((next-cmd (edraw-path-cmd-next cmd)))
               (cond
                ((or (null next-cmd)
                     (edraw-path-cmd-is-M next-cmd))
                 (unless (eq next-cmd subpath-first)
                   (push (cons subpath-first cmd) subpath-list))
                 nil)
                ((edraw-path-cmd-is-Z next-cmd)
                 (push (cons subpath-first next-cmd) subpath-list)
                 (setq subpath-first (edraw-path-cmd-next next-cmd))
                 (setq cmd next-cmd)
                 t)
                (t
                 (setq cmd next-cmd)
                 t))))
      (nreverse subpath-list))))

(defun edraw-path-cmd-Zs-from-M (move-cmd)
  "Return a list of Z commands that correspond to M command specified by MOVE-CMD.

One M command may correspond to multiple Z commands (see: https://www.w3.org/TR/SVG11/paths.html#PathDataClosePathCommand )"
  (when (and move-cmd
             (not (edraw-path-cmd-terminator-p move-cmd)))
    (let ((cmd (if (eq (edraw-path-cmd-type move-cmd) 'M)
                   (edraw-path-cmd-next move-cmd)
                 move-cmd))
          z-list)
      (while (and cmd
                  (not (eq (edraw-path-cmd-type cmd) 'M)))
        (when (eq (edraw-path-cmd-type cmd) 'Z)
          (push cmd z-list))
        (setq cmd (edraw-path-cmd-next cmd)))
      (nreverse z-list))))

(defun edraw-path-cmd-prev-M (close-cmd)
  (when close-cmd
    (let ((cmd close-cmd))
      (while (and (setq cmd (edraw-path-cmd-prev cmd))
                  (not (eq (edraw-path-cmd-type cmd) 'M))))
      cmd)))

(defun edraw-path-cmd-end-of-subpath (cmd)
  (when cmd
    (let (prev-cmd)
      (if (and (edraw-path-cmd-is-M cmd)
               (null (edraw-path-cmd-next cmd)))
          ;; End with M
          cmd
        ;; Skip first M
        (when (edraw-path-cmd-is-M cmd)
          (setq prev-cmd cmd)
          (setq cmd (edraw-path-cmd-next cmd)))

        ;; Find Z or M or End
        (while (and cmd
                    (not (edraw-path-cmd-is-type-p cmd 'M 'Z)))
          (setq prev-cmd cmd)
          (setq cmd (edraw-path-cmd-next cmd)))

        (if (or (null cmd)
                (edraw-path-cmd-is-M cmd))
            ;; Return one before M or the end of cmdlist. (Open path)
            prev-cmd
          ;; Return Z (Closed path)
          cmd)))))



;;;;;; cmd - Anchor Point

(defun edraw-path-cmd-has-anchor-point-p (cmd)
  (edraw-path-cmd-is-type-p cmd 'M 'L 'C))

(defun edraw-path-cmd-anchor-point-arg-index (cmd include-closing-segment-p)
  "Return the index of the argument that contains the anchor point in CMD.

Return nil if include-closing-segment-p is nil and CMD is a closing segment.

Specify t for include-closing-segment-p if you want the index of the anchor of closing segment, or specify 'fast if you know CMD is not a closing segment and do not need checking."
  (pcase (edraw-path-cmd-type cmd)
    ('M 0)
    ('L (if (or include-closing-segment-p (not (edraw-path-cmd-closing-segment-p cmd))) 0))
    ('C (if (or include-closing-segment-p (not (edraw-path-cmd-closing-segment-p cmd))) 2))))

(defun edraw-path-cmd-anchor-point (cmd include-closing-segment-p)
  "Return a edraw-path-point object that reference to CMD's anchor point.

You can change the anchor point through returned object."
  (when-let ((index (edraw-path-cmd-anchor-point-arg-index cmd include-closing-segment-p)))
    (edraw-path-cmd-arg-pt cmd index)))

(defun edraw-path-cmd-anchor-point-xy (cmd include-closing-segment-p)
  "Return coordinates of CMD's anchor point."
  (when-let ((index (edraw-path-cmd-anchor-point-arg-index cmd include-closing-segment-p)))
    (edraw-path-cmd-arg-xy cmd index)))

(defun edraw-path-cmd-prev-anchor-point (cmd)
  ;;- If cmd is M, jump before the corresponding closing sequence. If
  ;;  not exists, return nil
  ;;- If cmd is Z and prev Z is closing segment, skip it.
  ;;- If prev cmd is M, L or C, return it.
  ;;- If prev cmd is Z, return corresponding M. (Skip other subpaths)
  ;;- If prev cmd -fhp, skip it.
  (when cmd
    (cond
     ;; Jump M to corresponding Z if exists
     ((edraw-path-cmd-is-M cmd)
      ;;@todo support multiple Z issue
      (setq cmd (edraw-path-cmd-jump-to-prev-z-from-m cmd)))
     ;; Skip closing segment
     ((and (edraw-path-cmd-is-Z cmd)
           (edraw-path-cmd-closing-segment-p (edraw-path-cmd-prev cmd)))
      (setq cmd (edraw-path-cmd-prev cmd))))

    (when cmd
      (let (found)
        (while (not found)
          (setq cmd (edraw-path-cmd-prev cmd))
          (cond
           ((null cmd)
            (setq found (list nil)))
           ((edraw-path-cmd-is-type-p cmd 'M 'L 'C)
            (setq found (list cmd)))
           ((edraw-path-cmd-is-Z cmd)
            (setq found (list (edraw-path-cmd-prev-M cmd))))))
        (when found
          (edraw-path-cmd-anchor-point (car found) 'fast))))))

(defun edraw-path-cmd-jump-to-prev-z-from-m (m-cmd)
  ;;@todo support multiple Z issue. needs to subpath information
  (when-let ((z-cmd (car (edraw-path-cmd-Zs-from-M m-cmd)))
             (pz-cmd (edraw-path-cmd-prev z-cmd)))
    ;; check pz-cmd is closing segment
    (if (edraw-path-cmd-closing-segment-p pz-cmd m-cmd)
        ;; pz-cmd is a closing segment, skip z-cmd
        pz-cmd
      z-cmd)))

(defun edraw-path-cmd-next-anchor-point (cmd)
  ;;- If cmd is Z or closing segment, jump to corresponding M.
  ;;- If next cmd is M, return nil.
  ;;- If next cmd is -fhp, skip it.
  ;;- If next cmd is L or C, return it.
  (when cmd
    (let ((anchor-cmd
           (if (edraw-path-cmd-closing-sequence-p cmd)
               ;; skip closing sequence
               (edraw-path-cmd-prev-M cmd) ;;found
             ;; -fhp, M, L, C (not closing)
             (let (found)
               (while (not found)
                 (setq cmd (edraw-path-cmd-next cmd))
                 (cond
                  ;;end of path data
                  ((null cmd)
                   (setq found (list nil)))
                  ;;end of open sub-path
                  ((edraw-path-cmd-is-M cmd)
                   (setq found (list nil)))
                  ;;end of closed sub-path (L, C, or Z)
                  ((edraw-path-cmd-closing-sequence-p cmd)
                   (setq found (list (edraw-path-cmd-prev-M cmd))))
                  ;;anchor point (L or C)
                  ((edraw-path-cmd-has-anchor-point-p cmd)
                   (setq found (list cmd)))))
               (car found)))))
      (when anchor-cmd
        (edraw-path-cmd-anchor-point anchor-cmd 'fast)))))

(defun edraw-path-cmd-closing-sequence-p (cmd)
  "Return t if CMD is Z or edraw-path-cmd-closing-segment-p."
  (when cmd
    (or (edraw-path-cmd-is-Z cmd)
        (edraw-path-cmd-closing-segment-p cmd))))

(defun edraw-path-cmd-closing-segment-p (cmd &optional m-cmd-hint)
  "Return t if CMD is a closing segment.

A closing segment is the last segment of a path, and the anchor
point (end point) of the segment is the same as the start point
of the path (represented by the M command).

The anchor point of a closing segment must be equated with the
start point of the path. For example, if you move the start point
of a path, you must move the anchor of the closing segment at the
same time.

Externally, the anchor point of a closing segment is treated as
if it does not exist. Only the start point of the path by M
command is exposed.
"
  ;; M ... cmd(L or C) Z and M.xy==cmd.xy
  (when (and cmd
             (edraw-path-cmd-is-L-or-C cmd))
    (when-let ((next-cmd (edraw-path-cmd-next cmd)))
      (when (edraw-path-cmd-is-Z next-cmd)
        (when-let ((m-cmd (or m-cmd-hint (edraw-path-cmd-prev-M next-cmd))))
          (let ((m-xy (edraw-path-cmd-anchor-point-xy m-cmd 'fast))
                (cmd-xy (edraw-path-cmd-anchor-point-xy cmd t)))
            (edraw-xy-equal-p m-xy cmd-xy)))))))

(defun edraw-path-cmd-prev-anchor-point-xy (cmd)
  (when-let ((anchor (edraw-path-cmd-prev-anchor-point cmd)))
    (edraw-path-point-xy anchor)))

(defun edraw-path-cmd-next-anchor-point-xy (cmd)
  (when-let ((anchor (edraw-path-cmd-next-anchor-point cmd)))
    (edraw-path-point-xy anchor)))

;;;;;; cmd - Handle Point

(defun edraw-path-cmd-has-handle-point-0 (cmd
                                          &optional include-same-position-p)
  "Return t if command argument 0 has a valid handle point."
  (and
   (edraw-path-cmd-is-type-p cmd 'C '-forward-handle-point)
   (or include-same-position-p
       (not (edraw-xy-equal-p
             (edraw-path-cmd-prev-anchor-point-xy cmd) ;;prev anchor point
             (edraw-path-cmd-arg-xy cmd 0)))))) ;; handle point

(defun edraw-path-cmd-has-handle-point-1 (cmd
                                          &optional include-same-position-p)
  "Return t if command argument 1 has a valid handle point."
  (and
   (edraw-path-cmd-is-C cmd)
   (or include-same-position-p
       (not (edraw-xy-equal-p
             (edraw-path-cmd-arg-xy cmd 1);;handle point
             (edraw-path-cmd-arg-xy cmd 2))))));;anchor point

(defun edraw-path-cmd-handle-point-arg-indices
    (cmd &optional include-same-position-p)
  (let ((has0 (edraw-path-cmd-has-handle-point-0 cmd include-same-position-p))
        (has1 (edraw-path-cmd-has-handle-point-1 cmd include-same-position-p)))
    (cond
     ((and has0 has1) (list 0 1))
     (has0 (list 0))
     (has1 (list 1)))))

(defun edraw-path-cmd-handle-points (cmd &optional include-same-position-p)
  (mapcar
   (lambda (index) (edraw-path-cmd-arg-pt cmd index))
   (edraw-path-cmd-handle-point-arg-indices cmd include-same-position-p)))

(defun edraw-path-cmd-handle-points-xy (cmd
                                        &optional include-same-position-p)
  (mapcar
   (lambda (index) (edraw-path-cmd-arg-xy cmd index)) ;;clone or not?
   (edraw-path-cmd-handle-point-arg-indices cmd include-same-position-p)))

;;;;;; cmd - Point

(defun edraw-path-cmd-pick-point (cmd xy anchor-radius handle-radius)
  "Returns a edraw-path-point object that reference to the anchor point or handle point at the position specified by XY."
  (or
   ;; Find from handle points
   (seq-some
    (lambda (index)
      (when (edraw-in-square-p (edraw-path-cmd-arg-xy cmd index)
                               xy handle-radius)
        (edraw-path-cmd-arg-pt cmd index)))
    (edraw-path-cmd-handle-point-arg-indices cmd))

   ;; Find from anchor point
   (when-let ((index (edraw-path-cmd-anchor-point-arg-index cmd nil))) ;;ignore closing segment's anchor
     (when (edraw-in-square-p (edraw-path-cmd-arg-xy cmd index)
                              xy anchor-radius)
       (edraw-path-cmd-arg-pt cmd index)))))

(defun edraw-path-cmd-translate (cmd delta-xy)
  (pcase (edraw-path-cmd-type cmd)
    ('M
     (edraw-path-point-translate (edraw-path-cmd-arg-pt cmd 0) delta-xy))
    ('L
     (edraw-path-point-translate (edraw-path-cmd-arg-pt cmd 0) delta-xy))
    ('C
     (edraw-path-point-translate (edraw-path-cmd-arg-pt cmd 0) delta-xy)
     (edraw-path-point-translate (edraw-path-cmd-arg-pt cmd 1) delta-xy)
     (edraw-path-point-translate (edraw-path-cmd-arg-pt cmd 2) delta-xy))
    ('-forward-handle-point
     (edraw-path-point-translate (edraw-path-cmd-arg-pt cmd 0) delta-xy))))

(defun edraw-path-cmd-transform (cmd matrix)
  (pcase (edraw-path-cmd-type cmd)
    ('M
     (edraw-path-point-transform (edraw-path-cmd-arg-pt cmd 0) matrix))
    ('L
     (edraw-path-point-transform (edraw-path-cmd-arg-pt cmd 0) matrix))
    ('C
     (edraw-path-point-transform (edraw-path-cmd-arg-pt cmd 0) matrix)
     (edraw-path-point-transform (edraw-path-cmd-arg-pt cmd 1) matrix)
     (edraw-path-point-transform (edraw-path-cmd-arg-pt cmd 2) matrix))
    ('-forward-handle-point
     (edraw-path-point-transform (edraw-path-cmd-arg-pt cmd 0) matrix))))

;;;;;; cmd - Segment

(defun edraw-path-cmd-divide-segment (cmd)
  "Divide one path segment into two.

Insert a new command before CMD and return it.

If there is a handle point for the previous anchor point, it will be transferred to the new command. New anchor points and handle points are created as needed."
  (pcase (edraw-path-cmd-type cmd)
    ('M
     ;; When closed path
     (when-let ((z-cmd (car (edraw-path-cmd-Zs-from-M cmd))) ;;@todo multiple Z issue
                (pz-cmd (edraw-path-cmd-prev z-cmd)))
       ;; Divide closing segment
       (if (edraw-path-cmd-closing-segment-p pz-cmd)
           (edraw-path-cmd-divide-segment pz-cmd)
         ;;@todo there is no closing segment. make segment and divide? (see: edraw-path-cmdlist-close-path)
         )))
    ('L
     (when-let ((curr-xy (edraw-path-cmd-arg-xy cmd 0))
                (prev-xy (edraw-path-cmd-prev-anchor-point-xy cmd)))
       (edraw-path-cmd-insert-before ;;return the new cmd
        cmd
        (edraw-path-cmd 'L
                        (edraw-xy-midpoint-float curr-xy prev-xy)))))
    ('C
     (when-let ((prev-xy (edraw-path-cmd-prev-anchor-point-xy cmd))
                (handle0 (edraw-path-cmd-arg-pt cmd 0))
                (handle1 (edraw-path-cmd-arg-pt cmd 1))
                (anchor  (edraw-path-cmd-arg-pt cmd 2)))
       (let* ((seg2 (edraw-bezier-segment-divide
                     (vector
                      prev-xy
                      (edraw-path-point-xy handle0)
                      (edraw-path-point-xy handle1)
                      (edraw-path-point-xy anchor)))))
         ;; Do not invalidate existing path-point objects
         ;; second half
         (edraw-path-cmd-overwrite-from-ppoints
          cmd
          'C
          (edraw-path-point 'handle cmd 0 (elt (cdr seg2) 1)) ;;new handle
          (edraw-path-point-move handle1  (elt (cdr seg2) 2))
          (edraw-path-point-move anchor   (elt (cdr seg2) 3)))

         ;; first half
         (edraw-path-cmd-insert-before ;;return new cmd
          cmd
          (edraw-path-cmd-from-ppoints
           'C
           (edraw-path-point-move handle0  (elt (car seg2) 1))
           (edraw-path-point 'handle nil 1 (elt (car seg2) 2)) ;;new handle
           (edraw-path-point 'anchor nil 2 (elt (car seg2) 3)) ;;new anchor
           )))))))



;;;;; edraw-path-point

;;
;; - Represent the anchor point or (bezier curve) handle point in
;;   edraw-path-cmd object.
;; - Provides interface that makes it easy to change points.
;;

(defun edraw-path-point (type cmd arg-index xy)
  (vector type cmd arg-index (edraw-xy-clone xy)))

(defmacro edraw-path-point-type (ppoint) `(aref ,ppoint 0))
(defmacro edraw-path-point-cmd (ppoint) `(aref ,ppoint 1))
(defmacro edraw-path-point-arg-index (ppoint) `(aref ,ppoint 2))
;;(defmacro edraw-path-point-xy (ppoint) `(aref ,ppoint 3))
(defun edraw-path-point-xy (ppoint) (aref ppoint 3))

(defun edraw-path-point-transfer (ppoint cmd arg-index)
  "Change the related command of PPOINT to CMD.

Use only special functions such as `edraw-path-cmd-from-ppoints'
and `edraw-path-cmd-overwrite-from-ppoints'."
  (setf (edraw-path-point-cmd ppoint) cmd)
  (setf (edraw-path-point-arg-index ppoint) arg-index))

;;;;;; point - Type

(defun edraw-path-point-anchor-p (ppoint)
  (eq (edraw-path-point-type ppoint) 'anchor))

(defun edraw-path-point-handle-p (ppoint)
  (eq (edraw-path-point-type ppoint) 'handle))

;;;;;; point - Position

(defun edraw-path-point-equal-xy-p (ppoint xy)
  (edraw-xy-equal-p (edraw-path-point-xy ppoint) xy))

(defun edraw-path-point-equal-point-p (ppoint ppoint2)
  (edraw-xy-equal-p (edraw-path-point-xy ppoint)
                    (edraw-path-point-xy ppoint2)))

(defun edraw-path-point-in-square-p (ppoint xy r)
  (edraw-in-square-p (edraw-path-point-xy ppoint) xy r))

(defun edraw-path-point-in-circle-p (ppoint xy r)
  (edraw-in-circle-p (edraw-path-point-xy ppoint) xy r))

(defun edraw-path-point-translate (ppoint delta-xy)
  (let ((cell (edraw-path-point-xy ppoint)))
    (setcar cell (+ (car cell) (car delta-xy)))
    (setcdr cell (+ (cdr cell) (cdr delta-xy))))
  ppoint)

(defun edraw-path-point-transform (ppoint matrix)
  (let* ((cell (edraw-path-point-xy ppoint))
         (new-p (edraw-matrix-mul-mat-xy matrix cell)))
    (setcar cell (car new-p))
    (setcdr cell (cdr new-p)))
  ppoint)

(defun edraw-path-point-move (ppoint new-xy)
  (let ((xy-cell (edraw-path-point-xy ppoint)))
    (if (edraw-path-point-anchor-p ppoint)
        (let ((cmd (edraw-path-point-cmd ppoint)))
          (cond
           ;; Move all closing segments before Z that corresponding to M to xy
           ((edraw-path-cmd-is-M cmd)
            (dolist (z-cmd (edraw-path-cmd-Zs-from-M cmd))
              (when-let ((pz-cmd (edraw-path-cmd-prev z-cmd))
                         (pz-anchor (edraw-path-cmd-anchor-point pz-cmd t))
                         (pz-xy-cell (edraw-path-point-xy pz-anchor)))
                (when (edraw-xy-equal-p pz-xy-cell xy-cell)
                  (edraw-xy-assign pz-xy-cell new-xy))))
            (edraw-xy-assign xy-cell new-xy))
           ;; Closing segment cannot be moved directly. Please move M
           ((edraw-path-cmd-closing-segment-p cmd)
            nil)
           ;; Other anchor point
           (t
            (edraw-xy-assign xy-cell new-xy))))
      ;; Handle point
      (edraw-xy-assign xy-cell new-xy)))
  ppoint)

(defun edraw-path-point-move-with-related-points (ppoint new-xy)
  ;; Move handle points of anchor PPOINT
  (when (edraw-path-point-anchor-p ppoint)
    (let* ((old-xy (edraw-path-point-xy ppoint))
           (delta-xy (edraw-xy-sub new-xy old-xy))) ;; numerical error...
      ;;@todo multiple Z issue. get all backward handles and move
      (when-let ((handle (edraw-path-anchor-backward-handle ppoint t)))
        (edraw-path-point-move
         handle (edraw-xy-add (edraw-path-point-xy handle) delta-xy)))
      (when-let ((handle (edraw-path-anchor-forward-handle ppoint t)))
        (edraw-path-point-move
         handle (edraw-xy-add (edraw-path-point-xy handle) delta-xy)))))
  ;; Move PPOINT
  (edraw-path-point-move ppoint new-xy))

;;;;;; point - Relationship

(defun edraw-path-point-index-in-cmdlist (ppoint)
  "Return the index of PPOINT in cmdlist."
  (let* ((last-cmd (edraw-path-point-cmd ppoint))
         (cmd (edraw-path-cmd-prev last-cmd))
         (count 0))
    (while cmd
      (setq count
            (+ count
               (pcase (edraw-path-cmd-type cmd)
                 ((or 'M 'L) 1)
                 ((or 'C) 3)
                 (_ 0))))
      (setq cmd (edraw-path-cmd-prev cmd)))
    (+ count (edraw-path-point-arg-index ppoint))))

(defun edraw-path-point-prev-anchor (ppoint)
  (edraw-path-cmd-prev-anchor-point (edraw-path-point-cmd ppoint)))

(defun edraw-path-point-next-anchor (ppoint)
  (if (edraw-path-point-anchor-p ppoint)
      ;; anchor point
      (edraw-path-cmd-next-anchor-point (edraw-path-point-cmd ppoint))
    ;; handle point
    (let ((curr-cmd (edraw-path-point-cmd ppoint)))
      (if (eq (edraw-path-cmd-type curr-cmd) 'C)
          (if (edraw-path-cmd-closing-segment-p curr-cmd)
              ;; Return the previous M anchor
              (edraw-path-cmd-next-anchor-point curr-cmd)
            (edraw-path-cmd-anchor-point curr-cmd 'fast))
        ;;'-forward-handle-point ?
        (edraw-path-cmd-next-anchor-point curr-cmd)))))

(defun edraw-path-point-prev-anchor-xy (ppoint)
  (when-let ((anchor (edraw-path-point-prev-anchor ppoint)))
    (edraw-path-point-xy anchor)))

(defun edraw-path-point-next-anchor-xy (ppoint)
  (when-let ((anchor (edraw-path-point-next-anchor ppoint)))
    (edraw-path-point-xy anchor)))

;;;;;; point - Modifiers

(defun edraw-path-point-remove (ppoint)
  "Remove PPOINT from cmdlist."
  (cond
   ;; Remove anchor
   ((edraw-path-point-anchor-p ppoint) ;;M, L, C
    (let* ((curr-cmd (edraw-path-point-cmd ppoint))
           (next-cmd (edraw-path-cmd-next curr-cmd))
           (curr-cmd-type (edraw-path-cmd-type curr-cmd)) ;;either M, L, C
           (next-cmd-type (if next-cmd (edraw-path-cmd-type next-cmd) nil)))
      (cond
       ;; Do not remove closing segment directly. Please remove M
       ((and (eq next-cmd-type 'Z)
             (edraw-path-cmd-closing-segment-p curr-cmd))
        nil)

       ;; End of the path
       ((or (eq next-cmd-type 'Z)
            (eq next-cmd-type '-forward-handle-point)
            (edraw-path-cmd-back-p curr-cmd)) ;;(eq next-cmd-type nil)
        ;; Remove the forward handle of the anchor
        (when (eq next-cmd-type '-forward-handle-point)
          (edraw-path-cmd-remove next-cmd)) ;;invalidate handle0
        ;; Remove the last segment of the path
        (cond
         ;; Remove trailing M
         ((eq curr-cmd-type 'M)
          (when (eq next-cmd-type 'Z)
            (edraw-path-cmd-remove next-cmd))
          (edraw-path-cmd-remove curr-cmd)
          t)
         ;; Replace C with -forward-handle-point. Keep previous anchor's handle
         ((eq curr-cmd-type 'C)
          ;; (Keep next Z if exists)
          (edraw-path-cmd-overwrite-from-ppoints
           curr-cmd '-forward-handle-point
           (edraw-path-cmd-arg-pt curr-cmd 0)) ;;invalidate handle1 and anchor
          t)
         ;; Remove L
         (t
          ;; (Keep next Z if exists)
          (edraw-path-cmd-remove curr-cmd) ;;invalidate anchor
          t)))

       ;; Beginning of the path
       ((eq curr-cmd-type 'M)
        (dolist (subpath (edraw-path-cmd-subpaths-from-M curr-cmd))
          (let ((first (car subpath))
                (last (cdr subpath)))
            (cond
             ;; Remove the subpath if no segments remain
             ;; M (Z)
             ;; M (LC)
             ;; M (-fhp)
             ((eq first last)
              (edraw-path-cmd-remove first))
             ;; M (closing-segment Z)
             ;; M (LC Z)
             ;; M (LC LC) OK
             ;; M (LC -fhp)
             ((and (eq (edraw-path-cmd-next first) last)
                   (not (edraw-path-cmd-is-L-or-C last)))
              (edraw-path-cmd-remove first)
              (edraw-path-cmd-remove last))
             ;; M (LC closing-segment Z)
             ;; M (LC LC Z) OK
             ;; M (LC LC LC) OK
             ;; M (LC LC -fhp) OK
             ((edraw-path-cmd-closing-segment-p (edraw-path-cmd-next first))
              (edraw-path-cmd-remove first)
              (edraw-path-cmd-remove (edraw-path-cmd-prev last))
              (edraw-path-cmd-remove last))
             ;; M (LC LC .... Z-or-not) OK
             (t
              ;; Move anchor of closing segment to first command anchor
              (let ((pl-cmd (edraw-path-cmd-prev last)))
                (when (edraw-path-cmd-closing-segment-p pl-cmd)

                  (let ((handle0 (if (edraw-path-cmd-has-handle-point-0 pl-cmd)
                                     (edraw-path-cmd-arg-pt pl-cmd 0)))
                        (handle1 (if (edraw-path-cmd-has-handle-point-1 first)
                                     (edraw-path-cmd-arg-pt first 1)))
                        (pl-anchor (edraw-path-cmd-anchor-point pl-cmd t))
                        (prev-xy (edraw-path-cmd-prev-anchor-point-xy pl-cmd))
                        (first-xy (edraw-path-cmd-anchor-point-xy first 'fast)))

                    ;; Adjust handles
                    (if prev-xy ;; if nil, broken path?
                        (if (or handle0 handle1)
                            (edraw-path-cmd-overwrite-from-ppoints
                             pl-cmd 'C
                             (or handle0 (edraw-path-point 'handle pl-cmd 0 prev-xy))
                             (or handle1 (edraw-path-point 'handle pl-cmd 1 first-xy))
                             pl-anchor)
                          (edraw-path-cmd-overwrite-from-ppoints
                           pl-cmd 'L
                           pl-anchor)))
                    ;; Move anchor
                    (edraw-xy-assign
                     (edraw-path-point-xy pl-anchor)
                     first-xy))))
              ;; Replace first command to M
              (edraw-path-cmd-overwrite-from-ppoints
               first 'M
               (edraw-path-cmd-anchor-point first 'fast)) ;;invalidate handles when first is C
              ))))
        (edraw-path-cmd-remove curr-cmd) ;;invalidate anchor
        t)

       ;; There is no M at the beginning (illegal path data)
       ((edraw-path-cmd-front-p curr-cmd)
        nil)

       ;; merge L and L
       ((and (eq curr-cmd-type 'L) ;;and not closing segment
             (eq next-cmd-type 'L)) ;;or closing segment
        (edraw-path-cmd-remove curr-cmd) ;;invalidate anchor
        t)

       ;; merge L and C
       ((and (memq curr-cmd-type '(L C)) ;;and not closing segment
             (memq next-cmd-type '(L C))) ;;or closing segment
        (when-let ((prev-anchor-xy (edraw-path-point-prev-anchor-xy ppoint))) ;;if nil, path is broken
          (let* ((next-anchor-xy (edraw-path-cmd-anchor-point-xy next-cmd t))
                 (prev-handle (if (eq curr-cmd-type 'C)
                                  (edraw-path-cmd-arg-pt curr-cmd 0)))
                 (next-handle (if (eq next-cmd-type 'C)
                                  (edraw-path-cmd-arg-pt next-cmd 1))))
            (if (and (or (null prev-handle)
                         (edraw-xy-equal-p prev-anchor-xy
                                           (edraw-path-point-xy prev-handle)))
                     (or (null next-handle)
                         (edraw-xy-equal-p next-anchor-xy
                                           (edraw-path-point-xy next-handle))))
                ;; straight line
                (edraw-path-cmd-overwrite-from-ppoints
                 next-cmd 'L
                 (edraw-path-cmd-anchor-point next-cmd t)) ;;invalidate...
              ;; curve line
              (edraw-path-cmd-overwrite-from-ppoints
               next-cmd 'C
               (or prev-handle (edraw-path-point 'handle nil 0 prev-anchor-xy))
               (or next-handle (edraw-path-point 'handle nil 1 next-anchor-xy))
               (edraw-path-cmd-anchor-point next-cmd t)))) ;;invalidate...
          (edraw-path-cmd-remove curr-cmd) ;;invalidate...
          t)))))

   ;; Remove handle
   ((edraw-path-point-handle-p ppoint)
    (let* ((curr-cmd (edraw-path-point-cmd ppoint))
           (curr-cmd-type (edraw-path-cmd-type curr-cmd)))
      (pcase curr-cmd-type ;;either C, -forward-handle-point
        ('C
         (when-let ((prev-anchor-xy (edraw-path-point-prev-anchor-xy ppoint))) ;; if nil, path is broken
           (let* ((handle0 (edraw-path-cmd-arg-pt curr-cmd 0))
                  (handle1 (edraw-path-cmd-arg-pt curr-cmd 1))
                  (anchor (edraw-path-cmd-arg-pt curr-cmd 2))
                  (anchor-xy (edraw-path-point-xy anchor))
                  (arg-index (edraw-path-point-arg-index ppoint)))
             ;; Move the position of the handle point to be deleted to
             ;; the position of the corresponding anchor point.
             (cond
              ((= arg-index 0) (edraw-path-point-move handle0 prev-anchor-xy))
              ((= arg-index 1) (edraw-path-point-move handle1 anchor-xy)))
             ;; If two handle points are not necessary,
             ;; replace C command to L. (straight line)
             (when (and (edraw-xy-equal-p (edraw-path-point-xy handle0)
                                          prev-anchor-xy)
                        (edraw-xy-equal-p (edraw-path-point-xy handle1)
                                          anchor-xy))
               (edraw-path-cmd-overwrite-from-ppoints
                curr-cmd 'L
                anchor))) ;;invalidate handle0, handle1
           t))
        ('-forward-handle-point
         (edraw-path-cmd-remove curr-cmd) ;;invalidate handle0
         t))))))

;;;;;; point - Anchor Point

(defun edraw-path-anchor-first-p (ppoint)
  (null (edraw-path-point-prev-anchor ppoint)))

(defun edraw-path-anchor-last-p (ppoint)
  (null (edraw-path-point-next-anchor ppoint)))

(defun edraw-path-anchor-in-closed-subpath-p (ppoint)
  (not (null (edraw-path-cmd-Zs-from-M (edraw-path-point-cmd ppoint)))))

(defun edraw-path-anchor-insert-midpoint-before (ppoint)
  (when (edraw-path-point-anchor-p ppoint)
    (when-let ((new-cmd (edraw-path-cmd-divide-segment
                         (edraw-path-point-cmd ppoint))))
      (edraw-path-cmd-anchor-point new-cmd 'fast)))) ;; return new point if divided

(defun edraw-path-anchor-backward-handle (anchor-point
                                          &optional include-same-position-p)
  "Return a edraw-path-point object that reference to the
backward handle point of ANCHOR-POINT. Return nil if it does not
exist."
  (when (edraw-path-point-anchor-p anchor-point) ;;M, L, C
    (let ((cmd (edraw-path-point-cmd anchor-point)))
      (pcase (edraw-path-cmd-type cmd)
        ;; if cmd type is M, find the corresponding Z and return
        ;; handle1 of the cmd before Z
        ('M
         ;;@todo multiple Z issue
         (when-let ((z-cmd (car (edraw-path-cmd-Zs-from-M cmd)))
                    (pz-cmd (edraw-path-cmd-prev z-cmd)))
           (when (and
                  ;; pz-cmd has a handle1
                  ;; (pz-cmd is C, handle1 and anchor are not in the same position)
                  (edraw-path-cmd-has-handle-point-1
                   pz-cmd include-same-position-p)
                  ;; Anchor point of pz-cmd is the same position as M
                  ;; (check pz-cmd is closing segment)
                  (edraw-xy-equal-p
                   (edraw-path-cmd-anchor-point-xy pz-cmd t)
                   (edraw-path-cmd-anchor-point-xy cmd 'fast)))
             (edraw-path-cmd-arg-pt pz-cmd 1))))
        ;; L has no backward handle
        ('L
         nil)
        ;; C has a backward handle. But somecases it is invalid.
        ('C
         (when (edraw-path-cmd-has-handle-point-1 cmd include-same-position-p)
           (edraw-path-cmd-arg-pt cmd 1)))))))

(defun edraw-path-anchor-forward-handle (anchor-point
                                         &optional include-same-position-p)
  "Return a edraw-path-point object that reference to the forward
handle point of ANCHOR-POINT. Return nil if it does not exist."
  (when (edraw-path-point-anchor-p anchor-point) ;;M, L, C
    (let ((cmd (edraw-path-point-cmd anchor-point)))
      ;; Skip closing segment
      ;; (M ... cmd=(C or L) Z) and cmd.xy==M.xy
      (when (edraw-path-cmd-closing-segment-p cmd)
        (setq cmd (edraw-path-cmd-prev-M cmd)))

      (when cmd
        (let ((next-cmd (edraw-path-cmd-next cmd)))
          ;; Use next-cmd's first handle point
          (when (and next-cmd
                     (edraw-path-cmd-has-handle-point-0 next-cmd include-same-position-p))
            (edraw-path-cmd-arg-pt next-cmd 0)))))))

(defun edraw-path-anchor-create-backward-handle (anchor-point)
  "Return a edraw-path-point object that reference to the backward handle point of ANCHOR-POINT. If it doesn't exist, create it."
  (when (edraw-path-point-anchor-p anchor-point) ;;M, L, C
    (let ((cmd (edraw-path-point-cmd anchor-point)))
      (pcase (edraw-path-cmd-type cmd)
        ('M
         ;;@todo multiple Z issue
         (when-let ((z-cmd (car (edraw-path-cmd-Zs-from-M cmd)))
                    (pz-cmd (edraw-path-cmd-prev z-cmd))
                    (pz-xy (edraw-path-cmd-anchor-point-xy pz-cmd t)));;pz-cmd is either M, L, C
           ;; Anchor point of pz-cmd is the same position as M
           ;; (check pz-cmd is closing segment)
           (when (edraw-xy-equal-p
                  pz-xy
                  (edraw-path-cmd-anchor-point-xy cmd 'fast))
             (pcase (edraw-path-cmd-type pz-cmd)
               ('C
                (edraw-path-cmd-arg-pt pz-cmd 1))
               ('L
                (edraw-path-cmd-L-to-C pz-cmd) ;;Create two new handles
                (edraw-path-cmd-arg-pt pz-cmd 1))))))
        ('L
         (edraw-path-cmd-L-to-C cmd) ;;Create two new handles
         (edraw-path-cmd-arg-pt cmd 1))
        ('C
         (edraw-path-cmd-arg-pt cmd 1))))))

(defun edraw-path-anchor-create-forward-handle (anchor-point)
  "Return a edraw-path-point object that reference to the forward handle point of ANCHOR-POINT. If it doesn't exist, create it."
  (when (edraw-path-point-anchor-p anchor-point) ;;M, L, C
    (let ((cmd (edraw-path-point-cmd anchor-point)))
      ;; Skip closing segment
      ;; (M ... cmd=(C or L) Z) and cmd.xy==M.xy
      (when (edraw-path-cmd-closing-segment-p cmd)
        (setq cmd (edraw-path-cmd-prev-M cmd)))

      (when cmd
        (let ((next-cmd (edraw-path-cmd-next cmd)))
          (pcase (if next-cmd (edraw-path-cmd-type next-cmd))
            ;; End of path (or subpath)
            ((or 'nil 'M);;nil means no next-cmd exists
             ;; Create a new -forward-handle-point cmd, and push back
             (let* ((curr-xy (edraw-path-cmd-anchor-point-xy cmd 'fast))
                    (new-cmd (edraw-path-cmd-from-ppoints
                              '-forward-handle-point
                              (edraw-path-point 'handle nil 0 curr-xy))))
               (edraw-path-cmd-insert-after cmd new-cmd)
               (edraw-path-cmd-arg-pt new-cmd 0)))
            ('L
             ;; Convert L to C
             (edraw-path-cmd-L-to-C next-cmd)
             (edraw-path-cmd-arg-pt next-cmd 0))
            ;; Use next-cmd's first handle point
            ((or 'C '-forward-handle-point)
             (edraw-path-cmd-arg-pt next-cmd 0))
            ;; Closed without closing segment(broken path)
            ;; Ignore
            ('Z )))))))

(defun edraw-path-anchor-make-smooth (anchor-point)
  (let ((curr-xy (edraw-path-point-xy anchor-point))
        (prev-xy (edraw-path-point-prev-anchor-xy anchor-point))
        (next-xy (edraw-path-point-next-anchor-xy anchor-point)))
    ;;(message "make smooth prev=%s curr=%s next=%s" prev-xy curr-xy next-xy)
    (cond
     ((and (null prev-xy) (null next-xy))
      nil)
     ((null prev-xy) ;;(not (null next-xy))
      (when-let ((forward-handle
                  (edraw-path-anchor-create-forward-handle anchor-point)))
        (edraw-path-point-move forward-handle
                               (edraw-xy-midpoint-float curr-xy next-xy))))
     ((null next-xy) ;;(not (null prev-xy))
      (when-let ((backward-handle
                  (edraw-path-anchor-create-backward-handle anchor-point)))
        (edraw-path-point-move backward-handle
                               (edraw-xy-midpoint-float curr-xy prev-xy))))
     (t ;;(and prev-xy next-xy)
      ;;     hp+,
      ;;     ,`  `,
      ;;vp ,`   uvp`, curr-xy
      ;; p+<-----+<--+,
      ;;         vm+ | `,
      ;;             +uvn`+hn
      ;;             | , `
      ;;             +vn
      ;;             n
      (let* ((vp (edraw-xy-sub prev-xy curr-xy))
             (vn (edraw-xy-sub next-xy curr-xy))
             (vp-len (edraw-xy-length vp))
             (vn-len (edraw-xy-length vn))
             (dist-from-anchor 0.4))
        (when (and (>= vp-len 0.5) (>= vn-len 0.5))
          (let* ((uvp (edraw-xy-divn vp vp-len))
                 (uvn (edraw-xy-divn vn vn-len))
                 (vm (edraw-xy-nmul 0.5 (edraw-xy-add uvp uvn)))
                 (hp (edraw-xy-add
                      curr-xy
                      (edraw-xy-nmul
                       dist-from-anchor
                       (edraw-xy-sub vp (edraw-xy-nmul vp-len vm)))))
                 (hn (edraw-xy-add
                      curr-xy
                      (edraw-xy-nmul
                       dist-from-anchor
                       (edraw-xy-sub vn (edraw-xy-nmul vn-len vm))))))
            (when-let ((forward-handle
                        (edraw-path-anchor-create-forward-handle anchor-point)))
              (edraw-path-point-move forward-handle hn))
            (when-let ((backward-handle
                        (edraw-path-anchor-create-backward-handle anchor-point)))
              (edraw-path-point-move backward-handle hp))
            t)))))))

(defun edraw-path-anchor-split-path (anchor)
  "Split path at ANCHOR point.

Returns t if path has changed.

Splitting a path can generate multiple subpaths. Multiple
subpaths continue to stay within one path data (cmdlist
object). Use edraw-path-cmdlist-split-subpaths to retrieve
multiple subpaths from a single path data."
  (when (edraw-path-point-anchor-p anchor) ;;anchor is a M, L, C
    (let* ((cmd (edraw-path-point-cmd anchor))
           (cmd-type (edraw-path-cmd-type cmd))
           (next-cmd (edraw-path-cmd-next cmd))
           (next-cmd-type (if next-cmd (edraw-path-cmd-type next-cmd))))
      (pcase cmd-type
        ('M
         (let ((m-xy (edraw-path-cmd-arg-xy cmd 0))
               (zs (edraw-path-cmd-Zs-from-M cmd)))
           (if (null zs)
               ;; Cannot split at end of open path
               nil
             ;; Split all subpaths
             (dolist (z zs)
               ;; Insert L if necessary.
               (unless (edraw-path-cmd-closing-segment-p (edraw-path-cmd-prev z))
                 (edraw-path-cmd-insert-before
                  z
                  (edraw-path-cmd 'L m-xy)))
               ;; Replace Z to M
               (edraw-path-cmd-overwrite-from-ppoints
                cmd
                'M
                (edraw-path-point 'anchor cmd 0 m-xy))
               ;; Remove Z(now M) if before next M
               (when (or (edraw-path-cmd-is-M (edraw-path-cmd-next z))
                         (null (edraw-path-cmd-next z)))
                 (edraw-path-cmd-remove z)))
             t)))

        ((or 'L 'C)
         (cond
          ;; closing segment (next cmd is Z and same coordinates as M)
          ((edraw-path-cmd-closing-segment-p cmd)
           ;; Do not split. Instead, split the corresponding M.
           nil)

          ;; end of open path
          ((or (null next-cmd)
               (eq next-cmd-type '-forward-handle-point)
               (eq next-cmd-type 'M))
           ;; Cannot split
           nil)

          ;; in the middle of path
          ;; or before end of closed path (not closing segment)
          (t
           ;; Insert M
           (let* ((end-of-subpath
                   (edraw-path-cmd-end-of-subpath cmd))
                  (prev-m (edraw-path-cmd-prev-M cmd))
                  (zs (edraw-path-cmd-Zs-from-M prev-m))
                  (new-begin (edraw-path-cmd-insert-after
                              cmd
                              (edraw-path-cmd 'M (edraw-path-point-xy anchor)))))

             ;; If subpath is closed, connect closing point Z to M
             (when (edraw-path-cmd-is-Z end-of-subpath)
               ;; Move the new M to Z range before prev-m
               (edraw-path-cmd-remove-range new-begin end-of-subpath)
               (edraw-path-cmd-insert-range-before prev-m new-begin end-of-subpath)
               ;; make sure closing segment
               (unless (edraw-path-cmd-closing-segment-p
                        (edraw-path-cmd-prev end-of-subpath)
                        prev-m) ;;Need m-cmd-hint because end-of-subpath has been moved and a new M has been inserted before end-of-subpath
                 (edraw-path-cmd-insert-before
                  end-of-subpath
                  (edraw-path-cmd-from-ppoints
                   'L
                   (if (cdr zs)
                       (edraw-path-cmd-arg-xy prev-m 0)
                     (edraw-path-cmd-arg-pt prev-m 0))))) ;;Move anchor from prev-m if remove M
               ;; Remove M
               ;; (However, if multiple subpaths refer to M, keep M)
               (unless (cdr zs) ;;(= (length zs) 1)
                 (edraw-path-cmd-remove prev-m))
               ;; Remove Z
               (edraw-path-cmd-remove end-of-subpath)))
           t)))))))
;; TEST: (let ((cmdlist (edraw-path-cmdlist-from-d "M1,2L3,4 5,6"))) (edraw-path-anchor-split-path (nth 1 (edraw-path-cmdlist-anchor-points cmdlist))) (edraw-path-cmdlist-to-string cmdlist)) => "M1,2L3,4M3,4L5,6"
;; TEST: (let ((cmdlist (edraw-path-cmdlist-from-d "M1,2L3,4 5,6Z"))) (edraw-path-anchor-split-path (nth 1 (edraw-path-cmdlist-anchor-points cmdlist))) (edraw-path-cmdlist-to-string cmdlist)) => "M3,4L5,6L1,2L3,4"
;; TEST: (let ((cmdlist (edraw-path-cmdlist-from-d "M1,2L3,4 5,6Z"))) (edraw-path-anchor-split-path (nth 2 (edraw-path-cmdlist-anchor-points cmdlist))) (edraw-path-cmdlist-to-string cmdlist)) => "M5,6L1,2L3,4L5,6"
;; TEST: (let ((cmdlist (edraw-path-cmdlist-from-d "M1,2L3,4 5,6Z"))) (edraw-path-anchor-split-path (nth 0 (edraw-path-cmdlist-anchor-points cmdlist))) (edraw-path-cmdlist-to-string cmdlist)) => "M1,2L3,4L5,6L1,2"
;; TEST: (let ((cmdlist (edraw-path-cmdlist-from-d "M1,2L3,4 5,6 1,2Z"))) (edraw-path-anchor-split-path (nth 1 (edraw-path-cmdlist-anchor-points cmdlist))) (edraw-path-cmdlist-to-string cmdlist)) => "M3,4L5,6L1,2L3,4"


;;;;;; point - Handle Point

(defun edraw-path-handle-forward-p (handle-point)
  "Return t, if HANDLE-POINT is a control point of the next segment of the corresponding anchor point."
  (and (edraw-path-point-handle-p handle-point) ;;C, -fhp
       (edraw-path-cmd-is-type-p
        (edraw-path-point-cmd handle-point)
        'C '-forward-handle-point) ;;Unnecessary. Handle points can only held on C or -fhp
       (= (edraw-path-point-arg-index handle-point) 0)))

(defun edraw-path-handle-parent-anchor (handle-point)
  (when (edraw-path-point-handle-p handle-point)
    (if (edraw-path-handle-forward-p handle-point)
        (edraw-path-point-prev-anchor handle-point)
      (edraw-path-point-next-anchor handle-point))))

(defun edraw-path-handle-another-handle (handle-point)
  "Return another handle point that shares the same anchor point.
If it doesn't exist, return nil.

If HANDLE-POINT is the forward handle, return the backward handle.
If HANDLE-POINT is the backward handle, return the forward handle."
  (when (edraw-path-point-handle-p handle-point)
    (when-let ((anchor-point (edraw-path-handle-parent-anchor handle-point)))
      (if (edraw-path-handle-forward-p handle-point)
          (edraw-path-anchor-backward-handle anchor-point)
        (edraw-path-anchor-forward-handle anchor-point)))))

(defun edraw-path-handle-move-with-opposite-handle (handle-point new-xy)
  "Move HANDLE-POINT and the point exactly 180 degrees opposite of the anchor point."
  (when-let ((handle1 handle-point)
             (anchor (edraw-path-handle-parent-anchor handle1))
             (handle2 (if (edraw-path-handle-forward-p handle1)
                          (edraw-path-anchor-backward-handle anchor)
                        (edraw-path-anchor-forward-handle anchor))))
    (let* ((handle1-xy (edraw-path-point-xy handle1))
           (anchor-xy (edraw-path-point-xy anchor))
           (handle2-xy (edraw-path-point-xy handle2))
           (va1 (edraw-xy-sub handle1-xy anchor-xy))
           (va2 (edraw-xy-sub handle2-xy anchor-xy))
           (va1-len (edraw-xy-length va1))
           (va2-len (edraw-xy-length va2)))
      (when (or (< va1-len 1e-6)
                (and
                 (> va2-len 1e-6)
                 (< (edraw-xy-dot va1 va2) 0)
                 (< (abs (/ (edraw-xy-perpdot va1 va2)
                            (* va1-len va2-len)))
                    1e-4))) ;;(/ (* 180 (asin 1e-4)) pi) = 0.0057 degrees
        (let* ((van (edraw-xy-sub new-xy anchor-xy))
               (van-len (edraw-xy-length van)))
          (when (> van-len 1e-6)
            (edraw-path-point-move
             handle2
             (edraw-xy-add
              anchor-xy
              (edraw-xy-nmul (/ (- va2-len) van-len) van))))))))

  (edraw-path-point-move handle-point new-xy))

(defun edraw-path-handle-move-with-opposite-handle-symmetry (handle-point new-xy include-same-position-p)
  "Move HANDLE-POINT and opposite handle point."
  (when handle-point
    ;; opposite handle (symmetry)
    (when-let ((anchor (edraw-path-handle-parent-anchor handle-point))
               (opposite-handle
                (if (edraw-path-handle-forward-p handle-point)
                    (edraw-path-anchor-backward-handle anchor include-same-position-p)
                  (edraw-path-anchor-forward-handle anchor include-same-position-p))))
      (edraw-path-point-move
       opposite-handle
       (edraw-xy-sub (edraw-xy-nmul 2 (edraw-path-point-xy anchor))
                     new-xy)))
    ;; target handle
    (edraw-path-point-move handle-point new-xy)))



;;;; Path Geometry

(defun edraw-path-cmdlist-to-segment-list (cmdlist needs-closed-p)
  "Convert CMDLIST to segment list.

segment is straight line or bezier curve line.

straight line: [(x0 . y0) (x1 . y1)]

bezier curve line: [(x0 . y0) (x1 . y1) (x2 . y2) (x3 . y3)]
"
  (let ((initial-point nil) ;;last M point
        (current-point nil)
        segments)
    (cl-flet* ((push-segment (&rest points)
                             ;; Exclude length=0
                             (unless (edraw-xy-list-equal-all-p points)
                               (push (apply #'vector points) segments)))
               (close-path ()
                           (push-segment current-point initial-point)
                           (setq current-point initial-point)))
      (edraw-path-cmdlist-loop cmdlist cmd
        (pcase (edraw-path-cmd-type cmd)
          ('M
           (when (and needs-closed-p (not (equal current-point initial-point)))
             (close-path))
           (setq initial-point (edraw-path-cmd-arg-xy cmd 0)
                 current-point (edraw-path-cmd-arg-xy cmd 0)))
          ('L
           (when current-point
             (push-segment current-point
                           (edraw-path-cmd-arg-xy cmd 0))
             (setq current-point (edraw-path-cmd-arg-xy cmd 0))))
          ('C
           (when current-point
             (push-segment current-point
                           (edraw-path-cmd-arg-xy cmd 0)
                           (edraw-path-cmd-arg-xy cmd 1)
                           (edraw-path-cmd-arg-xy cmd 2))
             (setq current-point (edraw-path-cmd-arg-xy cmd 2))))
          ('Z
           (close-path))))
      (when (and needs-closed-p (not (equal current-point initial-point)))
        (close-path))
      (nreverse segments))))
;; TEST: (edraw-path-cmdlist-to-segment-list (edraw-path-cmdlist-from-d "M10,20 L30,40 L10,20 Z C20,0 80,0 100,20") nil) => ([(10 . 20) (30 . 40)] [(30 . 40) (10 . 20)] [(10 . 20) (20 . 0) (80 . 0) (100 . 20)])

;;;;; Path and Rectangle Intersection Test

(defun edraw-path-cmdlist-intersects-rect-p (cmdlist rect)
  (edraw-bezier-segments-intersects-rect-p
   (edraw-path-cmdlist-to-segment-list cmdlist nil)
   rect))

(defun edraw-bezier-segments-intersects-rect-p (segment-list rect)
  (cl-loop for seg in segment-list
           when (edraw-bezier-segment-intersects-rect-p seg rect)
           return t))

(defun edraw-bezier-segment-intersects-rect-p (seg rect)
  (if (= (length seg) 2)
      (edraw-straight-line-intersects-rect-p
       (elt seg 0) (elt seg 1) rect)
    (let* ((aabb (edraw-bezier-segment-rough-aabb seg))
           (aabb-left (caar aabb))
           (aabb-top (cdar aabb))
           (aabb-right (cadr aabb))
           (aabb-bottom (cddr aabb))
           (rect-left (caar rect))
           (rect-top (cdar rect))
           (rect-right (cadr rect))
           (rect-bottom (cddr rect)))
      (cond
       ((or (> aabb-left rect-right)
            (> aabb-top rect-bottom)
            (< aabb-right rect-left)
            (< aabb-bottom rect-top))
        nil) ;; NG: Square(pt,r) does not intersect AABB

       ((and (> aabb-left rect-left)
             (< aabb-right rect-right)
             (> aabb-top rect-top)
             (< aabb-bottom rect-bottom))
        t) ;; OK: Square(pt,r) contains AABB

       ((edraw-bezier-segment-straight-p seg) ;; SEG is a straight line
        (edraw-straight-line-intersects-rect-p
         (elt seg 0) (elt seg 3) rect))

       (t
        ;; Divide SEG
        (let ((seg2 (edraw-bezier-segment-divide seg)))
          (or (edraw-bezier-segment-intersects-rect-p (car seg2) rect)
              (edraw-bezier-segment-intersects-rect-p (cdr seg2) rect))))))))

(defun edraw-straight-line-intersects-rect-p (p0 p3 rect)
  (let* ((p0x (car p0))
         (p0y (cdr p0))
         (p3x (car p3))
         (p3y (cdr p3))
         (vx (- p3x p0x))
         (vy (- p3y p0y))
         (rect-left (caar rect))
         (rect-top (cdar rect))
         (rect-right (cadr rect))
         (rect-bottom (cddr rect))
         (tmin 0.0)
         (tmax 1.0)
         (epsilon 1e-6))
    (and
     ;; x
     (if (< (abs vx) epsilon)
         (and (>= p0x rect-left) (<= p0x rect-right))
       (let* ((t-left (/ (- rect-left p0x) (float vx)))
              (t-right (/ (- rect-right p0x) (float vx)))
              (t1 (min t-left t-right))
              (t2 (max t-left t-right)))
         (setq tmin (max tmin t1))
         (setq tmax (min tmax t2))
         (<= tmin tmax)))
     ;; y
     (if (< (abs vy) epsilon)
         (and (>= p0y rect-top) (<= p0y rect-bottom))
       (let* ((t-top (/ (- rect-top p0y) (float vy)))
              (t-bottom (/ (- rect-bottom p0y) (float vy)))
              (t1 (min t-top t-bottom))
              (t2 (max t-top t-bottom)))
         (setq tmin (max tmin t1))
         (setq tmax (min tmax t2))
         (<= tmin tmax))))))
;; TEST: (edraw-straight-line-intersects-rect-p '(10 . 100) '(20 . 110) '((15 . 100) . (25 . 105))) => t

;;;;; Point in Path Test

(defun edraw-path-cmdlist-contains-point-p
    (cmdlist pt &optional evenodd-p)
  (let ((count (edraw-bezier-segments-intersect-left-horizontal-half-line
                (edraw-path-cmdlist-to-segment-list cmdlist t)
                pt)))
    (/= 0 (if evenodd-p (mod count 2) count))))

(defun edraw-bezier-segments-contains-point-p
    (segment-list pt &optional evenodd-p)
  (let ((count (edraw-bezier-segments-intersect-left-horizontal-half-line
                segment-list pt)))
    (/= 0 (if evenodd-p (mod count 2) count))))

(defun edraw-bezier-segments-intersect-left-horizontal-half-line
    (segment-list pt)
  (cl-loop for seg in segment-list
           sum (edraw-bezier-segment-intersects-left-horizontal-half-line seg pt)))

(defun edraw-bezier-segment-intersects-left-horizontal-half-line (seg pt)
  (if (= (length seg) 2)
      (edraw-straight-line-intersects-left-horizontal-half-line
       (elt seg 0) (elt seg 1) pt)
    (let* ((aabb (edraw-bezier-segment-rough-aabb seg))
           (aabb-left (caar aabb))
           (aabb-top (cdar aabb))
           (aabb-right (cadr aabb))
           (aabb-bottom (cddr aabb))
           (pt-x (car pt))
           (pt-y (cdr pt)))
      (cond
       ;; PT is outside AABB

       ((> aabb-left pt-x) 0) ;;NG: AABB is right side of PT
       ((> aabb-top pt-y) 0) ;;NG: AABB is under PT
       ((< aabb-bottom pt-y) 0) ;;NG: AABB is above PT
       ((< aabb-right pt-x)
        (let ((p0y (cdr (elt seg 0)))
              (p3y (cdr (elt seg 3))))
          (if (eq (< p0y pt-y) (< p3y pt-y))
              0 ;;NG: AABB is left side of PT and P0-P3 line does not intersect PT-Y h-line (P0-P1-P2-P3 line and PT-Y h-line may intersect. but intersection count is 0 or even number)
            ;; SEG intersects the horizontal half line to the left of PT
            (if (< p0y p3y) 1 -1)))) ;;OK

       ;; PT is inside AABB

       ((and (< (- aabb-right aabb-left) 1)
             (< (- aabb-bottom aabb-top) 1)) ;; AABB is small
        0) ;;NG: PT is very close to the border. Either OK or NG is fine.
       ((edraw-bezier-segment-straight-p seg) ;; SEG is a straight line
        (edraw-straight-line-intersects-left-horizontal-half-line
         (elt seg 0) (elt seg 3) pt))
       (t
        ;;divide SEG
        (let ((seg2 (edraw-bezier-segment-divide seg)))
          (+ (edraw-bezier-segment-intersects-left-horizontal-half-line (car seg2) pt)
             (edraw-bezier-segment-intersects-left-horizontal-half-line (cdr seg2) pt))))))))
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(17 . 6) (9 . 12) (26 . 36) (7 . 21)] '(18 . 23)) => 0
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(17 . 6) (9 . 12) (26 . 36) (7 . 21)] '(11 . 17)) => 0
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(17 . 6) (9 . 12) (26 . 36) (7 . 21)] '(12 . 22)) => -1
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(24 . 7) (2 . 80) (1 . 1) (28 . 34)] '(12 . 31)) => -1
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(24 . 7) (2 . 80) (1 . 1) (28 . 34)] '(32 . 31)) => 1
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(24 . 7) (2 . 80) (1 . 1) (28 . 34)] '(25 . 29)) => 1
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(24 . 7) (2 . 80) (1 . 1) (28 . 34)] '(20 . 37)) => 0
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(3 . 22) (12 . 14) (17 . 10) (22 . 6)] '(17 . 14)) => -1
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(3 . 22) (12 . 14) (17 . 10) (22 . 6)] '(12 . 11)) => 0
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(14 . 8) (0 . 28) (32 . 28) (14 . 8)] '(15 . 18)) => 1
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(14 . 8) (0 . 28) (32 . 28) (14 . 8)] '(18 . 11)) => 0
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(13 . 14) (22 . 29) (22 . 1) (13 . 14)] '(16 . 12)) => 1
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(13 . 14) (22 . 29) (22 . 1) (13 . 14)] '(16 . 14)) => 1
;; TEST: (edraw-bezier-segment-intersects-left-horizontal-half-line [(13 . 14) (22 . 29) (22 . 1) (13 . 14)] '(16 . 16)) => 1

(defun edraw-straight-line-intersects-left-horizontal-half-line (p0 p3 pt)
  (let* ((p0x (car p0))
         (p0y (cdr p0))
         (p3x (car p3))
         (p3y (cdr p3))
         (pt-x (car pt))
         (pt-y (cdr pt)))
    ;; straight line p0 to p3 is left side of PT?
    (cond
     ((> (min p0y p3y) pt-y) 0) ;; NG: The straight line is under PT (minimum side is close)
     ((<= (max p0y p3y) pt-y) 0) ;; NG: The straight line is above PT (maximum side is open)
     ((> (min p0x p3x) pt-x) 0) ;; NG: The straight line is right side of PT
     ((= p0y p3y) 0) ;; NG: ignore horizontal line (?)
     ((eq
       (< (edraw-xy-perpdot
           (edraw-xy-sub p3 p0)
           (edraw-xy-sub pt p0)) 0) ;; pt is left of vector p0 to p3 ?
       (< p3y p0y)) ;; vector p0 to p3 is up dir?
      0) ;;NG: The straight line is right side of PT
     (t
      (if (< p0y p3y) 1 -1)))))
;; TEST: (edraw-straight-line-intersects-left-horizontal-half-line '(10 . 10) '(10 . 20) '(20 . 20)) => 0
;; TEST: (edraw-straight-line-intersects-left-horizontal-half-line '(10 . 20) '(10 . 30) '(20 . 20)) => 1

;;;;; Bezier Segment

(defun edraw-bezier-segment-straight-p (seg)
  (let* ((p0 (elt seg 0))
         (p1 (elt seg 1))
         (p2 (elt seg 2))
         (p3 (elt seg 3))
         (v03 (edraw-xy-sub p3 p0))
         (v01 (edraw-xy-sub p1 p0))
         (v32 (edraw-xy-sub p2 p3))
         (d03 (edraw-xy-length v03))
         (flatness 0.5)
         (flatness-sq (* flatness flatness))
         (allowable-perpdot (* flatness d03))
         (allowable-dot (* 0.5 d03)))
    (cond
     ((< d03 0.0001) (and (< (edraw-xy-length-squared v01) flatness-sq)
                          (< (edraw-xy-length-squared v32) flatness-sq)))
     ((> (abs (edraw-xy-perpdot v01 v03)) allowable-perpdot) nil) ;;p1 is far from v03
     ((> (abs (edraw-xy-perpdot v32 v03)) allowable-perpdot) nil) ;;p2 is far from v03
     ((not (< (- allowable-dot)
              (edraw-xy-dot v03 v01)
              (+ (* d03 d03) allowable-dot))) nil) ;;p1 is before p0 or after p3
     ((not (< (- allowable-dot)
              (- (edraw-xy-dot v03 v32))
              (+ (* d03 d03) allowable-dot))) nil) ;;p2 is before p0 or after p3
     (t t))))
;; TEST: (edraw-bezier-segment-straight-p [(10 . 10) (12 . 10) (18 . 10) (20 . 10)]) => t

(defun edraw-bezier-segment-rough-aabb (seg)
  "Return the axis-aligned bounding box of SEG."
  (let ((x0 (car (elt seg 0)))
        (x1 (car (elt seg 1)))
        (x2 (car (elt seg 2)))
        (x3 (car (elt seg 3)))
        (y0 (cdr (elt seg 0)))
        (y1 (cdr (elt seg 1)))
        (y2 (cdr (elt seg 2)))
        (y3 (cdr (elt seg 3))))
    (cons
     (cons (min x0 x1 x2 x3)
           (min y0 y1 y2 y3))
     (cons (max x0 x1 x2 x3)
           (max y0 y1 y2 y3)))))

(defun edraw-bezier-segment-divide (seg)
  "Divide the SEG into two at the midpoint."
  (let* ((p0 (elt seg 0))
         (p1 (elt seg 1))
         (p2 (elt seg 2))
         (p3 (elt seg 3))
         (q0 (edraw-xy-midpoint-float p0 p1))
         (q1 (edraw-xy-midpoint-float p1 p2))
         (q2 (edraw-xy-midpoint-float p2 p3))
         (r0 (edraw-xy-midpoint-float q0 q1))
         (r1 (edraw-xy-midpoint-float q1 q2))
         (s (edraw-xy-midpoint-float r0 r1)))
    (cons
     (vector p0 q0 r0 s)
     (vector s r1 q2 p3))))

(defun edraw-bezier-segment-transform (seg mat)
  (dotimes (i (length seg))
    (let ((xy (aref seg i)))
      (edraw-matrix-mul-mat-xy mat xy xy))))

(defun edraw-segment-list-transform (segments mat)
  (dolist (seg segments)
    (edraw-bezier-segment-transform seg mat)))




;;;; SVG Path Data Parser

;; Path Data Syntax
;; https://www.w3.org/TR/SVG11/paths.html#PathDataBNF

(defconst edraw-path-d-number
  "\\(?:[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\)")
(defconst edraw-path-d-wsp "\\(?:[ \t\n\f\r]+\\)")
(defconst edraw-path-d-wsp-opt "[ \t\n\f\r]*")
(defconst edraw-path-d-comma-wsp "\\(?:[ \t\n\f\r]+,?[ \t\n\f\r]*\\|,[ \t\n\f\r]*\\)")
(defconst edraw-path-d-command
  (concat
   edraw-path-d-wsp-opt
   "\\([A-Z]\\)" ;; (1) command type
   "\\(?:" edraw-path-d-wsp-opt
   "\\(" edraw-path-d-number ;;(2) command arguments
   "\\(?:" edraw-path-d-comma-wsp edraw-path-d-number "\\)*\\)" "\\)?"
   edraw-path-d-wsp "?"))

(defun edraw-path-d-parse (d)
  "Return (type . list of number)"
  (let ((pos 0)
        commands)
    (while (string-match edraw-path-d-command d pos)
      (when (/= (match-beginning 0) pos)
        (error "path data parsing error at %s" (substring d pos)))
      (setq pos (match-end 0))
      (let* ((type (intern (match-string 1 d)))
             (numbers-str (match-string 2 d))
             (numbers (if numbers-str
                          (mapcar #'string-to-number
                                  (split-string numbers-str
                                                edraw-path-d-comma-wsp)))))
        (push (cons type numbers) commands)))
    (nreverse commands)))
;; TEST: (edraw-path-d-parse "Z M 10 20.1 L .1 2e+1 20e1 -5e-1") => '((Z) (M 10 20.1) (L 0.1 20.0 200.0 -0.5))
;; TEST: (edraw-path-d-parse "ZM10 20.1L.1 2e+1 20e1 -5e-1") => '((Z) (M 10 20.1) (L 0.1 20.0 200.0 -0.5))

(defun edraw-path-d-from-command-list (command-list)
  (mapconcat (lambda (command)
               (mapconcat (lambda (arg) (format "%s" arg)) command " "))
             command-list
             " "))
;; TEST: (edraw-path-d-from-command-list '((Z) (M 10 20.1) (L 0.1 20.0 200.0 -0.5)))

(defun edraw-path-d-translate (d xy)
  (let ((x (car xy))
        (y (cdr xy)))

    (edraw-path-d-from-command-list
     (cl-loop for cmd in (edraw-path-d-parse d)
              collect (let ((type (car cmd))
                            (args (cdr cmd)))
                        (cons
                         type
                         (pcase type
                           ((or 'M 'L 'C 'S 'Q 'T)
                            (seq-map-indexed (lambda (n idx)
                                               (+ n (if (= (% idx 2) 0) x y)))
                                             args))
                           ;;(('H))
                           ;;(('V))
                           ;;(('A))
                           (_ args))))))))
;; TEST: (edraw-path-d-translate '"M 10 20 L 30 40 50 60" '(100 . 200))


(provide 'edraw-path)
;;; edraw-path.el ends here
