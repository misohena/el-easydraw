;;; edraw-path.el --- Things related to Bezier path -*- lexical-binding: t; -*-

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

(require 'seq)
(require 'cl-lib)
(require 'edraw-math)

;;;; Path Structure

;; Classes:
;; - `edraw-path-data' contains zero or more subpaths.
;; - `edraw-path-subpath' contains zero or more anchors and a closed state.
;; - `edraw-path-anchor' contains two handles and an xy.
;; - `edraw-path-handle' contains an xy.

;; String Conversion:
;; - `edraw-path-data-from-d'
;; - `edraw-path-data-to-string'


;; The structure of SVG path data (command sequence) is not suitable
;; for editing for the following reasons.
;;
;; - One M may be shared by multiple subpaths.
;; - When connecting the last segment of a closed path with a curve,
;;   the coordinates of the starting point will be duplicated.
;; - Absolute coordinates and relative coordinates may be mixed.
;; - Cubic Bezier and quadratic Bezier expressions may be mixed.
;; - Vertical and horizontal lines are represented separately from
;;   straight lines.
;; - The first control point of the C command is information that
;;   should be associated with the previous point.
;; - The next control point after end points cannot be retained.
;;
;; Therefore, it is necessary to convert the SVG path data into a
;; structure more suitable for editing. Previously, the structure
;; obtained by merely parsing the SVG path data (called cmdlist) was
;; directly edited, but this approach involved numerous conditional
;; branches and became exceedingly complex, making continued
;; development difficult.


;;;;; Extra Properties

;;
;; Each object has a slot to hold extra properties.
;;

(defmacro edraw-path--extra-props (obj) `(aref ,obj 1)) ;; 1: Extra Properties

(defun edraw-path-extra-props-get (obj key)
  (plist-get (edraw-path--extra-props obj) key))

(defun edraw-path-extra-props-set (obj key value)
  (setf (plist-get (edraw-path--extra-props obj) key) value))


;;;;; Intrusive Doubly Linked List

;;
;; The list of anchors and subpaths is made of an intrusive doubly linked list.
;;

;; Note: The slot index numbers of the `edraw-path-data',
;; `edraw-path-subpath', and `edraw-path-anchor' objects must match. Make
;; sure that the index numbers of the previous and next links in the
;; subpath list and anchor list match.
;;   2: Previous Subpath (Used by edraw-path-data, edraw-path-subpath)
;;   3: Next Subpath     (Used by edraw-path-data, edraw-path-subpath)
;;   4: Previous Anchor  (Used by edraw-path-subpath, edraw-path-anchor)
;;   5: Next Anchor      (Used by edraw-path-subpath, edraw-path-anchor)
(defmacro edraw-path--list-subpaths-prev (obj) `(aref ,obj 2)) ;; 2:
(defmacro edraw-path--list-subpaths-next (obj) `(aref ,obj 3)) ;; 3:
(defmacro edraw-path--list-anchors-prev (obj) `(aref ,obj 4))  ;; 4:
(defmacro edraw-path--list-anchors-next (obj) `(aref ,obj 5))  ;; 5:

(defmacro edraw-path-link-loop (container
                                element-var
                                first-fun-symbol
                                end-fun-symbol
                                next-fun-symbol
                                &rest body)
  (declare (indent 5))
  (let ((container-var (gensym))
        (it-var (gensym))
        (end-var (gensym)))
    `(let* ((,container-var ,container)
            (,it-var (,first-fun-symbol ,container-var))
            (,end-var (,end-fun-symbol ,container-var))
            ,element-var)
       (while (not (eq ,it-var ,end-var))
         (setq ,element-var ,it-var)
         ,@body
         (setq ,it-var (,next-fun-symbol ,it-var))))))


;;;;; Path Data

;;
;; Path data is a collection of zero or more subpaths.
;;

;;;;;; Construction

(defmacro edraw-path-data--extra-props (data) ;; 1:
  `(edraw-path--extra-props ,data))
(defmacro edraw-path-data--subpath-last (data) ;; 2:
  `(edraw-path--list-subpaths-prev ,data))
(defmacro edraw-path-data--subpath-first (data) ;; 3:
  `(edraw-path--list-subpaths-next ,data))

(defun edraw-path-data ()
  "Create a path data object."
  (let ((data
         (record 'edraw-path-data
                 nil ;; 1: Extra Properties
                 nil ;; 2: Last Subpath (Same location as `edraw-path-subpath')
                 nil ;; 3: First Subpath (Same location as `edraw-path-subpath')
                 )))
    ;; Initialize linked list
    (setf (edraw-path-data--subpath-last data) data)
    (setf (edraw-path-data--subpath-first data) data)
    data))

(defsubst edraw-path-data-p (obj)
  "Return non-nil if OBJ is a path data object."
  (and (recordp obj) (eq (aref obj 0) 'edraw-path-data)))

;;;;;; Subpaths in Path Data

(defmacro edraw-path-data-subpath-sentinel (data)
  "Return the sentinel of the subpath list in DATA.
A sentinel is a non-subpath object that represents the boundary of
a subpath at the beginning or end of a subpath list."
  data)

(defsubst edraw-path-data-subpath-empty-p (data)
  "Return non-nil if DATA has no subpaths.

This function returns whether DATA has any subpath objects. Use
`edraw-path-data-no-anchor-p' or `edraw-path-data-has-anchor-p'
to check if it contains anchor points."
  (eq (edraw-path-data--subpath-first data)
      (edraw-path-data-subpath-sentinel data)))

(defun edraw-path-data-first-or-nil (data)
  "Return the first subpath of the DATA, or nil if it is empty."
  (unless (edraw-path-data-subpath-empty-p data)
    (edraw-path-data--subpath-first data)))

(defun edraw-path-data-last-or-nil (data)
  "Return the last subpath of the DATA, or nil if it is empty."
  (unless (edraw-path-data-subpath-empty-p data)
    (edraw-path-data--subpath-last data)))

(defun edraw-path-data-last-or-create (data)
  "Return the last subpath of the DATA. If doesn't exist, create a
new subpath and return it."
  (or (edraw-path-data-last-or-nil data)
      (edraw-path-data-add-new-subpath data)))

(defun edraw-path-data-subpath-clear (data)
  "Remove all subpaths from the DATA."
  (unless (edraw-path-data-subpath-empty-p data)
    (edraw-path-subpath-remove-range
     (edraw-path-data--subpath-first data)
     (edraw-path-data--subpath-last data))))

(defun edraw-path-data-subpath-swap (data1 data2)
  "Exchange the subpaths of DATA1 and DATA2.
Return DATA1."
  (let ((data2-first (edraw-path-data-first-or-nil data2)))
    ;; Move DATA1 to beginning of DATA2
    (unless (edraw-path-data-subpath-empty-p data1)
      (edraw-path-subpath-insert-range-after
       (edraw-path-data-subpath-sentinel data2)
       (edraw-path-data--subpath-first data1)
       (edraw-path-data--subpath-last data1)))
    ;; Move Previous DATA2 to DATA1
    (when data2-first
      (edraw-path-subpath-insert-range-after
       (edraw-path-data-subpath-sentinel data1)
       data2-first
       (edraw-path-data--subpath-last data2)))
    data1))
;; TEST: (let ((data1 (edraw-path-data-from-d "M1 2 3 4"))) (edraw-path-data-subpath-swap data1 data1) (edraw-path-data-to-string data1)) => "M1 2 3 4"
;; TEST: (let ((data1 (edraw-path-data)) (data2 (edraw-path-data))) (edraw-path-data-subpath-swap data1 data2) (concat (edraw-path-data-to-string data1) "/" (edraw-path-data-to-string data2))) => "/"
;; TEST: (let ((data1 (edraw-path-data-from-d "M1 2 3 4")) (data2 (edraw-path-data))) (edraw-path-data-subpath-swap data1 data2) (concat (edraw-path-data-to-string data1) "/" (edraw-path-data-to-string data2))) => "/M1 2 3 4"
;; TEST: (let ((data1 (edraw-path-data)) (data2 (edraw-path-data-from-d "M5 6 7 8"))) (edraw-path-data-subpath-swap data1 data2) (concat (edraw-path-data-to-string data1) "/" (edraw-path-data-to-string data2))) => "M5 6 7 8/"
;; TEST: (let ((data1 (edraw-path-data-from-d "M1 2 3 4")) (data2 (edraw-path-data-from-d "M5 6 7 8"))) (edraw-path-data-subpath-swap data1 data2) (concat (edraw-path-data-to-string data1) "/" (edraw-path-data-to-string data2))) => "M5 6 7 8/M1 2 3 4"

(defun edraw-path-data-insert-data-first (data first-data)
  "Insert (move) all subpaths in another path data (FIRST-DATA)
 at the beginning of the DATA.
FIRST-DATA and DATA must be different objects.
After insertion, FIRST-DATA will be empty.
Return the DATA."
  (when (and (not (eq data first-data))
             (not (edraw-path-data-subpath-empty-p first-data)))
    (edraw-path-data-insert-subpaths-first
     data
     (edraw-path-data--subpath-first first-data)
     (edraw-path-data--subpath-last first-data)))
  data)
;; TEST: (edraw-path-data-to-string (edraw-path-data-insert-data-first (edraw-path-data-from-d "M1 2L3 4") (edraw-path-data-from-d "M10 11L12 13"))) => "M10 11 12 13M1 2 3 4"

(defun edraw-path-data-insert-data-last (data last-data)
  "Insert (move) all subpaths in another data (LAST-DATA)
 at the end of the DATA.
LAST-DATA and DATA must be different objects.
After insertion, LAST-DATA will be empty.
Return the DATA."
  (when (and (not (eq data last-data))
             (not (edraw-path-data-subpath-empty-p last-data)))
    (edraw-path-data-insert-subpaths-last
     data
     (edraw-path-data--subpath-first last-data)
     (edraw-path-data--subpath-last last-data)))
  data)
;; TEST: (edraw-path-data-to-string (edraw-path-data-insert-data-last (edraw-path-data-from-d "M1 2L3 4") (edraw-path-data-from-d "M10 11L12 13"))) => "M1 2 3 4M10 11 12 13"

(defun edraw-path-data-insert-subpath-first (data subpath)
  "Insert the SUBPATH at the beginning of the path DATA.
Return the SUBPATH."
  (edraw-path-subpath-insert-after ;; Return SUBPATH
   (edraw-path-data-subpath-sentinel data)
   subpath))

(defun edraw-path-data-insert-subpath-last (data subpath)
  "Insert the SUBPATH at the end of the path DATA.
Return the SUBPATH."
  (edraw-path-subpath-insert-before ;; Return SUBPATH
   (edraw-path-data-subpath-sentinel data)
   subpath))

(defun edraw-path-data-insert-subpaths-first (data first-subpath last-subpath)
  "Insert the list of subpaths from FIRST-SUBPATH to LAST-SUBPATH
 at the beginning of the path DATA."
  (edraw-path-subpath-insert-range-after
   (edraw-path-data-subpath-sentinel data)
   first-subpath last-subpath))

(defun edraw-path-data-insert-subpaths-last (data first-subpath last-subpath)
  "Insert the list of subpaths from FIRST-SUBPATH to LAST-SUBPATH
 at the end of the path DATA."
  (edraw-path-subpath-insert-range-before
   (edraw-path-data-subpath-sentinel data)
   first-subpath last-subpath))

(defun edraw-path-data-add-new-subpath (data)
  "Add a new, empty subpath to the path data.
Return the added subpath.
The new subpath is inserted at the end of the list.
 `edraw-path-data-last' also returns the newly added subpath."
  (edraw-path-data-insert-subpath-last ;; Return the new subpath
   data
   (edraw-path-subpath)))


(defmacro edraw-path-data-subpath-loop (data subpath-var &rest body)
  (declare (indent 2))
  `(edraw-path-link-loop ,data ,subpath-var
                         edraw-path-data--subpath-first
                         edraw-path-data-subpath-sentinel
                         edraw-path-subpath-next
     ,@body))

(defmacro edraw-path-data-subpath-rloop (data subpath-var &rest body)
  (declare (indent 2))
  `(edraw-path-link-loop ,data ,subpath-var
                         edraw-path-data--subpath-last
                         edraw-path-data-subpath-sentinel
                         edraw-path-subpath-prev
     ,@body))

(defun edraw-path-data-multiple-subpaths-p (data)
  "Return non-nil if DATA contains two or more subpaths."
  (let ((subpath (edraw-path-data--subpath-first data))
        (sentinel (edraw-path-data-subpath-sentinel data)))
    (and (not (eq subpath sentinel))
         (not (eq (edraw-path-subpath-next subpath) sentinel)))))
;; TEST: (edraw-path-data-multiple-subpaths-p (edraw-path-data-from-d "")) => nil
;; TEST: (edraw-path-data-multiple-subpaths-p (edraw-path-data-from-d "M1 2 3 4")) => nil
;; TEST: (edraw-path-data-multiple-subpaths-p (edraw-path-data-from-d "M1 2 3 4M5 6 7 8")) => t

(defun edraw-path-data-subpath-count (data)
  "Return the number of subpaths in DATA."
  (cl-loop for count from 0
           for subpath = (edraw-path-data--subpath-first data)
           then (edraw-path-subpath-next subpath)
           with end = (edraw-path-data-subpath-sentinel data)
           until (eq subpath end)
           finally return count))
;; TEST: (edraw-path-data-subpath-count (edraw-path-data-from-d "M1 2 3 4 Z L 5 6 Z L 7 8 M 9 10")) => 4

;;;;;; Anchors in Path Data

(defun edraw-path-data-has-anchor-p (data)
  "Return non-nil if the path data has one or more anchor points."
  (cl-loop for subpath = (edraw-path-data--subpath-first data)
           then (edraw-path-subpath-next subpath)
           with end = (edraw-path-data-subpath-sentinel data)
           until (eq subpath end)
           unless (edraw-path-subpath-empty-p subpath) ;; subpath is not empty
           return t))

(defun edraw-path-data-no-anchor-p (data)
  "Return non-nil if the path data has no anchor points."
  (not (edraw-path-data-has-anchor-p data)))

(defun edraw-path-data-anchor-count (data)
  "Return the number of anchors in DATA."
  (let ((result 0))
    (edraw-path-data-subpath-loop data subpath
      (cl-incf result (edraw-path-subpath-anchor-count subpath)))
    result))
;; TEST: (edraw-path-data-anchor-count (edraw-path-data-from-d "M1 2 3 4 Z L 5 6 Z L 7 8 M 9 10")) => 7

(defun edraw-path-data-anchor-nth (data index)
  "Return the INDEX-th anchor in DATA. If INDEX is out of range, returns nil."
  (edraw-path-anchor-next-nth data index))

(defun edraw-path-data-add-new-anchor (data
                                       &optional xy
                                       b-handle-xy-rel
                                       f-handle-xy-rel)
  "Add a new anchor to the last subpath of the path DATA.
If the path DATA does not have any subpaths, create one and add it there."
  (edraw-path-subpath-add-new-anchor
   (edraw-path-data-last-or-create data)
   xy b-handle-xy-rel f-handle-xy-rel))

(defun edraw-path-data-add-anchor (data anchor)
  "Add the ANCHOR to the last subpath of the path DATA.
If the path DATA does not have any subpaths, create one and add it there."
  (edraw-path-subpath-insert-anchor-last
   (edraw-path-data-last-or-create data)
   anchor))


;;;;; Subpath

;;
;; A subpath is a set of zero or more anchors that represent a line
;; that can be drawn in one stroke.
;;
;; A subpath is either an open path or a closed path. A closed path is
;; connected by a line from the last anchor to the first anchor.
;;

;;;;;; Construction

(defmacro edraw-path-subpath--extra-props (subpath) ;; 1:
  `(edraw-path--extra-props ,subpath))
(defmacro edraw-path-subpath--prev (subpath) ;; 2:
  `(edraw-path--list-subpaths-prev ,subpath))
(defmacro edraw-path-subpath--next (subpath) ;; 3:
  `(edraw-path--list-subpaths-next ,subpath))
(defmacro edraw-path-subpath--anchor-last (subpath) ;; 4:
  `(edraw-path--list-anchors-prev ,subpath))
(defmacro edraw-path-subpath--anchor-first (subpath) ;; 5:
  `(edraw-path--list-anchors-next ,subpath))
(defmacro edraw-path-subpath--closed-p (subpath) `(aref ,subpath 6))

(defun edraw-path-subpath (&optional closed)
  "Create a subpath object."
  (let ((subpath
         (record 'edraw-path-subpath
                 nil ;; 1: Extra Properties
                 nil ;; 2: Previous Subpath (Same location as `edraw-path-data')
                 nil ;; 3: Next Subpath (Same location as `edraw-path-data')
                 nil ;; 4: First anchor (Same location as `edraw-path-anchor')
                 nil ;; 5: Last anchor (Same location as `edraw-path-anchor')
                 closed))) ;; 6: Closed?
    ;; Initialize linked list
    (setf (edraw-path-subpath--anchor-last subpath) subpath)
    (setf (edraw-path-subpath--anchor-first subpath) subpath)
    subpath))

(defsubst edraw-path-subpath-p (obj)
  "Return non-nil if OBJ is a subpath object."
  (and (recordp obj) (eq (aref obj 0) 'edraw-path-subpath)))

;;;;;; Closed Path State

(defun edraw-path-subpath-closed-p (subpath)
  "Return non-nil if SUBPATH is a closed path, or nil if it is an
 open path.
A \"closed path\" is a seamless path where the last anchor is
connected to the first anchor by a segment."
  (edraw-path-subpath--closed-p subpath))

(defun edraw-path-subpath-open-p (subpath)
  "Return non-nil if SUBPATH is a open path.
Return the opposite of `edraw-path-subpath-closed-p'."
  (not (edraw-path-subpath-closed-p subpath)))

(defun edraw-path-subpath-close (subpath)
  "Makes SUBPATH a closed path."
  (setf (edraw-path-subpath--closed-p subpath) t))

(defun edraw-path-subpath-open (subpath)
  "Makes SUBPATH an open path."
  (setf (edraw-path-subpath--closed-p subpath) nil))

;;;;;; Subpath List (Self-operations)

(defsubst edraw-path-subpath-sentinel-p (obj)
  "Return non-nil if OBJ is a subpath sentinel. Return t even if OBJ is nil."
  (not (edraw-path-subpath-p obj)))

(defsubst edraw-path-subpath-sentinel-to-data (obj)
  "If OBJ is a subpath sentinel, return the path data that contains OBJ."
  (when (edraw-path-data-p obj)
    obj))

(defun edraw-path-subpath-parent-data (subpath)
  "Return the path data that holds the SUBPATH."
  (while (and subpath (not (edraw-path-subpath-sentinel-p subpath)))
    (setq subpath (edraw-path-subpath-prev subpath)))
  (edraw-path-subpath-sentinel-to-data subpath))

(defun edraw-path-subpath-next (subpath)
  "Return the next subpath of SUBPATH.
If the next subpath does not exist, this function returns the
value that `edraw-path-data-subpath-sentinel' would return. To check if
it is a valid subpath, compare it with the value, or use
`edraw-path-subpath-sentinel-p'."
  (edraw-path-subpath--next subpath))

(defun edraw-path-subpath-prev (subpath)
  "Return the previous subpath of SUBPATH.
If the previous subpath does not exist, this function returns the
value that `edraw-path-data-subpath-sentinel' would return. To check if
it is a valid subpath, compare it with the value, or use
`edraw-path-subpath-sentinel-p'."
  (edraw-path-subpath--prev subpath))

(defun edraw-path-subpath-insert-before (this-subpath new-subpath)
  "Insert NEW-SUBPATH before THIS-SUBPATH."
  (edraw-path-subpath-insert-range-before this-subpath new-subpath new-subpath)
  new-subpath)

(defun edraw-path-subpath-insert-after (this-subpath new-subpath)
  "Insert NEW-SUBPATH after THIS-SUBPATH."
  (edraw-path-subpath-insert-range-after this-subpath new-subpath new-subpath)
  new-subpath)

(defun edraw-path-subpath-remove (this-subpath)
  "Remove THIS-SUBPATH from the subpath list (path data).
The previous and next links for THIS-SUBPATH will be nil."
  (edraw-path-subpath-remove-range this-subpath this-subpath)
  this-subpath)

(defun edraw-path-subpath-remove-range (first last)
  "Remove subpaths FIRST through LAST from the subpath list (path data).
The previous subpath of FIRST and the next subpath of LAST will be nil."
  (when (and first last)
    (let ((prev (edraw-path-subpath--prev first))
          (next (edraw-path-subpath--next last)))
      (when prev
        (setf (edraw-path-subpath--next prev) next)
        (setf (edraw-path-subpath--prev first) nil))
      (when next
        (setf (edraw-path-subpath--prev next) prev)
        (setf (edraw-path-subpath--next last) nil)))))

(defun edraw-path-subpath-insert-range-after (subpath first last)
  "Insert subpaths FIRST through LAST after SUBPATH."
  (when (and subpath first last)
    (edraw-path-subpath-remove-range first last)
    (when-let ((next (edraw-path-subpath-next subpath)))
      (setf (edraw-path-subpath--prev next) last)
      (setf (edraw-path-subpath--next last) next))
    (setf (edraw-path-subpath--next subpath) first)
    (setf (edraw-path-subpath--prev first) subpath)))

(defun edraw-path-subpath-insert-range-before (subpath first last)
  "Insert subpaths FIRST through LAST before SUBPATH."
  (when (and subpath first last)
    (edraw-path-subpath-remove-range first last)
    (when-let ((prev (edraw-path-subpath-prev subpath)))
      (setf (edraw-path-subpath--next prev) first)
      (setf (edraw-path-subpath--prev first) prev))
    (setf (edraw-path-subpath--prev subpath) last)
    (setf (edraw-path-subpath--next last) subpath)))

;;;;;; Anchors List in Subpath

(defmacro edraw-path-subpath-anchor-loop (subpath anchor-var &rest body)
  (declare (indent 2))
  `(edraw-path-link-loop ,subpath ,anchor-var
                         edraw-path-subpath--anchor-first
                         edraw-path-subpath-anchor-sentinel
                         edraw-path-anchor-next
     ,@body))

(defsubst edraw-path-subpath-anchor-sentinel (subpath)
  "Return the sentinel of the anchor list in SUBPATH.
A sentinel is a non-anchor object that represents the boundary of
an anchor at the beginning or end of an anchor list."
  subpath)

(defun edraw-path-subpath-empty-p (subpath)
  "Return non-nil if SUBPATH has no anchors."
  (eq (edraw-path-subpath--anchor-first subpath)
      (edraw-path-subpath-anchor-sentinel subpath)))

(defun edraw-path-subpath-anchor-first-or-nil (subpath)
  "Return the first anchor of the SUBPATH, or nil if it is empty."
  (unless (edraw-path-subpath-empty-p subpath)
    (edraw-path-subpath--anchor-first subpath)))

(defun edraw-path-subpath-anchor-last-or-nil (subpath)
  "Return the last anchor of the SUBPATH, or nil if it is empty."
  (unless (edraw-path-subpath-empty-p subpath)
    (edraw-path-subpath--anchor-last subpath)))

(defun edraw-path-subpath-anchor-count (subpath)
  "Return the number of anchors in the SUBPATH."
  (cl-loop with end = (edraw-path-subpath-anchor-sentinel subpath)
           for count from 0
           for anchor = (edraw-path-subpath--anchor-first subpath)
           then (edraw-path-anchor-next anchor)
           until (eq anchor end)
           finally return count))

(defun edraw-path-subpath-anchor-nth (subpath index)
  "Return the INDEX-th anchor in SUBPATH.
 If INDEX is out of range, returns nil."
  (edraw-path-anchor-next-nth subpath index))

(defun edraw-path-subpath-anchor-clear (subpath)
  "Remove all anchors from the subpath.
The open/closed state does not change."
  (unless (edraw-path-subpath-empty-p subpath)
    (edraw-path-anchor-remove-range (edraw-path-subpath--anchor-first subpath)
                                    (edraw-path-subpath--anchor-last subpath))))

(defun edraw-path-subpath-insert-subpath-first (subpath first-subpath)
  "Insert (move) all anchors in another subpath (FIRST-SUBPATH)
 at the beginning of the SUBPATH.
FIRST-SUBPATH and SUBPATH must be different objects.
After insertion, FIRST-SUBPATH will be empty. The empty
FIRST-SUBPATH is not automatically removed and remains in the
path data.
Return the SUBPATH."
  (when (and (not (eq subpath first-subpath))
             (not (edraw-path-subpath-empty-p first-subpath)))
    (edraw-path-subpath-insert-anchors-first
     subpath
     (edraw-path-subpath--anchor-first first-subpath)
     (edraw-path-subpath--anchor-last first-subpath)))
  subpath)

(defun edraw-path-subpath-insert-subpath-last (subpath last-subpath)
  "Insert (move) all anchors in another subpath (LAST-SUBPATH)
 at the end of the SUBPATH.
LAST-SUBPATH and SUBPATH must be different objects.
After insertion, LAST-SUBPATH will be empty. The empty
LAST-SUBPATH is not automatically removed and remains in the
path data.
Return the SUBPATH."
  (when (and (not (eq subpath last-subpath))
             (not (edraw-path-subpath-empty-p last-subpath)))
    (edraw-path-subpath-insert-anchors-last
     subpath
     (edraw-path-subpath--anchor-first last-subpath)
     (edraw-path-subpath--anchor-last last-subpath)))
  subpath)

(defun edraw-path-subpath-insert-anchor-first (subpath anchor)
  "Insert the ANCHOR at the beginning of the SUBPATH.
Return the ANCHOR."
  (edraw-path-anchor-insert-after ;; Return ANCHOR
   (edraw-path-subpath-anchor-sentinel subpath)
   anchor))

(defun edraw-path-subpath-insert-anchor-last (subpath anchor)
  "Insert the ANCHOR at the end of the SUBPATH.
Return the ANCHOR."
  (edraw-path-anchor-insert-before ;; Return ANCHOR
   (edraw-path-subpath-anchor-sentinel subpath)
   anchor))

(defun edraw-path-subpath-insert-anchors-first (subpath
                                                first-anchor last-anchor)
  "Insert the list of anchors from FIRST-ANCHOR to LAST-ANCHOR
 at the beginning of the SUBPATH."
  (edraw-path-anchor-insert-range-after
   (edraw-path-subpath-anchor-sentinel subpath)
   first-anchor last-anchor))

(defun edraw-path-subpath-insert-anchors-last (subpath
                                               first-anchor last-anchor)
  "Insert the list of anchors from FIRST-ANCHOR to LAST-ANCHOR
 at the end of the SUBPATH."
  (edraw-path-anchor-insert-range-before
   (edraw-path-subpath-anchor-sentinel subpath)
   first-anchor last-anchor))

(defun edraw-path-subpath-add-new-anchor (subpath
                                          &optional xy
                                          b-handle-xy-rel f-handle-xy-rel)
  "Create an anchor with arguments XY B-HANDLE-XY-REL F-HANDLE-XY-REL and
 append it to the end of SUBPATH.
Returns the added anchor.
 `edraw-path-subpath-anchor-last' also returns the added ANCHOR."
  (edraw-path-anchor-insert-before ;; Return the new anchor
   (edraw-path-subpath-anchor-sentinel subpath)
   (edraw-path-anchor xy b-handle-xy-rel f-handle-xy-rel)))

(defun edraw-path-subpath-curve-to (subpath
                                    last-anchor-forward-handle-xy-abs
                                    new-anchor-backward-handle-xy-abs
                                    new-anchor-xy)
  "Add a curve segment to the end of the SUBPATH."
  ;; the forward handle of the last anchor
  (unless (edraw-path-subpath-empty-p subpath)
    (let ((last-anchor (edraw-path-subpath--anchor-last subpath)))
      (edraw-path-anchor-set-forward-handle-xy
       last-anchor last-anchor-forward-handle-xy-abs)))
  ;; the new anchor and its backward handle
  (edraw-path-subpath-add-new-anchor
   subpath
   new-anchor-xy
   ;; Relative
   (edraw-xy-sub new-anchor-backward-handle-xy-abs new-anchor-xy)))


;;;;; Anchor

;;
;; An anchor is a point that constitutes part of a subpath. The
;; subpath must always pass through the anchor.
;;
;; An anchor has two handles that control the curve of the subpath.
;;

;;;;;; Construction

(defmacro edraw-path-anchor--extra-props (anchor) ;; 1:
  `(edraw-path--extra-props ,anchor))
(defmacro edraw-path-anchor--backward-handle (anchor) `(aref ,anchor 2))
(defmacro edraw-path-anchor--forward-handle (anchor) `(aref ,anchor 3))
(defmacro edraw-path-anchor--prev (anchor)
  `(edraw-path--list-anchors-prev ,anchor)) ;; 4:
(defmacro edraw-path-anchor--next (anchor)
  `(edraw-path--list-anchors-next ,anchor)) ;; 5:
(defmacro edraw-path-anchor--xy (anchor) `(aref ,anchor 6))

(defun edraw-path-anchor (&optional xy b-handle-xy-rel f-handle-xy-rel)
  "Create an anchor object."
  (let ((anchor
         (record
          'edraw-path-anchor
          nil ;; 1: Extra properties
          nil ;; 2: Backward handle
          nil ;; 3: Forward handle
          nil ;; 4: Previous anchor (Same location as `edraw-path-subpath')
          nil ;; 5: Next anchor (Same location as `edraw-path-subpath')
          (if xy (edraw-xy-clone xy) (edraw-xy 0 0))))) ;; 6: XY
    (when b-handle-xy-rel
      (edraw-path-anchor-set-backward-handle-xy-relative anchor
                                                         b-handle-xy-rel))
    (when f-handle-xy-rel
      (edraw-path-anchor-set-forward-handle-xy-relative anchor
                                                        f-handle-xy-rel))
    anchor))

(defsubst edraw-path-anchor-p (obj)
  "Return non-nil if OBJ is an anchor object."
  (and (recordp obj) (eq (aref obj 0) 'edraw-path-anchor)))

(defun edraw-path-anchor-clone (anchor)
  "Duplicate ANCHOR and return it.
Create a new anchor with the same coordinates and handle state as
ANCHOR (new handle objects will also be created).
The new anchor does not yet belong to any subpath."
  (when (edraw-path-anchor-p anchor)
    (let ((new-anchor (edraw-path-anchor (edraw-path-anchor--xy anchor))))
      (when-let ((backward-handle (edraw-path-anchor--backward-handle anchor)))
        (edraw-path-handle-set-xy-relative
         (edraw-path-anchor-backward-handle new-anchor)
         (edraw-path-handle-xy-relative backward-handle)))
      (when-let ((forward-handle (edraw-path-anchor--forward-handle anchor)))
        (edraw-path-handle-set-xy-relative
         (edraw-path-anchor-forward-handle new-anchor)
         (edraw-path-handle-xy-relative forward-handle)))
      new-anchor)))
;; TEST: (let* ((a1 (edraw-path-anchor '(10 . 20) '(-10 . -10) '(20 . 20))) (a2 (edraw-path-anchor-clone a1)) (data (edraw-path-data)) (subpath (edraw-path-data-add-new-subpath data))) (edraw-path-anchor-set-xy a1 '(100 . 200)) (edraw-path-anchor-set-backward-handle-xy a1 '(-200 . -300)) (edraw-path-anchor-set-forward-handle-xy a1 '(200 . 300)) (edraw-path-subpath-insert-anchor-last subpath a1) (edraw-path-subpath-insert-anchor-last subpath a2) (edraw-path-data-to-string data)) => "M100 200C200 300 0 10 10 20"

;;;;;; Anchor Position

(defsubst edraw-path-anchor-xy (anchor)
  "Return the coordinates of the ANCHOR."
  (edraw-path-anchor--xy anchor))

(defun edraw-path-anchor-set-xy (anchor xy)
  "Set the coordinates of ANCHOR to XY.
The handle positions will also change (the relative coordinates
from the anchor will not change)."
  (edraw-xy-assign (edraw-path-anchor--xy anchor) xy))

(defun edraw-path-anchor-transform (anchor matrix)
  "Transform the ANCHOR position with MATRIX.
The handles are also transformed."
  (let* ((b-handle (edraw-path-anchor--backward-handle anchor))
         (f-handle (edraw-path-anchor--forward-handle anchor))
         (b-xy (and b-handle
                    (edraw-path-handle-active-p b-handle)
                    (edraw-path-handle-xy b-handle)))
         (f-xy (and f-handle
                    (edraw-path-handle-active-p f-handle)
                    (edraw-path-handle-xy f-handle)))
         (xy-cell (edraw-path-anchor--xy anchor)))
    ;; Transform ANCHOR
    (edraw-matrix-mul-mat-xy matrix xy-cell xy-cell)

    ;; Transform handles
    (when b-xy
      (edraw-path-handle-set-xy b-handle
                                (edraw-matrix-mul-mat-xy matrix b-xy)))
    (when f-xy
      (edraw-path-handle-set-xy f-handle
                                (edraw-matrix-mul-mat-xy matrix f-xy)))))

;;;;;; Handle Points of Anchor

(defun edraw-path-anchor-handles-symmetrical-p (anchor)
  "Return non-nil if the two handles of the ANCHOR are in
 point-symmetric positions around the ANCHOR."
  (let ((bh (edraw-path-anchor--backward-handle anchor))
        (fh (edraw-path-anchor--forward-handle anchor)))
    (if bh
        (if fh
            (let ((b (edraw-path-handle-xy-relative bh))
                  (f (edraw-path-handle-xy-relative fh)))
              (and (= (- (edraw-x b)) (edraw-x f))
                   (= (- (edraw-y b)) (edraw-y f))))
          nil)
      (if fh
          nil
        t))))

(defun edraw-path-anchor-has-backward-handle (anchor)
  "Return non-nil if ANCHOR has a active backward handle."
  (when-let ((handle (edraw-path-anchor--backward-handle anchor)))
    (edraw-path-handle-active-p handle)))

(defun edraw-path-anchor-has-forward-handle (anchor)
  "Return non-nil if ANCHOR has a active forward handle."
  (when-let ((handle (edraw-path-anchor--forward-handle anchor)))
    (edraw-path-handle-active-p handle)))

(defun edraw-path-anchor-backward-handle (anchor)
  "Return the backward handle of the ANCHOR."
  (or (edraw-path-anchor--backward-handle anchor)
      (setf (edraw-path-anchor--backward-handle anchor)
            (edraw-path-handle anchor))))

(defun edraw-path-anchor-forward-handle (anchor)
  "Return the forward handle of the ANCHOR."
  (or (edraw-path-anchor--forward-handle anchor)
      (setf (edraw-path-anchor--forward-handle anchor)
            (edraw-path-handle anchor))))

(defun edraw-path-anchor-backward-handle-or-nil (anchor)
  "Return the backward handle of the ANCHOR.
If the handle is in an inactive state, return nil."
  (when-let ((handle (edraw-path-anchor--backward-handle anchor)))
    (when (edraw-path-handle-active-p handle)
      handle)))

(defun edraw-path-anchor-forward-handle-or-nil (anchor)
  "Return the forward handle of the ANCHOR.
If the handle is in an inactive state, return nil."
  (when-let ((handle (edraw-path-anchor--forward-handle anchor)))
    (when (edraw-path-handle-active-p handle)
      handle)))

(defun edraw-path-anchor-backward-handle-xy (anchor)
  "Return the coordinates of the backward handle of the ANCHOR."
  (if-let ((handle (edraw-path-anchor--backward-handle anchor)))
      (edraw-path-handle-xy handle)
    (edraw-path-anchor-xy anchor)))

(defun edraw-path-anchor-forward-handle-xy (anchor)
  "Return the coordinates of the forward handle of the ANCHOR."
  (if-let ((handle (edraw-path-anchor--forward-handle anchor)))
      (edraw-path-handle-xy handle)
    (edraw-path-anchor-xy anchor)))

(defun edraw-path-anchor-set-backward-handle-xy (anchor xy)
  "Set the coordinates of the ANCHOR backward handle to XY."
  (edraw-path-handle-set-xy
   (edraw-path-anchor-backward-handle anchor)
   xy))

(defun edraw-path-anchor-set-forward-handle-xy (anchor xy)
  "Set the coordinates of the ANCHOR forward handle to XY."
  (edraw-path-handle-set-xy
   (edraw-path-anchor-forward-handle anchor)
   xy))

(defun edraw-path-anchor-backward-handle-xy-relative (anchor)
  "Return the relative coordinates of the backward handle of the ANCHOR."
  (if-let ((handle (edraw-path-anchor--backward-handle anchor)))
      (edraw-path-handle-xy-relative handle)
    (edraw-xy 0 0)))

(defun edraw-path-anchor-forward-handle-xy-relative (anchor)
  "Return the relative coordinates of the forward handle of the ANCHOR."
  (if-let ((handle (edraw-path-anchor--forward-handle anchor)))
      (edraw-path-handle-xy-relative handle)
    (edraw-xy 0 0)))

(defun edraw-path-anchor-set-backward-handle-xy-relative (anchor xy)
  "Set the relative coordinates of the ANCHOR backward handle to XY."
  (edraw-path-handle-set-xy-relative
   (edraw-path-anchor-backward-handle anchor)
   xy))

(defun edraw-path-anchor-set-forward-handle-xy-relative (anchor xy)
  "Set the relative coordinates of the ANCHOR forward handle to XY."
  (edraw-path-handle-set-xy-relative
   (edraw-path-anchor-forward-handle anchor)
   xy))

(defun edraw-path-anchor-remove-backward-handle (anchor)
  "Remove the backward handle of ANCHOR."
  (when-let ((handle (edraw-path-anchor--backward-handle anchor)))
    (edraw-path-handle-remove handle)))

(defun edraw-path-anchor-remove-forward-handle (anchor)
  "Remove the forward handle of ANCHOR."
  (when-let ((handle (edraw-path-anchor--forward-handle anchor)))
    (edraw-path-handle-remove handle)))

(defun edraw-path-anchor-reverse-handles (anchor)
  "Swap the handles that ANCHOR has.
forward-handle becomes backward-handle, and backward-handle
becomes forward-handle.
The contents of the handle object will not change, only the
anchor reference will change."
  (cl-rotatef (edraw-path-anchor--backward-handle anchor)
              (edraw-path-anchor--forward-handle anchor)))

;;;;;; Anchor List (Self-operations)

(defsubst edraw-path-anchor-sentinel-p (obj)
  "Return non-nil if OBJ is a anchor sentinel. Return t even if OBJ is nil."
  (not (edraw-path-anchor-p obj)))

(defsubst edraw-path-anchor-sentinel-to-subpath (obj)
  "If OBJ is a anchor sentinel, return the subpath that contains the OBJ."
  (when (edraw-path-subpath-p obj)
    obj))

(defun edraw-path-anchor-parent-subpath (anchor)
  "Return the subpath that holds the ANCHOR."
  (while (and anchor (not (edraw-path-anchor-sentinel-p anchor)))
    (setq anchor (edraw-path-anchor-prev anchor)))
  (edraw-path-anchor-sentinel-to-subpath anchor))

(defun edraw-path-anchor-next (anchor)
  "Return the next anchor of the ANCHOR.
If the next anchor does not exist, this function returns the
value that `edraw-path-subpath-anchor-sentinel' would return.
To check if it is a valid anchor, compare it with the value, or
use `edraw-path-anchor-sentinel-p'."
  (edraw-path-anchor--next anchor))

(defun edraw-path-anchor-prev (anchor)
  "Return the previous anchor of the ANCHOR.
If the previous anchor does not exist, this function returns the
value that `edraw-path-subpath-anchor-sentinel' would return.
To check if it is a valid anchor, compare it with the value, or
use `edraw-path-anchor-sentinel-p'."
  (edraw-path-anchor--prev anchor))

(defun edraw-path-anchor-next-or-nil (anchor)
  "Return the next anchor of the ANCHOR.
If the next anchor does not exist, return nil."
  (let ((next (edraw-path-anchor--next anchor)))
    (when (and next (not (edraw-path-anchor-sentinel-p next)))
      next)))

(defun edraw-path-anchor-prev-or-nil (anchor)
  "Return the previous anchor of the ANCHOR.
If the next anchor does not exist, return nil."
  (let ((prev (edraw-path-anchor--next anchor)))
    (when (and prev (not (edraw-path-anchor-sentinel-p prev)))
      prev)))

(defun edraw-path-anchor-next-round (anchor)
  "Return the next anchor of the ANCHOR.
If ANCHOR is in a closed subpath, the anchor after the last anchor
 is the first anchor.
If ANCHOR is in an open path, the anchor after the last anchor is nil."
  (let ((next (edraw-path-anchor--next anchor)))
    (if-let ((subpath (edraw-path-anchor-sentinel-to-subpath next)))
        (if (edraw-path-subpath-closed-p subpath)
            (edraw-path-subpath--anchor-first subpath)
          nil)
      next)))

(defun edraw-path-anchor-prev-round (anchor)
  "Return the previous anchor of the ANCHOR.
If ANCHOR is in a closed subpath, the anchor before the first anchor
 is the last anchor.
If ANCHOR is in an open path, the anchor before the first anchor is nil."
  (let ((prev (edraw-path-anchor--prev anchor)))
    (if-let ((subpath (edraw-path-anchor-sentinel-to-subpath prev)))
        (if (edraw-path-subpath-closed-p subpath)
            (edraw-path-subpath--anchor-last subpath)
          nil)
      prev)))

(defun edraw-path-anchor-first-p (anchor)
  "Return non-nil if ANCHOR is the first."
  (when anchor
    (let ((prev (edraw-path-anchor-prev anchor)))
      (or (null prev)
          (edraw-path-anchor-sentinel-p prev)))))

(defun edraw-path-anchor-last-p (anchor)
  "Return non-nil if ANCHOR is the last."
  (when anchor
    (let ((next (edraw-path-anchor-next anchor)))
      (or (null next)
          (edraw-path-anchor-sentinel-p next)))))

(defun edraw-path-anchor-endpoint-p (anchor)
  "Return non-nil if the ANCHOR is an endpoint of an open subpath.
Return non-nil only if ANCHOR is the first or last anchor on an open path.
If ANCHOR is on a closed path, always return nil."
  (when-let ((subpath (or (edraw-path-anchor-sentinel-to-subpath
                           (edraw-path-anchor--prev anchor))
                          (edraw-path-anchor-sentinel-to-subpath
                           (edraw-path-anchor--next anchor)))))
    (edraw-path-subpath-open-p subpath)))

(defun edraw-path-anchor-first-endpoint-p (anchor)
  "Return non-nil if the ANCHOR is an first endpoint of an open subpath.
If ANCHOR is on a closed path, always return nil."
  (when-let ((subpath (edraw-path-anchor-sentinel-to-subpath
                       (edraw-path-anchor--prev anchor))))
    (edraw-path-subpath-open-p subpath)))

(defun edraw-path-anchor-last-endpoint-p (anchor)
  "Return non-nil if the ANCHOR is an last endpoint of an open subpath.
If ANCHOR is on a closed path, always return nil."
  (when-let ((subpath (edraw-path-anchor-sentinel-to-subpath
                       (edraw-path-anchor--next anchor))))
    (edraw-path-subpath-open-p subpath)))

(defun edraw-path-anchor-insert-before (this-anchor new-anchor)
  "Insert NEW-ANCHOR before THIS-ANCHOR."
  (edraw-path-anchor-insert-range-before this-anchor new-anchor new-anchor)
  new-anchor)

(defun edraw-path-anchor-insert-after (this-anchor new-anchor)
  "Insert NEW-ANCHOR after THIS-ANCHOR."
  (edraw-path-anchor-insert-range-after this-anchor new-anchor new-anchor)
  new-anchor)

(defun edraw-path-anchor-remove (this-anchor)
  "Remove THIS-ANCHOR from the anchor list (subpath).
The previous and next links for THIS-ANCHOR will be nil."
  (edraw-path-anchor-remove-range this-anchor this-anchor)
  this-anchor)

(defun edraw-path-anchor-remove-range (first last)
  "Remove anchors FIRST through LAST from the anchor list (subpath).
The previous anchor of FIRST and the next anchor of LAST will be nil."
  (when (and first last)
    (let ((prev-anchor (edraw-path-anchor--prev first))
          (next-anchor (edraw-path-anchor--next last)))
      (when prev-anchor
        (setf (edraw-path-anchor--next prev-anchor) next-anchor)
        (setf (edraw-path-anchor--prev first) nil))
      (when next-anchor
        (setf (edraw-path-anchor--prev next-anchor) prev-anchor)
        (setf (edraw-path-anchor--next last) nil)))))

(defun edraw-path-anchor-insert-range-after (anchor first last)
  "Insert anchors FIRST through LAST after ANCHOR."
  (when (and anchor first last)
    (edraw-path-anchor-remove-range first last)
    (when-let ((next-anchor (edraw-path-anchor--next anchor)))
      (setf (edraw-path-anchor--prev next-anchor) last)
      (setf (edraw-path-anchor--next last) next-anchor))
    (setf (edraw-path-anchor--next anchor) first)
    (setf (edraw-path-anchor--prev first) anchor)))

(defun edraw-path-anchor-insert-range-before (anchor first last)
  "Insert anchors FIRST through LAST before ANCHOR."
  (when (and anchor first last)
    (edraw-path-anchor-remove-range first last)
    (when-let ((prev-anchor (edraw-path-anchor--prev anchor)))
      (setf (edraw-path-anchor--next prev-anchor) first)
      (setf (edraw-path-anchor--prev first) prev-anchor))
    (setf (edraw-path-anchor--prev anchor) last)
    (setf (edraw-path-anchor--next last) anchor)))

(defun edraw-path-anchor-split (anchor)
  "Split the subpath into two at the ANCHOR location.

Add a new anchor duplicating ANCHOR after ANCHOR, transfer the
range from the new anchor to the last anchor of the subpath to
a new subpath, and insert the new subpath after the subpath
with ANCHOR. The ANCHOR becomes the last anchor of the previous
subpath. The new anchor becomes the first anchor of the
subsequent subpath.

If the subpath being split is a closed path, the contents of the
new subpath are inserted at the beginning of the existing
subpath, and the subpath is turned into an open path.

Return the new anchor."
  (when-let ((subpath (edraw-path-anchor-parent-subpath anchor)))
    (let ((new-anchor (edraw-path-anchor-clone anchor))
          (new-subpath (edraw-path-subpath)))
      ;; Duplicate anchor
      (edraw-path-anchor-insert-after anchor new-anchor)
      ;; Move new-anchor ... last of subpath to new-subpath
      (edraw-path-anchor-insert-range-before
       (edraw-path-subpath-anchor-sentinel new-subpath)
       new-anchor (edraw-path-subpath-anchor-last-or-nil subpath))

      ;; Insert new-subpath
      (if (edraw-path-subpath-open-p subpath)
          (edraw-path-subpath-insert-after subpath new-subpath)
        ;; closed => open
        (edraw-path-subpath-insert-subpath-first subpath new-subpath)
        (edraw-path-subpath-open subpath))
      new-anchor)))

(defun edraw-path-anchor-connect (anchor1 anchor2)
  "Connect ANCHOR1 and ANCHOR2.

ANCHOR1 and ANCHOR2 must both be endpoints (the first or last
anchor of an open subpath).
 ANCHOR1 and ANCHOR2 must be different anchor points.

Transfer the contents of the subpath containing ANCHOR2 to the
subpath containing ANCHOR1.
If necessary, the contents of the subpath containing ANCHOR2 are reversed.

If the subpath containing ANCHOR2 becomes empty as a result of
the transfer, it is removed from the path data, and the path data
containing ANCHOR2 may become empty.

If ANCHOR1 and ANCHOR2 are both ends in the same open subpath,
the subpath is changed to a closed subpath."
  (let* ((prev-subpath-1 (edraw-path-anchor-sentinel-to-subpath
                          (edraw-path-anchor--prev anchor1)))
         (next-subpath-1 (edraw-path-anchor-sentinel-to-subpath
                          (edraw-path-anchor--next anchor1)))
         (subpath1 (or prev-subpath-1 next-subpath-1))
         (prev-subpath-2 (edraw-path-anchor-sentinel-to-subpath
                          (edraw-path-anchor--prev anchor2)))
         (next-subpath-2 (edraw-path-anchor-sentinel-to-subpath
                          (edraw-path-anchor--next anchor2)))
         (subpath2 (or prev-subpath-2 next-subpath-2)))
    (unless (or (eq anchor1 anchor2) ;; Same anchor
                (null subpath1) ;; anchor1 is a middle point
                (null subpath2) ;; anchor2 is a middle point
                (edraw-path-subpath-closed-p subpath1)  ;; closed subpath
                (edraw-path-subpath-closed-p subpath2)) ;; closed subpath
      (if (eq subpath1 subpath2)
          ;; Same subpath
          (edraw-path-subpath-close subpath1)
        ;; Different subpaths
        (if next-subpath-1
            ;; anchor1 is a last endpoint
            (progn
              (when next-subpath-2 ;; anchor2 is a last endpoint
                (edraw-path-subpath-reverse subpath2))
              (edraw-path-subpath-insert-subpath-last subpath1 subpath2))
          ;; anchor1 is a first endpoint
          (unless next-subpath-2 ;; anchor2 is a first endpoint
            (edraw-path-subpath-reverse subpath2))
          (edraw-path-subpath-insert-subpath-first subpath1 subpath2))
        ;; Remove empty subpath2
        (when (edraw-path-subpath-empty-p subpath2)
          (edraw-path-subpath-remove subpath2)))
      t)))

(defun edraw-path-anchor-insert-midpoint-before (anchor)
  "Add a new anchor halfway between the ANCHOR and the previous anchor.
Return the new anchor.

If the ANCHOR is the first anchor of a closed subpath, wrap
around (the previous anchor is taken from the last anchor)."
  (when-let ((anchor-0 (edraw-path-anchor-prev-round anchor)))
    (let ((a0 (edraw-path-anchor-xy anchor-0))
          (a1 (edraw-path-anchor-xy anchor)))
      (if (and (not (edraw-path-anchor-has-forward-handle anchor-0))
               (not (edraw-path-anchor-has-backward-handle anchor)))
          ;; Straight Line
          (let ((new-xy (edraw-xy-midpoint-float a0 a1)))
            (edraw-path-anchor-insert-before anchor (edraw-path-anchor new-xy)))
        ;; Curved Line
        (let* ((seg2 (edraw-path-bezier-seg-divide
                      (vector
                       a0
                       (edraw-path-anchor-forward-handle-xy anchor-0)
                       (edraw-path-anchor-backward-handle-xy anchor)
                       a1)))
               (new-f0 (elt (car seg2) 1))
               (new-b2 (elt (car seg2) 2))
               (new-a2 (elt (car seg2) 3))
               (new-f2 (elt (cdr seg2) 1))
               (new-b1 (elt (cdr seg2) 2)))
          ;; Prev anchor's forward handle
          (edraw-path-anchor-set-forward-handle-xy anchor-0 new-f0)
          ;; Curr anchor's backward handle
          (edraw-path-anchor-set-backward-handle-xy anchor new-b1)
          ;; New anchor
          (edraw-path-anchor-insert-before anchor
                                           (edraw-path-anchor
                                            new-a2
                                            (edraw-xy-sub new-b2 new-a2)
                                            (edraw-xy-sub new-f2 new-a2))))))))


;;;;;; Anchor Index

(defun edraw-path-anchor-index-in-data (anchor)
  "Return the position of ANCHOR within the path data.
If ANCHOR is not in the edraw-path-data object, return nil."
  (let ((index 0)
        (found-data nil))
    (while (progn
             ;; Seek anchor sentinel (parent subpath or nil)
             (while (progn
                      (setq anchor (edraw-path-anchor-prev anchor))
                      (not (edraw-path-anchor-sentinel-p anchor)))
               (cl-incf index))
             ;;
             (when-let* ((subpath ;; nil means not in subpath
                          (edraw-path-anchor-sentinel-to-subpath anchor))
                         (prev-subpath ;; nil means not in data
                          (edraw-path-subpath-prev subpath)))
               (let ((data (edraw-path-subpath-sentinel-to-data prev-subpath)))
                 (if data
                     (progn (setq found-data data) nil)
                   ;; Next
                   (setq anchor
                         (edraw-path-subpath-anchor-sentinel prev-subpath)))))))
    (when found-data
      index)))
;; TEST: (let ((data (edraw-path-data-from-d "M0 1"))) (edraw-path-anchor-index-in-data (edraw-path-subpath-anchor-last-or-nil (edraw-path-data-last-or-nil data)))) => 0
;; TEST: (let ((data (edraw-path-data-from-d "M0 1 2 3"))) (edraw-path-anchor-index-in-data (edraw-path-subpath-anchor-last-or-nil (edraw-path-data-last-or-nil data)))) => 1
;; TEST: (let ((data (edraw-path-data-from-d "M0 1L10 11M20 21 30 31ZL50 51M60 61 70 71"))) (edraw-path-anchor-index-in-data (edraw-path-subpath-anchor-last-or-nil (edraw-path-data-last-or-nil data)))) => 7
;; TEST: (let* ((data (edraw-path-data-from-d "M0 1L10 11M20 21 30 31ZL50 51M60 61 70 71")) (first (edraw-path-data-first-or-nil data)) (last (edraw-path-data-last-or-nil data))) (edraw-path-subpath-remove-range first last) (edraw-path-anchor-index-in-data (edraw-path-subpath-anchor-last-or-nil last))) => nil

(defun edraw-path-anchor-next-nth (anchor-or-subpath-or-data index)
  "Return the INDEX-th anchor from the position specified by
 ANCHOR-OR-SUBPATH-OR-DATA. If INDEX is out of range, returns nil."
  (let ((it anchor-or-subpath-or-data))
    (when (edraw-path-data-p it)
      ;; data to subpath or nil
      (setq it (edraw-path-data-first-or-nil it)))
    (while (progn
             (when (edraw-path-subpath-p it)
               ;; subpath to anchor or anchor-sentinel
               (setq it (edraw-path-subpath--anchor-first it)))
             (while (and (not (edraw-path-anchor-sentinel-p it))
                         (> index 0))
               (setq it (edraw-path-anchor-next it))
               (cl-decf index))
             (when-let ((subpath (edraw-path-anchor-sentinel-to-subpath it)))
               ;; anchor sentinel to next subpath
               (setq it (edraw-path-subpath-next subpath))
               t)))
    (when (edraw-path-anchor-p it)
      it)))
;; TEST: (edraw-path-anchor-xy (edraw-path-anchor-next-nth (edraw-path-data-from-d "M0 1 10 11 20 21M30 31M40 41 50 51ZL70 71") 0)) => (0.0 . 1.0)
;; TEST: (edraw-path-anchor-xy (edraw-path-anchor-next-nth (edraw-path-data-from-d "M0 1 10 11 20 21M30 31M40 41 50 51ZL70 71") 1)) => (10.0 . 11.0)
;; TEST: (edraw-path-anchor-xy (edraw-path-anchor-next-nth (edraw-path-data-from-d "M0 1 10 11 20 21M30 31M40 41 50 51ZL70 71") 3)) => (30.0 . 31.0)
;; TEST: (edraw-path-anchor-xy (edraw-path-anchor-next-nth (edraw-path-data-from-d "M0 1 10 11 20 21M30 31M40 41 50 51ZL70 71") 4)) => (40.0 . 41.0)
;; TEST: (edraw-path-anchor-xy (edraw-path-anchor-next-nth (edraw-path-data-from-d "M0 1 10 11 20 21M30 31M40 41 50 51ZL70 71") 6)) => (40.0 . 41.0)
;; TEST: (edraw-path-anchor-xy (edraw-path-anchor-next-nth (edraw-path-data-from-d "M0 1 10 11 20 21M30 31M40 41 50 51ZL70 71") 7)) => (70.0 . 71.0)
;; TEST: (edraw-path-anchor-next-nth (edraw-path-data-from-d "M0 1 10 11 20 21M30 31M40 41 50 51ZL70 71") 8) => nil

;;;;;; Anchor Change

(defun edraw-path-anchor-make-smooth (anchor)
  "Set the position of the handles appropriately to smooth the anchor point."
  (let* ((curr-xy (edraw-path-anchor-xy anchor))
         (prev-anchor (edraw-path-anchor-prev-round anchor))
         (prev-xy (when prev-anchor (edraw-path-anchor-xy prev-anchor)))
         (next-anchor (edraw-path-anchor-next-round anchor))
         (next-xy (when next-anchor (edraw-path-anchor-xy next-anchor)))
         (pn (edraw-path-anchor-make-smooth-xy curr-xy prev-xy next-xy)))
    ;;(message "make smooth prev=%s curr=%s next=%s" prev-xy curr-xy next-xy)

    (when (car pn)
      (edraw-path-handle-set-xy (edraw-path-anchor-backward-handle anchor)
                                (car pn)))
    (when (cdr pn)
      (edraw-path-handle-set-xy (edraw-path-anchor-forward-handle anchor)
                                (cdr pn)))
    (when (or (car pn) (cdr pn))
      t)))

(defun edraw-path-anchor-make-smooth-xy (curr-xy prev-xy next-xy)
  "Calculate the appropriate position of the handles for the
 anchor to be smooth.
CURR-XY is the position of the target anchor point, and PREV-XY
and NEXT-XY are the positions of the anchor points before and
after it.
Return the cons of the backward handle's position and the forward
handle's position."
  (cond
   ((and (null prev-xy) (null next-xy))
    nil)
   ((null prev-xy) ;;(not (null next-xy))
    (cons nil (edraw-xy-midpoint-float curr-xy next-xy)))
   ((null next-xy) ;;(not (null prev-xy))
    (cons (edraw-xy-midpoint-float curr-xy prev-xy) nil))
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
          (cons hp hn)))))))


;;;;; Handle

;;
;; The handles are the control points of the curve at the anchor points.
;;

;;;;;; Construction

(defmacro edraw-path-handle--extra-props (handle) ;; 1:
  `(edraw-path--extra-props ,handle))
(defmacro edraw-path-handle--parent-anchor (handle) `(aref ,handle 2))
(defmacro edraw-path-handle--xy-relative (handle) `(aref ,handle 3))

(defun edraw-path-handle (parent-anchor &optional xy-relative)
  "Create a handle object."
  (record 'edraw-path-handle
          nil ;; 1: Extra properties
          parent-anchor ;; 2: Parent anchor
          (if xy-relative
              (edraw-xy-clone xy-relative)
            (cons 0 0)))) ;; 3: xy-relative

(defsubst edraw-path-handle-p (obj)
  "Return non-nil if OBJ is a handle object."
  (and (recordp obj) (eq (aref obj 0) 'edraw-path-handle)))

;;;;;; Parent Anchor of Handle

(defun edraw-path-handle-parent (handle)
  "Return the anchor object that owns HANDLE."
  (edraw-path-handle--parent-anchor handle))

(defsubst edraw-path-handle-parent-xy (handle)
  "Return the coordinates of the anchor that owns HANDLE."
  (edraw-path-anchor-xy (edraw-path-handle-parent handle)))

;;;;;; Handle Direction

(defun edraw-path-handle-backward-p (handle)
  "Return non-nil if the HANDLE is a backward handle."
  (eq (edraw-path-anchor--backward-handle
       (edraw-path-handle-parent handle))
      handle))

(defun edraw-path-handle-forward-p (handle)
  "Return non-nil if the HANDLE is a forward handle."
  (eq (edraw-path-anchor--forward-handle
       (edraw-path-handle-parent handle))
      handle))

;;;;;; Opposite Handle

(defun edraw-path-handle-opposite-handle-or-nil (handle)
  "Return the handle on the other side of HANDLE.
Return another handle belonging to the same anchor.
Return nil if the opposite handle is not active."
  (let ((anchor (edraw-path-handle-parent handle)))
    (cond
     ((eq (edraw-path-anchor--backward-handle anchor) handle)
      (edraw-path-anchor-forward-handle-or-nil anchor))
     ((eq (edraw-path-anchor--forward-handle anchor) handle)
      (edraw-path-anchor-backward-handle-or-nil anchor)))))

(defun edraw-path-handle-opposite-handle (handle)
  "Return the handle on the other side of HANDLE.
Return another handle belonging to the same anchor."
  (let ((anchor (edraw-path-handle-parent handle)))
    (cond
     ((eq (edraw-path-anchor--backward-handle anchor) handle)
      (edraw-path-anchor-forward-handle anchor))
     ((eq (edraw-path-anchor--forward-handle anchor) handle)
      (edraw-path-anchor-backward-handle anchor)))))

;;;;;; Handle Status

(defun edraw-path-handle-active-p (handle)
  "Return non-nil if HANDLE has effect.
A handle at the same position as the anchor point has no effect."
  (not (edraw-xy-zero-p (edraw-path-handle--xy-relative handle))))

(defun edraw-path-handle-remove (handle)
  "Remove the HANDLE.
It is not actually removed, but is made inactive.
When it is inactive, `edraw-path-handle-active-p' returns t and
`edraw-path-handle-xy-relative' returns (0 . 0)."
  (edraw-path-handle-set-xy-relative handle (edraw-xy 0 0)))

;;;;;; Handle Position

(defun edraw-path-handle-xy-relative (handle)
  "Return the coordinates of HANDLE as relative coordinates from the anchor."
  (edraw-path-handle--xy-relative handle))

(defun edraw-path-handle-set-xy-relative (handle xy)
  "Set the coordinates of HANDLE to relative coordinates XY from the anchor."
  (edraw-xy-assign (edraw-path-handle--xy-relative handle) xy))

(defun edraw-path-handle-xy (handle)
  "Return the coordinates of the HANDLE."
  (edraw-xy-add
   (edraw-path-anchor-xy (edraw-path-handle-parent handle))
   (edraw-path-handle--xy-relative handle)))

(defun edraw-path-handle-set-xy (handle xy)
  "Set the HANDLE coordinates to XY."
  (edraw-xy-assign
   (edraw-path-handle--xy-relative handle)
   (edraw-xy-sub xy (edraw-path-anchor-xy (edraw-path-handle-parent handle)))))

(defun edraw-path-handle-transform (handle matrix)
  "Transform a HANDLE with a MATRIX.
Transform only the HANDLE. To transform the anchor and the two
handles as a whole, use `edraw-path-anchor-transform'."
  (edraw-path-handle-set-xy
   handle
   (edraw-matrix-mul-mat-xy matrix (edraw-path-handle-xy handle))))

(defun edraw-path-handle-move-with-opposite-handle (handle new-xy)
  "Move HANDLE and its opposite handle.
The handle on the opposite side is moved only when it is exactly
180 degrees opposite across the HANDLE and anchor."
  (when handle
    (when-let ((anchor (edraw-path-handle-parent handle))
               (handle2 (edraw-path-handle-opposite-handle-or-nil handle)))
      (let* ((handle1-xy (edraw-path-handle-xy handle))
             (anchor-xy (edraw-path-anchor-xy anchor))
             (handle2-xy (edraw-path-handle-xy handle2))
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
              (edraw-path-handle-set-xy-relative
               handle2
               (edraw-xy-nmul (/ (- va2-len) van-len) van)))))))

    (edraw-path-handle-set-xy handle new-xy)))

(defun edraw-path-handle-move-with-opposite-handle-symmetry (handle new-xy)
  "Move the HANDLE and its opposite handle symmetrically.
The position of the opposite handle is point symmetrical across
the anchor from NEW-XY."
  (when handle
    ;; opposite handle (symmetry)
    (when-let ((anchor (edraw-path-handle-parent handle))
               ;; Always create opposite handle
               (opposite-handle (edraw-path-handle-opposite-handle handle)))
      (edraw-path-handle-set-xy
       opposite-handle
       (edraw-xy-sub (edraw-xy-nmul 2 (edraw-path-anchor-xy anchor))
                     new-xy)))
    ;; target handle
    (edraw-path-handle-set-xy handle new-xy)))



;;;; Path Shape Processing

;;;;; Path Data Analyzing

(defun edraw-path-data-endpoints (data)
  "Return a list of all endpoints contained in the path DATA.
An endpoint is the first or last anchor of an open subpath. "
  (let (endpoints)
    (edraw-path-data-subpath-loop data subpath
      (when (edraw-path-subpath-open-p subpath)
        ;; Not closed path
        (let ((first (edraw-path-subpath--anchor-first subpath)))
          (unless (eq first (edraw-path-subpath-anchor-sentinel subpath))
            ;; Not empty
            (let ((last (edraw-path-subpath--anchor-last subpath)))
              (if (eq first last)
                  ;; 1 anchor
                  (push first endpoints)
                ;; 2 or more anchors in SUBPATH
                (push first endpoints)
                (push last endpoints)))))))
    (nreverse endpoints)))
;; TEST: (mapcar #'edraw-path-anchor-xy (edraw-path-data-endpoints (edraw-path-data-from-d "M1 2 3 4 5 6M10 11 12 13 14 15Zl16 17 18 19ZL20 21 22 23"))) => ((1.0 . 2.0) (5.0 . 6.0) (10.0 . 11.0) (22.0 . 23.0))


;;;;; Path Data Shape Change

(defun edraw-path-data-transform (data matrix)
  "Transform path DATA with MATRIX."
  (edraw-path-data-subpath-loop data subpath
    (edraw-path-subpath-transform subpath matrix)))

(defun edraw-path-data-reverse (data)
  "Reverse the order of the subpaths and anchors in the path DATA.
Return DATA."
  (let* ((end (edraw-path-data-subpath-sentinel data))
         (prev end)
         (curr (edraw-path-subpath--next prev)))
    (unless (eq curr end)
      (while (let ((next (edraw-path-subpath--next curr)))
               (setf (edraw-path-subpath--prev prev) curr
                     (edraw-path-subpath--next curr) prev)
               (unless (eq curr end)
                 ;; Reverse subpath
                 (edraw-path-subpath-reverse curr)
                 (setq prev curr
                       curr next)
                 t)))))
  data)
;; TEST: (edraw-path-data-to-string (edraw-path-data-reverse (edraw-path-data-from-d "M1 2 3 4 5 6M7 8M9 10 C 11 12 13 14 15 16"))) => "M15 16C13 14 11 12 9 10M7 8M5 6 3 4 1 2"

;;;;; Subpath Shape Change

(defun edraw-path-subpath-transform (subpath matrix)
  "Transform SUBPATH with MATRIX."
  (edraw-path-subpath-anchor-loop subpath anchor
    (edraw-path-anchor-transform anchor matrix)))

(defun edraw-path-subpath-reverse (subpath)
  "Reverse the order of the anchors in the SUBPATH.
Return SUBPATH.
The handles of each anchor are also reversed."
  (let* ((end (edraw-path-subpath-anchor-sentinel subpath))
         (prev end)
         (curr (edraw-path-anchor--next prev)))
    (unless (eq curr end)
      (while (let ((next (edraw-path-anchor--next curr)))
               (setf (edraw-path-anchor--prev prev) curr
                     (edraw-path-anchor--next curr) prev)
               (unless (eq curr end)
                 ;; Reverse backward and forward handles
                 (edraw-path-anchor-reverse-handles curr)
                 (setq prev curr
                       curr next)
                 t)))))
  subpath)
;; TEST: (let ((data (edraw-path-data))) (edraw-path-data-add-new-subpath data) (edraw-path-subpath-reverse (edraw-path-data-first-or-nil data)) (edraw-path-data-to-string data)) => ""
;; TEST: (let ((data (edraw-path-data-from-d "M1 2"))) (edraw-path-subpath-reverse (edraw-path-data-first-or-nil data)) (edraw-path-data-to-string data)) => "M1 2"
;; TEST: (let ((data (edraw-path-data-from-d "M1 2 3 4"))) (edraw-path-subpath-reverse (edraw-path-data-first-or-nil data)) (edraw-path-data-to-string data)) => "M3 4 1 2"
;; TEST: (let ((data (edraw-path-data-from-d "M10 10 C 20 20 30 20 40 10"))) (edraw-path-subpath-reverse (edraw-path-data-first-or-nil data)) (edraw-path-data-to-string data)) => "M40 10C30 20 20 20 10 10"



;;;; Path String Conversion

(defun edraw-path-d-convert (d move-to line-to curve-to close)
  "Convert path data attribute(<path d=D>) to move, line, curve, and close.

  MOVE-TO (xy)
  LINE-TO (xy)
  CURVE-TO (p1 p2 p)
  CLOSE (move-xy)"
  ;; ref: https://www.w3.org/TR/SVG11/paths.html#PathData
  (let ((cmd-list (edraw-path-d-parse d))
        (current-xy (cons 0 0)) ;; Last xy for relative calculation
        moved
        prev-cmd-type ;; Previous cmd type
        shared-move-xy ;; Start point of subpath used if Z is not followed by M
        prev-qt) ;; Previous control point for Q or T
    ;; Check first cmd is M or m
    (when (and cmd-list (not (memq (car (car cmd-list)) '(M m))))
      (error "First command must be M or m"))
    (cl-flet*
        ;; Inner functions
        ((get-args (num cmd-args)
           ;; "Take NUM arguments from CMD-ARGS."
           (let ((args-head (cdr cmd-args))
                 (args-last-cell (nthcdr num cmd-args)))
             (when (null args-last-cell)
               (error "Too short path command argument %s" cmd-args))
             (setcdr cmd-args (cdr args-last-cell))
             (setcdr args-last-cell nil)
             args-head))

         (get-xy (cmd-args &optional update-current-xy relative-p x-or-y)
           ;; "Take 2 (or 1) arguments from CMD-ARGS and return a
           ;; coordinate-pair."
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
             (when update-current-xy
               (setq current-xy xy))
             xy))

         (move-if-after-z ()
           (unless moved
             (funcall move-to shared-move-xy)
             (setq moved t))))
      ;; cmd loop
      (dolist (cmd-args cmd-list)
        (let* ((cmd-type (car cmd-args))
               (cmd-type-char (elt (symbol-name cmd-type) 0))
               (rel-p (<= ?a cmd-type-char ?z))) ;;lowercase-p
          (pcase cmd-type

            ((or 'M 'm)
             (setq shared-move-xy nil
                   moved t)
             (let ((xy (get-xy cmd-args t rel-p)))
               (funcall move-to xy)
               (setq shared-move-xy xy))
             (while (cdr cmd-args) ;; lineto-argument-sequence
               (funcall line-to (get-xy cmd-args t rel-p))))

            ((or 'Z 'z)
             (funcall close shared-move-xy)
             ;; The next subpath that does not start with M starts at
             ;; the position of the last M command.
             (setq current-xy shared-move-xy
                   moved nil))

            ((or 'L 'l)
             (move-if-after-z)
             (while (cdr cmd-args) ;; lineto-argument-sequence
               (funcall line-to (get-xy cmd-args t rel-p))))

            ((or 'H 'h)
             (move-if-after-z)
             (while (cdr cmd-args) ;; horizontal-lineto-argument-sequence
               (funcall line-to (get-xy cmd-args t rel-p 'x))))

            ((or 'V 'v)
             (move-if-after-z)
             (while (cdr cmd-args) ;; vertical-lineto-argument-sequence
               (funcall line-to (get-xy cmd-args t rel-p 'y))))

            ((or 'C 'c)
             (move-if-after-z)
             (while (cdr cmd-args) ;; curveto-argument-sequence
               (let ((p1 (get-xy cmd-args nil rel-p))
                     (p2 (get-xy cmd-args nil rel-p))
                     (p (get-xy cmd-args t rel-p)))
                 (setq prev-qt p2)
                 (funcall curve-to p1 p2 p))))

            ((or 'S 's)
             (move-if-after-z)
             (while (cdr cmd-args) ;; smooth-curveto-argument-sequence
               (let ((p1
                      ;; the reflection of the second control point on
                      ;; the previous command relative to the current point.
                      (if (memq prev-cmd-type '(C c S s))
                          (edraw-xy-sub (edraw-xy-nmul 2 current-xy) prev-qt)
                        current-xy))
                     (p2 (get-xy cmd-args nil rel-p))
                     (p (get-xy cmd-args t rel-p)))
                 (setq prev-qt p2)
                 (funcall curve-to p1 p2 p))))

            ((or 'Q 'q)
             (move-if-after-z)
             (while (cdr cmd-args) ;; quadratic-bezier-curveto-argument-sequence
               (let* ((p0 current-xy)
                      (qp1 (get-xy cmd-args nil rel-p))
                      (p (get-xy cmd-args t rel-p))
                      (cp (edraw-quadratic-bezier-curve-to-cubic p0 qp1 p))
                      (p1 (car cp))
                      (p2 (cdr cp)))
                 (setq prev-qt qp1)
                 (funcall curve-to p1 p2 p))))

            ((or 'T 't)
             (move-if-after-z)
             (while (cdr cmd-args) ;; smooth-quadratic-bezier-curveto-argument-sequence
               (let* ((p0 current-xy)
                      (qp1
                       ;; the reflection of the control point on the
                       ;; previous command relative to the current point.
                       (if (memq prev-cmd-type '(Q q T t))
                           (edraw-xy-sub (edraw-xy-nmul 2 current-xy) prev-qt)
                         current-xy))
                      (p (get-xy cmd-args t rel-p))
                      (cp (edraw-quadratic-bezier-curve-to-cubic p0 qp1 p))
                      (p1 (car cp))
                      (p2 (cdr cp)))
                 (setq prev-qt qp1)
                 (funcall curve-to p1 p2 p))))

            ;;@todo support A command
            ;; ((or 'A 'a) )
            (_ (error "Unsupported path command found: %s in %s" cmd-args d)))
          (setq prev-cmd-type cmd-type))))))

(defun edraw-path-data-from-d (d)
  "Convert path data attribute(<path d=D>) to edraw-path-data object."
  ;; ref: https://www.w3.org/TR/SVG11/paths.html#PathData
  (let ((data (edraw-path-data))
        ;; Current subpath
        subpath)
    (let* ((move-to
            (lambda (xy)
              ;; "Push XY to new subpath."
              (setq subpath (edraw-path-data-add-new-subpath data))
              (edraw-path-subpath-add-new-anchor subpath xy)))
           (push-anchor
            (lambda (xy)
              ;; "Push XY to the end of current subpath."
              (unless subpath
                (message "AssertionError: (not (null subpath))")
                (setq subpath (edraw-path-data-add-new-subpath data)))
              (edraw-path-subpath-add-new-anchor subpath xy)))
           (push-curve
            (lambda (p1 p2 p)
              ;; "Push curve segment to the end of current subpath."
              (funcall push-anchor p) ;; Ensure subpath and previous anchor
              (let* ((curr-anchor (edraw-path-subpath-anchor-last-or-nil subpath))
                     (prev-anchor (edraw-path-anchor-prev curr-anchor)))
                (edraw-path-anchor-set-backward-handle-xy curr-anchor p2)
                (edraw-path-anchor-set-forward-handle-xy prev-anchor p1))))
           (close
            (lambda (shared-move-xy)
              ;; Delete the closing segment.
              ;; If the last point of a closed path is in the same
              ;; position as the first point, delete it.
              (when (and subpath
                         (not (edraw-path-subpath-empty-p subpath)))
                (let ((last-anchor (edraw-path-subpath--anchor-last subpath)))
                  (when (edraw-xy-equal-p (edraw-path-anchor-xy last-anchor)
                                          shared-move-xy)
                    ;; Transfer the backward handle of the last anchor
                    ;; to the backward handle of the first anchor.
                    (when (edraw-path-anchor-has-backward-handle last-anchor)
                      (edraw-path-anchor-set-backward-handle-xy
                       (edraw-path-subpath--anchor-first subpath)
                       (edraw-path-anchor-backward-handle-xy last-anchor)))
                    ;; Delete the last anchor.
                    (edraw-path-anchor-remove last-anchor))))
              ;; Close the subpath
              (when subpath
                (edraw-path-subpath-close subpath)
                (setq subpath nil)))))
      (edraw-path-d-convert d move-to push-anchor push-curve close))
    data))
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "")) => ""
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M 10 10.1 L 20.2 20e1 .3 .3e-1 Z")) => "M10 10.1 20.2 200 0.3 0.03Z"
;; TEST: (let ((edraw-svg-number-format 'edraw-number-to-string)) (edraw-path-data-to-string (edraw-path-data-from-d "m 10 10.1 l 10.2 189.9 -19.9 -199.97 z"))) => "M10 10.1 20.2 200 0.3000000000000007 0.030000000000001137Z"
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M 10 20 h 30 40 -50 v 60 -70")) => "M10 20H40 80 30V80 10"
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M 10 20 H 30 40 -50 V 60 -70")) => "M10 20H30 40-50V60-70"
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M 10 20 C 30 40 50 60 70 85 S 150 160 170 180")) => "M10 20C30 40 50 60 70 85S150 160 170 180"
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M10 20C30 40 50 60 70 85S150 160 170 180")) => "M10 20C30 40 50 60 70 85S150 160 170 180"
;; TEST: (let ((edraw-svg-number-format 'edraw-number-to-string)) (edraw-path-data-to-string (edraw-path-data-from-d "M 65 50 Q 130 85 65 120"))) => "M65 50C108.33333333333334 73.33333333333333 108.33333333333334 96.66666666666667 65 120"
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M 100 100 c 50,50 100,50 150,0")) => "M100 100C150 150 200 150 250 100"
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M100 100C150 150 200 150 250 100C200 50 150 50 100 100Z")) => "M100 100C150 150 200 150 250 100 200 50 150 50 100 100Z"
;; TEST: (= (edraw-path-subpath-anchor-count (edraw-path-data-first-or-nil (edraw-path-data-from-d "M100 100C150 150 200 150 250 100C200 50 150 50 100 100Z"))) 2) => t
;; TEST: (edraw-path-anchor-backward-handle-xy (edraw-path-subpath-anchor-first-or-nil (edraw-path-data-first-or-nil (edraw-path-data-from-d "M100 100C150 150 200 150 250 100C200 50 150 50 100 100Z")))) => (150.0 . 50.0)


(defun edraw-path-data-stringize-make-command (anchor close)
  (if (and (not close) (edraw-path-anchor-first-p anchor))
      ;; First anchor
      (let ((xy (edraw-path-anchor-xy anchor))
            (prev-subpath (edraw-path-subpath-prev
                           (edraw-path-anchor-parent-subpath anchor))))
        ;; M ... Z ... Z ... Z [*]
        (if (and (not (edraw-path-subpath-sentinel-p prev-subpath))
                 (edraw-path-subpath-closed-p prev-subpath)
                 (not (edraw-path-subpath-empty-p prev-subpath))
                 (edraw-xy-equal-p
                  (edraw-path-anchor-xy
                   (edraw-path-subpath-anchor-first-or-nil prev-subpath))
                  xy))
            nil ;; Omit M after Z
          (list 'M xy)))
    (let ((prev-anchor (edraw-path-anchor-prev-round anchor)))
      (if (or (edraw-path-anchor-has-forward-handle prev-anchor)
              (edraw-path-anchor-has-backward-handle anchor))
          ;; Curve
          (if (and
               (not (edraw-path-anchor-first-p prev-anchor))
               (edraw-path-anchor-handles-symmetrical-p prev-anchor))
              (list 'S
                    (edraw-path-anchor-backward-handle-xy anchor)
                    (edraw-path-anchor-xy anchor))
            (list 'C
                  (edraw-path-anchor-forward-handle-xy prev-anchor)
                  (edraw-path-anchor-backward-handle-xy anchor)
                  (edraw-path-anchor-xy anchor)))
        ;; Straight Line
        (let ((prev-xy (edraw-path-anchor-xy prev-anchor))
              (curr-xy (edraw-path-anchor-xy anchor)))
          (cond
           ((= (edraw-y prev-xy) (edraw-y curr-xy))
            (list 'H (edraw-x curr-xy)))
           ((= (edraw-x prev-xy) (edraw-x curr-xy))
            (list 'V (edraw-y curr-xy)))
           (t
            (list 'L curr-xy))))))))

(defun edraw-path-data-stringize-concat-cmd (str cmd last-cmd)
  (when cmd
    ;; Write command
    (if (or (and (memq (car cmd) '(L H V C S)) (eq (car cmd) (car last-cmd)))
            (and (eq (car cmd) 'L) (eq (car last-cmd) 'M)))
        ;; Omit command name
        (setq str (edraw-path-cmdstr-concat-numstrs
                   str ;; ... <number>
                   (edraw-path-cmdstr-xys (cdr cmd))))
      ;; With command name
      (setq str (concat str
                        (symbol-name (car cmd)) ;; ... <CMD>
                        (edraw-path-cmdstr-xys (cdr cmd))))))
  str)

(defun edraw-path-data-to-string (data)
  (let (result)
    (edraw-path-data-subpath-loop data subpath
      (unless (edraw-path-subpath-empty-p subpath) ;; Ignore empty subpath
        (let (last-cmd)
          (edraw-path-subpath-anchor-loop subpath anchor
            (let ((cmd (edraw-path-data-stringize-make-command anchor nil)))
              (setq result (edraw-path-data-stringize-concat-cmd
                            result cmd last-cmd))
              (setq last-cmd cmd)))

          ;; Close
          (when (edraw-path-subpath-closed-p subpath)
            ;; If the subpath is closed by a curve, a closing segment
            ;; is required
            (let ((first-anchor (edraw-path-subpath--anchor-first subpath))
                  (last-anchor (edraw-path-subpath--anchor-last subpath)))
              (when (or (edraw-path-anchor-has-backward-handle first-anchor)
                        (edraw-path-anchor-has-forward-handle last-anchor))
                (let ((cmd (edraw-path-data-stringize-make-command
                            first-anchor t)))
                  (setq result (edraw-path-data-stringize-concat-cmd
                                result cmd last-cmd)))))
            ;; Z command
            (setq result (concat result "Z"))))))
    (or result "")))
;; TEST: (edraw-path-data-to-string (edraw-path-data)) => ""
;; TEST: (edraw-path-data-to-string (edraw-path-subpath-parent-data (edraw-path-data-add-new-subpath (edraw-path-data)))) => ""
;; TEST: (edraw-path-data-to-string (let* ((data (edraw-path-data)) (subpath (edraw-path-data-add-new-subpath data))) (dolist (xy '((1 . -2) (-3 . 4) (5 . 4) (5 . -6))) (edraw-path-subpath-add-new-anchor subpath xy)) data)) => "M1-2-3 4H5V-6"
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M10 10 20 10 30 30 30 40 50 50")) => "M10 10H20L30 30V40L50 50"
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M0,0 L40,-20 L40,20 Z L20,40 L-20,40 Z L-40,20 L-40,-20 Z L0,-40")) => "M0 0 40-20V20ZL20 40H-20ZL-40 20V-20ZV-40"
;; TEST: (edraw-path-data-to-string (edraw-path-data-from-d "M180 180C200 180 220 200 220 220 220 240 200 260 180 260 160 260 140 240 140 220 140 200 160 180 180 180ZC180 160 200 140 220 140 240 140 260 160 260 180 260 200 240 220 220 220")) => "M180 180C200 180 220 200 220 220S200 260 180 260 140 240 140 220 160 180 180 180ZC180 160 200 140 220 140S260 160 260 180 240 220 220 220"


(defun edraw-path-cmdstr-concat-numstrs (left right)
  (if left
      (if (eq (aref right 0) ?-)
          ;; 123-456
          (concat left right)
        ;; 123 456
        (concat left " " right))
    right))

(defun edraw-path-cmdstr-xy (xy)
  (cond
   ((consp xy)
    (edraw-path-cmdstr-concat-numstrs
     (edraw-svg-numstr (edraw-x xy))
     (edraw-svg-numstr (edraw-y xy))))
   ((numberp xy)
    (edraw-svg-numstr xy))
   ((stringp xy)
    xy)))

(defun edraw-path-cmdstr-xys (xys)
  (let (result)
    (dolist (xy xys)
      (setq result
            (edraw-path-cmdstr-concat-numstrs
             result
             (edraw-path-cmdstr-xy xy))))
    result))
;; TEST: (edraw-path-cmdstr-xys '((-1 . -2) (-3 . 4) (5 . -6) (7 . 8))) => "-1-2-3 4 5-6 7 8"
;; TEST: (edraw-path-cmdstr-xys '(1 -2 3)) => "1-2 3"

(defun edraw-path-cmdstr-curve (to-xy
                                to-backward-xy from-forward-xy
                                &optional from-xy from-backward-xy)
  "Create a curve-to command string.

TO-XY: Destination anchor coordinates.
TO-BACKWARD-XY: Backward handle coordinates of the destination anchor.
FROM-XY: Previous anchor coordinates.
FROM-FORWARD-XY: Forward handle coordinates of the previous anchor.
FROM-BACKWARD-XY: Backward handle coordinates of the previous anchor.

Specifying FROM-XY makes it possible to find the difference with
TO-XY, and a relative coordinate command may be created.

Specifying FROM-BACKWARD-XY may create an S or s command."
  (if from-xy
      (let* ((symmetric
              (or (and (null from-forward-xy)
                       (null from-backward-xy))
                  (and from-forward-xy
                       from-backward-xy
                       (edraw-xy-near-p
                        (edraw-xy-sub from-forward-xy from-xy)
                        (edraw-xy-sub from-xy from-backward-xy)
                        1e-6))))
             (str-xys-rel
              (edraw-path-cmdstr-xys
               (if symmetric
                   (list
                    (edraw-xy-sub to-backward-xy from-xy)
                    (edraw-xy-sub to-xy from-xy))
                 (list
                  (edraw-xy-sub from-forward-xy from-xy)
                  (edraw-xy-sub to-backward-xy from-xy)
                  (edraw-xy-sub to-xy from-xy)))))
             (str-xys-abs
              (edraw-path-cmdstr-xys
               (if symmetric
                   (list to-backward-xy to-xy)
                 (list from-forward-xy to-backward-xy to-xy)))))
        (if (< (length str-xys-rel) (length str-xys-abs))
            (concat (if symmetric "s" "c") str-xys-rel)
          (concat (if symmetric "S" "C") str-xys-abs)))
    (concat "C" (edraw-path-cmdstr-xys
                 (list
                  from-forward-xy to-backward-xy to-xy)))))
;; TEST: (edraw-path-cmdstr-curve '(100 . 110) '(70 . 80) '(50 . 60)) => "C50 60 70 80 100 110"
;; TEST: (edraw-path-cmdstr-curve '(-100 . -110) '(-70 . -80) '(-50 . -60)) => "C-50-60-70-80-100-110"
;; TEST: (edraw-path-cmdstr-curve '(-100 . -110) '(-70 . -80) '(-50 . -60) '(-30 . -45)) => "c-20-15-40-35-70-65"
;; TEST: (edraw-path-cmdstr-curve '(-100 . -110) '(-70 . -80) '(-50 . -60) '(-30 . -45) '(-10 . -30)) => "s-40-35-70-65"
;; TEST: (edraw-path-cmdstr-curve '(10 . -10) '(20 . -80) '(-10 . 70) '(-90 . -10)) => "C-10 70 20-80 10-10"

(defun edraw-path-cmdstr-line (to-xy &optional from-xy)
  "Create a line-to command string.

TO-XY: Destination anchor coordinates.
FROM-XY: Previous anchor coordinates.

Specifying FROM-XY makes it possible to calculate the difference
with TO-XY, and may create a relative coordinate command, H or V
command."
  (if from-xy
      (cond
       ;; Horizontal
       ((= (edraw-y to-xy) (edraw-y from-xy))
        (let ((str-x-rel (edraw-svg-numstr (- (edraw-x to-xy)
                                              (edraw-x from-xy))))
              (str-x-abs (edraw-svg-numstr (edraw-x to-xy))))
          (if (< (length str-x-rel) (length str-x-abs))
              (concat "h" str-x-rel)
            (concat "H" str-x-abs))))
       ;; Vertical
       ((= (edraw-x to-xy) (edraw-x from-xy))
        (let ((str-y-rel (edraw-svg-numstr (- (edraw-y to-xy)
                                              (edraw-y from-xy))))
              (str-y-abs (edraw-svg-numstr (edraw-y to-xy))))
          (if (< (length str-y-rel) (length str-y-abs))
              (concat "v" str-y-rel)
            (concat "V" str-y-abs))))
       ;; Move
       (t
        (let ((str-xy-rel (edraw-path-cmdstr-xy (edraw-xy-sub to-xy from-xy)))
              (str-xy-abs (edraw-path-cmdstr-xy to-xy)))
          (if (< (length str-xy-rel) (length str-xy-abs))
              (concat "l" str-xy-rel)
            (concat "L" str-xy-abs)))))
    (concat "L" (edraw-path-cmdstr-xy to-xy))))
;; TEST: (edraw-path-cmdstr-line '(100 . 200)) => "L100 200"
;; TEST: (edraw-path-cmdstr-line '(100 . 200) '(150 . 260)) => "l-50-60"
;; TEST: (edraw-path-cmdstr-line '(100 . 200) '(50 . 60)) => "l50 140"
;; TEST: (edraw-path-cmdstr-line '(100 . 200) '(-50 . 60)) => "L100 200"
;; TEST: (edraw-path-cmdstr-line '(100 . 200) '(120 . 200)) => "H100"
;; TEST: (edraw-path-cmdstr-line '(120 . 200) '(100 . 200)) => "h20"
;; TEST: (edraw-path-cmdstr-line '(100 . 180) '(100 . 200)) => "V180"
;; TEST: (edraw-path-cmdstr-line '(100 . 200) '(100 . 180)) => "v20"

(defun edraw-path-cmdstr-move (xy)
  "Create a move-to command string.

XY: Anchor coordinates."
  (concat
   "M"
   (edraw-path-cmdstr-xy xy)))
;; TEST: (edraw-path-cmdstr-move '(-10 . -20)) => "M-10-20"



;;;; Path Segment

(defvar edraw-path-data-to-seglist--include-empty-p nil)

(defun edraw-path-data-to-seglist (data &optional needs-closed-p)
  "Convert the path DATA into a list of segments.

Return a list of segments.

Each segment is either a straight line or a Bezier curve, represented
in the following formats:

- Straight line: [(x0 . y0) (x1 . y1)]
- Bezier curve: [(x0 . y0) (x1 . y1) (x2 . y2) (x3 . y3)]

If NEEDS-CLOSED-P is non-nil, modify the resulting list of segments to
ensure the path is closed. This means that even if the original path
data is an open path, it will be forcibly converted into a closed path,
which is useful for operations like interior-exterior determination."
  (let (segments)
    (edraw-path-data-subpath-loop data subpath
      (setq segments
            (nconc
             segments
             (edraw-path-subpath-to-seglist subpath needs-closed-p))))
    segments))
;; TEST: (edraw-path-data-to-seglist (edraw-path-data-from-d "M10,20 L30,40 L10,20 Z C20,0 80,0 100,20") nil) => ([(10.0 . 20.0) (30.0 . 40.0)] [(30.0 . 40.0) (10.0 . 20.0)] [(10.0 . 20.0) (20.0 . 0.0) (80.0 . 0.0) (100.0 . 20.0)])
;; TEST: (edraw-path-data-to-seglist (edraw-path-data-from-d "M10,20 30,40 10,20 100,200 100,200 120,220")) => ([(10.0 . 20.0) (30.0 . 40.0)] [(30.0 . 40.0) (10.0 . 20.0)] [(10.0 . 20.0) (100.0 . 200.0)] [(100.0 . 200.0) (120.0 . 220.0)])
;; TEST: (edraw-path-data-to-seglist (edraw-path-data-from-d "M0,0 L40,-20 L40,20 Z L20,40 L-20,40 Z L-40,20 L-40,-20 Z L0,-40")) => ([(0.0 . 0.0) (40.0 . -20.0)] [(40.0 . -20.0) (40.0 . 20.0)] [(40.0 . 20.0) (0.0 . 0.0)] [(0.0 . 0.0) (20.0 . 40.0)] [(20.0 . 40.0) (-20.0 . 40.0)] [(-20.0 . 40.0) (0.0 . 0.0)] [(0.0 . 0.0) (-40.0 . 20.0)] [(-40.0 . 20.0) (-40.0 . -20.0)] [(-40.0 . -20.0) (0.0 . 0.0)] [(0.0 . 0.0) (0.0 . -40.0)])
;; TEST: (edraw-path-data-to-seglist (edraw-path-data-from-d "M180 180C200 180 220 200 220 220 220 240 200 260 180 260 160 260 140 240 140 220 140 200 160 180 180 180ZC180 160 200 140 220 140 240 140 260 160 260 180 260 200 240 220 220 220")) => ([(180.0 . 180.0) (200.0 . 180.0) (220.0 . 200.0) (220.0 . 220.0)] [(220.0 . 220.0) (220.0 . 240.0) (200.0 . 260.0) (180.0 . 260.0)] [(180.0 . 260.0) (160.0 . 260.0) (140.0 . 240.0) (140.0 . 220.0)] [(140.0 . 220.0) (140.0 . 200.0) (160.0 . 180.0) (180.0 . 180.0)] [(180.0 . 180.0) (180.0 . 160.0) (200.0 . 140.0) (220.0 . 140.0)] [(220.0 . 140.0) (240.0 . 140.0) (260.0 . 160.0) (260.0 . 180.0)] [(260.0 . 180.0) (260.0 . 200.0) (240.0 . 220.0) (220.0 . 220.0)])

(defun edraw-path-subpath-to-seglist (subpath &optional needs-closed-p)
  "Convert subpath to segment list."
  (let (segments)
    (unless (edraw-path-subpath-empty-p subpath) ;; Ignore empty subpath
      (let (last-xy)
        (edraw-path-subpath-anchor-loop subpath anchor
          (when last-xy ;;Ignore first anchor
            (let ((anchor-prev (edraw-path-anchor-prev anchor)))
              (cond
               ;; Curve
               ((or
                 (edraw-path-anchor-has-forward-handle anchor-prev)
                 (edraw-path-anchor-has-backward-handle anchor))
                (setq segments
                      (edraw-path-data-to-seglist--push
                       segments
                       last-xy
                       (edraw-path-anchor-forward-handle-xy anchor-prev)
                       (edraw-path-anchor-backward-handle-xy anchor)
                       (edraw-path-anchor-xy anchor))))
               ;; Straight Line
               (t
                (setq segments
                      (edraw-path-data-to-seglist--push
                       segments
                       last-xy
                       (edraw-path-anchor-xy anchor)))))))
          (setq last-xy (edraw-path-anchor-xy anchor)))
        ;; Close
        (let ((first-anchor (edraw-path-subpath--anchor-first subpath))
              (last-anchor (edraw-path-subpath--anchor-last subpath)))
          (when (or (edraw-path-subpath-closed-p subpath)
                    (and needs-closed-p
                         (not (edraw-xy-equal-p
                               (edraw-path-anchor-xy first-anchor)
                               (edraw-path-anchor-xy last-anchor)))))
            ;; If the subpath is closed by a curve, a closing segment
            ;; is required
            (if (or (edraw-path-anchor-has-backward-handle first-anchor)
                    (edraw-path-anchor-has-forward-handle last-anchor))
                (setq segments
                      (edraw-path-data-to-seglist--push
                       segments
                       last-xy
                       (edraw-path-anchor-forward-handle-xy last-anchor)
                       (edraw-path-anchor-backward-handle-xy first-anchor)
                       (edraw-path-anchor-xy first-anchor)))
              (setq segments
                    (edraw-path-data-to-seglist--push
                     segments
                     last-xy
                     (edraw-path-anchor-xy first-anchor))))))))
    (nreverse segments)))

(defun edraw-path-data-to-seglist--push (segments &rest points)
  ;; Exclude length=0
  (when (or edraw-path-data-to-seglist--include-empty-p
            (not (edraw-xy-list-equal-all-p points)))
    (push (apply #'vector
                 (mapcar #'edraw-xy-clone points))
          segments))
  segments)


(defun edraw-path-seglist-from-d (d &optional needs-closed-p)
  "Convert the path data attribute(<path d=D>) to a list of segments."
  (let (segments first-xy last-xy)
    (let* ((line-to
            (lambda (xy)
              (let ((p0 last-xy) (p1 xy))
                (when (or edraw-path-data-to-seglist--include-empty-p
                          (not (edraw-xy-equal-p p0 p1)))
                  (push (vector (edraw-xy-clone p0) (edraw-xy-clone p1))
                        segments)))
              (setq last-xy xy)))
           (curve-to
            (lambda (p1 p2 p3)
              (let ((p0 last-xy))
                (when (or edraw-path-data-to-seglist--include-empty-p
                          (not (and (edraw-xy-equal-p p0 p1)
                                    (edraw-xy-equal-p p0 p2)
                                    (edraw-xy-equal-p p0 p3))))
                  (push (vector (edraw-xy-clone p0) (edraw-xy-clone p1)
                                (edraw-xy-clone p2) (edraw-xy-clone p3))
                        segments)))
              (setq last-xy p3)))
           (close
            (lambda (move-xy)
              (unless (edraw-xy-equal-p move-xy last-xy)
                (funcall line-to move-xy))
              (setq last-xy nil)))
           (close-if-open
            (lambda ()
              (when (and needs-closed-p last-xy)
                (funcall close first-xy))))
           (move-to
            (lambda (xy)
              (funcall close-if-open)
              ;; "Push XY to new subpath."
              (setq first-xy xy
                    last-xy xy))))
      (edraw-path-d-convert d move-to line-to curve-to close)
      (funcall close-if-open))
    (nreverse segments)))
;; TEST: (edraw-path-seglist-from-d "M10,20 L30,40 L10,20 Z C20,0 80,0 100,20") => ([(10.0 . 20.0) (30.0 . 40.0)] [(30.0 . 40.0) (10.0 . 20.0)] [(10.0 . 20.0) (20.0 . 0.0) (80.0 . 0.0) (100.0 . 20.0)])
;; TEST: (edraw-path-seglist-from-d "M10,20 L30,40 L10,20 Z C20,0 80,0 100,20" t) => ([(10.0 . 20.0) (30.0 . 40.0)] [(30.0 . 40.0) (10.0 . 20.0)] [(10.0 . 20.0) (20.0 . 0.0) (80.0 . 0.0) (100.0 . 20.0)] [(100.0 . 20.0) (10.0 . 20.0)])
;; TEST: (edraw-path-seglist-from-d "M10,20 30,40 10,20 100,200 100,200 120,220") => ([(10.0 . 20.0) (30.0 . 40.0)] [(30.0 . 40.0) (10.0 . 20.0)] [(10.0 . 20.0) (100.0 . 200.0)] [(100.0 . 200.0) (120.0 . 220.0)])
;; TEST: (edraw-path-seglist-from-d "M0,0 L40,-20 L40,20 Z L20,40 L-20,40 Z L-40,20 L-40,-20 Z L0,-40") => ([(0.0 . 0.0) (40.0 . -20.0)] [(40.0 . -20.0) (40.0 . 20.0)] [(40.0 . 20.0) (0.0 . 0.0)] [(0.0 . 0.0) (20.0 . 40.0)] [(20.0 . 40.0) (-20.0 . 40.0)] [(-20.0 . 40.0) (0.0 . 0.0)] [(0.0 . 0.0) (-40.0 . 20.0)] [(-40.0 . 20.0) (-40.0 . -20.0)] [(-40.0 . -20.0) (0.0 . 0.0)] [(0.0 . 0.0) (0.0 . -40.0)])
;; TEST: (edraw-path-seglist-from-d "M180 180C200 180 220 200 220 220 220 240 200 260 180 260 160 260 140 240 140 220 140 200 160 180 180 180ZC180 160 200 140 220 140 240 140 260 160 260 180 260 200 240 220 220 220") => ([(180.0 . 180.0) (200.0 . 180.0) (220.0 . 200.0) (220.0 . 220.0)] [(220.0 . 220.0) (220.0 . 240.0) (200.0 . 260.0) (180.0 . 260.0)] [(180.0 . 260.0) (160.0 . 260.0) (140.0 . 240.0) (140.0 . 220.0)] [(140.0 . 220.0) (140.0 . 200.0) (160.0 . 180.0) (180.0 . 180.0)] [(180.0 . 180.0) (180.0 . 160.0) (200.0 . 140.0) (220.0 . 140.0)] [(220.0 . 140.0) (240.0 . 140.0) (260.0 . 160.0) (260.0 . 180.0)] [(260.0 . 180.0) (260.0 . 200.0) (240.0 . 220.0) (220.0 . 220.0)])


;;;;; Path and Rectangle Intersection Test

(defun edraw-path-data-intersects-rect-p (data rect)
  (edraw-path-seglist-intersects-rect-p
   (edraw-path-data-to-seglist data nil)
   rect))

(defun edraw-path-seglist-intersects-rect-p (seglist rect)
  (cl-loop for seg in seglist
           when (edraw-path-seg-intersects-rect-p seg rect)
           return t))

(defun edraw-path-seg-intersects-rect-p (seg rect)
  (if (= (length seg) 2)
      (edraw-path-straight-seg-intersects-rect-p
       (elt seg 0) (elt seg 1) rect)
    (edraw-path-bezier-seg-intersects-rect-p seg rect)))

(defun edraw-path-bezier-seg-intersects-rect-p (seg rect)
  (let* ((aabb (edraw-path-bezier-seg-rough-aabb seg))
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

     ((edraw-path-bezier-seg-straight-p seg) ;; SEG is a straight line
      (edraw-path-straight-seg-intersects-rect-p
       (elt seg 0) (elt seg 3) rect))

     (t
      ;; Divide SEG
      (let ((seg2 (edraw-path-bezier-seg-divide seg)))
        (or (edraw-path-seg-intersects-rect-p (car seg2) rect)
            (edraw-path-seg-intersects-rect-p (cdr seg2) rect)))))))

(defun edraw-path-straight-seg-intersects-rect-p (p0 p3 rect)
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
;; TEST: (edraw-path-straight-seg-intersects-rect-p '(10 . 100) '(20 . 110) '((15 . 100) . (25 . 105))) => t

;;;;; Point in Path Test

(defun edraw-path-data-contains-point-p (data pt &optional evenodd-p)
  (let ((count (edraw-path-seglist-intersect-left-horizontal-half-line
                (edraw-path-data-to-seglist data t)
                pt)))
    (/= 0 (if evenodd-p (mod count 2) count))))

(defun edraw-path-seglist-contains-point-p (seglist pt &optional evenodd-p)
  (let ((count (edraw-path-seglist-intersect-left-horizontal-half-line
                seglist pt)))
    (/= 0 (if evenodd-p (mod count 2) count))))

(defun edraw-path-seglist-intersect-left-horizontal-half-line (seglist pt)
  (cl-loop for seg in seglist
           sum (edraw-path-seg-intersects-left-horizontal-half-line seg pt)))

(defun edraw-path-seg-intersects-left-horizontal-half-line (seg pt)
  (if (= (length seg) 2)
      (edraw-path-straight-seg-intersects-left-horizontal-half-line
       (elt seg 0) (elt seg 1) pt)
    (edraw-path-bezier-seg-intersects-left-horizontal-half-line seg pt)))
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(17 . 6) (9 . 12) (26 . 36) (7 . 21)] '(18 . 23)) => 0
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(17 . 6) (9 . 12) (26 . 36) (7 . 21)] '(11 . 17)) => 0
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(17 . 6) (9 . 12) (26 . 36) (7 . 21)] '(12 . 22)) => -1
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(24 . 7) (2 . 80) (1 . 1) (28 . 34)] '(12 . 31)) => -1
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(24 . 7) (2 . 80) (1 . 1) (28 . 34)] '(32 . 31)) => 1
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(24 . 7) (2 . 80) (1 . 1) (28 . 34)] '(25 . 29)) => 1
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(24 . 7) (2 . 80) (1 . 1) (28 . 34)] '(20 . 37)) => 0
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(3 . 22) (12 . 14) (17 . 10) (22 . 6)] '(17 . 14)) => -1
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(3 . 22) (12 . 14) (17 . 10) (22 . 6)] '(12 . 11)) => 0
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(14 . 8) (0 . 28) (32 . 28) (14 . 8)] '(15 . 18)) => 1
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(14 . 8) (0 . 28) (32 . 28) (14 . 8)] '(18 . 11)) => 0
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(13 . 14) (22 . 29) (22 . 1) (13 . 14)] '(16 . 12)) => 1
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(13 . 14) (22 . 29) (22 . 1) (13 . 14)] '(16 . 14)) => 1
;; TEST: (edraw-path-seg-intersects-left-horizontal-half-line [(13 . 14) (22 . 29) (22 . 1) (13 . 14)] '(16 . 16)) => 1

(defun edraw-path-bezier-seg-intersects-left-horizontal-half-line (seg pt)
  (let* ((aabb (edraw-path-bezier-seg-rough-aabb seg))
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
     ((edraw-path-bezier-seg-straight-p seg) ;; SEG is a straight line
      (edraw-path-straight-seg-intersects-left-horizontal-half-line
       (elt seg 0) (elt seg 3) pt))
     (t
      ;;divide SEG
      (let ((seg2 (edraw-path-bezier-seg-divide seg)))
        (+ (edraw-path-seg-intersects-left-horizontal-half-line (car seg2) pt)
           (edraw-path-seg-intersects-left-horizontal-half-line (cdr seg2) pt)))))))

(defun edraw-path-straight-seg-intersects-left-horizontal-half-line (p0 p3 pt)
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
           (edraw-xy-sub pt p0))
          0) ;; pt is left of vector p0 to p3 ?
       (< p3y p0y)) ;; vector p0 to p3 is up dir?
      0) ;;NG: The straight line is right side of PT
     (t
      (if (< p0y p3y) 1 -1)))))
;; TEST: (edraw-path-straight-seg-intersects-left-horizontal-half-line '(10 . 10) '(10 . 20) '(20 . 20)) => 0
;; TEST: (edraw-path-straight-seg-intersects-left-horizontal-half-line '(10 . 20) '(10 . 30) '(20 . 20)) => 1

;;;;; Line and Path Intersection Test

(defun edraw-path-seglist-and-horizontal-line-intersections (seglist y)
  (cl-loop for seg in seglist
           nconc (edraw-path-seg-and-horizontal-line-intersections seg y)))

(defun edraw-path-seg-and-horizontal-line-intersections (seg y)
  (if (= (length seg) 2)
      (edraw-path-straight-seg-and-horizontal-line-intersections
       (elt seg 0) (elt seg 1) y)
    (edraw-path-bezier-seg-and-horizontal-line-intersections seg y)))

(defun edraw-path-bezier-seg-and-horizontal-line-intersections (seg y)
  (let* ((aabb (edraw-path-bezier-seg-rough-aabb seg))
         (aabb-left (caar aabb))
         (aabb-top (cdar aabb))
         (aabb-right (cadr aabb))
         (aabb-bottom (cddr aabb)))
    (cond
     ((> aabb-top y) nil) ;;NG: AABB is under Y
     ((< aabb-bottom y) nil) ;;NG: AABB is above Y

     ;;@todo Solve cubic equation
     ;; If the range is small enough, treat seg and y as intersections.
     ;; Strictly, they may not intersect, or they may intersect twice
     ;; or three times.
     ((and (< (- aabb-right aabb-left) 1)
           (< (- aabb-bottom aabb-top) 1))
      (list (* 0.5 (+ aabb-left aabb-right))))

     ((edraw-path-bezier-seg-straight-p seg) ;; SEG is a straight line
      (edraw-path-straight-seg-and-horizontal-line-intersections
       (elt seg 0) (elt seg 3) y))
     (t
      ;;divide SEG
      (let ((seg2 (edraw-path-bezier-seg-divide seg)))
        (nconc
         (edraw-path-seg-and-horizontal-line-intersections (car seg2) y)
         (edraw-path-seg-and-horizontal-line-intersections (cdr seg2) y)))))))
;; TEST: (edraw-path-bezier-seg-and-horizontal-line-intersections '((40 . 40) (140 . 60) (60 . 220) (60 . 120)) 200) => nil
;; TEST: (edraw-path-bezier-seg-and-horizontal-line-intersections '((40 . 40) (140 . 60) (60 . 220) (60 . 120)) 140) => (81.94482534642032 61.84016247628084)
;; TEST: (edraw-path-bezier-seg-and-horizontal-line-intersections '((40 . 40) (140 . 60) (60 . 220) (60 . 120)) 120) => (88.41227347611202 60.0)
;; TEST: (edraw-path-bezier-seg-and-horizontal-line-intersections '((40 . 40) (140 . 60) (60 . 220) (60 . 120)) 100) => (90.22539239788199)
;; TEST: (edraw-path-bezier-seg-and-horizontal-line-intersections '((40 . 40) (140 . 60) (60 . 220) (60 . 120)) 60) => (76.22797424514114)
;; TEST: (edraw-path-bezier-seg-and-horizontal-line-intersections '((40 . 40) (140 . 60) (60 . 220) (60 . 120)) 40) => (40.0)
;; TEST: (edraw-path-bezier-seg-and-horizontal-line-intersections '((40 . 40) (140 . 60) (60 . 220) (60 . 120)) 39) => nil
;; TEST: (edraw-path-bezier-seg-and-horizontal-line-intersections '((40 . 120) (40 . 20) (140 . 180) (140 . 80)) 100) => (42.08159975036405 90.0 137.91840024963594)

(defun edraw-path-straight-seg-and-horizontal-line-intersections (p0 p3 y)
  (let* ((p0x (car p0))
         (p0y (cdr p0))
         (p3x (car p3))
         (p3y (cdr p3)))
    ;; straight line p0 to p3 is left side of PT?
    (cond
     ((> (min p0y p3y) y) nil) ;; NG: The straight line is under Y (minimum side is close)
     ((<= (max p0y p3y) y) nil) ;; NG: The straight line is above Y (maximum side is open)
     ((= p0y p3y) nil) ;; NG: ignore horizontal line (?)
     (t
      (let ((d03x (- p3x p0x))
            (d03y (- p3y p0y))
            (d0y (- y p0y)))
        (list (+ p0x (* (/ d03x d03y) d0y))))))))

;;;;; Bounding Box

(defun edraw-path-seg-aabb (seg)
  "Return the exact axis-aligned bounding box of SEG."
  (if (= (length seg) 2)
      (edraw-aabb (elt seg 0) (elt seg 1))
    (edraw-path-bezier-seg-aabb seg)))

(defun edraw-path-bezier-seg-aabb (seg)
  "Return the exact axis-aligned bounding box of SEG."
  (let ((x-min-max (edraw-cubic-bezier-min-max (car (elt seg 0)) (car (elt seg 1)) (car (elt seg 2)) (car (elt seg 3))))
        (y-min-max (edraw-cubic-bezier-min-max (cdr (elt seg 0)) (cdr (elt seg 1)) (cdr (elt seg 2)) (cdr (elt seg 3)))))
    (edraw-rect
     (car x-min-max)
     (car y-min-max)
     (cdr x-min-max)
     (cdr y-min-max))))

(defun edraw-path-seglist-aabb (segments)
  "Return the exact axis-aligned bounding box of SEGMENTS."
  (when segments
    (let (x-min-max
          y-min-max)
      (dolist (seg segments)
        (if (= (length seg) 2)
            (let ((p0 (elt seg 0))
                  (p1 (elt seg 1)))
              (setq x-min-max (edraw-min-max-update x-min-max (car p0)))
              (setq x-min-max (edraw-min-max-update x-min-max (car p1)))
              (setq y-min-max (edraw-min-max-update y-min-max (cdr p0)))
              (setq y-min-max (edraw-min-max-update y-min-max (cdr p1))))
          (let ((p0 (elt seg 0))
                (p1 (elt seg 1))
                (p2 (elt seg 2))
                (p3 (elt seg 3)))
            (setq x-min-max (edraw-cubic-bezier-min-max-update x-min-max (car p0) (car p1) (car p2) (car p3)))
            (setq y-min-max (edraw-cubic-bezier-min-max-update y-min-max (cdr p0) (cdr p1) (cdr p2) (cdr p3))))))
      (edraw-rect
       (car x-min-max)
       (car y-min-max)
       (cdr x-min-max)
       (cdr y-min-max)))))

;;TEST: (edraw-path-seglist-aabb (edraw-path-data-to-seglist (edraw-path-data-from-d "M10,20 L30,40 Z C20,0 80,0 100,20") nil)) => ((10.0 . 5.0) 100.0 . 40.0)

;;;;; Transform

(defun edraw-path-seglist-transform (segments mat)
  (dolist (seg segments)
    (edraw-path-seg-transform seg mat)))

(defun edraw-path-seg-transform (seg mat)
  (dotimes (i (length seg))
    (let ((xy (aref seg i)))
      (edraw-matrix-mul-mat-xy mat xy xy))))

(defun edraw-path-seglist-transform-mat22 (segments mat22)
  (dolist (seg segments)
    (edraw-path-seg-transform-mat22 seg mat22)))

(defun edraw-path-seg-transform-mat22 (seg mat22)
  (let* ((vx (car mat22))
         (vxx (car vx))
         (vxy (cdr vx))
         (vy (cdr mat22))
         (vyx (car vy))
         (vyy (cdr vy)))
    (dotimes (i (length seg))
      (let* ((xy (aref seg i))
             (x (car xy))
             (y (cdr xy))
             (new-x (+ (* x vxx) (* y vyx)))
             (new-y (+ (* x vxy) (* y vyy))))
        (setcar xy new-x)
        (setcdr xy new-y)))))

;;;;; Bezier Segment

(defun edraw-path-bezier-seg-straight-p (seg &optional flatness)
  (let* ((p0 (elt seg 0))
         (p1 (elt seg 1))
         (p2 (elt seg 2))
         (p3 (elt seg 3))
         (v03 (edraw-xy-sub p3 p0))
         (v01 (edraw-xy-sub p1 p0))
         (v32 (edraw-xy-sub p2 p3))
         (d03 (edraw-xy-length v03))
         (flatness (or flatness 0.5))
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
              (+ (* d03 d03) allowable-dot)))
      nil) ;;p1 is before p0 or after p3
     ((not (< (- allowable-dot)
              (- (edraw-xy-dot v03 v32))
              (+ (* d03 d03) allowable-dot)))
      nil) ;;p2 is before p0 or after p3
     (t t))))
;; TEST: (edraw-path-bezier-seg-straight-p [(10 . 10) (12 . 10) (18 . 10) (20 . 10)]) => t
;; TEST: (edraw-path-bezier-seg-straight-p [(10 . 10) (12 . 10.5) (18 . 9.5) (20 . 10)]) => t
;; TEST: (edraw-path-bezier-seg-straight-p [(10 . 10) (12 . 10.51) (18 . 9.5) (20 . 10)]) => nil

(defun edraw-path-bezier-seg-rough-aabb (seg)
  "Return the rough axis-aligned bounding box of SEG."
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

(defun edraw-path-bezier-seg-divide (seg)
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


;;;;; Length

(defun edraw-path-bezier-seg-small-p (seg length)
  "Return non-nil if the cubic bezier SEG is small enough relative to LENGTH."
  (let* ((p0 (elt seg 0))
         (p1 (elt seg 1))
         (p2 (elt seg 2))
         (p3 (elt seg 3))
         (len-sq (* length length)))
    (and
     (< (edraw-xy-distance-squared p0 p1) len-sq)
     (< (edraw-xy-distance-squared p0 p2) len-sq)
     (< (edraw-xy-distance-squared p0 p3) len-sq)
     (< (edraw-xy-distance-squared p1 p2) len-sq)
     (< (edraw-xy-distance-squared p1 p3) len-sq)
     (< (edraw-xy-distance-squared p2 p3) len-sq))))

(defun edraw-path-seg-length (seg epsilon)
  "Return the length of SEG.

EPSILON is the threshold of the allowable error. The smaller it is, the
more accurate the result will be, but the slower it will be. The actual
length error will accumulate during the complex calculation process, so
it will almost always be larger than this value."
  (cond
   ((= (length seg) 2)
    (edraw-xy-distance (elt seg 0) (elt seg 1)))
   ((or (edraw-path-bezier-seg-straight-p seg)
        (edraw-path-bezier-seg-small-p seg epsilon))
    (edraw-xy-distance (elt seg 0) (elt seg 3)))
   (t
    (let ((seg2 (edraw-path-bezier-seg-divide seg)))
      (+ (edraw-path-seg-length (car seg2) epsilon)
         (edraw-path-seg-length (cdr seg2) epsilon))))))

(defun edraw-path-seglist-length (seglist epsilon)
  "Return the length of SEGLIST.

EPSILON is the threshold of the allowable error. The smaller it is, the
more accurate the result will be, but the slower it will be. The actual
length error will accumulate during the complex calculation process, so
it will almost always be larger than this value."
  (cl-loop for seg in seglist
           sum (edraw-path-seg-length seg epsilon)))

;;;;; Iterate on Segments

(defun edraw-path-seg-for-each-intervals (seg interval-length
                                              dist-from-start
                                              epsilon
                                              function)
  "Call FUNCTION at points on line SEG with a constant INTERVAL-LENGTH."
  (cond
   ((= (length seg) 2)
    (edraw-path-straight-seg-for-each-intervals (elt seg 0) (elt seg 1)
                                                interval-length
                                                dist-from-start function))
   ((or (edraw-path-bezier-seg-straight-p seg)
        (edraw-path-bezier-seg-small-p seg epsilon))
    (edraw-path-straight-seg-for-each-intervals (elt seg 0) (elt seg 3)
                                                interval-length
                                                dist-from-start function))
   (t
    (let ((seg2 (edraw-path-bezier-seg-divide seg)))
      (setq dist-from-start
            (edraw-path-seg-for-each-intervals
             (car seg2) interval-length dist-from-start epsilon function))
      (setq dist-from-start
            (edraw-path-seg-for-each-intervals
             (cdr seg2) interval-length dist-from-start epsilon function))
      dist-from-start))))

(defun edraw-path-straight-seg-for-each-intervals (p0 p1
                                                      interval-length
                                                      dist-from-start
                                                      function)
  (if (edraw-xy-equal-p p0 p1)
      ;; Ignore empty segment
      dist-from-start
    (let* ((length (edraw-xy-distance p0 p1))
           (bos dist-from-start)
           (eos (+ dist-from-start length)))
      (cl-loop for interval-index from (ceiling (/ bos interval-length))
               for curr-dist = (* interval-index interval-length)
               while (< curr-dist eos) ;; NOTE: P1 is not included
               do (funcall function
                           ;; Number of intervals
                           interval-index
                           ;; Distance from start point
                           curr-dist
                           ;; Position
                           (edraw-xy-interpolate
                            p0
                            p1
                            (/ (- curr-dist bos) length))
                           ;; Tangent vector
                           (edraw-xy-normalize
                            (edraw-xy-sub p1 p0))))
      eos)))

(defun edraw-path-seglist-for-each-intervals (seglist
                                              interval-length
                                              dist-from-start
                                              epsilon
                                              function)
  (dolist (seg seglist)
    (setq dist-from-start
          (edraw-path-seg-for-each-intervals
           seg interval-length dist-from-start epsilon function)))
  ;; TODO: generate last point? how calculate tangent?
  ;;(when (= (mod dist-from-start interval-length) 0) )
  )


;;;;; Generate shapes along segments

(defun edraw-generate-path-subpath-zigzag-line-along-seglist
    (seglist closed wavelength amplitude &optional noerror)
  "Generate a zigzag line along SEGLIST.
Return an `edraw-path-subpath' object.
When CLOSED is non-nil, it means SEGLIST is a closed path.
WAVELENGTH and AMPLITUDE are generation parameters.
When NOERROR is non-nil, errors are ignored and the function returns nil."
  (when (and (not noerror) (null seglist))
    (error "Empty seglist"))
  (when seglist
    (let* ((epsilon 0.1)
           (seglist-length (edraw-path-seglist-length seglist epsilon))
           (num-waves (ceiling (/ seglist-length wavelength)))
           (actual-wavelength (/ seglist-length num-waves))
           (subpath (edraw-path-subpath closed)))
      (when (and (not noerror) (zerop seglist-length))
        (error "seglist length is 0"))
      (unless (zerop seglist-length)
        ;; First point
        (unless closed
          (edraw-path-subpath-add-new-anchor subpath (elt (car seglist) 0)))

        (edraw-path-seglist-for-each-intervals
         seglist
         (* 0.5 actual-wavelength)
         (- (* 0.25 actual-wavelength))
         epsilon
         (lambda (i _dist pt tan)
           (edraw-path-subpath-add-new-anchor
            subpath
            (edraw-xy-add pt (edraw-xy-nmul
                              (if (cl-oddp i) (- amplitude) amplitude)
                              (edraw-xy-rot90 tan))))))
        ;; Last point
        (unless closed
          (let ((last-seg (car (last seglist))))
            (edraw-path-subpath-add-new-anchor subpath
                                               (if (= (length last-seg) 2)
                                                   (elt last-seg 1)
                                                 (elt last-seg 3)))))
        subpath))))

(defun edraw-generate-path-subpath-wavy-line-along-seglist
    (seglist closed wavelength amplitude &optional noerror)
  "Generate a wavy line along SEGLIST.
Return an `edraw-path-subpath' object.
When CLOSED is non-nil, it means SEGLIST is a closed path.
WAVELENGTH and AMPLITUDE are generation parameters.
When NOERROR is non-nil, errors are ignored and the function returns nil."
  (when (and (not noerror) (null seglist))
    (error "Empty seglist"))
  (when seglist
    (let* ((epsilon 0.1)
           (seglist-length (edraw-path-seglist-length seglist epsilon))
           (num-waves (ceiling (/ seglist-length wavelength)))
           (actual-wavelength (/
                               (if closed
                                   seglist-length
                                 ;; Reduce slightly to include the end point
                                 (if (< seglist-length 1.0)
                                     (* seglist-length 0.99)
                                   (- seglist-length 0.01)))
                               num-waves))
           (subpath (edraw-path-subpath closed)))
      (when (and (not noerror) (zerop seglist-length))
        (error "seglist length is 0"))
      (unless (zerop seglist-length)
        (edraw-path-seglist-for-each-intervals
         seglist
         (* 0.5 actual-wavelength) 0 epsilon
         (lambda (i _dist pt tan)
           (let ((forward-handle
                  (edraw-xy-add
                   (edraw-xy-nmul (if (cl-oddp i) (- amplitude) amplitude)
                                  (edraw-xy-rot90 tan))
                   (edraw-xy-nmul (* 0.2 wavelength)
                                  tan))))
             (edraw-path-subpath-add-new-anchor subpath
                                                pt
                                                (edraw-xy-neg forward-handle)
                                                forward-handle))))
        subpath))))

(defun edraw-coil-line-fun (phase rx ry wavelength phase-shift)
  (let* ((th (/ (* 2 float-pi (+ phase phase-shift)) 4))
         (x (+ (* (- 1 (cos th)) rx) (* 0.25 wavelength phase-shift)))
         (y (* (sin th) ry)))
    (edraw-xy x y)))

(defun edraw-generate-path-subpath-coil-line-along-seglist
    (seglist closed wavelength amplitude aspect-ratio &optional noerror)
  "Generate a coil line along SEGLIST.
Return an `edraw-path-subpath' object.
When CLOSED is non-nil, it means SEGLIST is a closed path.
WAVELENGTH and AMPLITUDE are generation parameters.
When NOERROR is non-nil, errors are ignored and the function returns nil."
  (when (and (not noerror) (null seglist))
    (error "Empty seglist"))
  ;; TODO: Calculate more accurately.
  ;;
  ;; To calculate more accurately, we need to consider two things:
  ;; - Fitting the coil generation function with a Bezier curve
  ;; - More detailed transformation of coordinates along the path

  ;; The correct coil line can be generated with the following code:
  ;; (require 'svg)
  ;; (let ((wavelength 50)
  ;;       (amplitude 100)
  ;;       (aspect-ratio 0.5)
  ;;       (svg (svg-create 640 480 :viewBox "0 -240 640 480")))
  ;;   (svg-rectangle svg 0 -240 640 480 :fill "#ccc" :stroke "none")
  ;;   (svg-line svg 0 0 640 0 :stroke "red" :fill "none")
  ;;   (svg-polyline
  ;;    svg
  ;;    (cl-loop for d from 0 to 640
  ;;             for th = (/ (* 2 float-pi d) wavelength)
  ;;             collect
  ;;             (cons
  ;;              (+ (* (- 1 (cos th)) amplitude aspect-ratio) d)
  ;;              (* (sin th) amplitude)))
  ;;    :stroke "blue" :fill "none")
  ;;   (svg-insert-image svg))

  (when seglist
    (let* ((epsilon 0.1)
           (seglist-length (edraw-path-seglist-length seglist epsilon))
           (num-waves (ceiling (/ seglist-length wavelength)))
           (actual-wavelength (/
                               (if closed
                                   seglist-length
                                 ;; Reduce slightly to include the end point
                                 (if (< seglist-length 1.0)
                                     (* seglist-length 0.9999)
                                   (- seglist-length 0.01)))
                               num-waves))
           (rx (* aspect-ratio amplitude))
           (ry amplitude)
           (subpath (edraw-path-subpath closed)))
      (when (and (not noerror) (zerop seglist-length))
        (error "seglist length is 0"))
      (unless (zerop seglist-length)
        (edraw-path-seglist-for-each-intervals
         seglist
         (* 0.25 actual-wavelength) 0 epsilon
         (lambda (i _dist pt tan)
           (let* ((phase (% i 4))
                  (p0 (edraw-coil-line-fun phase rx ry actual-wavelength 0))
                  (p1 (edraw-coil-line-fun phase rx ry actual-wavelength 0.4))
                  (anchor-pt
                   (edraw-xy-add pt (edraw-xy-complex-mul p0 tan)))
                  (handle-vec
                   (pcase phase
                     ;; Tilt slightly when crossing the path (phase=0, 2).
                     (0 (let ((p2 (edraw-coil-line-fun
                                   phase rx ry actual-wavelength 0.2)))
                          (edraw-xy (- (edraw-x p2) (edraw-x p0))
                                    (- (edraw-y p1) (edraw-y p0)))))
                     (1 (edraw-xy (- (edraw-x p1) (edraw-x p0)) 0))
                     (2 (let ((p2 (edraw-coil-line-fun
                                   phase rx ry actual-wavelength -0.2)))
                          (edraw-xy (- (edraw-x p0) (edraw-x p2))
                                    (- (edraw-y p1) (edraw-y p0)))))
                     (3 (edraw-xy (- (edraw-x p1) (edraw-x p0)) 0))))
                  (forward-handle
                   (edraw-xy-complex-mul handle-vec tan)))
             (edraw-path-subpath-add-new-anchor subpath
                                                anchor-pt
                                                (edraw-xy-neg forward-handle)
                                                forward-handle))))
        subpath))))


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
   "\\([A-Za-z]\\)" ;; (1) command type
   "\\(?:" edraw-path-d-wsp-opt
   "\\(" edraw-path-d-number ;;(2) command arguments
   "\\(?:" edraw-path-d-comma-wsp "?" edraw-path-d-number "\\)*\\)" "\\)?"
   edraw-path-d-wsp "?"))

(defun edraw-path-d-split-numbers-str (numbers-str)
  "Split NUMBERS-STR into a list of number strings."
  (let (ret pos)
    (while (string-match edraw-path-d-number numbers-str pos)
      (setq pos (match-end 0))
      (push (match-string 0 numbers-str) ret))
    (nreverse ret)))

(defun edraw-path-d-parse (d)
  "Parse path data D and return ( (<command-type> <number>...)... ).

Note: The command types (command names) and number of arguments
are validated by `edraw-path-data-from-d'."
  (let ((pos 0)
        commands)
    (while (string-match edraw-path-d-command d pos)
      (when (/= (match-beginning 0) pos)
        (error "Path data parsing error at %s" (substring d pos)))
      (setq pos (match-end 0))
      (let* ((type (intern (match-string 1 d)))
             (numbers-str (match-string 2 d))
             (numbers (if numbers-str
                          (mapcar (lambda (str) (float (string-to-number str)))
                                  (edraw-path-d-split-numbers-str numbers-str)))))
        (push (cons type numbers) commands)))
    (when (/= pos (length d))
      (error "Path data parsing error at %s" (substring d pos)))
    (nreverse commands)))
;; TEST: (edraw-path-d-parse "Z M 10 20.1 L .1 2e+1 20e1 -5e-1") => ((Z) (M 10.0 20.1) (L 0.1 20.0 200.0 -0.5))
;; TEST: (edraw-path-d-parse "ZM10 20.1L.1 2e+1 20e1 -5e-1") => ((Z) (M 10.0 20.1) (L 0.1 20.0 200.0 -0.5))
;; TEST: (edraw-path-d-parse "ZM10 20.1L.1-2e+1 20e1.5e-1") => ((Z) (M 10.0 20.1) (L 0.1 -20.0 200.0 0.05))
;; TEST: (edraw-path-d-parse "M10 20.1L.1-2e+1 20e1+5e-1Z") => ((M 10.0 20.1) (L 0.1 -20.0 200.0 0.5) (Z))
;; TEST: (edraw-path-d-parse "M100-200+300.5-400.6") => ((M 100.0 -200.0 300.5 -400.6))
;; TEST: (edraw-path-d-parse "M100.200.300.400.500") => ((M 100.2 0.3 0.4 0.5))
;; TEST: (edraw-path-d-parse "M100..200e2+.300-1.e3.500") => ((M 100.0 20.0 0.3 -1000.0 0.5))
;; TEST: (edraw-path-d-parse "  M100 200\nL300 400 \n  ") => ((M 100.0 200.0) (L 300.0 400.0))
;; TEST: (edraw-path-d-parse "M100 200,L300 400") => error
;; TEST: (edraw-path-d-parse "M100 200L,300 400") => error
;; TEST: (edraw-path-d-parse "M1e23e4") => ((M 1e+23) (e 4.0))
;; TEST: (let ((case-fold-search nil)) (edraw-path-d-parse "M100 100l100 0l0 100")) => ((M 100.0 100.0) (l 100.0 0.0) (l 0.0 100.0))

(defun edraw-path-d-from-command-list (command-list)
  (mapconcat (lambda (command)
               (concat
                (symbol-name (car command))
                (edraw-path-cmdstr-xys (cdr command))))
             command-list))
;; TEST: (edraw-path-d-from-command-list '((Z) (M 10 20.1) (L 0.1 20.0 200.0 -0.5))) => "ZM10 20.1L0.1 20 200-0.5"

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
                           ('H
                            (mapcar (lambda (n) (+ n x)) args))
                           ('V
                            (mapcar (lambda (n) (+ n y)) args))
                           ;;('A)
                           (_ args))))))))
;; TEST: (edraw-path-d-translate "M 10 20 L 30 40 50 60" '(100 . 200)) => "M110 220L130 240 150 260"
;; TEST: (edraw-path-d-translate "M 10 20 H 30 40 V 50 60" '(100 . 200)) => "M110 220H130 140V250 260"


(provide 'edraw-path)
;;; edraw-path.el ends here
