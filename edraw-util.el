;;; edraw-util.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena.jp>
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


;;;; Message Catalog

(defun edraw-msg (msg-id)
  ;;@todo translate messages
  msg-id)

;;;; Clipboard

(defvar edraw-clipboard-type-data nil)

(defun edraw-clipboard-set (type data)
  (setq edraw-clipboard-type-data (cons type data)))

(defun edraw-clipboard-empty-p ()
  (null edraw-clipboard-type-data))

(defun edraw-clipboard-type ()
  (car edraw-clipboard-type-data))

(defun edraw-clipboard-data ()
  (cdr edraw-clipboard-type-data))

;;;; gzip

(defconst edraw-detect-coding-system-p nil)

(defun edraw-decode-coding-region (begin end &optional coding-system)
  (decode-coding-region
   begin end
   (if edraw-detect-coding-system-p
       (detect-coding-region begin end t)
     (or coding-system 'utf-8))))

(defun edraw-shell-command-on-buffer (command)
  (let ((error-buffer "*edraw gzip error*"))
    (when (/= 0 (shell-command-on-region
                 (point-min) (point-max) command nil t error-buffer t))
      (error (format "%s error" command)))))

(defun edraw-gunzip-buffer (&optional coding-system)
  (when (edraw-buffer-gzip-p)
    (if (zlib-available-p)
        (progn
          (set-buffer-multibyte nil)
          (zlib-decompress-region (point-min) (point-max))
          (edraw-decode-coding-region (point-min) (point-max) coding-system)
          (set-buffer-multibyte t))
      (let ((coding-system-for-read 'no-conversion)
            (coding-system-for-write 'no-conversion))
        (edraw-shell-command-on-buffer "gunzip")
        (edraw-decode-coding-region (point-min) (point-max) coding-system)))))

(defun edraw-gzip-buffer (&optional coding-system)
  (let ((coding-system-for-read 'no-conversion)
        (coding-system-for-write (or coding-system 'utf-8)))
    (edraw-shell-command-on-buffer "gzip")))

(defun edraw-buffer-gzip-p ()
  ;; gzip's magic number 1f 8b
  (and
   (>= (- (point-max) (point-min)) 2)
   (= (multibyte-char-to-unibyte (char-after (+ (point-min) 0))) #x1f)
   (= (multibyte-char-to-unibyte (char-after (+ (point-min) 1))) #x8b)))

;;;; gzip & base64

(defun edraw-decode-buffer (base64-p &optional coding-system)
  (when base64-p
    (base64-decode-region (point-min) (point-max))
    (unless (edraw-buffer-gzip-p)
      (edraw-decode-coding-region (point-min) (point-max) coding-system)))
  (when (edraw-buffer-gzip-p)
    (edraw-gunzip-buffer)))

(defun edraw-encode-buffer (base64-p gzip-p &optional coding-system)
  (when gzip-p
    (edraw-gzip-buffer))
  (when base64-p
    (unless gzip-p
      (encode-coding-region (point-min) (point-max) (or coding-system 'utf-8)))
    (base64-encode-region (point-min) (point-max) t)))

(defun edraw-decode-string (data base64-p &optional coding-system)
  (with-temp-buffer
    (insert data)
    (edraw-decode-buffer base64-p coding-system)
    (buffer-string)))

(defun edraw-encode-string (data base64-p gzip-p coding-system)
  (with-temp-buffer
    (insert data)
    (edraw-encode-buffer base64-p gzip-p coding-system)
    (buffer-string)))


;;;; Menu UI

(defun edraw-y-or-n-p (prompt)
  (x-popup-menu
   t
   (list prompt
         (cons ""
               (list (cons (edraw-msg "Yes") t)
                     (cons (edraw-msg "No") nil))))))

(defun edraw-popup-menu (name items &rest args)
  (let* ((menu-map (edraw-make-menu-map name items))
         (events (x-popup-menu t menu-map))
         (fn (lookup-key menu-map (apply 'vector events))))
    (when (functionp fn)
      (apply fn args))))

(defun edraw-make-menu-map (name items)
  (nconc
   (cons 'keymap (if name (list name)))
   (let ((id 0))
     (cl-loop for item in items
              collect
              (let ((id-symbol (intern (format "item%s" id))))
                (setq id (1+ id))
                (cond
                 ((or (symbolp (cadr item))
                      (functionp (cadr item)))
                  (append
                   (list id-symbol 'menu-item)
                   item))
                 ((listp (cadr item))
                  (list id-symbol 'menu-item (car item)
                        (edraw-make-menu-map nil (cadr item))))
                 (t (error "Unkonwn menu item format %s"
                           (prin1-to-string item)))))))))



;;;; Emacs UI Utility

;;
;;

(defun edraw-track-dragging (down-event on-move
                                        &optional on-up on-leave target)
  (if (not (memq 'down (event-modifiers down-event)))
      (error "down-event is not down event. %s" (event-modifiers down-event)))
  (let* ((down-basic-type (event-basic-type down-event))
         (down-position (event-start down-event))
         (target-window (posn-window down-position))
         (target-point (posn-point down-position))
         (target-object (posn-object down-position)))

    (track-mouse
      (let (result)
        (while (null result)
          (let ((event (read-event)))
            (cond
             ;; mouse move
             ((mouse-movement-p event)
              ;; check same object
              (if (and (eq (posn-window (event-start event))
                           target-window)
                       (if (memq target '(point object nil))
                           (= (posn-point (event-start event))
                              target-point)
                         t)
                       (if (memq target '(object nil))
                           (eq (car (posn-object (event-start event))) ;;ex: 'image
                               (car target-object)) ;;ex: 'image
                         t))
                  (if on-move (funcall on-move event))
                ;; out of target
                (if on-up (funcall on-leave event))
                (setq result event)))
             ;; mouse up
             ((and (eq (event-basic-type event) down-basic-type)
                   (or (memq 'click (event-modifiers event))
                       (memq 'drag (event-modifiers event))))
              (if on-up (funcall on-up event))
              (setq result event))
             ;; otherwise
             (t
              (if on-up (funcall on-up event))
              (setq result event)
              (push (cons t event) unread-command-events)))))
        result))))



;;;; Hook Utility

;; - Hooks deleted during callback will never be called.
;; - Hooks added during the callback will never be called until next time.
;; - To determine the equivalence when removing the hook, use the eq function
;;  to determine the combination of the function and additional arguments.

(defun edraw-hook-make ()
  (cons 0 nil)) ;;(re-entry-count . function-list)

(defun edraw-hook-add (hook function &rest args)
  ;; push front only
  (setcdr hook (cons (cons function args) (cdr hook))))

(defun edraw-hook-remove (hook function &rest args)
  (edraw-hook-mark-delete hook (cons function args))
  (edraw-hook-sweep hook))

(defun edraw-hook-mark-delete (hook function-args)
  (let ((p (cdr hook)))
    (while p
      ;; eq function and eq args
      (when (and (= (length function-args) (length (car p)))
                 (cl-every 'eq function-args (car p)))
        (setcar p nil))
      (setq p (cdr p)))))

(defun edraw-hook-sweep (hook)
  (when (= (car hook) 0)
    (setcdr hook (delq nil (cdr hook)))))

(defun edraw-hook-call (hook &rest args)
  (cl-incf (car hook))
  (unwind-protect
      (let ((p (cdr hook)))
        (while p
          (when (car p)
            (apply (caar p) (append (cdar p) args)))
          (setq p (cdr p))))
    (cl-decf (car hook)))
  (edraw-hook-sweep hook))



;;;; Misc

(defun edraw-alist-get-as-number (key alist default)
  (let ((value (alist-get key alist default nil (if (stringp key) #'equal))))
    (if (stringp value)
        (string-to-number value)
      value)))

(cl-defmethod edraw-to-string ((str string))
  str)

(cl-defmethod edraw-cast (obj type)
  (when (cl-typep obj type)
    obj))



(provide 'edraw-util)
;;; edraw-util.el ends here
