;;; edraw-editor-util.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2023 AKIYAMA Kouhei

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

;; Utilities for use only while edraw-editor is running.

;; Intended for use from edraw-property-editor.el,
;; edraw-shape-picker.el and edraw.el. Not used from edraw-org.el.

;;; Code:

(require 'eieio)

;;;; Buffer Display

;; A mechanism for displaying a buffer in a window or frame.

(defconst edraw-buffer-display-frame-parameters-to-save
  '(left top width height z-group)
  "Frame parameters to be saved.")

(defconst edraw-buffer-display-frame-parameters-default
  '((z-group . above) ;;top most
    (user-position . t)
    ;;(left-fringe . 0)
    ;;(right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    ;;(unsplittable . t)
    (width . 56)
    (height . 30))
  "Default frame parameters.")

(defclass edraw-buffer-display ()
  ((buffer :initarg :buffer)
   (window :initform nil)
   (frame :initform nil)
   (frame-mode :initform nil :initarg :frame-mode)
   (frame-delete-hook :initform nil)
   (frame-parameters-last :initform nil :initarg :frame-parameters-last)
   (frame-parameters-to-save
    :initform (identity edraw-buffer-display-frame-parameters-to-save)
    :initarg :frame-parameters-to-save)
   (frame-parameters-default
    :initform (identity edraw-buffer-display-frame-parameters-default)
    :initarg :frame-parameters-default)
   (frame-child-p :initform nil :initarg :frame-child-p)
   (frame-mode-line-p :initform nil :initarg :frame-mode-line-p)
   (save-function :initform nil :initarg :save-function))
  "A class that controls how the buffer is displayed.")

(cl-defmethod edraw-destroy ((bd edraw-buffer-display))
  (edraw-unobserve-frame bd))

;;;;; Save Settings

(cl-defmethod edraw-save-state ((bd edraw-buffer-display) key value)
  (with-slots (save-function) bd
    (when save-function
      (funcall save-function bd key value))))

;;;;; Open Window or Frame

(cl-defmethod edraw-display-buffer ((bd edraw-buffer-display))
  (with-slots (frame-mode) bd
    (if frame-mode
        (edraw-open-frame bd)
      (edraw-open-window bd))))

(cl-defmethod edraw-open-window ((bd edraw-buffer-display))
  (save-selected-window
    (with-slots (window buffer) bd
      ;; Show mode line
      (with-current-buffer buffer
        (kill-local-variable 'mode-line-format))

      (cond
       ;; Use existing window
       ((and (window-live-p window)
             (eq (window-buffer window) buffer))
        (select-window window))
       ;; Create new window
       (t
        (pop-to-buffer buffer)
        (setq window (selected-window))))
      (edraw-buffer-display-fit-window-size-to-content))))

(defun edraw-buffer-display-fit-window-size-to-content ()
  (when-let ((parent-window (window-parent)))
    (let* ((parent-window-height (window-height parent-window))
           (max-height (/ parent-window-height 2)))
      (fit-window-to-buffer nil max-height)
      (enlarge-window 1))))

(cl-defmethod edraw-open-frame ((bd edraw-buffer-display))
  (with-slots (frame window buffer frame-mode-line-p) bd
    (if (frame-live-p frame)
        ;; Use existing frame
        (progn
          (select-frame frame)
          (delete-other-windows)
          (setq window (selected-window)))
      ;; Create new frame
      (setq frame (make-frame (edraw-make-frame-parameters bd)))
      (setq window (frame-root-window frame))
      (edraw-observe-frame bd))
    ;; Hide mode line
    (unless frame-mode-line-p
      (with-current-buffer buffer
        (setq-local mode-line-format nil)))
    ;; Show buffer
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)))

;;;;; Observe Frame

(cl-defmethod edraw-observe-frame ((bd edraw-buffer-display))
  (with-slots (frame-delete-hook) bd
    (unless frame-delete-hook
      (setq frame-delete-hook
            (lambda (frame) (edraw-on-delete-frame bd frame)))
      (add-hook 'delete-frame-functions frame-delete-hook))))

(cl-defmethod edraw-unobserve-frame ((bd edraw-buffer-display))
  (with-slots (frame-delete-hook) bd
    (when frame-delete-hook
      (remove-hook 'delete-frame-functions frame-delete-hook)
      (setq frame-delete-hook nil))))

(cl-defmethod edraw-on-delete-frame ((bd edraw-buffer-display) frame-to-del)
  ;; Backup frame parameters
  (with-slots (frame window buffer) bd
    (when (eq frame-to-del frame)
      ;; Save frame position, size, etc.
      (edraw-save-frame-parameters bd)
      ;; No more observing the frame
      (edraw-unobserve-frame bd)
      ;; Clear Variables
      (setq frame nil
            window nil)
      ;; Delete buffer
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  nil)

;;;;; Frame Parameters

(cl-defmethod edraw-make-frame-parameters ((bd edraw-buffer-display))
  (with-slots (frame-parameters-last
               frame-parameters-default
               frame-child-p)
      bd
    ;;@todo Remove duplicated? Always prefer the first param?
    (append frame-parameters-last
            frame-parameters-default
            (when frame-child-p
              (list (cons 'parent-frame (selected-frame)))))))

(cl-defmethod edraw-get-frame-parameters-last ((bd edraw-buffer-display))
  (oref bd frame-parameters-last))

(cl-defmethod edraw-set-frame-parameters-last ((bd edraw-buffer-display) params)
  (oset bd frame-parameters-last params))

(cl-defmethod edraw-save-frame-parameters ((bd edraw-buffer-display))
  (with-slots (frame
               frame-parameters-last
               frame-parameters-to-save
               buffer)
      bd
    (when (frame-live-p frame)
      (setq frame-parameters-last
            (cl-loop for param in (frame-parameters frame)
                     when (memq (car param)
                                frame-parameters-to-save)
                     collect param))
      (edraw-save-state bd 'frame-parameters-last frame-parameters-last))))


;;;;; Close Window or Frame

(cl-defmethod edraw-close ((bd edraw-buffer-display))
  (with-slots (frame window buffer) bd
    ;; close window
    (edraw-delete-display bd)

    ;; delete buffer
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(cl-defmethod edraw-delete-display ((bd edraw-buffer-display))
  (with-slots (frame window buffer) bd
    (cond
     ;; Delete frame
     (frame
      (edraw-save-frame-parameters bd) ;; Save position
      (edraw-unobserve-frame bd) ;; Keep buffer
      (when (frame-live-p frame)
        (delete-frame frame))
      (setq frame nil
            window nil))
     ;; Delete window
     (window
      (when (and (window-live-p window)
                 buffer
                 (eq (window-buffer window) buffer)
                 (window-parent window))
        (delete-window window))
      (setq window nil)))))

(cl-defmethod edraw-set-frame-parameter ((bd edraw-buffer-display)
                                         parameter value)
  (with-slots (frame) bd
    (when frame
      (set-frame-parameter frame parameter value))))

(cl-defmethod edraw-get-frame-parameter ((bd edraw-buffer-display)
                                         parameter)
  (with-slots (frame) bd
    (when frame
      (frame-parameter frame parameter))))

;;;;; Frame Mode

(cl-defmethod edraw-get-frame-mode ((bd edraw-buffer-display))
  (oref bd frame-mode))

(cl-defmethod edraw-toggle-frame-mode ((bd edraw-buffer-display))
  (with-slots (frame-mode) bd
    (edraw-delete-display bd)
    (setq frame-mode (not frame-mode))
    (edraw-display-buffer bd)
    (edraw-save-state bd 'frame-mode frame-mode)))

(cl-defmethod edraw-set-frame-mode ((bd edraw-buffer-display) mode)
  (unless (eq (edraw-get-frame-mode bd) mode)
    (edraw-toggle-frame-mode bd)))

;;;;; Frame Mode Line

(cl-defmethod edraw-get-frame-mode-line-p ((bd edraw-buffer-display))
  (oref bd frame-mode-line-p))

(cl-defmethod edraw-toggle-frame-mode-line-p ((bd edraw-buffer-display))
  (with-slots (buffer frame frame-mode-line-p) bd
    (setq frame-mode-line-p (not frame-mode-line-p))
    (when frame
      (with-current-buffer buffer
        (if frame-mode-line-p
            (kill-local-variable 'mode-line-format)
          (setq-local mode-line-format nil))
        (redraw-display)))
    (edraw-save-state bd 'frame-mode-line-p frame-mode-line-p)))

(cl-defmethod edraw-set-frame-mode-line-p ((bd edraw-buffer-display) flag)
  (unless (eq flag (oref bd frame-mode-line-p))
    (edraw-toggle-frame-mode-line-p bd)))

;;;;; Frame Child

(cl-defmethod edraw-get-frame-child-p ((bd edraw-buffer-display))
  (oref bd frame-child-p))

(cl-defmethod edraw-toggle-frame-child-p ((bd edraw-buffer-display))
  (with-slots (frame frame-child-p frame-parameters-last) bd
    (setq frame-child-p (not frame-child-p))
    (when frame
      (edraw-delete-display bd)
      ;; Reset position
      (setf (alist-get 'left frame-parameters-last nil t) nil)
      (setf (alist-get 'top frame-parameters-last nil t) nil)
      (edraw-display-buffer bd))
    (edraw-save-state bd 'frame-child-p frame-child-p)))

(cl-defmethod edraw-set-frame-child-p ((bd edraw-buffer-display) flag)
  (unless (eq flag (oref bd frame-child-p))
    (edraw-toggle-frame-child-p bd)))



;;;; UI State Store

(defcustom edraw-ui-state-file
  (locate-user-emacs-file "edraw-ui-state.config")
  ""
  :type 'file
  :group 'edraw-editor)

(defvar edraw-ui-state-store nil)

(defun edraw-ui-state-load ()
  (unless edraw-ui-state-store
    (setq edraw-ui-state-store
          (condition-case _err
              (with-temp-buffer
                (insert-file-contents edraw-ui-state-file)
                (goto-char (point-min))
                (read (current-buffer)))
            (error (list (cons 'edraw-ui-state (list (cons 'version 1)))))))))

(defun edraw-ui-state-save ()
  (with-temp-file edraw-ui-state-file
    (insert ";;; Edraw UI State ---    -*- mode: lisp-data -*-\n")
    (pp edraw-ui-state-store (current-buffer))))

(defun edraw-ui-state-get (domain key &optional default)
  (edraw-ui-state-load)
  (alist-get key (alist-get domain edraw-ui-state-store) default))

(defun edraw-ui-state-set (domain key value)
  (edraw-ui-state-load)
  (setf (alist-get key (alist-get domain edraw-ui-state-store)) value))


(provide 'edraw-editor-util)
;;; edraw-editor-util.el ends here
