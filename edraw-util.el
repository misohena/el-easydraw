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



;;;; gzip

(defconst edraw-detect-coding-system-p nil)

(defun edraw-decode-coding-region (begin end)
  (decode-coding-region
   begin end
   (if edraw-detect-coding-system-p
       (detect-coding-region begin end t)
     'utf-8)))

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
          (edraw-decode-coding-region (point-min) (point-max))
          (set-buffer-multibyte t))
      (let ((coding-system-for-read 'no-conversion)
            (coding-system-for-write 'no-conversion))
        (edraw-shell-command-on-buffer "gunzip")
        (edraw-decode-coding-region (point-min) (point-max))))))

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


(provide 'edraw-util)
;;; edraw-util.el ends here
