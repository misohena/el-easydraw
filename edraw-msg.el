;;; edraw-msg.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

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

(defgroup edraw-msg nil
  "The language of the message text to display."
  :tag "Edraw Message Text"
  :group 'edraw)

(defconst edraw-msg-language-file-alist
  '(("Japanese" . "msg/edraw-msg-ja")))

(defun edraw-msg-file ()
  (defvar edraw-msg-file)
  (cond
   ((eq edraw-msg-file 'auto)
    (alist-get current-language-environment
               edraw-msg-language-file-alist nil nil #'equal))
   ((stringp edraw-msg-file)
    edraw-msg-file)))

(defvar edraw-msg-hash-table nil)

(defun edraw-msg-load ()
  (setq edraw-msg-hash-table nil)
  (let ((file (edraw-msg-file)))
    (when file
      (ignore-errors
        ;; (setq edraw-msg-hash-table ...)
        (load-library file)))))

(defun edraw-msg-register (hash-table)
  (setq edraw-msg-hash-table hash-table))

(defun edraw-msg (msgid)
  (if edraw-msg-hash-table
      (or (gethash msgid edraw-msg-hash-table)
          msgid)
    msgid))

;;;; Message Catalog File

;;@todo add-hook 'set-language-environment-hook ?

(defun edraw-msg-file-set (sym value)
  (set-default-toplevel-value sym value)
  (edraw-msg-load))

(defcustom edraw-msg-file 'auto
  "File name of message catalog."
  :group 'edraw-msg
  :set 'edraw-msg-file-set ;; (edraw-msg-load) is called immediately!!
  :type '(choice (const :tag "Determine by current-language-environment and edraw-msg-language-file-alist" auto)
                 (const :tag "Disable translation" nil)
                 (file :tag "File name in load path")))

(provide 'edraw-msg)
;;; edraw-msg.el ends here
