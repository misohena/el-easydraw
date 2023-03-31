;;; edraw-msg-tools.el --- Tools for Translators    -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'cl-lib)

;;;; Parse Source File

(defun edraw-msg-collect ()
  "Return list of messages used in source files."
  (seq-uniq
   (sort
    (edraw-msg-collect-from-dir)
    #'string-lessp)
   #'string=))

(defun edraw-msg-source-files ()
  "Return list of source files."
  (directory-files (file-name-directory (locate-library "edraw")) t "\\.el$"))

(defun edraw-msg-collect-from-dir ()
  "Return list of messages used in the source directory."
  (cl-loop for source-file in (edraw-msg-source-files)
           nconc (edraw-msg-collect-from-file source-file)))

(defun edraw-msg-collect-from-file (source-file)
  "Return list of messages used in the SOURCE-FILE."
  (with-temp-buffer
    (insert-file-contents source-file)
    (goto-char (point-min))
    (cl-loop while (re-search-forward
                    "(edraw-msg[ \n\t]+\\(\"\\(?:\\\\\"\\|[^\"]\\)+\"\\)" nil t)
             collect (match-string-no-properties 1))))

;;;; Search Source

(defun edraw-msg-search (msg-str)
  "Search for places using MSG-STR."
  (interactive
   (list
    (or (save-excursion
          (forward-line 0)
          (when (looking-at "^[ \t]*\\(\"\\(?:\\\\\"\\|[^\"]\\)*\"\\)")
            (match-string 1)))
        (concat "\"" (read-string "Message: ") "\""))))

  (let ((files (edraw-msg-search-from-dir msg-str)))
    (cond
     ((null files)
      (error "Not found"))

     ((and (= (length files) 1) (= (length (cadr (car files))) 1))
      (find-file (caar files))
      (goto-char (point-min))
      (forward-line (1- (car (cadar files)))))

     (t
      (ignore-errors
        (kill-buffer "*edraw msg search*"))
      (let ((buffer (get-buffer-create "*edraw msg search*")))
        (with-current-buffer buffer
          (insert "\n")
          (cl-loop for (file line-numbers) in files
                   do (cl-loop for line-number in line-numbers
                               do
                               (insert file ":" (number-to-string line-number) ":" "\n")))
          (goto-char (point-min))
          (grep-mode))
        (pop-to-buffer buffer)
        (first-error)
        )))))

(defun edraw-msg-search-from-dir (msg-str)
  (cl-loop for source-file in (edraw-msg-source-files)
           for line-numbers = (edraw-msg-search-from-file source-file msg-str)
           when line-numbers
           collect (list source-file line-numbers)))

(defun edraw-msg-search-from-file (source-file msg-str)
  (let ((regexp (concat "(edraw-msg[ \n\t]+" (regexp-quote msg-str))))
    (with-temp-buffer
      (insert-file-contents source-file)
      (goto-char (point-min))
      (cl-loop while (re-search-forward regexp nil t)
               collect (line-number-at-pos (match-beginning 0))))))

;;;; Catalog File Generation

(defun edraw-msg-make-hash-table ()
  (interactive)
  (insert "(setq
 edraw-msg-hash-table
 #s(hash-table
    size 65
    test equal
    data
    (
     ;; [BEGIN MSG DATA]
     ;; [END MSG DATA]
")
  (dolist (msgid (edraw-msg-collect))
    (insert "     \"" msgid "\" \"???\"\n"))
  (insert "     )))"))

;;;; Update Catalog File

(defun edraw-msg-update-catalog-buffer ()
  (interactive)
  (let ((messages (edraw-msg-collect))
        translated
        (count-deleted 0)
        (count-added 0))
    ;; Delete messages
    (goto-char (point-min))
    (re-search-forward ";; *\\[BEGIN MSG DATA\\][^\n]*\n")
    (condition-case _err
        (while t
          (let* ((msgid-end (progn (forward-sexp) (point)))
                 (msgid-begin (progn (backward-sexp) (point)))
                 (msgid-str (buffer-substring-no-properties
                             msgid-begin msgid-end)))
            (push msgid-str translated)
            (forward-sexp)
            (forward-sexp)
            (unless (seq-contains-p messages msgid-str #'string=)
              (comment-region msgid-begin (point))
              (cl-incf count-deleted))))
      (error nil))
    (setq translated (nreverse translated))

    ;; Add new messages
    (let ((new-messages
           (sort (seq-difference messages translated #'string=)
                 #'string-lessp)))
      (goto-char (point-min))
      (re-search-forward ";; *\\[BEGIN MSG DATA\\][^\n]*\n")
      (condition-case _err
          (while t
            (let* ((msgid-end (progn (forward-sexp) (point)))
                   (msgid-begin (progn (backward-sexp) (point)))
                   (msgid-str (buffer-substring-no-properties
                               msgid-begin msgid-end)))
              (when (string-lessp (car new-messages) msgid-str)
                (insert (car new-messages) " " "nil")
                (newline-and-indent)
                (cl-incf count-added)
                (pop new-messages)))
            (forward-sexp)
            (forward-sexp))
        (error nil))

      (dolist (new-msg new-messages)
        (newline-and-indent)
        (insert new-msg " " "nil")
        (cl-incf count-added)))

    (message "%s added, %s deleted" count-added count-deleted)))


(provide 'edraw-msg-tools)
;;; edraw-msg-tools.el ends here
