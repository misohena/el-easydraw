;;; edraw-shape-picker.el ---                       -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'eieio)
(require 'edraw-dom-svg)
(require 'edraw-util)

;;;; Customize

(defgroup edraw-shape-picker nil
  "Emacs Easy Draw Shape Picker"
  :prefix "edraw-shape-picker-"
  :group 'edraw-editor)

(defcustom edraw-shape-picker-entries-file
  (locate-user-emacs-file "edraw-custom.edraw-shapes")
  ""
  :type 'file
  :group 'edraw-shape-picker)

;; Thumbnail

(defcustom edraw-shape-picker-thumbnail-scaling nil
  "Scale thumbnails."
  :type 'boolean
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-width 50;;100
  "Width of a thumbnail."
  :type 'integer
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-height 50;;100
  "Height of a thumbnail."
  :type 'integer
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-max-width nil
  "Maximum width of a thumbnail."
  :type '(choice integer
                 (const nil :tag "Same as `edraw-shape-picker-thumbnail-width'"))
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-max-height nil
  "Maximum height of a thumbnail."
  :type '(choice integer
                 (const nil :tag "Same as `edraw-shape-picker-thumbnail-height'"))
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-padding 5
  "Space around a shape, inside of image."
  :type '(choice integer
                 (list (integer :tag "Left Right")
                       (integer :tag "Top Bottom"))
                 (list (integer :tag "Left")
                       (integer :tag "Top")
                       (integer :tag "Right")
                       (integer :tag "Bottom")))
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-background
  '(full (fill . "#fff"))
  "Thumbnail background"
  :type '(cons :tag "Background Spec"
          (choice (const full)
                  (const content))
          (repeat
           (cons :tag "SVG Attribute"
                 (symbol :tag "Attribute")
                 (string :tag "Value")))
          )
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-foreground-selected
  '(full (stroke . "rgba(0,100,255,0.75)") (stroke-width . "8") (fill . "none"))
  "Thumbnail background"
  :type '(cons :tag "Foreground Spec"
          (choice (const full)
                  (const content))
          (repeat
           (cons :tag "SVG Attribute"
                 (symbol :tag "Attribute")
                 (string :tag "Value")))
          )
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-thumbnail-margin
  1
  "Thumbnail margin."
  :type '(choice (integer)
                 (cons
                  (integer :tag "X")
                  (integer :tag "Y")))
  :group 'edraw-shape-picker)

(defcustom edraw-shape-picker-string-between-thumbnails
  nil
  ;; (propertize " "
  ;;             'display '(space :width (1))
  ;;             'pointer 'arrow)
  "String between thumbnails"
  :type 'string
  :group 'edraw-shape-picker)

;;

(defvar edraw-shape-picker-entries-default
  ;; (:section <root-props>... <entry>...)
  ;;
  ;; <entry> :
  ;;   (:section <section-prop>... <entry>...)
  ;;     see: `edraw-shape-picker-insert-section'
  ;;
  ;;   (:layout <layout-prop>... <entry>...)
  ;;     see: `edraw-shape-picker-insert-layout'
  ;;
  ;;   (:shape <shape-prop>... <shape-def>)
  ;;     see: `edraw-shape-picker-insert-shape'
  '(:section :name "Default Shape Set"
    (:section :name "Test"
              (:shape :name "red square" "<rect x=\"0\" y=\"0\" width=\"100\" height=\"100\" fill=\"red\" />")
              (:shape :name "bg test" :background ((fill . "blue")) "<ellipse cx=\"0\" cy=\"0\" rx=\"100\" ry=\"100\" fill=\"red\" />")
              (:shape :name "small blue circle" "<ellipse cx=\"0\" cy=\"0\" rx=\"8\" ry=\"8\" fill=\"blue\" />")
              (:shape :name "uzu" "<path stroke=\"#999\" stroke-width=\"4\" fill=\"none\" d=\"M3,11C8,13 15,6 15,0C15,-6 6,-14 -1,-14C-8,-14 -21.590,-7.032 -21,3C-20,20 -11,30 5,31C24.067,32.191 37,20 39,0C41,-20 22.011,-35.226 5,-36C-17,-37 -42.040,-22.980 -43,1C-44,26 -32.045,32.954 -21,44\" />")
              (:shape :name "star" "<path stroke=\"#999\" stroke-width=\"4\" fill=\"none\" d=\"M-12,-16L5,-43L18,-15L51,-12C51,-12 26,10 25,10C24,10 36,45 36,45L1,24C1,24 -35,41 -35,40C-35,39 -20,7 -20,7L-44,-15L-12,-16Z\" />")
              (:shape :name "wave" "<path stroke=\"#999\" stroke-width=\"4\" fill=\"none\" d=\"M-30,-45C-20,-35 -20,-25 -30,-15C-37.071,-7.928 -37.071,7.928 -30,15C-20,25 -20,35 -30,45\" /><path d=\"M0,-45C10,-35 10,-25 0,-15C-7.071,-7.928 -7.071,7.928 0,15C10,25 10,35 0,45\" fill=\"none\" stroke=\"#999\" stroke-width=\"4\" /><path d=\"M30,-45C40,-35 40,-25 30,-15C22.928,-7.928 22.928,7.928 30,15C40,25 40,35 30,45\" fill=\"none\" stroke=\"#999\" stroke-width=\"4\" />")
              (:shape :name "plus" "<path stroke=\"#999\" stroke-width=\"4\" fill=\"none\" d=\"M-10,-10L-10,-50L10,-50L10,-10L50,-10L50,10L10,10L10,50L-10,50L-10,10L-50,10L-50,-10L-10,-10Z\" />")
              (:shape "<rect x=\"0\" y=\"0\" width=\"100\" height=\"100\" fill=\"red\" />")
              (:shape "<rect x=\"0\" y=\"0\" width=\"100\" height=\"100\" fill=\"red\" />")
              (:shape :name "up arrow" "<path stroke=\"#999\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L-50,50L-20,50L-20,100L20,100L20,50L50,50L0,0Z\" />")
              (:shape :name "right arrow" "<path stroke=\"#999\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L-50,-50L-50,-20L-100,-20L-100,20L-50,20L-50,50L0,0Z\" />")
              (:shape :name "down arrow" "<path stroke=\"#999\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L-50,-50L-20,-50L-20,-100L20,-100L20,-50L50,-50L0,0Z\" />")
              (:shape :name "left arrow" "<path stroke=\"#999\" stroke-width=\"4\" fill=\"none\" d=\"M0,0L50,-50L50,-20L100,-20L100,20L50,20L50,50L0,0Z\" />")
              (:shape :name "marker test" "<g><path stroke=\"#999\" fill=\"none\" d=\"M20,40C20,0 80,0 80,40\" marker-end=\"url(#edraw-def-0-arrow)\" stroke-width=\"5\" /><path stroke=\"#999\" fill=\"none\" d=\"M80,60C80,100 20,100 20,60\" marker-end=\"url(#edraw-def-0-arrow)\" stroke-width=\"5\" /></g>")
              (:shape :name "marker test" "<defs id=\"edraw-defs\"><marker markerWidth=\"6\" markerHeight=\"6\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\" orient=\"auto\" stroke=\"none\" fill=\"#999\" id=\"edraw-def-0-arrow\"><path d=\"M0,1.5 0,8.5 7.0,5Z\" /></marker></defs><path stroke=\"#999\" fill=\"none\" d=\"M20,40C20,0 80,0 80,40\" marker-end=\"url(#edraw-def-0-arrow)\" stroke-width=\"5\" /><path stroke=\"#999\" fill=\"none\" d=\"M80,60C80,100 20,100 20,60\" marker-end=\"url(#edraw-def-0-arrow)\" stroke-width=\"5\" />")

              (:layout
               :thumbnail-max-width 400
               :thumbnail-max-height 200
               :thumbnail-background ((fill . "#333"))
               (:shape :name "star" "<path stroke=\"#eee\" stroke-width=\"4\" fill=\"none\" d=\"M-12,-16L5,-43L18,-15L51,-12C51,-12 26,10 25,10C24,10 36,45 36,45L1,24C1,24 -35,41 -35,40C-35,39 -20,7 -20,7L-44,-15L-12,-16Z\" />")
               (:shape :name "fujisan" "<path stroke=\"#999\" stroke-width=\"4\" d=\"M6,100C50,61 73,32 87,3C87,3 119,5 136,3C154,32 209,94 217,100C195,99 29,101 6,100Z\" fill=\"#8c94b1\" /><path d=\"M67,38C74,26 73,32 87,3C87,3 119,5 136,3C154,32 156,33 164,39C163.75,38.9375 139,23 139,23L129,38C129,38 114.0,20.0 114,20C114.0,20.0 100.0,39.0 100,39C100.0,39.0 89.0,23.0 89,23C89.0,23.0 70.5,37.5 67,38Z\" stroke=\"#999\" stroke-width=\"4\" fill=\"#eeeeee\" />")
               )
              )))



;;;; Setup

(defconst edraw-shape-picker-buffer-name "*Easy Draw Shape Picker*")

(defvar-local edraw-shape-picker-opened-by-user nil)
(defvar-local edraw-shape-picker-initial-window nil)

(defun edraw-shape-picker ()
  (interactive)
  (edraw-shape-picker-open)
  (setq-local edraw-shape-picker-opened-by-user t))

(defun edraw-shape-picker-open ()
  (edraw-shape-picker-pop-to-buffer
   (cond
    ;; org format
    ((and edraw-shape-picker-entries-file
          (equal (file-name-extension edraw-shape-picker-entries-file) "org"))
     (find-file-noselect edraw-shape-picker-entries-file)
     ;;@todo run edraw-shape-picker-org-mode minor mode
     )
    ;; .edraw-shapes format
    (t
     (edraw-shape-picker-get-buffer-file-mode edraw-shape-picker-entries-file)))))
;;(edraw-shape-picker-open)

(defun edraw-shape-picker-get-buffer-file-mode (file)
  (if file
      (or
       ;; Existing buffer
       (find-buffer-visiting file)
       ;; Existing file
       (when (file-exists-p file)
         (let ((buffer (find-file-noselect file)))
           (with-current-buffer buffer
             (edraw-shape-picker-file-mode)
             buffer)))
       ;; New buffer
       (edraw-shape-picker-create-buffer-file-mode file))
    ;; New buffer (not yet associated with a file)
    (edraw-shape-picker-create-buffer-file-mode nil)))

(defun edraw-shape-picker-create-buffer-file-mode (file)
  (let ((buffer (generate-new-buffer edraw-shape-picker-buffer-name)))
    (with-current-buffer buffer
      (with-silent-modifications
        (edraw-shape-picker-write-entries buffer edraw-shape-picker-entries-default)
        (when file
          (set-visited-file-name file)))
      (edraw-shape-picker-file-mode))
    buffer))

(defun edraw-shape-picker-pop-to-buffer (buffer)
  (pop-to-buffer buffer)
  (setq-local edraw-shape-picker-initial-window (selected-window))
  buffer)

;; For read-only buffers never associated with a file.
;; Use edraw-shape-picker-ui-mode (not -file-mode).

(defun edraw-shape-picker-open-neverfile (&optional buffer-name)
  (edraw-shape-picker-pop-to-buffer
   (edraw-shape-picker-get-buffer-neverfile buffer-name)))

(defun edraw-shape-picker-get-buffer-neverfile (&optional buffer-name)
  (unless buffer-name
    (setq buffer-name edraw-shape-picker-buffer-name))
  (or
   ;; Existing buffer
   (get-buffer buffer-name)
   ;; New buffer
   (edraw-shape-picker-create-buffer-neverfile buffer-name)))

(defun edraw-shape-picker-create-buffer-neverfile (buffer-name)
  (unless buffer-name
    (setq buffer-name edraw-shape-picker-buffer-name))
  (let ((buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (edraw-shape-picker-set-local-entries
       edraw-shape-picker-entries-default t)
      (edraw-shape-picker-make-buffer-contents) ;;silent modification
      (edraw-shape-picker-ui-mode))
    buffer))

;;

(defun edraw-shape-picker-connect (buffer on-notify)
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'edraw-shape-picker-notification-hook on-notify nil t)))

(defun edraw-shape-picker-disconnect (buffer on-notify)
  (with-current-buffer (or buffer (current-buffer))
    (remove-hook 'edraw-shape-picker-notification-hook on-notify t)
    ;; Close automatically
    (when (and (edraw-shape-picker-notification-hook-empty-p)
               (not edraw-shape-picker-opened-by-user))
      (edraw-shape-picker-close buffer))))

(defun edraw-shape-picker-close (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (when (buffer-live-p buffer)
    ;; Kill Window
    (with-current-buffer buffer
      (if (and edraw-shape-picker-initial-window
               (window-live-p edraw-shape-picker-initial-window))
          (quit-restore-window edraw-shape-picker-initial-window 'bury)
        (when-let ((window (get-buffer-window buffer)))
          (when (window-parent window)
            (delete-window window))))
      (setq-local edraw-shape-picker-initial-window nil))
    ;; Kill buffer?
    (if (buffer-modified-p buffer)
        (message (edraw-msg "Custom shapes have unsaved changes."))
      (kill-buffer buffer))))

;;;; Picker Mode (Picker Buffer Control)
;;;;; Key Map
(defvar edraw-shape-picker-ui-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "q" #'edraw-shape-picker-quit)
    (define-key km "g" #'edraw-shape-picker-refresh)
    (define-key km [mouse-3] #'edraw-shape-picker-quit-by-mouse)
    ;;(define-key km "y" #'edraw-shape-picker-paste-shape-at)
    km))

(defvar edraw-shape-picker-thumbnail-map
  (let ((km (make-sparse-keymap)))
    (define-key km [mouse-1] #'edraw-shape-picker-on-click-thumbnail)
    (define-key km (kbd "RET") #'edraw-shape-picker-select-thumbnail-at)
    (define-key km "e" #'edraw-shape-picker-edit-shape-at)
    (define-key km "R" #'edraw-shape-picker-rename-shape-at)
    (define-key km "D" #'edraw-shape-picker-delete-shape-at)
    ;;(define-key km "w" #'edraw-shape-picker-copy-shape-at)
    ;;(define-key km "W" #'edraw-shape-picker-cut-shape-at)
    km))

(defvar-local edraw-shape-picker-entries nil)
(defvar-local edraw-shape-picker-selected-thumbnail-marker nil)
(defvar-local edraw-shape-picker-selected-thumbnail nil)
(defvar-local edraw-shape-picker-notification-hook nil)
;;;;; Mode

(define-derived-mode edraw-shape-picker-ui-mode nil "EShapes UI"
  "Major mode for edraw shape picker UI.

\\{edraw-shape-picker-ui-mode-map}

\\{edraw-shape-picker-thumbnail-map}"
  (setq-local line-move-visual t))

(defun edraw-shape-picker-notification-hook-empty-p ()
  (null edraw-shape-picker-notification-hook))

(defun edraw-shape-picker-quit ()
  (interactive)
  (edraw-shape-picker-close))

(defun edraw-shape-picker-quit-by-mouse (event)
  (interactive "e")
  (when-let ((startpos (event-start event)) ;;@todo macro?
             (pos (posn-point startpos))
             (window (posn-window startpos))
             (buffer (window-buffer window)))
    (with-current-buffer buffer
      (edraw-shape-picker-close))))

(defun edraw-shape-picker-set-local-entries (entries copy)
  (setq-local edraw-shape-picker-entries
              (if copy
                  (copy-tree entries)
                entries)))

(defun edraw-shape-picker-refresh ()
  (interactive)
  (edraw-shape-picker-make-buffer-contents))

;;;;; Menu on Shape

(defun edraw-shape-picker-open-shape-menu-at ()
  (interactive)
  ;;;@todo impl
  )

;;;;; Rename Shape

(defun edraw-shape-picker-rename-shape-at (pos)
  (interactive "d")
  (pcase (edraw-shape-picker-thumbnail-at pos)
    (`(,thumbnail . (,_beg . ,_end))
     (with-slots (entry) thumbnail
       (let* ((old-name
               (or (edraw-shape-picker-get-shape-entry-property entry :name)
                   ""))
              (new-name
               (read-string (edraw-msg "Input name: ") old-name old-name)))
         (edraw-shape-picker-set-shape-entry-property
          entry :name new-name))))))

;;;;; Edit Shape

;;@todo Combine common parts with `edraw-org-edit-link'. Create a library so that editors can be generated easily.

(defun edraw-shape-picker-edit-shape-at (pos)
  (interactive "d")

  (let ((picker-buffer (current-buffer)))
    (pcase (edraw-shape-picker-thumbnail-at pos)
      (`(,thumbnail . (,_beg . ,_end))
       (with-slots (entry) thumbnail

         ;; Change current buffer
         (pop-to-buffer (generate-new-buffer "*Easy Draw Shape Editor*"))

         (with-silent-modifications
           (insert " "))
         (goto-char (point-min))

         (let* ((body (edraw-shape-picker-shape-to-svg-node-list
                       (edraw-shape-picker-get-shape-entry-shape entry)))
                (svg (edraw-create-document-svg nil nil nil body))
                (editor-overlay (make-overlay (point-min) (point-max)))
                (editor (edraw-editor
                         :overlay editor-overlay
                         :svg svg
                         :document-writer (edraw-shape-picker-edit-make-writer
                                           picker-buffer
                                           entry)
                         :menu-filter #'edraw-shape-picker-edit-menu-filter
                         )))
           (edraw-initialize editor)

           ;; Add key bindings
           (overlay-put editor-overlay 'keymap
                        (edraw-shape-picker-edit-make-keymap
                         (or (overlay-get editor-overlay 'keymap)
                             edraw-editor-map))))
         ;; Hook kill buffer
         (add-hook 'kill-buffer-query-functions 'edraw-buffer-kill-query nil t)

         (message "%s" (substitute-command-keys "\\[edraw-shape-picker-edit-finish-edit]:Finish Edit, \\[edraw-shape-picker-edit-cancel-edit]:Cancel Edit")))))))

(defun edraw-shape-picker-edit-menu-filter (menu-type items)
  (pcase menu-type
    ('main-menu
     (append
      items
      '(((edraw-msg "Finish Edit") edraw-shape-picker-edit-finish-edit)
        ((edraw-msg "Cancel Edit") edraw-shape-picker-edit-cancel-edit))))
    (_ items)))

(defun edraw-shape-picker-edit-make-keymap (original-keymap)
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km original-keymap)
    (define-key km (kbd "C-c C-c") 'edraw-shape-picker-edit-finish-edit)
    (define-key km (kbd "C-c C-k") 'edraw-shape-picker-edit-cancel-edit)
    km))

(defun edraw-shape-picker-edit-finish-edit (&optional editor)
  (interactive)
  (when-let ((editor (or editor (edraw-editor-at-input last-input-event))))
    (when (or (not (edraw-modified-p editor))
              (condition-case err
                  (edraw-save editor)
                (error
                 (yes-or-no-p
                  (format
                   (edraw-msg "Failed to save. %s. Discard changes?")
                   (error-message-string err))))))
      (edraw-shape-picker-edit-close-editor editor))))

(defun edraw-shape-picker-edit-cancel-edit (&optional editor)
  (interactive)
  (when-let ((editor (or editor (edraw-editor-at-input last-input-event))))
    (when (or (null (edraw-modified-p editor))
              (yes-or-no-p (edraw-msg "Discard changes?")))
      (edraw-shape-picker-edit-close-editor editor))))

(defun edraw-shape-picker-edit-close-editor (editor)
  (edraw-close editor)
  (kill-buffer))

(defun edraw-shape-picker-edit-make-writer (picker-buffer entry)
  (lambda (svg)
    (edraw-shape-picker-edit-save-svg svg picker-buffer entry)))

(defun edraw-shape-picker-edit-save-svg (svg picker-buffer entry)
  ;; Move to beginning of editing link
  (unless (buffer-live-p picker-buffer)
    (error "The picker buffer has been removed"))

  ;; Modify ENTRY object and refresh image.
  (with-current-buffer picker-buffer
    (edraw-shape-picker-set-shape-entry-shape
     entry
     ;;@todo add defs
     ;; SVG string
     (mapconcat
      (lambda (node) (edraw-svg-encode node nil nil))
      ;; Under body nodes
      (dom-children (edraw-get-document-body svg))
      ""))))

;;;;; Delete Shape

(defun edraw-shape-picker-delete-shape-at (_pos)
  (interactive "d")
  ;;;@todo impl
  )

;;;;; Kill/Yank Shape

(defun edraw-shape-picker-copy-shape-at (_pos)
  (interactive "d")
  ;;;@todo impl
  )

(defun edraw-shape-picker-cut-shape-at (_pos)
  (interactive "d")
  ;;;@todo impl
  )

(defun edraw-shape-picker-paste-shape-at (_pos)
  (interactive "d")
  ;;;@todo impl
  )

;; Move Shape to Next
;; Move Shape to Previous



;;;; Entry

;;;;; Entry Common

;;;;;; Entry Type

(defun edraw-shape-picker-entry-container-p (entry)
  "Return non-nil if ENTRY can contain child entries."
  (memq (edraw-shape-picker-entry-type entry) '(:section :layout)))

(defun edraw-shape-picker-entry-can-have-name-p (entry)
  (memq (edraw-shape-picker-entry-type entry) '(:section :shape)))

;;;;;; Entry Name

(defun edraw-shape-picker-entry-name-for-msg (entry)
  (if-let ((name-p (edraw-shape-picker-entry-can-have-name-p entry))
           (name (edraw-shape-picker-entry-prop-get entry :name)))
      name
    (substring (symbol-name (edraw-shape-picker-entry-type entry)) 1)))

;;;;;; Entry Modification

(defvar-local edraw-shape-picker-inhibit-refresh nil)

(defun edraw-shape-picker-on-entry-modified (&rest _entries)
  ;;@todo Do not update if entry has not been added yet
  (set-buffer-modified-p t)

  ;; Update thumbnail image
  ;;@todo delay update?
  (unless edraw-shape-picker-inhibit-refresh
    ;;@todo Update only the entry?
    ;;@todo keep point exact
    (let ((point (point)))
      (edraw-shape-picker-refresh)
      (when (<= (point-min) point (point-max))
        (goto-char point)))))

;;;;;; Entry Basic Accessors

;; (:<type> <:keywordN> <valueN> ... <contents>...)

(defun edraw-shape-picker-entry-type (entry)
  (and (consp entry)
       (keywordp (car entry))
       (car entry)))

(defun edraw-shape-picker-entry-prev-contents (entry)
  "Returns the previous link (cons cell) of the ENTRY's contents list."
  (let ((prev entry))
    (while (and (cddr prev)
                (keywordp (cadr prev)))
      (setq prev (cddr prev)))
    prev))
;;TEST: (edraw-shape-picker-entry-prev-contents '(:tag :p1 11 :p2 22 "DAT1" "DAT2")) => (22 "DAT1" "DAT2")
;;TEST: (edraw-shape-picker-entry-prev-contents '(:tag)) => (:tag)
;;TEST: (edraw-shape-picker-entry-prev-contents '(:tag "DAT1")) => (:tag "DAT1")

(defun edraw-shape-picker-entry-contents-get (entry)
  (cdr (edraw-shape-picker-entry-prev-contents entry)))
;;TEST: (edraw-shape-picker-entry-contents-get '(:tag :p1 11 :p2 22 "DAT1" "DAT2")) => ("DAT1" "DAT2")

(defun edraw-shape-picker-entry-contents-set (entry list)
  (prog1 (setcdr (edraw-shape-picker-entry-prev-contents entry) list)
    (edraw-shape-picker-on-entry-modified entry)))

(defun edraw-shape-picker-entry-contents-first-get (entry)
  (car (edraw-shape-picker-entry-contents-get entry)))
;;TEST: (edraw-shape-picker-entry-contents-first-get '(:tag :p1 11 :p2 22)) => nil
;;TEST: (edraw-shape-picker-entry-contents-first-get '(:tag :p1 11 :p2 22 "DAT1" "DAT2")) => "DAT1"

(defun edraw-shape-picker-entry-contents-first-set (entry val)
  (prog1
      (let ((prev (edraw-shape-picker-entry-prev-contents entry)))
        (if (cdr prev)
            (setcar (cdr prev) val)
          (setcdr prev (cons val nil))))
    (edraw-shape-picker-on-entry-modified entry)))

(defun edraw-shape-picker-entry-props (entry)
  "Extract the property list part of the ENTRY."
  (cl-loop for curr on (cdr entry) by #'cddr
           while (and (cdr entry)
                      (keywordp (car curr)))
           nconc (list (car curr) (cadr curr))))

(defun edraw-shape-picker-entry-prop-memq (entry prop)
  (let ((curr (cdr entry)))
    (while (and (cdr curr)
                (keywordp (car curr))
                (not (eq (car curr) prop)))
      (setq curr (cddr curr)))
    (when (eq (car curr) prop)
      curr)))

(defun edraw-shape-picker-entry-prop-get (entry prop)
  (cadr (edraw-shape-picker-entry-prop-memq entry prop)))

(defun edraw-shape-picker-entry-prop-put (entry prop val)
  (let ((prev entry)
        (curr (cdr entry)))
    (while (and (cdr curr)
                (keywordp (car curr))
                (not (eq (car curr) prop)))
      (setq prev (cdr curr)
            curr (cddr curr)))
    (if (eq (car curr) prop)
        (setcar (cdr curr) val)
      (setcdr prev (cons prop (cons val (cdr prev))))))
  (edraw-shape-picker-on-entry-modified entry)
  entry)

;;;;;; Entry Tree

(defun edraw-shape-picker-entry-child-entries (entry)
  (when (edraw-shape-picker-entry-container-p entry)
    ;; Only :section, :layout
    (edraw-shape-picker-entry-contents-get entry)))

(defun edraw-shape-picker-entry-parent (target-entry &optional root)
  (unless root (setq root edraw-shape-picker-entries))
  (let ((children (edraw-shape-picker-entry-child-entries root)))
    (if (memq target-entry children)
        root
      (cl-loop for child in children
               for found = (edraw-shape-picker-entry-parent target-entry child)
               when found
               return found))))

(defun edraw-shape-picker-entry-parent-index (target-entry &optional root)
  "Return ENTRY's parent and index.

(parent . index)"
  (unless root (setq root edraw-shape-picker-entries))
  (cl-loop for index from 0
           for child in (edraw-shape-picker-entry-child-entries root)
           when (eq child target-entry) return (cons root index)
           for found = (edraw-shape-picker-entry-parent-index target-entry
                                                              child)
           when found return found))

(defun edraw-shape-picker-entry-path (target-entry &optional root)
  "Return path from ROOT to TARGET-ENTRY.

((root . index) (ancestor . index) ... (parent . index))."
  (unless root (setq root edraw-shape-picker-entries))
  (cl-loop for index from 0
           for child in (edraw-shape-picker-entry-child-entries root)
           when (eq child target-entry) return (cons (cons root index) nil)
           for found = (edraw-shape-picker-entry-path target-entry child)
           when found return (cons (cons root index) found)))

(defun edraw-shape-picker-entry-remove-child (parent child)
  (when (and parent child
             (edraw-shape-picker-entry-container-p parent))
    (let ((prev (edraw-shape-picker-entry-prev-contents parent)))
      (while (and (cdr prev)
                  (not (eq (cadr prev) child)))
        (setq prev (cdr prev)))
      (when (eq (cadr prev) child)
        (setcdr prev (cddr prev))
        (edraw-shape-picker-on-entry-modified parent)
        ;; Return removed entry
        child))))

(defun edraw-shape-picker-entry-remove (entry &optional root)
  (unless root (setq root edraw-shape-picker-entries))
  (edraw-shape-picker-entry-remove-child
   (edraw-shape-picker-entry-parent entry root)
   entry))

(defun edraw-shape-picker-entry-insert (parent index child)
  "The CHILD becomes the PARENT's INDEX-th child."
  ;;@todo check CHILD already has parent?
  (when (and parent child
             (edraw-shape-picker-entry-container-p parent))
    (let ((prev (nthcdr index (edraw-shape-picker-entry-prev-contents parent))))
      (setcdr prev (cons child (cdr prev)))
      (edraw-shape-picker-on-entry-modified parent)
      ;; Return inserted entry
      child)))

(defun edraw-shape-picker-entries-insert (parent index children)
  "The first of CHILDREN becomes the PARENT's INDEX-th child.

CHILDREN list is not copied. At the end of Children, the
subsequent entry will be connected."
  ;;@todo check CHILDREN already has parent?
  (when (and parent children
             (edraw-shape-picker-entry-container-p parent))
    (let ((prev (nthcdr index (edraw-shape-picker-entry-prev-contents parent))))
      (setcdr (last children) (cdr prev))
      (setcdr prev children)
      (edraw-shape-picker-on-entry-modified parent)
      ;; Return inserted entry
      children)))

(defun edraw-shape-picker-entries-insert-at (entries pos)
  (if-let ((parent-index (edraw-shape-picker-entry-insertion-point-at pos)))
      (edraw-shape-picker-entries-insert (car parent-index) (cdr parent-index)
                                         entries)
    (message (edraw-msg "Failed to find insertion point"))))


;;;;; Section

;;;;; Layout

;;;;; Shape

(defun edraw-shape-picker-shape-entry-name-get (entry)
  "Return the name of the shape ENTRY."
  (edraw-shape-picker-entry-prop-get entry :name))

(defun edraw-shape-picker-shape-entry-shape-get (entry)
  "Return the shape definition of the shape ENTRY."
  (edraw-shape-picker-entry-contents-first-get entry))

(defun edraw-shape-picker-shape-entry-shape-set (entry shape)
  "Change the SHAPE definition of the shape ENTRY."
  (edraw-shape-picker-entry-contents-first-set entry shape))

(defconst edraw-shape-picker-thumbnail-param-names
  '(width height max-width max-height
          padding background foreground-selected margin))

(defun edraw-shape-picker-shape-entry-thumbnail-params (entry)
  (let (;; from custom variables
        (params (mapcar
                 (lambda (name)
                   (cons
                    name
                    (symbol-value
                     (intern (format "edraw-shape-picker-thumbnail-%s" name)))))
                 edraw-shape-picker-thumbnail-param-names)))

    ;; from ancestors
    (dolist (ancestor (mapcar #'car ;;discard index
                              (edraw-shape-picker-entry-path entry)))
      (dolist (name edraw-shape-picker-thumbnail-param-names)
        (when-let ((prop (edraw-shape-picker-entry-prop-memq
                          ancestor
                          (intern (format ":thumbnail-%s" name)))))
          (setf (alist-get name params) (cadr prop)))))

    ;; from shape entry
    (dolist (name edraw-shape-picker-thumbnail-param-names)
      (when-let ((prop (edraw-shape-picker-entry-prop-memq
                        entry
                        (intern (format ":%s" name)))))
        (setf (alist-get name params) (cadr prop))))

    params))

;;;; Thumbnail Image

(defun edraw-shape-picker-shape-to-svg-node-list (shape)
  (cond
   ((stringp shape)
    (dom-children (edraw-svg-decode (concat "<g>" shape "</g>") nil)))
   ;;@todo Convert shape-descriptor to svg
   ((edraw-dom-element-p shape) (list shape))))

(defun edraw-shape-picker-create-thumbnail-image (entry &optional selected)
  (let* ((params (edraw-shape-picker-shape-entry-thumbnail-params entry))
         (shape (edraw-shape-picker-shape-entry-shape-get entry))
         (svg-node-list (edraw-shape-picker-shape-to-svg-node-list shape)))
    (when svg-node-list
      (svg-image (edraw-svg-shape-thumbnail
                  ;; to single element
                  (if (<= (length svg-node-list) 1)
                      (car svg-node-list)
                    (apply #'dom-node 'g nil svg-node-list))
                  (alist-get 'width params)
                  (alist-get 'height params)
                  (alist-get 'padding params)
                  (alist-get 'background params)
                  (when selected
                    (alist-get 'foreground-selected params))
                  (alist-get 'max-width params)
                  (alist-get 'max-height params))
                 :ascent 'center
                 :margin (alist-get 'margin params)))))

;;;; Shape Selection

(defun edraw-shape-picker-selected-shape-entry ()
  edraw-shape-picker-selected-shape-entry)

(defun edraw-shape-picker-selected-shape ()
  (when edraw-shape-picker-selected-shape-entry
    (edraw-shape-picker-shape-entry-shape-get
     edraw-shape-picker-selected-shape-entry)))

(defun edraw-shape-picker-ensure-deselect-shape (shape)
  (when (eq edraw-shape-picker-selected-shape-entry shape)
    (edraw-shape-picker-deselect-shape)))

(defun edraw-shape-picker-deselect-shape ()
  (when edraw-shape-picker-selected-shape-entry
    ;; Update Image
    (when-let ((range (edraw-shape-picker-find-entry-text
                       edraw-shape-picker-selected-shape-entry)))
      (with-silent-modifications
        (put-text-property (car range) (cdr range)
                           'display
                           (edraw-shape-picker-create-thumbnail-image
                            edraw-shape-picker-selected-shape-entry
                            nil))))
    ;; Change Variables
    (setq-local edraw-shape-picker-selected-shape-entry nil)
    ;; Notify Changes
    (edraw-shape-picker-notify 'deselect)))

(defun edraw-shape-picker-select-shape-at (pos)
  (interactive "d")
  (when-let ((entry (edraw-shape-picker-entry-at pos))
             (range (edraw-shape-picker-lookup-text-entry-range pos))
             (beg (car range))
             (end (cdr range)))
    (edraw-shape-picker-deselect-shape)
    ;; Update Image
    (with-silent-modifications
      (put-text-property (car range) (cdr range)
                         'display
                         (edraw-shape-picker-create-thumbnail-image entry t)))
    ;; Change Variables
    (setq-local edraw-shape-picker-selected-shape-entry entry)
    ;; Message
    (message
     (edraw-msg "Select %s")
     (or (edraw-shape-picker-shape-entry-name-get entry)
         (edraw-msg "<no name>")))
    ;; Notify Changes
    (edraw-shape-picker-notify
     'select
     (edraw-shape-picker-shape-entry-shape-get entry))))

(defun edraw-shape-picker-on-click-thumbnail (event)
  (interactive "e")
  (when-let ((startpos (event-start event)) ;;@todo macro?
             (pos (posn-point startpos))
             (window (posn-window startpos))
             (buffer (window-buffer window)))
    (with-current-buffer buffer
      (edraw-shape-picker-select-shape-at pos))))


;;;; Buffer Contents

(defun edraw-shape-picker-insert-text (str)
  (insert (propertize str
                      'read-only t
                      'front-sticky t)))

;;;;; Create

(defvar-local edraw-shape-picker-section-level 0)

(defun edraw-shape-picker-make-buffer-contents ()
  (let ((edraw-shape-picker-section-level 0))
    (with-silent-modifications
      (erase-buffer)
      (edraw-shape-picker-insert-section edraw-shape-picker-entries)
      (unless (bolp)
        (edraw-shape-picker-insert-text "\n"))
      (goto-char (point-min)))))

(defun edraw-shape-picker-insert-entries (entries)
  (dolist (entry entries)
    (edraw-shape-picker-insert-entry entry)))

(defun edraw-shape-picker-insert-entry (entry)
  (pcase (edraw-shape-picker-entry-type entry)
    (:section (edraw-shape-picker-insert-section entry))
    (:layout  (edraw-shape-picker-insert-layout entry))
    (:shape   (edraw-shape-picker-insert-shape entry))))

(defun edraw-shape-picker-insert-section (entry)
  ;; (:section
  ;;     :name <string>
  ;;     <entry>...)
  (unless (bolp)
    (edraw-shape-picker-insert-text "\n"))
  (let ((edraw-shape-picker-section-level
         (1+ edraw-shape-picker-section-level))
        (name (edraw-shape-picker-entry-prop-get entry :name)))
    (edraw-shape-picker-insert-text
     (propertize
      (concat
       ;; *
       (propertize
        (make-string edraw-shape-picker-section-level ?*)
        'display (concat (make-string (1- edraw-shape-picker-section-level) ? )
                         "\u29bf"))
       ;;  <name>\n
       (propertize
        (concat (or (concat " " name) "") "\n")))
      'edraw-shape-picker-entry entry))

    (edraw-shape-picker-insert-entries
     (edraw-shape-picker-entry-contents-get entry))))

(defun edraw-shape-picker-insert-layout (entry)
  ;; (:layout
  ;;     :thumbnail-width <integer>
  ;;     :thumbnail-height <integer>
  ;;     :thumbnail-max-width <integer>
  ;;     :thumbnail-max-height <integer>
  ;;     :thumbnail-padding <padding-spec>
  ;;     :thumbnail-background <svg-attrs-spec>
  ;;     :thumbnail-foreground-selected <svg-attrs-spec>
  ;;     :thumbnail-margin <margin-spec>
  ;;     <entry>...)
  ;;
  ;; see: `edraw-shape-picker-thumbnail-*' variables.
  (edraw-shape-picker-insert-entries
   (edraw-shape-picker-entry-contents-get entry)))

(defun edraw-shape-picker-insert-shape (entry)
  ;; (:shape
  ;;     :width <integer>
  ;;     :height <integer>
  ;;     :max-width <integer>
  ;;     :max-height <integer>
  ;;     :padding <padding-spec>
  ;;     :background <svg-attrs-spec>
  ;;     :foreground-selected <svg-attrs-spec>
  ;;     :margin <margin-spec>
  ;;     <shape-def>)
  ;;
  ;; see: `edraw-shape-picker-thumbnail-*' variables.
  ;;
  ;; <shape-def> :
  ;;     <svg-string>
  ;;     <svg-node-list>
  ;;     ;;<shape-descriptor-list>

  ;; Space
  (when (and edraw-shape-picker-string-between-thumbnails
             (not (bolp)))
    (edraw-shape-picker-insert-text edraw-shape-picker-string-between-thumbnails))

  ;; Thumbnail
  (let ((name (edraw-shape-picker-shape-entry-name-get entry)))
    (edraw-shape-picker-insert-text
     (propertize
      (format " [%s] " (or name ""))
      'display (edraw-shape-picker-create-thumbnail-image
                entry
                ;; selected?
                (eq (edraw-shape-picker-selected-shape-entry) entry))
      'rear-nonsticky t
      'keymap edraw-shape-picker-thumbnail-map
      'pointer 'hand
      'edraw-shape-picker-entry entry
      'help-echo name
      ))))

;;;;; Detect Entry

(defun edraw-shape-picker-entry-at (pos)
  (get-text-property pos 'edraw-shape-picker-entry))

(defun edraw-shape-picker-lookup-text-prop-range (pos prop val)
  (cons
   (if (or (= pos (point-min))
           (not (eq (get-text-property (1- pos) prop)
                    val)))
       pos
     (previous-single-property-change pos prop))
   (next-single-property-change pos prop)))

(defun edraw-shape-picker-lookup-text-entry-range (pos)
  (when-let ((entry (get-text-property pos 'edraw-shape-picker-entry)))
    (edraw-shape-picker-lookup-text-prop-range pos 'edraw-shape-picker-entry
                                               entry)))

(defun edraw-shape-picker-shape-entry-at (pos)
  (when-let ((entry (edraw-shape-picker-entry-at pos)))
    (when (eq (edraw-shape-picker-entry-type entry) :shape)
      entry)))

(defun edraw-shape-picker-find-entry-text (entry)
  (save-excursion
    (goto-char (point-min))
    (when-let ((match-data (text-property-search-forward
                            'edraw-shape-picker-entry entry #'eq)))
      (cons
       (prop-match-beginning match-data)
       (prop-match-end match-data)))))

(defun edraw-shape-picker-entry-insertion-point-at (pos)
  "Detect entry insertion point at POS."
  (if-let* ((entry-at-pos (edraw-shape-picker-entry-at pos))
            (range (edraw-shape-picker-lookup-text-entry-range pos))
            (parent-index (edraw-shape-picker-entry-parent-index entry-at-pos))
            (parent (car parent-index))
            (index (cdr parent-index)))
      ;; POS points to an entry
      (if (= (car range) pos)
          ;; before the entry
          ;; |* SECTION
          ;; [shape][shape]|[shape]
          parent-index
        (pcase (edraw-shape-picker-entry-type entry-at-pos)
          (:section
           ;; first child of the entry
           ;; * SECTION|
           (cons entry-at-pos 0))
          (:shape
           ;; after the entry
           ;; [shape] [shape] [sh|ape]
           (cons parent (1+ index)))))
    ;; POS points between entries
    ;; [shape][shape][shape]|
    (when-let ((entry (edraw-shape-picker-search-backward-entry-pos pos nil))
               (parent-index (edraw-shape-picker-entry-parent-index entry))
               (parent (car parent-index))
               (index (cdr parent-index)))
      (cons parent (1+ index)))))

(defun edraw-shape-picker-search-backward-entry-pos (pos include-pos-p)
  (unless include-pos-p
    (setq pos (1- pos)))
  (let (entry)
    (while (and (>= pos (point-min))
                (null
                 (setq entry
                       (get-text-property pos 'edraw-shape-picker-entry))))
      (setq pos (1- pos))) ;;@todo use previous-property-change?
    (when entry
      (cons entry pos))))



;;;; File I/O

(defun edraw-shape-picker-write-entries (buffer entries)
  (with-current-buffer (or buffer (current-buffer))
    (insert ";; Custom Shape Definition for edraw-shape-picker\n")
    (pp (list
         (cons :format 'edraw-shape-picker-entries)
         (cons :version 1)
         (cons :entries entries))
        (current-buffer))))

(defun edraw-shape-picker-save-entries (file entries)
  (with-temp-file file
    (edraw-shape-picker-write-entries (current-buffer) entries)))
;;TEST (edraw-shape-picker-save-entries edraw-shape-picker-entries-file edraw-shape-picker-entries)

(defun edraw-shape-picker-read-entries (buffer)
  (let* ((sexp (read (or buffer (current-buffer))))
         (format (alist-get :format sexp))
         (version (alist-get :version sexp))
         (entries (alist-get :entries sexp)))
    (unless (eq format 'edraw-shape-picker-entries)
      (error "Invalid file format: %s" format))
    (unless (eq version 1)
      (error "Invalid file version: %s" version))
    entries))

(defun edraw-shape-picker-load-entries (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (edraw-shape-picker-read-entries (current-buffer))))
;;TEST (edraw-shape-picker-load-entries edraw-shape-picker-entries-file)


;;;; Picker File Mode

(defvar edraw-shape-picker-file-mode-map
  (let ((km (make-sparse-keymap)))
    km))

(defun edraw-shape-picker-file-mode ()
  "Major mode for visually editing .eshapes.el files.

Like `hexl-mode', open a .eshapes.el file and then launch this
mode to use it. Run `edraw-shape-picker-file-mode-exit' to return
to the original mode.

The following commands are available in buffer:

\\{edraw-shape-picker-ui-mode-map}

The following commands are available on thumbnails:

\\{edraw-shape-picker-thumbnail-map}"
  (interactive)

  (delay-mode-hooks
    (unless (eq major-mode 'edraw-shape-picker-file-mode)
      (major-mode-suspend)

      ;; Visualize
      (let ((entries (progn
                       ;; Parse buffer
                       (goto-char (point-min))
                       (edraw-shape-picker-read-entries (current-buffer))))
            (modified (buffer-modified-p)))
        ;; When parsing is successful

        ;; Call parent mode (and kill all local variables)
        (edraw-shape-picker-ui-mode)

        ;; Discard undo list
        (setq buffer-undo-list nil)

        ;; Disable auto save
        (auto-save-mode -1)

        ;; Refresh buffer contents
        (edraw-shape-picker-set-local-entries entries nil)
        (edraw-shape-picker-make-buffer-contents) ;;silent modification

        (restore-buffer-modified-p modified))

      ;;  Key maps
      (set-keymap-parent edraw-shape-picker-file-mode-map (current-local-map))
      (use-local-map edraw-shape-picker-file-mode-map)
      ;;  Mode
      (setq-local mode-name "EShapes")
      (setq-local major-mode 'edraw-shape-picker-file-mode)
      ;;  Hooks
      (add-hook 'write-contents-functions #'edraw-shape-picker-file-mode--save-buffer nil t)
      (add-hook 'change-major-mode-hook #'edraw-shape-picker-file-mode--maybe-devisualize nil t)
      (setq-local revert-buffer-function #'edraw-shape-picker-file-mode--revert-buffer-function)
      ))
  (run-mode-hooks 'edraw-shape-picker-file-mode-hook))

(defun edraw-shape-picker-file-mode--exit ()
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (edraw-shape-picker-file-mode--devisualize)
    (restore-buffer-modified-p modified))

  (remove-hook 'write-contents-functions #'edraw-shape-picker-file-mode--save-buffer t)
  (remove-hook 'change-major-mode-hook #'edraw-shape-picker-file-mode--maybe-devisualize t)
  (setq-local revert-buffer-function nil))

(defun edraw-shape-picker-file-mode-exit ()
  (interactive)
  (edraw-shape-picker-file-mode--exit)
  (major-mode-restore))

(defun edraw-shape-picker-file-mode--maybe-devisualize ()
  (when (y-or-n-p "Convert contents back to binary format? ")
    (edraw-shape-picker-file-mode--exit)))

(defun edraw-shape-picker-file-mode--revert-buffer-function (_ignore-auto _noconfirm)
  ;; see: hexl-revert-buffer-function
  (let (revert-buffer-function)
    (revert-buffer nil nil t)
    (remove-hook 'change-major-mode-hook #'edraw-shape-picker-file-mode--maybe-devisualize t)
    (setq major-mode 'fundamental-mode)
    (edraw-shape-picker-file-mode)))

(defvar edraw-shape-picker-file-mode--in-save-buffer nil)

(defun edraw-shape-picker-file-mode--save-buffer ()
  (cond
   (edraw-shape-picker-file-mode--in-save-buffer
    nil) ;; Ignore
   ((not (buffer-modified-p))
    (message "(No changes need to be saved)")
    t) ;; Saved
   (t
    ;; Backup current buffer to temp buffer
    (let* ((buffer (current-buffer))
           (buffer-start (point-min))
           (buffer-end (point-max))
           (buffer-len (- buffer-end buffer-start))
           (inhibit-read-only t))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer))
              (temp-buffer-start (point))
              (temp-buffer-end (+ (point) buffer-len)))
          (insert-buffer-substring buffer buffer-start buffer-end)
          ;; Save devisualized buffer
          (with-current-buffer buffer
            (edraw-shape-picker-file-mode--devisualize)
            (let ((edraw-shape-picker-file-mode--in-save-buffer t))
              (save-buffer)) ;;Recursive!!
            ;; Restore(Visualize)
            (let ((modified (buffer-modified-p)))
              (delete-region (point-min) (point-max))
              (insert-buffer-substring temp-buffer temp-buffer-start temp-buffer-end)
              (restore-buffer-modified-p modified))))))
    t))) ;; Saved

(defun edraw-shape-picker-file-mode--devisualize ()
  (delete-region (point-min) (point-max))
  (edraw-shape-picker-write-entries (current-buffer) edraw-shape-picker-entries))


(provide 'edraw-shape-picker)
;;; edraw-shape-picker.el ends here
