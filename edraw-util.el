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

(require 'mwheel)
(require 'cl-lib)
(require 'cl-print)
(require 'seq)
(require 'subr-x)
(require 'eieio)
(require 'edraw-msg)

;;;; Customize

(defgroup edraw nil
  "A drawing environment that runs within Emacs."
  :tag "Edraw"
  :prefix "edraw-"
  :group 'multimedia)


;;;; Debug

(eval-and-compile
  (defcustom edraw-log-generate nil
    "Non-nil means generate logging code."
    :group 'edraw :type 'boolean))

(defcustom edraw-log-output nil
  "Non-nil means to output logs."
  :group 'edraw :type 'boolean)

(defcustom edraw-log-output-regexp nil
  "Regular expression that matches FORMAT-STRING. Only matches are output."
  :group 'edraw :type '(choice (const nil) regexp))

(defun edraw-log-watch (regexp)
  (interactive (list (read-regexp "Regexp matching FORMAT-STRING: ")))
  (if (and (stringp regexp) (not (string-empty-p regexp)))
      (setq edraw-log-output t
            edraw-log-output-regexp regexp)
    (setq edraw-log-output nil
          edraw-log-output-regexp nil)
    (message "Disabled log output")))

(defun edraw-log--match-p (format-string)
  (and edraw-log-output
       (or (null edraw-log-output-regexp)
           (string-match-p edraw-log-output-regexp format-string))
       format-string))

(defun edraw-log--message (format-string &rest args)
  (when (or (null edraw-log-output-regexp)
            (string-match-p edraw-log-output-regexp format-string))
    (apply #'message (concat "edraw: " format-string) args)))

(defmacro edraw-log (format-string &rest args)
  (when edraw-log-generate
    `(let ((fmt (edraw-log--match-p ,format-string)))
       (when fmt
         (edraw-log--message fmt ,@args)))))

(defmacro edraw-log-when (cond format-string &rest args)
  (when edraw-log-generate
    `(let ((fmt (edraw-log--match-p ,format-string)))
       (when (and fmt ,cond)
         (edraw-log--message fmt ,@args)))))

(defmacro edraw-log-unless (cond format-string &rest args)
  (when edraw-log-generate
    `(let ((fmt (edraw-log--match-p ,format-string)))
       (when (and fmt (not ,cond))
         (edraw-log--message fmt ,@args)))))

(defmacro edraw-assert (cond msg)
  (when edraw-log-generate
    `(when (and edraw-log-output (not ,cond))
       (display-warning 'edraw
                        (format-message
                         (concat ,msg " : Assertion failed: %s")
                         ,(prin1-to-string cond))))))

(defvar edraw-log-stringize-depth 0)
(defvar edraw-log-stringize-print-level 2)

;; (defun edraw-log-stringize (object)
(cl-defgeneric edraw-log-stringize (object)
  (let ((print-level edraw-log-stringize-print-level)
        (print-length 12))
    (cl-prin1-to-string object)))

(cl-defmethod edraw-log-stringize ((object list))
  (cond
   ((null object)
    "nil")
   ((>= edraw-log-stringize-depth
        (or print-level edraw-log-stringize-print-level))
    "(...)")
   (t
    (let ((edraw-log-stringize-depth (1+ edraw-log-stringize-depth))
          (result (concat "(" (edraw-log-stringize (car object)))))
      (setq object (cdr object))
      (while (consp object)
        (setq result (concat result " " (edraw-log-stringize (car object))))
        (setq object (cdr object)))
      (when object
        (setq result (concat result " . " (edraw-log-stringize object))))
      (concat result ")")))))
;; TEST: (edraw-log-stringize nil) => "nil"
;; TEST: (edraw-log-stringize '(1 . 2)) => "(1 . 2)"
;; TEST: (edraw-log-stringize '((1 . 2) . (3 . 4))) => "((1 . 2) 3 . 4)"

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

;;;; File Coding Systems

(defun edraw-insert-xml-file-contents (path)
  "Insert the contents of the XML file into the buffer.

PATH is the path to the XML file.

UTF-8 is the preferred character encoding for the file.

The encoding specified in the XML declaration is also taken into account
by the `auto-coding-functions'."
  ;; The default value of `auto-coding-functions' includes
  ;; `sgml-xml-auto-coding-function', but set it just to be safe.
  (let ((auto-coding-functions '(sgml-xml-auto-coding-function)))
    ;; Prefer UTF-8 over the `coding-system-priority-list' setting.
    (with-coding-priority '(utf-8)
      (insert-file-contents path))))

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

(defconst edraw-y-or-n-p-function
  (pcase system-type
    ('android #'y-or-n-p)
    ('windows-nt #'edraw-y-or-n-p--popup-menu)
    ;; I don't know much about other platforms
    (_ #'y-or-n-p)))

(defun edraw-y-or-n-p (prompt)
  (funcall edraw-y-or-n-p-function prompt))

(defun edraw-y-or-n-p--popup-menu (prompt)
  (if (edraw-use-dialog-box-p)
      (x-popup-menu
       t
       (list prompt
             (cons ""
                   (list (cons (edraw-msg "Yes") t)
                         (cons (edraw-msg "No") nil)))))
    (y-or-n-p prompt)))

(defcustom edraw-popup-menu-style 'x
  "How to display menus."
  :group 'edraw
  :type '(choice (const :tag "Determine with `use-dialog-box-p'" nil)
                 (const :tag "Use `tmm-prompt'" tmm)
                 (const :tag "Use `x-popup-menu'" x)))

(defun edraw-popup-menu-style ()
  (if (memq edraw-popup-menu-style '(x tmm))
      edraw-popup-menu-style
    (if (edraw-use-dialog-box-p)
        'x
      'tmm)))

(defun edraw-use-dialog-box-p ()
  (if (fboundp 'use-dialog-box-p) ;; Emacs 28 or later
      (use-dialog-box-p)
    ;; Emacs 27 behavior
    (and (display-popup-menus-p)
         last-input-event
         (listp last-nonmenu-event)
         use-dialog-box)))

(defun edraw-popup-menu (name items &rest args)
  (if (eq (edraw-popup-menu-style) 'tmm)
      (edraw-popup-menu--tmm name items args)
    (edraw-popup-menu--x name items args)))

(defun edraw-popup-menu--x-position ()
  (let* ((pos (event-start last-input-event))
         (xy (posn-x-y pos)))
    (list (list (car xy) (cdr xy)) (posn-window pos))))

(defun edraw-popup-menu--x (name items args)
  (let* ((menu-map (edraw-make-menu-map name items))
         (events (x-popup-menu (edraw-popup-menu--x-position) menu-map))
         (fn (lookup-key menu-map (apply 'vector events))))
    (cond
     ((commandp fn)
      (call-interactively fn))
     ((functionp fn)
      (apply fn args)))))

(defun edraw-popup-menu--tmm (name items args)
  (let* ((menu-map (edraw-make-menu-map name items))
         (fn (tmm-prompt menu-map nil nil t)))
    (cond
     ((commandp fn)
      (call-interactively fn))
     ((functionp fn)
      (apply fn args)))))


(defun edraw-popup-menu-call-interactively (name items)
  (let* ((menu-map (edraw-make-menu-map name items))
         (events (x-popup-menu t menu-map))
         (fn (lookup-key menu-map (apply 'vector events))))
    (when (functionp fn)
      (call-interactively fn))))

(defun edraw-make-menu-map (name items)
  ;; Support name=nil items=("Main Menu" ((item1) (item2)...)) format
  ;;   => name="Main Menu" items=((item1) (item2)...)
  (when (stringp (car items))
    (unless name
      (setq name (car items)))
    (setq items (cadr items)))

  (nconc
   (list 'keymap)
   ;; Menu Name
   (when name
     (list
      (if (and (consp name) (functionp (car name)))
          (eval name)
        name)))
   ;; Items
   (cl-loop with result = nil
            for (item-name binding . props) in items
            for id from 0
            for key = (intern (format "item%s" id))
            ;; Remove invisible item
            when (if-let* ((visible (plist-member props :visible)))
                     (progn
                       ;; Remove :visible property
                       (setq props
                             (edraw-plist-remove-first-key props :visible))
                       ;; Eval visible property
                       (eval (cadr visible)))
                   t)
            do
            (cond
             ;; ("--<???>" nil . <properties>) => Separator
             ((and (stringp item-name)
                   (string-prefix-p "--" item-name)
                   (null binding))
              (push (nconc (list key 'menu-item item-name binding) props)
                    result))
             ;; (<item-name> nil . <properties>) => Ignore
             ((null binding) )
             ;; (<item-name> <function> . <properties>)
             ((or (symbolp binding) (functionp binding))
              (push (nconc (list key 'menu-item item-name binding) props)
                    result))
             ;; (<item-name> <submenu> . <properties>)
             ((listp binding)
              (let ((sub-km (edraw-make-menu-map nil binding)))
                (when (cdr sub-km)
                  (push (nconc (list key 'menu-item item-name sub-km) props)
                        result))))
             (t (error "Unkonwn menu item binding %s"
                       (prin1-to-string binding))))
            finally return (nreverse result))))


;;;; Input Events

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Input-Events.html

;;;;; Touchscreen Events

(defsubst edraw-touch-event-p (event)
  "Return non-nil if EVENT is a touchscreen event, and nil otherwise."
  (memq (car-safe event)
        '(touchscreen-begin touchscreen-end touchscreen-update)))

(defsubst edraw-touch-event-begin-p (event)
  "Return non-nil if EVENT is a touchscreen-begin event, and nil otherwise."
  (eq (car-safe event) 'touchscreen-begin))

(defsubst edraw-touch-event-end-p (event)
  "Return non-nil if EVENT is a touchscreen-end event, and nil otherwise."
  (eq (car-safe event) 'touchscreen-end))

(defsubst edraw-touch-event-update-p (event)
  "Return non-nil if EVENT is a touchscreen-update event, and nil otherwise."
  (eq (car-safe event) 'touchscreen-update))

(defsubst edraw-touch-event-begin-or-end-p (event)
  "Return non-nil if EVENT is a touchscreen-begin or touchscreen-end event,
and nil otherwise."
  (memq (car-safe event) '(touchscreen-begin touchscreen-end)))

(defsubst edraw-touch-event-point (event)
  "Return (ID . POSN) of touchscreen-begin or -end EVENT."
  (when (edraw-touch-event-begin-or-end-p event)
    (cadr event)))

(defsubst edraw-touch-event-id (event)
  "Return ID of touchscreen-begin or -end EVENT."
  (when (edraw-touch-event-begin-or-end-p event)
    (caadr event)))

(defsubst edraw-touch-event-posn (event)
  "Return POSN of touchscreen-begin or -end EVENT."
  (when (edraw-touch-event-begin-or-end-p event)
    (cdadr event)))

(defsubst edraw-touch-event-cancelled (event)
  "Return CANCELLED flag of touchscreen-end EVENT."
  (when (edraw-touch-event-end-p event)
    (caddr event)))

(defsubst edraw-touch-event-points (update-event)
  "Return a list of (ID . POSN) of UPDATE-EVENT."
  (when (edraw-touch-event-update-p update-event)
    (cadr update-event)))

(defsubst edraw-touch-event-assoc (event id)
  "If touchscreen EVENT has a point matching ID, return its POSN."
  (pcase (car-safe event)
    ('touchscreen-update
     ;; (touchscreen-update ( (id . posn) ... ))
     (cdr (assoc id (cadr event))))
    ((or 'touchscreen-begin 'touchscreen-end)
     ;; (touchscreen-(begin|end) (id . posn)
     (let ((point (cadr event)))
       (when (eq (car point) id)
         (cdr point))))))

;;;;; Read Event

(defun edraw-read-event-silent (&optional not-keep-echo-area)
  ;; Suppress display of events
  ;; (e.g. down-mouse-1-) in echo area
  (let ((echo-keystrokes 0))
    (if not-keep-echo-area
        (read-event)
      (let ((old-message (current-message)))
        (prog1 (read-event)
          (edraw-echo old-message))))))

;;;;; Track Drag Operation

(cl-defun edraw-track-drag (down-event
                            on-move
                            &key
                            on-up on-leave target
                            allow-pointer-shape-change-p
                            allow-out-of-target-p
                            not-keep-echo-area
                            need-intermediate-points)
  (cond
   ;; Start with touchscreen-begin event
   ((eq (car-safe down-event) 'touchscreen-begin)
    (edraw-track-drag--touchscreen
     down-event on-move on-up on-leave target allow-out-of-target-p
     not-keep-echo-area
     need-intermediate-points))
   ;; Start with down-mouse-(1|2|3) event
   ((memq 'down (event-modifiers down-event))
    (edraw-track-drag--mouse
     down-event on-move on-up on-leave target allow-out-of-target-p
     not-keep-echo-area
     allow-pointer-shape-change-p))
   (t
    (error "down-event is not down event. %s" (event-modifiers down-event)))))

(defun edraw-track-drag--mouse (down-event
                                on-move
                                on-up on-leave target
                                allow-out-of-target-p
                                not-keep-echo-area
                                allow-pointer-shape-change-p)
  (edraw-log "Track Drag: mouse")
  (let* ((down-basic-type (event-basic-type down-event))
         (down-posn (event-start down-event))
         (result nil)
         ;; Generate detailed movement events even on characters,
         ;; fringes and scrollbars
         (mouse-fine-grained-tracking t))
    (track-mouse ;; Enable mouse-movement events
      (unless allow-pointer-shape-change-p
        (setq track-mouse 'dragging))
      (while (null result)
        (let ((event (edraw-read-event-silent not-keep-echo-area)))
          (edraw-log "Track Drag: Read event %s" (car-safe event))
          (cond
           ;; mouse-movement
           ((mouse-movement-p event)
            (when-let* ((posn (event-start event)))
              (setq result (edraw-track-drag--on-move
                            event on-move on-leave target
                            allow-out-of-target-p down-posn))))
           ;; mouse up
           ((or
             (and (eq (event-basic-type event) down-basic-type)
                  (or (memq 'click (event-modifiers event))
                      (memq 'drag (event-modifiers event))))
             ;; Just in case.
             ;; If touchscreen-begin is converted to down-mouse-1, then
             ;; up-mouse-1 will not come and touchscreen-end will come.
             (eq (car-safe event) 'touchscreen-end))
            (setq result (edraw-track-drag--on-up event on-up)))
           (t
            (setq result (edraw-track-drag--on-unknown-event event on-up)))))))
    result))

(defconst edraw-track-drag--skip-move-delay 0.3)

(defun edraw-track-drag--touchscreen (down-event
                                      on-move
                                      on-up on-leave target
                                      allow-out-of-target-p
                                      not-keep-echo-area
                                      need-intermediate-points)
  (edraw-log "Track Drag: touchscreen")
  (let* ((down-posn (event-start down-event))
         (down-point-id (caadr down-event))
         (pending-events nil)
         (timer nil)
         (result nil)
         on-timer)
    (setq on-timer
          (lambda ()
            (setq timer nil)
            (when pending-events
              (edraw-log "Track Drag: Dispatch pending-events %d"
                         (length pending-events))
              ;; Use only the last event if not NEED-INTERMEDIATE-POINTS
              (when (and (not need-intermediate-points) (cdr pending-events))
                (edraw-log "Track Drag: Skip delayed events %d"
                           (1- (length pending-events)))
                (setcdr pending-events nil))

              ;; Reorder events from oldest to newest
              (setq pending-events (nreverse pending-events))

              ;; Dispatch on-move or on-leave
              (while (and (null result) pending-events)
                (let* ((event-time-posn (pop pending-events))
                       (event-time (car event-time-posn))
                       (event-posn (cdr event-time-posn))
                       (delay (- (float-time) event-time)))
                  (when (or (null pending-events)
                            (< delay edraw-track-drag--skip-move-delay))
                    (setq result
                          (edraw-track-drag--on-move
                           ;; Convert to mouse-movement event
                           (list 'mouse-movement event-posn)
                           on-move on-leave target allow-out-of-target-p
                           down-posn))))))))
    (while (null result)
      (let* ((event (edraw-read-event-silent not-keep-echo-area))
             (posn (edraw-touch-event-assoc event down-point-id)))
        (edraw-log "Track Drag: Read event %s" (car-safe event))

        (unless result ;; Might be set to nil from the timer in read-event?
          (if posn ;; -end or -update, and matching DOWN-POINT-ID
              (pcase (car-safe event)
                ('touchscreen-update
                 (push (cons (float-time) posn) pending-events)
                 (unless timer
                   (edraw-log "Track Drag: Schedule timer")
                   (setq timer
                         ;; (run-at-time (time-add nil (list 0 0 5000))
                         (run-with-idle-timer 0 nil on-timer))))
                ('touchscreen-end
                 (when timer
                   (cancel-timer timer)
                   (funcall on-timer))
                 (setq result (edraw-track-drag--on-up event on-up))))
            (setq result (edraw-track-drag--on-unknown-event event on-up))))))
    result))

(defun edraw-track-drag--on-move (move-event ;; mouse-movement event (not touch)
                                  on-move on-leave
                                  target
                                  allow-out-of-target-p
                                  down-posn)
  (edraw-log "Track Drag: On move event")
  (if (or allow-out-of-target-p
          (edraw-posn-same-object-p (cadr move-event) ;; posn of mouse-movement
                                    down-posn
                                    target))
      (progn
        (when on-move (funcall on-move move-event))
        nil)
    ;; out of target
    (when on-leave (funcall on-leave move-event))
    move-event))

(defun edraw-track-drag--on-up (up-event on-up)
  (edraw-log "Track Drag: On up event")
  (when on-up (funcall on-up up-event))
  up-event)

(defun edraw-track-drag--on-unknown-event (event on-up)
  (edraw-log "Track Drag: On unknown event %s" (car-safe event))
  (cond
   ;; Ignore some events
   ((memq
     (car-safe event)
     '(;; (For Ubuntu 22/Emacs 27.1, To allow dragging in child frames)
       switch-frame
       ;; Unrelated touch events
       touchscreen-begin touchscreen-update touchscreen-end))
    nil)
   ;; Ignore unrelated mouse events
   ((mouse-event-p event)
    nil)
   ;; Otherwise
   (t
    (when on-up (funcall on-up event))
    (push (cons t event) unread-command-events)
    event)))

;;;;; Event

(defun edraw-posn-same-object-p (pos1 pos2 &optional target)
  (and (eq (posn-window pos1)
           (posn-window pos2))
       (if (memq target '(point object nil))
           (equal (posn-point pos1)
                  (posn-point pos2))
         t)
       (if (memq target '(object nil))
           (eq (car (posn-object pos1)) ;;ex: 'image
               (car (posn-object pos2))) ;;ex: 'image
         t)))

(defun edraw-this-command-event ()
  "Return the first event with parameters that invoked the current command.

This function corresponds to (interactive \"e\"). However, if there is
no event with parameters, this function returns nil (while (interactive
\"e\") signals an error).

`last-command-event' and `last-input-event' return the last event that
occurred, so they cannot be used for the same purpose. For example, if
you right-click on text in `context-menu-mode', then select an item in
the menu and execute a command, `last-command-event' and
`last-input-event' will return the event for the menu item you
selected. Using this function, you can get the event at the time you
right-clicked.

For the implementation of (interactive \"e\"), see the
`call-interactively' function in callint.c."
  (seq-find #'consp (this-command-keys-vector)))


;;;;; Mouse Wheel Event

(defconst edraw-wheel-down-event
  (if (and (version< emacs-version "30")
           (boundp 'mouse-wheel-up-event))
      ;; Emacs 29:
      ;;  w32-win ns-win haiku-win pgtk-win => wheel-down
      ;;  other environments => mouse-5
      mouse-wheel-up-event ;; up!
    ;; Emacs 30:
    ;;  always => wheel-down
    'wheel-down))

(defconst edraw-wheel-up-event
  (if (and (version< emacs-version "30")
           (boundp 'mouse-wheel-down-event))
      ;; Emacs 29:
      ;;  w32-win ns-win haiku-win pgtk-win => wheel-up
      ;;  other environments => mouse-4
      mouse-wheel-down-event ;; down!
    ;; Emacs 30:
    ;;  always => wheel-up
    'wheel-up))

;;;;; Input Event Coordinates

(defun edraw-posn-x-y-on-frame (position &optional default-inside-window-p)
  "Convert POSITION to frame coordinates.

POSITION should be a list of the form returned by `event-start'
and `event-end'.

If area of POSITION is an unknown location and
DEFAULT-INSIDE-WINDOW-P is non-nil, the event is assumed to be
within window and the result is returned. If it is nil, it
returns nil."
  (let* ((window-or-frame (posn-window position))
         (window (and (windowp window-or-frame) window-or-frame))
         (area (posn-area position))
         (xy (posn-x-y position))
         (x (car xy))
         (y (cdr xy)))
    (cond
     (window
      (let ((edges (cond
                    ((null area)
                     (window-inside-pixel-edges window))
                    ((memq area '(horizontal-scroll-bar
                                  mode-line header-line tab-line))
                     (window-pixel-edges window))
                    ((memq area '(left-margin
                                  left-fringe
                                  ;; Right
                                  right-fringe
                                  right-margin
                                  vertical-line
                                  vertical-scroll-bar))
                     (let ((win (window-pixel-edges window))
                           (ins (window-inside-pixel-edges window)))
                       ;; Just to make sure, do the processing when
                       ;; x is not a number.
                       (unless (numberp x)
                         (setq x (if (memq area '(left-margin left-fringe))
                                     (nth 0 ins)
                                   (nth 2 ins))))
                       ;; x=win, y=ins
                       ;; NOTE: Elisp manual says "x does not have
                       ;; meaningful data" But at least in
                       ;; MS-Windows x means the coordinate from the
                       ;; left edge of the window.
                       (list (nth 0 win) (nth 1 ins)
                             (nth 2 win) (nth 3 ins))))
                    (default-inside-window-p
                     (window-inside-pixel-edges window)))))
        (when edges
          (cons (+ x (car edges))
                (+ y (cadr edges))))))
     ((framep window-or-frame)
      xy))))

(defun edraw-posn-delta-xy-frame-to-object (down-pos)
  "Calculate coordinate delta from frame to image."
  (let* ((down-xy-on-frame (edraw-posn-x-y-on-frame down-pos))
         (down-xy-on-object (posn-object-x-y down-pos)))
    (and down-xy-on-frame
         (cons (- (car down-xy-on-object) (car down-xy-on-frame))
               (- (cdr down-xy-on-object) (cdr down-xy-on-frame))))))

(defun edraw-posn-x-y-on-display (position &optional default-inside-window-p)
  (let* ((window-or-frame (posn-window position))
         (frame (cond
                 ((windowp window-or-frame) (window-frame window-or-frame))
                 ((framep window-or-frame) window-or-frame))))
    (when frame
      (let ((xy (edraw-posn-x-y-on-frame position default-inside-window-p))
            (frame-xy (frame-position frame)))
        (cons
         (+ (car frame-xy) (car xy))
         (+ (cdr frame-xy) (cdr xy)))))))


;;;;; Input Event Modifiers

(defun edraw-event-mouse-modifiers (event)
  (seq-remove (lambda (m) (not (memq m '(click double triple drag down))))
              (event-modifiers event)))

(defun edraw-event-key-modifiers (event)
  (seq-remove (lambda (m) (not (memq m '(meta control shift hyper super alt))))
              (event-modifiers event)))

(defun edraw-event-key-modifiers-equal (event modifiers)
  (seq-set-equal-p
   (edraw-event-key-modifiers event)
   modifiers))

(defun edraw-event-modifiers-symbol (event)
  (edraw-make-event-modifiers-symbol (event-modifiers event)))

(defun edraw-make-event-modifiers-symbol (modifiers)
  (let ((modifier-symbols '(alt control hyper meta shift super
                                double triple
                                drag down click))
        (modifier-strings '("A" "C" "H" "M" "S" "s"
                            "double" "triple" "drag" "down" "click"))
        (code 0))
    (cl-loop for m in modifiers
             for pos = (seq-position modifier-symbols m #'eq)
             when pos do (setq code (logior code (ash 1 pos))))
    (when (/= code 0)
      (intern
       (cl-loop with str = nil
                for s in modifier-strings
                for bit = 1 then (ash bit 1)
                when (/= (logand code bit) 0)
                if str do (setq str (concat str "-" s)) else do (setq str s)
                finally return str)))))


;;;; Frame

;;;;; Frame Dragging

(defconst edraw-frame-move-debounce-time 0.07
  "Debounce time (in seconds) after moving the frame.
Touch event coordinates immediately after moving a frame are unreliable,
so events are ignored until this many seconds have elapsed since the
last move.")

(defun edraw-move-frame-on-mouse-down (down-event)
  (interactive "e")
  (let ((down-frame (window-frame (posn-window (event-start down-event))))
        (buffer (window-buffer (posn-window (event-start down-event)))))
    (when (and (frame-live-p down-frame)
               ;; If decorated, the use native title bar should be used.
               (frame-parameter down-frame 'undecorated)
               (buffer-live-p buffer))

      (let* ((down-xy (edraw-posn-x-y-on-display (event-start down-event)))
             (down-frame-xy (frame-position down-frame))
             (moved nil)
             (last-frame-moved-time 0.0)
             (touch (eq (car-safe down-event) 'touchscreen-begin))
             (on-move
              (lambda (move-event)
                (when (or (not touch)
                          (>= (- (float-time) last-frame-moved-time)
                              edraw-frame-move-debounce-time))
                  (let* ((curr-xy (edraw-posn-x-y-on-display (event-start move-event)))
                         (dx (- (car curr-xy) (car down-xy)))
                         (dy (- (cdr curr-xy) (cdr down-xy))))
                    (when (and (not moved)
                               (> (+ (* dx dx) (* dy dy))
                                  (* double-click-fuzz double-click-fuzz)))
                      (setq moved t))
                    (when moved
                      (edraw-set-frame-position
                       down-frame
                       (cons (+ (car down-frame-xy) dx)
                             (+ (cdr down-frame-xy) dy)))
                      (setq last-frame-moved-time (float-time))))))))
        (edraw-track-drag down-event on-move :on-up on-move
                          :allow-out-of-target-p t)

        moved))))

(defun edraw-set-frame-position (frame xy)
  (setq xy (edraw-limit-frame-position frame xy))
  (modify-frame-parameters frame `((left . (+ ,(car xy)))
                                   (top . (+ ,(cdr xy))))))

(defcustom edraw-frame-keep-visible-margins
  '(80 80 80 80)
  "Margins to keep visible when dragging frames.

These values define the minimum amount (in pixels) of the frame that
must remain visible within the parent display area during dragging
operations.  This prevents the frame from being moved completely
off-screen while still allowing partial positioning outside the visible
area.

The list contains four integers: (LEFT TOP RIGHT BOTTOM).

Each margin value is capped by the corresponding frame dimension.
For example, if BOTTOM is set to a value larger than the frame height,
it effectively equals the frame height, preventing the frame from
extending above the parent display area.  This ensures the title bar
remains accessible."
  :type '(list :tag "Margins (In pixels)"
               (integer :tag "Left")
               (integer :tag "Top")
               (integer :tag "Right")
               (integer :tag "Bottom"))
  :group 'edraw)

(defun edraw-limit-frame-position (frame xy)
  "Limit position XY to keep FRAME partially visible within the parent area.

XY is a cons cell (X . Y) representing the desired frame position.
Returns a cons cell with the position adjusted to satisfy the
visibility constraints defined by `edraw-frame-keep-visible-margins'."
  (if-let* ((parent-frame (frame-parent frame)))
      (let* ((frame-w (frame-outer-width frame))
             (frame-h (frame-outer-height frame))
             (parent-inner-edges (frame-edges parent-frame 'inner))
             (parent-origin-x (nth 0 parent-inner-edges))
             (parent-origin-y (nth 1 parent-inner-edges))
             (parent-limit-edges (frame-edges parent-frame 'native))
             (parent-l (- (nth 0 parent-limit-edges) parent-origin-x))
             (parent-t (- (nth 1 parent-limit-edges) parent-origin-y))
             (parent-r (- (nth 2 parent-limit-edges) parent-origin-x))
             (parent-b (- (nth 3 parent-limit-edges) parent-origin-y))
             (margin-l (min (nth 0 edraw-frame-keep-visible-margins)
                            frame-w))
             (margin-t (min (nth 1 edraw-frame-keep-visible-margins)
                            frame-h))
             (margin-r (min (nth 2 edraw-frame-keep-visible-margins)
                            frame-w))
             (margin-b (min (nth 3 edraw-frame-keep-visible-margins)
                            frame-h))
             (limit-l (+ (- parent-l frame-w) margin-r))
             (limit-r (- parent-r margin-l))
             (limit-t (+ (- parent-t frame-h) margin-b))
             (limit-b (- parent-b margin-t)))
        (cons (max limit-l (min (car xy) limit-r))
              (max limit-t (min (cdr xy) limit-b))))
    ;; Not a child frame
    ;; TODO: Limit coordinates to within the screen area
    xy))



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

(defun edraw-hook-length (hook)
  (length (cdr hook)))

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



;;;; List Structure

(cl-defstruct (edraw-list
               (:constructor edraw-list-construct))
  (data nil))

(defun edraw-list (&optional list)
  (edraw-list-construct :data (mapcar #'identity list)))

(cl-defmethod edraw-as-list ((list edraw-list))
  (edraw-list-data list))

(cl-defmethod edraw-to-new-list ((list edraw-list))
  (copy-sequence (edraw-list-data list)))

(cl-defmethod edraw-assign ((list edraw-list) sequence)
  (setf (edraw-list-data list) (mapcar #'identity sequence))
  list)

(cl-defmethod edraw-clear ((list edraw-list))
  (setf (edraw-list-data list) nil)
  list)

(cl-defmethod edraw-length ((list edraw-list))
  (length (edraw-list-data list)))

(cl-defmethod edraw-empty-p ((list edraw-list))
  (null (edraw-list-data list)))

(cl-defmethod edraw-push-front ((list edraw-list) element)
  (push element (edraw-list-data list))
  list)

(cl-defmethod edraw-pop-front ((list edraw-list))
  (pop (edraw-list-data list))
  list)

(cl-defmethod edraw-front ((list edraw-list))
  (car (edraw-list-data list)))

(cl-defmethod edraw-shrink ((list edraw-list) n)
  (if (< n 1)
      (edraw-clear list)
    (when-let* ((cell (nthcdr (1- n) (edraw-list-data list))))
      (setcdr cell nil)))
  list)

(cl-defmethod edraw-resize ((list edraw-list) n)
  (if (< n 1)
      (edraw-clear list)
    (let ((size (edraw-length list)))
      (cond
       ((< n size)
        (setcdr (nthcdr (1- n) (edraw-list-data list)) nil))
       ((> n size)
        (nconc (edraw-list-data list) (make-list (- n size) nil))))))
  list)

(cl-defmethod edraw-remove-if ((list edraw-list) predicate)
  (setf (edraw-list-data list)
        (cl-remove-if predicate (edraw-list-data list)))
  list)

(cl-defmethod edraw-nth ((list edraw-list) n)
  (nth n (edraw-list-data list)))

(cl-defmethod edraw-set-nth ((list edraw-list) n value)
  (setf (nth n (edraw-list-data list)) value)
  list)


;;;; List Wrapper

(cl-defmethod edraw-assign ((_list list) sequence)
  (mapcar #'identity sequence))

(cl-defmethod edraw-as-list ((list list))
  list)

(cl-defmethod edraw-to-new-list ((list list))
  (copy-sequence list))

(cl-defmethod edraw-empty-p ((list list))
  (null list))

(cl-defmethod edraw-length ((list list))
  (length list))

(cl-defmethod edraw-nth ((list list) n)
  (nth n list))

(cl-defmethod edraw-front ((list list))
  (car list))

(cl-defmethod edraw-push-front ((list list) element)
  (cons element list))

(cl-defmethod edraw-set-nth ((list list) n value)
  (setf (nth n list) value)
  list)


;;;; Property List

(defun edraw-plist-member (plist key &optional testfn)
  "Scan every second element of PLIST and return the list after the element
that matches KEY.
Use TESTFN to match KEY and elements. If TESTFN is nil, use `eq'.

This is an alternative to `plist-member' that accepts matching functions
in Emacs27.
Note: The PREDICATE argument of `plist-member' requires Emacs 29 or later."
  (if (or (null testfn) (eq testfn #'eq))
      (plist-member plist key)
    (while (and plist
                (not (funcall testfn (car plist) key)))
      (setq plist (cddr plist)))
    plist))

(defun edraw-plist-get (plist key &optional testfn)
  "Scan every second element of PLIST and return the element following the
one that matches KEY.
Use TESTFN to match KEY and elements. If TESTFN is nil, use `eq'.

This is an alternative to `plist-get' that accepts matching functions in
Emacs27.
Note: The PREDICATE argument of `plist-get' requires Emacs 29 or later.

`edraw-plist-get' is also a generalized variable."
  (if (or (null testfn) (eq testfn #'eq))
      (plist-get plist key)
    (while (and plist
                (not (funcall testfn (car plist) key)))
      (setq plist (cddr plist)))
    (cadr plist)))

(gv-define-expander edraw-plist-get
  (lambda (do plist prop &optional predicate)
    (macroexp-let2 macroexp-copyable-p key prop
      (gv-letplace (getter setter) plist
        (macroexp-let2 nil p `(cdr (edraw-plist-member ,getter ,key ,predicate))
          (funcall do
                   `(car ,p)
                   (lambda (val)
                     `(if ,p
                          (setcar ,p ,val)
                        ,(funcall setter
                                  `(cons ,key (cons ,val ,getter)))))))))))

(defun edraw-plist-set! (plist key value &optional testfn)
  "Set a property with KEY and VALUE in non-nil PLIST and return VALUE.
If KEY already exists in PLIST, rewrite its value. If not, add a new
property to the end of the PLIST.
If PLIST is nil, signal an error.

Use TESTFN to compare KEY. If TESTFN is nil, use `eq'.

This function has the same effect as `plist-put' for non-nil plists in
Emacs 29 or later, or `map-put!'.

This function is used to indicate the intention to modify an
existing (non-nil) plist while preserving the head of the plist."
  (unless plist
    (error "Cannot append property to nil"))
  (if (or (null testfn) (eq testfn #'eq))
      (plist-put plist key value)
    (let ((p plist))
      (while (cond
              ((funcall testfn (car p) key)
               (setcar (cdr p) value)
               nil)
              ((null (cddr p))
               (setcdr (cdr p) (list key value))
               nil)
              (t
               (setq p (cddr p)))))))
  value)

(defmacro edraw-plist-set (plist key value &optional testfn)
  "Set a property with KEY and VALUE in PLIST and return VALUE.
If KEY already exists in PLIST, rewrite its value. If not, add a new
property to the beginning of the PLIST.

Use TESTFN to compare KEY. If TESTFN is nil, use `eq'.

PLIST is a generalized variable.
This macro is equivalent to the following.
  (setf (edraw-plist-get PLIST KEY TESTFN) VALUE)"
  `(setf (edraw-plist-get ,plist ,key ,testfn) ,value))

(defun edraw-plist--delete (plist key testfn)
  "Used from `edraw-plist-delete'."
  (if (or (null testfn) (eq testfn #'eq))
      (progn
        (while (and plist (eq (car plist) key))
          (setq plist (cddr plist)))
        (when plist
          (let ((p (cdr plist)))
            (while (cdr p)
              (if (eq (cadr p) key)
                  (setcdr p (cdddr p))
                (setq p (cddr p)))))))
    (progn
      (while (and plist (funcall testfn (car plist) key))
        (setq plist (cddr plist)))
      (when plist
        (let ((p (cdr plist)))
          (while (cdr p)
            (if (funcall testfn (cadr p) key)
                (setcdr p (cdddr p))
              (setq p (cddr p))))))))
  plist)

(defmacro edraw-plist-delete (plist key &optional testfn)
  "Delete all properties named KEY from PLIST.
Use TESTFN to compare KEY. If TESTFN is nil, use `eq'.
PLIST is a generalized variable.

The differences from `cl-remf' are:
- Delete all matching properties.
- TESTFN can be specified."
  (gv-letplace (getter setter) plist
    (funcall setter `(edraw-plist--delete ,getter ,key ,testfn))))

(defun edraw-plist-take-while (plist pred)
  (cl-loop for (k v) on plist by #'cddr
           while (funcall pred k v)
           collect k collect v))

(defun edraw-plist-remove-first-key (plist key
                                           ;; &optional predicate
                                           )
  "Return a property list with the property KEY removed from PLIST.

The original PLIST will not be modified.

The rest of the PLIST after the modified part is shared with the
returned list.

If there are multiple KEYs, the first one is removed."
  ;; @todo Using plist-get as GV doesn't work on Emacs 27! Create edraw-plist-delete-first-key
  ;; +If you want to make destructive changes to the list, consider+
  ;; +using `cl-remf'.+
  ;; (unless predicate (setq predicate #'eq))
  (if-let* ((head (plist-member
                   plist key
                   ;; @todo The third argument of plist-member can be used with Emacs 29 or later.
                   ;; predicate
                   )))
      (nconc
       (edraw-plist-take-while plist (lambda (k _)
                                       (not
                                        ;; (funcall predicate k key)
                                        (eq k key)
                                        )))
       (cddr head))
    plist))
;; TEST: (edraw-plist-remove-first-key '(a 1 b 2 c 3 d 4 a 10 b 20) 'a) => (b 2 c 3 d 4 a 10 b 20)

(defun edraw-plist-remove-key (plist key)
  "Return a new plist excluding all properties with KEY from PLIST."
  (cl-loop for (k v) on plist by #'cddr
           unless (eq k key)
           collect k and collect v))
;; TEST: (edraw-plist-remove-key '(a 1 b 2 c 3 d 4 a 10 b 20) 'a) => (b 2 c 3 d 4 b 20)
;; TEST: (edraw-plist-remove-key '(a 1 b 2 c 3 d 4 nil nil) nil) => (a 1 b 2 c 3 d 4)

(defun edraw-plist-remove-keys (plist keys)
  "Return a new plist excluding all properties with KEYS from PLIST."
  (cl-loop for (k v) on plist by #'cddr
           unless (memq k keys)
           collect k and collect v))
;; TEST: (edraw-plist-remove-keys '(a 1 b 2 c 3 d 4 a 10 b 20) '(a c)) => (b 2 d 4 b 20)
;; TEST: (edraw-plist-remove-keys '(a 1 b 2 c 3 d 4 nil nil) nil) => (a 1 b 2 c 3 d 4 nil nil)

(defun edraw-plist-remove-nil (plist)
  "Return a new property list from PLIST where all properties whose keys or
values are nil have been removed."
  (cl-loop for (k v) on plist by #'cddr
           when (and k v)
           collect k and collect v))
;; TEST: (edraw-plist-remove-nil nil) => nil
;; TEST: (edraw-plist-remove-nil '(a 1 b nil c 3 nil 4 e 5 nil nil g 7)) => (a 1 c 3 e 5 g 7)

(defun edraw-plist-put (plist prop value
                              ;; &optional predicate
                              )
  "Return a plist with the PROP of PLIST changed to VALUE.
This is a non-destructive version of `plist-put'."
  (cons
   prop
   (cons
    value
    (edraw-plist-remove-first-key plist prop
                                  ;; predicate
                                  ))))

(defun edraw-plist-put-default (plist prop value)
  "Add the property PROP VALUE to PLIST if PROP is not in PLIST.

Return a list with the property prepended to the PLIST or the PLIST
itself."
  (if (plist-member plist prop)
      plist
    (cons prop
          (cons value
                plist))))

(defun edraw-plist-append (&rest plists)
  "Return a new plist by concatenating PLISTS and removing duplicates.

For duplicate properties, the first occurrence takes precedence.
Therefore, the following evaluation results will be the same:

- (plist-get (edraw-plist-append PLIST1 PLIST2) PROP)
- (plist-get (append PLIST1 PLIST2) PROP)"
  (let* ((head (cons nil nil))
         (last head))
    (cl-loop for plist in plists
             do (cl-loop for (key value) on plist by #'cddr
                         unless (plist-member (cdr head) key)
                         do (progn
                              (setcdr last (cons key (cons value nil)))
                              (setq last (cddr last)))))
    (cdr head)))
;; TEST: (edraw-plist-append) => nil
;; TEST: (edraw-plist-append  '(:a 1 :b 2 :a 3)) => (:a 1 :b 2)
;; TEST: (edraw-plist-append '(:a 1 :b 2) '(:c 3 :d 4)) => (:a 1 :b 2 :c 3 :d 4)
;; TEST: (edraw-plist-append '(:a 1 :b 2) '(:a 3 :d 4)) => (:a 1 :b 2 :d 4)
;; TEST: (edraw-plist-append nil '(:a 11 :b 22 :c 33 :d 44)) => (:a 11 :b 22 :c 33 :d 44)

(defun edraw-plist-to-alist (plist)
  (cl-loop for (prop value) on plist by #'cddr
           collect (cons prop value)))
;; TEST: (edraw-plist-to-alist nil) => nil
;; TEST: (edraw-plist-to-alist '(a 1 b 2)) => ((a . 1) (b . 2))

(defun edraw-plistp (object)
  ;; Emacs 28 does not have `plistp'
  ;; Emacs 29 have `plistp'
  ;; Note: proper-list-p requires Emacs 27
  (when-let* ((len (proper-list-p object)))
    (= (% len 2) 0)))

(defun edraw-plist-mapconcat (function plist &optional separator remove-nil)
  "Apply FUNCTION to each property of PLIST, and concat the results as strings.
In between each pair of results, stick in SEPARATOR.  Thus, \" \" as
SEPARATOR results in spaces between the values returned by FUNCTION.

FUNCTION must be a function of two arguments (a key and a value), and
must return a value that is a string.

A non-nil REMOVE-NIL means to remove properties whose keys or values are nil."
  (let ((result "")
        (p plist))
    (while (and (consp p) (consp (cdr p)))
      (let ((key (car p)) (value (cadr p)))
        (when (or (not remove-nil) (and key value))
          (let ((str (funcall function key value)))
            (setq result (if (eq p plist) str (concat result separator str)))))
        (setq p (cddr p))))
    result))
;; TEST: (edraw-plist-mapconcat (lambda (k v) (format "%s=%s" k v)) '(a 1) ", ") => "a=1"
;; TEST: (edraw-plist-mapconcat (lambda (k v) (format "%s=%s" k v)) '(a 1 b 2 c 3) ", ") => "a=1, b=2, c=3"
;; TEST: (edraw-plist-mapconcat (lambda (k v) (format "%s=%s" k v)) '(a 1 b nil c 3 nil 4 e 5 nil nil g 7) ", " t) => "a=1, c=3, e=5, g=7"

;;;; Association List

(defmacro edraw-alist-set (alist key value &optional testfn)
  "Set VALUE for KEY in ALIST destructively.

ALIST should be a generalized variable (place). This macro modifies ALIST
in place using `setf'.

TESTFN is the equality predicate used to compare keys (default is `eq').

If an entry for KEY already exists, its cdr is replaced with VALUE.
If KEY is not found, a new entry is added to ALIST."
  `(setf (alist-get ,key ,alist nil nil ,testfn) ,value))

(defmacro edraw-alist-delete (alist key &optional testfn)
  "Delete the entry for KEY from ALIST destructively.

ALIST should be a generalized variable (place). This macro modifies ALIST
in place using `setf'.

TESTFN is the equality predicate used to compare keys (default is `eq').

This is a wrapper around `alist-get' with `setf' that removes the entry
for KEY if it exists."
  `(setf (alist-get ,key ,alist nil t ,testfn) nil))

(defun edraw-alist-append (&rest alists)
  "Return a new alist by concatenating ALISTS and removing duplicates.

For duplicate keys, the first occurrence takes precedence.
Therefore, the following evaluation results will be the same:

- (alist-get (edraw-alist-append ALIST1 ALIST2) KEY)
- (alist-get (append ALIST1 ALIST2) PROP)"
  (let (result)
    (cl-loop for alist in alists
             do (cl-loop for cell in alist
                         unless (assq (car cell) result)
                         do (push cell result)))
    (nreverse result)))
;; TEST: (edraw-alist-append) => nil
;; TEST: (edraw-alist-append '((a . 1) (b . 2))) => ((a . 1) (b . 2))
;; TEST: (edraw-alist-append '((a . 1) (b . 2) (c . 3)) '((a . 11) (b . 22) (d . 44))) => ((a . 1) (b . 2) (c . 3) (d . 44))

(defun edraw-alist-to-plist (alist)
  "Non-destructively convert ALIST to a plist."
  (cl-loop for (key . value) in alist
           collect key collect value))
;; TEST: (edraw-alist-to-plist nil) => nil
;; TEST: (edraw-alist-to-plist '((a . 1) (b . 2))) => (a 1 b 2)

(defun edraw-n-alist-to-plist (alist)
  "Destructively convert ALIST to a plist.

Cons cells contained in the ALIST are reused in the returned plist."
  (let ((pos alist))
    (while pos
      (let ((pair (car pos))
            (next (cdr pos)))
        (setcar pos (car pair))
        (setcdr pos pair)
        (setcar pair (cdr pair))
        (setcdr pair next)
        (setq pos next))))
  alist)
;; TEST: (edraw-n-alist-to-plist nil) => nil
;; TEST: (edraw-n-alist-to-plist (list (cons 'a 1) (cons 'b 2))) => (a 1 b 2)

(defun edraw-alist-remove-first-key (alist key)
  "Return a list with the first entry for KEY removed from ALIST.

KEY is compared using `eq'.

If KEY is not found, return ALIST unchanged (the same object).
If KEY is found, return a list without that entry."
  (let ((p alist))
    (while (and p (not (eq (caar p) key)))
      (setq p (cdr p)))
    (if p
        (nconc
         (cl-loop for x on alist
                  until (eq x p)
                  collect (car x))
         (cdr p))
      alist)))
;; TEST: (edraw-alist-remove-first-key nil 'a) => nil
;; TEST: (edraw-alist-remove-first-key '((a . 1)) 'a) => nil
;; TEST: (edraw-alist-remove-first-key '((b . 1) (a . 1)) 'a) => ((b . 1))
;; TEST: (edraw-alist-remove-first-key '((a . 1) (b . 1)) 'a) => ((b . 1))
;; TEST: (edraw-alist-remove-first-key '((c . 1) (a . 2) (b . 3)) 'a) => ((c . 1) (b . 3))
;; TEST: (edraw-alist-remove-first-key '((c . 1) (a . 2) (b . 3) (a . 4)) 'a) => ((c . 1) (b . 3) (a . 4))

(defun edraw-alist-put (alist key value)
  "Return an alist with (KEY . VALUE) added and the first old KEY entry
removed.

If KEY already exists in ALIST, only the first occurrence is removed before
adding the new entry. If ALIST contains duplicate entries for KEY, the
duplicates after the first one will remain.

This function does not modify the original ALIST."
  (cons
   (cons key value)
   (edraw-alist-remove-first-key alist key)))

(defmacro edraw-alist-set-nd (alist key value)
  "Set VALUE for KEY in ALIST non-destructively.

ALIST should be a generalized variable (place). This macro replaces ALIST
with an alist containing the updated entry.

This is the non-destructive version of `edraw-alist-set'. The original
alist structure is not modified."
  `(cl-callf edraw-alist-put ,alist ,key ,value))

(defmacro edraw-alist-delete-nd (alist key)
  "Delete the first entry for KEY from ALIST non-destructively.

ALIST should be a generalized variable (place). This macro replaces ALIST
with an alist that has the first entry for KEY removed.

KEY is compared using `eq'.

If KEY is not found, ALIST remains unchanged (the same object).
If KEY is found, ALIST is replaced with a list without that entry.

This is the non-destructive version of `edraw-alist-delete'."
  `(cl-callf edraw-alist-remove-first-key ,alist ,key))


;;;; Max Image Size

(defun edraw-max-image-width ()
  "Return max image width in pixels."
  (defvar max-image-size)
  (let ((size max-image-size))
    (cond
     ((integerp size) size)
     ((floatp size) (round (* size (frame-pixel-width))))
     (t 1024))))

(defun edraw-max-image-height ()
  "Return max image height in pixels."
  (defvar max-image-size)
  (let ((size max-image-size))
    (cond
     ((integerp size) size)
     ((floatp size) (round (* size (frame-pixel-height))))
     (t 1024))))

;;;; Transient Map

;; Example:
;; (edraw-transient-map
;;  (let ((km (make-sparse-keymap)))
;;    (define-key km "a" (lambda () (interactive) (message "AAA")))
;;    (define-key km "b" (lambda () (interactive) (message "BBB")))
;;    (define-key km "c" (lambda ()
;;                         (interactive)
;;                         (message "%s" (read-string "Your Message: "))))
;;    km)
;;  t
;;  (lambda () (message "Exit")))

(defun edraw-transient-map (map &optional keep-pred on-exit message)
  "This is an alternative to `set-transient-map'.

Compared to set-transient-map, it has the following differences:

- Buffer local
- Minibuffer support (read-string, etc. can be used)
- Provide `edraw-transient-map--this-command-in-map-p' for help
  implement KEEP-PRED
- Restrictions
  - TIMEOUT argument not supported
  - MESSAGE argument formatting method simplified"
  ;; Derived from `set-transient-map' (Emacs 30.0.93)
  (let* ((tr-map (let ((km (make-sparse-keymap)))
                   (set-keymap-parent km map)
                   km))
         (target-buffer (current-buffer))
         (target-mb-depth (minibuffer-depth))
         (suspended nil)
         (exited nil)
         exitfun
         (suspend-override
          (lambda ()
            (unless suspended
              (set-keymap-parent tr-map (make-sparse-keymap))
              (setq suspended t))))
         (resume-override
          (lambda ()
            (when suspended
              (set-keymap-parent tr-map map)
              (setq suspended nil))))
         (on-pre-command
          (lambda ()
            (with-demoted-errors "edraw-transient-map pre-command: %S"
              (unless suspended
                (if (edraw-transient-map--keep-p keep-pred map)
                    (progn
                      (funcall suspend-override)
                      ;; Repeat the message for the next command.
                      (when message (message "%s" message)))
                  (funcall exitfun))))))
         (on-post-command
          (lambda ()
            (with-demoted-errors "edraw-transient-map post-command: %S"
              (when (and suspended
                         ;; Same buffer
                         (eq (current-buffer) target-buffer)
                         ;; Not in minibuffer
                         (<= (minibuffer-depth) target-mb-depth))
                (funcall resume-override)))))
         (setup
          (lambda ()
            (add-hook 'pre-command-hook on-pre-command)
            (add-hook 'post-command-hook on-post-command)
            (internal-push-keymap tr-map 'overriding-terminal-local-map)
            (when message (message "%s" message))))
         (teardown
          (lambda ()
            (unless exited
              (internal-pop-keymap tr-map 'overriding-terminal-local-map)
              (remove-hook 'post-command-hook on-post-command)
              (remove-hook 'pre-command-hook on-pre-command)
              (when message (message ""))
              (setq exited t)
              (when on-exit
                (funcall on-exit))))))
    (setq exitfun teardown)
    (funcall setup)
    ;; Return a function to exit the transient map
    exitfun))

(defun edraw-transient-map--keep-p (keep-pred transient-map)
  (cond
   ((null keep-pred) nil)
   ((and (not (eq transient-map (cadr overriding-terminal-local-map)))
         (memq transient-map (cddr overriding-terminal-local-map)))
    t)
   ((eq t keep-pred)
    (edraw-transient-map--this-command-in-map-p transient-map))
   (t (funcall keep-pred))))

(defun edraw-transient-map--this-command-in-map-p (map)
  ;; Derived from `set-transient-map' (Emacs 30.0.93)
  (let ((mc (lookup-key map (this-command-keys-vector))))
    ;; We may have a remapped command, so chase
    ;; down that.
    (when (and mc (symbolp mc))
      (setq mc (or (command-remapping mc) mc)))
    ;; If the key is unbound `this-command` is
    ;; nil and so is `mc`.
    (and mc (eq this-command mc))))


;;;; Lazy Image Updator

(defconst edraw-image-update-delay-min 0.0001)

(defcustom edraw-image-update-interval (/ 1.0 60.0)
  "Image update interval time (in seconds).

The larger the value, the lower the load, but the display will be rougher."
  :group 'edraw
  :type 'float)

(defcustom edraw-image-update-fixed-delay-p nil
  "Non-nil means that `edraw-image-update-interval' is a fixed delay after
`edraw-invalidate-image'.

If nil, the delay time for consecutive updates is calculated so that the
update interval is as much as `edraw-image-update-interval'."
  :group 'edraw
  :type 'boolean)

(defclass edraw-lazy-image-updator ()
  ((image-update-timer :initform nil)
   (image-update-last-scheduled-time :initform nil))
  "A class for updating images asynchronously.

Base class for classes that display information through a image.

Updating the image every time the information is updated is inefficient,
so a timer is used to update image all at once.

When it is necessary to update the image, call the
`edraw-invalidate-image' method.

When it is time to update the image, the `edraw-update-image' method will
be called.

Derived classes must define the edraw-update-image method.")

(cl-defmethod edraw-invalidate-image ((updator edraw-lazy-image-updator))
  "Mark the image held by UPDATOR as invalid.

If the image update has not been scheduled yet, schedule it.

After the image update is scheduled, the `edraw-update-image' method
will be called after a short delay.

UPDATOR must implement the `edraw-update-image' method. When
implementing `edraw-update-image', it is recommended to call the
`edraw-update-image-timer-cancel' method to avoid unnecessary updates
when `edraw-update-image' is called directly."
  (edraw-log "Display: Invalidate image")
  (with-slots (image-update-timer) updator
    (unless image-update-timer
      (edraw-log "Display: Schedule update image")
      ;; Post update command
      (setq image-update-timer
            (run-at-time (edraw-update-image-delay updator) nil
                         'edraw-update-image-on-timer updator)))))

(cl-defmethod edraw-update-image-delay ((updator edraw-lazy-image-updator))
  "Return the delay time of the image update timer."
  (if edraw-image-update-fixed-delay-p
      ;; Use `edraw-image-update-interval' as is
      edraw-image-update-interval
    ;; Make the average update interval closer to `edraw-image-update-interval'.
    (let* ((curr (float-time))
           ;; Note: If the timer is cancelled, it may not be the last
           ;; time the image was updated.
           (last (or (oref updator image-update-last-scheduled-time) 0.0))
           (next (+ last edraw-image-update-interval))
           (delay (- next curr)))
      (cond
       ((< delay edraw-image-update-delay-min)
        (edraw-log "Display: Delayed %.3f[ms]" (* delay 1000))
        (setq delay (* edraw-image-update-interval 0.5)))
       ((> delay edraw-image-update-interval)
        (setq delay edraw-image-update-interval))
       (t
        (edraw-log "Display: Not Delayed %.3f[ms]" (* delay 1000))
        ))

      (oset updator image-update-last-scheduled-time (+ curr delay))

      delay)))

(cl-defmethod edraw-update-image-timer-cancel ((updator
                                                edraw-lazy-image-updator))
  "Cancel the effect of `edraw-invalidate-image'."
  (with-slots (image-update-timer) updator
    (when image-update-timer
      (cancel-timer image-update-timer)
      ;; (oset updator image-update-last-scheduled-time ??)
      (setq image-update-timer nil))))

(cl-defmethod edraw-update-image-on-timer ((updator edraw-lazy-image-updator))
  (oset updator image-update-timer nil)
  (edraw-update-image updator))

(cl-defgeneric edraw-update-image (object)
  "Update the image held by OBJECT.")

(cl-defmethod edraw-update-image ((updator edraw-lazy-image-updator))
  (edraw-log "Display: Update image %.3f[ms]" (* (float-time) 1000))
  (edraw-update-image-timer-cancel updator))


;;;; Misc

(defun edraw-alist-get-as-number (key alist default)
  (let ((value (alist-get key alist default nil (if (stringp key) #'equal))))
    (if (stringp value)
        (string-to-number value)
      value)))

(cl-defmethod edraw-to-string ((str string))
  str)

(cl-defmethod edraw-to-string ((sym symbol))
  (symbol-name sym))

(cl-defmethod edraw-cast (obj type)
  (when (cl-typep obj type)
    obj))

(defun edraw-read-string-or-nil (prompt &optional initial-input)
  (let ((input (read-string prompt (or initial-input ""))))
    (if (string-empty-p input) nil input)))

(defun edraw-read-integer-or-nil (prompt &optional initial-input)
  (edraw-read-number prompt initial-input t t))

(defun edraw-read-number-or-nil (prompt &optional initial-input)
  (edraw-read-number prompt initial-input nil t))

(defun edraw-read-integer (prompt &optional initial-input)
  (edraw-read-number prompt initial-input t nil))

(defun edraw-read-number (prompt &optional initial-input integer-p nullable-p)
  (let ((pred (if integer-p #'integerp #'numberp))
        result)
    (while (let ((input (read-string
                         prompt
                         (format "%s" (or initial-input "")))))
             (not
              (if (string-empty-p input)
                  nullable-p
                (let ((value (ignore-errors (read input))))
                  (when (funcall pred value)
                    (setq result value)
                    t)))))
      (message
       (if nullable-p
           (if integer-p
               (edraw-msg "Please enter a integer or empty.")
             (edraw-msg "Please enter a number or empty."))
         (if integer-p
             (edraw-msg "Please enter a integer.")
           (edraw-msg "Please enter a number."))))
      (sit-for 1))
    result))

(defvar edraw-minibuffer-default-prompt-format
  (if (boundp 'minibuffer-default-prompt-format)
      minibuffer-default-prompt-format ;; Emacs 28.1~
    " (default %s)"))

(defconst edraw-minibuffer-number-regexp
  (concat "\\(?:"
          "[-+]?"
          ;; Valid: 12  12.34  .34  12.
          "\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)"
          "\\(?:[eE][-+]?[0-9]+\\)?"
          "\\)"))

(defun edraw-read-number-with-unit--convert (input-string default units pred
                                                          regexp)
  (cond
   ((and default (string-empty-p input-string))
    default)
   ((string-match regexp input-string)
    (let ((num (string-to-number (match-string 1 input-string)))
          (unit (match-string 2 input-string)))
      ;; Unit conversion
      (when unit
        (let ((conv (alist-get unit units nil nil #'equal)))
          (cond
           ((numberp conv)
            (setq num (* conv num)))
           ((functionp conv)
            (setq num (funcall conv num default)))
           (t
            (error (edraw-msg "Unsupported unit"))))))
      ;; Validation
      (when (and num pred (not (funcall pred num)))
        (error (edraw-msg "Invalid number")))
      num))
   (t
    (error (edraw-msg "Not a number")))))

(defun edraw-read-number-with-unit--read (prompt default units pred regexp)
  "Almost the same as read-string, but with a preview of the result."
  (let* (overlay
         (on-post-command
          (lambda ()
            (let* ((curr-input
                    (minibuffer-contents-no-properties))
                   (curr-result
                    (format
                     "%s"
                     (condition-case err
                         (edraw-read-number-with-unit--convert
                          curr-input default units pred regexp)
                       (error (error-message-string err))))))
              (overlay-put
               overlay 'before-string
               (if (or (equal curr-input curr-result)
                       (string-empty-p curr-input))
                   nil
                 (propertize
                  (concat curr-result " <= ")
                  'face 'font-lock-comment-face))))))
         (on-minibuffer-setup
          (lambda ()
            (setq overlay (make-overlay (minibuffer-prompt-end)
                                        (point-max)
                                        nil nil t))
            (add-hook 'post-command-hook on-post-command nil t)))
         (minibuffer-setup-hook (cons on-minibuffer-setup
                                      minibuffer-setup-hook)))
    (unwind-protect
        (read-from-minibuffer prompt nil nil nil nil
                              (when default (number-to-string default)))
      (when overlay
        (delete-overlay overlay)))))

(defun edraw-read-number-with-unit (prompt &optional default units pred)
  (let ((result nil)
        (regexp (concat "\\`\\(" edraw-minibuffer-number-regexp "\\)"
                        (when units
                          (concat
                           "\\(" (mapconcat #'car units "\\|") "\\)?\\'")))))

    (when default
      (string-match "\\(?:\\(\\):\\)?\\([ \t]*\\)\\'" prompt)
      (setq prompt
            (replace-match
             (format edraw-minibuffer-default-prompt-format default) t t prompt
             (if (match-beginning 1) 1 0))))

    (while
        (null
         (setq result
               (let ((str (edraw-read-number-with-unit--read
                           prompt default units pred regexp)))
                 (condition-case err
                     (edraw-read-number-with-unit--convert str default units
                                                           pred regexp)
                   (error
                    (message "%s" (error-message-string err))
                    (sit-for 1)
                    nil))))))
    result))
;; Example: (edraw-read-number-with-unit "Length (px, %): " 640 '(("%" . (lambda (n def) (round (/ (* n def) 100.0)))) ("px" . (lambda (n _) n))) (lambda (n) (or (integerp n) (error "Not a integer"))))


(defun edraw-where-is-string (definition &optional keymap firstonly)
  (when-let* ((keys (where-is-internal definition keymap firstonly)))
    (if (listp keys)
        (mapcar #'key-description keys)
      (key-description keys))))

(defun edraw-remove-text-properties-except (string prop-names)
  "Remove all text properties from STRING except PROP-NAMES.

STRING is the text from which to modify properties.
PROP-NAMES is a list of properties to retain in the string."
  (let ((len (length string))
        (pos 0))
    (while (< pos len)
      (let* ((old-props (text-properties-at pos string))
             (next (or (next-property-change pos string) len))
             (new-props (cl-loop for pname in prop-names
                                 for pvalue = (plist-get old-props pname)
                                 when pvalue
                                 collect pname and collect pvalue)))
        (set-text-properties pos next new-props string)
        (setq pos next)))
    string))

(defun edraw-echo (string)
  (let ((message-log-max nil))
    (if (and (stringp string) (not (string= string "")))
        (message "%s" string)
      (message nil))))

(defun edraw-echo-format (format &rest args)
  (let ((message-log-max nil))
    (apply #'message format args)))

(provide 'edraw-util)
;;; edraw-util.el ends here
