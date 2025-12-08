;;; edraw-color-picker-mode.el --- Use color picker in any buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Color Picker

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

;; This file defines minor modes that enable a color picker in any buffer.
;;
;; It provides the following two minor modes:
;; - `edraw-color-picker-mode'
;; - `edraw-color-picker-global-mode'

;; * `edraw-color-picker-mode'
;;
;; `edraw-color-picker-mode' binds commands provided by
;; edraw-color-picker.el to keys in the buffer where the mode is
;; turned on. These commands replace or insert text representing
;; colors.
;;
;; Note: If you prefer to manually bind these commands to your desired keymap
;; (e.g., by adding hooks in init.el), this minor mode is generally unnecessary.
;;
;; Configuring key bindings:
;;
;;   Via Customize:
;;     M-x customize-variable edraw-color-picker-mode-custom-bindings
;;
;; Then, turn on `edraw-color-picker-mode' in the desired buffer:
;;
;;   M-x edraw-color-picker-mode

;; * `edraw-color-picker-global-mode'
;;
;; `edraw-color-picker-global-mode' is a global minor mode that turns
;; on `edraw-color-picker-mode' across all buffers.
;;
;; Configuring enabled major modes:
;;
;;   M-x customize-variable edraw-color-picker-global-modes
;;
;; Then, turn on `edraw-color-picker-global-mode' after Emacs starts:
;;
;;   M-x edraw-color-picker-global-mode
;;
;; Or, add the following to your init.el:
;;
;;   (require 'edraw-color-picker-mode)
;;   (edraw-color-picker-global-mode)

;;; Code:

(require 'cl-lib)

;;;; Declarations

(autoload 'edraw-color-picker-insert-color-at
  "edraw-color-picker" nil t)
(autoload 'edraw-color-picker-replace-color-at
  "edraw-color-picker" nil t)
(autoload 'edraw-color-picker-replace-or-insert-color-at
  "edraw-color-picker" nil t)

(autoload 'edraw-color-info-at "edraw-color")

(defgroup edraw-color-picker-mode nil
  "Use color picker in any buffer."
  :tag "Edraw Color Picker Mode"
  :group 'edraw)

;;;; Minor mode (edraw-color-picker-mode)
;;;;; Customization variables

(defvar edraw-color-picker-mode-custom-bindings)

(defcustom edraw-color-picker-mode-use-context-menu t
  "Non-nil means add color picker related menu items to the context menu."
  :group 'edraw-color-picker-mode
  :type 'boolean)

;;;;; Minor mode maps

(defvar edraw-color-picker-mode-menu-map
  (easy-menu-create-menu
   "Color Picker"
   '(["Insert color" edraw-color-picker-insert-color-at]
     ["Replace color at point" edraw-color-picker-replace-color-at]
     ["Replace or insert color at point"
      edraw-color-picker-replace-or-insert-color-at]))
  "Menu of command `edraw-color-picker-mode'.")

(defalias 'edraw-color-picker-mode--cusmap-global-map (make-sparse-keymap))

(defvar edraw-color-picker-mode-map
  (let ((km (make-composed-keymap
             (list 'edraw-color-picker-mode--cusmap-global-map))))
    (define-key km
                [menu-bar edraw-color-picker-mode]
                edraw-color-picker-mode-menu-map)
    km)
  "The keymap used by `edraw-color-picker-mode'.
\\{edraw-color-picker-mode-map}")

;;;;; Minor mode definition

;;;###autoload
(define-minor-mode edraw-color-picker-mode
  "Toggle color picker support for any buffer.

`edraw-color-picker-mode' is a minor mode that enables replacing or
inserting color text using a color picker in any buffer.

The following keybindings are available in `edraw-color-picker-mode'
\\{edraw-color-picker-mode-map}
The buffer local bindings:
\\{edraw-color-picker-mode--cusmap-buffer-local-keymap}
\(you can change the binding by M-x customize-variable
`edraw-color-picker-mode-custom-bindings'"
  :group 'edraw-color-picker-mode

  (cond
   (edraw-color-picker-mode
    ;; Turn-on
    (edraw-color-picker-mode--cusmap-activate-local-keymap)
    (add-hook 'change-major-mode-hook #'edraw-color-picker-mode--teardown
              nil t)
    (when edraw-color-picker-mode-use-context-menu
      (add-hook 'context-menu-functions #'edraw-color-picker-mode-context-menu
                10 t)))
   (t
    ;; Turn-off
    (remove-hook 'context-menu-functions #'edraw-color-picker-mode-context-menu
                 t)
    (remove-hook 'change-major-mode-hook #'edraw-color-picker-mode--teardown t)
    (edraw-color-picker-mode--teardown))))

(defun edraw-color-picker-mode--teardown ()
  (when (edraw-color-picker-mode--cusmap-deactivate-local-keymap)
    (edraw-color-picker-mode--cusmap-clean-unused-minor-mode)))

;;;;; Custom Keymap

;; The following is the mechanism required to achieve different
;; keymaps per buffer via defcustom.

(defvar-local edraw-color-picker-mode--cusmap-buffer-local-keymap nil
  "The local keymap for the current buffer.
This is only there to be referenced from the docstring of
`edraw-color-picker-mode' function.
Do not modify the contents of this keymap directly. Even the key
bindings of unrelated buffers may change.")

(defvar-local edraw-color-picker-mode--cusmap-buffer-local-minor-mode nil
  "The name of the minor mode that implements the buffer-local keymap.")

(defun edraw-color-picker-mode--cusmap-needs-local-map-p ()
  "Return non-nil if the current buffer needs a buffer-local keymap."
  (cl-loop for (condition . command)
           in edraw-color-picker-mode-custom-bindings
           when (eq (edraw-color-picker-mode--cusmap-test condition 'all)
                    'local)
           return t))

(defun edraw-color-picker-mode--cusmap-create-keymap (scope)
  "Create a keymap based on `edraw-color-picker-mode-custom-bindings'.

SCOPE is one of the symbols `all', `global', or `local'."
  (let ((keymap (make-sparse-keymap)))
    (cl-loop for (condition . command)
             in edraw-color-picker-mode-custom-bindings
             when (edraw-color-picker-mode--cusmap-test condition scope)
             do
             (let* ((keyspec (if (consp condition) (cdr condition) condition))
                    (key (if (stringp keyspec) (kbd keyspec) keyspec)))
               (define-key keymap key command)))
    keymap))

(defun edraw-color-picker-mode--cusmap-test (condition scope)
  "Return non-nil if CONDITION is true.

CONDITION is the condition part specified in
`edraw-color-picker-mode-custom-bindings'.

SCOPE is one of the symbols `all', `global', or `local'.

  `global' : Ignore conditions that are true only under certain conditions.
  `local'  : Ignore conditions that are true all the time.
  `all'    : Consider all conditions."
  (if (and (memq scope '(all global))
           (or (stringp condition) (vectorp condition)))
      'global
    (when (memq scope '(all local))
      (pcase condition
        ;; (MODE . KEY)
        (`(,(and (pred symbolp) mode) . ,_key)
         (when (derived-mode-p mode) 'local))
        ;; ((pred FUNCTION ARG...) . KEY)
        (`((pred ,function . ,args) . ,_key)
         (when (apply function args) 'local))
        ;; ((MODE...) . KEY)
        (`(,(and (pred consp) modes) . ,_key)
         (when (apply #'derived-mode-p modes) 'local))
        (_ nil)))))

(defun edraw-color-picker-mode--cusmap-keymap-to-minor-mode-map (keymap)
  "Convert a KEYMAP to a minor mode map."
  ;; Find an existing minor mode that matches KEYMAP.
  (let ((mmm-or-new-id
         (edraw-color-picker-mode--cusmap-find-minor-mode-map keymap)))
    (if (consp mmm-or-new-id)
        ;; If found, return minor mode map.
        mmm-or-new-id
      ;; If not found, create a new minor mode.
      (edraw-color-picker-mode--cusmap-create-new-minor-mode-map
       keymap mmm-or-new-id))))

(defconst edraw-color-picker-mode--cusmap-minor-mode-regexp
  "\\`edraw-color-picker-mode-local-\\([0-9]+\\)-mode\\'")

(defconst edraw-color-picker-mode--cusmap-minor-mode-format
  "edraw-color-picker-mode-local-%d-mode")

(defun edraw-color-picker-mode--cusmap-find-minor-mode-map (keymap)
  "Find a minor mode map in `minor-mode-map-alist' whose name was generated
by `edraw-color-picker-mode--cusmap-create-new-minor-mode-map' and whose
keymap matches KEYMAP. If found, return (varname . keymap). If not
found, return a integer that can be used for a new definition."
  (cl-loop for mmm in minor-mode-map-alist
           for (mmm-varname . mmm-keymap) = mmm
           for mmm-varname-str = (symbol-name mmm-varname)
           when (string-match
                 edraw-color-picker-mode--cusmap-minor-mode-regexp
                 mmm-varname-str)
           if (equal mmm-keymap keymap)
           return mmm
           else
           maximize (string-to-number (match-string 1 mmm-varname-str))
           into max-id
           finally return (1+ (or max-id -1))))

(defun edraw-color-picker-mode--cusmap-create-new-minor-mode-map (keymap id)
  "Create a minor mode map to realize the buffer-local KEYMAP.
ID is an integer that is an unused minor mode ID.
Return the entry of the form (varname . KEYMAP) that was added to
`minor-mode-map-alist'."
  (let* ((varname
          (intern
           (format edraw-color-picker-mode--cusmap-minor-mode-format id)))
         (mmm (cons varname keymap)))
    ;; Create buffer local variable
    (set varname nil)
    (make-variable-buffer-local varname)
    ;; Add KEYMAP to `minor-mode-map-alist'
    (push mmm minor-mode-map-alist)
    mmm))

(defun edraw-color-picker-mode--cusmap-activate-local-keymap ()
  "Enable local keymap in current buffer."
  ;; Disable the current local keymap
  (edraw-color-picker-mode--cusmap-deactivate-local-keymap)

  ;; Set a new local keymap minor mode if needed
  (when (edraw-color-picker-mode--cusmap-needs-local-map-p)
    (let* ((keymap (edraw-color-picker-mode--cusmap-create-keymap 'local))
           (mmm ;; MMM is a cons cell used in `minor-mode-map-alist'
            (edraw-color-picker-mode--cusmap-keymap-to-minor-mode-map keymap))
           (mmm-varname (car mmm))
           (mmm-keymap (cdr mmm)))
      ;; Turn on local keymap minor mode
      (set mmm-varname t)
      ;; Record the name of the local keymap minor mode used in the buffer
      (setq edraw-color-picker-mode--cusmap-buffer-local-minor-mode
            mmm-varname
            ;; Use MMM-KEYMAP instead of KEYMAP for reduce memory
            edraw-color-picker-mode--cusmap-buffer-local-keymap
            mmm-keymap))))

(defun edraw-color-picker-mode--cusmap-deactivate-local-keymap ()
  "Disable local keymap in current buffer."
  (when edraw-color-picker-mode--cusmap-buffer-local-minor-mode
    ;; Turn off local keymap minor mode
    (kill-local-variable
     edraw-color-picker-mode--cusmap-buffer-local-minor-mode)
    (setq edraw-color-picker-mode--cusmap-buffer-local-minor-mode nil
          edraw-color-picker-mode--cusmap-buffer-local-keymap nil)
    t))

(defun edraw-color-picker-mode--cusmap-clean-unused-minor-mode ()
  "Remove unused local keymap minor modes."
  (let (used-minor-modes)
    ;; Enumerate used local keymap minor mode names
    (dolist (buffer (buffer-list))
      (let ((varname (buffer-local-value
                      'edraw-color-picker-mode--cusmap-buffer-local-minor-mode
                      buffer)))
        (when (and varname (not (memq varname used-minor-modes)))
          (push varname used-minor-modes))))
    ;; Delete unused local keymap minor mode in `minor-mode-map-alist'
    (setq minor-mode-map-alist
          (cl-delete-if
           (lambda (varname-keymap)
             (let ((varname (car varname-keymap)))
               (when (and
                      (string-match-p
                       "\\`edraw-color-picker-mode-local-\\([0-9]+\\)-mode\\'"
                       (symbol-name varname))
                      (not (memq varname used-minor-modes)))
                 (makunbound varname)
                 t)))
           minor-mode-map-alist))))

(defun edraw-color-picker-mode--cusmap-update-global-map ()
  "Update the keymap to be used commonly in all buffers."
  (fset 'edraw-color-picker-mode--cusmap-global-map
        (edraw-color-picker-mode--cusmap-create-keymap 'global)))

(defun edraw-color-picker-mode--cusmap-update-all-keymaps ()
  "Update all keymaps."
  (edraw-color-picker-mode--cusmap-update-global-map)

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if edraw-color-picker-mode
          (edraw-color-picker-mode--cusmap-activate-local-keymap)
        (edraw-color-picker-mode--cusmap-deactivate-local-keymap))))

  (edraw-color-picker-mode--cusmap-clean-unused-minor-mode))

(defun edraw-color-picker-mode-update-custom-map ()
  "Update all keymaps set by the customization variable
`edraw-color-picker-mode-custom-bindings'."
  (edraw-color-picker-mode--cusmap-update-all-keymaps))

;;;;; Customization Variable for custom keymap

;; Note: The following customization variable must be placed after the
;; definition of the function `edraw-color-picker-mode-update-custom-map'.
(defcustom edraw-color-picker-mode-custom-bindings
  '(("C-c C-." . edraw-color-picker-replace-or-insert-color-at)
    ("M-S-<mouse-1>" . edraw-color-picker-replace-color-at))
  "A list of key bindings for `edraw-color-picker-mode'.

Each element of the list is a cons cell where the car is a condition and
the cdr is a command.

The condition is either a key or a mode-specific key.

A key is a string passed to the kbd function.

A mode-specific key is a cons cell where the car is a list of major mode
symbols and the cdr is a key.

  `edraw-color-picker-mode-keys' : ( <binding> ... )

  <binding> : (<condition> . <command>)

  <condition> : <key> | <mode-specific-key>

  <mode-specific-key> : (<major-mode-symbol-list> . <key>)

  <key> : Pass to `kbd'.
  <major-mode-symbol-list> : Pass to `derived-mode-p'."
  :group 'edraw-color-picker-mode
  :type '(repeat
          :tag "Bindings"
          (cons
           :tag "Binding"
           (choice :tag "Condition"
                   (string :tag "Key")
                   (cons
                    :tag "Specific major-mode"
                    (repeat :tag "Major-modes" (symbol :tag "Mode"))
                    (string :tag "Key")))
           (choice :tag "Command"
                   (const
                    :tag "Insert color"
                    edraw-color-picker-insert-color-at)
                   (const
                    :tag "Replace color"
                    edraw-color-picker-replace-color-at)
                   (const
                    :tag "Replace or insert color"
                    edraw-color-picker-replace-or-insert-color-at)
                   (function :tag "Command"))))
  :risky t
  :set
  (lambda (variable value)
    (set-default variable value)
    (when (fboundp 'edraw-color-picker-mode-update-custom-map)
      (edraw-color-picker-mode-update-custom-map))
    value))

;;;;; Context menu

(defun edraw-color-picker-mode-context-menu (menu click)
  (let* ((posn (event-end click))
         (buffer (window-buffer (posn-window posn)))
         (point (posn-point posn))
         (color-info (with-current-buffer buffer
                       (edraw-color-info-at point))))

    (define-key-after menu [separator-edraw-color-picker] menu-bar-separator)
    (if color-info
        (define-key-after
          menu [edraw-color-picker-replace]
          '(menu-item "Replace Color" edraw-color-picker-replace-color-at
                      :help "Replace the color text using color picker"))
      (define-key-after
        menu [edraw-color-picker-insert]
        '(menu-item "Insert Color" edraw-color-picker-insert-color-at
                    :help "Insert color text using color picker"))))
  menu)


;;;; Global minor mode (edraw-color-picker-global-mode)

(defcustom edraw-color-picker-global-modes t
  "Modes in which `edraw-color-picker-mode' is turned on in
`edraw-color-picker-global-mode'.

  nil : None
  t : All modes
  (MODE ... ) : Specific MODEs
  (not (MODE ... )) : Not specific MODEs"
  :group 'edraw-color-picker-mode
  :type '(choice (const :tag "None" nil)
                 (const :tag "All modes" t)
                 (cons :tag "Not modes"
                       (const :format "" not)
                       (repeat :tag "Modes" (symbol :tag "Mode")))
                 (repeat :tag "Modes" (symbol :tag "Mode"))))

;;;###autoload
(define-globalized-minor-mode edraw-color-picker-global-mode
  edraw-color-picker-mode
  edraw-color-picker-global-mode--turn-on
  :group 'edraw-color-picker-mode
  (cond
   (edraw-color-picker-global-mode
    (easy-menu-add-item
     nil '("Tools") edraw-color-picker-mode-menu-map))
   (t
    (easy-menu-remove-item
     nil '("Tools") (cadr edraw-color-picker-mode-menu-map)))))

(defun edraw-color-picker-global-mode--turn-on ()
  (when (edraw-color-picker-global-mode--target-mode-p)
    (edraw-color-picker-mode)))

(defun edraw-color-picker-global-mode--target-mode-p ()
  (pcase edraw-color-picker-global-modes
    ('nil nil)
    ('t t)
    (`(not . ,(and (pred listp) modes)) (not (memq major-mode modes)))
    ((and (pred listp) modes) (memq major-mode modes))))


(provide 'edraw-color-picker-mode)
;;; edraw-color-picker-mode.el ends here
