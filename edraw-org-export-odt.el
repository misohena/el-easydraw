;;; edraw-org-export-odt.el --- Export edraw link As ODT in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Graphics, Drawing, SVG, Editor, Orgmode

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

(require 'ox-odt)
(require 'edraw-org)

;;;; Export

(defun edraw-org-export-odt-link (path _description _back-end info link)
  ;; @todo Check inline-image-rules? However, unless it is converted
  ;; into an inline image, it will just output base64 data, so I don't
  ;; think it is practical.
  ;; (when (org-export-inline-image-p
  ;;        link (plist-get info :odt-inline-image-rules))
  (if-let* ((file (edraw-org-export-get-file-from-edraw-path path)))
      (edraw-org-export-link-as-file
       link info file
       ;; You may process any ODT specific options set in LINK
       ;; (e.g.[[edraw:odt-some-option=??;file=...]]) here.
       ;; (lambda (link info) .... (org-odt-link--inline-image link info))
       #'org-odt-link--inline-image)
    ""))

(provide 'edraw-org-export-odt)
;;; edraw-org-export-odt.el ends here
