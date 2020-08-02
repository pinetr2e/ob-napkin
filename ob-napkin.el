;;; ob-napkin.el --- org-babel functions for template evaluation

;; Copyright (C) Hans Jang

;; Author: Hans Jang
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.5

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;; Org-Babel support for evaluating Napkin script, a Python based DSL to write
;; sequence diagrams.
;;

;;; Requirements:

;;
;; pip install napkin plantuml
;;

;;; Code:
(require 'ob)

(defcustom org-babel-napkin-command "napkin"
  "Name of the command for running napkin command line"
  :group 'org-babel
  :type 'string)

(defcustom org-babel-napkin-plantuml-server-url ""
  "Server URL to use in generating image file. The empty string
means to use the public server."
  :group 'org-babel
  :type 'string)

(defvar org-babel-default-header-args:napkin
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a napkin src block.")

(defun org-babel-napkin-out-file (params)
  (or (cdr (assq :file params))
      (error "napkin src block requires :file header argument.")))

(defun org-babel-expand-body:napkin (body params)
  (let* ((out-file (org-babel-napkin-out-file params))
         (vars (org-babel--get-vars params)))
    (if (string-match (rx buffer-start (0+ blank) "def" (1+ blank) "seq_diagram" (0+ blank) "(") body)
        (concat "import napkin\n"
                (format "@napkin.seq_diagram('%s')\n" (file-name-base (org-babel-process-file-name out-file)))
                body)
      (error "napkin src block requires def seq_diagram() as the first line of the contents"))))


(defun org-babel-execute:napkin (body params)
  (let* ((out-file (org-babel-napkin-out-file params))
         (img-type (file-name-extension out-file))
         (in-file (org-babel-temp-file "napkin-" ".py"))
         (expanded-body (org-babel-expand-body:napkin body params))
         (server-url org-babel-napkin-plantuml-server-url)
         (command (format "%s %s -o %s -f %s %s"
                          org-babel-napkin-command
                          (org-babel-process-file-name in-file)
                          (file-name-directory (org-babel-process-file-name out-file))
                          (concat "plantuml_" img-type)
                          (if (string-empty-p server-url) "" (concant "--server-url" server-url)))))
    (with-temp-file in-file (insert expanded-body))
    (message "%s" command)
    (org-babel-eval command "")
    nil)) ;; signal that output has already been written to file

(eval-after-load 'org
  '(add-to-list 'org-src-lang-modes '("napkin" . python)))


(provide 'ob-napkin)

;;; ob-napkin.el ends here
