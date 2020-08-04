;;; ob-napkin.el --- Babel functions for Napkin -*- lexical-binding: t -*-

;; Copyright (C) Hans Jang

;; Author: Hans Jang
;; Keywords: literate programming, reproducible research, napkin, plantuml
;; Homepage: https://github.com/pinetr2e/ob-napkin
;; Version: 0.9
;; Package-Requires: ((emacs "26.1"))

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Org-Babel support for evaluating Napkin script, a Python based DSL to write
;; sequence diagrams.

;;; Requirements:

;;
;; pip install napkin plantuml
;;
;; See https://github.com/pinetr2e/napkin

;;; Code:
(require 'ob)

(defcustom org-babel-napkin-command "napkin"
  "Name of the command for running napkin tool command line."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-napkin-plantuml-server-url ""
  "Server URL to use in generating image file.
The empty string means to use the public server."
  :group 'org-babel
  :type 'string)

(defvar org-babel-default-header-args:napkin
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a napkin src block.")

(defun org-babel-napkin-out-file (params)
  "Return output file name from PARAMS."
  (or (cdr (assq :file params))
      (error "Napkin src block requires :file header argument")))

(defun org-babel-expand-body:napkin (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((out-file (org-babel-napkin-out-file params)))
    (if (string-match (rx buffer-start (0+ blank) "def" (1+ blank) "seq_diagram" (0+ blank) "(") body)
        (concat "import napkin\n"
                (format "@napkin.seq_diagram('%s')\n" (file-name-base (org-babel-process-file-name out-file)))
                body)
      (error "Napkin src block requires def seq_diagram() as the first line of the contents"))))


(defun org-babel-execute:napkin (body params)
  "Execute a block of napkin code with BODY and PARAMS with Babel.
This function is called by `org-babel-execute-src-block'."
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
                          (if (string-equal server-url "") "" (concat "--server-url " server-url)))))
    (with-temp-file in-file (insert expanded-body))
    (message "%s" command)
    (org-babel-eval command "")
    nil)) ;; signal that output has already been written to file

(eval-after-load 'org
  '(add-to-list 'org-src-lang-modes '("napkin" . python)))


(provide 'ob-napkin)

;;; ob-napkin.el ends here
