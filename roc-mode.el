;;; lang/roc/roc-mode.el -*- lexical-binding: t; -*-

;;; roc-mode.el --- Roc programming language mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Tad Lispy
;;
;; Author: Tad Lispy <tadeusz@lazurski.pl>
;; Maintainer: Tad Lispy <tadeusz@lazurski.pl>
;; Created: November 30, 2023
;; Version: 0.1-pre
;; Keywords: languages
;; Homepage: https://gitlab.com/tad-lispy/roc-mode
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Roc is a strongly typed functional programming language inspired by Elm, but
;;  for variety of platforms. See https://www.roc-lang.org/.
;;
;;; Code:


(require 'treesit)

;;;;; Commands

(define-derived-mode roc-mode prog-mode "Roc"
  "Major mode for the Roc programming language"
  (setq-local comment-start "#")
  (when (treesit-ready-p 'roc)
    (treesit-parser-create 'roc)
    (roc-mode--ts-setup)))

(add-to-list 'auto-mode-alist
             '("\\.roc\\'" . roc-mode))

(add-to-list 'treesit-language-source-alist
             '(roc . ("https://github.com/faldor20/tree-sitter-roc/")))

(defgroup roc nil
  "Major mode for the Roc programming language."
  :group 'languages)

(defcustom roc-mode-indent-offset 4
  "The basic indentation offset in `roc-mode'."
  :type 'natnum
  :group 'roc)

;;;; Private

(defvar roc-mode--ts-font-lock-rules
  '(:language roc
    :override t
    :feature basics
    ((line_comment) @font-lock-comment-face
     (string) @font-lock-string-face
     (identifier) @font-lock-variable-use-face
     (ident) @font-lock-variable-use-face ;; TODO: What's the difference?
     (module) @font-lock-constant-face
     (field_name) @font-lock-property-name-face
     (expect "expect" @font-lock-keyword-face))

    :language roc
    :override t
    :feature application-header
    ((app_header
      "app" @font-lock-keyword-face
      (app_name) @font-lock-string-face
      (app_header_body
       (packages ("packages") @font-lock-keyword-face)
       (imports ("imports") @font-lock-keyword-face)
       (provides
        ("provides") @font-lock-keyword-face
        (to) @font-lock-keyword-face))))

    :language roc
    :override t
    :feature if-then-else
    ((if_expr
      (if) @font-lock-keyword-face
      (then ("then") @font-lock-keyword-face)
      (else ("else") @font-lock-keyword-face)))))

(defvar roc-mode--ts-indent-rules
  `(((node-is ,(rx bos "app_header" eos)) column-0 0)
    ((n-p-gp ,(rx (or "]" "}" ")")) nil nil) parent-bol 0)
    ((parent-is "file") parent-bol 0)
    ((n-p-gp ,(rx bos "annotation_type_def" eos) ,(rx bos "value_declaration" eos) nil) parent-bol 0)
    ((n-p-gp ,(rx bos "decl_left" eos) ,(rx bos "value_declaration" eos) nil) parent-bol 0)
    ((n-p-gp ,(rx bos (or "else" "else_if" "then") eos) "if_expr" nil) parent-bol 0)
    ((parent-is ,(rx bos "bin_op_expr" eos)) parent-bol 0)
    ((parent-is ,(rx bos "function_type" eos)) parent-bol 0)
    ((n-p-gp nil ,(rx bos "apply_type_args" eos) ,(rx bos "apply_type" eos)) parent-bol 0)
    (catch-all parent-bol roc-mode-indent-offset))
  "Rules for indenting Roc code based on tree-sitter.

This is assigned to an entry of `treesit-simple-indent-rules'.")

(defun roc-mode--ts-setup ()
  "Setup Tree Sitter for the Roc mode"

  ;; TODO: There is a highlight.scm file in the tree-sitter-roc codebase. How can I use it?

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules roc-mode--ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
              '((basics)
                (application-header)
                (if-then-else)))

  (setf (alist-get 'roc treesit-simple-indent-rules)
        roc-mode--ts-indent-rules)

  (treesit-major-mode-setup))

;;;; Footer

(provide 'roc-mode)

;;; roc-mode.el ends here
