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

;;;; Private

(defvar roc-mode--ts-font-lock-rules
  '(:language roc
    :override t
    :feature comments
    ((line_comment) @font-lock-comment-face
     (doc_comment) @font-lock-comment-face)

    :language roc
    :override t
    :feature doc-comments
    ((doc_comment) @font-lock-doc-face)

    ;; https://www.roc-lang.org/tutorial#reserved-keywords
    :language roc
    :override t
    :feature keywords
    ((where) @font-lock-keyword-face
     (implements) @font-lock-keyword-face
     (when) @font-lock-keyword-face
     (is) @font-lock-keyword-face
     (exposing) @font-lock-keyword-face
     "if" @font-lock-keyword-face
     "then" @font-lock-keyword-face
     "else" @font-lock-keyword-face
     "as" @font-lock-keyword-face
     "dbg" @font-lock-keyword-face
     "expect" @font-lock-keyword-face
     ;; "expect-fx" @font-lock-keyword-face
     ;; "crash" @font-lock-keyword-face
     ;; "interface" @font-lock-keyword-face
     "app" @font-lock-keyword-face
     "package" @font-lock-keyword-face
     "platform" @font-lock-keyword-face
     "module" @font-lock-keyword-face
     ;; "hosted" @font-lock-keyword-face
     "exposes" @font-lock-keyword-face
     "imports" @font-lock-keyword-face
     "import" @font-lock-keyword-face
     ;; "with" @font-lock-keyword-face
     ;; "generates" @font-lock-keyword-face
     "packages" @font-lock-keyword-face
     "requires" @font-lock-keyword-face
     ;; "to" @font-lock-keyword-face
     "provides" @font-lock-keyword-face)

    :language roc
    :override t
    :feature strings
    ((string) @font-lock-string-face)

    :language roc
    :override t
    :feature string-escapes
    ((escape_char) @font-lock-escape-face
     (interpolation_char) @font-lock-escape-face)

    :language roc
    :override t
    :feature types
    ((concrete_type) @font-lock-type-face
     (inferred) @font-lock-type-face
     (wildcard) @font-lock-type-face
     (ability) @font-lock-type-face)

    :language roc
    :override t
    :feature type-variables
    ((bound_variable) @font-lock-type-face)

    :language roc
    :override t
    :feature tag-types
    ((tags_type
      (apply_type (concrete_type) @font-lock-builtin-face)))

    :language roc
    :override t
    :feature numbers
    ((int) @font-lock-number-face
     (xint) @font-lock-number-face
     (uint) @font-lock-number-face
     (iint) @font-lock-number-face
     (float) @font-lock-number-face
     (decimal) @font-lock-number-face
     (natural) @font-lock-number-face)

    :language roc
    :override t
    :feature record-field-declaration
    ((field_name) @font-lock-property-name-face)

    :language roc
    :override t
    :feature record-field-access
    ((field_access_expr
      target: (variable_expr (identifier))
      (identifier) @font-lock-property-use-face))

    :language roc
    :override t
    :feature function-calls
    ((function_call_expr
      caller: (variable_expr (identifier) @font-lock-function-call-face)))

    :language roc
    :override t
    :feature variable-use
    ((identifier) @font-lock-variable-use-face
     (ident) @font-lock-variable-use-face) ;; TODO: What's the difference?

    :language roc
    :override t
    :feature definition-names
    ((annotation_pre_colon) @font-lock-variable-name-face
     (decl_left
      (identifier_pattern
       (identifier) @font-lock-variable-name-face)))

    :language roc
    :override t
    :feature tags
    ((tag) @font-lock-builtin-face)

    :language roc
    :override t
    :feature modules
    ((module) @font-lock-constant-face)

    :language roc
    :override t
    :feature operators
    ((operator) @font-lock-operator-face)

    :language roc
    :override t
    :feature boolean-negation
    ((prefixed_expression
      (operator "!" @font-lock-negation-char-face)))

    :language roc
    :override t
    :feature delimiters
    ("," @font-lock-delimiter-face)

    :language roc
    :override t
    :feature brackets
    ("[" @font-lock-bracket-face
     "]" @font-lock-bracket-face
     "{" @font-lock-bracket-face
     "}" @font-lock-bracket-face
     "(" @font-lock-bracket-face
     ")" @font-lock-bracket-face)

    :language roc
    :override t
    :feature arrows
    ((arrow) @font-lock-misc-punctuation-face)

    :language roc
    :override t
    :feature lambdas
    ((backslash) @font-lock-misc-punctuation-face)

    :language roc
    :override t
    :feature assignments
    ((value_declaration "=" @font-lock-misc-punctuation-face)))
  "The rules for syntax highlighting Roc code based on tree-sitter.

This is passed to `treesit-font-lock-rules' and assigned to
`treesit-font-lock-settings' in `roc-mode--ts-setup'.")

(defun roc-mode--ts-setup ()
  "Setup Tree Sitter for the Roc mode"

  ;; TODO: There is a highlight.scm file in the tree-sitter-roc codebase. How can I use it?

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules roc-mode--ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
              '((comments doc-comments definition-names)
                (keywords strings string-escapes types type-variables tag-types)
                (numbers)
                (record-field-declaration record-field-access function-calls tags variable-use modules operators boolean-negation delimiters brackets arrows lambdas assignments)))

  (treesit-major-mode-setup))

;;;; Footer

(provide 'roc-mode)

;;; roc-mode.el ends here
