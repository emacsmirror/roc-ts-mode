;;; roc-ts-mode.el --- Roc programming language mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Tad Lispy
;;
;; Author: Tad Lispy <tadeusz@lazurski.pl>
;;         Ajai Khatri Nelson <emacs@ajai.dev>
;; Maintainer: Tad Lispy <tadeusz@lazurski.pl>
;;             Ajai Khatri Nelson <emacs@ajai.dev>
;; Created: November 30, 2023
;; Version: 0.1-pre
;; Keywords: languages
;; Homepage: https://gitlab.com/tad-lispy/roc-ts-mode
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
(require 'hideshow)
(require 'newcomment)

;;;; Custom variables

(defgroup roc-ts nil
  "Major mode for the Roc programming language."
  :group 'languages)

(defcustom roc-ts-indent-offset 4
  "The basic indentation offset in `roc-ts-mode'."
  :type 'natnum
  :safe #'natnump
  :group 'roc-ts)

;;;; Commands

(defvar-keymap roc-ts-mode-map
  "C-c C-f" #'roc-ts-format
  "C-c C-b" #'roc-ts-build
  "C-c C-t" #'roc-ts-test
  "C-c C-r" #'roc-ts-run
  "C-c C-d" #'roc-ts-dev
  "C-c C-c" #'roc-ts-check
  "C-c C-e" #'roc-ts-repl
  "C-c C-s C-a" #'roc-ts-start-app
  "C-c C-s C-p" #'roc-ts-start-pkg
  "C-c C-s C-u" #'roc-ts-start-update)

(defvar roc-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; comments "# ...\n"
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; spaces
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)

    ;; strings
    (modify-syntax-entry ?\" "\"" table)
    ;; characters (can be treated as strings)
    (modify-syntax-entry ?\' "\"" table)

    ;; parens
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; part of symbol
    (modify-syntax-entry ?\_ "_" table)
    (modify-syntax-entry ?\! "_" table) ; can also be NOT

    ;; unused
    (modify-syntax-entry ?\` "@" table)
    (modify-syntax-entry ?\; "@" table)
    (modify-syntax-entry ?\~ "@" table)

    ;; escape character
    (modify-syntax-entry ?\\ "\\" table)
    ;; This is used for string interpolation. We shouldn't designate it an
    ;; escape character ("\\") because then Emacs thinks the dollar sign escapes
    ;; just the first open brace: "${something}". So we call it punctuation (".").
    (modify-syntax-entry ?\$ "." table)

    ;; punctuation
    (mapc (lambda (x)
            (modify-syntax-entry x "." table))
          "^%&|*+,-./:<=>?@")

    table))

;;;###autoload
(define-derived-mode roc-ts-mode prog-mode "Roc"
  "Major mode for the Roc programming language."
  :group 'roc-ts
  (setq-local comment-start "#"
              comment-start-skip (rx (one-or-more "#") (zero-or-more blank))
              comment-column 0
              indent-tabs-mode nil
              tab-width roc-ts-indent-offset)
  (when (treesit-ready-p 'roc)
    (treesit-parser-create 'roc)
    (roc-ts--ts-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.roc\\'" . roc-ts-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("roc" . roc-ts-mode))

(add-to-list 'treesit-language-source-alist
             '(roc . ("https://github.com/faldor20/tree-sitter-roc/")))

;;;###autoload
(defun roc-ts-install-treesit-grammar ()
  "Install the tree-sitter grammar for Roc.

Uses `treesit-install-language-grammar'."
  (interactive)
  (treesit-install-language-grammar 'roc))

;;;; Private

(defvar roc-ts--ts-font-lock-rules
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
    ((string) @font-lock-string-face
     (package_uri) @font-lock-string-face)

    :language roc
    :override t
    :feature types
    ((inferred) @font-lock-type-face
     (ability) @font-lock-type-face)

    :language roc
    :override t
    :feature type-variables
    ((bound_variable) @font-lock-type-face)

    :language roc
    :override t
    :feature tag-types
    ((tags_type
      (tag_type) @font-lock-builtin-face))

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
    ((function_call_pnc_expr
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

    ;; must be after brackets feature (because should be prioritized over brackets)
    :language roc
    :override t
    :feature string-escapes
    ((escape_char) @font-lock-escape-face
     (interpolation_char
      "${" @font-lock-escape-face
      "}" @font-lock-escape-face))

    :language roc
    :override t
    :feature arrows
    ((arrow) @font-lock-misc-punctuation-face
     (fat_arrow) @font-lock-misc-punctuation-face)

    :language roc
    :override t
    :feature lambdas
    ("|" @font-lock-misc-punctuation-face)

    :language roc
    :override t
    :feature assignments
    ((value_declaration "=" @font-lock-misc-punctuation-face)))
  "The rules for syntax highlighting Roc code based on tree-sitter.

This is passed to `treesit-font-lock-rules' and assigned to
`treesit-font-lock-settings' in `roc-ts--ts-setup'.")

(defvar roc-ts--ts-indent-rules
  `(;; The app header should be in the first column:
    ((node-is ,(rx bos "app_header" eos)) column-0 0)
    ;; Node types that should be at the same indentation level as their parents:
    ;; - closing brackets
    ((n-p-gp ,(rx (or "]" "}" ")")) nil nil) parent-bol 0)
    ;; - | in a pattern match
    ((n-p-gp ,(rx bos "|" eos) "disjunct_pattern" nil) parent-bol 0)
    ;; - all top-level things
    ((parent-is "file") parent-bol 0)
    ;; - type annotations and the LHS of value declarations
    ((n-p-gp ,(rx bos "annotation_type_def" eos) ,(rx bos "value_declaration" eos) nil) parent-bol 0)
    ((n-p-gp ,(rx bos "decl_left" eos) ,(rx bos "value_declaration" eos) nil) parent-bol 0)
    ;; - else, else if, then
    ((n-p-gp ,(rx bos (or "else" "else_if" "then") eos) "if_expr" nil) parent-bol 0)
    ;; - binary operators
    ((parent-is ,(rx bos "bin_op_expr" eos)) parent-bol 0)
    ;; - function types
    ((parent-is ,(rx bos "function_type" eos)) parent-bol 0)
    ;; - type arguments
    ((n-p-gp nil ,(rx bos "apply_type_args" eos) ,(rx bos "apply_type" eos)) parent-bol 0)
    ;; - multi-part identifiers (e.g., with "!" at the end)
    ((n-p-gp nil ,(rx bos "identifier" eos) nil) parent-bol 0)
    ;; Everything else should be indented one level further then its parent:
    (catch-all parent-bol roc-ts-indent-offset))
  "Rules for indenting Roc code based on tree-sitter.

This is assigned to an entry of `treesit-simple-indent-rules'.")

;; see `treesit-simple-indent-presets'
(defun roc-ts--first-sibling-bol (_n parent &rest _)
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-child parent 0)))
    (back-to-indentation)
    (point)))

(defconst roc-ts--next-line-further-indent-regex
  (rx (group
       (or "=" "[" "{" "(" "->" "=>" "|" ":" "expect-fx"
           (seq word-start
                (or "is" "then" "else" "expect" "where" "dbg" "app" "package" "platform" "module" "exposes" "imports" "import" "with" "packages" "requires" "provides")
                word-end)))
      (*? (syntax whitespace))
      (? "#" (* not-newline))
      eol)
  "A regex matching lines where the next line should probably be indented further.")

(defun roc-ts--prev-line ()
  "Go to the previous line. Throw an error if we're already on the first line."
  (interactive)
  (let ((old-point (point)))
    (forward-line -1)
    (when (and (bobp)
               (eq (line-number-at-pos (point)) (line-number-at-pos old-point)))
      (goto-char old-point)
      (error "Already on first line"))))

(defun roc-ts--last-nonblank-line ()
  "Go to the previous line, and keep going until we get to a non-blank one."
  (roc-ts--prev-line)
  (while (string-match-p (rx bol (* blank)
                             (? "#" (* not-newline)) ;If there's just a comment, it's still a blank line
                             eol)
                         (buffer-substring (pos-bol) (pos-eol)))
    (roc-ts--prev-line)))

(defun roc-ts--line-string ()
  "Return the current line as a string."
  (buffer-substring (pos-bol) (pos-eol)))

(defun roc-ts--line-match (regex)
  "Does the current line match this REGEX? Save match data."
  (string-match regex (roc-ts--line-string)))

(defun roc-ts--line-match-p (regex)
  "Does the current line match this REGEX? Don't save match data."
  (string-match-p regex (roc-ts--line-string)))

(defun roc-ts--line-indent-level ()
  "Return column where the text of the current line begins."
  (progn (beginning-of-line-text)
         (current-column)))

(defun roc-ts--indent-line ()
  "Like `treesit-indent', but handle some cases it gets wrong for Roc."
  (if-let* ((target-indent-level
             (without-restriction
               (or
                ;; When typing "else" at the wrong indentation level
                (ignore-errors
                  (save-excursion
                    (when (roc-ts--line-match-p (rx bol (* blank) "else" word-end))
                      (let ((num-ifs 0)
                            (num-elses 0))
                        (while (<= num-ifs num-elses)
                          (when (eq (roc-ts--line-indent-level) 0)
                            (error "roc-ts--indent-line: Can't find a matching if"))
                          (roc-ts--prev-line)
                          (when (roc-ts--line-match-p (rx word-start "if" word-end))
                            (cl-incf num-ifs))
                          (when (roc-ts--line-match-p (rx word-start "else" word-end))
                            (cl-incf num-elses)))
                        (roc-ts--line-indent-level)))))
                ;; When the last line ends with things in `roc-ts--next-line-further-indent-regex'
                ;; (like "->" or "[when ...] is").
                (ignore-errors
                  (save-excursion
                    ;; Go back to last non-blank line
                    (roc-ts--last-nonblank-line)
                    (and (roc-ts--line-match roc-ts--next-line-further-indent-regex)
                         (progn
                           ;; Shouldn't be in a comment
                           (goto-char (+ (pos-bol) (match-beginning 0)))
                           (not (equal (treesit-node-type (treesit-node-at (point)))
                                       "line_comment")))
                         ;; 4 + indent level of this line
                         (+ (roc-ts--line-indent-level)
                            roc-ts-indent-offset))))
                ;; Right after an app/module declaration
                ;; (see after-app tests in roc-ts-mode-newline-and-indent.erts)
                (ignore-errors
                  (save-excursion
                    (roc-ts--last-nonblank-line)
                    (end-of-line)
                    (let* ((node (treesit-node-at (point)))
                           (parent (treesit-node-parent node)))
                      (and (or
                            ;; it's the root node itself
                            (null (treesit-node-parent node))
                            ;; it's a child of the root node
                            (null (treesit-node-parent parent))
                            ;; it's the closing curly in an app/module declaration
                            (and (or (equal (treesit-node-type node) "}")
                                     (equal (treesit-node-type node) "]"))
                                 (or (equal (treesit-node-type parent) "packages_list")
                                     (equal (treesit-node-type parent) "exposes_list"))))
                           0))))))))
      (let ((position-within-line-text (- (point) (save-excursion (beginning-of-line-text) (point)))))
        (indent-line-to target-indent-level)
        (forward-char position-within-line-text))
    (treesit-indent)))

(defvar roc-ts--ts-simple-imenu-settings
  `(("Definition" ,(rx bos "value_declaration" eos) nil nil)
    ("Type alias" ,(rx bos "alias_type_def" eos) nil nil)
    ("Opaque type" ,(rx bos "opaque_type_def" eos) nil nil))
  "Rules for finding `imenu' entries in Roc code based on tree-sitter.")

(defun roc-ts--ts-defun-p (node)
  "Return non-nil if NODE is a value declaration or a top-level construct in Roc.

This is used as the `cdr' of `treesit-defun-type-regexp'."
  (or (equal (treesit-node-type node) "value_declaration")
      (string-match-p (rx "type_def") (treesit-node-type node))
      (equal (treesit-node-type (treesit-node-parent node)) "file")))

(defun roc-ts--ts-defun-name (node)
  "Return the name of the type or value being declared at NODE in Roc.

If NODE is not a defun or has no name, return nil.

This is assigned to `treesit-defun-name-function'."
  (cond
   ((equal (treesit-node-type node) "value_declaration")
    (treesit-node-text (treesit-node-child (car (treesit-filter-child node (lambda (n) (equal (treesit-node-type n) "decl_left")))) 0) 0))
   ((string-match-p (rx "type_def") (treesit-node-type node))
    (treesit-node-text (treesit-node-child (treesit-node-child node 0) 0)))))

(defun roc-ts--ts-setup ()
  "Setup Tree Sitter for the Roc mode."

  ;; TODO: There is a highlight.scm file in the tree-sitter-roc codebase. How can I use it?

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules roc-ts--ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
              '((comments doc-comments definition-names)
                (keywords strings string-escapes types type-variables tag-types)
                (numbers)
                (record-field-declaration record-field-access function-calls tags variable-use modules operators  delimiters brackets arrows lambdas assignments)))

  (setq-local treesit-defun-type-regexp (cons "" #'roc-ts--ts-defun-p)
              treesit-defun-name-function #'roc-ts--ts-defun-name
              treesit-simple-imenu-settings roc-ts--ts-simple-imenu-settings)

  (setf (alist-get 'roc treesit-simple-indent-rules)
        roc-ts--ts-indent-rules)

  (treesit-major-mode-setup)

  (setq indent-line-function #'roc-ts--indent-line))

;;;; Hideshow
(setf (alist-get 'roc-ts-mode hs-special-modes-alist)
      `(,roc-ts--next-line-further-indent-regex ;START
        ""                                   ;END
        ,(rx "#")                            ;COMMENT-START
        roc-ts--hideshow-end-of-block))         ;FORWARD-SEXP-FUNC

(defun roc-ts--hideshow-end-of-block (_arg)
  "Go to the end of the current block that should be folded."
  (let ((started-with-bracket-p (looking-at-p (rx (any "[{("))))
        (node (treesit-node-at (point))))
    (goto-char (treesit-node-end (treesit-node-parent node)))
    (when (and started-with-bracket-p (memq (char-before) '(?\) ?\] ?\})))
      (backward-char))))

;;;; Footer

(provide 'roc-ts-mode)

;;; roc-ts-mode.el ends here
