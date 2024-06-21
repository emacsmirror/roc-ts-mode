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
(eval-when-compile
  (require 'cl-lib))

;;;; Custom variables

(defgroup roc nil
  "Major mode for the Roc programming language."
  :group 'languages)

(defcustom roc-mode-indent-offset 4
  "The basic indentation offset in `roc-mode'."
  :type 'natnum
  :group 'roc)

(defcustom roc-mode-program "roc"
  "The path to the roc executable."
  :type 'file
  :group 'roc)

(defcustom roc-mode-compile-function #'compile
  "The function to use when running commands like `roc-mode-test'.

This function is passed a single argument, the shell command to
run."
  :type 'function
  :group 'roc)

(defcustom roc-mode-format-replace-buffer-contents-max-secs 3
  "See the second argument of `replace-buffer-contents'."
  :type '(choice
          (integer :tag "Seconds")
          (const :tag "No timeout" nil))
  :group 'roc)

;;;; Commands

(defun roc-mode-format (&optional buffer)
  "Run \"roc format\" on BUFFER.

BUFFER defaults to the current buffer. Interactively, if a prefix
argument is specified, then BUFFER is t, which means all Roc
files in the current directory."
  (interactive (list (if current-prefix-arg 't)))
  (or buffer (setq buffer (current-buffer)))
  (cond
   ((or (eq buffer 't)
        (and (buffer-file-name buffer)
             (file-directory-p (buffer-file-name buffer))))
    (let ((directory-to-run-in (if (eq buffer 't)
                                   default-directory
                                 (buffer-file-name buffer))))
      (when (cl-loop
             for buffer in (buffer-list)
             when (with-current-buffer buffer
                    (and (derived-mode-p 'roc-mode)
                         buffer-file-name
                         (buffer-modified-p)
                         (file-in-directory-p buffer-file-name directory-to-run-in)))
             always (when (yes-or-no-p (format "Save file %s?"
                                               (buffer-file-name buffer)))
                      (with-current-buffer buffer
                        (save-buffer)
                        t)))
        (unwind-protect
            (call-process roc-mode-program nil nil nil "format" directory-to-run-in)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and (derived-mode-p 'roc-mode buffer-file-name))
                (revert-buffer t))))))))
   (t
    (with-temp-buffer
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer buffer
          (if (equal (call-process-region nil nil roc-mode-program nil temp-buffer nil "format" "--stdin" "--stdout")
                     0)
              (replace-buffer-contents temp-buffer roc-mode-format-replace-buffer-contents-max-secs)
            (message "The \"roc format\" command exited unsuccessfully."))))))))

(defun roc-mode-build (&optional file)
  "Run the \"roc build\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc build\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc-mode--run-roc-subcommand "build" (and file (list file))))

(defun roc-mode-test (&optional file)
  "Run the \"roc test\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc test\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc-mode--run-roc-subcommand "test" (and file (list file))))

(defun roc-mode-run (&optional file)
  "Run the \"roc run\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc run\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc-mode--run-roc-subcommand "run" (and file (list file))))

(defun roc-mode-dev (&optional file)
  "Run the \"roc dev\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc dev\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc-mode--run-roc-subcommand "dev" (and file (list file))))

(defun roc-mode-check (&optional file)
  "Run the \"roc check\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc check\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc-mode--run-roc-subcommand "check" (and file (list file))))

(defun roc-mode-version ()
  "Print the current version of Roc and save it to the kill ring."
  (interactive)
  (let ((output (shell-command-to-string (format "%s version" (shell-quote-argument roc-mode-program)))))
    (message (string-trim output))
    (with-temp-buffer
      (insert (string-trim output))
      (kill-ring-save (point-min) (point-max)))))

(defvar-keymap roc-mode-map
  "C-c C-f" #'roc-mode-format
  "C-c C-b" #'roc-mode-build
  "C-c C-t" #'roc-mode-test
  "C-c C-r" #'roc-mode-run
  "C-c C-d" #'roc-mode-dev
  "C-c C-c" #'roc-mode-check)

(define-derived-mode roc-mode prog-mode "Roc"
  "Major mode for the Roc programming language."
  (setq-local comment-start "#"
              comment-start-skip (rx (one-or-more "#") (zero-or-more blank))
              comment-column 0
              indent-tabs-mode nil
              tab-width roc-mode-indent-offset)
  (when (treesit-ready-p 'roc)
    (treesit-parser-create 'roc)
    (roc-mode--ts-setup)))

(add-to-list 'auto-mode-alist
             '("\\.roc\\'" . roc-mode))

(add-to-list 'treesit-language-source-alist
             '(roc . ("https://github.com/faldor20/tree-sitter-roc/")))

;;;; Private

(defun roc-mode--run-roc-subcommand (subcommand &optional arguments)
  "Run ``roc SUBCOMMAND ARGUMENTS'' in a compilation buffer."
  (when (listp arguments)
    (setq arguments (string-join (mapcar #'shell-quote-argument arguments) " ")))
  (save-selected-window
    (funcall roc-mode-compile-function
             (format "%s %s %s"
                     (shell-quote-argument roc-mode-program)
                     subcommand
                     arguments))))

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

(defvar roc-mode--ts-indent-rules
  `(;; The app header should be in the first column:
    ((node-is ,(rx bos "app_header" eos)) column-0 0)
    ;; Node types that should be at the same indentation level as their parents:
    ;; - closing brackets
    ((n-p-gp ,(rx (or "]" "}" ")")) nil nil) parent-bol 0)
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
    ;; Everything else should be indented one level further then its parent:
    (catch-all parent-bol roc-mode-indent-offset))
  "Rules for indenting Roc code based on tree-sitter.

This is assigned to an entry of `treesit-simple-indent-rules'.")

(defun roc-mode--ts-setup ()
  "Setup Tree Sitter for the Roc mode."

  ;; TODO: There is a highlight.scm file in the tree-sitter-roc codebase. How can I use it?

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules roc-mode--ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
              '((comments doc-comments definition-names)
                (keywords strings string-escapes types type-variables tag-types)
                (numbers)
                (record-field-declaration record-field-access function-calls tags variable-use modules operators boolean-negation delimiters brackets arrows lambdas assignments)))

  (setf (alist-get 'roc treesit-simple-indent-rules)
        roc-mode--ts-indent-rules)

  (treesit-major-mode-setup))

;;;; Footer

(provide 'roc-mode)

;;; roc-mode.el ends here
