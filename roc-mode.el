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
(require 'hideshow)
(require 'newcomment)
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

;; silence warnings about #'roc-repl not necessarily being defined
(autoload 'roc-repl "roc-repl")
(autoload 'roc-start-fetch "roc-start")
(autoload 'roc-start-app "roc-start")
(autoload 'roc-start-pkg "roc-start")
(autoload 'roc-start-update "roc-start")

(defvar-keymap roc-mode-map
  "C-c C-f" #'roc-mode-format
  "C-c C-b" #'roc-mode-build
  "C-c C-t" #'roc-mode-test
  "C-c C-r" #'roc-mode-run
  "C-c C-d" #'roc-mode-dev
  "C-c C-c" #'roc-mode-check
  "C-c C-e" #'roc-repl
  "C-c C-s C-f" #'roc-start-fetch
  "C-c C-s C-a" #'roc-start-app
  "C-c C-s C-p" #'roc-start-pkg
  "C-c C-s C-u" #'roc-start-update)

;;;###autoload
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

;;;###autoload
(defun roc-mode-install-treesit-grammar ()
  "Install the tree-sitter grammar for Roc.

Uses `treesit-install-language-grammar'."
  (interactive)
  (treesit-install-language-grammar 'roc))

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
    ;; Everything else should be indented one level further then its parent:
    (catch-all parent-bol roc-mode-indent-offset))
  "Rules for indenting Roc code based on tree-sitter.

This is assigned to an entry of `treesit-simple-indent-rules'.")

(defconst roc-mode--next-line-further-indent-regex
  (rx (group
       (or "=" "[" "{" "(" "->" ":" "expect-fx"
           (seq word-start
                (or "is" "then" "else" "expect" "where" "dbg" "app" "package" "platform" "module" "exposes" "imports" "import" "with" "packages" "requires" "provides")
                word-end)))
      (*? (syntax whitespace))
      (? "#" (* not-newline))
      eol)
  "A regex matching lines where the next line should probably be indented further.")

(defun roc-mode--last-nonblank-line ()
  "Go to the previous line, and keep going until we get to a non-blank one."
  (forward-line -1)
  (while (string-match-p (rx bol (* blank) eol)
                         (buffer-substring (pos-bol) (pos-eol)))
    (forward-line -1)))

(defun roc-mode--line-string ()
  "Return the current line as a string."
  (buffer-substring (pos-bol) (pos-eol)))

(defun roc-mode--line-match (regex)
  "Does the current line match this REGEX? Save match data."
  (string-match regex (roc-mode--line-string)))

(defun roc-mode--line-match-p (regex)
  "Does the current line match this REGEX? Don't save match data."
  (string-match-p regex (roc-mode--line-string)))

(defun roc-mode--line-indent-level ()
  "Return column where the text of the current line begins."
  (progn (beginning-of-line-text)
         (current-column)))

(defun roc-mode--indent-line ()
  "Like `treesit-indent', but handle some cases it gets wrong for Roc."
  (if-let* ((target-indent-level
             (or
              ;; When typing "else" at the wrong indentation level
              (ignore-errors
                (save-excursion
                  (when (roc-mode--line-match-p (rx bol (* blank) "else" word-end))
                    (let ((num-ifs 0)
                          (num-elses 0))
                      (while (<= num-ifs num-elses)
                        (when (eq (roc-mode--line-indent-level) 0)
                          (error "roc-mode--indent-line: Can't find a matching if"))
                        (forward-line -1)
                        (when (roc-mode--line-match-p (rx word-start "if" word-end))
                          (cl-incf num-ifs))
                        (when (roc-mode--line-match-p (rx word-start "else" word-end))
                          (cl-incf num-elses)))
                      (roc-mode--line-indent-level)))))
              ;; When the last line ends with things in `roc-mode--next-line-further-indent-regex'
              ;; (like "->" or "[when ...] is").
              (ignore-errors
                (save-excursion
                  ;; Go back to last non-blank line
                  (roc-mode--last-nonblank-line)
                  (and (roc-mode--line-match roc-mode--next-line-further-indent-regex)
                       (progn
                         ;; Shouldn't be in a comment
                         (goto-char (+ (pos-bol) (match-beginning 0)))
                         (not (equal (treesit-node-type (treesit-node-at (point)))
                                     "line_comment")))
                       ;; 4 + indent level of this line
                       (+ (roc-mode--line-indent-level)
                          roc-mode-indent-offset)))))))
      (let ((position-within-line-text (- (point) (save-excursion (beginning-of-line-text) (point)))))
        (indent-line-to target-indent-level)
        (forward-char position-within-line-text))
    (treesit-indent)))

(defvar roc-mode--ts-simple-imenu-settings
  `(("Definition" ,(rx bos "value_declaration" eos) nil nil)
    ("Type alias" ,(rx bos "alias_type_def" eos) nil nil)
    ("Opaque type" ,(rx bos "opaque_type_def" eos) nil nil))
  "Rules for finding `imenu' entries in Roc code based on tree-sitter.")

(defun roc-mode--ts-defun-p (node)
  "Return non-nil if NODE is a value declaration or a top-level construct in Roc.

This is used as the `cdr' of `treesit-defun-type-regexp'."
  (or (equal (treesit-node-type node) "value_declaration")
      (string-match-p (rx "type_def") (treesit-node-type node))
      (equal (treesit-node-type (treesit-node-parent node)) "file")))

(defun roc-mode--ts-defun-name (node)
  "Return the name of the type or value being declared at NODE in Roc.

If NODE is not a defun or has no name, return nil.

This is assigned to `treesit-defun-name-function'."
  (cond
   ((equal (treesit-node-type node) "value_declaration")
    (treesit-node-text (treesit-node-child (car (treesit-filter-child node (lambda (n) (equal (treesit-node-type n) "decl_left")))) 0) 0))
   ((string-match-p (rx "type_def") (treesit-node-type node))
    (treesit-node-text (treesit-node-child (treesit-node-child node 0) 0)))))

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

  (setq-local treesit-defun-type-regexp (cons "" #'roc-mode--ts-defun-p)
              treesit-defun-name-function #'roc-mode--ts-defun-name
              treesit-simple-imenu-settings roc-mode--ts-simple-imenu-settings)

  (setf (alist-get 'roc treesit-simple-indent-rules)
        roc-mode--ts-indent-rules)

  (treesit-major-mode-setup)

  (setq indent-line-function #'roc-mode--indent-line))

;;;; Hideshow
(setf (alist-get 'roc-mode hs-special-modes-alist)
      `(,roc-mode--next-line-further-indent-regex ;START
        ""                                        ;END
        ,(rx "#")                                 ;COMMENT-START
        roc-mode--hideshow-end-of-block           ;FORWARD-SEXP-FUNC
        nil                                       ;ADJUST-BEG-FUNC
        nil                                       ;FIND-BLOCK-BEGINNING-FUNC
        nil                                       ;FIND-NEXT-BLOCK-FUNC
        roc-mode--hideshow-block-start-p))        ;LOOKING-AT-BLOCK-START-P-FUNC

(defun roc-mode--hideshow-end-of-block (_arg)
  ""
  (let ((started-with-bracket-p (looking-at-p (rx (any "[{("))))
        (node (treesit-node-at (point))))
    (goto-char (treesit-node-end (treesit-node-parent node)))
    (when (and started-with-bracket-p (memq (char-before) '(?\) ?\] ?\})))
      (backward-char))))

(defun roc-mode--hideshow-block-start-p ()
  ""
  (and (hs-looking-at-block-start-p)
       (not
        (equal (treesit-node-type (treesit-node-at (point)))
               "line_comment"))))

;;;; Footer

(provide 'roc-mode)

;;; roc-mode.el ends here
