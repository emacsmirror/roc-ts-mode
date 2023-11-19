;;; lang/roc/config.el -*- lexical-binding: t; -*-

;; Setup the major mode for roc source files

(define-derived-mode roc-mode prog-mode "Roc"
  "Major mode for the Roc programming language"
  (setq-local comment-start "#")
  (when (treesit-ready-p 'roc)
    (treesit-parser-create 'roc)
    (roc-mode-ts-setup)))

(add-to-list 'auto-mode-alist
             '("\\.roc\\'" . roc-mode))

(defun roc-mode-ts-setup ()
  "Setup Tree Sitter for the Roc mode"
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules roc-ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
              '((comment)
                ;; TODO: Split into features
                (everything)))

  (treesit-major-mode-setup))

(defvar roc-ts-font-lock-rules
  '(:language roc
    :override t
    :feature comment
    ((line_comment) @font-lock-constant-face)

    :language roc
    :override t
    :feature everything
    ((string) @font-lock-string-face
     (app_name) @font-lock-string-face
     (app_header) @font-lock-keyword-face
     (record) @font-lock-delimiter-face
     (record_field_expr (identifier) @font-lock-variable-name-face)
     (imports_entry) @font-lock-variable-name-face
     (provides (ident) @font-lock-variable-use-face)
     (identifier) @font-lock-variable-use-face
     (value_declaration (value_declaration_left (identifier_pattern (long_identifier (identifier))))  @font-lock-variable-name-face)
    )))


;; Available Font Lock faces
;; @font-lock-bracket-face
;; @font-lock-builtin-face
;; @font-lock-comment-face
;; @font-lock-constant-face
;; @font-lock-delimiter-face
;; @font-lock-function-name-face
;; @font-lock-keyword-face
;; @font-lock-misc-punctuation-face
;; @font-lock-number-face
;; @font-lock-operator-face
;; @font-lock-string-face
;; @font-lock-variable-name-face
;; @font-lock-variable-use-face


;; Setup the LSP support
;; TODO: Is this the right way to set up the server? Am I hindering Doom's performance?
;; See https://discourse.doomemacs.org/t/common-config-anti-patterns/119#loading-packages-too-early-3
(if (and
     (modulep! :tools lsp)
     (modulep! :lang roc +lsp))
    (use-package! lsp-mode
      :config
      (progn
        (add-to-list 'lsp-language-id-configuration '(roc-mode . "roc"))
        (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection "roc_ls")
                                              :activation-fn (lsp-activate-on "roc")
                                              :major-modes '(roc-mode)
                                              :server-id 'roc_ls))
        (add-hook 'roc-mode-local-vars-hook #'lsp!))))
