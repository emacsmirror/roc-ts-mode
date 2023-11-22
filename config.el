;;; lang/roc/config.el -*- lexical-binding: t; -*-

;; Setup the major mode for roc source files

(define-derived-mode roc-mode prog-mode "Roc"
  "Major mode for the Roc programming language"
  (setq-local comment-start "#")
  (when (treesit-ready-p 'roc)
    (treesit-parser-create 'roc)
    (roc-mode-ts-setup)))

;; TODO: Remove once I'm done working on the highlighting.
(add-hook! 'roc-mode-hook #'treesit-inspect-mode)


(add-to-list 'auto-mode-alist
             '("\\.roc\\'" . roc-mode))

(defun roc-mode-ts-setup ()
  "Setup Tree Sitter for the Roc mode"
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules roc-ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
              '((basics)
                (application-header)
                (records)
                (if-then-else)))

  (treesit-major-mode-setup))

(defvar roc-ts-font-lock-rules
  '(:language roc
    :override t
    :feature basics
    ((line_comment) @font-lock-comment-face
     (string) @font-lock-string-face
     (identifier) @font-lock-variable-use-face
     (module) @font-lock-constant-face
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
        (to "to") @font-lock-keyword-face))))

    :language roc
    :override t
    :feature records
    ((record
      (record_field_expr (identifier)  @font-lock-property-name-face)))

    :language roc
    :override t
    :feature if-then-else
    ((if_expression
      ("if" @font-lock-keyword-face)
      ("then" @font-lock-keyword-face))
     (else_expression "else" @font-lock-keyword-face))



     ;; Available Font Lock faces: https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
    ))




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
