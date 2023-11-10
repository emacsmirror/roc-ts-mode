;;; lang/roc/config.el -*- lexical-binding: t; -*-

;; Setup the major mode for roc source files

(define-derived-mode roc-mode fundamental-mode "Roc"
  "Major mode for Roc programming language")
(add-to-list 'auto-mode-alist
             '("\\.roc\\'" . roc-mode))


;; Setup LSP
;; TODO: Is this the right way to set up the server? Am I hindering Doom's performance?
;; See https://discourse.doomemacs.org/t/common-config-anti-patterns/119#loading-packages-too-early-3
(use-package! lsp-mode
  :config
  (progn
    (add-to-list 'lsp-language-id-configuration '(roc-mode . "roc"))
    (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection "roc_ls")
                                          :activation-fn (lsp-activate-on "roc")
                                          :major-modes '(roc-mode)
                                          :server-id 'roc_ls))
    (add-hook 'roc-mode-local-vars-hook #'lsp!)))
