;;; lang/roc/config.el -*- lexical-binding: t; -*-

(load! "roc-mode.el")
(require 'roc-mode)

;; TODO: Remove once I'm done working on the highlighting.
(add-hook! 'roc-mode-hook #'treesit-inspect-mode)

;; Setup the LSP support

(defun roc-mode-lsp-setup ()
  "Setup LSP for Roc. Requires lsp-mode package."
  (add-to-list 'lsp-language-id-configuration '(roc-mode . "roc"))
  (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection "roc_ls")
                                        :activation-fn (lsp-activate-on "roc")
                                        :major-modes '(roc-mode)
                                        :server-id 'roc_ls)))

;; TODO: Is this the right way to set up the server? Am I hindering Doom's performance?
;; See https://discourse.doomemacs.org/t/common-config-anti-patterns/119#loading-packages-too-early-3
(if (and
     (modulep! :tools lsp)
     (modulep! :lang roc +lsp))
    (use-package! lsp-mode
      :config
      (progn
        (roc-mode-lsp-setup)
        (add-hook 'roc-mode-local-vars-hook #'lsp!))))
