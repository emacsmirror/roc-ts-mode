;;; roc-mode-test.el --- Roc programming language mode test -*- lexical-binding: t; -*-
;;; Code:

(require 'roc-mode)

(ert-deftest indent-examples ()
  (ert-test-erts-file "./roc-mode-examples.erts"))

(provide 'roc-mode-test)
;;; roc-mode-test.el ends here
