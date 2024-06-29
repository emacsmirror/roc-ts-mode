;;; roc-mode-test.el --- Roc programming language mode test -*- lexical-binding: t; -*-
;;; Code:

(require 'roc-mode)

(ert-deftest indent-examples ()
  "Check that roc-mode indentation works correctly."
  (ert-test-erts-file "./roc-mode-examples.erts"
                      (lambda ()
                        (roc-mode)
                        (indent-region (point-min) (point-max)))))

(ert-deftest confirm-roc-format ()
  "Check that the roc format command produces the same result.

This is not a test of roc-mode itself; it's just testing that our
indentation examples in ./roc-mode-examples.erts are still
up-to-date with the output of Roc's formatter."
  (ert-test-erts-file "./roc-mode-examples.erts"
                      (lambda ()
                        (shell-command-on-region (point-min) (point-max)
                                                 "roc format --stdin --stdout"
                                                 (current-buffer) t))))

(ert-deftest roc-newline-and-indent ()
  (ert-test-erts-file "./roc-mode-newline-and-indent.erts"))

(provide 'roc-mode-test)
;;; roc-mode-test.el ends here
