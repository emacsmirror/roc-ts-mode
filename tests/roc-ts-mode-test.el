;;; roc-ts-mode-test.el --- Roc programming language mode tests -*- lexical-binding: t; -*-
;;; Code:

(require 'roc-ts-mode)

(defconst roc-ts-mode-test--dir (if load-file-name
                                    (file-name-directory load-file-name)
                                  default-directory))

(ert-deftest indent-examples ()
  "Check that roc-ts-mode indentation works correctly."
  (ert-test-erts-file (expand-file-name "./roc-ts-mode-examples.erts" roc-ts-mode-test--dir)
                      (lambda ()
                        (roc-ts-mode)
                        (indent-region (point-min) (point-max)))))

(ert-deftest confirm-roc-ts-format ()
  "Check that the roc format command produces the same result.

This is not a test of roc-ts-mode itself; it's just testing that our
indentation examples in ./roc-ts-mode-examples.erts are still
up-to-date with the output of Roc's formatter."
  (ert-test-erts-file (expand-file-name "./roc-ts-mode-examples.erts" roc-ts-mode-test--dir)
                      (lambda ()
                        (shell-command-on-region (point-min) (point-max)
                                                 "roc format --stdin --stdout"
                                                 (current-buffer) t))))

(ert-deftest roc-ts-newline-and-indent ()
  (ert-test-erts-file (expand-file-name "./roc-ts-mode-newline-and-indent.erts" roc-ts-mode-test--dir)))

(provide 'roc-ts-mode-test)
;;; roc-ts-mode-test.el ends here
