;;; roc-ts-repl.el --- Roc REPL -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ajai Khatri Nelson
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
;;  Running REPLs for the Roc programming language.
;;
;;; Code:

(require 'roc-ts-mode)
(require 'roc-ts-cli)
(require 'comint)

;;;###autoload (put 'roc-ts-repl-arguments 'risky-local-variable t)
(defcustom roc-ts-repl-arguments '()
  "Command line arguments to pass to `roc-ts-repl'."
  :group 'roc-ts
  :risky t
  :type '(repeat string))

;;;###autoload (put 'roc-ts-repl-buffer-name 'risky-local-variable t)
(defcustom roc-ts-repl-buffer-name "*roc-ts-repl*"
  "Buffer name for the comint buffer created by `roc-ts-repl'.

If this is a function, the function will be called with no
arguments and should return a buffer name.

If a buffer of that name already exists, `roc-ts-repl' will use that
buffer instead of creating a new one."
  :group 'roc-ts
  :type '(choice string function))

(defvar roc-ts-repl-prompt-regexp (rx bol "» ")
  "Prompt for `roc-ts-repl'.")

(defun roc-ts-repl--buffer-name ()
  "Get the buffer name to be used for `roc-ts-repl'.

See `roc-ts-repl-buffer-name'."
  (if (functionp roc-ts-repl-buffer-name)
      (funcall roc-ts-repl-buffer-name)
    roc-ts-repl-buffer-name))

;;;###autoload
(defun roc-ts-repl (&optional do-not-pop-buffer)
  "Open an inferior Roc REPL.

If there is a buffer with the name `roc-ts-repl-buffer-name', use
that buffer. Otherwise, create a new one.

If DO-NOT-POP-BUFFER is non-nil, do not switch to the buffer.

Returns the Roc REPL buffer."
  (interactive)
  (let* ((buffer (get-buffer-create (roc-ts-repl--buffer-name)))
         (proc-alive (comint-check-proc buffer)))
    (unless proc-alive
      ;; recreate process
      (with-current-buffer buffer
        (apply #'make-comint-in-buffer
               "Roc"                    ;NAME
               buffer                   ;BUFFER
               roc-ts-program           ;PROGRAM
               nil                      ;STARTFILE
               "repl"                   ;&rest SWITCHES
               roc-ts-repl-arguments)
        ;; reset mode
        (roc-ts-repl-mode)))
    ;; Open the roc repl buffer
    (when (and buffer (not do-not-pop-buffer))
      (pop-to-buffer buffer))
    buffer))

(define-derived-mode roc-ts-repl-mode comint-mode "Roc REPL"
  "Major mode for `roc-ts-repl'.

\\<roc-ts-mode-map>"
  (setq-local comint-prompt-regexp roc-ts-repl-prompt-regexp
              comint-use-prompt-regexp t
              comint-process-echoes t
              comint-prompt-read-only t

              indent-tabs-mode nil
              comment-start "#"
              comment-start-skip (rx (one-or-more "#") (zero-or-more blank))
              comment-column 0
              indent-tabs-mode nil
              tab-width roc-ts-indent-offset))

(defun roc-ts-repl-send-string (string)
  "Send the given STRING to a Roc REPL."
  (comint-send-string (roc-ts-repl t) string))

(defun roc-ts-repl-send-region (start end)
  "Send the region delimited by START and END to a Roc REPL."
  (comint-send-region (roc-ts-repl t) start end))

(provide 'roc-ts-repl)
;;; roc-ts-repl.el ends here
