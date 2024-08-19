;;; roc-repl.el --- Description -*- lexical-binding: t; -*-
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
;;  Description
;;
;;; Code:

(require 'roc-mode)
(require 'roc-cli)
(require 'comint)

(defcustom roc-repl-arguments '()
  "Command line arguments to pass to `roc-repl'."
  :group 'roc
  :type '(repeat string))

(defcustom roc-repl-buffer-name "*roc-repl*"
  "Buffer name for the comint buffer created by `roc-repl'.

If this is a function, the function will be called with no
arguments and should return a buffer name.

If a buffer of that name already exists, `roc-repl' will use that
buffer instead of creating a new one."
  :group 'roc
  :type '(choice string function))

(defvar roc-repl-prompt-regexp (rx bol "» ")
  "Prompt for `roc-repl'.")

(defun roc-repl--buffer-name ()
  "Get the buffer name to be used for `roc-repl'.

See `roc-repl-buffer-name'."
  (if (functionp roc-repl-buffer-name)
      (funcall roc-repl-buffer-name)
    roc-repl-buffer-name))

;;;###autoload
(defun roc-repl (&optional do-not-pop-buffer)
  "Open an inferior Roc REPL.

If there is a buffer with the name `roc-repl-buffer-name', use
that buffer. Otherwise, create a new one.

If DO-NOT-POP-BUFFER is non-nil, do not switch to the buffer.

Returns the Roc REPL buffer."
  (interactive)
  (let* ((buffer (get-buffer-create (roc-repl--buffer-name)))
         (proc-alive (comint-check-proc buffer)))
    (unless proc-alive
      ;; recreate process
      (with-current-buffer buffer
        (apply #'make-comint-in-buffer
               "Roc"                    ;NAME
               buffer                   ;BUFFER
               roc-program              ;PROGRAM
               nil                      ;STARTFILE
               "repl"                   ;&rest SWITCHES
               roc-repl-arguments)
        ;; reset mode
        (roc-repl-mode)))
    ;; Open the roc repl buffer
    (when (and buffer (not do-not-pop-buffer))
      (pop-to-buffer buffer))
    buffer))

(define-derived-mode roc-repl-mode comint-mode "Roc REPL"
  "Major mode for `roc-repl'.

\\<roc-mode-map>"
  (setq-local comint-prompt-regexp roc-repl-prompt-regexp
              comint-use-prompt-regexp t
              comint-process-echoes t
              comint-prompt-read-only t

              indent-tabs-mode nil
              comment-start "#"
              comment-start-skip (rx (one-or-more "#") (zero-or-more blank))
              comment-column 0
              indent-tabs-mode nil
              tab-width roc-indent-offset))

(defun roc-repl-send-string (string)
  "Send the given STRING to a Roc REPL."
  (comint-send-string (roc-repl t) string))

(defun roc-repl-send-region (start end)
  "Send the region delimited by START and END to a Roc REPL."
  (comint-send-region (roc-repl t) start end))

(provide 'roc-repl)
;;; roc-repl.el ends here
