;;; roc-cli.el --- Commands for running the Roc CLI -*- lexical-binding: t; -*-
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
;;  Commands for running Roc programming language CLI commands.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;; Custom variables

(defcustom roc-program "roc"
  "The path to the roc executable."
  :type 'file
  :group 'roc)

(defcustom roc-compile-function #'compilation-start
  "The function to use when running commands like `roc-test'.

This function is passed a single argument, the shell command to
run."
  :type 'function
  :group 'roc)

(defcustom roc-format-replace-buffer-contents-max-secs 3
  "See the second argument of `replace-buffer-contents'."
  :type '(choice
          (integer :tag "Seconds")
          (const :tag "No timeout" nil))
  :group 'roc)

;;;; Commands

(defun roc-format (&optional buffer)
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
            (call-process roc-program nil nil nil "format" directory-to-run-in)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and (derived-mode-p 'roc-mode) buffer-file-name)
                (revert-buffer t))))))))
   (t
    (with-temp-buffer
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer buffer
          (if (equal (call-process-region nil nil roc-program nil temp-buffer nil "format" "--stdin" "--stdout")
                     0)
              (replace-buffer-contents temp-buffer roc-format-replace-buffer-contents-max-secs)
            (message "The \"roc format\" command exited unsuccessfully."))))))))

(defun roc-build (&optional file)
  "Run the \"roc build\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc build\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc--run-roc-subcommand "build" (and file (list file))))

(defun roc-test (&optional file)
  "Run the \"roc test\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc test\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc--run-roc-subcommand "test" (and file (list file))))

(defun roc-run (&optional file)
  "Run the \"roc run\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc run\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc--run-roc-subcommand "run" (and file (list file))))

(defun roc-dev (&optional file)
  "Run the \"roc dev\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc dev\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc--run-roc-subcommand "dev" (and file (list file))))

(defun roc-check (&optional file)
  "Run the \"roc check\" command on FILE.

Interactively, FILE is the current file. If a prefix argument is
specified, then FILE is nil, meaning no file argument is passed
to \"roc check\"."
  (interactive (list (unless current-prefix-arg (buffer-file-name))))
  (roc--run-roc-subcommand "check" (and file (list file))))

(defun roc-version ()
  "Print the current version of Roc and save it to the kill ring."
  (interactive)
  (let ((output (shell-command-to-string (format "%s version" (shell-quote-argument roc-program)))))
    (message (string-trim output))
    (with-temp-buffer
      (insert (string-trim output))
      (kill-ring-save (point-min) (point-max)))))

;;;; Private

(defun roc--run-roc-subcommand (subcommand &optional arguments)
  "Run ``roc SUBCOMMAND ARGUMENTS'' in a compilation buffer."
  (when (listp arguments)
    (setq arguments (string-join (mapcar #'shell-quote-argument arguments) " ")))
  (save-selected-window
    (funcall roc-compile-function
             (format "%s %s %s"
                     (shell-quote-argument roc-program)
                     subcommand
                     arguments))))

(provide 'roc-cli)
;;; roc-cli.el ends here
