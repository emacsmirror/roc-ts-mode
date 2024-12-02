;;; roc-ts-start.el --- Starting new Roc apps/packages -*- lexical-binding: t; -*-
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
;;  Experimental commands for using Ian McLerran's roc-start program
;;  (<https://github.com/imclerran/roc-start>) to start new Roc applications and
;;  packages.
;;
;;; Code:

(require 'roc-ts-mode)

(defcustom roc-ts-start-program "roc-start"
  "The path to the roc-start executable."
  :type 'file
  :group 'roc-ts)

;;;###autoload (put 'roc-ts-start-platforms-file 'risky-local-variable t)
(defcustom roc-ts-start-platforms-file "~/.roc-start/pf-data.rvn"
  "The file that includes a list of Roc platforms."
  :type 'file
  :risky t
  :group 'roc-ts)

;;;###autoload (put 'roc-ts-start-packages-file 'risky-local-variable t)
(defcustom roc-ts-start-packages-file "~/.roc-start/pkg-data.rvn"
  "The file that includes a list of Roc packages."
  :type 'file
  :risky t
  :group 'roc-ts)

;; TODO: actually parse the rvn
(defun roc-ts-start--read-file-repos (filename)
  "Parse the list of \"repo\"s from the given rvn FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let (ret)
      (while (re-search-forward (rx "repo:" (* whitespace) ?\" (group (+? (not ?\"))) ?\") nil t)
        (push (match-string 1) ret))
      ret)))

(defun roc-ts-start--platforms ()
  "Return the list of available platforms."
  (roc-ts-start--read-file-repos roc-ts-start-platforms-file))

(defun roc-ts-start--packages ()
  "Return the list of available packages."
  (roc-ts-start--read-file-repos roc-ts-start-packages-file))

(defun roc-ts-start--ensure-executable-present ()
  "Print an error if the roc-start executable can't be found."
  (when (not (executable-find roc-ts-start-program))
    (user-error "Error: roc-start executable not found. Please install roc-start (or customize `roc-ts-start-program' to tell me where it is)")))

(defun roc-ts-start--ensure-files-present (files)
  "Print an error if the given FILES can't be found."
  (dolist (file files)
    (when (not (file-exists-p file))
      (user-error "File %s doesn't exist. Please run `M-x roc-ts-start-update'" file))))

(defun roc-ts-start--run-cmd (subcmd &rest arguments)
  "Run the command \"roc-start SUBCMD ...ARGUMENTS\" synchronously.

Throws an error if it returns a non-zero exit status."
  (setq arguments (seq-mapcat #'ensure-list arguments))
  (with-temp-buffer
    (message "Running %s..." roc-ts-start-program)
    (let ((status (apply #'call-process
                         roc-ts-start-program nil (current-buffer) nil
                         subcmd arguments)))
      (funcall (if (eq status 0) #'message #'user-error)
               "%s"
               (buffer-substring (point-min) (point-max))))))

;;;###autoload
(defun roc-ts-start-app (file platform packages)
  "EXPERIMENTAL: Start a new Roc app using roc-start.

FILE is the name of the new app file.
PLATFORM is the name of the platform for the app.
PACKAGES is a list of Roc packages."
  (interactive
   (progn
     (roc-ts-start--ensure-executable-present)
     (roc-ts-start--ensure-files-present (list roc-ts-start-platforms-file roc-ts-start-packages-file))
     (list
      (expand-file-name (read-file-name "File name [main.roc]: " nil "main.roc" nil ".roc"))
      (completing-read "Choose a platform: " (roc-ts-start--platforms) nil t nil nil "basic-cli")
      (completing-read-multiple "Choose packages (optional): " (roc-ts-start--packages) nil t))))
  (let ((default-directory (file-name-directory file)))
    (roc-ts-start--run-cmd "app" (file-name-base file) platform packages))
  (find-file file))

;;;###autoload
(defun roc-ts-start-pkg (dir &optional packages)
  "EXPERIMENTAL: Start a new Roc package in the directory DIR.

PACKAGES is a list of Roc packages (dependencies of the new package)."
  (interactive
   (progn
     (roc-ts-start--ensure-executable-present)
     (roc-ts-start--ensure-files-present (list roc-ts-start-packages-file))
     (list
      (read-directory-name "Choose a directory for your package: ")
      (completing-read-multiple "Choose package dependencies (optional): " (roc-ts-start--packages) nil t))))
  (make-directory dir t)
  (let ((default-directory dir))
    (roc-ts-start--run-cmd "pkg" packages))
  (find-file (file-name-concat dir "main.roc")))

;;;###autoload
(defalias 'roc-ts-start-package #'roc-ts-start-pkg)

;;;###autoload
(defun roc-ts-start-update ()
  "EXPERIMENTAL: Fetch the latest packages, platforms, and app stubs."
  (interactive)
  (roc-ts-start--ensure-executable-present)
  (compile "roc-ts-start update"))

(provide 'roc-ts-start)
;;; roc-ts-start.el ends here
