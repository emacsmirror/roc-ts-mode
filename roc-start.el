;;; roc-start.el --- Description -*- lexical-binding: t; -*-
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

(defcustom roc-start-program "roc-start"
  "The path to the roc-start executable."
  :type 'file
  :group 'roc)

(defcustom roc-start-platforms-file "~/.roc-start/pf-data.rvn"
  "The file that includes a list of Roc platforms."
  :type 'file
  :group 'roc)

(defcustom roc-start-packages-file "~/.roc-start/pkg-data.rvn"
  "The file that includes a list of Roc packages."
  :type 'file
  :group 'roc)

(defcustom roc-start-update-skip-prompt nil
  "If non-nil, `roc-start-update' will not prompt."
  :type 'boolean
  :group 'roc)

;; TODO: actually parse the rvn
(defun roc-start--read-file-repos (filename)
  "Parse the list of \"repo\"s from the given rvn FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let (ret)
      (while (re-search-forward (rx "repo:" (* whitespace) ?\" (group (+? (not ?\"))) ?\") nil t)
        (push (match-string 1) ret))
      ret)))

(defun roc-start--platforms ()
  "Return the list of available platforms."
  (roc-start--read-file-repos roc-start-platforms-file))

(defun roc-start--packages ()
  "Return the list of available packages."
  (roc-start--read-file-repos roc-start-packages-file))

(defun roc-start--ensure-executable-present ()
  "Print an error if the roc-start executable can't be found."
  (when (not (executable-find roc-start-program))
    (user-error "Error: roc-start executable not found. Please install roc-start (or customize `roc-start-program' to tell me where it is)")))

(defun roc-start--ensure-files-present (files)
  "Print an error if the given FILES can't be found."
  (dolist (file files)
    (when (not (file-exists-p file))
      (user-error "File %s doesn't exist. Please run `M-x roc-start-fetch'" file))))

(defun roc-start--run-cmd (subcmd &rest arguments)
  "Run the command \"roc-start SUBCMD ...ARGUMENTS\" synchronously.

Throws an error if it returns a non-zero exit status."
  (setq arguments (seq-mapcat #'ensure-list arguments))
  (with-temp-buffer
    (message "Running %s..." roc-start-program)
    (let ((status (apply #'call-process
                         roc-start-program nil (current-buffer) nil
                         subcmd arguments)))
      (funcall (if (eq status 0) #'message #'user-error)
               "%s"
               (buffer-substring (point-min) (point-max))))))

;;;###autoload
(defun roc-start-app (file platform packages)
  "EXPERIMENTAL: Start a new Roc app using roc-start.

FILE is the name of the new app file.
PLATFORM is the name of the platform for the app.
PACKAGES is a list of Roc packages."
  (interactive
   (progn
     (roc-start--ensure-executable-present)
     (roc-start--ensure-files-present (list roc-start-platforms-file roc-start-packages-file))
     (list
      (expand-file-name (read-file-name "File name [main.roc]: " nil "main.roc" nil ".roc"))
      (completing-read "Choose a platform: " (roc-start--platforms) nil t nil nil "basic-cli")
      (completing-read-multiple "Choose packages (optional): " (roc-start--packages) nil t))))
  (let ((default-directory (file-name-directory file)))
    (roc-start--run-cmd "app" (file-name-base file) platform packages))
  (find-file file))

;;;###autoload
(defun roc-start-pkg (dir &optional packages)
  "EXPERIMENTAL: Start a new Roc package in the directory DIR.

PACKAGES is a list of Roc packages (dependencies of the new package)."
  (interactive
   (progn
     (roc-start--ensure-executable-present)
     (roc-start--ensure-files-present (list roc-start-packages-file))
     (list
      (read-directory-name "Choose a directory for your package: ")
      (completing-read-multiple "Choose package dependencies (optional): " (roc-start--packages) nil t))))
  (make-directory dir t)
  (let ((default-directory dir))
    (roc-start--run-cmd "pkg" packages))
  (find-file (file-name-concat dir "main.roc")))

;;;###autoload
(defalias 'roc-start-package #'roc-start-pkg)

;;;###autoload
(defun roc-start-fetch ()
  "EXPERIMENTAL: Fetch the latest packages and platforms."
  (interactive)
  (roc-start--ensure-executable-present)
  (compile "roc-start update --packages --platforms"))

;;;###autoload
(defun roc-start-update ()
  "EXPERIMENTAL: Update all dependencies (platforms/packages) of the current dir.

Also see `roc-start-update-skip-prompt'."
  (interactive "p")
  (roc-start--ensure-executable-present)
  (when (or roc-start-update-skip-prompt (y-or-n-p "Update all dependencies? "))
    (compile "roc-start update")))

(provide 'roc-start)
;;; roc-start.el ends here
