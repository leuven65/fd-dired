;;; fd-dired.el --- fd dired tool. -*- lexical-binding: t -*-

;; Author: Jian Wang <leuven65@gmail.com>
;; URL: https://github.com/leuven65/fd-dired
;; Version: 0.1.0
;; Keywords: fd, dired

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(defvar fd-dired-args-history nil
  "history of fd dired command")

(defun fd-dired-fd-verison ()
  (string-trim (shell-command-to-string "fd -V") "fd "))

(defvar fd-dired-command-template
  (if (eq system-type 'windows-nt)
      ;; windows
      (if (version< (fd-dired-fd-verison) "8.0")
          "fd -x ls -ld \\; --full-path --path-separator '//' %s &"
        "fd -l --full-path --path-separator '//' %s &")
    ;; linux
    (if (version< (fd-dired-fd-verison) "8.0")
        ;; -b: print C-style escapes for nongraphic characters
        "fd -x ls -ldb \\; --full-path %s &"
      "fd -l --full-path %s &")))

;; just a hack on the original function `find-dired'
;; it is not good idea to rewrite the function, so hacking is good idea here
;; Maybe I am the first person to hack this function for `fd' ^-^.
(defun fd-dired-hacked-find-dired (dir my-cmd-args)
  (let ((default-directory dir))
    ;; define advice on the shell-command to replace the command
    (define-advice shell-command (:around (orig-fun find-cmd-args &rest r) my-cmd)
      (advice-remove 'shell-command #'shell-command@my-cmd)
      (message "shell-command@my-cmd: %s" my-cmd-args)
      (cl-assert (string-match-p "^find" find-cmd-args))
      ;; call origin function with my command
      (apply orig-fun my-cmd-args r))

    ;; call function
    ;; the above advice will be called
    (find-dired dir my-cmd-args)

    ;; replace the 2nd line by my command
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (let ((buffer-read-only nil))
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (delete-region beg end)
          (insert "  " my-cmd-args)
          (cl-assert (> end (point)))
          ;; insert space to keep this line length is same as before
          ;; as find-dired passed the position to process-filter
          (insert (make-string (- end (point)) ?\s))
          )
        )
      )

    ;; set refresh function
    (when (local-variable-p 'revert-buffer-function)
      (setq revert-buffer-function
            `(lambda (ignore-auto noconfirm)
               (fd-dired-hacked-find-dired ,dir ,my-cmd-args)))
      )
    ))

(defun fd-dired-read-fd-args ()
  (read-string "Run fd (with args): "
               (let ((ss (thing-at-point 'sexp)))
                 (when ss (format "%s" ss)))
               'fd-dired-args-history))

(defun fd-dired-find-dired (dir args)
  (interactive (list (read-directory-name "Run fd in directory: " nil "" t)
                     (fd-dired-read-fd-args)))
  (let ((fd-cmd-args (format fd-dired-command-template args)))
    (fd-dired-hacked-find-dired dir fd-cmd-args)
    ))

;;;###autoload
(defun fd-dired (dir args)
  "Like find-dired, run command `fd' and put output to dired mode."
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Run fd in directory: " nil "" t)
                       default-directory)
                     (fd-dired-read-fd-args)))
  (fd-dired-find-dired dir args))

;; (defalias 'fd 'fd-dired)

;; support fd in eshell
(with-eval-after-load 'esh-mode
  (defun eshell/fd (&rest args)
    "Use fd in eshell"
    (let ((sh-cmd (mapconcat #'shell-quote-argument args " ")))
      ;; (message "fd: %s" sh-cmd)
      (eshell-printn (format fd-dired-command-template sh-cmd))
      (fd-dired-find-dired default-directory sh-cmd))
    )
  )

;; add it to action of helm
(with-eval-after-load "helm-files"
  (defun fd-dired-helm-find-file-action (file-path)
    (fd-dired-find-dired (if (file-directory-p file-path)
                             file-path
                           (file-name-directory file-path))
                         (fd-dired-read-fd-args)))
  (add-to-list 'helm-find-files-actions
               '("Find dired by fd" . fd-dired-helm-find-file-action)
               t)
  )

(provide 'fd-dired)

;;; fd-dired.el ends here
