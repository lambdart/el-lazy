;;; lazy.el --- Automatic generate package autoloads -*- lexical-binding: t -*-
;;
;; URL: https://github.com/esac-io/lazy
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
;; Version: Alpha 0.0.4
;; Package-Requires: autoload filenotify cl-seq dired-aux timer
;; Keywords: autoloads load definitions
;;
;;; MIT License
;;
;; Copyright (c) 2020 esac
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;; Code:

(require 'cl-seq)
(require 'autoload)
(require 'filenotify)
(require 'dired-aux)
(require 'timer)

(defgroup lazy nil
  "Autoloads generator."
  :group 'extensions
  :group 'convenience)

(defcustom lazy-files-alist
  (list
   ;; lisp directory
   (cons "lisp-loaddefs.el"
         (expand-file-name "lisp/" user-emacs-directory))
   ;; site-lisp directory
   (cons "site-lisp-loaddefs.el"
         (expand-file-name "site-lisp/" user-emacs-directory)))

  "Associative list of file-names (loaddefs) destination and \
respective source path (root) directory.

Each element has the form (FILE-NAME DIRECTORY).

FILE-NAME can be any string, it's recommended though to choose the file-names
carefully, and that ends with '.el' (the convenient elisp file extension).
Remember this files will be latter used at your `init.el', e.g:

\(require 'site-lisp-loaddefs)

DIRECTORY, must be the full-path, see `expand-file-name'
- that will be monitored by `filenotify' and is the location where
the generated autoloads file will be saved."

  :type '(alist :key-type string :value-type string)
  :group 'lazy
  :safe t)

(defcustom lazy-minor-mode-string (purecopy "lazy")
  "String to be displayed at mode-line."
  :type 'string
  :group 'lazy
  :safe t)

(defcustom lazy-enable-filenotify-flag nil
  "Non-nil means starts to monitor the directories listed at `lazy-files-alist'.

When one of the directories is modified events will be emitted, so
`lazy-update-autoloads' will be invoked, i.e, if your add a new package
- download files to that directory - the necessary load definitions
will be created and the referent ('loaddefs') file updated automatically."

  :type 'boolean
  :group 'lazy
  :safe t)

(defcustom lazy-kill-autoload-file-buffer-flag t
  "Non-nil means kill `generated-autoload-file' dynamic buffer file."
  :type 'boolean
  :group 'lazy
  :safe t)

(defcustom lazy-debug-messages-flag nil
  "Non-nil means show debug messages."
  :type 'boolean
  :group 'lazy
  :safe t)

(defcustom lazy-run-idle-flag nil
  "Non-nil means run `lazy-update-autoloads' when Emacs is idle."
  :type 'boolean
  :group 'lazy
  :safe t)

(defcustom lazy-idle-seconds 4
  "Idle timer value that will be used by `run-with-idle-timer'."
  :type 'integer
  :group 'lazy
  :safe t)

(defcustom lazy-timer-interval 4
  "Timer interval in seconds, used to trigger the timer callback function."
  :type 'integer
  :group 'lazy
  :safe t)

(defcustom lazy-message-prefix "[Lazy]: "
  "Lazy message prefix."
  :type 'string
  :group 'lazy
  :safe t)

(defvar lazy-file-names '()
  "File-names list.")

(defvar lazy-file-directories '()
  "List of directories.")

(defvar lazy-file-descriptors '()
  "List of file descriptors.")

(defvar lazy-internal-vars
  '(lazy-file-descriptors
    lazy-file-directories)
  "List of internal variables.")

(defvar lazy-idle-timer nil
  "Idle system interface timer.")

(defvar lazy-timer nil
  "Overall system interface timer.")

(defvar lazy-mode nil
  "Non-nil means that lazy-mode is enabled.")

(defmacro lazy--message (fmt &rest args)
  "Display a internal message at the bottom of the screen.
See `message' for more information about FMT and ARGS arguments."
  `(when lazy-debug-messages-flag
     (message (concat lazy-message-prefix ,fmt) ,@args)))

(defun lazy--file-notify-callback (event)
  "The `filenotify' related callback, called when a EVENT occur.

If timer was already initialized, restart it
this is used to avoid calling `lazy-update-autoloads'
after each modification on the target directories,
when you download a lot of packages for instance,
wait a little time (seconds) and then update the load definitions."

  (let ((decriptor (car event))
        (action (cadr event)))
    ;; set logs message
    (if (not (file-notify-valid-p decriptor))
        (lazy--message "Error, invalid file descriptor")
      ;; look to this events:
      (when (or (eq action 'created)
                (eq action 'deleted)
                (eq action 'renamed))
        ;; cancel the timer, if necessary
        (when lazy-timer
          (cancel-timer lazy-timer)
          (lazy--message "Timer stopped"))
        ;; start timer
        (setq lazy-timer
              (run-with-timer lazy-timer-interval nil 'lazy-update-autoloads))
        (lazy--message "Timer started")))))

(defun lazy--add-file-notify-watch (dirs)
  "Add DIRS to the notifications system: `filenotofy'.
Push the returned file descriptor `lazy-file-descriptors' list."
  ;; iterate over the directories list
  (dolist (dir dirs)
    ;; add descriptor to the `lazy-file-descriptors' list
    (push
     ;; add a file-notify watcher
     (file-notify-add-watch dir
                            '(change attribute-change)
                            'lazy--file-notify-callback)
     ;; descriptors internal list

     lazy-file-descriptors)))

(defun lazy--rm-file-notify-watch ()
  "Remove `filenotify' watch using the saved descriptors.
See, `lazy-file-descriptors' variable to see the list of active
descriptors."
  (dolist (descriptor lazy-file-descriptors)
    (file-notify-rm-watch descriptor)))

(defun lazy--set-internal-lists ()
  "Set internal lists, `lazy-file-names' and `lazy-filename-directories'.
Using as a source the custom `lazy-file-alist'."
  (let ((size (length lazy-files-alist))
        (file-name nil))
    (when (> size 0)
      (dotimes (i size)
        (setq file-name (car (nth i lazy-files-alist)))
        ;; add (push) file-name to file-names list
        (push file-name lazy-file-names) ; push file
        ;; add (push) directory to directories list
        (push (cdr (assoc file-name lazy-files-alist))
              lazy-file-directories)))))

(defun lazy--clean-internal-lists ()
  "Clean internal lists."
  ;; clean internal variables
  (dolist (var lazy-internal-vars)
    (set var nil)))

;;;###autoload
(defun lazy-update-directory-autoloads (dir file)
  "Generate autoloads from a DIR and save in FILE destination."
  ;; can be called directly (interactively, more flexible)!
  ;; map function parameters, if necessary
  (interactive
   (let* ((dir (read-directory-name "Dir: " nil nil t))
          (file (read-file-name "File: " dir nil 'confirm)))
     (list dir file)))
  (let (;; TODO: find another way (nthcdr 2 is used to remove '.' and '..')
        (dirs (nthcdr 2 (directory-files dir t)))
        (generated-autoload-file (expand-file-name file dir)))
    ;; if file does not exist create it
    (when (not (file-exists-p generated-autoload-file))
      (dired-create-empty-file generated-autoload-file))
    ;; remove files that aren't directories
    (setq dirs (cl-remove-if-not #'file-directory-p dirs))
    ;; apply update-packages-autoloads using all dirs
    (apply 'update-directory-autoloads dirs)
    ;; delete generated-autoload-file buffer
    (when lazy-kill-autoload-file-buffer-flag
        (ignore-errors
          (kill-buffer (get-file-buffer generated-autoload-file))))))

;;;###autoload
(defun lazy-update-autoloads ()
  "Generate autoloads from directories file defined in `lazy-files-alist'.
This function will iterate over the custom associative list
`lazy-files-alist' using its parameters to determinate
the resulting `loaddefs' file-name and location."
  (interactive)
  ;; initialize lazy files list if necessary
  (when (eq lazy-files-alist '())
    (lazy--set-internal-lists))
  (let ((size (length lazy-files-alist))
        (file-name nil)
        (dir nil))
    (dotimes (i size)
      (setq file-name (car (nth i lazy-files-alist)))
      (setq dir (cdr (assoc file-name lazy-files-alist)))
      (lazy-update-directory-autoloads dir file-name))))

;;;###autoload
(defun lazy-add-idle-timer (&optional arg)
  "Add lazy idle timer functionality.
If ARG is non-nil force the activation, otherwise
verify `lazy-run-idle-flag' for this control."
  (interactive "P")
  ;; add run-idle-timer if prefix arg or lazy-run-idle-flag are true
  (when (or arg lazy-run-idle-flag)
    (setq lazy-idle-timer
          (run-with-idle-timer lazy-idle-seconds t
                               'lazy-update-autoloads))
    (lazy--message "run idle on")))

;;;###autoload
(defun lazy-rm-idle-timer ()
  "Cancel `lazy-idle-timer'."
  (interactive)
  (when lazy-idle-timer
    (cancel-timer lazy-idle-timer)
    (setq lazy-idle-timer nil)
    (lazy--message "run idle off")))

;;;###autoload
(defun lazy-toggle-debug-messages (&optional arg)
  "Toggle `lazy-debug-messages-flag' bool value.
If optional ARG is non-nil, force the activation of debug messages."
  (interactive "P")
  ;; toggle logic
  (setq lazy-debug-messages-flag
        (or arg (not lazy-debug-messages-flag)))
  ;; logs: show message at the bottom (echo area)
  (message "[Lazy]: Debug messages: %s"
           (if lazy-debug-messages-flag "on" "off")))

;;;###autoload
(defun lazy-mode-state ()
  "Show lazy minor mode state: on/off."
  (interactive)
  (message "[Lazy]: %s" (if lazy-mode "on" "off")))

;;;###autoload
(define-minor-mode lazy-mode
  "Define a new minor mode `lazy-mode'.

This defines the toggle command `lazy-mode' and (by default)
a control variable `lazy-mode'.

Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise."

  :group lazy
  :lighter lazy-minor-mode-string
  (cond
   (lazy-mode
    ;; set internal lists
    (lazy--set-internal-lists)
    ;; add file watchers
    (when lazy-enable-filenotify-flag
      (lazy--add-file-notify-watch lazy-file-directories))
    ;; add idle timer
    (lazy-add-idle-timer)
    ;; set mode indicator to true
    (setq lazy-mode t))
   (t
    ;; remove file watchers
    (lazy--rm-file-notify-watch)
    ;; remove idle timer
    (lazy-rm-idle-timer)
    ;; clean internal lists
    (lazy--clean-internal-lists)
    ;; set mode indicator to nil (false)
    (setq lazy-mode nil)))
  ;; default message: show its state (on/off)
  (lazy-mode-state))

;;;###autoload
(defun turn-on-lazy-mode ()
  "Enable lazy minor mode."
  (interactive)
  (unless lazy-mode (lazy-mode 1)))

;;;###autoload
(defun turn-off-lazy-mode ()
  "Disable lazy minor mode."
  (interactive)
  (when lazy-mode (lazy-mode 0)))

(provide 'lazy)
;;; lazy.el ends here
