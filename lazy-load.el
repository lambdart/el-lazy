;;; lazy-load.el --- Automatic generate package autoloads -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: Alpha 0.0.8
;; Homepage: https://github.com/lambdart/lazy
;; Keywords: autoloads load definitions
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
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

(require 'files)
(require 'timer)
(require 'cl-seq)
(require 'autoload)
(require 'filenotify)

(defgroup lazy-load nil
  "Autoloads generator."
  :group 'extensions
  :group 'convenience)

(defcustom lazy-load-files-alist
  (list
   ;; lisp directory
   (cons "lisp-loaddefs.el"
         (expand-file-name "lisp/" user-emacs-directory))
   ;; site-lisp directory
   (cons "site-lisp-loaddefs.el"
         (expand-file-name "site-lisp/" user-emacs-directory)))

  "Associative list of output file names (loaddefs) and \
respective source path (root) directory.

Each element (pair) has the form (OUTPUT-FILE-NAME DIRECTORY).

OUTPUT-FILE-NAME can be any string, it's recommended though to choose the file names
carefully, and that ends with '.el' (the convenient elisp file extension).
Remember this files will be latter used at your `init.el', e.g:

\(require 'site-lisp-loaddefs)

DIRECTORY, must be the full-path, see `expand-file-name'
- that will be monitored by `filenotify' and is the location where
the generated autoloads file will be saved."

  :type '(alist :key-type string :value-type string)
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-minor-mode-string (purecopy "lazy-load")
  "String to be displayed at mode-line."
  :type 'string
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-enable-filenotify-flag nil
  "Non-nil means starts to monitor the directories listed at `lazy-load-files-alist'.

When one of the directories is modified events will be emitted, so
`lazy-load-update-autoloads' will be invoked, i.e, if your add a new package
- download files to that directory - the necessary load definitions
will be created and the referent ('loaddefs') file updated automatically."

  :type 'boolean
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-kill-autoload-file-buffer-flag t
  "Non-nil means kill `generated-autoload-file' dynamic buffer file."
  :type 'boolean
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-debug-messages-flag nil
  "Non-nil means show debug messages."
  :type 'boolean
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-enable-run-idle-flag nil
  "Non-nil means run `lazy-load-update-autoloads' when Emacs is idle."
  :type 'boolean
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-idle-seconds 30
  "Idle timer value that will be used by `run-with-idle-timer'."
  :type 'integer
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-timer-interval 8
  "Timer interval in seconds, used to trigger the timer callback function."
  :type 'integer
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-message-prefix "[Lazy]: "
  "Lazy message prefix."
  :type 'string
  :group 'lazy-load
  :safe t)

(defvar lazy-load-dirs '()
  "Lazy internal: list of directories.")

(defvar lazy-load-file-descriptors '()
  "Lazy internal: list of file descriptors.")

(defvar lazy-load-internal-vars
  '(lazy-load-file-descriptors lazy-load-dirs)
  "Lazy internal: list of internal variables.")

(defvar lazy-load-idle-timer nil
  "Lazy internal: Idle system interface timer.")

(defvar lazy-load-timer nil
  "Lazy internal: Auxiliary system interface timer.")

(defvar lazy-load-mode nil
  "Non-nil means that lazy-load-mode is enabled.
Altering this variable directly has no effect.")

(defmacro lazy-load--debug-message (fmt &rest args)
  "Display a internal message at the bottom of the screen.
See `message' for more information about FMT and ARGS arguments."
  `(when lazy-load-debug-messages-flag
     (message (concat lazy-load-message-prefix ,fmt) ,@args)))

(defun lazy-load--file-notify-callback (event)
  "The `filenotify' related callback, called when a EVENT occur.

If timer was already initialized, restart it
this is used to avoid calling `lazy-load-update-autoloads'
after each modification on the target directories,
when you download a lot of packages for instance,
wait a little time (seconds) and then update the load definitions."

  (let ((descriptor (car event))
        (action (cadr event)))
    ;; set logs message
    (if (not (file-notify-valid-p descriptor))
        (lazy-load--debug-message "Error, invalid file descriptor")
      ;; look to this events:
      (when (or (eq action 'created)
                (eq action 'deleted)
                (eq action 'renamed))
        ;; cancel the timer, if necessary
        (when lazy-load-timer
          (cancel-timer lazy-load-timer)
          (lazy-load--debug-message "Timer stopped"))
        ;; start timer
        (setq lazy-load-timer
              (run-with-timer lazy-load-timer-interval nil 'lazy-load-update-autoloads))
        (lazy-load--debug-message "Timer started")))))

(defun lazy-load--add-file-notify-watch (dirs)
  "Add DIRS to the notifications system: `filenotofy'.
Push the returned file descriptor `lazy-load-file-descriptors' list."
  ;; iterate over the directories list
  (dolist (dir dirs)
    ;; add descriptor to the `lazy-load-file-descriptors' list
    (push
     ;; add a file-notify watcher
     (file-notify-add-watch dir
                            '(change attribute-change)
                            'lazy-load--file-notify-callback)
     ;; descriptors internal list
     lazy-load-file-descriptors)))

(defun lazy-load--rm-file-notify-watch ()
  "Remove `filenotify' watch using the saved descriptors.
See, `lazy-load-file-descriptors' variable to see the list of active
descriptors."
  (dolist (descriptor lazy-load-file-descriptors)
    (file-notify-rm-watch descriptor)))

(defun lazy-load--set-dirs-list ()
  "Set internal directories lists `lazy-load-dirs'.
Using as a source the custom `lazy-load-file-alist'.
This directories will be monitored using the filenotify library."
  (let ((size (length lazy-load-files-alist))
        (dir nil)
        (output-file nil))
    (dotimes (i size)
      ;; set (load definitions) output file name
      (setq output-file (car (nth i lazy-load-files-alist)))
      ;; set directory (expand again to avoid unexpected errors)
      (setq dir (expand-file-name
                 (cdr (assoc output-file lazy-load-files-alist))))
      ;; verify if the directory exists TODO: and its attributes
      (when (file-directory-p dir)
        ;; add (push) directory to directories list
        (push dir lazy-load-dirs)))))

(defun lazy-load--clean-internal-vars ()
  "Clean internal variables."
  (dolist (var lazy-load-internal-vars)
    (set var nil)))

(defun lazy-load--create-empty-file (file)
  "Create a empty file."
  (when (not (file-exists-p file))
    (make-empty-file file nil)))

(defun lazy-load--update-directory-autoloads (dirs output-file)
  "Make directory autoloads using obsolete `update-directory-autoloads'."
  (let ((generated-autoload-file output-file))
    ;; if file does not exist create it
    (lazy-load--create-empty-file generated-autoload-file)
    ;; apply update-packages-autoloads using all dirs
    (apply 'update-directory-autoloads dirs)))

(defun lazy-load-update-directory-autoloads (dir output-file)
  "Generate autoloads from a DIR and save in OUTPUT-FILE destination."
  ;; maps DIR OUTPUT-FILE parameters
  (interactive
   (let* ((dir (read-directory-name "Dir: " nil nil t))
          (output-file (read-file-name "File: " dir nil 'confirm)))
     (list dir output-file)))
  (let ((dirs (directory-files dir t "^[^.]"))
        (output-file (expand-file-name output-file dir)))
    ;; remove files that aren't directories
    (setq dirs (cl-remove-if-not #'file-directory-p dirs))
    ;; select the right update autoloads function
    (if (fboundp 'make-directory-autoloads)
        (make-directory-autoloads dirs output-file)
      ;; update directory autoloads using obsolete function
      (lazy-load--update-directory-autoloads dirs output-file))
    ;; delete generated autoload file buffer
    (when lazy-load-kill-autoload-file-buffer-flag
      (ignore-errors
        (kill-buffer (get-file-buffer output-file))))))

(defun lazy-load-update-autoloads ()
  "Generate autoloads from directories file defined in `lazy-load-files-alist'.
This function will iterate over the custom associative list
`lazy-load-files-alist' using its parameters to determinate
the resulting `loaddefs' file-name and location."
  (interactive)
  ;; get the file names and file directories
  (let ((size (length lazy-load-files-alist))
        (dir  nil)
        (output-file nil))
    ;; for each equivalent
    (dotimes (i size)
      ;; set auxiliary variables
      (setq output-file (car (nth i lazy-load-files-alist)))
      (setq dir (cdr (assoc output-file lazy-load-files-alist)))
      ;; update autoloads
      (lazy-load-update-directory-autoloads dir output-file))))

(defun lazy-load-run-idle-timer ()
  "Set lazy-load idle timer functionality."
  (interactive)
  (cond
   ;; verify if idle timer was already set
   ((not (eq lazy-load-idle-timer nil))
    (lazy-load--debug-message "run idle already on"))
   ;; default: add run-idle-timer
   (t
    ;; set the idle auxiliary timer
    (setq lazy-load-idle-timer
          (run-with-idle-timer lazy-load-idle-seconds t
                               'lazy-load-update-autoloads))
    ;; show debug message
    (lazy-load--debug-message "run idle on"))))

(defun lazy-load-cancel-idle-timer ()
  "Cancel `lazy-load-idle-timer'."
  (interactive)
  ;; remove time if was set
  (if (not lazy-load-idle-timer)
      (lazy-load--debug-message "run idle already off")
    ;; cancel timer
    (cancel-timer lazy-load-idle-timer)
    ;; clean timer variable
    (setq lazy-load-idle-timer nil)
    ;; debug message
    (lazy-load--debug-message "run idle off")))

(defun lazy-load-reload-idle-timer ()
  "Reload idle timer.
Invoke this function to apply the new value of `lazy-load-idle-timer.'"
  (interactive)
  ;; remove (cancel) previous timer
  (lazy-load-cancel-idle-timer)
  ;; set (add) idle timer
  (lazy-load-run-idle-timer)
  ;; show the current idle
  (lazy-load--debug-message "current idle time %ds"
                       lazy-load-idle-seconds))

(defun lazy-load-update-idle-time (time &optional arg)
  "Update `lazy-load-idle-seconds' interactively using TIME in seconds.
Reload the idle timer when optional argument (ARG) is used."
  ;; interactively function arguments mapping (TIME, ARG)
  (interactive
   (list (read-number "Time in seconds: ")
         current-prefix-arg))
  ;; update idle elapse time (in seconds)
  (setq lazy-load-idle-seconds time)
  ;; reload idle timer functionality
  (when arg (lazy-load-reload-idle-timer)))

(defun lazy-load-toggle-debug-messages (&optional arg)
  "Toggle `lazy-load-debug-messages-flag' bool value.
If optional ARG is non-nil, force the activation of debug messages."
  (interactive "P")
  ;; toggle logic
  (setq lazy-load-debug-messages-flag
        (or arg (not lazy-load-debug-messages-flag)))
  ;; display log message in echo area
  (message "[Lazy]: Debug messages: %s"
           (if lazy-load-debug-messages-flag "on" "off")))

(defun lazy-load-echo-mode-state ()
  "Show lazy-load minor mode state: on/off."
  (interactive)
  (message "[Lazy]: mode %s" (if lazy-load-mode "on" "off")))

;;;###autoload
(define-minor-mode lazy-load-mode
  "Define a new minor mode `lazy-load-mode'.

This defines the toggle command `lazy-load-mode' and (by default)
a control variable `lazy-load-mode'.

Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise."

  :group lazy-load
  :lighter lazy-load-minor-mode-string
  (cond
   (lazy-load-mode
    ;; set internal directories lists
    (lazy-load--set-dirs-list)
    ;; add file watchers
    (when lazy-load-enable-filenotify-flag
      (lazy-load--add-file-notify-watch lazy-load-dirs))
    ;; add idle timer
    (when lazy-load-enable-run-idle-flag
      (lazy-load-run-idle-timer))
    ;; set mode indicator: true
    (setq lazy-load-mode t))
   (t
    ;; remove file watchers
    (lazy-load--rm-file-notify-watch)
    ;; remove idle timer
    (lazy-load-cancel-idle-timer)
    ;; clean internal lists
    (lazy-load--clean-internal-vars)
    ;; set mode indicator: false (nil)
    (setq lazy-load-mode nil))))

;;;###autoload
(defun turn-on-lazy-load-mode ()
  "Enable lazy-load minor mode."
  (interactive)
  ;; turn on lazy-load mode
  (lazy-load-mode 1)
  ;; show lazy-load mode state: on/off
  (lazy-load-echo-mode-state))

(defun turn-off-lazy-load-mode ()
  "Disable lazy-load minor mode."
  (interactive)
  ;; turn off lazy-load mode
  (lazy-load-mode 0)
  ;; show lazy-load mode state
  (lazy-load-echo-mode-state))

(provide 'lazy-load)

;;; lazy-load.el ends here
