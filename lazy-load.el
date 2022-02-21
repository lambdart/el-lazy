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

  "Associative list of (loaddefs-file-name . full-path-directory).

Each element (pair) has the form (OUTPUT-FILE-NAME DIRECTORY).

OUTPUT-FILE-NAME can be any string, it's recommended though to choose
the file names carefully, and that ends with '.el'
\(the convenient elisp file extension). Remember this files will
be latter used at your `init.el', e.g:

\(require 'site-lisp-loaddefs)

DIRECTORY, must be the full-path, see `expand-file-name'
- that will be monitored by `filenotify' and is the location
where the generated autoloads file will be saved."

  :type '(alist :key-type string :value-type string)
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-minor-mode-string (purecopy "lazy-load")
  "String to be displayed at mode-line."
  :type 'string
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-enable-filenotify-flag nil
  "Non-nil means starts to monitor the target directories.
Target directories are listed in `lazy-load-files-alist' variable.
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

(defcustom lazy-load-timer-interval 30
  "Timer interval in seconds, used to trigger the timer callback function."
  :type 'integer
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-message-prefix "[LAZY-LOAD]: "
  "Lazy message prefix."
  :type 'string
  :group 'lazy-load
  :safe t)

(defcustom lazy-load-mode-hook nil
  "Hooks to un after the mode was turned on."
  :group 'lazy-load
  :type 'hook)

(defvar lazy-load-ignore-loaddefs-regex ".+-loaddefs.el$"
  "Lazy internal: ignore load definitions files regex.")

(defvar lazy-load-file-descriptors '()
  "Lazy internal: list of file descriptors.")

(defvar lazy-load-watched-dirs '()
  "Directories that are being watched.")

(defvar lazy-load-internal-vars
  '(lazy-load-file-descriptors
    lazy-load-watched-dirs)
  "Lazy internal: list of internal variables.")

(defvar lazy-load-idle-timer nil
  "Lazy internal: Idle system interface timer.")

(defvar lazy-load-timer nil
  "Lazy internal: Auxiliary system interface timer.")

(defvar lazy-load-mode nil
  "Non-nil means that Lazy-Load is enabled.
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

  (let ((descriptor (car event)))
    ;; verify if the descriptor is a valid one
    (if (not (file-notify-valid-p descriptor))
        (lazy-load--debug-message "invalid file descriptor"))
    (let ((action (cadr event))
          (file (caddr event)))
      ;; monitor this events:
      (when (or (eq action 'created)
                (eq action 'deleted)
                (eq action 'renamed))
        ;; cancel the timer, if necessary
        (and lazy-load-timer
             (cancel-timer lazy-load-timer))
        ;; ignore created loaddefs
        (unless (string-match-p lazy-load-ignore-loaddefs-regex file)
          (lazy-load--debug-message "timer started")
          ;; start timer
          (setq lazy-load-timer
                (run-with-timer lazy-load-timer-interval
                                nil
                                'lazy-load-update-autoloads)))))))

(defun lazy-load--add-file-notify-watch (dirs)
  "Add DIRS to the notifications system: `filenotofy'.
Push the returned file descriptor `lazy-load-file-descriptors' list."
  ;; iterate over the directories list
  (mapc (lambda (dir)
          ;; add descriptor to the `lazy-load-file-descriptors' list
          (push (file-notify-add-watch dir
                                       '(change attribute-change)
                                       'lazy-load--file-notify-callback)
                ;; descriptors internal list
                lazy-load-file-descriptors))
        dirs))

(defun lazy-load--rm-file-notify-watch ()
  "Remove `filenotify' watch using the saved descriptors.
See, `lazy-load-file-descriptors' variable to see the list of active
descriptors."
  (mapc (lambda (d)
          (file-notify-rm-watch d))
        lazy-load-file-descriptors)
  ;; clean file descriptors list
  (setq lazy-load-file-descriptors nil))

(defun lazy-load--set-dirs-list ()
  "Set internal directories lists `lazy-load-watched-dirs'.
Using as a source the custom `lazy-load-file-alist'.
This directories will be monitored using the filenotify library."
  (mapc (lambda (x)
          (let ((dir (expand-file-name (cdr x))))
            (when (not (member dir lazy-load-watched-dirs))
              (push dir lazy-load-watched-dirs))))
        lazy-load-files-alist))

(defun lazy-load--clean-internal-vars ()
  "Clean internal variables."
  (mapc (lambda (v)
          (set v nil))
        lazy-load-internal-vars))

(defun lazy-load--create-empty-file (file)
  "Create a empty FILE if doesn't exists."
  (or (file-exists-p file)
      (make-empty-file file nil)))

(defun lazy-load--update-directory-autoloads (dirs output-file)
  "Generate autoload-file using OUTPUT-FILE from DIRS.
Uses the outdated `update-directory-autoloads' function."
  (let ((generated-autoload-file output-file))
    (apply 'update-directory-autoloads dirs)))

(defun lazy-load--read-args ()
  "Read DIR and output file through `minibuffer' facilities."
  (let ((dir (read-directory-name "Dir: " nil nil t)))
    (list dir (read-file-name "File: " dir nil 'confirm))))

(defun lazy-load-update-directories-autoloads (dir output-file)
  "Generate autoloads from DIR and save it in OUTPUT-FILE."
  (interactive (lazy-load--read-args))
  ;; if file does not exist create it
  (lazy-load--create-empty-file output-file)
  ;; use make-directory-autoloads function if available
  (funcall (if (fboundp 'make-directory-autoloads)
               'make-directory-autoloads
             'lazy-load--update-directory-autoloads)
           ;; directories list
           (cl-remove-if-not #'file-directory-p
                             (directory-files dir t "^[^.]"))
           output-file)
  ;; delete generated autoload file buffer
  (when lazy-load-kill-autoload-file-buffer-flag
    (save-excursion
      (let ((buffer (get-file-buffer output-file)))
        (and buffer (kill-buffer buffer))))))

(defun lazy-load-loaddefs ()
  "Reload files defined in `lazy-load-files-alist'."
  (interactive)
  (mapc (lambda (e)
          (load-file
           (expand-file-name
            (concat (cdr e)
                    (car e)))))
        lazy-load-files-alist)
  ;; debug message
  (lazy-load--debug-message "Loaddefs reloaded!"))

;;;###autoload
(defun lazy-load-update-autoloads ()
  "Generate autoloads from directories file defined in `lazy-load-files-alist'.
This function will iterate over the custom associative list
`lazy-load-files-alist' using its parameters to determinate
the resulting `loaddefs' file-name and location."
  (interactive)
  (mapc (lambda (x)
          (lazy-load-update-directories-autoloads
           (cdr x) ; target directory
           (expand-file-name
            (concat (cdr x)
                    (car x))))) ; expanded output file
        lazy-load-files-alist)
  ;; reload load definitions
  (lazy-load-loaddefs))

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
  (cond
   ;; if not set, just show debug message and leave
   ((not lazy-load-idle-timer)
    (lazy-load--debug-message "run idle already off"))
   ;; else (default)
   (t
    ;; cancel timer
    (cancel-timer lazy-load-idle-timer)
    ;; clean timer variable
    (setq lazy-load-idle-timer nil)
    ;; debug message
    (lazy-load--debug-message "run idle off"))))

(defun lazy-load-reload-idle-timer ()
  "Reload idle timer.
Invoke this function to apply the new value of `lazy-load-idle-timer.'"
  (interactive)
  ;; remove (cancel) previous timer
  (lazy-load-cancel-idle-timer)
  ;; set (add) idle timer
  (lazy-load-run-idle-timer)
  ;; show the current idle
  (lazy-load--debug-message "current idle time %ds" lazy-load-idle-seconds))

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

(defun lazy-load-show-mode-state ()
  "Show lazy-load minor mode state: on/off."
  (interactive)
  (message "[Lazy]: mode %s" (if lazy-load-mode "on" "off")))

;;;###autoload
(define-minor-mode lazy-load-mode
  "Define a new minor mode `lazy-load-mode'.

This defines the toggle command \[lazy-load-mode] and (by default)
a control variable `lazy-load-mode'.

Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise."

  :global t
  :group 'lazy-load
  :lighter lazy-load-minor-mode-string
  (cond
   (lazy-load-mode
    ;; set internal directories lists
    (lazy-load--set-dirs-list)
    ;; maybe add file watchers
    (and lazy-load-enable-filenotify-flag
         (lazy-load--add-file-notify-watch lazy-load-watched-dirs))
    ;; maybe add idle timer
    (and lazy-load-enable-run-idle-flag (lazy-load-run-idle-timer))
    ;; run hooks
    (run-hooks lazy-load-mode-hook)
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
    (setq lazy-load-mode nil)))
  ;; show mode state
  (lazy-load-show-mode-state))

;;;###autoload
(defun turn-on-lazy-load-mode ()
  "Enable lazy-load minor mode."
  (interactive)
  ;; turn on lazy-load mode
  (or lazy-load-mode (lazy-load-mode 1)))

(defun turn-off-lazy-load-mode ()
  "Disable lazy-load minor mode."
  (interactive)
  ;; turn off lazy-load mode
  (and lazy-load-mode (lazy-load-mode 0)))

(provide 'lazy-load)

;;; lazy-load.el ends here
