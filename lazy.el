;;; lazy.el --- Automatic generate package autoloads -*- lexical-binding: t -*-
;;
;; URL: https://github.com/esac-io/lazy
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
;; Version: Alpha 0.0.7
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

(require 'files)
(require 'timer)
(require 'cl-seq)
(require 'autoload)
(require 'filenotify)

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

(defcustom lazy-enable-run-idle-flag nil
  "Non-nil means run `lazy-update-autoloads' when Emacs is idle."
  :type 'boolean
  :group 'lazy
  :safe t)

(defcustom lazy-idle-seconds 30
  "Idle timer value that will be used by `run-with-idle-timer'."
  :type 'integer
  :group 'lazy
  :safe t)

(defcustom lazy-timer-interval 8
  "Timer interval in seconds, used to trigger the timer callback function."
  :type 'integer
  :group 'lazy
  :safe t)

(defcustom lazy-message-prefix "[Lazy]: "
  "Lazy message prefix."
  :type 'string
  :group 'lazy
  :safe t)

(defvar lazy-dirs '()
  "Lazy internal: list of directories.")

(defvar lazy-file-descriptors '()
  "Lazy internal: list of file descriptors.")

(defvar lazy-internal-vars
  '(lazy-file-descriptors lazy-dirs)
  "Lazy internal: list of internal variables.")

(defvar lazy-idle-timer nil
  "Lazy internal: Idle system interface timer.")

(defvar lazy-timer nil
  "Lazy internal: Auxiliary system interface timer.")

(defvar lazy-mode nil
  "Non-nil means that lazy-mode is enabled.")

(defmacro lazy--debug-message (fmt &rest args)
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

  (let ((descriptor (car event))
        (action (cadr event)))
    ;; set logs message
    (if (not (file-notify-valid-p descriptor))
        (lazy--debug-message "Error, invalid file descriptor")
      ;; look to this events:
      (when (or (eq action 'created)
                (eq action 'deleted)
                (eq action 'renamed))
        ;; cancel the timer, if necessary
        (when lazy-timer
          (cancel-timer lazy-timer)
          (lazy--debug-message "Timer stopped"))
        ;; start timer
        (setq lazy-timer
              (run-with-timer lazy-timer-interval nil 'lazy-update-autoloads))
        (lazy--debug-message "Timer started")))))

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

(defun lazy--set-dirs-list ()
  "Set internal directories lists `lazy-dirs'.
Using as a source the custom `lazy-file-alist'.
This directories will be monitored using the filenotify library."
  (let ((size (length lazy-files-alist))
        (dir nil)
        (output-file nil))
    (dotimes (i size)
      ;; set (load definitions) output file name
      (setq output-file (car (nth i lazy-files-alist)))
      ;; set directory (expand again to avoid unexpected errors)
      (setq dir (expand-file-name
                 (cdr (assoc output-file lazy-files-alist))))
      ;; verify if the directory exists TODO: and its attributes
      (when (file-directory-p dir)
        ;; add (push) directory to directories list
        (push dir lazy-dirs)))))

(defun lazy--clean-internal-lists ()
  "Clean internal lists."
  ;; clean internal variables
  (dolist (var lazy-internal-vars)
    (set var nil)))

(defun lazy--create-empty-file (file)
  "Create a empty file."
  (when (not (file-exists-p file))
    (make-empty-file file nil)))

(defun lazy--update-directory-autoloads (dirs output-file)
  "Make directory autoloads using obsolete `update-directory-autoloads'."
  (let ((generated-autoload-file output-file))
    ;; if file does not exist create it
    (lazy--create-empty-file generated-autoload-file)
    ;; apply update-packages-autoloads using all dirs
    (apply 'update-directory-autoloads dirs)))

(defun lazy-update-directory-autoloads (dir output-file)
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
      (lazy--update-directory-autoloads dirs output-file))
    ;; delete generated autoload file buffer
    (when lazy-kill-autoload-file-buffer-flag
      (ignore-errors
        (kill-buffer (get-file-buffer output-file))))))

(defun lazy-update-autoloads ()
  "Generate autoloads from directories file defined in `lazy-files-alist'.
This function will iterate over the custom associative list
`lazy-files-alist' using its parameters to determinate
the resulting `loaddefs' file-name and location."
  (interactive)
  ;; get the file names and file directories
  (let ((size (length lazy-files-alist))
        (dir  nil)
        (output-file nil))
    ;; for each equivalent
    (dotimes (i size)
      ;; set auxiliary variables
      (setq output-file (car (nth i lazy-files-alist)))
      (setq dir (cdr (assoc output-file lazy-files-alist)))
      ;; update autoloads
      (lazy-update-directory-autoloads dir output-file))))

(defun lazy-add-idle-timer ()
  "Set lazy idle timer functionality."
  (interactive)
  (cond
   ;; verify if idle timer was already set
   ((not (eq lazy-idle-timer nil))
    (lazy--debug-message "run idle already on"))
   ;; default: add run-idle-timer
   (t
    ;; set the idle auxiliary timer
    (setq lazy-idle-timer
          (run-with-idle-timer lazy-idle-seconds t
                               'lazy-update-autoloads))
    ;; show debug message
    (lazy--debug-message "run idle on"))))

(defun lazy-rm-idle-timer ()
  "Cancel `lazy-idle-timer'."
  (interactive)
  ;; remove time if was set
  (if (not lazy-idle-timer)
      (lazy--debug-message "run idle already off")
    ;; cancel timer
    (cancel-timer lazy-idle-timer)
    ;; clean timer variable
    (setq lazy-idle-timer nil)
    ;; debug message
    (lazy--debug-message "run idle off")))

(defun lazy-reload-idle-timer ()
  "Reload idle timer.
Invoke this function to apply the new value of `lazy-idle-timer.'"
  (interactive)
  ;; remove (cancel) previous timer
  (lazy-rm-idle-timer)
  ;; set (add) idle timer
  (lazy-add-idle-timer)
  ;; show the current idle
  (lazy--debug-message "current idle time %ds"
                       lazy-idle-seconds))

(defun lazy-update-idle-time (time &optional arg)
  "Update `lazy-idle-seconds' interactively using TIME in seconds.
Reload the idle timer when optional argument (ARG) is used."
  ;; interactively function arguments mapping
  ;; time in seconds
  ;; arg boolean flag
  (interactive
   (list (read-number "Time in seconds: ")
         current-prefix-arg))
  ;; update idle elapse time (in seconds)
  (setq lazy-idle-seconds time)
  ;; reload idle timer functionality
  (when arg (lazy-reload-idle-timer)))

(defun lazy-toggle-debug-messages (&optional arg)
  "Toggle `lazy-debug-messages-flag' bool value.
If optional ARG is non-nil, force the activation of debug messages."
  (interactive "P")
  ;; toggle logic
  (setq lazy-debug-messages-flag
        (or arg (not lazy-debug-messages-flag)))
  ;; display log message in echo area
  (message "[Lazy]: Debug messages: %s"
           (if lazy-debug-messages-flag "on" "off")))

(defun lazy-show-mode-state ()
  "Show lazy minor mode state: on/off."
  (interactive)
  ;; show lazy mode state in echo area
  (message "[Lazy]: mode %s"
           (if lazy-mode "on" "off")))

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
    ;; set internal directories lists
    (lazy--set-dirs-list)
    ;; add file watchers
    (when lazy-enable-filenotify-flag
      (lazy--add-file-notify-watch lazy-dirs))
    ;; add idle timer
    (when lazy-enable-run-idle-flag
      (lazy-add-idle-timer))
    ;; set mode indicator: true
    (setq lazy-mode t))
   (t
    ;; remove file watchers
    (lazy--rm-file-notify-watch)
    ;; remove idle timer
    (lazy-rm-idle-timer)
    ;; clean internal lists
    (lazy--clean-internal-lists)
    ;; set mode indicator: false (nil)
    (setq lazy-mode nil))))

;;;###autoload
(defun turn-on-lazy-mode ()
  "Enable lazy minor mode."
  (interactive)
  ;; turn on lazy mode
  (lazy-mode 1)
  ;; show lazy mode state: on/off
  (lazy-show-mode-state))

(defun turn-off-lazy-mode ()
  "Disable lazy minor mode."
  (interactive)
  ;; turn off lazy mode
  (lazy-mode 0)
  ;; show lazy mode state
  (lazy-show-mode-state))

(provide 'lazy)
;;; lazy.el ends here
