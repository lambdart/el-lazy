;;; lazy.el --- summary -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
;; Version: 0.0.1
;; Package-Requires:
;; Keywords: autoload
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
;;
;;; Code:

(require 'cl-seq)
(require 'autoload)
(require 'filenotify)

(defcustom lazy-files-alist
  '((cons "lisp"      (expand-file-name "lisp/" user-emacs-directory))
    (cons "site-lisp" (expand-file-name "site-lisp/" user-emacs-directory)))
  "Each element is a list of the form (FILE-NAME DIRECTORY).

FILE-NAME can be any string, it's recommended though to choose the file-names
carefully, and that ends with '.el' (the convenient elisp file extension).
Remember this files will be latter used at your `init.el', e.g:

\(require 'site-lisp-loaddefs)

DIRECTORY, must be the full-path, see `expand-file-name'
- that will be monitored by `filenotify' and is the location where
the generated autoloads file will be saved."

  :type '(alist :key-txype string :value-type string)
  :safe t)

(defcustom lazy-enable-filenotify-flag t
  "Non-nil means start to monitor the directories listed at `lazy-files-alist'.

When one of the directories is modified events will be emitted, so
`lazy-update-autoloads' will be invoked, i.e, if your add a new package
- download files to that directory - the necessary load definitions
will be created and the autoloads file updated automatically."

  :type 'bool
  :safe t)

(defcustom lazy-debug-message-flag nil
  "Non-nil means show debug messages."
  :type 'bool
  :sate t)

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
    ;; remove files that aren't directories
    (setq dirs (cl-remove-if-not #'file-directory-p dirs))
    ;; apply update-packages-autoloads using all dirs
    (apply 'update-directory-autoloads dirs)))

;;;###autoload
(defun lazy-update-autoloads ()
  "Generate autoloads from directories file defined in `lazy-files-alist'.

This function will iterate over the custom associative list using its
parameters to determinate the resulting `loaddefs' file-name and
location, create the target file if doesn't exists, otherwise just update
its contents (autoload definitions), see `autoload' for more information."

  (interactive)
  (let ((dirs lazy-directories)
        (dir nil)
        (file nil))
    ;; update lisp autoloads
    (dolist (dirname dirs)
      (setq dir (expand-file-name dirname user-emacs-directory))
      (setq file (format "%s/%s-loaddefs.el" dir dirname))
      (when (not (file-exists-p file))
        (dired-create-empty-file file))
      (lazy-update-directory-autoloads dir file))))

;; TODO: function
(defun lazy--filenotify-callback (event)
  "The `filenotify' related callback, called when a EVENT occur."
  (message "Event %S" event))

;; TODO: function register directories to filenotify
(defun lazy--add-files-notification (dirs)
  "Add DIRS to the notifications system: `filenotofy'."
  (when lazy-enable-filenotify-flag
    (dolist (dir dirs)
      (file-notify-add-watch
       dir '(change attribute-change)
       'lazy--filenotify-callback)))

;; TODO: Define
(defun turn-on-lazy-mode()
  "Turn lazy-mode to on."
  (interactive)
  (lazy-mode 1))

(defun turn-off-lazy-mode()
  "Turn lazy-mode to off."
  (lazy-mode 0))

(provide 'lazy)
;;; lazy.el ends here
