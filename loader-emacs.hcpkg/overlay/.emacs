; Because we update Info-directory-* below
(require 'info)

;;;
;;; Customization
;;;

(defconst homectl-dir (concat (getenv "HOME") "/.homectl")
  "The location of the homectl directory.  You should not change
  this unless you're a homectl developer and you change it in all
  the other places where it's hard-coded. :)")

(defconst homectl-bin (concat homectl-dir "/common/bin/hc"))

(defgroup homectl nil "Homectl configuration options" :tag "Homectl")

(defcustom homectl-darwin-fixup-path t
  "By default, homectl will try to set the PATH in Emacs itself,
because when Emacs is run through Aqua, it won't pick up the PATH
as set in the shell (via /etc/paths.d or via homectl).  However,
this might break other customizations you have in place, so you
can disable it here.

Note this ONLY applies on Darwin/OSX, so you don't need to worry
about this behvior on Linux."
  :type '(boolean) :group 'homectl :tag "Fix-up $PATH (on Darwin only)")



;;;
;;; Support functions for homectl packages
;;;

(defmacro homectl-foreach-enabled (path-var &rest body)
  "Loop through each enabled homectl package, placing its path in /path-var/."
  `(dolist (,path-var (process-lines homectl-bin "list"))
     (when (null (string-match-p "^\\." (file-name-nondirectory ,path-var)))
       ,@body)))

(defun homectl-require (repo-name repo-url pkg-symbol)
  "Loads a package, optionally downloading it from the specified package
repository if it is not already available on the system."

  (cond
   ; It's already available locally
   ((require pkg-symbol nil t)
    pkg-symbol)

   ; See if package.el knows about it
   (t
    (homectl-require-package-el)
    (add-to-list 'package-archives (cons repo-name repo-url))

    ; Try loading it NOW...
    (unless (require pkg-symbol nil t)
      ; Must fetch it from the internet
      (unless (y-or-n-p (concat "[homectl] Install " (symbol-name pkg-symbol)
                                " from " repo-url "?"))
        ; User aborted package install
        (error (concat "[homectl] Couldn't install " (symbol-name pkg-symbol)
                       " from " repo-url)))

      (package-refresh-contents)
      (package-install pkg-symbol)
      (require pkg-symbol)))))



;;;
;;; package.el support
;;;

(defvar homectl-package-el-ready-p nil
  "Have we already loaded/initialized package.el?")

(defun homectl-require-package-el ()
  (when (not homectl-package-el-ready-p)
    (require 'package)
    (package-initialize)
    (setq homectl-package-el-ready-p t)))



;;;
;;; Startup-related utility functions
;;;

(defun homectl-enable-pkg (pkg-path)
  "Enable a single homectl package, located at /pkg-path/."

  (let ((start-file (concat pkg-path "/" "emacs.el")))

    ; Load the package's startup file
    (when (file-exists-p start-file)
      (load-file start-file))))

(defun homectl-update-env (var hook)
  (let
      ((new-path (process-lines homectl-bin "path" "-n" hook var)))
        (setenv var (mapconcat #'identity new-path path-separator))
        (message (concat "[homectl] Updating " var ": " (getenv var)))))



;;;
;;; Main entry point
;;;

(defun homectl-startup ()
  "Load all enabled homectl packages with Emacs customizations,
and make sure the package's binaries are available in Emacs's
environment."
  (interactive)

  ; The following is a Darwin-specific hack to make sure the shell's $PATH
  ; is picked up by Emacs when running in Aqua mode.
  (when (and (string= system-type "darwin") homectl-darwin-fixup-path)
    (with-temp-buffer
    ; collect /etc/paths.d/* and /etc/paths in a buffer
    (dolist (f (directory-files "/etc/paths.d" t "[^\\.]"))
      (insert-file-contents f))
    (insert-file-contents "/etc/paths")

    ; s/\n+/:/g
    (goto-char (point-min))
    (while (re-search-forward "\n+" nil t)
      (replace-match ":"))
    ; strip trailing :
    (re-search-backward ":+")
    (replace-match "")

    (message (concat "[homectl] Fixing up PATH: " (buffer-string)))
    (setenv "PATH" (buffer-string))

    ; Appending here to preserve Emacs-internal paths
    (setq exec-path (append (split-string (getenv "PATH") ":" t) exec-path))
    (delete-dups exec-path)))

  ; Find all the enabled homectl packages and load them/add them to Emacs's
  ; paths.
  (homectl-update-env "PATH" "bin")
  (cond
   ((string= system-type "darwin")
    (homectl-update-env "DYLD_FRAMEWORK_PATH" "Frameworks")
    (homectl-update-env "DYLD_LIBRARY_PATH" "lib"))
   ((string= system-type "linux")
    (homectl-update-env "LD_LIBRARY_PATH" "lib")
    (homectl-update-env "LD_LIBRARY_PATH" "lib32")
    (homectl-update-env "LD_LIBRARY_PATH" "lib64")))

  ; Update emacs internal paths from environment variables
  (setq exec-path (append (split-string (getenv "PATH") ":" t) exec-path))
  (setq load-path (append (process-lines homectl-bin "path" "emacs")
                          load-path))

  ; Load all emacs packages from homectl
  (dolist (pdir (process-lines homectl-bin "path" "emacs"))
    (dolist (f (directory-files pdir nil "^[^.].*\.el"))
      (let ((pkg (replace-regexp-in-string "\.el$" "" f)))
        (message "[homectl] Loading %s" pkg)
        (require (intern pkg)))))

  ; Load old-style homectl hooks
  (message "[homectl] Loading standard init files...")
  (homectl-foreach-enabled pkg
    (homectl-enable-pkg pkg)))



;;;
;;; Startup homectl
;;;

(homectl-startup)

(provide 'homectl)
