; Because we update Info-directory-* below
(require 'info)

;;;
;;; Customization
;;;

(defvar homectl-dir (concat (getenv "HOME") "/.homectl")
  "The location of the homectl directory.  You should not change
  this unless you're a homectl developer and you change it in all
  the other places where it's hard-coded. :)")

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
  `(dolist (,path-var (directory-files homectl-dir t nil t))
     (when (not (string-prefix-p "." (file-name-nondirectory ,path-var)))
       ,@body)))

(defun homectl-require (repo-name repo-url pkg-symbol)
  "Loads a package, optionally downloading it from the specified package
repository if it is not already available on the system."

  (cond
   ; It's already available locally
   ((require pkg-symbol nil t)
    pkg-symbol)

   ; Else try to fetch the package
   (t
    (when (not (assoc repo-name package-archives))
      (add-to-list 'package-archives (cons repo-name repo-url)))
    (package-install pkg-symbol)
    (require pkg-symbol))))



;;;
;;; Startup-related utility functions
;;;

(defun homectl-load-package-el ()
  "Download a version of package.el if it's not available."

  (let ((my-user-dir (expand-file-name "~/.emacs.d/elpa"))
        (my-package-el (expand-file-name "~/.emacs.d/elpa/package.el")))

    (cond
     ; Load it locally if it exists already.
     ((require 'package nil t)
      t)

     ((file-exists-p my-package-el)
      (load my-package-el)
      t)

     ; Don't load code from remote sites without asking the user first.
     ((not (y-or-n-p "package.el not available; download it now?"))
      (message "[homectl] WARNING: Remote packages may not work")
      nil)

     ; Grab package.el from the web
     (t
      (let ((buffer (url-retrieve-synchronously
                     "http://tromey.com/elpa/package.el")))
        (save-excursion
          (set-buffer buffer)

          ; Drop HTTP headers
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (delete-region (point-min) (point))

          ; Save package.el to a file
          (setq buffer-file-name my-package-el)
          (make-directory my-user-dir t)
          (save-buffer)
          (kill-buffer buffer)))

      ; Load it
      (load my-package-el)))))

(defun homectl-env-add-to-path (var path)
  "Add a path to a :-separated environment variable."
  (when (file-directory-p path)
    (let ((path-list (split-string (getenv var) path-separator t)))
      (when (not (member path path-list))
        (message (concat "[homectl] Updating " var ": " path))
        (setenv var (concat path path-separator (getenv var)))))))

(defun homectl-enable-pkg (pkg-path)
  "Enable a single homectl package, located at /pkg-path/."
  (message (concat "[homectl] Enabling package: " pkg-path))

  (let ((start-file (concat pkg-path "/" "emacs.el"))
        (lp-dir (concat pkg-path "/" "emacs"))
        (bin-dir (concat pkg-path "/" "bin")))

    ; Emacs dir
    (when (file-directory-p lp-dir)
      (add-to-list 'load-path lp-dir)
      (add-to-list 'Info-directory-list lp-dir)
      (add-to-list 'Info-default-directory-list lp-dir))

    ; bin/
    (when (file-directory-p bin-dir)
      (add-to-list 'exec-path bin-dir)
      (homectl-env-add-to-path "PATH" bin-dir))

    ; lib*/
    (cond
     ((string= system-type "darwin")
      (let ((lib-dir (concat pkg-path "/" "lib"))
            (fw-dir (concat pkg-path "/" "Frameworks")))
        (homectl-env-add-to-path "DYLD_LIBRARY_PATH" lib-dir)
        (homectl-env-add-to-path "DYLD_FRAMEWORK_PATH" fw-dir)))
     (t
      (dolist (l '("lib" "lib32" "lib64"))
        (let ((lib-dir (concat pkg-path "/" l)))
          (homectl-env-add-to-path "LD_LIBRARY_PATH" lib-dir)))))

    (delete-dups exec-path)
    (delete-dups load-path)
    (delete-dups Info-directory-list)
    (delete-dups Info-default-directory-list)

    ; Load the package's startup file
    (when (file-exists-p start-file)
      (load-file start-file))))



;;;
;;; Main entry point
;;;

(defun homectl-startup ()
  "Load all enabled homectl packages with Emacs customizations,
and make sure the package's binaries are available in Emacs's
environment."
  (interactive)

  ; Make sure we're ready to fetch remote packages
  (when (homectl-load-package-el)
    (package-initialize))

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
  (homectl-foreach-enabled pkg
    (homectl-enable-pkg pkg)))



;;;
;;; Startup homectl
;;;

(homectl-startup)

(provide 'homectl)
