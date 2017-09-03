; Because we update Info-directory-* below
(require 'info)

(package-initialize)

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
;;; Startup-related utility functions
;;;

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
  (setq load-path (append (process-lines homectl-bin "path" "emacs-startup")
                          load-path))

  ; Load all emacs startup packages from homectl
  (dolist (pdir (process-lines homectl-bin "path" "emacs-startup"))
    (dolist (f (directory-files pdir nil "^[^.].*\\.el"))
      (let ((pkg (replace-regexp-in-string "\.el$" "" f)))
        (message "[homectl] Loading %s" pkg)
        (require (intern pkg))))))



;;;
;;; Startup homectl
;;;

(homectl-startup)

(provide 'homectl)
