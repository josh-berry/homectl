;;; Helper library which provides a nice wrapper around package.el.
;;;
;;; This is in its own module so user homectl packages can just require it.

(require 'package)

(defvar homectl-package-refreshed-recently-p nil)



(defun homectl-package-add-repo (name url)
    "Adds a repository to package.el's `package-archives' variable if it's
not already present."

    (add-to-list 'package-archives (cons name url)))



(defun homectl-package-require (&rest pkg-symbols)
  "Loads packages, possibly installing them through package.el if necessary."

  (dolist (pkg pkg-symbols)
    ;; Try to load the package first locally
    (unless (require pkg nil t)

      ;; It wasn't found; refresh the package list if it hasn't been loaded yet
      ;; this session
      (unless homectl-package-refreshed-recently-p
        (package-refresh-contents)
        (setq homectl-package-refreshed-recently-p t))

      ;; Install and load the package
      (message "[homectl] Installing package: %s" pkg)
      (package-install pkg)
      (require pkg))

    ;; If the package was installed by package.el at some point (now or in the
    ;; past), explicitly mark it as user-selected.  (This explicit marking is
    ;; necessary because of a bug in package.el where if the
    ;; package-selected-packages list is empty, package.el thinks the package is
    ;; already marked as user-selected.)
    (when (not (package-built-in-p pkg))
      (add-to-list 'package-selected-packages pkg))))



;; Add MELPA by default since that's where most packages come from.
(homectl-package-add-repo "melpa" "https://melpa.org/packages/")

;; After startup, make sure package-selected-packages is saved to custom.el so
;; the user will notice the differences in their homectl setup.
(defun homectl-package--save-selected-packages ()
  (customize-save-variable 'package-selected-packages
                           package-selected-packages))

(add-to-list 'emacs-startup-hook #'homectl-package--save-selected-packages)

(provide 'homectl-package)
