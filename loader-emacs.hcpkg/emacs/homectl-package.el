;;; Helper library which provides a nice wrapper around package.el.
;;;
;;; This is in its own module so user homectl packages can just require it.

(require 'package)
(package-initialize)

(defvar homectl-package-refreshed-recently-p nil)



(defun homectl-package-add-repo (name url)
    "Adds a repository to package.el's `package-archives' variable if it's
not already present."

    (add-to-list 'package-archives (cons name url)))



(defun homectl-package-require (&rest pkg-symbols)
  "Loads packages, possibly installing them through package.el if necessary."

  (dolist (pkg pkg-symbols)
    (unless (require pkg nil t)
      (unless homectl-package-refreshed-recently-p
        (package-refresh-contents)
        (setq homectl-package-refreshed-recently-p t))
      (package-install pkg)
      (require pkg))))



;; Add MELPA by default since that's where most packages come from.
(homectl-package-add-repo "melpa" "https://melpa.org/packages/")

(provide 'homectl-package)
