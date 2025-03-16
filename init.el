;; Function to load all modules from lisp directory
(defun load-directory (dir)
  "Load all Emacs Lisp files in directory DIR."
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; Load package management module first
(load-file (expand-file-name "lisp/package-config.el" user-emacs-directory))

;; Add lisp directory to load-path so require can find the modules
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load core configuration modules
(require 'ui-config)       ;; UI settings
(require 'themes-config)   ;; Theme settings
(require 'modeline-config) ;; Modeline configuration
(require 'dired-config)    ;; Dired enhancements
(require 'org-config)      ;; Org mode configuration
(require 'navigation-config) ;; Navigation and search enhancements
(require 'dev-tools-config) ;; Development tools
(require 'lang-support-config) ;; Language support
(require 'org-enhancements-config) ;; Org mode enhancements
(require 'terminal-config) ;; Terminal integration

;; File handling settings
(setq auto-save-default nil)         ;; Disable auto save
(setq create-lockfiles nil)          ;; Don't create lock files

;; Performance optimizations
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq process-adaptive-read-buffering nil)   ;; Disable adaptive buffering

;; Install and configure which-key for modern key binding discovery
(use-package which-key
  :straight t
  :defer 1
  :init
  (setq which-key-separator " → "
        which-key-prefix-prefix "→"
        which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator-face 'font-lock-comment-face)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; JavaScript and TypeScript support
(use-package typescript-mode
  :straight t
  :defer t
  :mode ("\\.ts\\'" "\\.tsx\\'"))

(use-package js2-mode
  :straight t
  :defer t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2
        js2-highlight-level 3
        js2-mode-show-parse-errors t
        js2-mode-show-strict-warnings t
        js2-strict-missing-semi-warning nil))

;; Basic C/C++ Development Environment Setup
(use-package cc-mode
  :straight (:type built-in)
  :defer t
  :config
  (setq c-default-style "linux"
        c-basic-offset 4))

;; Load transparency control module
(load-file (expand-file-name "transparency.el" user-emacs-directory))

;; Measure and report startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Add org-mode to the list of modes that don't show line numbers
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

