;; early-init.el --- Early Init File for faster startup

;; Increase the garbage collection threshold to allow more memory during startup
;; This significantly reduces the number of garbage collections during initialization
(setq gc-cons-threshold 100000000) ;; 100MB (default is 800KB)

;; Restore a more conservative threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 1600000))) ;; 1.6MB for regular use

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a time-consuming operation, especially
;; when done repeatedly during initialization. This prevents frame resizing
;; during startup.
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Faster startup by not loading the site-wide default.el
(setq site-run-file nil)

;; Disable bidirectional text scanning for a modest performance boost
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions
(setq fast-but-imprecise-scrolling t)

;; Disable GUI elements
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a time-consuming operation
;; so we use this trick to avoid it during initialization
(setq frame-inhibit-implied-resize t)

;; Disable backup files creation during startup
(setq make-backup-files nil)

;; Fundamental mode is faster than other major modes
(setq initial-major-mode 'fundamental-mode)