;; Basic UI settings
(tooltip-mode -1)       ;; Disable tooltips
(set-fringe-mode 10)    ;; Give some padding around text

;; Display line numbers except in some modes
(global-display-line-numbers-mode t)
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; File handling settings
(setq auto-save-default nil)         ;; Disable auto save
(setq create-lockfiles nil)          ;; Don't create lock files

;; Performance optimizations
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq process-adaptive-read-buffering nil)   ;; Disable adaptive buffering

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el
(setq straight-use-package-by-default t)

;; Install and configure use-package
(straight-use-package 'use-package)

;; Always use straight.el for use-package expressions
(setq straight-use-package-by-default t)

;; Set default frame size
(add-to-list 'default-frame-alist '(width . 120))   ;; Set width to 120 columns
(add-to-list 'default-frame-alist '(height . 45))  ;; Set height to 45 rows
(setq initial-frame-alist default-frame-alist)     ;; Apply same size to initial frame

;; Transparent titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Set font configuration
;; Using Cascadia Code as the primary font with increased size (14pt)
(set-face-attribute 'default nil :font "Cascadia Code" :height 140)
;; If Cascadia Code is not available, try these alternatives:
;; (set-face-attribute 'default nil :font "Menlo" :height 140)
;; (set-face-attribute 'default nil :font "Monaco" :height 140)
;; Note: :height 140 means 14pt (height is in 1/10pt)


;; Install and configure themes
(use-package timu-rouge-theme
  :straight t
  :defer t)

(use-package doom-themes
  :straight (:host github :repo "doomemacs/emacs-doom-themes")
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
  ;; theme may have their own settings.
  (load-theme 'doom-one t)
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Set up dark and light themes
(setq dark-theme 'doom-one
      light-theme 'doom-one-light)

;; Function to toggle between light and dark themes
(defun toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) dark-theme)
      (progn
        (disable-theme dark-theme)
        (load-theme light-theme t))
    (progn
      (disable-theme light-theme)
      (load-theme dark-theme t))))

;; Function to detect macOS dark mode
(defun system-dark-mode-enabled-p ()
  "Check if macOS dark mode is enabled."
  (when (eq system-type 'darwin)
    (string-equal
     "true"
     (string-trim
      (shell-command-to-string
       "defaults read -g AppleInterfaceStyle 2>/dev/null || echo light"))
     )))

;; Function to apply theme based on system appearance
(defun apply-theme-based-on-system-appearance ()
  "Apply appropriate theme based on macOS system appearance."
  (interactive)
  (if (system-dark-mode-enabled-p)
      (unless (eq (car custom-enabled-themes) dark-theme)
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme dark-theme t))
    (unless (eq (car custom-enabled-themes) light-theme)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme light-theme t))))

;; Use a single theme by default (doom-one-light)
;; No automatic theme switching - use F6 to toggle themes manually

;; Bind theme toggle to F6 (manual override)
(global-set-key [f6] 'toggle-theme)


;; Install and configure doom-modeline
(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12
        doom-modeline-env-version t
        doom-modeline-irc-stylize 'identity
        doom-modeline-github-timer nil)
  :hook (after-init . doom-modeline-mode))

;; Install all-the-icons (required for doom-modeline)
(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :defer t)

;; Enhance Dired with icons and colors
(use-package all-the-icons-dired
  :straight t
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Add colorful highlighting to different file types in dired
(use-package dired-rainbow
  :straight t
  :defer t
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi" "bin" "xpi" "dll" "deb" "dmg" "iso" "jar" "class"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem")))

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

;; Customize dired appearance and behavior
(use-package dired
  :straight (:type built-in)
  :defer t
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  
  ;; Auto refresh dired when file changes
  (setq dired-auto-revert-buffer t)
  
  ;; Use human-readable sizes
  (setq dired-listing-switches "-alh")
  
  ;; Move files between split panes
  (setq dired-dwim-target t)
  
  ;; Hide details by default for cleaner view
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; Add dired-subtree for expandable directories
(use-package dired-subtree
  :straight t
  :defer t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; Add dired-collapse to make nested directories more compact
(use-package dired-collapse
  :straight t
  :defer t
  :hook (dired-mode . dired-collapse-mode))

;; Removed tree-sitter configuration to use default syntax highlighting

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

;; Add dired-open to open files with external applications
(use-package dired-open
  :straight t
  :defer t
  :after dired
  :config
  (setq dired-open-extensions
        '((".pdf" . "open")
          (".docx" . "open")
          (".xlsx" . "open")
          (".png" . "open")
          (".jpg" . "open")
          (".mp4" . "open"))))

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

