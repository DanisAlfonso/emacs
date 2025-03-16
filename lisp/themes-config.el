;; Themes Configuration Module

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

;; Bind theme toggle to F6 (manual override)
(global-set-key [f6] 'toggle-theme)

;; Provide the module
(provide 'themes-config)