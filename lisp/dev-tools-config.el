;; Development Tools Configuration Module

;; Magit for Git integration
(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))

;; Projectile for project management
(use-package projectile
  :straight t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Company mode for code completion
(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1))

;; Flycheck for syntax checking
(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

;; Provide the module
(provide 'dev-tools-config)