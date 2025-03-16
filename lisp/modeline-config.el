;; Modeline Configuration Module

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

;; Provide the module
(provide 'modeline-config)