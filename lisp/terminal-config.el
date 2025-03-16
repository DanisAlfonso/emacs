;; Terminal Integration Configuration Module

;; Vterm for better terminal emulation
(use-package vterm
  :straight t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-buffer-name-string "vterm: %s")
  :bind
  ("C-c t" . vterm))

;; Provide the module
(provide 'terminal-config)