;; Language Support Configuration Module

;; LSP Support for various programming languages
(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

;; Markdown Support
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'"))

;; Provide the module
(provide 'lang-support-config)