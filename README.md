# Emacs Configuration

This is a modern Emacs configuration using straight.el for package management and use-package for organizing package configuration.

## Current Features

- **Performance Optimizations**: GC tuning, faster scrolling, and startup optimizations
- **Modern UI**: Doom themes, doom-modeline, and all-the-icons integration
- **Dired Enhancements**: Icons, rainbow coloring, and improved navigation
- **Org Mode Beautification**: Modern styling with org-modern, org-superstar, and mixed-pitch
- **Transparency Control**: Dynamic frame transparency adjustment
- **Theme Switching**: Toggle between light and dark themes
- **Modular Configuration**: Split configuration into multiple files organized by functionality
- **Development Tools**: Magit, Projectile, Company, and Flycheck integration
- **Navigation and Search**: Ivy/Counsel and Ace-window for improved navigation
- **Language Support**: LSP Mode and Markdown support
- **Quality of Life Improvements**: Helpful, Undo-tree, YASnippet, and Multiple-cursors
- **Org Mode Enhancements**: Org-roam and Org-bullets for better note-taking
- **Performance Improvements**: Native compilation support
- **Terminal Integration**: Vterm for better terminal emulation

## Implemented Improvements

All of the following improvements have been implemented in this Emacs configuration:

### 1. Package Management and Organization

- **Modular Configuration**: Split your init.el into multiple files organized by functionality
  ```elisp
  ;; Create a lisp/ directory for modules
  ;; In init.el, load modules like this:
  (defun load-directory (dir)
    (let ((load-it (lambda (f)
                     (load-file (concat (file-name-as-directory dir) f)))))
      (mapc load-it (directory-files dir nil "\\.el$"))))
  (load-directory "~/.emacs.d/lisp/")
  ```

### 2. Development Tools

- **Version Control**: Add Magit for Git integration
  ```elisp
  (use-package magit
    :straight t
    :bind ("C-x g" . magit-status))
  ```

- **Project Management**: Add Projectile for project navigation
  ```elisp
  (use-package projectile
    :straight t
    :diminish projectile-mode
    :config
    (projectile-mode +1)
    :bind-keymap
    ("C-c p" . projectile-command-map))
  ```

- **Code Completion**: Add Company mode
  ```elisp
  (use-package company
    :straight t
    :hook (prog-mode . company-mode)
    :config
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.1))
  ```

- **Syntax Checking**: Add Flycheck
  ```elisp
  (use-package flycheck
    :straight t
    :hook (prog-mode . flycheck-mode))
  ```

### 3. Navigation and Search

- **Improved Search**: Add Ivy/Counsel or Helm
  ```elisp
  ;; Ivy/Counsel option
  (use-package ivy
    :straight t
    :diminish
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          ivy-count-format "%d/%d "))
  
  (use-package counsel
    :straight t
    :after ivy
    :config (counsel-mode 1)
    :bind
    ("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file))
  ```

- **Window Management**: Add Ace-window for easier window navigation
  ```elisp
  (use-package ace-window
    :straight t
    :bind ("M-o" . ace-window))
  ```

### 4. Language Support

- **LSP Support**: Add Language Server Protocol support
  ```elisp
  (use-package lsp-mode
    :straight t
    :commands lsp
    :hook ((typescript-mode js2-mode web-mode) . lsp)
    :config
    (setq lsp-headerline-breadcrumb-enable nil))
  
  (use-package lsp-ui
    :straight t
    :commands lsp-ui-mode)
  ```

- **Markdown Support**: Add markdown-mode
  ```elisp
  (use-package markdown-mode
    :straight t
    :mode ("\\.md\\'"))
  ```

### 5. Quality of Life Improvements

- **Better Help**: Add Helpful for improved help documentation
  ```elisp
  (use-package helpful
    :straight t
    :bind
    ("C-h f" . helpful-callable)
    ("C-h v" . helpful-variable)
    ("C-h k" . helpful-key))
  ```

- **Undo System**: Add Undo-tree or Undo-fu for better undo/redo
  ```elisp
  (use-package undo-tree
    :straight t
    :config
    (global-undo-tree-mode 1))
  ```

- **Snippets**: Add YASnippet for code templates
  ```elisp
  (use-package yasnippet
    :straight t
    :hook (prog-mode . yas-minor-mode)
    :config
    (yas-reload-all))
  
  (use-package yasnippet-snippets
    :straight t
    :after yasnippet)
  ```

- **Multiple Cursors**: Add multiple-cursors for editing multiple regions
  ```elisp
  (use-package multiple-cursors
    :straight t
    :bind
    ("C-S-c C-S-c" . mc/edit-lines)
    ("C->" . mc/mark-next-like-this)
    ("C-<" . mc/mark-previous-like-this))
  ```

### 6. Org Mode Enhancements

- **Org Roam**: Add Org-roam for networked note-taking
  ```elisp
  (use-package org-roam
    :straight t
    :custom
    (org-roam-directory "~/org-roam")
    :bind
    ("C-c n l" . org-roam-buffer-toggle)
    ("C-c n f" . org-roam-node-find)
    ("C-c n i" . org-roam-node-insert)
    :config
    (org-roam-setup))
  ```

- **Org Bullets**: Add org-bullets for prettier headings
  ```elisp
  (use-package org-bullets
    :straight t
    :hook (org-mode . org-bullets-mode))
  ```

### 7. Performance Improvements

- **Native Compilation**: Enable native compilation if using Emacs 28+
  ```elisp
  ;; In early-init.el
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t))
  ```

### 8. Terminal Integration

- **Vterm**: Add vterm for better terminal emulation
  ```elisp
  (use-package vterm
    :straight t)
  ```

Implementing these improvements will enhance your Emacs experience with better development tools, navigation, and quality of life features while maintaining the clean and modern UI you've already established.