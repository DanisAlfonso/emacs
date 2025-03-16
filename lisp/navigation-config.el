;; Navigation and Search Configuration

;; Ivy - Enhanced completion framework
(use-package ivy
  :straight t
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))

;; Counsel - Collection of Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :straight t
  :after ivy
  :diminish
  :config 
  (counsel-mode 1)
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x b" . counsel-switch-buffer)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-c C-r" . ivy-resume))

;; Swiper - Isearch with an overview
(use-package swiper
  :straight t
  :after ivy
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper-backward))

;; Ace-window - Window navigation
(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background t))

;; Provide the module
(provide 'navigation-config)