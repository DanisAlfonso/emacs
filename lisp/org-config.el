;; Org Mode Configuration Module

;; Basic org mode configuration
(use-package org
  :straight (:type built-in)
  :defer t
  :hook (org-mode . visual-line-mode)
  :config
  ;; Basic org settings
  (setq org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-pretty-entities t)
  
  ;; Set org heading sizes
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :height (cdr face)))
  
  ;; Improve spacing in org mode
  (setq org-cycle-separator-lines 2
        org-list-allow-alphabetical t
        org-export-headline-levels 6
        org-blank-before-new-entry '((heading . t) (plain-list-item . auto))))

;; Org modern for a cleaner, more modern UI
(use-package org-modern
  :straight t
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-star ["◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"])
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  (org-modern-list '((43 . "➤")
                     (45 . "–")
                     (42 . "•")))
  (org-modern-todo-faces
   '(("TODO" :inverse-video t :inherit org-todo)
     ("PROJ" :inverse-video t :inherit +org-todo-project)
     ("STRT" :inverse-video t :inherit +org-todo-active)
     ("[-]"  :inverse-video t :inherit +org-todo-active)
     ("HOLD" :inverse-video t :inherit +org-todo-onhold)
     ("WAIT" :inverse-video t :inherit +org-todo-onhold)
     ("[?]"  :inverse-video t :inherit +org-todo-onhold)
     ("DONE" :inverse-video t :inherit org-done)
     ("KILL" :inverse-video t :inherit +org-todo-cancel))))

;; Variable pitch fonts in Org mode
(use-package mixed-pitch
  :straight t
  :defer t
  :hook
  (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 1.1))

;; Visual fill column for better text wrapping
(use-package visual-fill-column
  :straight t
  :defer t
  :hook
  (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

;; Org superstar for prettier headings and bullets
(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (org-superstar-item-bullet-alist '(("*" . "•")
                                     ("+" . "➤")
                                     ("-" . "–")))
  (org-superstar-leading-bullet " "))

;; Olivetti mode for distraction-free writing
(use-package olivetti
  :straight t
  :defer t
  :bind ("<f10>" . olivetti-mode)  ;; Changed from F7 to F10 to avoid conflict with transparency control
  :custom
  (olivetti-body-width 100))

;; Provide the module
(provide 'org-config)