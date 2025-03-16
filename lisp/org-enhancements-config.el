;; Org Mode Enhancements Configuration Module

;; Org Roam for networked note-taking
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

;; Org Bullets for prettier headings
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")))

;; Provide the module
(provide 'org-enhancements-config)