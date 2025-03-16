;; UI Configuration Module

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
                pdf-view-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set default frame size
(add-to-list 'default-frame-alist '(width . 120))   ;; Set width to 120 columns
(add-to-list 'default-frame-alist '(height . 40))  ;; Set height to 45 rows
(setq initial-frame-alist default-frame-alist)     ;; Apply same size to initial frame

;; Transparent titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Set font configuration
;; Try to use JetBrains Mono if available, otherwise fall back to system fonts
(defun font-exists-p (font)
  "Check if FONT is available."
  (if (null (x-list-fonts font)) nil t))

;; Set font with fallbacks
(cond
 ((font-exists-p "JetBrains Mono")
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 140))
 ((font-exists-p "Cascadia Code")
  (set-face-attribute 'default nil :font "Cascadia Code" :height 140))
 ((font-exists-p "Menlo")
  (set-face-attribute 'default nil :font "Menlo" :height 140))
 ((font-exists-p "Monaco")
  (set-face-attribute 'default nil :font "Monaco" :height 140))
 (t
  (set-face-attribute 'default nil :height 140))) ;; Just set the height if no preferred fonts are available

;; Note: :height 140 means 14pt (height is in 1/10pt)

;; Provide the module
(provide 'ui-config)