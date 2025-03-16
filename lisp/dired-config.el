;; Dired Configuration Module

;; Customize dired appearance and behavior
(use-package dired
  :straight (:type built-in)
  :defer t
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  
  ;; Auto refresh dired when file changes
  (setq dired-auto-revert-buffer t)
  
  ;; Use human-readable sizes
  (setq dired-listing-switches "-alh")
  
  ;; Move files between split panes
  (setq dired-dwim-target t)
  
  ;; Hide details by default for cleaner view
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; Enhance Dired with icons and colors
(use-package all-the-icons-dired
  :straight t
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Add colorful highlighting to different file types in dired
(use-package dired-rainbow
  :straight t
  :defer t
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi" "bin" "xpi" "dll" "deb" "dmg" "iso" "jar" "class"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem")))

;; Add dired-subtree for expandable directories
(use-package dired-subtree
  :straight t
  :defer t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; Add dired-collapse to make nested directories more compact
(use-package dired-collapse
  :straight t
  :defer t
  :hook (dired-mode . dired-collapse-mode))

;; Add dired-open to open files with external applications
(use-package dired-open
  :straight t
  :defer t
  :after dired
  :config
  (setq dired-open-extensions
        '((".pdf" . "open")
          (".docx" . "open")
          (".xlsx" . "open")
          (".png" . "open")
          (".jpg" . "open")
          (".mp4" . "open"))))

;; Provide the module
(provide 'dired-config)