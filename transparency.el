;; transparency.el --- Dynamic transparency control for Emacs

;; Default transparency values
(defvar frame-transparency-alpha-value 100
  "Current frame transparency value (0-100).")

(defvar frame-transparency-step 5
  "Step size for transparency adjustments.")

(defvar frame-transparency-min 30
  "Minimum transparency value (percentage).")

(defvar frame-transparency-max 100
  "Maximum transparency value (percentage).")

;; Function to set frame transparency
(defun set-frame-transparency (value)
  "Set the transparency of the frame window to VALUE (0-100)."
  (interactive "nTransparency Value (0-100): ")
  ;; Ensure value is within bounds
  (setq value (max frame-transparency-min (min frame-transparency-max value)))
  ;; Set the transparency value
  (setq frame-transparency-alpha-value value)
  ;; Convert percentage to decimal (0.0-1.0) which is what Emacs actually uses
  (let ((alpha-value (/ (float value) 100.0)))
    ;; Apply transparency to all frames
    (dolist (frame (frame-list))
      ;; Set frame transparency using the most basic format
      (modify-frame-parameters frame `((alpha . ,alpha-value)))))
  ;; Display current transparency in minibuffer
  (message "Transparency set to %d%%" value))

;; Function to increase transparency (make more transparent)
(defun increase-transparency ()
  "Increase the transparency of the frame (make more transparent)."
  (interactive)
  (set-frame-transparency (- frame-transparency-alpha-value frame-transparency-step)))

;; Function to decrease transparency (make more opaque)
(defun decrease-transparency ()
  "Decrease the transparency of the frame (make more opaque)."
  (interactive)
  (set-frame-transparency (+ frame-transparency-alpha-value frame-transparency-step)))

;; Function to reset transparency to fully opaque
(defun reset-transparency ()
  "Reset transparency to fully opaque (100%)."
  (interactive)
  (set-frame-transparency 100))

;; Key bindings for transparency control
;; Define the prefix key map for transparency-related commands
(defvar transparency-map (make-sparse-keymap)
  "Keymap for transparency-related commands.")
(global-set-key (kbd "C-c t") transparency-map)

;; Now bind keys in the transparency keymap
(define-key transparency-map (kbd "+") 'decrease-transparency) ;; More opaque
(define-key transparency-map (kbd "-") 'increase-transparency) ;; More transparent
(define-key transparency-map (kbd "r") 'reset-transparency)    ;; Reset to opaque

;; Alternative key bindings using function keys
(global-set-key (kbd "<f7>") 'increase-transparency)  ;; F7 to increase transparency
(global-set-key (kbd "<f8>") 'decrease-transparency)  ;; F8 to decrease transparency
(global-set-key (kbd "<f9>") 'reset-transparency)     ;; F9 to reset transparency

;; Initialize transparency to fully opaque by default
(set-frame-transparency frame-transparency-alpha-value)

(provide 'transparency)
;;; transparency.el ends here