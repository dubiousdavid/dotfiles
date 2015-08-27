(require 'auto-highlight-symbol)

(define-key global-map (kbd "C-c h h") 'auto-highlight-symbol-mode)
(define-key auto-highlight-symbol-mode-map (kbd "C-c h r") 'ahs-change-range)
(define-key auto-highlight-symbol-mode-map (kbd "C-c h b") 'ahs-backward)
(define-key auto-highlight-symbol-mode-map (kbd "C-c h f") 'ahs-forward)
(define-key auto-highlight-symbol-mode-map (kbd "C-c h e") 'ahs-edit-mode)
