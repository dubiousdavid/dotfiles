(defvar emacs-dir "~/.emacs.d/")

;; Magit
(defun magit-status-window ()
  (magit-status)
  (delete-other-windows))
;; Load local file
(defun load-local (f)
  (load-file (concat emacs-dir f)))
;; Use these package archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
;; Initialize packages
(setq package-enable-at-startup nil)
(package-initialize)
;; Line numbers
(global-linum-mode t)
(setq linum-format "%4d ")
;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#1C1C1C")
;; Company mode
(global-set-key (kbd "C-x C-o") 'company-complete)
;; Disable menu bar
(menu-bar-mode -1)
;; No bell
(setq ring-bell-function 'ignore)
;; IDO
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
;; Undo tree
(global-undo-tree-mode)
;; Ensime
(require 'ensime)
(setq ensime-sem-high-enabled-p nil)
;; Theme
(load-local "atom-dark-theme.el")
;; Highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; Smart mode line
(setq sml/no-confirm-load-theme t)
(sml/setup)
;; Mouse
(load-local "mouse.el")
;; OSX
(load-local "osx.el")
;; Evil mode
(require 'evil)
(evil-mode 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "f" 'find-file
                     "b" 'switch-to-buffer
                     "k" 'kill-this-buffer
                     "g" 'magit-status
		     "a" 'ack
                     "w" 'evil-ace-jump-word-mode
                     "l" 'evil-ace-jump-line-mode
                     "c" 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
						(interactive)
						(evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
						(interactive)
						(evil-scroll-down nil)))
;; surround
(require 'evil-surround)
(global-evil-surround-mode 1)
;; numbers
(require 'evil-numbers)
;; (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
;; Split and move the cursor to the new split
(define-key evil-normal-state-map (kbd "-") (lambda ()
					      (interactive)
					      (split-window-vertically)
					      (other-window 1)))
(define-key evil-normal-state-map (kbd "|") (lambda ()
					      (interactive)
					      (split-window-horizontally)
					      (other-window 1)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ack evil-surround evil-numbers smart-mode-line highlight-numbers ace-jump-mode evil-leader ido-vertical-mode flx-ido evil ensime undo-tree magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
