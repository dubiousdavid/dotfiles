;; Path to emacs directory
(defvar emacs-dir "~/.emacs.d/")
;; Duplicate line below
(defun duplicate-line-below ()
  "Duplicate current line below."
  (interactive)
  (let ((origianl-column (current-column))
        line-content)
    (setq line-content (buffer-substring (line-beginning-position) (line-end-position)))
    (beginning-of-line)
    (forward-line +1)
    (newline +1)
    (forward-line -1)
    (insert line-content)
    (move-to-column origianl-column t)))
;; Run ack from root directory
(defun ack-from-root ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'ack)))
;; y$
(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))
;; Magit
(defun magit-status-window ()
  (magit-status)
  (delete-other-windows))
;; Load local file
(defun load-local (f)
  (load-file (concat emacs-dir f)))
;; Normal mode key chord
(defmacro normal-key-chord (chord f)
  `(key-chord-define evil-normal-state-map ,chord ,f))
;; Normal mode key
(defmacro normal-key (keys f)
  `(define-key evil-normal-state-map (kbd ,keys) ,f))
;; Split and move the cursor to the new split
(defmacro split-and-focus (f)
  `(lambda ()
     (interactive)
     (,f)
     (other-window 1)))
;; Use these package archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
;; Initialize packages
(setq package-enable-at-startup nil)
(package-initialize)
;; Manual mode selection
(add-to-list 'auto-mode-alist '("\\.sc" . scala-mode))
;; Line numbers
(global-linum-mode t)
(setq linum-format "%4d ")
;; Auto close brackets
(electric-pair-mode 1)
;; Highlight matching bracket
(show-paren-mode 1)
;; Prettify symbols
(global-prettify-symbols-mode 1)
;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#1C1C1C")
;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Company 
(global-set-key (kbd "C-x C-o") 'company-complete)
;; Disable menu bar
(menu-bar-mode -1)
;; No bell
(setq ring-bell-function 'ignore)
;; IDO (flx, vertical)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-show-count t)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
;; Undo tree
(global-undo-tree-mode)
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
(setq evil-want-C-u-scroll t)
(setq evil-shift-width 2)
(setq evil-regexp-search nil)
(require 'evil)
(evil-mode 1)
;; Leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "f" 'find-file
                     "b" 'switch-to-buffer
                     "k" 'kill-this-buffer
                     "g" 'magit-status
                     "d" 'duplicate-line-below
		     "a" 'ack-from-root
		     "u" 'undo-tree-visualize)
;; Args
(require 'evil-args)
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
;; Comments
(evil-commentary-mode)
;; nmap Y y$
(normal-key "Y" 'copy-to-end-of-line)
;; Buffers
(normal-key "C-l" 'evil-buffer)
(normal-key "C-j" 'previous-buffer)
(normal-key "C-k" 'next-buffer)
;; Key chord
(require 'key-chord)
(key-chord-mode 1)
;; Ace jump
(normal-key-chord "mw" 'evil-ace-jump-word-mode)
(normal-key-chord "ml" 'evil-ace-jump-line-mode)
(normal-key-chord "mc" 'evil-ace-jump-char-mode)
;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)
;; Inc/dec numbers
(require 'evil-numbers)
(normal-key "C-a" 'evil-numbers/inc-at-pt)
(normal-key "C-s" 'evil-numbers/dec-at-pt)
;; Split and focus
(normal-key "_" (split-and-focus split-window-vertically))
(normal-key "|" (split-and-focus split-window-horizontally))
;; Ensime
(require 'ensime)
(setq ensime-sem-high-enabled-p nil)
;; Inspect type
(eval-after-load 'ensime '(evil-leader/set-key "i" 'ensime-inspect-type-at-point))
;; Errors and warnings
(eval-after-load 'ensime '(evil-leader/set-key "e" 'ensime-show-all-errors-and-warnings))
;; References of symbol
(eval-after-load 'ensime '(evil-leader/set-key "r" 'ensime-show-uses-of-symbol-at-point))
;; Jump to definition
(eval-after-load 'ensime '(evil-leader/set-key "j" 'ensime-edit-definition))
;; Expand selection
(eval-after-load 'ensime '(evil-leader/set-key "." 'ensime-expand-selection-command))
;; Lookup type
(eval-after-load 'ensime '(evil-leader/set-key "l" 'ensime-search))
;; SBT
(eval-after-load 'ensime '(evil-leader/set-key "s" 'ensime-sbt))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fsharp-mode evil-args evil-commentary key-chord ack evil-surround evil-numbers smart-mode-line highlight-numbers ace-jump-mode evil-leader ido-vertical-mode flx-ido evil ensime undo-tree magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
