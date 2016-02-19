;; Paste pop
(defun paste-pop (count)
  "Pop the previous entry in the kill ring."
  (interactive "p")
  (let (interprogram-paste-function)
    (evil-paste-pop count)))
;; Paste pop next
(defun paste-pop-next (count)
  "Pop the next entry in the kill ring."
  (interactive "p")
  (let (interprogram-paste-function)
    (evil-paste-pop-next count)))
;; Copy (OSX)
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
;; Paste (OSX)
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
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
;; Tidy markdown
(defun tidy-markdown ()
  (interactive)
  (shell-command-on-region
     (point-min)
     (point-max)
     "tidy-markdown"
     (current-buffer)
     t
     "*tidy-markdown-error*"
     t))
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
  (load-file (concat user-emacs-directory f)))
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
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))
;; Line numbers
(global-linum-mode t)
(setq linum-format "%4d ")
;; Auto close brackets
(electric-pair-mode 1)
;; Highlight matching bracket
(show-paren-mode 1)
;; Prettify symbols
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
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
;; Save cursor position
(save-place-mode t)
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
(add-to-list 'ido-ignore-files "\\.DS_Store")
;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-globally-ignored-files
      '(".ensime_cache" ".ensime" "target" "project" "bin" "tmp"))
(setq projectile-mode-line "")
;; Undo tree
(global-undo-tree-mode)
;; Theme
(load-local "atom-dark-theme.el")
;; Highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; Smart mode line
(setq sml/no-confirm-load-theme t)
(setq rm-blacklist '(" Undo-Tree" " yas" " s-/"))
(sml/setup)
;; Ack
(setq ack-command "ag ")
;; Mouse in terminal
(xterm-mouse-mode 1)
;; Mouse yank
(setq mouse-yank-at-point t)
;; Enable scrolling
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
;; Input method
(setq default-input-method "MacOSX")
;; Make cut and paste work with the OS X clipboard
(when (not window-system)
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))
;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; Markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\.erb\\'" . markdown-mode))
(setq markdown-open-command "~/bin/mark")
;; Evil mode
(setq evil-want-C-u-scroll t)
(setq evil-shift-width 2)
(setq evil-regexp-search nil)
(evil-mode 1)
(load-local "evil-sexp.el")
(evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'ensime-inspector-mode 'motion)
(evil-set-initial-state 'sbt-mode 'insert)
(evil-set-initial-state 'ack-mode 'motion)
(evil-declare-change-repeat 'company-complete)
;; Leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "f" 'projectile-find-file
                     "b" 'switch-to-buffer
                     "g" 'magit-status
                     "h" 'github-browse-file
                     "d" 'duplicate-line-below
		     "a" 'ack-from-root
		     "u" 'undo-tree-visualize)
;; Args
(require 'evil-args)
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
;; Paredit
(evil-define-key 'motion emacs-lisp-mode-map
  (kbd "(") 'evil-backward-sexp
  (kbd ")") 'evil-forward-sexp)
(evil-define-key 'visual emacs-lisp-mode-map
  (kbd "(") 'evil-backward-sexp
  (kbd ")") 'evil-forward-sexp)
(evil-define-key 'motion emacs-lisp-mode-map
  (kbd "{") 'evil-backward-section-begin
  (kbd "}") 'evil-forward-section-begin)
(evil-define-key 'visual emacs-lisp-mode-map
  (kbd "{") 'evil-backward-section-begin
  (kbd "}") 'evil-forward-section-begin)
;; Buffers
(normal-key "DEL" 'kill-this-buffer)
(normal-key "<up>" 'buf-move-up)
(normal-key "<down>" 'buf-move-down)
;; Comments
(evil-commentary-mode)
;; Drag stuff
(drag-stuff-mode t)
(normal-key "C-j" 'drag-stuff-down)
(normal-key "C-k" 'drag-stuff-up)
(define-key evil-visual-state-map (kbd "C-j") 'drag-stuff-down)
(define-key evil-visual-state-map (kbd "C-k") 'drag-stuff-up)
;; Paste pop
(normal-key "C-p" 'paste-pop)
(normal-key "C-n" 'paste-pop-next)
;; nmap Y y$
(normal-key "Y" 'copy-to-end-of-line)
;; Buffers
(normal-key "C-l" 'evil-buffer)
;; Key chord
(require 'key-chord)
(key-chord-mode 1)
;; Ace jump
(normal-key-chord "mw" 'evil-ace-jump-word-mode)
(normal-key-chord "ml" 'evil-ace-jump-line-mode)
(normal-key-chord "mc" 'evil-ace-jump-char-mode)
;; Easy motion
;; (evilem-default-keybindings "[")
;; (setq avy-keys (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
;; (setq avy-background t)
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
(eval-after-load 'ensime '(setq ensime-sem-high-enabled-p nil))
(evil-leader/set-key-for-mode 'scala-mode
  ;; Inspect type
  "i" 'ensime-inspect-type-at-point
  ;; Errors and warnings
  "e" 'ensime-show-all-errors-and-warnings
  ;; References of symbol
  "r" 'ensime-show-uses-of-symbol-at-point
  ;; Jump to definition
  "j" 'ensime-edit-definition
  ;; Expand selection
  "." 'ensime-expand-selection-command
  ;; Lookup type
  "l" 'ensime-search
  ;; SBT
  "s" 'ensime-sbt)
;; Find function (elisp)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "j" 'find-function)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (web-mode markdown-mode json-mode ace-jump-mode key-chord buffer-move drag-stuff paredit csharp-mode browse-kill-ring projectile fsharp-mode evil-args evil-commentary ack evil-surround evil-numbers smart-mode-line highlight-numbers evil-leader ido-vertical-mode flx-ido evil ensime undo-tree magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
