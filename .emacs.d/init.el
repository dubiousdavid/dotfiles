(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " (file-name-directory filename))))
	(rename-file filename new-name t)
	(set-visited-file-name new-name t t)))))

(defun toggle-evil (mode)
  (if mode
      (evil-emacs-state 1)
    (evil-normal-state 1)))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defmacro make-motion (name fun)
  `(evil-define-motion ,name (count)
     :type inclusive
     (dotimes (i (or count 1))
       (,fun))))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
  Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-open-line (arg)
  "Insert an empty line after the current line.
  Position the cursor at its beginning, according to the current mode.
  With a prefix ARG open line above the current line."
  (interactive "P")
  (move-end-of-line nil)
  (newline-and-indent))
(defun copy-function-definition (new old)
  "Define NEW with the same function definition as OLD."
  (fset new (symbol-function old)))
;; Open recent files with IDO
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
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
;; Duplicate region
(defun duplicate-region ()
  (interactive)
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning) end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))
;; Duplicate line or region
(defun duplicate-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-line-below)))
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
     "tidy-markdown --no-ensure-first-header-is-h1"
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
  (interactive)
  (magit-status)
  (delete-other-windows))
;; Load local file
(defun load-local (f)
  (load-file (concat user-emacs-directory f)))
;; Tern
(defun tern-start ()
  (interactive)
  (tern-mode t))
;; Define keys for evil states
(defmacro normal-key (keys f)
  `(define-key evil-normal-state-map (kbd ,keys) ,f))
(defmacro insert-key (keys f)
  `(define-key evil-insert-state-map (kbd ,keys) ,f))
(defmacro visual-key (keys f)
  `(define-key evil-visual-state-map (kbd ,keys) ,f))
(defmacro motion-key (keys f)
  `(define-key evil-motion-state-map (kbd ,keys) ,f))
;; Split and move the cursor to the new split
(defmacro split-and-focus (f)
  `(lambda ()
     (interactive)
     (,f)
     (other-window 1)))
(defun scroll-up-3-lines ()
  "Scroll up 3 lines"
  (interactive)
  (scroll-up 3))
(defun scroll-down-3-lines ()
  "Scroll down 3 lines"
  (interactive)
  (scroll-down 3))
;; Initialize packages
(require 'package)
(setq package-enable-at-startup nil) ; To prevent initialising twice
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
;; Pin packages
(use-package ensime :pin melpa-stable)
;; Smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
;; No splash screen
(setq inhibit-splash-screen t)
;; Manual mode selection
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.agignore\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.tern-project\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ackrc\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.plist\\'" . xml-mode))
;; Line numbers
(global-linum-mode t)
(setq linum-format "%4d ")
;; Mouse
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'scroll-down-3-lines)
(global-set-key (kbd "<mouse-5>") 'scroll-up-3-lines)
;; Highlight matching bracket
(show-paren-mode 1)
;; Smartparens
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
;; Projectile
(add-hook 'prog-mode-hook 'projectile-mode)
(setq projectile-mode-line "")
;; Whitespace mode
(setq whitespace-line-column 120)
;; Emacs lisp mode
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'evil-smartparens-mode)
;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#1C1C1C")
;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Company
(add-hook 'prog-mode-hook 'company-mode)
(global-set-key (kbd "C-x C-o") 'company-complete)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(with-eval-after-load 'company (add-to-list 'company-backends 'company-tern))
;; Disable menu bar
(menu-bar-mode -1)
;; No bell
(setq ring-bell-function 'ignore)
;; Save cursor position
(save-place-mode t)
;; Indentation
(setq standard-indent 2)
(setq js-indent-level 2)
;; IDO (flx, vertical)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-vertical-show-count t)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(add-to-list 'ido-ignore-files "\\.DS_Store")
;; Rename current file
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)
;; Undo tree
(global-undo-tree-mode)
;; Theme
(load-local "atom-dark-theme.el")
;; Highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'json-mode-hook (lambda () (highlight-numbers-mode -1)))
;; Smart mode line
(setq sml/no-confirm-load-theme t)
(setq rm-blacklist '(" Undo-Tree" " s-/" " es" " yas" " Anzu" " Isearch" " company"))
(sml/setup)
;; Window number
(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode 1)
;; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-auto-cleanup 60)
(setq recentf-exclude '("/\\.git/.*\\'" "/elpa/.*\\'" ".*\\.gz\\'"))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; Keep recent files in sync across multiple emacs instances
(copy-function-definition 'recentf-save-list* 'recentf-save-list)

(defun recentf-save-list ()
  (let ((current-list recentf-list))
    (recentf-load-list)
    (setq recentf-list (append current-list recentf-list))
    (recentf-cleanup)
    (recentf-save-list*)))
;; Ack
(setq ack-command "ag ")
;; Input method
(setq default-input-method "MacOSX")
;; Add clipboard to kill ring
(setq save-interprogram-paste-before-kill t)
;; Web mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\.erb\\'" . markdown-mode))
(with-eval-after-load 'markdown-mode (setq markdown-open-command "~/bin/mark"))
;; FZF
(load-local "fzf.el")
;; Evil mode
(defface evil-normal-tag
  `((t (:foreground "#dfff00"))) ; Yellow
  "Evil normal mode indicator face")
(defface evil-insert-tag
  `((t (:foreground "#005fff"))) ; Blue
  "Evil insert mode indicator face")
(defface evil-visual-tag
  `((t (:foreground "#c82829"))) ; Red
  "Evil visual mode indicator face")
(defface evil-motion-tag
  `((t (:foreground "#FF73FD"))) ; Pink
  "Evil motion mode indicator face")
(defface evil-emacs-tag
  `((t (:weight bold :foreground "gray50"))) ; Gray
  "Evil emacs mode indicator face")
(setq evil-normal-state-tag (propertize " NORMAL" 'face 'evil-normal-tag))
(setq evil-operator-state-tag (propertize " NORMAL" 'face 'evil-normal-tag))
(setq evil-insert-state-tag (propertize " INSERT" 'face 'evil-insert-tag))
(setq evil-replace-state-tag (propertize " REPLACE" 'face 'evil-insert-tag))
(setq evil-visual-state-tag (propertize " VISUAL" 'face 'evil-visual-tag))
(setq evil-motion-state-tag (propertize " MOTION" 'face 'evil-motion-tag))
(setq evil-emacs-state-tag (propertize " EMACS" 'face 'evil-emacs-tag))
(setq evil-want-C-u-scroll t)
(setq evil-shift-width 2)
(setq evil-regexp-search nil)
(setq evil-move-cursor-back nil)
(require 'evil)
(evil-mode 1)
(insert-key "C-p" 'previous-line)
(insert-key "C-n" 'next-line)
(insert-key "C-a" 'beginning-of-line)
(insert-key "C-e" 'end-of-line)
(make-motion evil-forward-sexp sp-next-sexp)
(make-motion evil-backward-sexp sp-backward-sexp)
(evil-set-initial-state 'fundamental-mode 'emacs)
(add-hook 'git-commit-mode-hook 'evil-insert-state)
(evil-set-initial-state 'ensime-inspector-mode 'motion)
(evil-set-initial-state 'sbt-mode 'insert)
(evil-set-initial-state 'ack-mode 'motion)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'cider-docview-mode 'motion)
(evil-set-initial-state 'cider-repl-mode 'insert)
(evil-set-initial-state 'cider-stacktrace-mode 'motion)
(evil-set-initial-state 'cider-popup-buffer-mode 'motion)
(evil-declare-change-repeat 'company-complete)
(add-hook 'magit-blame-mode-hook (lambda () (toggle-evil magit-blame-mode)))
;; Visual star
(global-evil-visualstar-mode)
;; Leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "f" 'fzf
		     "o" 'smart-open-line
		     "O" 'smart-open-line-above
                     "b" 'switch-to-buffer
                     "d" 'duplicate-line-or-region
		     "a" 'ack-from-root
		     "n" 'show-file-name
		     "u" 'undo-tree-visualize
		     "w" 'evil-ace-jump-word-mode
		     "c" 'evil-ace-jump-char-mode
		     ;; Git
                     "gs" 'magit-status-window
                     "gh" 'github-browse-file
		     "gb" 'magit-blame
		     "gl" 'magit-log-buffer-file)
;; Dumb jump
(dumb-jump-mode)
(normal-key "gj" 'dumb-jump-go)
;; Args
(require 'evil-args)
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(motion-key "(" 'evil-backward-arg)
(motion-key ")" 'evil-forward-arg)
(visual-key "(" 'evil-backward-arg)
(visual-key ")" 'evil-forward-arg)
;; Buffers and windows
(normal-key "DEL" 'kill-this-buffer)
;; Forward buffer
(global-set-key (kbd "M-f") 'next-buffer)
;; Back buffer
(global-set-key (kbd "M-b") 'previous-buffer)
;; Enlarge window
(global-set-key (kbd "M-e") 'enlarge-window)
;; Shrink window
(global-set-key (kbd "M-s") 'shrink-window)
;; Grow window
(global-set-key (kbd "M-g") 'delete-other-windows)
;; Move buffer up
(global-set-key (kbd "M-u") 'buf-move-up)
;; Move buffer down
(global-set-key (kbd "M-d") 'buf-move-down)
;; Close buffer
(global-set-key (kbd "M-c") 'kill-this-buffer)
;; Comments
(evil-commentary-mode)
;; Anzu
(with-eval-after-load 'evil
  (require 'evil-anzu)
  (global-anzu-mode 1))
;; Drag stuff
(drag-stuff-mode t)
(normal-key "C-j" 'drag-stuff-down)
(normal-key "C-k" 'drag-stuff-up)
(visual-key "C-j" 'drag-stuff-down)
(visual-key "C-k" 'drag-stuff-up)
;; nmap Y y$
(normal-key "Y" 'copy-to-end-of-line)
(motion-key "Y" 'copy-to-end-of-line)
;; Buffers
(global-set-key (kbd "C-l") 'evil-buffer)
;; JSON
(load-local "json-mode.el")
;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)
;; Split and focus
(normal-key "_" (split-and-focus split-window-vertically))
(normal-key "|" (split-and-focus split-window-horizontally))
;; Rust
(setq racer-cmd "/Users/dsargeant/.cargo/bin/racer")
(setq racer-rust-src-path "/Users/dsargeant/Projects/rustc-1.8.0/src")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
;; Clojure
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'evil-smartparens-mode)
(with-eval-after-load 'clojure-mode
  (load-local "clojure.el")
  (make-motion evil-forward-sexp-clojure clojure-forward-logical-sexp)
  (make-motion evil-backward-sexp-clojure clojure-backward-logical-sexp)
  (evil-define-key 'motion clojure-mode-map
    (kbd "(") 'evil-backward-sexp-clojure
    (kbd ")") 'evil-forward-sexp-clojure)
  (evil-define-key 'visual clojure-mode-map
    (kbd "(") 'evil-backward-sexp-clojure
    (kbd ")") 'evil-forward-sexp-clojure)
  (define-key clojure-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)
  (define-key clojure-mode-map (kbd "M-}") 'sp-forward-barf-sexp)
  (define-key clojure-mode-map (kbd "M-(") 'sp-backward-slurp-sexp)
  (define-key clojure-mode-map (kbd "M-{") 'sp-backward-barf-sexp)
  (setq cider-repl-use-pretty-printing t)
  (normal-key "{" 'sp-backward-up-sexp)
  (normal-key "}" 'sp-up-sexp)
  (visual-key "{" 'sp-backward-up-sexp)
  (visual-key "}" 'sp-up-sexp)
  (define-key clojure-mode-map (kbd "C-c C-f") 'cider-format-buffer))
(evil-leader/set-key-for-mode 'clojure-mode
  ;; Raise sexp
  "r" 'sp-raise-sexp
  ;; Split sexp
  "s" 'sp-split-sexp
  ;; Kill sexp
  "k" 'kill-sexp
  ;; Jump to definition
  "j" 'cider-find-var
  ;; Lookup symbol in docs
  "l" 'cider-doc
  ;; Evaluate buffer
  "e" 'cider-load-buffer)
;; Ensime
(with-eval-after-load 'ensime
  (setq ensime-sem-high-enabled-p nil)
  (setq ensime-left-margin-gutter nil))
(evil-leader/set-key-for-mode 'scala-mode
  ;; Inspect type
  "i" 'ensime-inspect-type-at-point
  ;; Errors and warnings
  "e" 'ensime-show-all-errors-and-warnings
  ;; References of symbol
  "r" 'ensime-show-uses-of-symbol-at-point
  ;; Rename variable
  "sn" 'ensime-refactor-diff-rename
  ;; Jump to definition
  "j" 'ensime-edit-definition
  ;; Lookup symbol in docs
  "l" 'ensime-show-doc-for-symbol-at-point
  ;; Type at point
  "t" 'ensime-type-at-point)
;; Format scala source
(with-eval-after-load 'scala-mode
  (define-key scala-mode-map (kbd "C-c C-f") 'ensime-format-source))
;; Find function (elisp)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "j" 'find-function)
;; Tern
(evil-leader/set-key-for-mode 'js-mode "j" 'tern-find-definition)
(evil-leader/set-key-for-mode 'js-mode "t" 'tern-get-type)
;; Format markdown (tidy)
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-f") 'tidy-markdown))
;; Go
(add-hook 'go-mode-hook
  (lambda ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t)
    (define-key go-mode-map (kbd "C-c C-f") 'gofmt)
    (evil-leader/set-key-for-mode 'go-mode "j" 'godef-jump)
    (evil-leader/set-key-for-mode 'go-mode "t" 'godef-describe)
    (evil-leader/set-key-for-mode 'go-mode "l" 'godoc-at-point)
    (evil-leader/set-key-for-mode 'go-mode "ia" 'go-import-add)
    (evil-leader/set-key-for-mode 'go-mode "ir" 'go-remove-unused-imports)
    (setq-default)
    (setq tab-width 2)
    (setq standard-indent 2)
    (setq indent-tabs-mode nil)
    (set (make-local-variable 'company-backends) '(company-go))))
;; Elm
(add-hook 'elm-mode-hook
  (lambda ()
    (elm-oracle-setup-completion)
    (evil-leader/set-key-for-mode 'elm-mode "t" 'elm-oracle-type-at-point)
    (evil-leader/set-key-for-mode 'elm-mode "l" 'elm-oracle-doc-at-point)))
(with-eval-after-load 'company (add-to-list 'company-backends 'company-elm))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-visualstar elm-mode projectile yaml-mode company-go dumb-jump cider use-package-chords use-package dockerfile-mode gitignore-mode gitconfig-mode go-mode toml-mode racer evil-anzu rainbow-delimiters elixir-mode window-number clojure-mode smex evil-smartparens smartparens company-tern github-browse-file web-mode markdown-mode ace-jump-mode buffer-move drag-stuff csharp-mode browse-kill-ring fsharp-mode evil-args evil-commentary ack evil-surround smart-mode-line highlight-numbers evil-leader ido-vertical-mode flx-ido evil ensime undo-tree magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "grey55"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#b0b1a3"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "grey55"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#b0b1a3"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "grey55"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#b0b1a3"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "grey55"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#b0b1a3"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "grey55"))))
 '(sml/position-percentage ((t (:foreground "gray50" :weight normal)))))
