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
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))
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
  (tern-mode t)
  (company-mode t))
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
;; Use these package archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
;; Initialize packages
(setq package-enable-at-startup nil)
(package-initialize)
;; Smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
;; No splash screen
(setq inhibit-splash-screen t)
;; Manual mode selection
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
;; Line numbers
(global-linum-mode t)
(setq linum-format "%4d ")
;; Auto close brackets
;; (electric-pair-mode 1)
;; Highlight matching bracket
(show-paren-mode 1)
;; Smartparens
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))
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
(global-set-key (kbd "C-x C-o") 'company-complete)
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
(setq rm-blacklist '(" Undo-Tree" " s-/" " es" " yas" " Anzu"))
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
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\.erb\\'" . markdown-mode))
(with-eval-after-load 'markdown-mode (setq markdown-open-command "~/bin/mark"))
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
(make-motion evil-forward-sexp sp-next-sexp)
(make-motion evil-backward-sexp sp-backward-sexp)
(evil-set-initial-state 'fundamental-mode 'emacs)
(add-hook 'git-commit-mode-hook 'evil-insert-state)
(evil-set-initial-state 'ensime-inspector-mode 'motion)
(evil-set-initial-state 'sbt-mode 'insert)
(evil-set-initial-state 'ack-mode 'motion)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-declare-change-repeat 'company-complete)
;; Leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "f" 'projectile-find-file
		     "o" 'smart-open-line
		     "O" 'smart-open-line-above
                     "b" 'switch-to-buffer
                     "g" 'magit-status-window
                     "h" 'github-browse-file
                     "d" 'duplicate-line-or-region
		     "a" 'ack-from-root
		     "u" 'undo-tree-visualize
		     "w" 'evil-ace-jump-word-mode
		     "c" 'evil-ace-jump-char-mode)
;; Args
(require 'evil-args)
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(motion-key "(" 'evil-backward-arg)
(motion-key ")" 'evil-forward-arg)
(visual-key "(" 'evil-backward-arg)
(visual-key ")" 'evil-forward-arg)
;; Sexp motions
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
(global-set-key (kbd "M-r") 'sp-raise-sexp)
(global-set-key (kbd "C-a") 'beginning-of-defun)
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
;; Paste pop
(normal-key "C-p" 'paste-pop)
(normal-key "C-n" 'paste-pop-next)
;; nmap Y y$
(normal-key "Y" 'copy-to-end-of-line)
;; Buffers
(global-set-key (kbd "C-l") 'evil-buffer)
;; JSON
(with-eval-after-load 'json-mode
  (define-key json-mode-map (kbd "C-c C-f") 'json-pretty-print-buffer))
;; Clojure
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'evil-smartparens-mode)
(with-eval-after-load 'clojure-mode
  (make-motion evil-forward-sexp-clojure clojure-forward-logical-sexp)
  (make-motion evil-backward-sexp-clojure clojure-backward-logical-sexp)
  (evil-define-key 'motion clojure-mode-map
    (kbd "(") 'evil-backward-sexp-clojure
    (kbd ")") 'evil-forward-sexp-clojure)
  (evil-define-key 'visual clojure-mode-map
    (kbd "(") 'evil-backward-sexp-clojure
    (kbd ")") 'evil-forward-sexp-clojure)
  (evil-define-key 'motion clojure-mode-map
    (kbd "{") 'evil-backward-section-begin
    (kbd "}") 'evil-forward-section-begin)
  (evil-define-key 'visual clojure-mode-map
    (kbd "{") 'evil-backward-section-begin
    (kbd "}") 'evil-forward-section-begin))
;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)
;; Split and focus
(normal-key "_" (split-and-focus split-window-vertically))
(normal-key "|" (split-and-focus split-window-horizontally))
;; Ensime
(with-eval-after-load 'ensime (setq ensime-sem-high-enabled-p nil))
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
  ;; Lookup symbol in docs
  "l" 'ensime-show-doc-for-symbol-at-point
  ;; Type at point
  "t" 'ensime-type-at-point
  ;; SBT
  "s" 'ensime-inf-switch)
;; Format scala source
(add-hook 'scala-mode-hook
  (lambda () (define-key scala-mode-map (kbd "C-c C-f") 'ensime-format-source)))
;; Find function (elisp)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "j" 'find-function)
;; Tern
(evil-leader/set-key-for-mode 'js-mode "j" 'tern-find-definition)
(evil-leader/set-key-for-mode 'js-mode "t" 'tern-get-type)
;; Markdown
(evil-leader/set-key-for-mode 'markdown-mode "t" 'tidy-markdown)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-anzu rainbow-delimiters elixir-mode fzf window-number clojure-mode smex evil-smartparens smartparens company-tern json-mode github-browse-file web-mode markdown-mode ace-jump-mode buffer-move drag-stuff csharp-mode browse-kill-ring projectile fsharp-mode evil-args evil-commentary ack evil-surround smart-mode-line highlight-numbers evil-leader ido-vertical-mode flx-ido evil ensime undo-tree magit))))
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
