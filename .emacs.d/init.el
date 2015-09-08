(defvar emacs-dir "~/.emacs.d/")

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
;; No bell
(setq ring-bell-function 'ignore)
;; IDO
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
;; Undo tree
(global-undo-tree-mode)
;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(defun magit-status-window ()
  (magit-status)
  (delete-other-windows))
;; Theme
(load-local "atom-dark-theme.el")
(setq sml/no-confirm-load-theme t)
(sml/setup)
;; Mouse
(load-local "mouse.el")
;; OSX
(load-local "osx.el")
