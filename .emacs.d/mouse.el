; Mouse in terminal
(xterm-mouse-mode 1)
;; Yank
(setq mouse-yank-at-point t)
;; Enable scrolling
(global-set-key [mouse-4] '(lambda ()
                             (interactive)
                             (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
                             (interactive)
                             (scroll-up 1)))
