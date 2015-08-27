(require 'find-file-in-project)

(setq ffip-limit 2048)
(setq ffip-patterns (append ffip-patterns '("*.cljs" "*.scss" ".css" "*.java" "*.dtm" "*.edn")))

(global-set-key (kbd "C-x f") 'find-file-in-project)
