(require 'clojure-mode)

;; Key bindings
(global-set-key (kbd "M-SPC") 'mark-sexp)

(defvar clojure-keyword-vars
  '("defn$" "defn'" "fna" "fnv" "fn'" "fn$"))

;; rk-annotate keywords vars
(font-lock-add-keywords 'clojure-mode
                        `((,(concat "(\\(?:\.*/\\)?"
                                    (regexp-opt clojure-keyword-vars t)
                                    "\\>")
                           1 font-lock-keyword-face)))

;; Function name support for defn$
(font-lock-add-keywords 'clojure-mode
                        `((,(concat "(\\(?:[a-z\.-]+/\\)?\\(defn\\$\\)"
                                    ;; Function declarations
                                    "\\>"
                                    ;; Any whitespace
                                    "[ \r\n\t]*"
                                    ;; Possibly type or metadata
                                    "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                                    "\\(\\sw+\\)?")
                           2 font-lock-function-name-face)))

;; Indentation
(define-clojure-indent
  (defroutes 'defun)
  (defrouter 'defun)
  (GET 'defun)
  (POST 'defun)
  (PUT 'defun)
  (DELETE 'defun)
  (GET-int 'defun)
  (GET-date 'defun)
  (GET-uuid 'defun)
  (GET-slug 'defun)
  (match-int 'defun)
  (match-date 'defun)
  (match-uuid 'defun)
  (match-slug 'defun)
  (match-regex 'defun)
  (request 'defun)
  (headers 'defun)
  (body 'defun)
  (pred 'defun)
  (path 1)
  (param 'defun)
  (http 'defun)
  (https 'defun)
  (truncate 'defun)
  (index 'defun)
  (domain 1)
  (port 1)
  (guard 1)
  (remote-address 1)
  (let-routes 1)
  (state-routes 1)
  (maybe-routes 1)
  (valid 1)
  (invalid 1)
  (typecheck 1)
  (fact 1)
  (facts 1)
  (describe 1)
  (it 1)
  (context 1)
  (state-> 'defun)
  (maybe-> 'defun)
  (either-> 'defun)
  (stateE-> 'defun)
  (stateM-> 'defun)
  (stateW-> 'defun)
  (stateWE-> 'defun)
  (stateWM-> 'defun)
  (reader-> 'defun)
  (readerE-> 'defun)
  (readerM-> 'defun)
  (readerW-> 'defun)
  (readerWE-> 'defun)
  (readerWM-> 'defun)
  (writer-> 'defun)
  (writerE-> 'defun)
  (writerM-> 'defun)
  (statefn 'defun)
  (execute 'defun)
  (match 'defun)
  (fn$ 1)
  (fna 1)
  (fnv 1)
  (fn' 1)
  (forever 'defun)
  (div 'defun)
  (ul 'defun)
  (li 'defun)
  (span 'defun)
  (table 'defun)
  (tr 'defun)
  (td 'defun)
  (select 'defun)
  (button 'defun)
  (textarea 'defun))
