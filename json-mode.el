;;; json-mode.el --- Major mode for editing JSON files

(require 'json)

(defvar json-key-face 'json-key-face
  "Face used to highlight JSON keys.")

(defface json-key-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight JSON numbers.")

(defvar json-keyword-face 'json-keyword-face
  "Face used to highlight JSON keywords.")

(defface json-keyword-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight JSON numbers.")

(defvar json-number-face 'json-number-face
  "Face used to highlight JSON numbers.")

(defface json-number-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight JSON numbers.")

(defconst json-mode-key-re
  (rx (and (group (char ?\") (0+ (not (any ?\"))) (char ?\"))
	   (0+ space) (char ?\:))))

(defconst json-mode-string-re
  (rx (and (char ?\") (0+ (not (any ?\"))) (char ?\"))))

(defconst json-mode-keyword-re
  (rx (and (or "true" "false" "null"))))

(defconst json-mode-number-re
  (rx (and (optional ?\-) (1+ digit) (optional ?\. (1+ digit)))))

(defconst json-font-lock
  (list (list json-mode-key-re 1 json-key-face)
	(list json-mode-keyword-re 0 json-keyword-face)
	(list json-mode-string-re 0 font-lock-string-face)
	(list json-mode-number-re 0 json-number-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.tern-project$" . json-mode))

;;;###autoload
(define-derived-mode json-mode js-mode "JSON"
  "Major mode for editing JSON"
  (set (make-local-variable 'font-lock-defaults) '(json-font-lock t)))

(define-key json-mode-map (kbd "C-c C-f") 'json-pretty-print-buffer)

(provide 'json-mode)
;;; json-mode.el ends here
