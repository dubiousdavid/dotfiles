(defmacro make-motion (name fun)
  `(evil-define-motion ,name (count)
     :type inclusive
     (dotimes (i (or count 1))
       (,fun))))

(make-motion evil-forward-sexp paredit-forward)
(make-motion evil-backward-sexp paredit-backward)
