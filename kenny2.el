
(defmacro make-interactive-command-generator4 (macro-name  &rest macro-rest)
  "defines an interactive command creating macro"
  (let ((inner-macro 
          `(defun ',name nil
             (interactive)
             ,@macro-rest)))

  `(defmacro  ,macro-name (name &rest interactive-body)
     ,` ,@inner-macro)))
(pp (macroexpand 
     '(make-interactive-command-generator4
       def-paddy-compile-command
       (paddy-compile ,@interactive-body))))

;; lets try to make a defmacro that expands anything into ,@

(defmacro foo (name &rest forms)
  `(,name ,@forms))

(pp (macroexpand '(foo bar (list (,@baz)))))
;; (foo bar
;;     (list
;;      (\,@baz)))

(defmacro foo2 (name &rest forms)
  `(,name \,@forms))

(pp (macroexpand '(foo2 bar (list (,@baz)))))
;; (bar \,@forms)

(progn 
  (let ((quaf (list '(+ 3 2))))
  (let ((bar `('baz ,@quaf)))
    (pp (macroexpand `(foo ,@bar))))))
;(foo 'baz
;     (+ 3 2))



(progn 
  (let ((quaf (list '(+ 3 2))))
  (let ((bar `('baz ',@quaf)))
    (pp (macroexpand `(foo ,`,@bar))))))
;(foo 'baz
;     (+ 3 2))


(defmacro make-interactive-command-generator (macro-name  &rest macro-rest)
  "defines an interactive command creating macro"
  `(defmacro  ,macro-name (name &rest interactive-body)
     `(defun ,name nil
        (interactive)
        ,,@macro-rest)))

(pp (macroexpand 
     '(make-interactive-command-generator
       def-paddy-compile-command
       (paddy-compile ,@interactive-body))))
(make-interactive-command-generator
       def-paddy-compile-command
       (paddy-compile ,@interactive-body))
(pp (macroexpand 
     '(def-paddy-compile-command foo
        (format " foo bar "))))