(setq point-one "asdf")
(setq point-two "zxcv")
(setq list-1 (list point-one point-two))
(setq point-one "1234")
(assert (equal list-1 '("asdf" "zxcv")))


(defmacro def-interactive (name args &rest body)
  "automatically inserts a call to (interactive) into the
function body"
  (let
      ((docstring
        (if (and
             (eq (type-of (car body)) 'string)
             (cdr body))
            (car body)
          nil))
       (body-without-docstring
        (if  (and
             (eq (type-of (car body)) 'string)
             (cdr body))
            (cdr body)
          body)))
    `(defun ,name ,args
       ,docstring
       (interactive)
       ,@body-without-docstring)))
(defmacro def-paddy-compile-command (name &rest compile-form)
  """allows a compile-command to be defined like
     (def-paddy-compile-command  runfile
       "docstring"
       (format "python %s" buffer-file-name))
     (defun  runfile ()
       "docstring"
       (interactive)
       (paddy-compile
        (format "python %s" buffer-file-name) t))
"""
           
  (let
      ((docstring
        (if (and
             (eq (type-of (car compile-form)) 'string)
             (cdr compile-form))
            (car compile-form)
          nil))
       (body-without-docstring
        (if  (and
             (eq (type-of (car compile-form)) 'string)
             (cdr compile-form))
            (cdr compile-form)
          compile-form)))
    `(defun ,name nil
       ,docstring
       (interactive)
       (paddy-compile ,@body-without-docstring))))


(defmacro make-interactive-command (macro-name macro-docstr &rest macro-rest)
  "defines a compilation command and adds emacs syntax highlighting to the command "

  (font-lock-add-keywords 'emacs-lisp-mode
                          '((
                             (format
                              "( *\\(%s\\) *\\([-a-zA-Z]*\\)" macro-name)
                             (1 font-lock-keyword-face)
                             (2 font-lock-function-name-face))))

  `(defmacro  ,macro-name (name &rest interactive-body)
     ,macro-docstr
     (let
         ((docstring
           (if (and
                (eq (type-of (car interactive-body)) 'string)
                (cdr interactive-body))
               (car interactive-body)
             nil))
          (body-without-docstring
           (if  (and
                 (eq (type-of (car interactive-body)) 'string)
                 (cdr interactive-body))
               (cdr interactive-body)
             interactive-body)))
       (list `(defun ,name nil
          ,docstring
          (interactive)
          ',,@macro-rest)))))

(pp (macroexpand 
     '(make-interactive-command 
       def-paddy-compile-command
       "docstring for def-paddy-compile-command"
       (paddy-compile ,@body-without-docstring))))
(make-interactive-command 
       def-paddy-compile-command
       "docstring for def-paddy-compile-command"
       (paddy-compile ,@body-without-docstring))

(pp (macroexpand '(def-paddy-compile-command
                     "paddy-compile-command docstring"
                     "base-comp-str pwd; ls; cd")))


(append '(foo bar baz (boop)) '"quake")

(cons '`(defun ,name nil ,docstring
         (interactive))
      '("foo"))
  ,((paddy-compile \,@body-without-docstring)))))


(make-interactive-command 
       def-paddy-compile-command
       "docstring for def-paddy-compile-command"
       (paddy-compile ,@body-without-docstring))

(make-interactive-command
      run-django-command
      "runs a manage.py command"
      (paddy-compile (format "%s ; "))))
