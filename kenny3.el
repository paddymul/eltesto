(defmacro make-interactive-command-5 (macro-name macro-docstr &rest macro-rest)
  "defines a compilation command and adds emacs syntax highlighting to the command "

  ;; the following form adds syntax highlighting for the macro about to be created
  ;; this runs at first expansion time
  (font-lock-add-keywords 'emacs-lisp-mode
                          '((
                             (format
                              "( *\\(%s\\) *\\([-a-zA-Z]*\\)" macro-name)
                             (1 font-lock-keyword-face)
                             (2 font-lock-function-name-face))))

  `(defmacro  ,macro-name (name &rest interactive-body)
     ,macro-docstr
     ;; here I pull apart the interactive-body form, looking for the docstring
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


;; first macro that I am trying to write
(font-lock-add-keywords 'emacs-lisp-mode
                        '((
                           (format
                            "( *\\(%s\\) *\\([-a-zA-Z]*\\)" "def-interactive")
                           (1 font-lock-keyword-face)
                           (2 font-lock-function-name-face))))

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

;; second macro that I am want
(font-lock-add-keywords 'emacs-lisp-mode
                        '((
                           (format
                            "( *\\(%s\\) *\\([-a-zA-Z]*\\)" "def-paddy-compile-command")
                           (1 font-lock-keyword-face)
                           (2 font-lock-function-name-face))))


(defmacro def-paddy-compile-command (name &rest compile-form)
           
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


;; third  macro that I want
(font-lock-add-keywords 'emacs-lisp-mode
                        '((
                           (format
                            "( *\\(%s\\) *\\([-a-zA-Z]*\\)" "def-paddy-django-test-command")
                           (1 font-lock-keyword-face)
                           (2 font-lock-function-name-face))))


(defmacro def-paddy-django-test-command (name &rest compile-form)
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
       (paddy-compile 
        (concat 
         (format "%s ; python manage.py test --noinput " python-environment-pre-testing-command)
                 ,@body-without-docstring)))))



;;;; now these may seem like slightly contrived examples, -- why don't
;;;; people just write out the whole defun themselves, well for a
;;;; couple of reasons, 
;;;;
;;;; 1. I think that is a bit ugly
;;;;
;;;; 2. two I am trying to make a novice unduser package, I want to
;;;; hide as much of the guts and accidental complexity as possible.
;;;; 
;;;; 3. I might later want to register all commands of type
;;;; def-paddy-django-test-command so that they are easivly vissible
;;;; in a command browser

(print '(:foo 'bar))