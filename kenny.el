;; Paddy Mullen
;; paddy@chartwidget.com
;; http://paddymullen.com


;; I am trying to write a macro defining macro in elisp

;;; I want code that does this

(make-interactive-command-generator foo-something
   (compile (concat (format "cd %s;  foo " (get-cwd)) ,@body)))

; to expand to

(defmacro foo-something (foo-name &rest body)
  `(defun ,foo-name nil
     (interactive)
     (compile (concat (format "cd %s;  foo " (get-cwd)) ,@body))))

;; thus make-interactive-command-generator must be a macro defining macro

;; now to simplify

(defmacro make-interactive-command-generator (macro-name  &rest macro-rest)
  "defines an interactive command creating macro"
  `(defmacro  ,macro-name (name &rest interactive-body)
     `(defun ,name nil
        (interactive)
        ,,@macro-rest)))

;; running
(macroexpand 
 '(make-interactive-command-generator
   def-paddy-compile-command
   (paddy-compile ,@interactive-body)))
;; yields 
;; (defmacro def-paddy-compile-command
;;  (name &rest interactive-body)
;;  `(defun ,name nil
;;     (interactive)
;;     ,(paddy-compile \,@interactive-body)))

;;;; notice the , before (paddy-compile
;;;; so let's try to remove a comma

(defmacro make-interactive-command-generator2 (macro-name  &rest macro-rest)
  "defines an interactive command creating macro"
  `(defmacro  ,macro-name (name &rest interactive-body)
     `(defun ,name nil
        (interactive)
        ,@macro-rest)))
(macroexpand 
 '(make-interactive-command-generator2
   def-paddy-compile-command
   (paddy-compile ,@interactive-body)))
;; yields
;; (defmacro def-paddy-compile-command  
;;   (name &rest interactive-body)      
;;   `(defun ,name nil                  
;;      (interactive)                   
;;      ,@macro-rest))                  

;;;; notice macro-rest wasn't properly expanded


(defmacro make-interactive-command-generator3 (macro-name  &rest macro-rest)
  "defines an interactive command creating macro"
  (let ((inner-macro 
         (quote 
          `(defun ',name nil
             (interactive)
             ,@macro-rest))))
  `(defmacro  ,macro-name (name &rest interactive-body)
     ,@inner-macro)))

(macroexpand 
 '(make-interactive-command-generator3
   def-paddy-compile-command
   (paddy-compile ,@interactive-body)))

;; yields 
;;(defmacro def-paddy-compile-command
;;  (name &rest interactive-body)
;;  \`
;;  (defun
;;    '(\, name)
;;    nil
;;    (interactive)
;;    (\,@ macro-rest)))
;; notice that macro-rest wasn't expanded


(defmacro make-interactive-command-generator4 (macro-name  &rest macro-rest)
  "defines an interactive command creating macro"
  (let ((inner-macro 
          `(defun ',name nil
             (interactive)
             ,@macro-rest)))

  `(defmacro  ,macro-name (name &rest interactive-body)
     ,` ,@inner-macro)))
(macroexpand 
 '(make-interactive-command-generator4
   def-paddy-compile-command
   (paddy-compile ,@interactive-body)))

;; throws an error



;; my full macro
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
