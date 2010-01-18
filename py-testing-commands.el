
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
       (paddy-compile ,@body-without-docstring t))))

;;;; make def-paddy-compile-command calls look like lisp builtins 
(font-lock-add-keywords 'emacs-lisp-mode
  '(("( *\\(def-paddy-compile-command\\) *\\([-a-zA-Z]*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))))

(defvar paddy-py-test-bin-directory
  "/Library/Frameworks/Python.framework/Versions/2.6/bin/" 
  "the directory that contains user installed python scripts")


(defvar paddy-py-test-single-file-test-command
  (format "%s%s" paddy-py-test-bin-directory "qr_py_tf")
  "the path to the command that tests a single python file, it
  should be a command that accepts a single argument of the full
  path to that python file to be tested " )

(def-interactive  paddy-py-test-current-file  ()
  "call the unit_testing framework on the current file"
  (paddy-compile (format "source ~/permalink/env.sh ; %s %s" 
             paddy-py-test-single-file-test-command buffer-file-name)t))

(if nil (progn 
(defvar paddy-py-test-tree-command
  (format  "%s%s" paddy-py-test-bin-directory "qr_py_tt")
  "the path to the command that searches up the tree until it
  finds the top most __init__.py, collects and then runs all the
  unit tests")

(defvar paddy-py-integration-test-command
   "the command that runs your entire test tree, this is a great
    variable to override with a .dir-locals file"
   " bash -c '/Library/Frameworks/Python.framework/Versions/2.6/bin/python ~/qrgit/qr_site/qr_site/util/unit_testing/integration_tests.py'")


;'(def-paddy-compile-command  paddy-py-test-current-file 
;  "call the unit_testing framework on the current file"
;  (format "source ~/permalink/env.sh ; %s %s" 
;             paddy-py-test-single-file-test-command buffer-file-name)))

(defun paddy-py-test-current-file ()
  "call the unit_testing framework on the current file"
  (interactive)
  (paddy-compile (format "source ~/permalink/env.sh ; %s %s" 
             paddy-py-test-single-file-test-command buffer-file-name)t))
  
(def-paddy-compile-command paddy-py-test-current-tree 
  "look at the current file, continue going up directories until
  one without a __init__.py is found, then add all subdirectories
  to test suite and run tests
  "
   (format "%s %s"  
           paddy-py-test-tree-command buffer-file-name))
(def-paddy-compile-command  paddy-py-test-integration 
  "run all unit tests located within
  unit_testing.integration_tests.test_all_roots
  "
  paddy-py-integration-test-command)

))