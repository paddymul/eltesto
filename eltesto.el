(require 'compile)

(defvar paddy-compile-history '()
  " the list of previous compile commands ")
  
(defun paddy-add-ordered-uniq (el lst)
  (append (list el) (remove el lst)))

(assert  (equal (paddy-add-ordered-uniq 'a '(b c a))  '(a b c)))
(assert  (equal (paddy-add-ordered-uniq 'c '(b c a))  '(c b a)))
(assert  (equal (paddy-add-ordered-uniq 'd '(b c a))  '(d b c a)))
(assert  (equal (paddy-add-ordered-uniq 'b '(b c a))  '(b c a)))


(defun paddy-add-to-compile-list (compile-args)
  (setq paddy-compile-history (paddy-add-ordered-uniq compile-args paddy-compile-history)))


(defun paddy-compile-actual (compile-command-str &optional comint directory)
  (compile (format "cd %s; %s" default-directory compile-command-str) comint))

(defun paddy-compile (compile-command-str &optional comint directory)
  """ a wrapper for paddy-compile-actual adds the compile command to the compile history """
  (let ((default-directory (or directory (file-name-directory buffer-file-name))))
    (paddy-add-to-compile-list
     (list compile-command-str t default-directory ))
    (paddy-compile-actual 
     compile-command-str t default-directory )))

(defun paddy-recompile ()
  (interactive)
  (apply 'paddy-compile-actual (car paddy-compile-history)))


