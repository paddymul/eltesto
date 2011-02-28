
(defvar paddy-py-django-manage-command 
  "cd  ~/permalink/ ; source ~/permalink/env_local.sh ; source ~/permalink/env.sh ;  python ~/permalink/permalink/manage.py  "
  "the command to get to your manage.py to run django commands,
  this is a great variable to override in dir-locals")

;(defun run-django-command (command-str)
;  paddy
;"test  --noinput proxy.WorkingTestCase" t))

(defun paddy-py-test-django-working ()
  "call the unit_testing framework on the current file"
  (interactive)
  (paddy-compile "cd  ~/permalink/ ; source ~/permalink/env.sh ;  python ~/permalink/permalink/manage.py  test  --noinput proxy.WorkingTestCase" t))


(def-paddy-compile-command  paddy-py-test-integration 
  "run all unit tests located within
  unit_testing.integration_tests.test_all_roots
  "
  paddy-py-integration-test-command)



(defun paddy-py-test-django-full ()
  "call the unit_testing framework on the current file"
  (interactive)
  (paddy-compile "cd  ~/permalink/ ; source ~/permalink/env.sh ;  python ~/permalink/permalink/manage.py  test  --noinput proxy" t))



(defun dj-sv ()
  "start django development server in a compile buffer "
  (interactive)
  (let ((old-compilation-arguments compilation-arguments))

    (if (get-buffer "dj-sv")
        (pop-to-buffer "dj-sv")
      (progn
        (pop-to-buffer 
         (progn
           (setq compilation-arguments t)
           (compile 
            "cd  /Users/patrickmullen/qrgit/code/exit_project_django ; /Library/Frameworks/Python.framework/Versions/2.6/bin/python dev-manage.py runserver 0.0.0.0:8000" t)))
        (setq compilation-arguments old-compilation-arguments)
        (rename-buffer "dj-sv")))))

(defun tc-dj-sv ()
  "start django development server in a compile buffer "
  (interactive)
  (let ((old-compilation-arguments compilation-arguments))

    (if (get-buffer "tc-dj-sv")
        (pop-to-buffer "tc-dj-sv")
      (progn
        (pop-to-buffer 
         (progn
           (setq compilation-arguments t)
           (compile 
            "cd  /Users/patrickmullen/dj_terminalcast ; /Library/Frameworks/Python.framework/Versions/2.6/bin/python manage.py runserver 0.0.0.0:8000" t)))
        (setq compilation-arguments old-compilation-arguments)
        (rename-buffer "tc-dj-sv")))))

