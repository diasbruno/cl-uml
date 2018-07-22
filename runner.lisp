(progn (push (car (directory ".")) ql:*local-project-directories*)
       (push (car (directory "../cl-xcb-xlib")) ql:*local-project-directories*)
       (push (car (directory "../cl-cairo2")) ql:*local-project-directories*)
       (ql:quickload :cl-uml))
