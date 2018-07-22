;;;; cl-uml.asd

(asdf:defsystem #:cl-uml
  :description "Describe cl-uml here"
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-xcb-xlib #:cl-cairo2-xcb #:cl-cairo2)
  :components ((:file "package")
               (:file "cl-uml")))
