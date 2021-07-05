;;;; metztli.asd

(asdf:defsystem #:metztli
  :description "Describe metztli here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:yaclml)
  :components ((:file "package")
               (:file "metztli")))
