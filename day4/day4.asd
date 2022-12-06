;;;; day4.asd

(asdf:defsystem #:day4
  :description "Describe day4 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "day4")))
