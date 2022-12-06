;;;; day5.asd

(asdf:defsystem #:day5
  :description "Describe day5 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "day5")))
