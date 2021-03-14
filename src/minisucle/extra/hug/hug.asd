;;;; hug.asd

(asdf:defsystem #:hug
  :description "Describe hug here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on
  (#:alexandria
   #:py4cl2)
  :components ((:file "package")
               (:file "hug")))
