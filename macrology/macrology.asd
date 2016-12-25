(asdf:defsystem #:macrology
  :description "general purpose macros and code generation facilities"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on ()

  :serial t
  :components  
  ((:file "package")
   (:file "macrology")
   (:file "code-generation")))
