(asdf:defsystem #:recycler
  :description "recycle arrays and memory and stuff"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on ()

  :serial t
  :components  
  ((:file "package")
   (:file "recycler")))
