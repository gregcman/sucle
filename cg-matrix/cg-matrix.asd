(asdf:defsystem #:cg-matrix
  :description "sb-cga but with destructive matrix operations"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on (
#:defpackage-plus
#:sb-cga
)

  :serial t
  :components  
  ((:file "package")
   (:file "cg-matrix")))
