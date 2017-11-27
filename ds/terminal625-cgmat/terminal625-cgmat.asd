(asdf:defsystem #:terminal625-cgmat
  :description "sb-cga but with destructive matrix operations"
  :depends-on (
#:defpackage-plus
#:sb-cga
)

  :serial t
  :components  
  ((:file "package")
   (:file "cg-matrix")))
