(asdf:defsystem #:glinfo
  :description "get gl internals, like sizes of types and constant values"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on (#:cl-opengl)

  :serial t
  :components  
  ((:file "package")
   (:file "glinfo")))
