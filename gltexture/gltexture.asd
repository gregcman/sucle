(asdf:defsystem #:gltexture
  :description "opengl texture functions"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on (#:cl-opengl
               #:glinfo)

  :serial t
  :components  
  ((:file "package")
   (:file "gltexture")))
