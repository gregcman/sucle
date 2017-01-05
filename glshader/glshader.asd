(asdf:defsystem #:glshader
  :description "opengl shaders and uniforms"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on (#:cl-opengl)

  :serial t
  :components  
  ((:file "package")
   (:file "glshader")))
