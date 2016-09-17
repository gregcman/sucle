(asdf:defsystem #:sandbox
  :description "when parts come together"
  :version "0.0.0"
  :author "a little boy in the sand"
  :maintainer "tain mainer"
  :licence "fuck that shit"

  :depends-on (#:cl-opengl
               #:lispbuilder-sdl
               #:cl-utilities
	       #:chipz
	       #:opticl)

  :serial t
  :components  
  ((:file "package")
   (:file "window")
   (:file "magic")
   (:file "mat")
   (:file "mesh")
   (:file "render")
   (:file "nbt")
   (:file "blocks")
   (:file "chunk")
   (:file "chunk-meshing")
   (:file "lovely-renderer")
   (:file "world-physics")
   (:file "sandbox")))
