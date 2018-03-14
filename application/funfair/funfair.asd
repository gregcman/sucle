(asdf:defsystem #:funfair
  :depends-on (
	       #:funland	       
	       #:opengl-glfw3
	       #:cl-program
	       #:nsb-cga
	       #:bordeaux-threads
	       #:cl-opengl
	       #:music
	       #:glhelp)
  :serial t
  :components
  ((:file "deflazy")
   (:file "funfair")))

