(asdf:defsystem #:application
  :depends-on (#:funland	       
	       #:opengl-glfw3
	       #:application-subsystem
	       #:nsb-cga
	       #:bordeaux-threads
	       #:cl-opengl
	       #:music
	       #:glhelp)
  :components
  ((:file "application")))

