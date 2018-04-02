(asdf:defsystem #:application
  :depends-on (#:utility
	       #:opengl-glfw3
	       #:application-subsystem
	       #:nsb-cga
	       #:bordeaux-threads
	       #:cl-opengl
	       #:music
	       #:glhelp
	       #:dependency-graph)
  :components
  ((:file "application")))

