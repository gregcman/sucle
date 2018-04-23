(asdf:defsystem #:application
  :depends-on (#:utility
	       #:window
	       #:application-subsystem
	       #:nsb-cga
	       #:bordeaux-threads
	       #:cl-opengl
	       #:music
	       #:glhelp
	       #:dependency-graph)
  :components
  ((:file "package")
   (:file "lazy-loading")
   (:file "application")))

