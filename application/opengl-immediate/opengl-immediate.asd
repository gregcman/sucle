(asdf:defsystem #:opengl-immediate
  :depends-on (#:utility
	       #:cl-opengl
	       #:reverse-array-iterator
	       #:application-subsystem)
  :components 
  ((:file "opengl-immediate"))) 
