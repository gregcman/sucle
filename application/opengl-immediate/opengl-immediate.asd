(asdf:defsystem #:opengl-immediate
  :depends-on (#:utility
	       #:cl-opengl
	       #:iterator
	       #:application-subsystem)
  :components 
  ((:file "opengl-immediate"))) 
