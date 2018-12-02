(asdf:defsystem #:opengl-immediate
  :depends-on (#:utility
	       #:cl-opengl
	       #:reverse-array-iterator
	       #:scratch-buffer)
  :components 
  ((:file "opengl-immediate"))) 
