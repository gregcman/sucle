(asdf:defsystem #:opengl-immediate
  :author "terminal625"
  :license "MIT"
  :description "Draw coordinates like OpenGL immediate, even if unsupported"
  :depends-on (#:utility
	       #:cl-opengl
	       #:reverse-array-iterator
	       #:scratch-buffer)
  :components 
  ((:file "opengl-immediate"))) 
