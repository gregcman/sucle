(asdf:defsystem #:funfair
  :depends-on (
	       #:funland	       
	       #:terminal625-glfw3
	       #:cl-program
	       #:terminal625-camat
	       #:terminal625-cgmat
	       #:bordeaux-threads
	       #:cl-opengl
	       #:glhelp)
  :serial t
  :components
  ((:file "funfair")))

