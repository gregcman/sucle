(asdf:defsystem #:funfair
  :depends-on (
	       #:funland	       
	       #:terminal625-glfw3
	       #:cl-program
	       #:terminal625-camat
	       #:terminal625-cgmat
	       #:bordeaux-threads
	       #:cl-opengl
	       #:music
	       #:glhelp)
  :serial t
  :components
  ((:file "deflazy")
   (:file "funfair")))

