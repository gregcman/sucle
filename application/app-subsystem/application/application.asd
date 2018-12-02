(asdf:defsystem #:application
  :depends-on (#:utility
	       #:window
	       #:scratch-buffer
	       #:nsb-cga
	       #:bordeaux-threads
	       #+darwin
	       #:trivial-main-thread
	       #:cl-opengl
	       ;;#:music
	       #:glhelp
	       #:deflazy)
  :components
  ((:file "application")))

