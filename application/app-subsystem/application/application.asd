(asdf:defsystem #:application
  :depends-on (#:utility
	       #:window
	       #:scratch-buffer
	       #:nsb-cga
	       #:bordeaux-threads
	       #:cl-opengl
	       ;;#:music
	       #:glhelp
	       #:deflazy)
  :components
  ((:file "application")))

