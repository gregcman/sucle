(asdf:defsystem #:application
  :author "terminal625"
  :license "MIT"
  :description "Bring the components together for a desktop app"
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

