(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility
	       #:doubly-linked-list
	       #:text-subsystem
	       #:opengl-immediate
	       #:opticl
	       #:character-modifier-bits
	       #:terminal-3bst-sbcl)
  :serial t
  :components 
  ((:file "basic")))
