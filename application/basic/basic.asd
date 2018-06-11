(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility
	       #:sprite-chain
	       #:text-subsystem
	       #:opengl-immediate
	       #:image-utility
	       #:character-modifier-bits
	       #:terminal-3bst-sbcl
	       #:uncommon-lisp
	       #:testbed
	       #:quads
	       #:point)
  :serial t
  :components 
  ((:file "basic")))
