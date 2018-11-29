(asdf:defsystem #:sucle
  :depends-on (#:application
	       #:utility
	       #:sprite-chain
	       #:text-subsystem
	       #:opengl-immediate
	       #:image-utility
	       #:character-modifier-bits
	       #:uncommon-lisp
	       #:quads
	       #:point
	       #:testbed)
  :serial t
  :components 
  ((:file "basic0")))
