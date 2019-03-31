(asdf:defsystem #:sucle
  :author "terminal625"
  :license "MIT"
  :description "Cube Demo Game"
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
	       #:testbed
	       #:vecto-stuff)
  :serial t
  :components 
  ((:file "basic0")))
