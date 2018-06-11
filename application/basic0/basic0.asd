(asdf:defsystem #:basic0
  :depends-on (#:application
	       #:utility
	       #:sprite-chain
	       #:text-subsystem
	       #:opengl-immediate
	       #:image-utility
	       #:character-modifier-bits
	       #:uncommon-lisp
	       #:quads
	       #:point)
  :serial t
  :components 
  ((:file "basic0")))
