(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility
	       #:doubly-linked-list
	       #:text-subsystem
	       #:opengl-immediate
	       #:opticl
	       #:character-modifier-bits
	       #:terminal-3bst-sbcl
	       #:uncommon-lisp
	       #:testbed)
  :serial t
  :components 
  ((:file "sprite-chain")
   (:file "basic")))
