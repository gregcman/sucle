(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility
	       #:text-subsystem
	       #:opengl-immediate
	       #:opticl
	       #:character-modifier-bits
	       #:terminal-3bst-sbcl)
  :serial t
  :components 
  ((:file "double-link")
   (:file "basic")))
