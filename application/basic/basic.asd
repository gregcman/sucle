(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility
	       #:text-subsystem
	       #:opengl-immediate
	       #:opticl
	       #:3bst)
  :serial t
  :components 
  ((:file "keys-to-chars")
   (:file "double-link")
   (:file "terminal-test")
   (:file "basic")))
