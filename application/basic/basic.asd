(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility
	       #:text-subsystem
	       #:opengl-immediate
	       #:opticl
	       #:terminal-3bst-sbcl)
  :serial t
  :components 
  ((:file "keys-to-chars")
   (:file "double-link")
   (:file "basic")))
