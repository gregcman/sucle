(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility
	       #:text-subsystem
	       #:opticl)
  :serial t
  :components 
  ((:file "keys-to-chars")
   (:file "double-link")
   (:file "more")
   (:file "basic")))
