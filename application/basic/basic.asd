(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility
	       #:text-subsystem
	       #:opticl)
  :serial t
  :components 
  ((:file "basic")))
