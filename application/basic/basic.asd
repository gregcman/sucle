(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility
	       #:text-subsystem)
  :serial t
  :components 
  ((:file "basic")))
