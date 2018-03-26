(asdf:defsystem #:text-subsystem
  :depends-on (#:application
	       #:utility
	       #:opticl)
  :serial t
  :components 
  ((:file "text-subsystem"))) 
