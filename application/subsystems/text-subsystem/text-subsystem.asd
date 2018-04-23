(asdf:defsystem #:text-subsystem
  :depends-on (#:application
	       #:utility
	       #:image-utility)
  :serial t
  :components 
  ((:file "text-subsystem"))) 
