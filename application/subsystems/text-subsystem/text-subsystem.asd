(asdf:defsystem #:text-subsystem
  :depends-on (#:deflazy
	       #:utility
	       #:image-utility
	       #:quads)
  :serial t
  :components 
  ((:file "text-subsystem"))) 
