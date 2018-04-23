(asdf:defsystem #:text-subsystem
  :depends-on (#:deflazy
	       #:utility
	       #:image-utility
	       #:filesystem-util
	       #:quads)
  :serial t
  :components 
  ((:file "text-subsystem"))) 
