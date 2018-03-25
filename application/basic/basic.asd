(asdf:defsystem #:basic
  :depends-on (#:application
	       #:utility)
  :serial t
  :components 
  ((:file "basic")))
