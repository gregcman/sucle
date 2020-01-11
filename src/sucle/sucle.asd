(asdf:defsystem #:sucle
  :author "terminal625"
  :license "MIT"
  :description "Cube Demo Game"
  :depends-on (#:application
	       #:utility
	       #:testbed
	       #:text-subsystem)
  :serial t
  :components 
  ((:file "sucle")))
