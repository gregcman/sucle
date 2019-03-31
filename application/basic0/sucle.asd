(asdf:defsystem #:sucle
  :author "terminal625"
  :license "MIT"
  :description "Cube Demo Game"
  :depends-on (#:application
	       #:utility
	       #:testbed
	       #:fast-text-grid-sprites
	       #:vecto-stuff)
  :serial t
  :components 
  ((:file "basic0")))
