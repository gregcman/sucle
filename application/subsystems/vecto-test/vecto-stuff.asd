(asdf:defsystem #:vecto-stuff
  :author "terminal625"
  :license "MIT"
  :description "vector graphics subsystem"
  :depends-on (#:application
	       #:utility
	       #:vecto)
  :serial t
  :components 
  ((:file "vecto-test")
   (:file "vecto-stuff")))
