(asdf:defsystem #:cartesian-graphing
  :author "terminal625"
  :license "MIT"
  :description "math graphing subsystem"
  :depends-on (#:application
	       #:utility
	       #:alexandria)
  :serial t
  :components 
  ((:file "cartesian-graphing")))
