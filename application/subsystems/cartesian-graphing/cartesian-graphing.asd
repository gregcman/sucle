(asdf:defsystem #:cartesian-graphing
  :author "terminal625"
  :license "MIT"
  :description "math graphing subsystem"
  :depends-on (#:application
	       #:utility
	       #:alexandria
	       #:opengl-immediate)
  :serial t
  :components 
  ((:file "cartesian-graphing")))
