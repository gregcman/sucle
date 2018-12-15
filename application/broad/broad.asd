(asdf:defsystem #:broad
  :author "terminal625"
  :license "MIT"
  :description "test rendering a textured mesh"
  :depends-on (#:cl-mesh
	       #:testbed)
  :serial t
  :components 
  ((:file "broad")))
