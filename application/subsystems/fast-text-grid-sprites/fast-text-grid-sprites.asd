(asdf:defsystem #:fast-text-grid-sprites
  :author "terminal625"
  :license "MIT"
  :description "text buttons subsystem"
  :depends-on (#:application
	       #:utility
	       #:sprite-chain
	       #:text-subsystem
	       #:opengl-immediate
	       #:image-utility
	       #:quads
	       #:point)
  :serial t
  :components 
  ((:file "fast-text-grid-sprites")))
