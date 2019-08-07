(asdf:defsystem #:testbed
  :author "terminal625"
  :license "MIT"
  :description "Cube Game Demo physics part"
  :depends-on (#:application
	       #:sandbox
	       #:image-utility
	       #:reverse-array-iterator
	       #:uncommon-lisp
	       #:fps-independent-timestep
	       #:aabbcc
	       #:control
	       #:camera-matrix
	       #:alexandria
  
	       ;;for world-generation
	       #:black-tie)
  :serial t
  :components 
  ((:file "sandbox-subsystem")
   (:file "testbed")))
