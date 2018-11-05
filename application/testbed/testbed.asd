(asdf:defsystem #:testbed
  :depends-on (#:application
	       #:sandbox
	       #:image-utility
	       #:reverse-array-iterator
	       #:uncommon-lisp
	       #:math-modify-macros
	       #:fps-independent-timestep
	       #:aabbcc
	       #:control
	       #:triangles
	       #:camera-matrix
	       #:alexandria)
  :serial t
  :components 
  ((:file "sandbox-subsystem")
   (:module "minecraft" :serial t 
	    :components ((:file "blocks")))
   (:file "testbed")))
