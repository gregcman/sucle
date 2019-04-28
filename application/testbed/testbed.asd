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
	       #:alexandria)
  :serial t
  :components 
  ((:file "sandbox-subsystem")
   (:module "minecraft" :serial t 
	    :components ((:file "blocks")
			 (:module "voxel" :serial t 
				  :components ((:file "test")))))
   (:file "testbed")))
