(asdf:defsystem #:sucle
  :author "terminal625"
  :license "MIT"
  :description "Cube Demo Game, physics part,The voxel backend for testbed"
  :depends-on (
	       ;;;sucle
	       #:application
	       #:utility
	       #:text-subsystem


	       ;;<TESTBED>
	       ;;<SANDBOX>
	       #:cl-opengl
	       #:scratch-buffer
	       #:glhelp
	       #:nsb-cga
	       #:quads

	       
	       #:sucle-serialize
	       #:sucle-multiprocessing

	       #:aabbcc ;;for occlusion culling
	       
	       ;;</SANDBOX>
	      
	       #:image-utility
	       #:uncommon-lisp
	       #:fps-independent-timestep
	       #:control
	       #:camera-matrix
	       #:alexandria
	       
	       ;;for world-generation
	       #:black-tie

	       ;;for the terrain picture
	       #:sucle-temp
	       ;;</TESTBED>

	       )
  :serial t
  :components 
  (
   ;;<SANDBOX>
   (:file "queue")
   (:file "voxel-chunks")
   ;;(:file "block-light") ;;light propogation
   (:file "world")
   (:file "mesher");;world data -> mesh 
   (:file "change-world")
   ;;</SANDBOX>
   (:file "sucle-package")
   (:file "extra")
   (:file "physics")
   (:file "sucle")
   (:file "render")))
