(asdf:defsystem #:sandbox
  :author "terminal625"
  :license "MIT"
  :description "The voxel backend for testbed"
  :depends-on (#:cl-opengl
	       #:utility
	       #:scratch-buffer
	       #:reverse-array-iterator
	       #:glhelp
	       #:nsb-cga
	       #:quads
	       ;;for serialize
	       #:cl-conspack
	       #:salza2
	       #:chipz
	       ;;for multiprocessing
	       #:lparallel
	       #:bordeaux-threads
	       #:cl-cpus
	       #:uncommon-lisp
	       #:utility

	       ;;for world-generation
	       #:black-tie)
  :serial t
  :components
  ((:file "queue")
   (:file "serialize") ;;generic file loading and stuff
   (:file "multiprocessing")
   (:file "package")
   (:file "world")
   (:file "persist-world") ;;world <-> filesystem
   (:file "block-data") ;;block data
   ;;(:file "block-light") ;;light propogation
   (:file "block-meshing");;world data -> mesh 
   (:file "change-world")
   ))
