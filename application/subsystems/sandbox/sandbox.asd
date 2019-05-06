(asdf:defsystem #:sandbox
  :author "terminal625"
  :license "MIT"
  :description "The voxel backend for testbed"
  :depends-on (#:cl-opengl
	       #:utility
	       #:scratch-buffer
	       #:reverse-array-iterator
	       #:glhelp
	       #:lparallel
	       #:bordeaux-threads
	       #:nsb-cga
	       #:quads
	       #:cl-conspack
	       #:salza2
	       #:chipz)
  :serial t
  :components
  ((:file "queue")
   (:file "package")
   (:file "world")
   (:file "serialize") ;;generic file loading and stuff
   (:file "persist-world") ;;world <-> filesystem
   (:file "block-data") ;;block data
   ;;(:file "block-light") ;;light propogation
   (:file "block-meshing");;world data -> mesh 
   (:file "change-world")
   ))
