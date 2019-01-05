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
	       #:quads)
  :serial t
  :components
  ((:file "queue")
   (:file "package")
   (:file "vox") ;;generate a voxel hash structure  
   (:file "world")
   (:file "persist-world") ;;world <-> filesystem
   (:file "block-data") ;;block data
   (:file "block-light") ;;light propogation
   (:file "block-meshing");;world data -> mesh 
   (:file "change-world")
   ))
