(asdf:defsystem #:sandbox
  :depends-on (#:cl-opengl
	       #:utility
	       #:reverse-array-iterator
	       #:glhelp

	       #:bordeaux-threads
	       #:nsb-cga
	       #:quads)
  :serial t
  :components
  ((:file "queue")
   (:file "filesystem-util")
   (:file "package")
   (:file "vox") ;;generate a voxel hash structure  
   (:file "world")
   (:file "persist-world") ;;world <-> filesystem
   (:file "block-data") ;;block data
   (:file "block-light") ;;light propogation
   (:file "block-meshing");;world data -> mesh 
   (:file "change-world")
   ))
