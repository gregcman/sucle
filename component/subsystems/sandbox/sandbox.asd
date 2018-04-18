(asdf:defsystem #:sandbox
  :depends-on (#:cl-opengl
	       #:application-subsystem
	       #:utility
	       #:reverse-array-iterator
	       #:singleton-lparallel
	       #:glhelp
	       #:gamma-correction)
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
