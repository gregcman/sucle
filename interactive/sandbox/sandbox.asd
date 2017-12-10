(asdf:defsystem #:sandbox
  :depends-on (#:cl-opengl
	       #:cl-program
	       #:funland
	       #:terminal625-queue
	       #:terminal625-zeorp
	       #:lparallel
	       #:glhelp)
  :serial t
  :components

  ((:file "package")
   (:file "vox") ;;generate a voxel hash structure
   
   (:file "buffers") ;;iterators with memory pool
   
   (:file "world")
   (:file "persist-world") ;;world <-> filesystem

   (:file "blocks") ;;minecraft block data
   (:file "block-light") ;;light propogation
   (:file "block-meshing");;world data -> mesh
   
   (:file "change-world")
   ))
