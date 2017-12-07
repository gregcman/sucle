(asdf:defsystem #:sandbox
    :depends-on (#:cl-opengl
                 #:terminal625-cgmat
		 #:bordeaux-threads		 
		 #:funland
		 #:terminal625-queue
		 #:terminal625-rcclr
		 #:terminal625-zeorp
		 #:terminal625-aabbc
		 #:lparallel
		 #:glhelp)
    :serial t
    :components

    ((:file "package")
     (:file "this-directory") 
     (:file "vox") ;;generate a voxel hash structure
     
     (:file "buffers") ;;iterators with memory pool
     
     (:file "world")
     (:file "persist-world") ;;world <-> filesystem

     (:file "blocks") ;;minecraft block data
     (:file "block-light") ;;light propogation
     (:file "block-meshing");;world data -> mesh
     
     (:file "change-world")

     (:file "voxel-trace")
     (:file "fists")
     (:file "neck")
     (:file "collide-and-contact")    
     (:file "player-controls")
     ))
