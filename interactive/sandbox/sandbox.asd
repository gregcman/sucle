(asdf:defsystem #:sandbox
    :depends-on (#:cl-opengl
                 #:terminal625-cgmat
		 #:bordeaux-threads		 
		 #:funland
		 #:terminal625-queue
		 #:terminal625-rcclr
		 #:terminal625-zeorp
		 #:terminal625-mcblk
		 #:terminal625-aabbc
		 #:opticl
		 #:glhelp)
    :serial t
    :components
    
    ((:file "package")
     (:file "vox")
     (:file "buffers")
     (:module two
	      :serial t
	      :pathname "2"
	      :components ((:file "world")
			   (:file "block-light") ;;lighting
			   (:file "block-meshing");;turn world data into a mesh to render
			   ))
     (:file "draw-environment") ;;;drawing environmental factors     
     (:file "meshing-thread");;send the meshing work to a separate thread

     (:module phys
	      :serial t
	      :components ((:file "voxel-trace")
			   (:file "fists")
			   (:file "neck")
			   (:file "collide-and-contact")))
     (:file "player-controls") ;;moving the player around

     (:file "sync") ;;keeping various parts up to date
     (:file "sandbox") ;;timer for physics and rendering threads
     ))
