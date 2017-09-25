(asdf:defsystem #:sandbox
    :depends-on (#:cl-opengl
                 #:cg-matrix
		 #:bordeaux-threads		 
		 #:funland
		 #:black-tie)
    :serial t
    :components
    
    ((:file "package")
     (:module other
	      :components ((:file "queue")))

     (:module one
	      :pathname "1"
	      :components (			   
			   (:file "aabbcc") ;;box collisions			   
			   (:file "vox")))
     (:module two
	      :serial t
	      :pathname "2"
	      :components ((:file "recycler")
			   (:file "world")
			   (:file "blocks") ;;list of minecraft block values
			   (:file "block-light") ;;lighting
			   (:file "block-meshing");;turn world data into a mesh to render
			   ))

     (:file "camera-matrix") ;;matrices for cameras - view, projection

     (:file "lovely-renderer") ;;generic rendering glue
     
     (:file "draw-environment") ;;;drawing environmental factors     
     (:file "meshing-thread");;send the meshing work to a separate thread

     (:module phys
	      :serial t
	      :components ((:file "voxel-trace")
			   (:file "fists")
			   (:file "collide-and-contact")))
     (:file "player-controls") ;;moving the player around

					;  (:file "wotut")
     (:file "sync") ;;keeping various parts up to date
     (:file "sandbox") ;;timer for physics and rendering threads
     ))
