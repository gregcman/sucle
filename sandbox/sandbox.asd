(asdf:defsystem #:sandbox
    :depends-on (#:cl-opengl
                 #:cg-matrix
		 #:bordeaux-threads
		 
		 
		 #:opticl
		 
		 #:window-glfw3
		 #:pileup

		 #:funland
		 #:cl-mc-shit)
    :serial t
    :components

    ;;;;The module numbers roughly correspond to what gets loaded first
    ;;;;The stuff at the beginning has no dependents
    
    ((:file "package")
     (:module other
	      :components ((:file "queue")))

     (:module one
	      :pathname "1"
	      :components (
			   (:file "misc")
			   (:file "imagewise");;images?
			   
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
     (:file "magic") ;;initial asset loading
     (:file "lovely-renderer") ;;generic rendering glue
     
     (:file "meshes");;;various types of functions to generate meshes
     (:file "boxes") ;;various size boxes
     
     (:file "draw-environment") ;;;drawing environmental factors     
     (:file "meshing-thread");;send the meshing work to a separate thread
     (:file "player-controls") ;;moving the player around
     
     (:file "sync") ;;keeping various parts up to date
     (:file "sandbox") ;;timer for physics and rendering threads
     ))
