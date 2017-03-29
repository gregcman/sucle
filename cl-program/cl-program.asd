(asdf:defsystem #:cl-program
    :depends-on (#:cl-opengl
                 #:cg-matrix
		 #:bordeaux-threads

		 #:alexandria
		 #:opticl  

		 #:window-glfw3

		 #:funland
		 #:fixed-leaf-hashed-array-tree)
    :serial t
    :components
    
    ((:module "src"
	      :serial t
	      :components
	      ((:file "package")
	       
	       (:file "camera-matrix") ;;;1st person perspective vision
	       (:file "meshing")
	       (:file "tilemap")
	       (:file "axis-aligned-quads")
	       
	       (:file "misc") ;;;everything else
	       
	       (:file "lovely-renderer") ;;;layers over opengl
	       
	       (:file "draw-environment") ;;;random rendering
	       
	       (:file "player-controls");;;layers over input
	       
	       (:file "sandbox"))))) ;;;initialization and loop
