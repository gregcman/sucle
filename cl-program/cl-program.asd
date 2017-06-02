(asdf:defsystem #:cl-program
    :depends-on (#:cl-opengl
                 #:cg-matrix
		 #:bordeaux-threads

		 #:alexandria
		 #:opticl

		 #:cl-freetype2

		 #:window-glfw3
		 #:cl-conspack
		 
		 #:funland
		 #:fixed-leaf-hashed-array-tree)
    :serial t
    :components
    
    ((:module "src"
	      :serial t
	      :components
	      ((:file "package")
	       (:file "vec-param-layout")
	       (:file "camera-matrix") ;;;1st person perspective vision
	       (:file "meshing")
	       (:file "tilemap")
	       (:file "fonts")
	       (:file "axis-aligned-quads")
	       (:file "keys-ascii")
	       (:file "list-junction")
	       
	       (:file "globjects")
	       (:file "pix")
	       
	       (:file "misc") ;;;everything else
	       
	       (:file "lovely-renderer") ;;;layers over opengl
	       
	       (:file "draw-environment") ;;;random rendering
	       
	       (:file "player-controls");;;layers over input
	       
	       (:file "sandbox"))))) ;;;initialization and loop
