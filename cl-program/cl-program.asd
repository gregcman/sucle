(asdf:defsystem #:cl-program
    :depends-on (#:cl-opengl
                 #:cg-matrix
		 #:bordeaux-threads

		 #:alexandria
		 #:cl-conspack
		 #:opticl
		 #:window-glfw3
		 
		 #:funland
		 #:fixed-leaf-hashed-array-tree)
    :serial t
    :components
    
    ((:module "auxiliary-functions"
	      :components
	      ((:file "rectangular-tilemap")
	       (:file "axis-aligned-quads")
	       (:file "pix")
	       (:file "lovely-shader-and-texture-uploader")))

     (:module "src"
	      :serial t
	      :components
	      ((:file "package")
	       
	       (:file "vec-param-layout")	       	       
	       (:file "misc")

	       (:file "draw-environment")
	       (:file "pix2")
	       (:file "player-controls")

	       (:file "draw2")
	       ))))
