(asdf:defsystem #:cl-program
    :depends-on (#:cl-opengl
                 #:cg-matrix
		 #:bordeaux-threads

		 #:alexandria
		 #:opticl
		 #:trivial-main-thread
		 #:window-glfw3
		 #:cl-conspack
		 
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
	       (:file "keys-ascii")
	       (:file "physical-keyboard")
	       
	       (:file "misc") ;;;everything else	       
	       (:file "globjects")
	       (:file "draw-environment") ;;;random rendering
	       (:file "pix2")
	       (:file "player-controls");;;layers over input
	       ))))
