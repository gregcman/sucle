(asdf:defsystem #:cl-program
    :depends-on (#:cl-opengl

		 #:alexandria
		 #:opticl
		 
		 #:funland
		 #:terminal625-flhat)
    :serial t
    :components
    
    ((:module "auxiliary-functions"
	      :components
	      ((:file "rectangular-tilemap")
	       (:file "axis-aligned-quads")
	       (:file "lovely-shader-and-texture-uploader")))

     (:module "src"
	      :serial t
	      :components
	      ((:file "package")
	       
	       (:file "misc")
	       (:file "draw-environment")
	       ))))
