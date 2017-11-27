(asdf:defsystem #:cl-program
    :depends-on (#:cl-opengl

		 #:alexandria
		 #:opticl
		 
		 #:funland
		 #:terminal625-flhat
		 #:terminal625-zeorp)
    :serial t
    :components
    
    ((:module "auxiliary-functions"
	      :components
	      ((:file "rectangular-tilemap")
	       (:file "axis-aligned-quads")))

     (:module "src"
	      :serial t
	      :components
	      ((:file "buffers")
	       ))))
