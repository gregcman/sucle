(asdf:defsystem #:sandbox
    :description "???"
    :version "0.0.0"
    :author "Gregorio Manabat III"
    :maintainer "???"
    :licence "???"

    :depends-on (#:cl-opengl
                 #:cg-matrix
		 #:bordeaux-threads
		 
		 
		 #:opticl

		 #:window-glfw3
		 #:pileup

		 
		 #:cl-mc-shit)
    :serial t
    :components

    ;;;;The module numbers roughly correspond to what gets loaded first
    ;;;;The stuff at the beginning has no dependents
    
    ((:file "macrology") 
     (:file "package")
     (:module other
	      :components ((:file "zymbol");;symbol alternative
			   (:file "cons-pool");;so cons cells can be reused
			   (:file "hash");;hash functions for standard data)
			   (:file "unique-id-fixnum");;give out number names
			   (:file "string-fixnum");;putting strings into fixnums
			   (:file "hook-elisp")
			   (:file "queue")))

     (:module one
	      :pathname "1"
	      :components (
			   (:file "misc")
			   (:file "imagewise");;images?
			   
			   (:file "aabbcc") ;;box collisions			   
			   (:file "pix")
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
     (:module webglsl
	      :serial t
	      :components ((:file "package")
			   (:file "types")
			   (:file "webglsl-grammar")
			   (:file "webglsl-generator")))

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
