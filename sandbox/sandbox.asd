(asdf:defsystem #:sandbox
    :description "???"
    :version "0.0.0"
    :author "Gregorio Manabat III"
    :maintainer "???"
    :licence "???"

    :depends-on (#:cl-opengl
                 #:window-glfw3
		 #:opticl
		 
		 #:cg-matrix
		 #:cl-mc-shit)

    :serial t
    :components

    ;;;;The module numbers roughly correspond to what gets loaded first
    ;;;;The stuff at the beginning has no dependents
    
    ((:file "package")
     (:module zero
	      :pathname "0"
	      :components ((:file "cons-pool");;so cons cells can be reused
			   (:file "hash");;hash functions for standard data
			   (:file "hook")
			   (:file "queue")
			   (:file "camera-matrix") ;;matrices for cameras - view, projection
			   (:file "recycler")
			   (:file "zymbol");;symbol alternative
			   (:file "macrology")))
     (:module one
	      :pathname "1"
	      :components ((:file "misc")
			   (:file "aabbcc") ;;box collisions			   
			   (:file "imagewise");;images?
			   (:file "unique-id-fixnum");;give out number names
			   (:file "string-fixnum");;putting strings into fixnums			   
			   (:file "pathwise")	
			   (:file "pix")
			   (:file "vox")))
     (:module two
	      :pathname "2"
	      :components ((:file "world")))
     (:file "sandbox") ;;timer for physics and rendering threads
     

     (:file "magic") ;;initial asset loading
     (:file "blocks") ;;list of minecraft block values

     (:file "lovely-renderer") ;;generic rendering glue
     (:file "draw-environment") ;;;drawing environmental factors
     (:file "meshes");;;various types of functions to generate meshes
     
     (:file "meshing-thread");;send the meshing work to a separate thread


     (:file "player-controls") ;;moving the player around
     (:file "sync") ;;keeping various parts up to date
     (:file "boxes") ;;various size boxes
     
     (:file "block-meshing") ;;turn world data into a mesh to render
     (:file "block-light") ;;lighting
     (:file "load-world");;importing nbt data into the game

     (:file "test") ;;random tests

     ;;"symbols" which are not variables
     ))
