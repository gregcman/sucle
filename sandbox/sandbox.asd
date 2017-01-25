(asdf:defsystem #:sandbox
    :description "???"
    :version "0.0.0"
    :author "Gregorio Manabat III"
    :maintainer "???"
    :licence "???"

    :depends-on (#:cl-opengl
                 #:window-glfw3
                 
		 #:cg-matrix
                 #:hook
		 
		 #:imagewise
                 #:pathwise

		 #:macrology
                 #:timer256

		 #:queue
                 #:world

		 #:cl-mc-shit)

    :serial t
    :components  
    ((:file "package")
     (:file "misc")
     (:file "sandbox") ;;timer for physics and rendering threads
     (:file "aabbcc") ;;box collisions

     (:file "camera-matrix") ;;matrices for cameras - view, projection

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

     (:file "cons-pool");;so cons cells can be reused
     (:file "hash");;hash functions for standard data
     (:file "unique-id-fixnum");;give out number names
     (:file "string-fixnum");;putting strings into fixnums
     (:file "zymbol");;"symbols" which are not variables
     ))
