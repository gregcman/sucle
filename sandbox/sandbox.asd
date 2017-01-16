(asdf:defsystem #:sandbox
    :description "???"
    :version "0.0.0"
    :author "Gregorio Manabat III"
    :maintainer "???"
    :licence "???"

    :depends-on (#:cl-opengl
	         #:sb-cga
                 #:window-glfw3
		 
		 #:imagewise
                 #:pathwise

		 #:glshader
                 #:glinfo

		 #:macrology
                 #:timer

		 #:queue
                 #:world

		 #:cl-mc-shit
		 #:aabbcc)

    :serial t
    :components  
    ((:file "package")
     (:file "misc")

     (:file "global") ;;global vars

     (:file "misc-math") ;;random math stuff
     (:file "camera-matrix") ;;matrices for cameras - view, projection

     (:file "magic") ;;initial asset loading
     (:file "blocks") ;;list of minecraft block values

     (:file "lovely-renderer")
     (:file "draw-environment") ;;;drawing environmental factors
     (:file "meshing-thread");;send the meshing work to a separate thread


     (:file "player-controls") ;;moving the player around, collision ---
     (:file "block-meshing") ;;turn world data into a mesh to render
     (:file "block-light") ;;lighting
     (:file "load-world");;importing nbt data into the game


     (:file "sandbox") ;;timer for physics and rendering threads
     ))
