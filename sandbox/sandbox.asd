(asdf:defsystem #:sandbox
    :description "???"
    :version "0.0.0"
    :author "Gregorio Manabat III"
    :maintainer "???"
    :licence "???"

    :depends-on (#:cl-opengl
		 
		 #:imagewise
                 #:pathwise
		 #:glshader
                 #:gltexture
                 #:glinfo
		 #:macrology
		 #:the-matrix-is-everywhere	
                 #:timer

		 #:queue
                 #:world
		 #:window
		 #:cl-mc-shit
		 #:aabbcc
		 #:vox
		 #:pix)

    :serial t
    :components  
    ((:file "package")
     (:file "global") ;;global vars

     (:file "misc-math") ;;random math stuff

     (:file "magic") ;;initial asset loading
     (:file "blocks") ;;list of minecraft block values

     (:file "draw-environment") ;;;drawing environmental factors
     (:file "lovely-renderer") ;;all the rendering shit packed into a file---
     (:file "meshing-thread");;send the meshing work to a separate thread to prevent lag
     (:file "player-controls") ;;moving the player around, collision ---
     (:file "block-meshing") ;;turn world data into a mesh to render
     (:file "block-light") ;;lighting
     (:file "load-world");;importing nbt data into the game
     (:file "sandbox") ;;timer for physics and rendering threads
     ))
