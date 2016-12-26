(asdf:defsystem #:sandbox
    :description "???"
    :version "0.0.0"
    :author "Gregorio Manabat III"
    :maintainer "???"
    :licence "???"

    :depends-on (#:cl-opengl
		 #:cl-utilities  
		 #:opticl
		 
		 #:macrology
		 #:queue
                 #:world
		 #:window
		 #:cl-mc-shit
		 #:the-matrix-is-everywhere
		 #:aabbcc
		 #:vox
		 #:pix)

    :serial t
    :components  
    ((:file "package")
     (:file "misc-math") ;;random math stuff
     (:file "misc-list") ;;random list stuff
     (:file "misc-image") ;;random image array manipulation
     (:file "misc-fad");;;miscellaneous file and directory stuff
     (:file "timer") ;;;facilities for dealing with time
     (:file "thread") ;;; threading utilities
     (:file "meshing") ;;generic meshing utilities
     (:file "misc-opengl") ;;random opengl stuff
     (:file "shader") ;;generic shader utilities
     (:file "magic") ;;file io type stuff
     (:file "blocks") ;;list of minecraft block values
     (:file "vao");;opengl vertex array objects
     (:file "draw-environment") ;;;drawing environmental factors
     (:file "lovely-renderer") ;;all the rendering shit packed into a file---
     (:file "meshing-thread");;send the meshing work to a separate thread to prevent lag
     (:file "player-controls") ;;moving the player around, collision ---
     (:file "block-meshing") ;;turn world data into a mesh to render
     (:file "block-light") ;;lighting
     (:file "load-world");;importing nbt data into the game
     (:file "sandbox") ;;timer for physics and rendering threads
     ))
