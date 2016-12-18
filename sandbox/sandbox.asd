(asdf:defsystem #:sandbox
    :description "when parts come together"
    :version "0.0.0"
    :author "a little boy in the sand"
    :maintainer "tain mainer"
    :licence "fuck that shit"

    :depends-on (#:cl-opengl
		 #:cl-utilities  
		 #:opticl
		 
		 #:window
		 #:cl-mc-shit
		 #:the-matrix-is-everywhere
		 #:aabbcc
		 #:vox
		 #:pix)

    :serial t
    :components  
    ((:file "package")
     (:file "big-mac");;convenience macros
     (:file "magic") ;;file io type stuff
     (:file "blocks") ;;list of minecraft block values
     (:file "world") ;;get blocks, contain blocks, set blocks...
     (:file "lovely-renderer") ;;all the rendering shit packed into a file---
     (:file "player-controls") ;;moving the player around, collision ---
     (:file "block-light") ;;lighting
     (:file "load-world");;importing nbt data into the game
     (:file "sandbox") ;;timer for physics and rendering threads
     ))
