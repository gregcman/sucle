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
    ((:file "big-mac")
     (:file "package")
     (:file "magic")
     (:file "blocks")
     (:file "lovely-renderer")
     (:file "player-collision")
     (:file "player-controls")
     (:file "block-light")
     (:file "retarded-vectors")
     (:file "load-world")
     (:file "world-physics")
     (:file "sandbox")))
