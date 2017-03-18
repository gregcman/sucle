(asdf:defsystem #:cl-program
    :depends-on (#:cl-opengl
                 #:cg-matrix
		 #:bordeaux-threads
		 
		 
		 #:opticl

		 #:window-glfw3)
    :serial t
    :components
    
    ((:module "macros"
	      :serial t
	      :components ((:file "package")
			    
			   (:file "dobox")
			   (:file "progno")
			   (:file "toggle")))

     (:file "package")

     (:file "camera-matrix")
     
     (:file "misc")
     
     (:file "magic")
     
     (:file "lovely-renderer")
     
     (:file "aabbcc") 
     
     (:file "draw-environment")
     
     (:file "player-controls")
     
     (:file "sandbox")))
