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
     
     (:file "camera-matrix") ;;;1st person perspective vision
     
     (:file "misc") ;;;everything else
     
     (:file "lovely-renderer") ;;;layers over opengl
     
     (:file "draw-environment") ;;;random rendering
     
     (:file "player-controls");;;layers over input
     
     (:file "sandbox"))) ;;;initialization and loop
