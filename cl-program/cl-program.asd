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
     (:file "repl")
     
     (:file "camera-matrix") ;;;1st person perspective vision
     
     (:file "misc") ;;;everything else
     
     (:file "magic") ;;; rudimentary dependency system
     
     (:file "lovely-renderer") ;;;layers over opengl
     
     (:file "aabbcc") ;;;box collisions
     
     (:file "draw-environment") ;;;random rendering
     
     (:file "player-controls");;;layers over input
     
     (:file "sandbox"))) ;;;initialization and loop
