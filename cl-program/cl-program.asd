(asdf:defsystem #:cl-program
    :depends-on (#:cl-opengl
                 #:cg-matrix
		 #:bordeaux-threads
		 
		 
		 #:opticl

		 #:window-glfw3)
    :serial t
    :components
    
    ((:file "macrology") 
     (:file "package")
     (:file "hook-elisp")
     (:file "misc")
     (:file "imagewise")
     (:file "camera-matrix") 
     (:file "magic")
     (:file "lovely-renderer")
     (:file "aabbcc")

     (:file "meshes")
     (:file "draw-environment")
     (:file "meshing-thread")
     (:file "player-controls")
    
     (:file "sandbox")
     ))
