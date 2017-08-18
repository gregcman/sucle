(asdf:defsystem #:game-loop
    :depends-on (#:sandbox
                 #:cl-program
		 #:funland
		 #:hook-elisp)
    :serial t
    :components
    
    ((:module "src"
	      :serial t
	      :components
	      ((:file "package")
	       (:file "game-loop")))))
 
