(asdf:defsystem #:fuck
  :depends-on (
	       #:funland
	       
	       #:window-glfw3
	       #:cl-program
	       #:sandbox
	       

	       )
  :serial t
  :components
  
  ((:module "src"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "tick-governor")
	     (:file "fuck")))))

