(asdf:defsystem #:fuck
  :depends-on (
	       #:funland
	       
	       #:window-glfw3
	       #:cl-program
	       #:sandbox
	       #:black-tie

	       )
  :serial t
  :components
  
  ((:module "src"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "tick-governor")
	     (:file "eyes")
	     (:file "fuck")
	     (:file "test")))))

