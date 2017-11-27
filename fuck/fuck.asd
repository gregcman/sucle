(asdf:defsystem #:fuck
  :depends-on (
	       #:funland
	       
	       #:window-glfw3
	       #:cl-program
	       #:sandbox
	       #:black-tie
	       #:terminal625-tickr
	       )
  :serial t
  :components
  
  ((:module "src"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "eyes")
	     (:file "fuck")
	     (:file "test")))))

