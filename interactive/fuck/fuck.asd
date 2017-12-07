(asdf:defsystem #:fuck
  :depends-on (
	       #:funland
	       
	       #:terminal625-glfw3
	       #:cl-program
	       #:sandbox
	       #:terminal625-tickr
	       #:terminal625-camat
	       #:opticl
	       )
  :serial t
  :components
  
  ((:module "src"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "fuck")
	     (:file "test")))))

