(asdf:defsystem #:fuck
    :depends-on (#:sandbox
                 #:cl-program
		 #:funland)
    :serial t
    :components
    
    ((:module "src"
	      :serial t
	      :components
	      ((:file "package")
	       (:file "fuck")))))
 
