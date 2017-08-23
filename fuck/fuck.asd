(asdf:defsystem #:fuck
    :depends-on (#:sandbox
                 #:cl-program
		 #:cl-openal
		 #:cl-alc
		 #:cl-ffmpeg
		 #:cl-mesh
		 #:funland)
    :serial t
    :components
    
    ((:module "src"
	      :serial t
	      :components
	      ((:file "package")
	       (:file "cl-ffmpeg")
	       (:file "fuck")))))
 
