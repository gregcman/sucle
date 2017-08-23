(asdf:defsystem #:fuck
  :depends-on (
	       #:funland
	       
	       #:cl-openal
	       #:cl-alc
	       #:cl-ffmpeg
	       #:cl-mesh


	       #:cl-program
	       #:sandbox
	       

	       )
  :serial t
  :components
  
  ((:module "src"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "cl-ffmpeg")
	     (:file "fuck")))))

