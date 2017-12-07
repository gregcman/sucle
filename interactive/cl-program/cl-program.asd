(asdf:defsystem #:cl-program
    :depends-on ()
    :serial t
    :components
    
    ((:module "auxiliary-functions"
	      :components
	      ((:file "rectangular-tilemap")
	       (:file "axis-aligned-quads")
	       (:file "flip-image")
	       (:file "filesystem-util")))))
