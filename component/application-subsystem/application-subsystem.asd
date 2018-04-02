(asdf:defsystem #:application-subsystem
  :depends-on (#:utility
	       #:reverse-array-iterator
	       #:bordeaux-threads
	       #:nsb-cga)
    :components
    ((:file "camera-matrix")
     (:file "rectangular-tilemap")
     (:file "axis-aligned-quads")
     (:file "flip-image")
     (:file "filesystem-util")
     (:file "fps-independent-timestep")
     (:file "scratch-buffer")))
