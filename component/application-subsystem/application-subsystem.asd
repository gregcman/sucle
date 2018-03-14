(asdf:defsystem #:application-subsystem
  :depends-on (#:utility
	       #:iterator
	       #:bordeaux-threads
	       #:nsb-cga)
    :components
    ((:file "lazy-place")
     (:file "camera-matrix")
     (:file "rectangular-tilemap")
     (:file "axis-aligned-quads")
     (:file "flip-image")
     (:file "filesystem-util")
     (:file "fps-independent-timestep")
     (:file "scratch-buffer")))
