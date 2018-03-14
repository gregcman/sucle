(asdf:defsystem #:cl-program
  :depends-on (#:funland
	       #:terminal625-zeorp
	       #:nsb-cga)
    :components
    ((:file "camera-matrix")
     (:file "rectangular-tilemap")
     (:file "axis-aligned-quads")
     (:file "flip-image")
     (:file "filesystem-util")
     (:file "fps-independent-timestep")
     (:file "scratch-buffer")))
