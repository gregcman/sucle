(asdf:defsystem #:cl-program
  :depends-on (#:funland
	       #:terminal625-zeorp)
    :components
    ((:file "rectangular-tilemap")
     (:file "axis-aligned-quads")
     (:file "flip-image")
     (:file "filesystem-util")
     (:file "fps-independent-timestep")
     (:file "scratch-buffer")))
