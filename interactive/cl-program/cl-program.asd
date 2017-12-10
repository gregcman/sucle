(asdf:defsystem #:cl-program
    :depends-on (#:funland)
    :serial t
    :components
    ((:file "rectangular-tilemap")
     (:file "axis-aligned-quads")
     (:file "flip-image")
     (:file "filesystem-util")
     (:file "fps-independent-timestep")))
