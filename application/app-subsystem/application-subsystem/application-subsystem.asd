(asdf:defsystem #:application-subsystem
  :depends-on (#:utility
	       #:reverse-array-iterator
	       #:bordeaux-threads
	       #:nsb-cga
	       #:filesystem-util)
    :components
    ((:file "camera-matrix")
     (:file "rectangular-tilemap")
     (:file "axis-aligned-quads")
     (:file "flip-image")
     (:file "scratch-buffer")))
