(asdf:defsystem #:application-subsystem
  :depends-on (#:utility
	       #:reverse-array-iterator
	       #:bordeaux-threads
	       #:nsb-cga
	       #:filesystem-util
	       #:image-utility)
    :components
    ((:file "camera-matrix")
     (:file "rectangular-tilemap")
     (:file "axis-aligned-quads")
     (:file "scratch-buffer")))
