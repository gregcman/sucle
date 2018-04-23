(asdf:defsystem #:application-subsystem
  :depends-on (#:utility
	       #:reverse-array-iterator
	       #:bordeaux-threads
	       #:nsb-cga
	       #:filesystem-util
	       #:image-utility
	       #:quads)
    :components
    ((:file "camera-matrix")
     (:file "scratch-buffer")))
