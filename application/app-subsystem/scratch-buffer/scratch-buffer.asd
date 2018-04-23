(asdf:defsystem #:scratch-buffer
  :depends-on (#:utility
	       #:reverse-array-iterator
	       #:bordeaux-threads)
    :components
    ((:file "scratch-buffer")))
