(asdf:defsystem #:fps-independent-timestep
  :depends-on (#:utility
	       #:clock)
    :components
    ((:file "fps-independent-timestep")))
