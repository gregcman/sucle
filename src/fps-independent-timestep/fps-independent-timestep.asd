(asdf:defsystem #:fps-independent-timestep
  :author "terminal625"
  :license "MIT"
  :description "do physics simulations without worrying about frames per second"
  :depends-on (#:utility
	       #:clock)
    :components
    ((:file "fps-independent-timestep")))
