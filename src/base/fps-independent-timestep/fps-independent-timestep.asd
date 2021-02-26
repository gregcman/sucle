(asdf:defsystem #:fps-independent-timestep
  :author "terminal625"
  :license "MIT"
  :description
  "do physics simulations without worrying about frames per second,
And a millisecond or microsecond timer"
  :depends-on (#:alexandria
	       #:local-time)
    :components
    ((:file "fps-independent-timestep")))
