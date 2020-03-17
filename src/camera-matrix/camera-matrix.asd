(asdf:defsystem #:camera-matrix
  :author "terminal625"
  :license "MIT"
  :description "projection matrices for 3d rendering"
  :depends-on (#:nsb-cga
	       #:uncommon-lisp)
    :components
    ((:file "camera-matrix")))
