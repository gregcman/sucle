(asdf:defsystem #:testbed
  :depends-on (#:application
	       #:sandbox
	       #:image-utility
	       #:reverse-array-iterator
	       #:singleton-lparallel
	       #:uncommon-lisp
	       #:math-modify-macros
	       #:fps-independent-timestep
	       #:aabbcc
	       #:control
	       #:triangles
	       #:camera-matrix)
  :serial t
  :components 
  ((:file "sandbox-subsystem")
   (:file "testbed")))
