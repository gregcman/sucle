(asdf:defsystem #:testbed
  :depends-on (#:application
	       #:sandbox
	       #:opticl
	       #:reverse-array-iterator
	       #:singleton-lparallel
	       #:uncommon-lisp
	       #:math-modify-macros
	       #:fps-independent-timestep
	       #:aabbcc)
  :serial t
  :components 
  ((:file "sandbox-subsystem")
   (:file "testbed")))
