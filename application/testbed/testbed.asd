(asdf:defsystem #:testbed
  :depends-on (#:application
	       #:sandbox
	       #:opticl
	       #:reverse-array-iterator
	       #:singleton-lparallel
	       #:uncommon-lisp
	       #:math-modify-macros
	       #:fps-independent-timestep)
  :serial t
  :components 
  ((:file "aabbcc")
   (:file "sandbox-subsystem")
   (:file "testbed")))
