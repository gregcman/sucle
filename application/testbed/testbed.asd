(asdf:defsystem #:testbed
  :depends-on (#:application
	       #:sandbox
	       #:opticl
	       #:reverse-array-iterator
	       #:singleton-lparallel)
  :serial t
  :components 
  ((:file "sandbox-subsystem")
   (:file "testbed")))
