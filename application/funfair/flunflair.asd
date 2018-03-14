(asdf:defsystem #:flunflair
  :depends-on (#:application
	       #:sandbox
	       #:opticl
	       #:iterator
	       #:singleton-lparallel)
  :serial t
  :components 
  ((:file "text-subsystem")
   (:file "sandbox-subsystem")
   (:file "flunflair")))
