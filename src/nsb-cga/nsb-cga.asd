(asdf:defsystem #:nsb-cga
  :description "adds destructive matrix functions to sb-cga"
  :depends-on (#:cl-reexport
	       #:sb-cga)

  :serial t
  :components  
  ((:file "package")
   (:file "nsb-cga")))
