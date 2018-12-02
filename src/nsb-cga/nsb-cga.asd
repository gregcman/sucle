(asdf:defsystem #:nsb-cga
  :description "adds destructive matrix functions to sb-cga"
  :depends-on (#:defpackage-plus
		  #:sb-cga)

  :serial t
  :components  
  ((:file "package")
   (:file "nsb-cga")))
