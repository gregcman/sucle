(asdf:defsystem #:%sb-cga
  :description "adds destructive operations to sb-cga"
  :depends-on (#:defpackage-plus
		  #:sb-cga)

  :serial t
  :components  
  ((:file "package")
   (:file "%sb-cga")))
