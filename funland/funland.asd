(asdf:defsystem #:funland
  :depends-on (#:alexandria)
  :serial t
  :components
  ((:module "ultra"
	    :serial t
	    :components ((:file "package")
			 (:file "etouq")
			 (:file "dobox")
			 (:file "progno")
			 (:file "toggle")
			 (:file "eval-always")
			 (:file "with-unsafe-speed")
			 (:file "with-declaim-inline")
			 (:file "with-vec-params")
			 (:module "defdestructure"
				  :serial t
				  :components ((:file "defdestructure")))
			 (:module "let-mapped-places"
				  :components ((:file "let-mapped-places")))
			 (:file "map-home-symbols"))))) 
