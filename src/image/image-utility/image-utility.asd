(asdf:defsystem #:image-utility
  :author "terminal625"
  :license "MIT"
  :description "Load an image and/or flip it"
  :depends-on (#:opticl
	       ;;FIXME::uncommon-lisp is used solely to wrap the opticl-loaded-data
	       ;;in case it ever needs to be updated on the fly
	       #:uncommon-lisp
	       )
  :components
  ((:file "image-utility")))
