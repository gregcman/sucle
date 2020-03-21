(asdf:defsystem #:sucle-multiprocessing
  :author "terminal625"
  :license "MIT"
  :description "multiprocessing fcailities"
  :depends-on (
	       ;;for multiprocessing
	       #:lparallel
	       #:bordeaux-threads
	       #:cl-cpus
	       #:uncommon-lisp
	       #:utility
	       #:alexandria
	       )
  :serial t
  :components 
  ((:file "package")
   (:file "multiprocessing")
   (:file "read-writer")))
