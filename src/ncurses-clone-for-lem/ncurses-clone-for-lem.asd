(asdf:defsystem "ncurses-clone-for-lem"
  :depends-on ("cffi"
	       "lparallel"
	       #:nsb-cga
	       #:cl-ppcre
	       #:application
	       #:utility
	       #:text-subsystem)
  :serial t
  :components ((:file "ncurses-clone")
               (:file "term")
	       (:file "ncurses-clone-lem-view")
	       (:file "app")))
