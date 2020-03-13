(defsystem "lem-opengl"
  :depends-on (
	       #:ncurses-clone-for-lem
	       ;;"cl-charms"
	       "control"
               "trivial-clipboard"
               ;;#+(or (and ccl unix) (and lispworks unix))"lem-setlocale"
               "minilem"

	       #:application
	       #:utility
	       ;;#:opengl-immediate
	       ;;#:character-modifier-bits
	       #:uncommon-lisp
	       #:livesupport

	       #:sucle)
  :serial t
  :components ((:file "package")   
	       (:file "impl")
	       (:file "keys")
               (:file "sucle")
	       (:module "other"
			:components
			((:file "test")
			 (:file "test2")))))
