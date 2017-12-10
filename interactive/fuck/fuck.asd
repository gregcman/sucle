(asdf:defsystem #:fuck
  :depends-on (
	       #:funland	       
	       #:terminal625-glfw3
	       #:cl-program
	       #:sandbox
	       #:terminal625-camat
	       #:terminal625-cgmat
	       #:opticl
	       )
  :serial t
  :components
  ((:file "fuck")
   (:file "test")
   (:file "player-controls")))

