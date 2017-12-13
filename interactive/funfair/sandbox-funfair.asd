(asdf:defsystem #:sandbox-funfair
  :depends-on (#:funfair
	       #:sandbox
	       #:opticl)
  :serial t
  :components
  ((:file "test")
   (:file "player-controls")))

 
