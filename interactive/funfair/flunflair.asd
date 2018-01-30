(asdf:defsystem #:flunflair
  :depends-on (:music
	       :sandbox-funfair
	       :text-funfair)
    :serial t
    :components 
    ((:file "flunflair")))
