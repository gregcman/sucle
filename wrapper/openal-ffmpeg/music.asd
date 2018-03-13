(asdf:defsystem #:music
  :depends-on (:cl-openal
	       :cl-alc
	       :bordeaux-threads
	       :funland
	       :ffmpeg-bindings
	       :singleton-lparallel)
    :serial t
    :components 
    ((:file "ffmpeg")
     (:file "openal")))
