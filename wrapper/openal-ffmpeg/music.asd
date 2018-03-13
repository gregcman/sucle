(asdf:defsystem #:music
  :depends-on (:cl-openal
	       :cl-alc
	       :cl-ffmpeg
	       :bordeaux-threads
	       :funland
	       :singleton-lparallel)
    :serial t
    :components 
    ((:file "ffmpeg")
     (:file "openal")))
