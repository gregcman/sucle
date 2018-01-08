(asdf:defsystem #:music
  :depends-on (:cl-openal
	       :cl-alc
	       :cl-ffmpeg
	       :bordeaux-threads)
    :serial t
    :components 
    ((:file "openal")))
