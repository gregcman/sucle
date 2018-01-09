(asdf:defsystem #:music
  :depends-on (:cl-openal
	       :cl-alc
	       :cl-ffmpeg
	       :bordeaux-threads
	       :singleton-lparallel)
    :serial t
    :components 
    ((:file "openal")))
