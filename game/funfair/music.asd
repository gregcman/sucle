(asdf:defsystem #:music
  :depends-on (:cl-openal
	       :cl-alc
	       :cl-ffmpeg)
    :serial t
    :components 
    ((:file "openal")))
