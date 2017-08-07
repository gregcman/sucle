(asdf:defsystem #:ffmpeg
  :depends-on (#:funland
	       :cffi
	       :cl-openal
	       :cl-alc)
    :serial t
    :components
    
    ((:file "ffmpeg")))
