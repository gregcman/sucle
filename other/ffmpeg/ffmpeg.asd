(asdf:defsystem #:ffmpeg
  :depends-on (#:funland
	       :cffi)
    :serial t
    :components
    
    ((:file "ffmpeg")))
