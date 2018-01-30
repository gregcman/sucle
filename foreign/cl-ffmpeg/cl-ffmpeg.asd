(asdf:defsystem #:cl-ffmpeg
  :depends-on (:cffi
	       :funland)
    :serial t
    :components 
    ((:file "ffmpeg-bindings")
     (:file "cl-ffmpeg")))
