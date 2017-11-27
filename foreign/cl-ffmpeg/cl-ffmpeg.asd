(asdf:defsystem #:cl-ffmpeg
  :depends-on (:cffi)
    :serial t
    :components 
    ((:file "ffmpeg-bindings")))
