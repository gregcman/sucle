(asdf:defsystem #:ffmpeg-bindings
  :depends-on (:cffi)
    :components 
    ((:file "ffmpeg-bindings")))
