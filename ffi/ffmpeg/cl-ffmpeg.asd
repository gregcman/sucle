(asdf:defsystem #:cl-ffmpeg
  :depends-on (:cffi)
    :components 
    ((:file "bindings")))
