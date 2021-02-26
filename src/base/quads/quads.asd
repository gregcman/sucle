(asdf:defsystem #:quads
  :author "terminal625"
  :license "MIT"
  :description "quads, as in shapes with 4 vertices"
  :depends-on (#:utility)
    :components
    ((:file "rectangular-tilemap")
     (:file "axis-aligned-quads")
     (:file "rectangle")))
