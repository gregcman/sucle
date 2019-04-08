;;;; sketch-examples.asd

(asdf:defsystem #:sketch-sucle-examples
  :description "Sketch examples"
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:alexandria
               #:sketch-sucle)
  :serial t
  :components ((:file "examples")
               ))
