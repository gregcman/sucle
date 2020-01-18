(asdf:defsystem #:sucle-test
  :author "terminal625"
  :license "MIT"
  :description "Test system for sucle libraries"
  :depends-on
  (
   #:alexandria
   #:aabbcc
   #:application
   #:camera-matrix
   #:character-modifier-bits
   #:control
   #:deflazy
   #:fps-independent-timestep
   #:image-utility
   #:ncurses-clone-for-lem
   #:nsb-cga
   #:cl-opengl
   #:quads
   #:scratch-buffer
   #:sucle
   #:sucle-multiprocessing
   #:sucle-serialize
   #:text-subsystem
   #:uncommon-lisp
   #:window
   )
  :serial t
  :components 
  ((:file "sucle-test")))
