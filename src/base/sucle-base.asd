(asdf:defsystem #:sucle-base
  :author "N/A"
  :license "MIT"
  :description "Cube Demo Game"
  :depends-on
  (
   #:application
   #:camera-matrix
   ;;#:character-modifier-bits
   #:control
   #:crud
   #:deflazy
   #:fps-independent-timestep
   #:image-utility
   #:ncurses-clone-for-lem
   #:nsb-cga
   #:glhelp
   #:quads
   #:scratch-buffer
   #:sucle-multiprocessing
   #:sucle-serialize
   #:text-subsystem
   #:uncommon-lisp
   #:window

   
   ;;temp folder
   #:sucle-temp
   
   #:cl-opengl))
