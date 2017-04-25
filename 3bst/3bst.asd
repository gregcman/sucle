(defsystem 3bst
  :description "CL port of the terminal emulation part of st (http://st.suckless.org/)"
  :depends-on (#:alexandria
               #:split-sequence)
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :serial t
  :components
  ((:file "package")
   (:file "st")
   (:file "bindings")))
