(defmacro progno (&rest body) (declare (ignore body)))

(asdf:defsystem #:window
  :description "a windowing thingy"
  :version "0.0.0"
  :author "morpheus"
  :maintainer "tain mainer"
  :licence "i am not sure"

  :depends-on (#:cl-opengl
               ;;#:lispbuilder-sdl
               #:cl-glfw3
               #:trivial-main-thread)

  :serial t
    :components  
    ((:file "package")
    (:file "glfw3/window")))
;;"lispbuilder-sdl/window" 