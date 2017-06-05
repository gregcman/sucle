(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)

(cffi:defcallback hello-fun :void ((arg :pointer))
  (declare (ignore arg))
  (format t "Hello "))

(glfw:init)
(let ((thread (glfw:create-thread (cffi:callback hello-fun) (cffi:null-pointer))))
  (glfw:wait-thread thread glfw:+wait+)
  (format t "world~%"))

(glfw:terminate)
