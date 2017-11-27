(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)

(glfw:with-init 
  (destructuring-bind (width height redbits greenbits bluebits) (glfw:get-desktop-mode) 
    (format t "Desktop mode: ~d x ~d x ~d~%" width height (+ redbits greenbits bluebits)))

  (format t "Available modes:~%")
  (loop for i from 0
     for mode in (glfw:get-video-modes 400)
     do (destructuring-bind (width height redbits greenbits bluebits) mode 
	  (format t "~3d: ~d x ~d x ~d~%" i width height (+ redbits greenbits bluebits)))))

