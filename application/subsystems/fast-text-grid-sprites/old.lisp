
#+nil
(let ((program (getfnc 'flat-shader)))
  (glhelp::use-gl-program program)
  (glhelp:with-uniforms uniform program
    (gl:uniform-matrix-4fv (uniform :pmv)
			   (nsb-cga:matrix*
			    (nsb-cga:scale*
			     (/ 2.0 (floatify window::*width*))
			     (/ 2.0 (floatify window::*height*))
			     1.0)
			    (nsb-cga:translate* 
			     (/ (floatify window::*width*)
				-2.0)				 
			     (/ (floatify window::*height*)
				-2.0)
			     0.0))
			   nil)))
#+nil
(progn
  (do-sprite-chain (sprite t) ()
    (render-sprite sprite))
  (gl:with-primitive :quads
    (mesh-vertex-tex-coord-color)))

#+nil
(defparameter *pen-color* (list 1.0 0.0 0.0 1.0))

#+nil
(defun render-sprite (sprite)
  (with-slots (absolute-rectangle)
      sprite
    (let ((*pen-color*
	   (cond ((eq sprite *selection*)
		  '(1.0 0.0 0.0 1.0))
		 ((eq sprite *hovering*)
		  '(0.0 0.0 0.0 1.0))
		 (t
		  '(1.0 1.0 1.0 1.0)))))
      (with-slots (x0 y0 x1 y1) absolute-rectangle
	(draw-quad x0 y0 
		   x1 y1)))))

#+nil
(defun render-tile (char-code x y background-color foreground-color)
  (color (byte/255 char-code)
	 (byte/255 background-color)
	 (byte/255 foreground-color))
  (vertex
   (floatify x)
   (floatify y)))
#+nil
;;a rainbow
(let ((count 0))
  (dotimes (x 16)
    (dotimes (y 16)
      (render-tile count x y count (- 255 count))
      (incf count))))

;;;more geometry
#+nil
(defun draw-quad (x0 y0 x1 y1)
  (destructuring-bind (r g b a) *pen-color*
    (color r g b a)
    (vertex x0 y0)
    (color r g b a)
    (vertex x0 y1)
    (color r g b a)
    (vertex x1 y1)
    (color r g b a)
    (vertex x1 y0)))
