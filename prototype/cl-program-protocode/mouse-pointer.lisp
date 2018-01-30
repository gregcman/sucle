(in-package :sandbox)

(progno
 (progn
   (defun draw-mouse (bufs
		      lookup value char-width char-height x y z)
     (declare (type single-float x y z char-width char-height)
	      (type simple-vector lookup)
	      (optimize (speed 3) (safety 0))
	      (type fixnum value))
     (with-iterators (epos etex) bufs iter-ator:wasabios iter-ator:iter-ator
       (let ((offset (* value 4)))
	 (etouq
	  (with-vec-params `((offset ,@(vec-slots :rectangle '((x0 :x0) (y0 :y0) (x1 :x1) (y1 :y1)))))
	    '(lookup)
	    '(etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1))))))))
       (let ((x1 (+ char-width x))
	     (y0 (- y char-height)))
	 (etouq (ngorp (preach 'epos (quadk+ 'z '(x x1 y0 y))))))
       4))
   
   (defun render-mouse (pmv)       
     (rescale-screen-matrix *screen-scaled-matrix*
			    (* *one-over-window-width* *mouse-x*)
			    (* *one-over-window-height* *mouse-y*))       
     (gl:uniform-matrix-4fv pmv *screen-scaled-matrix* nil)
     (gl:call-list (get-stuff :cursor-list *stuff* *backup*)))))

(progno
   (let* ((simpleshader (get-stuff :simpleshader *stuff* *backup*))
	  (simpleshader-uniforms (glget simpleshader :program))
	  (pmv (getuniform simpleshader-uniforms :pmv)))
     (gl:use-program simpleshader)
     (gl:bind-texture :texture-2d (get-stuff :cursor *stuff* *backup*))
     (render-mouse pmv)))

(progno (progn
	      (namexpr backup :cursor-image
		       (lambda ()
			 (flip-image
			  (load-png
			   (img-path #P"cursor/windos-cursor.png")))))
	      (namexpr backup :cursor
		       (lambda ()
			 (pic-texture (get-stuff :cursor-image *stuff* *backup*)
				      :rgba
				      *default-tex-params*)))
	      (let ((scale 64.0))
		(namexpr *backup* :cursor-list
			 (lambda ()
			   (create-call-list-from-func
			    (lambda ()
			      (let ((iter *attrib-buffer-iterators*))
				(let ((buf (get-buf-param iter (etouq (vector 0 8 9)))))
				  (reset-attrib-buffer-iterators iter)
				  (attrib-repeat (aref buf 2) 4 (vector 1f0 1f0 1f0))
				  (let ((times (draw-mouse
						buf
						*4x4-tilemap* 0
						scale 
						scale
						-2.0 2.0
						(- +single-float-just-less-than-one+))))
				    (reset-attrib-buffer-iterators iter)
				    (gl:with-primitives :quads
				      (mesh-test4269 times buf)))))))))))

	    (progn

	      (namexpr backup :simpleshader
		       (lambda ()
			 (let ((program
				(make-shader-program-from-strings
				 (get-stuff :simple-vs *stuff* *backup*)
				 (get-stuff :simple-frag *stuff* *backup*)
				 *postexcol*)))
			   (let ((table (make-eq-hash)))
			     (register program :program table)
			     (cache-program-uniforms program table (quote ((:pmv . "PMV")))))
			   program)))
	      (namexpr backup :simple-vs
		       (lambda () (file-string (shader-path "pos4f-col3f-tex2f.vs"))))
	      (namexpr backup :simple-frag
		       (lambda () (file-string (shader-path "fcol3f-ftex2f-no0a.frag"))))))

(progno (when (e:mice-locked-p)))

(progno
 (defparameter old-mouse-x 0)
 (defparameter old-mouse-y 0)
 (defun delta ()
   (multiple-value-bind (newx newy) (window:get-mouse-position)
     (multiple-value-prog1 (values
			    (- newx old-mouse-x)
			    (- newy old-mouse-y))
       (setf old-mouse-x newx
	     old-mouse-y newy))))
 (defun next-mouse-state (x y sensitivity)
   (multiple-value-bind (dx dy) (delta)
     (let ((width e:*width*)
	   (height e:*height*))
       (let ((deltax (* sensitivity dx))
	     (deltay (* sensitivity dy)))
	 (values (clamp (+ x deltax) (- width) (- width 2.0))
		 (clamp (- y deltay) (+  2.0 (- height)) height)))))))

(progno
  (defparameter *mouse-sensitivity* (coerce 2.0 'single-float)))

(progno (next-mouse-state *mouse-x* *mouse-y* *mouse-sensitivity*))

(progno
 (defun mesh-test4269 (times bufs)
   (with-iterators (xyz uv eft) bufs iter-ator:wasabiis iter-ator:iter-ator
     (dotimes (x times)
       (%gl:vertex-attrib-2f 8 (uv) (uv))
       (%gl:vertex-attrib-3f 9 (eft) (eft) (eft))
       (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz))))))

(progno (defparameter *4x4-tilemap* (regular-enumeration 4 4)))
