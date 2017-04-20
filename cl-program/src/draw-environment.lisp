(in-package :sandbox)


(defparameter vsync? t)

(defparameter *mat4-identity* (cg-matrix:identity-matrix))

(defconstant +single-float-just-less-than-one+ 0.99999997)

(defparameter *16x16-tilemap* (regular-enumeration 16 16))
(defparameter *4x4-tilemap* (regular-enumeration 4 4))

(defparameter *screen-scaled-matrix* (cg-matrix:identity-matrix))

(defparameter *attrib-buffers* (fill-with-flhats (make-attrib-buffer-data)))
(defparameter *attrib-buffer-iterators*
  (make-iterators *attrib-buffers* (make-attrib-buffer-data)))
(defparameter *attrib-buffer-fill-pointer*
  (tally-buffer *attrib-buffer-iterators* (make-attrib-buffer-data)))

(defun make-eq-hash ()
  (make-hash-table :test (quote eq)))

(defparameter *backup* (make-eq-hash))
(defparameter *stuff* (make-eq-hash))

(defparameter *default-tex-params* (quote ((:texture-min-filter . :nearest)
					   (:texture-mag-filter . :nearest)
					   (:texture-wrap-s . :repeat)
					   (:texture-wrap-t . :repeat))))

(defparameter *postex* (quote (("POS" . 0)	
			       ("TEX" . 8)
			       ("FGCOL" . 9)
			       ("BGCOL" . 10)
			       )))

(defparameter *clear-display-buffer* nil)

(defun render ()
  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  (draw-things)

  (window:update-display))

(defun draw-things ()
  (gl:disable :depth-test :blend)
  (gl:depth-mask :false)
  (gl:depth-func :always)
  (gl:clear-color 0.0 0.0 0.0 0f0)
  (when *clear-display-buffer*
    (gl:clear :color-buffer-bit))
  
  (gl:viewport 0 0 e:*width* e:*height*)
  
  (let* ((solidshader (get-stuff :textshader *stuff* *backup*))
	 (solidshader-uniforms (glget solidshader :program)))
    (gl:use-program solidshader)
    (cg-matrix:%scale* *screen-scaled-matrix* (/ 1.0 e:*width*) (/ 1.0 e:*height*) 1.0) 
    (gl:uniform-matrix-4fv
     (getuniform solidshader-uniforms :pmv)
     (cg-matrix:%matrix* *temp-matrix2*
			 *screen-scaled-matrix*
			 (cg-matrix:%translate* *temp-matrix*
						(- *camera-x*)
						(- *camera-y*)
						0.0))
     nil))
  (gl:bind-texture :texture-2d (get-stuff :font *stuff* *backup*))

  (progn
   (let ((x0 (floor *window-min-x-block* *chunk-width*))
	 (x1 (floor *window-max-x-block* *chunk-width*))
	 (y0 (floor *window-min-y-block* *chunk-height*))
	 (y1 (floor *window-max-y-block* *chunk-height*)))
     (dobox ((x-index x0 (1+ x1))
	     (y-index y0 (1+ y1)))
	    (let ((xstart (* *chunk-width* x-index))
		  (ystart (* *chunk-height* y-index)))
	 ;     (print (list xstart ystart))
	      (let ((index (pix:xy-index xstart ystart)))
		(multiple-value-bind (value exists) (gethash index *chunk-call-lists*)
		  (declare (ignorable exists))
		  (if value
		      (gl:call-list value)
		      (if (gethash index *chunks*)
			  (let ((mesh (funcall
				       (quad-mesh 
					(lambda (tex-buf pos-buf fg-buf bg-buf)
					  (let ((times 
						 (draw-box-char
						  pos-buf tex-buf
						  *16x16-tilemap* *chunks*
						  xstart (+ xstart *chunk-width*)
						  ystart (+ ystart *chunk-height*)
						  *block-width*
						  *block-height*
						  +single-float-just-less-than-one+)))
					    
					    (attrib-repeat fg-buf times
							   (map-into *vec3-scratch* (lambda () (random 1f0))))
					    (attrib-repeat bg-buf times
							   (map-into *vec3-scratch* (lambda () (random 1f0))))
					    times))))))
			    (setf (gethash index *chunk-call-lists*) mesh)
			    (gl:call-list mesh)))))))))))

(defparameter *vec3-scratch* (vector 1f0 1f0 1f0))

(progn
  (declaim (ftype (function (iter-ator:iter-ator fixnum (simple-array t)))
		  attrib-repeat))
  (with-unsafe-speed
    (defun attrib-repeat (buf times vector)
      (iter-ator:wasabios ((ebuf buf))
	(let ((len (length vector)))
	  (dotimes (x times)
	    (dotimes (index len)
	      (let ((value (aref vector index)))
		(ebuf value)))))))))

(defun quad-mesh (func)
  (lambda ()
    (create-call-list-from-func
     (lambda ()
       (gl-draw-quads func)))))


(defun glinnit ()
  (reset *gl-objects*)
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (clrhash *chunk-call-lists*)
  
  (let ((width (if t 480 854))
	(height (if t 360 480)))
    (window:push-dimensions width height))
  (setf e:*resize-hook* #'on-resize)

  (let ((hash *stuff*))
    (maphash (lambda (k v)
	       (if (integerp v)
		   (remhash k hash)))
	     hash))
  (let ((backup *backup*))
    (progn    
      (namexpr backup :textshader
	       (lambda ()
		 (let ((program
			(make-shader-program-from-strings
			 (get-stuff :text-vs *stuff* *backup*)
			 (get-stuff :text-frag *stuff* *backup*)
			 *postex*)))
		   (let ((table (make-eq-hash)))
		     (register program :program table)
		     (cache-program-uniforms program table (quote ((:pmv . "PMV")))))
		   program)))
     
      (namexpr backup :text-vs
	       (lambda () (file-string (shader-path "pos4f-tex2f-bgcol3f-fgcol3f.vs"))))
      
      (namexpr backup :text-frag
	       (lambda () (file-string (shader-path "ftex2f-bg3f-fg3f.frag")))))
    
    (progn
      (namexpr backup :font-image
	       (lambda ()
		 (flip-image
		  (load-png
		   (img-path #P"font/font.png")))))
      (namexpr backup :font
	       (lambda ()
		 (pic-texture (get-stuff :font-image *stuff* *backup*)
			      :rgba
			      *default-tex-params*))))))

(defun cache-program-uniforms (program table args)
  (dolist (arg args)
    (setf (gethash (car arg) table)
	  (gl:get-uniform-location program (cdr arg)))))
(defun getuniform (shader-info name)
  (gethash name shader-info))

(defun on-resize (w h)
  (setf *window-height* h
	*window-width* w))


(defun gl-draw-quads (func)
  (let ((iter *attrib-buffer-iterators*))
    (reset-attrib-buffer-iterators iter)
    (let ((pos-buf (aref iter 0))
	  (tex-buf (aref iter 8))
	  (fg-buf (aref iter 9))
	  (bg-buf (aref iter 10)))
      (let ((times (funcall func tex-buf pos-buf fg-buf bg-buf)))
	(gl:with-primitives :quads
	  (reset-attrib-buffer-iterators iter)
	  (mesh-test42 times tex-buf pos-buf fg-buf bg-buf))))))

(defun mesh-test42 (times tex pos fg bg)
  (declare (type iter-ator:iter-ator tex pos))
  (iter-ator:wasabiis ((uv tex)
		       (xyz pos)
		       (eft fg)
		       (ebg bg))
    (dotimes (x times)
      (%gl:vertex-attrib-2f 8 (uv) (uv))
      (%gl:vertex-attrib-3f 9 (eft) (eft) (eft))
      (%gl:vertex-attrib-3f 10 (ebg) (ebg) (ebg))
      (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz)))))

(progn
  (declaim (ftype (function (iter-ator:iter-ator iter-ator:iter-ator
						 simple-vector
						 pix:pix-world
						 fixnum fixnum fixnum fixnum
						 single-float single-float single-float)
			    fixnum)
		  draw-box-char))
  (with-unsafe-speed
    (defun draw-box-char (pos-buf tex-buf
			  lookup world
			  bx0 bx1 by0 by1
			  char-width char-height
			  z)
      (iter-ator:wasabios ((epos pos-buf)
			   (etex tex-buf))
	(dobox ((ix bx0 bx1)
		(iy by0 by1))
	       (let ((char (pix:get-obj (pix:xy-index ix iy) world)))
		 (let ((code (if char
				 (char-code char)
				 0)))
		   (multiple-value-bind (x0 y0 x1 y1) (index-quad-lookup lookup code)
		     (etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1)))))))
		 (let ((foox0 (* (float ix) char-width))
		       (fooy0 (* (float iy) char-height)))
		   (etouq (ngorp
			   (preach
			    'epos
			    (quadk+ 'z '(foox0
					 (+ foox0 char-width)
					 fooy0
					 (+ fooy0 char-height))))))))))
      (* 4 (- bx1 bx0) (- by1 by0)))))
