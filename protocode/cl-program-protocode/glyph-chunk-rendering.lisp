(in-package :sandbox)

(progno
 (defun generate-chunk-vertices-lookup (&optional
					  (char-width *block-width*)
					  (char-height *block-height*))
   (let ((chunk-size-x 16)
	 (chunk-size-y 16))
     (let ((array (make-array (* chunk-size-x chunk-size-y 4))))
       (let ((iter (iter-ator:make-iterator (length array) array nil nil)))	
	 (iter-ator:wasabios ((epos iter))
	   (dobox ((iy 0 chunk-size-y)
		   (ix 0 chunk-size-x))
		  (let ((foox0 (* (float ix) char-width))
			(fooy0 (* (float iy) char-height)))
		    (let ((x1 (+ foox0 char-width))
			  (y1 (+ fooy0 char-height)))
		      (etouq (ngorp (preach 'epos '(foox0 fooy0 x1 y1)))))))))
       (nreverse array))))

 (defparameter *chunk-vertices-lookup*
   (generate-chunk-vertices-lookup)))

(progno
 (let* ((solidshader (get-stuff :textshader *stuff* *backup*))
	(solidshader-uniforms (glget solidshader :program))
	(pmv (getuniform solidshader-uniforms :pmv))
	(sampler2d (getuniform solidshader-uniforms :sampler-2d)))
   (gl:use-program solidshader)
   (gl:uniformi sampler2d 1)
   (gl:active-texture (+ 1 +gltexture0+))
   (gl:bind-texture :texture-2d (get-stuff :font *stuff* *backup*))
   (draw-text-layers pmv)))

(progno
 (defmacro with-char-colors ((fg-rvar fg-gvar fg-bvar bg-rvar bg-gvar bg-bvar) value &body body)
   `(let ((,fg-rvar (ldb (byte 8 8) ,value))
	  (,fg-gvar (ldb (byte 8 16) ,value))
	  (,fg-bvar (ldb (byte 8 24) ,value))
	  (,bg-rvar (ldb (byte 8 32) ,value))
	  (,bg-gvar (ldb (byte 8 40) ,value))
	  (,bg-bvar (ldb (byte 8 48) ,value)))
      ,@body))

 (progn
   (declaim (ftype (function (simple-vector
			      simple-vector
			      simple-vector
			      fixnum
			      simple-vector
			      single-float single-float single-float)
			     fixnum)
		   draw-box-char))
   (with-unsafe-speed
     (defun draw-box-char (bufs
			   tilemap-lookup world
			   amount
			   mesh-lookup
			   x y z)
       (let ((nope 0)
	     (lookup +byte-fraction-lookup+))
	 (with-iterators (epos etex efg ebg) bufs iter-ator:wasabios iter-ator:iter-ator
	   (dobox ((index 0 amount))
		  (let ((obj (aref world index)))
		    (if obj
			(let ((value (get-char-num obj)))
			  (declare (type fixnum value))
			  (with-char-colors (xfg-r xfg-g xfg-b xbg-r xbg-g xbg-b) value
			    (etouq
			     (with-vec-params '((XFG-R FG-R)
						(XFG-G FG-G)
						(XFG-B FG-B)
						(XBG-R BG-R)
						(XBG-G BG-G)
						(XBG-B BG-B))
			       '(lookup)
			       '(dotimes (x 4)
				 (etouq (ngorp (preach 'efg '(fg-r fg-g fg-b))))
				 (etouq (ngorp (preach 'ebg '(bg-r bg-g bg-b))))))))
			  (let ((offset (* 4 (mod value 256))))
			    (etouq
			     (with-vec-params
				 `((offset ,@(vec-slots :rectangle
							'((x0 :x0) (y0 :y0) (x1 :x1) (y1 :y1)))))
			       '(tilemap-lookup)
			       '(etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1))))))))
			  (let ((offset (* 4 index)))
			    (etouq
			     (with-vec-params
				 `((offset ,@(vec-slots :rectangle
							'((x1 :x1) (y1 :y1) (foox0 :x0) (fooy0 :y0)))))
			       '(mesh-lookup)
			       '(declare (type single-float foox0 fooy0 x1 y1))
			       '(progn
				 (incf x1 x)
				 (incf y1 y)
				 (incf foox0 x)
				 (incf fooy0 y)
				 (etouq (ngorp (preach 'epos (quadk+ 'z '(foox0 x1 fooy0 y1))))))))))
			(incf nope)))))
	 (* 4 (- amount nope)))))))

(progno
 (defun mesh-test42 (times bufs)
   (with-iterators (xyz uv eft ebg) bufs iter-ator:wasabiis iter-ator:iter-ator
     (dotimes (x times)
       (%gl:vertex-attrib-2f 8 (uv) (uv))
       (%gl:vertex-attrib-4f 9 (eft) (eft) (eft) 1f0)
       (%gl:vertex-attrib-4f 10 (ebg) (ebg) (ebg) 1f0)
       (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz))))))


(progno
 (defun rescale-screen-matrix (result x y)
   (setf (cg-matrix:mref result 0 3) x
	 (cg-matrix:mref result 1 3) y)
   result)

 (defun draw-text-layers (pmv)
   (let ((auxvarw (/ *block-width* (- *window-width*)))
	 (auxvarh (/ *block-height* (- *window-height*))))
     (flet ((draw-window (rectangle world call-lists chunk-width chunk-height xoffset yoffset)
	      (rescale-screen-matrix *screen-scaled-matrix*
				     (* auxvarw xoffset)
				     (* auxvarh yoffset))
	      (gl:uniform-matrix-4fv pmv *screen-scaled-matrix* nil)
	      (draw-ensure world call-lists
			   rectangle
			   (floor chunk-width)
			   (floor chunk-height))))
       (draw-window *cam-rectangle* *chunks* *chunk-call-lists* 16 16 *camera-x* *camera-y*))))

 (progn
   (declaim (ftype (function (hash-table hash-table simple-vector fixnum fixnum))
		   draw-ensure))
   (with-unsafe-speed
     (defun draw-ensure (world call-lists rectangle chunk-width chunk-height)
       (etouq
	(with-vec-params (vec-slots :rectangle '((minx :x0) (miny :y0) (maxx :x1) (maxy :y1)))
	  '(rectangle)
	  '(declare (type single-float minx miny maxx maxy))
	  '(let ((x0 (floor minx chunk-width))
		 (x1 (1+ (floor maxx chunk-width)))
		 (y0 (floor miny chunk-height))
		 (y1 (1+ (floor maxy chunk-height))))
	    (declare (type fixnum x0 x1 y0 y1))
	    (dobox ((xstart x0 x1)
		    (ystart y0 y1))
	     (let ((index (pix:xy-index xstart ystart)))
	       (let ((thechunk (gethash index world)))
		 (when thechunk
		   (let ((chunk-timestamp (aref thechunk (* 16 16)))
			 (value (gethash index call-lists)))
		     (if (eq chunk-timestamp (cdr value))
			 (gl:call-list (car value))
			 (let ((mesh 
				(draw-16x16-page (* chunk-width xstart) (* chunk-height ystart)
						 thechunk (or (car value)
							      (gl:gen-lists 1)))))
			   (let ((new-value (if value
						(progn (setf (car value) mesh
							     (cdr value) chunk-timestamp)
						       value)
						(cons mesh chunk-timestamp))))
			     (setf (gethash index call-lists) new-value))
			   (gl:call-list mesh))))))))))))))

 (defun draw-16x16-page (xstart ystart thechunk &optional (display-list (gl:gen-lists 1)))
   (let ((iter *attrib-buffer-iterators*))
     (let ((buf (get-buf-param iter
			       (etouq (vector 0 8 9 10)))))
       (reset-attrib-buffer-iterators iter)
       (let ((times (draw-box-char
		     buf
		     *16x16-tilemap* thechunk
		     (* 16 16) *chunk-vertices-lookup*
		     (* *block-width* (float xstart)) 
		     (* *block-height* (float ystart)) 
		     +single-float-just-less-than-one+)))
	 (reset-attrib-buffer-iterators iter)
	 (gl:with-new-list (display-list :compile)
	   (gl:with-primitives :quads
	     (mesh-test42 times buf)))
	 display-list)))))

(progno
       (namexpr backup :textshader
		(lambda ()
		  (let ((program
			 (make-shader-program-from-strings
			  (get-stuff :text-vs *stuff* *backup*)
			  (get-stuff :text-frag *stuff* *backup*)
			  (quote (("POS" . 0)	
				  ("TEX" . 8)
				  ("FGCOL" . 9)
				  ("BGCOL" . 10)
				  )))))
		    (let ((table (make-eq-hash)))
		      (register program :program table)
		      (cache-program-uniforms program table (quote ((:pmv . "PMV")
								    (:sampler-2d . "samptwodee")))))
		    program)))
       
       (namexpr backup :text-vs
		(lambda () (file-string (shader-path "pos4f-tex2f-bgcol4f-fgcol4f.vs")))))

(progno
   (when *clear-display-buffer*
     (gl:clear :color-buffer-bit)))

(progno
 (defparameter *clear-display-buffer* t))

(progno
 (defparameter *screen-scaled-matrix* (cg-matrix:identity-matrix)))

(progno
   (cg-matrix:%scale* *screen-scaled-matrix*
		      (/ 1.0 w)
		      (/ 1.0 h)
		      1.0))
