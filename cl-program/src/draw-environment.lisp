(in-package :sandbox)


(defconstant +single-float-just-less-than-one+ 0.99999997)

(defparameter *16x16-tilemap* (regular-enumeration 16 16))

(defparameter +byte-fraction-lookup+
  (let ((array (make-array 256 :element-type 'single-float)))
    (dotimes (x 256)
      (setf (aref array x) (/ (float x) 255.0)))
    array))

(progn
  (defparameter *block-height* (/ (* 2 11.0) 1.0))
  (defparameter *block-width* (/ (* 2 6.0) 1.0)))

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
  (generate-chunk-vertices-lookup))

(defparameter *screen-scaled-matrix* (cg-matrix:identity-matrix))

(defparameter *attrib-buffers* (fill-with-flhats (make-attrib-buffer-data)))
(defparameter *attrib-buffer-iterators*
  (make-iterators *attrib-buffers* (make-attrib-buffer-data)))
(defparameter *attrib-buffer-fill-pointer*
  (tally-buffer *attrib-buffer-iterators* (make-attrib-buffer-data)))

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

(defparameter *postexcol* (quote (("POS" . 0)	
				  ("TEX" . 8)
				  ("COL" . 9))))

(defparameter *clear-display-buffer* t)
(defparameter vsync? t)

(defun render ()
  (draw-things)

  (window:update-display))

(defparameter *window-width* nil)
(defparameter *window-height* nil)

(defun draw-things ()
  (when *clear-display-buffer*
    (gl:clear :color-buffer-bit)) 

  (let* ((solidshader (get-stuff :textshader *stuff* *backup*))
	 (solidshader-uniforms (glget solidshader :program))
	 (pmv (getuniform solidshader-uniforms :pmv)))
    (gl:use-program solidshader)
    (gl:bind-texture :texture-2d (get-stuff :font *stuff* *backup*))
    (draw-text-layers pmv)))

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
	display-list))))

(defmacro with-iterators ((&rest bufvars) buf func type &body body)
  (let* ((syms (mapcar (lambda (x) (gensym (string x))) bufvars))
	 (bindings (mapcar (lambda (x y) (list x y))
			   bufvars syms))
	 (decl `(declare (type ,type ,@syms))))
    (with-vec-params syms `(,buf)
      decl
      `(,func ,bindings
	      ,@body))))

(defun glinnit ()
  (reset *gl-objects*)
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (clrhash *chunk-call-lists*)

  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  
  (let ((width (if t 480 854))
	(height (if t 360 480)))
    (window:push-dimensions width height))
  (setf e:*resize-hook* #'on-resize)

  (progn
    (gl:clear-color 0.0
		    (aref +byte-fraction-lookup+ 8)
		    (aref +byte-fraction-lookup+ 16) 0f0)
    (gl:disable :depth-test :blend)
    (gl:depth-mask :false)
    (gl:depth-func :always)
    (gl:disable :cull-face))

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
	       (lambda () (file-string (shader-path "pos4f-tex2f-bgcol4f-fgcol4f.vs"))))      
      (namexpr backup :text-frag
	       (lambda () (file-string (shader-path "ftex2f-bg4f-fg4f.frag")))))
    
    (progn
      (namexpr backup :font-image
	       (lambda ()
		 (flip-image
		  (load-png
		   (saves-path #P"achar.png")))))
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
	*window-width* w)
  (cg-matrix:%scale* *screen-scaled-matrix*
		     (/ 1.0 w)
		     (/ 1.0 h)
		     1.0)
  (gl:viewport 0 0 w h))

(defparameter *buffer-vector-scratch* (make-array 16))

(defun get-buf-param (iter attrib-order &optional (newarray *buffer-vector-scratch*))
  (let ((len (length attrib-order)))
    (dotimes (x len)
      (setf (aref newarray x)
	    (aref iter (aref attrib-order x)))))
  newarray)

(defun mesh-test42 (times bufs)
  (with-iterators (xyz uv eft ebg) bufs iter-ator:wasabiis iter-ator:iter-ator
    (dotimes (x times)
      (%gl:vertex-attrib-2f 8 (uv) (uv))
      (%gl:vertex-attrib-4f 9 (eft) (eft) (eft) 1f0)
      (%gl:vertex-attrib-4f 10 (ebg) (ebg) (ebg) 1f0)
      (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz)))))


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
	(* 4 (- amount nope))))))
