(in-package :sandbox)

(defparameter *daytime* 1.0)

(defun dark-fun (darkness b0 b1 b2 b3 s0 s1 s2 s3)
  (let ((time *daytime*))
    (* darkness
       0.25
       (+ (max b0 (* time s0))
	  (max b1 (* time s1))
	  (max b2 (* time s2))
	  (max b3 (* time s3))))))

(defparameter *mesh-etex* nil)
(defparameter *mesh-dark* nil)
(defparameter *mesh-epos* nil)

(defun chunk-shape (chunk-position iter)
  (declare (optimize (debug 3)))
  (with-vec (*mesh-epos* *mesh-etex* *mesh-dark*) (iter)
    (multiple-value-bind (io ko jo) (world:unhashfunc chunk-position)
      (dobox ((i io (+ 16 io))
	      (j jo (+ 16 jo))
	      (k ko (+ 16 ko)))
	     (let ((blockid (world:getblock i j k)))
	       (unless (zerop blockid)
		 (blockshape
		  i j k
		  blockid)))))
    (values chunk-position iter)))


(defmacro %edge-aux ((i j k)
		     getfunc
		     (x0 y0 z0)
		     (x1 y1 z1)
		     (x2 y2 z2)
		     (x3 y3 z3))
  `((let ((xd (+ ,i ,x0))
	  (yd (+ ,j ,y0))
	  (zd (+ ,k ,z0)))
      (declare (type fixnum xd yd zd))
      (lightfunc (,getfunc xd yd zd)))
    (let ((xd (+ ,i ,x1))
	  (yd (+ ,j ,y1))
	  (zd (+ ,k ,z1)))
      (declare (type fixnum xd yd zd))
      (lightfunc (,getfunc xd yd zd)))
    (let ((xd (+ ,i ,x2))
	  (yd (+ ,j ,y2))
	  (zd (+ ,k ,z2)))
      (declare (type fixnum xd yd zd))
      (lightfunc (,getfunc xd yd zd)))
    (let ((xd (+ ,i ,x3))
	  (yd (+ ,j ,y3))
	  (zd (+ ,k ,z3)))
      (declare (type fixnum xd yd zd))
      (lightfunc (,getfunc xd yd zd)))))
(eval-when (:compile-toplevel)
  (progn
    (defun light-edge-i (i j k)
      (macroexpand-1
       `(%edge-aux (,i ,j ,k)
		   world:getlight
		   (0 1 1)
		   (0 0 1)
		   (0 0 0)
		   (0 1 0))))
    (defun light-edge-j (i j k)
      (macroexpand-1
       `(%edge-aux (,i ,j ,k)
		   world:getlight 
		   (1 0 1)
		   (0 0 1)
		   (0 0 0)
		   (1 0 0))))
    (defun light-edge-k (i j k)
      (macroexpand-1
       `(%edge-aux (,i ,j ,k)
		   world:getlight
		   (1 1 0)
		   (0 1 0)
		   (0 0 0)
		   (1 0 0))))
    (defun skylight-edge-i (i j k)
      (macroexpand-1
       `(%edge-aux (,i ,j ,k)
		   world:skygetlight
		   (0 1 1)
		   (0 0 1)
		   (0 0 0)
		   (0 1 0))))
    (defun skylight-edge-j (i j k)
      (macroexpand-1
       `(%edge-aux (,i ,j ,k)
		   world:skygetlight
		   (1 0 1)
		   (0 0 1)
		   (0 0 0)
		   (1 0 0))))
    (defun skylight-edge-k (i j k)
      (macroexpand-1
       `(%edge-aux (,i ,j ,k)
		   world:skygetlight
		   (1 1 0)
		   (0 1 0)
		   (0 0 0)
		   (1 0 0))))))

(eval-when (:compile-toplevel)
  ;;0.9 for nether???
  ;;0.8 for overworld(in-package :sandbox)???
  (defun light-gen? (f i)
    (expt (+ 0.7 (* 2 f)) (- 15 i)))

  ;;;overworld f = 0.05
;;;nether f = 0.1
  (defun light-gen (f i)
    (+ f
       (/ (* i (- 1 f))
	  (- 60 i i i))))
  (defparameter light-index-table
    (let ((foo-array (make-array 16 :element-type 'single-float)))
      (dotimes (x 16)
	(setf (aref foo-array x)
	      (light-gen? 0.05 x)))
      #+nil
      (let ((a (aref foo-array 0)))
	(map-into foo-array
		  (lambda (x)
		    (/ (- x a) (- 1 a)))
		  foo-array))
      foo-array)))

(declaim (inline lightfunc))
(defun lightfunc (light)
  (aref (etouq light-index-table) light))

(defmacro texface2 (u0 u1 v0 v1 &optional (start 1) (clockwise-winding nil))
  ((lambda (&rest forms)
     (cons (quote progn)
	   (apply (function nconc) forms)))
   ((lambda (value form)
      (mapcar (lambda (x)
		(list value x))
	      form))
    'etex
    (axis-aligned-quads:duaq
     start
     clockwise-winding
     (list u0 u1 v0 v1)))))

(defmacro squareface (light-edge-fnc
		      skylight-edge-fnc
		      color	      
		      (i0 j0 k0)		      
		      (i1 j1 k1)		      
		      (i2 j2 k2)		      
		      (i3 j3 k3))
  (let ((light-edge-code (funcall (symbol-function light-edge-fnc) 'xpos 'ypos 'zpos))
	(sky-edge-code (funcall (symbol-function skylight-edge-fnc) 'xpos 'ypos 'zpos)))
    `(progn
      (let ((xpos (+ i ,i0))
	    (ypos (+ j ,j0))
	    (zpos (+ k ,k0)))
	(declare (type fixnum xpos ypos zpos))
	(dark (dark-fun
	       ,color
	       ,@light-edge-code
	       ,@sky-edge-code)))
      (let ((xpos (+ i ,i1))
	    (ypos (+ j ,j1))
	    (zpos (+ k ,k1)))
	(declare (type fixnum xpos ypos zpos ))
	(dark (dark-fun
	       ,color
	       ,@light-edge-code
	       ,@sky-edge-code)))
      (let ((xpos (+ i ,i2))
	    (ypos (+ j ,j2))
	    (zpos (+ k ,k2)))
	(declare (type fixnum xpos ypos zpos))
	(dark (dark-fun
	       ,color
	       ,@light-edge-code
	       ,@sky-edge-code)))     
      (let ((xpos (+ i ,i3))
	    (ypos (+ j ,j3))
	    (zpos (+ k ,k3)))
	(declare (type fixnum xpos ypos zpos ))
	(dark (dark-fun
	       ,color
	       ,@light-edge-code
	       ,@sky-edge-code))))))

(defmacro posface ((x0 y0 z0) 
		   (x1 y1 z1)		      
		   (x2 y2 z2)		     
		   (x3 y3 z3)) 
  `(progn
     (let ((xp (+ i ,x0))
	   (yp (+ j ,y0))
	   (zp (+ k ,z0)))
       (declare (type fixnum xp yp zp))     
       (epos (coerce xp 'single-float))
       (epos (coerce yp 'single-float))
       (epos (coerce zp 'single-float)))
     (let ((xp (+ i ,x1))
	   (yp (+ j ,y1))
	   (zp (+ k ,z1)))
       (declare (type fixnum  xp yp zp))      
       (epos (coerce xp 'single-float))
       (epos (coerce yp 'single-float))
       (epos (coerce zp 'single-float)))
     (let ((xp (+ i ,x2))
	   (yp (+ j ,y2))
	   (zp (+ k ,z2)))
       (declare (type fixnum  xp yp zp))
       (epos (coerce xp 'single-float))
       (epos (coerce yp 'single-float))
       (epos (coerce zp 'single-float)) ) 
     (let ((xp (+ i ,x3))
	   (yp (+ j ,y3))
	   (zp (+ k ,z3)))
       (declare (type fixnum  xp yp zp))
       (epos (coerce xp 'single-float))
       (epos (coerce yp 'single-float))
       (epos (coerce zp 'single-float)))))

(defmacro face-header (name &body body)
  `(defun ,name (i j k u0 v0 u1 v1)
     (declare (type fixnum i j k)
	      (type single-float u0 v0 u1 v1))
     (iterator:bind-iterator-out
      (epos single-float) *mesh-epos*
      (iterator:bind-iterator-out
       (etex single-float) *mesh-etex*
       (iterator:bind-iterator-out
	(dark single-float) *mesh-dark*	
	,@body)))))

(with-unsafe-speed
  (face-header side-i  
    (posface (0 0 0)
	     (0 0 1)
	     (0 1 1)
	     (0 1 0))
    (texface2 u0 u1 v0 v1 3 nil)
    (squareface light-edge-i
		skylight-edge-i
		0.6
		(-1 -1 -1)
		(-1 -1 00)
		(-1 00 00)
		(-1 00 -1)))
  (face-header side+i  
    (posface (1 0 0)
	     (1 1 0)
	     (1 1 1)
	     (1 0 1))
    (texface2 u0 u1 v0 v1 4 nil)
    (squareface light-edge-i
		skylight-edge-i
		0.6
		(1 -1 -1)
		(1 00 -1)
		(1 00 00)
		(1 -1 00)))
  (face-header side-j
    (posface (0 0 0)
	     (1 0 0)
	     (1 0 1)
	     (0 0 1))
    (texface2 u0 u1 v0 v1 3 nil) 
    (squareface light-edge-j
		skylight-edge-j
		0.5		  
		(-1 -1 -1)		   
		(00 -1 -1)		  
		(00 -1 00)		   
		(-1 -1 00)))
  (face-header side+j 
    (posface (0 1 0)
	     (0 1 1)
	     (1 1 1)
	     (1 1 0))
    (texface2 u0 u1 v0 v1 3 nil)
    (squareface light-edge-j
		skylight-edge-j
		1.0
		(-1 1 -1)
		(-1 1 00)
		(00 1 00)
		(00 1 -1)))
  (face-header side-k 
    (posface (0 0 0)
	     (0 1 0)
	     (1 1 0)
	     (1 0 0))
    (texface2 u0 u1 v0 v1 4 nil)
    (squareface light-edge-k
		skylight-edge-k
		0.8   
		(-1 -1 -1)
		(-1 00 -1)
		(00 00 -1)
		(00 -1 -1)))
  (face-header side+k
    (posface (0 0 1)
	     (1 0 1)
	     (1 1 1)
	     (0 1 1))
    (texface2 u0 u1 v0 v1 3 nil)
    (squareface light-edge-k
		skylight-edge-k
		0.8     
		(-1 -1 1)
		(00 -1 1)    
		(00 00 1)    
		(-1 00 1))))

(defun blockshape (i j k blockid)
  (case blockid
    (2 (rendergrass blockid i j k))
    (t (renderstandardblock blockid i j k))))

;;;if the block is air, the side gets rendered. if the block is transparent
;;;and the same type ex: texture between glass - there is no texture - if they are
;;;different - water and glass -it shows
(defun show-sidep (blockid other-blockid)
  (or (zerop other-blockid)
      (and (/= blockid other-blockid)
	   (not (aref mc-blocks:*opaquecubelooukup* other-blockid)))))

(defmacro with-texture-translator2 ((u0 u1 v0 v1) num-form &body body)
  (let ((id (gensym)))
    `(let ((,id (* 4 ,num-form)))
       ,(apply #'with-vec-params `((,id ,u0 ,v0 ,u1 ,v1)) `(,*16x16-tilemap*)
	       body))))

(eval-when (:compile-toplevel)
  (defparameter *16x16-tilemap* (rectangular-tilemap:regular-enumeration 16 16)))

(defun rendergrass (id i j k)
  (with-texture-translator2 (u0 u1 v0 v1) 2
    (let ((adj-id (world:getblock i (1- j) k)))
      (when (show-sidep id adj-id)
	(side-j i j k u0 v0 u1 v1))))
  (with-texture-translator2 (u0 u1 v0 v1) 0
    (let ((adj-id (world:getblock i (1+ j) k)))
      (when (show-sidep id adj-id)
	(side+j i j k u0 v0 u1 v1))))
  (with-texture-translator2 (u0 u1 v0 v1) 3
    (let ((adj-id (world:getblock (1- i) j k)))
      (when (show-sidep id adj-id)
	(side-i i j k u0 v0 u1 v1)))
    (let ((adj-id (world:getblock (1+ i) j k)))
      (when (show-sidep id adj-id)
	(side+i i j k u0 v0 u1 v1)))    
    (let ((adj-id (world:getblock i j (1- k))))
      (when (show-sidep id adj-id)
	(side-k i j k u0 v0 u1 v1)))
    (let ((adj-id (world:getblock i j (1+ k))))
      (when (show-sidep id adj-id)
	(side+k i j k u0 v0 u1 v1)))))

(defun renderstandardblock (id i j k)
  (let ((texid (aref mc-blocks:*blockIndexInTexture* id)))
    (with-texture-translator2 (u0 u1 v0 v1) texid
      (let ((adj-id (world:getblock i (1- j) k)))
	(when (show-sidep id adj-id)
	  (side-j i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock i (1+ j) k)))
	(when (show-sidep id adj-id)
	  (side+j i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock (1- i) j k)))
	(when (show-sidep id adj-id)
	  (side-i i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock (1+ i) j k)))
	(when (show-sidep id adj-id)
	  (side+i i j k u0 v0 u1 v1)))    
      (let ((adj-id (world:getblock i j (1- k))))
	(when (show-sidep id adj-id)
	  (side-k i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock i j (1+ k))))
	(when (show-sidep id adj-id)
	  (side+k i j k u0 v0 u1 v1))))))
