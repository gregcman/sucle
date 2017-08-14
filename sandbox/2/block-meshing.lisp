(in-package :sandbox)

(defparameter shapebuffer-index 0)
(defparameter shapebuffer-vs (make-array (ash 2 20) :element-type 'single-float))

(declaim (inline add-to-shapebuffer))
(defun add-to-shapebuffer (x y z u v darkness b0 b1 b2 b3 s0 s1 s2 s3)
  (declare (type (simple-array single-float *) shapebuffer-vs))
  (declare (type single-float x y z darkness b0 b1 b2 b3 s0 s1 s2 s3))
  ;(declare (optimize (speed 3) (safety 0)))
  (let ((vertlength shapebuffer-index))
    (declare (type fixnum vertlength))
    (let ((fp (* 6 vertlength)))
      (setf (aref shapebuffer-vs (+ fp 0)) x
	    (aref shapebuffer-vs (+ fp 1)) y
	    (aref shapebuffer-vs (+ fp 2)) z
	    (aref shapebuffer-vs (+ fp 3)) (coerce u 'single-float)
	    (aref shapebuffer-vs (+ fp 4)) (coerce v 'single-float)	    
	    (aref shapebuffer-vs (+ fp 5)) (* 0.25
					      darkness
					      (+ (max b0 (* daytime s0))
						 (max b1 (* daytime s1))
						 (max b2 (* daytime s2))
						 (max b3 (* daytime s3)))))))
  (incf shapebuffer-index))

(defun chunk-shape (chunk-position)
  (declare (optimize (speed 3)))
  (multiple-value-bind (io ko jo) (world:unhashfunc chunk-position)
    (setf shapebuffer-index 0)
    (let ((new-shape shapebuffer-vs))
      (dobox ((i io (+ 16 io))
	      (j jo (+ 16 jo))
	      (k ko (+ 16 ko)))
	     (let ((blockid (world:getblock i j k)))
	       (unless (zerop blockid)
		 (blockshape
		  i j k
		  blockid))))
      (values
       new-shape
       shapebuffer-index
       chunk-position))))

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
	   (not (aref mc-blocks::opaquecubelooukup other-blockid)))))

(defmacro with-texture-translator ((x-name y-name) num-form &body body)
  (let ((num-form-sym (gensym)))
    `(progn
       (let ((,num-form-sym ,num-form))
	 (let ((,x-name (mod ,num-form-sym 16)))
	   (let ((,y-name (- 15 (ash (- ,num-form-sym ,x-name) -4))))
	     ,@body))))))

(defun rendergrass (id i j k)
  (with-texture-translator (tu tv) 2
    (let ((adj-id (world:getblock i (1- j) k)))
      (when (show-sidep id adj-id) 
	(side-j i j k tu tv))))
  (with-texture-translator (tu tv) 0
    (let ((adj-id (world:getblock i (1+ j) k)))
      (when (show-sidep id adj-id) 
	(side+j i j k tu tv))))
  (with-texture-translator (tu tv) 3
    (let ((adj-id (world:getblock (1- i) j k)))
      (when (show-sidep id adj-id) 
	(side-i i j k tu tv)))
    (let ((adj-id (world:getblock (1+ i) j k)))
      (when (show-sidep id adj-id) 
	(side+i i j k tu tv)))    
    (let ((adj-id (world:getblock i j (1- k))))
      (when (show-sidep id adj-id)
	(side-k i j k tu tv)))
    (let ((adj-id (world:getblock i j (1+ k))))
      (when (show-sidep id adj-id) 
	(side+k i j k tu tv)))))

(defun renderstandardblock (id i j k)
  (with-texture-translator (tu tv) (aref mc-blocks::blockIndexInTexture id)
    (let ((adj-id (world:getblock i (1- j) k)))
      (when (show-sidep id adj-id) 
	(side-j i j k tu tv)))
    (let ((adj-id (world:getblock i (1+ j) k)))
      (when (show-sidep id adj-id) 
	(side+j i j k tu tv)))
    (let ((adj-id (world:getblock (1- i) j k)))
      (when (show-sidep id adj-id) 
	(side-i i j k tu tv)))
    (let ((adj-id (world:getblock (1+ i) j k)))
      (when (show-sidep id adj-id) 
	(side+i i j k tu tv)))    
    (let ((adj-id (world:getblock i j (1- k))))
      (when (show-sidep id adj-id)
	(side-k i j k tu tv)))
    (let ((adj-id (world:getblock i j (1+ k))))
      (when (show-sidep id adj-id) 
	(side+k i j k tu tv)))))

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

(defparameter light-index-table
  (let ((foo-array (make-array 16 :element-type 'single-float)))
    (dotimes (x 16)
      (setf (aref foo-array x) (expt 0.8 (- 15 x))))
    foo-array))

(setf (aref light-index-table 15) 1.0)

(declaim (inline lightfunc))
(defun lightfunc (light)
  (aref light-index-table light))

;;0.9 for nether
;;0.8 for overworld(in-package :sandbox)

(declaim (inline trans))
(defun trans (foo foo-translator)
  (/ (+ foo foo-translator) 16.0))

(defmacro squareface (light-edge-fnc
		      skylight-edge-fnc
		      color
		      (x0 y0 z0)
		      (u0 v0)
		      (i0 j0 k0)
		      (x1 y1 z1)
		      (u1 v1)
		      (i1 j1 k1)
		      (x2 y2 z2)
		      (u2 v2)
		      (i2 j2 k2)
		      (x3 y3 z3)
		      (u3 v3)
		      (i3 j3 k3))
  `(progn
     (let ((xpos (+ i ,i0))
	   (ypos (+ j ,j0))
	   (zpos (+ k ,k0))
	   (xp (+ i ,x0))
	   (yp (+ j ,y0))
	   (zp (+ k ,z0)))
       (declare (type fixnum xpos ypos zpos xp yp zp))
       (add-to-shapebuffer
	(coerce xp 'single-float) (coerce yp 'single-float) (coerce zp 'single-float) 
	(trans ,u0 u-offset) (trans ,v0 v-offset)
	,color
	,@(funcall (symbol-function light-edge-fnc) 'xpos 'ypos 'zpos)
	,@(funcall (symbol-function skylight-edge-fnc) 'xpos 'ypos 'zpos)))
     (let ((xpos (+ i ,i1))
	   (ypos (+ j ,j1))
	   (zpos (+ k ,k1))
	   (xp (+ i ,x1))
	   (yp (+ j ,y1))
	   (zp (+ k ,z1)))
       (declare (type fixnum xpos ypos zpos xp yp zp))
       (add-to-shapebuffer
	(coerce xp 'single-float) (coerce yp 'single-float) (coerce zp 'single-float) 
	(trans ,u1 u-offset) (trans ,v1 v-offset)
	,color
	,@(funcall (symbol-function light-edge-fnc) 'xpos 'ypos 'zpos)
	,@(funcall (symbol-function skylight-edge-fnc) 'xpos 'ypos 'zpos)))
     (let ((xpos (+ i ,i2))
	   (ypos (+ j ,j2))
	   (zpos (+ k ,k2))
	   (xp (+ i ,x2))
	   (yp (+ j ,y2))
	   (zp (+ k ,z2)))
       (declare (type fixnum xpos ypos zpos xp yp zp))
       (add-to-shapebuffer
	(coerce xp 'single-float) (coerce yp 'single-float) (coerce zp 'single-float) 
	(trans ,u2 u-offset) (trans ,v2 v-offset)
	,color
	,@(funcall (symbol-function light-edge-fnc) 'xpos 'ypos 'zpos)
	,@(funcall (symbol-function skylight-edge-fnc) 'xpos 'ypos 'zpos)))
     (let ((xpos (+ i ,i3))
	   (ypos (+ j ,j3))
	   (zpos (+ k ,k3))
	   (xp (+ i ,x3))
	   (yp (+ j ,y3))
	   (zp (+ k ,z3)))
       (declare (type fixnum xpos ypos zpos xp yp zp))
       (add-to-shapebuffer
	(coerce xp 'single-float) (coerce yp 'single-float) (coerce zp 'single-float) 
	(trans ,u3 u-offset) (trans ,v3 v-offset)
	,color
	,@(funcall (symbol-function light-edge-fnc) 'xpos 'ypos 'zpos)
	,@(funcall (symbol-function skylight-edge-fnc) 'xpos 'ypos 'zpos)))))

(progn
  (defun side-i (i j k u-offset v-offset)
    (declare (type fixnum i j k u-offset v-offset))
    (declare (optimize (speed 3) (safety 0)))
    (squareface light-edge-i
		skylight-edge-i
		0.6
		(0 0 0) (0 0) (-1 -1 -1)
		(0 0 1) (1 0) (-1 -1 00)
		(0 1 1) (1 1) (-1 00 00)
		(0 1 0) (0 1) (-1 00 -1)))
  (defun side+i (i j k u-offset v-offset)
    (declare (type fixnum i j k u-offset v-offset))
    (declare (optimize (speed 3) (safety 0)))
    (squareface light-edge-i
		skylight-edge-i
		0.6
		(1 0 0) (1 0) (1 -1 -1)
		(1 1 0) (1 1) (1 00 -1)
		(1 1 1) (0 1) (1 00 00)
		(1 0 1) (0 0) (1 -1 00)))
  (defun side-j (i j k u-offset v-offset)
    (declare (type fixnum i j k u-offset v-offset))
    (declare (optimize (speed 3) (safety 0)))
    (squareface light-edge-j
		skylight-edge-j
		0.5
		(0 0 0) (0 0) (-1 -1 -1)
		(1 0 0) (1 0) (00 -1 -1)
		(1 0 1) (1 1) (00 -1 00)
		(0 0 1) (0 1) (-1 -1 00)))
  (defun side+j (i j k u-offset v-offset)
    (declare (type fixnum i j k u-offset v-offset))
    (declare (optimize (speed 3) (safety 0)))
    (squareface light-edge-j
		skylight-edge-j
		1.0
		(0 1 0) (0 0) (-1 1 -1)
		(0 1 1) (1 0) (-1 1 00)
		(1 1 1) (1 1) (00 1 00)
		(1 1 0) (0 1) (00 1 -1)))
  (defun side-k (i j k u-offset v-offset)
    (declare (type fixnum i j k u-offset v-offset))
    (declare (optimize (speed 3) (safety 0)))
    (squareface light-edge-k
		skylight-edge-k
		0.8   
		(0 0 0) (1 0) (-1 -1 -1)
		(0 1 0) (1 1) (-1 00 -1)
		(1 1 0) (0 1) (00 00 -1)
		(1 0 0) (0 0) (00 -1 -1)))
  (defun side+k (i j k u-offset v-offset)
    (declare (type fixnum i j k u-offset v-offset))
    (declare (optimize (speed 3) (safety 0)))
    (squareface light-edge-k
		skylight-edge-k
		0.8     
		(0 0 1) (0 0) (-1 -1 1)
		(1 0 1) (1 0) (00 -1 1)    
		(1 1 1) (1 1) (00 00 1)    
		(0 1 1) (0 1) (-1 00 1))))
