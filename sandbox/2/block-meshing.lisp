(in-package :sandbox)



(defun dark-fun (darkness b0 b1 b2 b3 s0 s1 s2 s3)
  (let ((time daytime))
    (* darkness
       0.25
       (+ (max b0 (* time s0))
	  (max b1 (* time s1))
	  (max b2 (* time s2))
	  (max b3 (* time s3))))))

(defparameter *bluffs* nil)

(defun chunk-shape (chunk-position iter)
  (declare (optimize (debug 3)))
  (let ((buf (aplayground::get-buf-param iter
					 (etouq (vector 0 2 8)))))
    (aplayground::reset-attrib-buffer-iterators iter)
    (let ((times 0))
      (let ((*bluffs* buf))
	(multiple-value-bind (io ko jo) (world:unhashfunc chunk-position)
	  (dobox ((i io (+ 16 io))
		  (j jo (+ 16 jo))
		  (k ko (+ 16 ko)))
		 (let ((blockid (world:getblock i j k)))
		   (unless (zerop blockid)
		     (incf times
			   (blockshape
			    i j k
			    blockid)))))))
      (values buf times chunk-position iter))))


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
  (let ((times 0))
    (with-texture-translator (tu tv) 2
      (let ((adj-id (world:getblock i (1- j) k)))
	(when (show-sidep id adj-id)
	  (incf times 4)
	  (side-j i j k tu tv))))
    (with-texture-translator (tu tv) 0
      (let ((adj-id (world:getblock i (1+ j) k)))
	(when (show-sidep id adj-id)
	  (incf times 4)
	  (side+j i j k tu tv))))
    (with-texture-translator (tu tv) 3
      (let ((adj-id (world:getblock (1- i) j k)))
	(when (show-sidep id adj-id)
	  (incf times 4)
	  (side-i i j k tu tv)))
      (let ((adj-id (world:getblock (1+ i) j k)))
	(when (show-sidep id adj-id)
	  (incf times 4)
	  (side+i i j k tu tv)))    
      (let ((adj-id (world:getblock i j (1- k))))
	(when (show-sidep id adj-id)
	  (incf times 4)
	  (side-k i j k tu tv)))
      (let ((adj-id (world:getblock i j (1+ k))))
	(when (show-sidep id adj-id)
	  (incf times 4)
	  (side+k i j k tu tv))))
    times))

(defun renderstandardblock (id i j k)
  (let ((times 0))
    (with-texture-translator (tu tv) (aref mc-blocks::blockIndexInTexture id)
      (let ((adj-id (world:getblock i (1- j) k)))
	(when (show-sidep id adj-id)
	  (incf times 1)
	  (side-j i j k tu tv)))
      (let ((adj-id (world:getblock i (1+ j) k)))
	(when (show-sidep id adj-id)
	  (incf times 1)
	  (side+j i j k tu tv)))
      (let ((adj-id (world:getblock (1- i) j k)))
	(when (show-sidep id adj-id)
	  (incf times 1)
	  (side-i i j k tu tv)))
      (let ((adj-id (world:getblock (1+ i) j k)))
	(when (show-sidep id adj-id)
	  (incf times 1)
	  (side+i i j k tu tv)))    
      (let ((adj-id (world:getblock i j (1- k))))
	(when (show-sidep id adj-id)
	  (incf times 1)
	  (side-k i j k tu tv)))
      (let ((adj-id (world:getblock i j (1+ k))))
	(when (show-sidep id adj-id)
	  (incf times 1)
	  (side+k i j k tu tv))))
    (* 4 times)))

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
(eval-when (:compile-toplevel :execute)
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
	    (light-gen 0.05 x)))
    foo-array))


(declaim (inline lightfunc))
(defun lightfunc (light)
  (aref light-index-table light))


(declaim (inline trans))
(defun trans (foo foo-translator)
  (/ (+ foo foo-translator) 16.0))

(defmacro texface ((u0 v0)
		   (u1 v1)
		   (u2 v2)
		   (u3 v3))
  `(progn    
     (etex (coerce (trans ,u0 u-offset) 'single-float))
     (etex (coerce (trans ,v0 v-offset) 'single-float))     
     (etex (coerce (trans ,u1 u-offset) 'single-float))
     (etex (coerce (trans ,v1 v-offset) 'single-float))    
     (etex (coerce (trans ,u2 u-offset) 'single-float))
     (etex (coerce (trans ,v2 v-offset) 'single-float))   
     (etex (coerce (trans ,u3 u-offset) 'single-float))
     (etex (coerce (trans ,v3 v-offset) 'single-float))))

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
	    (zpos (+ k ,k0))

	    )
	(declare (type fixnum xpos ypos zpos))

	(dark (dark-fun
	       ,color
	       ,@light-edge-code
	       ,@sky-edge-code)))
      (let ((xpos (+ i ,i1))
	    (ypos (+ j ,j1))
	    (zpos (+ k ,k1))

	    )
	(declare (type fixnum xpos ypos zpos ))

	(dark (dark-fun
	       ,color
	       ,@light-edge-code
	       ,@sky-edge-code)))
      (let ((xpos (+ i ,i2))
	    (ypos (+ j ,j2))
	    (zpos (+ k ,k2))

	    )
	(declare (type fixnum xpos ypos zpos))

	(dark (dark-fun
	       ,color
	       ,@light-edge-code
	       ,@sky-edge-code)))

      
      (let ((xpos (+ i ,i3))
	    (ypos (+ j ,j3))
	    (zpos (+ k ,k3))

	    )
	(declare (type fixnum xpos ypos zpos ))

	(dark (dark-fun
	       ,color
	       ,@light-edge-code
	       ,@sky-edge-code))))
    
    ))))

(defmacro posface ((x0 y0 z0) 
		   (x1 y1 z1)		      
		   (x2 y2 z2)		     
		   (x3 y3 z3)) 
  `(progn
     (let (
	   (xp (+ i ,x0))
	   (yp (+ j ,y0))
	   (zp (+ k ,z0)))
       (declare (type fixnum xp yp zp))
       
       (epos (coerce xp 'single-float))
       (epos (coerce yp 'single-float))
       (epos (coerce zp 'single-float)) 
       )
     (let (
	   (xp (+ i ,x1))
	   (yp (+ j ,y1))
	   (zp (+ k ,z1)))
       (declare (type fixnum  xp yp zp))
       
       (epos (coerce xp 'single-float))
       (epos (coerce yp 'single-float))
       (epos (coerce zp 'single-float)) 
       )
     (let ((xp (+ i ,x2))
	   (yp (+ j ,y2))
	   (zp (+ k ,z2)))
       (declare (type fixnum  xp yp zp))
       (epos (coerce xp 'single-float))
       (epos (coerce yp 'single-float))
       (epos (coerce zp 'single-float)) 
       ) 
     (let (
	   (xp (+ i ,x3))
	   (yp (+ j ,y3))
	   (zp (+ k ,z3)))
       (declare (type fixnum  xp yp zp))
       (epos (coerce xp 'single-float))
       (epos (coerce yp 'single-float))
       (epos (coerce zp 'single-float)) )))



(with-unsafe-speed
  (progn
    (defun side-i (i j k u-offset v-offset)
      (declare (type fixnum i j k u-offset v-offset))
      (aplayground::with-iterators (epos etex dark)
	  *bluffs* iter-ator:wasabios iter-ator:iter-ator
	(posface (0 0 0)
		 (0 0 1)
		 (0 1 1)
		 (0 1 0))
	(texface (0 0)
		 (1 0)
		 (1 1)
		 (0 1))
	(squareface light-edge-i
		    skylight-edge-i
		    0.6
		    (-1 -1 -1)
		    (-1 -1 00)
		    (-1 00 00)
		    (-1 00 -1))))
    (defun side+i (i j k u-offset v-offset)
      (declare (type fixnum i j k u-offset v-offset))
      (aplayground::with-iterators (epos etex dark)
	  *bluffs* iter-ator:wasabios iter-ator:iter-ator
	(posface (1 0 0)
		 (1 1 0)
		 (1 1 1)
		 (1 0 1))
	(texface (1 0)
		 (1 1)
		 (0 1)
		 (0 0))
	(squareface light-edge-i
		    skylight-edge-i
		    0.6
		    (1 -1 -1)
		    (1 00 -1)
		    (1 00 00)
		    (1 -1 00))))
    (defun side-j (i j k u-offset v-offset)
      (declare (type fixnum i j k u-offset v-offset))
      (aplayground::with-iterators (epos etex dark)
	  *bluffs* iter-ator:wasabios iter-ator:iter-ator
	(posface (0 0 0)
		 (1 0 0)
		 (1 0 1)
		 (0 0 1))
	(texface (0 0)
		 (1 0)
		 (1 1)
		 (0 1)) 
	(squareface light-edge-j
		    skylight-edge-j
		    0.5		  
		    (-1 -1 -1)		   
		    (00 -1 -1)		  
		    (00 -1 00)		   
		    (-1 -1 00))))
    (defun side+j (i j k u-offset v-offset)
      (declare (type fixnum i j k u-offset v-offset))
      (aplayground::with-iterators (epos etex dark)
	  *bluffs* iter-ator:wasabios iter-ator:iter-ator
	(posface (0 1 0)
		 (0 1 1)
		 (1 1 1)
		 (1 1 0))
	(texface (0 0)
		 (1 0)
		 (1 1)
		 (0 1) )
	(squareface light-edge-j
		    skylight-edge-j
		    1.0
		    (-1 1 -1)
		    (-1 1 00)
		    (00 1 00)
		    (00 1 -1))))
    (defun side-k (i j k u-offset v-offset)
      (declare (type fixnum i j k u-offset v-offset))
      (aplayground::with-iterators (epos etex dark)
	  *bluffs* iter-ator:wasabios iter-ator:iter-ator
	(posface (0 0 0)
		 (0 1 0)
		 (1 1 0)
		 (1 0 0))
	(texface (1 0)
		 (1 1)
		 (0 1)
		 (0 0))
	(squareface light-edge-k
		    skylight-edge-k
		    0.8   
		    (-1 -1 -1)
		    (-1 00 -1)
		    (00 00 -1)
		    (00 -1 -1))))
    (defun side+k (i j k u-offset v-offset)
      (declare (type fixnum i j k u-offset v-offset))
      (aplayground::with-iterators (epos etex dark)
	  *bluffs* iter-ator:wasabios iter-ator:iter-ator
	(posface (0 0 1)
		 (1 0 1)
		 (1 1 1)
		 (0 1 1))
	(texface (0 0)
		 (1 0)
		 (1 1)
		 (0 1))
	(squareface light-edge-k
		    skylight-edge-k
		    0.8     
		    (-1 -1 1)
		    (00 -1 1)    
		    (00 00 1)    
		    (-1 00 1))))))

;;(defparameter shapebuffer-index 0)
;;(defparameter shapebuffer-vs (make-array (ash 2 20) :element-type 'single-float))

					;(declare (optimize (speed 3) (safety 0)))
;;  (declare (ignorable b0 b1 b2 b3))
#+nil
(defun add-to-shapebuffer (x y z u v darkness b0 b1 b2 b3 s0 s1 s2 s3)
  (declare (type (simple-array single-float *) shapebuffer-vs))
  (declare (type single-float x y z darkness))
  (declare (type (unsigned-byte 4) b0 b1 b2 b3 s0 s1 s2 s3))
					;(declare (optimize (speed 3) (safety 0)))
  (declare (ignorable b0 b1 b2 b3))
  (let ((vertlength shapebuffer-index))
    (declare (type fixnum vertlength))
    (let ((fp (* 6 vertlength)))
      (setf (aref shapebuffer-vs (+ fp 0)) x
	    (aref shapebuffer-vs (+ fp 1)) y
	    (aref shapebuffer-vs (+ fp 2)) z
	    (aref shapebuffer-vs (+ fp 3)) (coerce u 'single-float)
	    (aref shapebuffer-vs (+ fp 4)) (coerce v 'single-float)	    
	    (aref shapebuffer-vs (+ fp 5))
	    (* darkness
	       0.25
	       (+ (max b0 (* daytime s0))
		  (max b1 (* daytime s1))
		  (max b2 (* daytime s2))
		  (max b3 (* daytime s3)))))))
  (incf shapebuffer-index))


#+nil
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

#+nil
(progno
 (declaim (inline add-to-shapebuffer))
 (defun add-to-shapebuffer (x y z u v)
   (declare (type (simple-array single-float *) shapebuffer-vs))
   (declare (type single-float x y z darkness))
   (declare (type (unsigned-byte 4) b0 b1 b2 b3 s0 s1 s2 s3))

   (aplayground::with-iterators (epos etex dark)
       *bluffs* iter-ator:wasabios iter-ator:iter-ator
     (epos x)
     (epos y)
     (epos z)
     (etex (coerce u 'single-float))
     (etex (coerce v 'single-float))	    
     (dark
      ))))
