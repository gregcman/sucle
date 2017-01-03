(in-package :sandbox)

(defparameter shapebuffer (make-shape))

(defun chunk-shape (chunk-position)
  (declare (optimize (speed 3)))
  (multiple-value-bind (io ko jo) (world:unhashfunc chunk-position)
    (let* ((new-shape (destroy-shape shapebuffer)))
      (dorange
       (i io 16)
       (dorange
	(j jo 16)
	(dorange
	 (k ko 16)
	 (let ((blockid (world:getblock i j k)))
	   (if (not (zerop blockid))
	       (let ((fineshape
		      (blockshape
		       i j k
		       blockid)))
		 (reduce
		  #'add-verts
		  fineshape
		  :initial-value new-shape)))))))
      new-shape)))

(defun blockshape (i j k blockid)
  (ret faces (renderstandardblock blockid i j k)))

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
	   (let ((,y-name (- 15.0 (ash (- ,num-form-sym ,x-name) -4))))
	     ,@body))))))


(defun trans (foo foo-translator)
  (/ (+ foo foo-translator) 16.0))


(defun renderstandardblock (id i j k)
  (with-texture-translator (tu tv) (aref mc-blocks::blockIndexInTexture id)
      (ret faces nil
	(let ((adj-id (world:getblock i (1- j) k)))
	  (when (show-sidep id adj-id) 
	    (let ((newvert (side-j i j k tu tv)))
	      (push newvert faces))))
	(let ((adj-id (world:getblock i (1+ j) k)))
	  (when (show-sidep id adj-id) 
	    (let ((newvert (side+j i j k tu tv)))
	      (push newvert faces))))
	(let ((adj-id (world:getblock (1- i) j k)))
	  (when (show-sidep id adj-id) 
	    (let ((newvert (side-i i j k tu tv)))
	      (push newvert faces))))
	(let ((adj-id (world:getblock (1+ i) j k)))
	  (when (show-sidep id adj-id) 
	    (let ((newvert (side+i i j k tu tv)))
	      (push newvert faces))))
	(let ((adj-id (world:getblock i j (1- k))))
	  (when (show-sidep id adj-id)
	    (let ((newvert (side-k i j k tu tv)))
	      (push newvert faces))))
	(let ((adj-id (world:getblock i j (1+ k))))
	  (when (show-sidep id adj-id) 
	    (let ((newvert (side+k i j k tu tv)))
	      (push newvert faces)))))))

(defmacro %edge-aux (getfunc
		     (x0 y0 z0)
		     (x1 y1 z1)
		     (x2 y2 z2)
		     (x3 y3 z3))
  `(vector
    (lightfunc (,getfunc (+ i ,x0) (+ j ,y0) (+ k ,z0)))
    (lightfunc (,getfunc (+ i ,x1) (+ j ,y1) (+ k ,z1)))
    (lightfunc (,getfunc (+ i ,x2) (+ j ,y2) (+ k ,z2)))
    (lightfunc (,getfunc (+ i ,x3) (+ j ,y3) (+ k ,z3)))))

(defun light-edge-i (i j k)
  (%edge-aux world:getlight
	     (0 1 1)
	     (0 0 1)
	     (0 0 0)
	     (0 1 0)))
(defun light-edge-j (i j k)
  (%edge-aux world:getlight
	     (1 0 1)
	     (0 0 1)
	     (0 0 0)
	     (1 0 0)))
(defun light-edge-k (i j k)
  (%edge-aux world:getlight
	     (1 1 0)
	     (0 1 0)
	     (0 0 0)
	     (1 0 0)))
(defun skylight-edge-i (i j k)
  (%edge-aux world:skygetlight
	     (0 1 1)
	     (0 0 1)
	     (0 0 0)
	     (0 1 0)))
(defun skylight-edge-j (i j k)
  (%edge-aux world:skygetlight
	     (1 0 1)
	     (0 0 1)
	     (0 0 0)
	     (1 0 0)))
(defun skylight-edge-k (i j k)
  (%edge-aux world:skygetlight
	     (1 1 0)
	     (0 1 0)
	     (0 0 0)
	     (1 0 0)))

(defun lightfunc (light)
  (expt 0.8 (- 15 light)))

;;0.9 for nether
;;0.8 for overworld(in-package :sandbox)

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
  `(list
    (let ((xpos (+ i ,i0))
	  (ypos (+ j ,j0))
	  (zpos (+ k ,k0)))
      (vertex
       (pos (+ i ,x0) (+ j ,y0) (+ k ,z0)) (uv (trans ,u0 u-offset) (trans ,v0 v-offset))
       (opgray ,color)   
       (,light-edge-fnc xpos ypos zpos)
       (,skylight-edge-fnc xpos ypos zpos)))
    (let ((xpos (+ i ,i1))
	  (ypos (+ j ,j1))
	  (zpos (+ k ,k1)))
      (vertex
       (pos (+ i ,x1) (+ j ,y1) (+ k ,z1)) (uv (trans ,u1 u-offset) (trans ,v1 v-offset))
       (opgray ,color)
       (,light-edge-fnc xpos ypos zpos)
       (,skylight-edge-fnc xpos ypos zpos)))
    (let ((xpos (+ i ,i2))
	  (ypos (+ j ,j2))
	  (zpos (+ k ,k2)))
      (vertex
       (pos (+ i ,x2) (+ j ,y2) (+ k ,z2)) (uv (trans ,u2 u-offset) (trans ,v2 v-offset))
       (opgray ,color)
       (,light-edge-fnc xpos ypos zpos)
       (,skylight-edge-fnc xpos ypos zpos)))
    (let ((xpos (+ i ,i3))
	  (ypos (+ j ,j3))
	  (zpos (+ k ,k3)))
      (vertex
       (pos (+ i ,x3) (+ j ,y3) (+ k ,z3)) (uv (trans ,u3 u-offset) (trans ,v3 v-offset))
       (opgray ,color)
       (,light-edge-fnc xpos ypos zpos)
       (,skylight-edge-fnc xpos ypos zpos)))))

(defun side-i (i j k u-offset v-offset)
  (squareface light-edge-i
	      skylight-edge-i
	      0.6
	      (-0.5 -0.5 -0.5) (0.0 0.0) (-1 -1 -1)
	      (-0.5 -0.5  0.5) (1.0 0.0) (-1 -1 00)
	      (-0.5  0.5  0.5) (1.0 1.0) (-1 00 00)
	      (-0.5  0.5 -0.5) (0.0 1.0) (-1 00 -1)))
(defun side+i (i j k u-offset v-offset) 
  (squareface light-edge-i
	      skylight-edge-i
	      0.6
	      (0.5 -0.5 -0.5) (1.0 0.0) (1 -1 -1)
	      (0.5  0.5 -0.5) (1.0 1.0) (1 00 -1)
	      (0.5  0.5  0.5) (0.0 1.0) (1 00 00)
	      (0.5 -0.5  0.5) (0.0 0.0) (1 -1 00)))
(defun side-j (i j k u-offset v-offset)
  (squareface light-edge-j
	      skylight-edge-j
	      0.5
	      (-0.5 -0.5 -0.5) (0.0 0.0) (-1 -1 -1)
	      ( 0.5 -0.5 -0.5) (1.0 0.0) (00 -1 -1)
	      ( 0.5 -0.5  0.5) (1.0 1.0) (00 -1 00)
	      (-0.5 -0.5  0.5) (0.0 1.0) (-1 -1 00)))
(defun side+j (i j k u-offset v-offset)
  (squareface light-edge-j
	      skylight-edge-j
	      1.0
	      (-0.5 0.5 -0.5) (0.0 0.0) (-1 1 -1)
	      (-0.5 0.5  0.5) (1.0 0.0) (-1 1 00)
	      ( 0.5 0.5  0.5) (1.0 1.0) (00 1 00)
	      ( 0.5 0.5 -0.5) (0.0 1.0) (00 1 -1)))
(defun side-k (i j k u-offset v-offset)
  (squareface light-edge-k
	      skylight-edge-k
	      0.8   
	      (-0.5 -0.5 -0.5) (1.0 0.0) (-1 -1 -1)
	      (-0.5  0.5 -0.5) (1.0 1.0) (-1 00 -1)
	      ( 0.5  0.5 -0.5) (0.0 1.0) (00 00 -1)
	      ( 0.5 -0.5 -0.5) (0.0 0.0) (00 -1 -1)))
(defun side+k (i j k u-offset v-offset)
  (squareface  light-edge-k
	       skylight-edge-k
	       0.8     
	       (-0.5 -0.5  0.5) (0.0 0.0) (-1 -1 1)
	       ( 0.5 -0.5  0.5) (1.0 0.0) (00 -1 1)    
	       ( 0.5  0.5  0.5) (1.0 1.0) (00 00 1)    
	       (-0.5  0.5  0.5) (0.0 1.0) (-1 00 1)))


(defun opgray (val)
  (rgba val val val 1.0))
(defun vertex (&rest args)
  (make-array (length args) :initial-contents args))
(defun rgba (r g b a)
  (vector r g b a))
(defun pos (x y z)
  (vector x y z))
(defun uv (u v)
  (vector u v))
