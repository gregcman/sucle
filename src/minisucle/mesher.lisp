(defpackage #:mesher
  (:use :cl :utility)
  (:export
   :mesh-chunk
   :block-shape
   :draw-dispatch
   :with-texture-translator2)
  (:export
   :side-i
   :side+i
   :side-j
   :side+j
   :side-k
   :side+k))
(in-package :mesher)

(defparameter *mesh-etex* nil)
(defparameter *mesh-dark* nil)
(defparameter *mesh-epos* nil)

(defgeneric draw-dispatch (obj i j k))
(with-unsafe-speed
  (defun mesh-chunk (iter io jo ko)
    (declare (type voxel-chunks:block-coord io jo ko))
    (with-vec (*mesh-epos* *mesh-etex* *mesh-dark*) (iter)
      ;;(draw-dispatch (voxel-chunks:getobj io jo ko) io jo ko)
      ;;#+nil
      (dobox ((i io (the voxel-chunks:block-coord (+ 16 io)))
	      (j jo (the voxel-chunks:block-coord (+ 16 jo)))
	      (k ko (the voxel-chunks:block-coord (+ 16 ko))))
	     (draw-dispatch (voxel-chunks:getobj i j k) i j k)))))

(eval-always
  ;;;;total faces touched by a light of distance n
  (defun manhattan-perimeter (n)
    (let ((n (+ n 1)))
      (+ (* n n
	    4)
	 2)))

  (defun light-gen-aux-fun (x &optional (max 15))
    (log
     (/
      (/ 1 (manhattan-perimeter (- max x)))
      (/ 1 (manhattan-perimeter max)))))
  #+nil
  (defun gamma-correct (x &optional (gamma-value 2.33))
    (expt x gamma-value))
  
  (defun light-gen (x &optional (max 15))
    ;;Ripped from minecraft beta WorldProvider.java
    #+nil
    (let* ((f 0.05)
	   (i x)
	   (f1 (- 1.0 (/ i 15.0))))
      (+ (* (/ (- 1.0 f1)
	       (+ (* f1 3.0) 1.0))
	    (- 1.0 f))
	 f))
    
    ;;#+nil
    (let ((umm (/ 1.0 (light-gen-aux-fun max max))))
      (*
       (light-gen-aux-fun x max)
       umm))
    
    #+nil
    (gamma-correction:gamma-correct
     #+nil
     (expt 0.8 (- 15 x))
     (let ((a (/ x 15)))
       (* a a))
     1.0)
    )

  (defparameter *light-index-table*
    (let ((foo-array (make-array 16 :element-type 'single-float)))
      (dotimes (x 16)
	(setf (aref foo-array x)
	      (floatify (light-gen x))))
      foo-array)))

(declaim (inline lightfunc))
(defun lightfunc (light)
  (aref (etouq *light-index-table*) light))

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

#+nil
(defun dark-fun (darkness b0 b1 b2 b3 s0 s1 s2 s3)
  (let ((time *daytime*))
    (* darkness
       0.25
       (+ (max b0 (* time s0))
	  (max b1 (* time s1))
	  (max b2 (* time s2))
	  (max b3 (* time s3))))))

(defmacro squareface (((x0 y0 z0)
		       (x1 y1 z1)
		       (x2 y2 z2)
		       (x3 y3 z3))
		      color	      
		      (i0 j0 k0)		      
		      (i1 j1 k1)		      
		      (i2 j2 k2)		      
		      (i3 j3 k3))
  `(flet ((add-edge (i0 j0 k0)
	    (let ((xpos (+ i i0))
		  (ypos (+ j j0))
		  (zpos (+ k k0)))
	      (declare (type voxel-chunks:block-coord xpos ypos zpos))
	      ,(with-gensyms (actual-color)
		 (flet ((%edge-aux (getfunc q0 q1 q2 q3 &rest body)
			  `(flet ((value (i j k)
				    (let ((xd (+ i xpos))
					  (yd (+ j ypos))
					  (zd (+ k zpos)))
				      (declare (type voxel-chunks:block-coord xd yd zd))
				      (* ,actual-color (lightfunc (,getfunc xd yd zd))))))
			     (let ((,q0 (value ,x0 ,y0 ,z0))
				   (,q1 (value ,x1 ,y1 ,z1))
				   (,q2 (value ,x2 ,y2 ,z2))
				   (,q3 (value ,x3 ,y3 ,z3)))
			       ,@body))))
		   `(let ((,actual-color ,color))
		      (declare (type single-float ,actual-color))
		      ,(%edge-aux
			'world:getlight 'q0 'q1 'q2 'q3 
			(%edge-aux
			 'world:skygetlight 'q4 'q5 'q6 'q7
			 ;;Write block light and sky light values
			 `(dark q0 q1 q2 q3 q4 q5 q6 q7)
			 #+nil
			 ;;Write block light and sky light values precomputed into one float,
			 ;;as opposed to calculating light in the vertex shader
			 `(dark (dark-fun ,actual-color q0 q1 q2 q3 q4 q5 q6 q7))))))))))
     (add-edge ,i0 ,j0 ,k0)
     (add-edge ,i1 ,j1 ,k1)
     (add-edge ,i2 ,j2 ,k2)
     (add-edge ,i3 ,j3 ,k3)))

(defmacro posface ((x0 y0 z0) 
		   (x1 y1 z1)		      
		   (x2 y2 z2)		     
		   (x3 y3 z3)) 
  `(flet ((add (x y z)
	    (let ((xp (+ i x))
		  (yp (+ j y))
		  (zp (+ k z)))
	      (declare (type voxel-chunks:block-coord xp yp zp))
	      (epos (floatify xp)
		    (floatify yp)
		    (floatify zp)))))
     (add ,x0 ,y0 ,z0)
     (add ,x1 ,y1 ,z1)
     (add ,x2 ,y2 ,z2)
     (add ,x3 ,y3 ,z3)))

(defmacro face-header (name &body body)
  `(defun ,name (i j k u0 v0 u1 v1)
     (declare (type voxel-chunks:block-coord i j k)
	      (type single-float u0 v0 u1 v1))
     (scratch-buffer:bind-out* ((*mesh-epos* epos)
				(*mesh-etex* etex)
				(*mesh-dark* dark)) 
      ,@body)))

(eval-always
  (defun simple-float-array (&rest args)
    (make-array (length args) :initial-contents args :element-type 'single-float))
  (defparameter *blockface-color*  
    (simple-float-array 0.6 0.6 0.5 1.0 0.8 0.8)
    ;;(simple-float-array 0.55 0.95 0.2 1.0 0.45 0.85)
    ;;#+nil
    ;;(simple-float-array 1.0 1.0 1.0 1.0 1.0 1.0)
    #+nil
    (let ((side (expt 0.5 2.33)))
      (simple-float-array side side 0.0 1.0 side side))
    ))

(etouq
  (let
    ((light-edge-i
	'((0 1 1)
	  (0 0 1)
	  (0 0 0)
	  (0 1 0)))
     (light-edge-j   
       '((1 0 1)
	 (0 0 1)
	 (0 0 0)
	 (1 0 0)))
     (light-edge-k
       '((1 1 0)
	 (0 1 0)
	 (0 0 0)
	 (1 0 0))))
    `(#+(not (or sbcl ecl))
	progn ;;ccl
	#+(or sbcl ecl)
	with-unsafe-speed
	(face-header side-i  
	  (posface (0 0 0)
		   (0 0 1)
		   (0 1 1)
		   (0 1 0))
	  (texface2 u0 u1 v0 v1 3 nil)
	  (squareface ,light-edge-i
		      (etouq (aref *blockface-color* 0))
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
	  (squareface ,light-edge-i
		      (etouq (aref *blockface-color* 1))
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
	  (squareface ,light-edge-j
		      (etouq (aref *blockface-color* 2))
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
	  (squareface ,light-edge-j
		      (etouq (aref *blockface-color* 3))
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
	  (squareface ,light-edge-k
		      (etouq (aref *blockface-color* 4))
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
	  (squareface ,light-edge-k
		      (etouq (aref *blockface-color* 5))
		      (-1 -1 1)
		      (00 -1 1)    
		      (00 00 1)    
		      (-1 00 1))))))

(defmacro with-texture-translator2 ((u0 u1 v0 v1) num-form &body body)
  (let ((id (gensym)))
    `(let ((,id (* 4 ,num-form)))
       ,(apply #'with-vec-params `((,id ,u0 ,v0 ,u1 ,v1)) `(,*16x16-tilemap*)
	       body))))

(eval-always
  (defparameter *16x16-tilemap* (rectangular-tilemap:regular-enumeration 16 16)))
