(in-package :sandbox)

;;i- i+ j- j+ k- k+

(defmacro progno (&rest args) (declare (ignore args)))
(defparameter blockfaces
  (vector
   (lambda ()
     (list
      (vertex
       (pos -0.5 -0.5 -0.5) (uv 0.0 0.0) (opgray 0.6) (blocklight) (skylight))
      (vertex
       (pos -0.5 -0.5  0.5)  (uv 1.0 0.0) (opgray 0.6) (blocklight) (skylight))
      (vertex
       (pos -0.5  0.5  0.5)  (uv 1.0 1.0) (opgray 0.6) (blocklight) (skylight))
      (vertex
       (pos -0.5  0.5 -0.5) (uv 0.0 1.0) (opgray 0.6) (blocklight) (skylight))))
   (lambda () 
     (list
      (vertex
       (pos 0.5 -0.5 -0.5)  (uv 0.0 0.0) (opgray 0.6) (blocklight) (skylight))
      (vertex
       (pos 0.5  0.5 -0.5)  (uv 0.0 1.0) (opgray 0.6) (blocklight) (skylight))
      (vertex
       (pos 0.5  0.5  0.5)  (uv 1.0 1.0) (opgray 0.6) (blocklight) (skylight))
      (vertex
       (pos 0.5 -0.5  0.5)  (uv 1.0 0.0) (opgray 0.6) (blocklight) (skylight))))
   (lambda ()
     (list
      (vertex
       (pos -0.5 -0.5 -0.5)  (uv 0.0 0.0) (opgray 0.5) (blocklight) (skylight))
      (vertex
       (pos 0.5 -0.5 -0.5)  (uv 1.0 0.0)  (opgray 0.5) (blocklight) (skylight))
      (vertex
       (pos 0.5 -0.5  0.5)  (uv 1.0 1.0)  (opgray 0.5) (blocklight) (skylight))
      (vertex
       (pos -0.5 -0.5  0.5)  (uv 0.0 1.0) (opgray 0.5) (blocklight) (skylight))))
   (lambda ()
     (list
      (vertex
       (pos -0.5 0.5 -0.5) (uv 0.0 0.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos -0.5 0.5 0.5) (uv 0.0 1.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos 0.5 0.5 0.5) (uv 1.0 1.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos 0.5 0.5 -0.5) (uv 1.0 0.0) (opgray 1.0) (blocklight) (skylight))))
   (lambda ()
     (list
      (vertex
       (pos -0.5 -0.5 -0.5) (uv 0.0 0.0) (opgray 0.8) (blocklight) (skylight))
      (vertex
       (pos -0.5 0.5 -0.5) (uv 0.0 1.0) (opgray 0.8)(blocklight) (skylight))
      (vertex
       (pos 0.5 0.5 -0.5) (uv 1.0 1.0) (opgray 0.8) (blocklight) (skylight))
      (vertex
       (pos 0.5 -0.5 -0.5) (uv 1.0 0.0) (opgray 0.8) (blocklight) (skylight))))
   (lambda ()
     (list
      (vertex
       (pos -0.5 -0.5  0.5)  (uv 0.0 0.0)(opgray 0.8) (blocklight) (skylight))
      (vertex
       (pos 0.5 -0.5  0.5)  (uv 1.0 0.0) (opgray 0.8) (blocklight) (skylight))
      (vertex
       (pos 0.5  0.5  0.5)  (uv 1.0 1.0) (opgray 0.8)(blocklight) (skylight))
      (vertex
       (pos -0.5  0.5  0.5)  (uv 0.0 1.0) (opgray 0.8) (blocklight) (skylight))))))

(defun skylight ()
  (vector 0.0))
(defun blocklight ()
  (vector 0.0))
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

;;current layout: 3 position floats, 2 texcoord floats, 4 color floats

(defun increment-verts (x y z verts)
  "linear translation of vertices"
  (dolist (n verts)
    (let ((pos (elt n 0)))
      (incf (aref pos 0) x)
      (incf (aref pos 1) y)
      (incf (aref pos 2) z)))
  verts)

(defun fuck-verts (r g b a verts)
  (dolist (n verts)
    (let ((color (elt n 2)))
      (incf (aref color 0) r)
      (incf (aref color 1) g)
      (incf (aref color 2) b)
      (incf (aref color 3) a)))
  verts)

(defun cunt-verts (r g b a verts)
  (dolist (n verts)
    (cunt-vert r g b a (elt n 2)))
  verts)

(defun cunt-vert (r g b a n)
  "colorize a vertex"
  (setf (aref n 0) (* (aref n 0) r))
  (setf (aref n 1) (* (aref n 1) g))
  (setf (aref n 2) (* (aref n 2) b))
  (setf (aref n 3) (* (aref n 3) a))
  n)

(defun %damn-fuck (verts num)
  "converts 0-1 texcoords to terrain.png coords"
  (let* ((xtrans (mod num 16))
	 (ytrans (- 15 (/ (- num xtrans) 16))))
    (dolist (vim verts)
      (let ((v (elt vim 1)))
	(setf (aref v 0) (+ (/ (aref v 0) 16) (/ xtrans 16)))
	(setf (aref v 1) (+ (/ (aref v 1) 16) (/  ytrans 16)))))
    verts))

(defparameter shapebuffer (make-shape))

(defmacro dorange ((var start length) &rest body)
  (let ((temp (gensym))
	(temp2 (gensym))
	(tempstart (gensym))
	(templength (gensym)))
    `(block nil
       (let* ((,templength ,length)
	      (,tempstart ,start)
	      (,var ,tempstart))
	 (declare (type signed-byte ,var))
	 (tagbody
	    (go ,temp2)
	    ,temp
	    (tagbody ,@body)
	    (psetq ,var (1+ ,var))
	    ,temp2
	    (unless (>= ,var (+ ,tempstart ,templength)) (go ,temp))
	    (return-from nil (progn nil)))))))

(defun chunk-shape (io jo ko)
  "turn a chunk into a shape, complete
with positions, textures, and colors. no normals"
  (let* ((new-shape (destroy-shape shapebuffer)))
    (dorange
     (i (* io 16) 16)
     (dorange
      (j (* jo 16) 16)
      (dorange
       (k (* ko 16) 16)
       (let ((blockid (getblock i j k)))
	 (if (not (zerop blockid))
	     (let ((fineshape
		    (blockshape
		     io jo ko
		     blockid
		     (lambda (a b c)
		       (getblock (+ a i) (+ b j) (+ c k)))
		     (lambda (a b c)
		       (getlight (+ a i) (+ b j) (+ c k)))
		     (lambda (a b c)
		       (skygetlight (+ a i) (+ b j) (+ c k))))))
	       (dolist (face (coerce (delete nil fineshape) 'list))
		 (increment-verts i j k face))
	       (reduce
		#'add-verts
		fineshape
		:initial-value new-shape)))))))
    new-shape))

(defun blockshape (i j k blockid getempty betlight getskylightz)
  (let ((faces
	 (case (aref mc-blocks::getrendertype blockid)
	   (0 (renderstandardblock blockid getempty betlight getskylightz))
	   (1 (renderblockreed blockid i j k betlight getskylightz))
	   (t (make-array 6 :initial-element nil)))))
  
    (if (= blockid 2)
	(let ((ourfunc (aref mc-blocks::getblocktexture blockid)))
	  (dotimes (n 6)
	    (let ((newvert (aref faces n)))
	      (%damn-fuck newvert (funcall ourfunc n)))))
	(let ((the-skin (aref mc-blocks::blockIndexInTexture blockid)))
	  (dotimes (n (length faces))
	    (let ((newvert (aref faces n)))
	      (%damn-fuck newvert the-skin)))))
    (let ((colorizer (aref mc-blocks::colormultiplier blockid)))
      (if (functionp colorizer)
	  (if (= 2 blockid)
	      (let ((face (elt faces 1)))
		(colorize face (funcall colorizer)))
	      (dotimes (n (length faces))
		(let ((face (elt faces n)))
		  (colorize face (funcall colorizer)))))))
    faces))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun meep (n)
    (let ((position nil)
	  (val nil))
      (multiple-value-bind (a b) (floor n 2)
	(setf val (if (zerop b)
		      -1
		      1))
	(setf position (mod (- 1 a) 3)))
      (list position val)))

  (defun moop (n)
    (let* ((posses (list 0 0 0))
	   (vals (meep n))
	   (wowee (vector 0 1 2))
	   (pair (vector 2 3 0 1 4 5))
	   (position (first vals)))
      (setf (elt posses  position) (second vals))
      (setf wowee (concatenate 'list (remove position wowee) (vector position)))
      (list* n posses wowee (elt pair n) vals))))

(defmacro drawblockface (side)
  (let ((vals (moop side)))
    (let ((a (second vals))
	  (b (third vals)))
      `(let ((blockidnexttome (funcall getempty ,@a)))
	 (if
	  (or
	   (zerop blockidnexttome)
	   (and (not (aref mc-blocks::opaquecubelooukup blockidnexttome))
		(not (= blockid blockidnexttome))))
	  (let ((newvert (funcall (elt blockfaces ,(fourth vals)))))
	    (lightvert2 newvert betlight ,@b getskylightz)
	    (setf (aref faces ,side) newvert)))))))

(defmacro actuallywow ()
  (let ((tot (list 'progn)))
    (dotimes (n 6)
      (push (list 'drawblockface n) tot))
    (nreverse tot)))

(defun renderstandardblock (blockid getempty betlight getskylightz)
  (let* ((faces (make-array 6 :initial-element nil)))
    (actuallywow)
    faces))

(defparameter xfaces
  (vector
   (lambda ()
     (list
      (vertex
       (pos -0.5 -0.5 -0.5) (uv 0.0 0.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos  0.5 -0.5  0.5)  (uv 1.0 0.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos  0.5  0.5  0.5)  (uv 1.0 1.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos -0.5  0.5 -0.5) (uv 0.0 1.0) (opgray 1.0) (blocklight) (skylight))))
   (lambda () 
     (list
      (vertex
       (pos -0.5 -0.5 -0.5)  (uv 0.0 0.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos -0.5  0.5 -0.5)  (uv 0.0 1.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos 0.5  0.5  0.5)  (uv 1.0 1.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos 0.5 -0.5  0.5)  (uv 1.0 0.0) (opgray 1.0) (blocklight) (skylight))))
   (lambda ()
     (list
      (vertex
       (pos 0.5 -0.5 -0.5) (uv 0.0 0.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos 0.5 0.5 -0.5) (uv 0.0 1.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos -0.5 0.5 0.5) (uv 1.0 1.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos -0.5 -0.5 0.5) (uv 1.0 0.0) (opgray 1.0) (blocklight) (skylight))))
   (lambda ()
     (list
      (vertex
       (pos 0.5 -0.5  -0.5)  (uv 0.0 0.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos -0.5 -0.5  0.5)  (uv 1.0 0.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos -0.5  0.5  0.5)  (uv 1.0 1.0) (opgray 1.0) (blocklight) (skylight))
      (vertex
       (pos 0.5  0.5  -0.5)  (uv 0.0 1.0) (opgray 1.0) (blocklight) (skylight))))))

(defun renderBlockReed (blockid i j k betlight getskylightz)
  (let* ((faces (make-array 4 :initial-element nil)))
    (let ((lighthere (coerce (funcall betlight 0 0 0) 'float))
	  (skylighthere (coerce (funcall getskylightz 0 0 0) 'float)))
      (dotimes (n 4)
	(let ((newface (funcall (elt xfaces n))))
	  (setf (elt faces n) newface)
	  (dolist (v newface)
	    (setf (elt v 3) (vector (lightfunc lighthere)))
	    (setf (elt v 4) (vector (lightfunc skylighthere)))))))
    faces))

(defun allvec4 (num)
  (vector
   num
   num
   num
   num))

(defun vec4colorize (face colorizer)
  (cunt-verts
   (elt colorizer 0)
   (elt colorizer 1)
   (elt colorizer 2)
   (elt colorizer 3)
   face))

(defun colorize (face colorizer)
  (cunt-verts
   (/ (elt colorizer 0) 256)
   (/ (elt colorizer 1) 256)
   (/ (elt colorizer 2) 256)
   (/ (elt colorizer 3) 256)
   face))

(defun renderblockbyrendertype ())

(defun lightmultiplier (light)
  (let ((ans (lightfunc light)))
    (vector
     ans
     ans
     ans
     1.0)))

(defun lightfunc (light)
  (expt 0.8 (- 15 light)))

(defun lightvert (vert light)
  (let ((anum (lightfunc light)))
    (cunt-verts anum anum anum 1.0 vert)))

(defun vec3getlight (lelight vec3)
  (coerce
   (funcall lelight
	    (elt vec3 0)
	    (elt vec3 1)
	    (elt vec3 2))
   'float))

(defun avg (&rest args)
  (/ (apply (function +) args) (length args)))

;;buns r fun ahun
(defun dayify (num)
  (round (* daytime num)))

(defun lightvert2 (face getlight a b unchange skylit)
  (dolist (v face)
    (let ((vert (elt v 0)))    
      (let ((foo (round (* 2 (elt vert a))))
	    (bar (round (* 2 (elt vert b))))
	    (qux (round (* 2 (elt vert unchange)))))
	(let ((uno (vec3getlight getlight (insert-at  qux (vector foo bar) unchange )))
	      (dos (vec3getlight getlight (insert-at  qux (vector foo 0) unchange )))
	      (tres (vec3getlight getlight (insert-at qux (vector 0 bar) unchange )))
	      (quatro (vec3getlight getlight (insert-at qux (vector 0 0) unchange ))))
	  (let ((foo (round (* 2 (elt vert a))))
		(bar (round (* 2 (elt vert b))))
		(qux (round (* 2 (elt vert unchange)))))
	    (let* ((1dos (vec3getlight skylit (insert-at qux (vector foo 0) unchange )))
		   (1tres (vec3getlight skylit (insert-at qux (vector 0 bar) unchange )))
		   (1quatro (vec3getlight skylit (insert-at qux (vector 0 0) unchange )))
		   (1uno (vec3getlight skylit (insert-at  qux (vector foo bar) unchange ))))
	      (setf (elt v 3) (vector (lightfunc (avg uno dos tres quatro))))
	      (setf (elt v 4) (vector (lightfunc (avg 1uno 1dos 1tres 1quatro))))
	      (progno
	       (let ((anum (lightfunc (avg (max 1uno uno)
					   (max 1dos dos)
					   (max 1tres tres)
					   (max 1quatro quatro)))))
		 (cunt-vert anum anum anum 1.0 (elt v 2)))))))))))

;;0.9 for nether
;;0.8 for overworld