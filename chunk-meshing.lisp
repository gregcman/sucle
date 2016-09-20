(in-package :sandbox)

;;this file takes chunk data and turns
;; it into a mesh. The mesh is passed
;; to the renderer for rendering

;;a list of faces and colors which are
;;not abstracted away

(progn
 (defun i-face ()
   (list (vector -0.5  0.5  0.5  1.0 1.0  0.6 0.6 0.6 0.6)
	 (vector -0.5  0.5 -0.5  0.0 1.0  0.6 0.6 0.6 0.6)
	 (vector -0.5 -0.5 -0.5  0.0 0.0  0.6 0.6 0.6 0.6)
	 (vector -0.5 -0.5  0.5  1.0 0.0  0.6 0.6 0.6 0.6)))
 (defun i+face ()
   (list (vector 0.5 -0.5  0.5  1.0 0.0  0.6 0.6 0.6 0.6)
	 (vector 0.5 -0.5 -0.5  0.0 0.0  0.6 0.6 0.6 0.6)
	 (vector 0.5  0.5 -0.5  0.0 1.0  0.6 0.6 0.6 0.6)
	 (vector 0.5  0.5  0.5  1.0 1.0  0.6 0.6 0.6 0.6)))
 (defun j-face ()
   (list (vector -0.5 -0.5 -0.5  0.0 0.0  0.5 0.5 0.5 0.3)
	 (vector 0.5 -0.5 -0.5  1.0 0.0 0.5 0.5 0.5 0.3)
	 (vector 0.5 -0.5  0.5  1.0 1.0  0.5 0.5 0.5 0.3)
	 (vector -0.5 -0.5  0.5  0.0 1.0  0.5 0.5 0.5 0.3)))
 (defun j+face ()
   (list (vector -0.5 0.5 0.5 0.0 1.0  1.0 1.0 1.0 1.0)
	 (vector 0.5 0.5 0.5 1.0 1.0  1.0 1.0 1.0 1.0)
	 (vector 0.5 0.5 -0.5 1.0 0.0  1.0 1.0 1.0 1.0)
	 (vector -0.5 0.5 -0.5 0.0 0.0  1.0 1.0 1.0 1.0)))
 (defun k-face ()
   (list (vector -0.5 0.5 -0.5 0.0 1.0  0.8 0.8 0.8 0.8)
	 (vector 0.5 0.5 -0.5 1.0 1.0  0.8 0.8 0.8 0.8)
	 (vector 0.5 -0.5 -0.5 1.0 0.0  0.8 0.8 0.8 0.8)
	 (vector -0.5 -0.5 -0.5 0.0 0.0  0.8 0.8 0.8 0.8)))
 (defun k+face ()
   (list (vector -0.5 -0.5  0.5  0.0 0.0 0.8 0.8 0.8 0.8)
	 (vector 0.5 -0.5  0.5  1.0 0.0  0.8 0.8 0.8 0.8)
	 (vector 0.5  0.5  0.5  1.0 1.0  0.8 0.8 0.8 0.8)
	 (vector -0.5  0.5  0.5  0.0 1.0 0.8 0.8 0.8 0.8))))

;;current layout: 3 position floats, 2 texcoord floats, 4 color floats

(defun increment-verts (x y z verts)
  "linear translation of vertices"
  (dolist (n verts)
    (incf (aref n 0) x)
    (incf (aref n 1) y)
    (incf (aref n 2) z))
  verts)

(defun fuck-verts (r g b a verts)
  "linear translation of vertices"
  (dolist (n verts)
    (incf (aref n 5) r)
    (incf (aref n 6) g)
    (incf (aref n 7) b)
    (incf (aref n 8) a))
  verts)

(defun cunt-verts (r g b a verts)
  "linear translation of vertices"
  (dolist (n verts)
    (cunt-vert r g b a n))
  verts)

(defun cunt-vert (r g b a n)
  "linear translation of vertice, ns the vert dumbass"
  (setf (aref n 5) (* (aref n 5) r))
  (setf (aref n 6) (* (aref n 6) g))
  (setf (aref n 7) (* (aref n 7) b))
  (setf (aref n 8) (* (aref n 8) a))
  n)

(defun %damn-fuck (verts num)
  "converts 0-1 texcoords to terrain.png coords"
  (let* ((xtrans (mod num 16))
	 (ytrans (- 15 (/ (- num xtrans) 16))))
    (dolist (v verts)
      (setf (aref v 3) (+ (/ (aref v 3) 16) (/ xtrans 16)))
      (setf (aref v 4) (+ (/ (aref v 4) 16) (/  ytrans 16))))
    verts))

(defun something... (a b c d e f)
  "get a block"
  (getblock (+ a (* 16 d)) (+ b (* 16 e))(+ c (* 16 f))))

(defun something2... (a b c d e f)
  "get a block"
  (getlight (+ a (* 16 d)) (+ b (* 16 e))(+ c (* 16 f))))

(defun something3... (a b c d e f)
  "get a block"
  (skygetlight (+ a (* 16 d)) (+ b (* 16 e))(+ c (* 16 f))))

(defparameter shapebuffer (make-shape))

(defun chunk-shape (coords)
  "turn a chunk into a shape, complete
with positions, textures, and colors. no normals tho or smooth lighting"
  (if (gethash coords chunkhash)
      (let* ((chunk (gethash coords chunkhash))
	     (new-shape (destroy-shape shapebuffer))
	     (dims (array-dimensions chunk))
	     (il (first dims))
	     (jl (second dims))
	     (kl (third dims))
	     (io (first coords))
	     (jo (second coords))
	     (ko (third coords)))
	(flet ((getablock (x y z)
		 (something... x y z io jo ko))
	       (getlight (x y z)
		 (something2... x y z io jo ko))
	       (getskylight (x y z)
		 (something3... x y z io jo ko)))
	  (time
	   (dotimes (i il)
	     (dotimes (j jl)
	       (dotimes (k kl)
		 (let ((blockid (getablock i j k)))
		   (if (not (zerop blockid))
		       (let ((the-skin (aref blockIndexInTexture blockid)))
			 (if (numberp the-skin)
			     
			     (progn
			       (let ((fineshape
				      (get-a-default-block2-shape #'getablock #'getlight
								  #'getskylight
								  the-skin i j k)))
				 (reduce
				  #'add-verts
				  fineshape
				  :initial-value new-shape)))
			     (progn
			       (reduce
				#'add-verts
				(get-a-variable-block2-shape #'getablock #'getlight
							     #'getskylight
							     the-skin i j k)
				:initial-value new-shape)))))))))))
	new-shape)))

(defun get-a-variable-block2-shape (getempty getlight fuckme the-skin i j k)
  "the default block shape. same texture every side."
  (let ((betlight
	 (lambda (a b c)
	   (funcall getlight (+ a i) (+ b j) (+ c k))))
	(getskylightz
	 (lambda (a b c)
	   (funcall fuckme (+ a i) (+ b j) (+ c k)))))
    (let* ((faces nil))
      (if
       (zerop (funcall getempty (1+ i) j k))
       (let ((newvert (i+face)))
	 (lightvert2 newvert betlight 1 2 0 getskylightz)
	  (%damn-fuck newvert (funcall the-skin 3))
	 (push newvert faces)))
      (if
       (zerop (funcall getempty (1- i) j k))
       (let ((newvert (i-face)))
	 (lightvert2 newvert betlight 1 2 0 getskylightz)
	  (%damn-fuck newvert (funcall the-skin 2))
	 (push newvert faces)))
      (if
       (zerop (funcall getempty i (1+ j) k))
       (let ((newvert (j+face)))
	 (lightvert2 newvert betlight 0 2 1 getskylightz)
	  (%damn-fuck newvert (funcall the-skin 1))
	 (push newvert faces)) )
      (if
       (zerop  (funcall getempty i (1- j) k))
       (let ((newvert (j-face)))
	 (lightvert2 newvert betlight 0 2 1 getskylightz)
	  (%damn-fuck newvert (funcall the-skin 0))
	 (push newvert faces)) )
      (if
       (zerop  (funcall getempty i j (1+ k)))
       (let ((newvert (k+face)))
	 (lightvert2 newvert betlight 0 1 2 getskylightz)
	 (%damn-fuck newvert (funcall the-skin 5))
	 (push newvert faces)))
      (if
       (zerop  (funcall getempty i j (1- k)))
       (let ((newvert (k-face)))
	 (lightvert2 newvert betlight 0 1 2 getskylightz)
	 (%damn-fuck newvert (funcall the-skin 4))
	 (push newvert faces)))
      (dolist (face faces)
	(increment-verts i j k face))
      faces)))

(defun get-a-default-block2-shape (getempty getlight fuckme skin i j k)
  "the default block shape. same texture every side."
  (let ((betlight
	 (lambda (a b c)
	   (funcall getlight (+ a i) (+ b j) (+ c k))))
	(getskylightz
	 (lambda (a b c)
	   (funcall fuckme (+ a i) (+ b j) (+ c k)))))
    (let* ((faces nil))
      (if
       (zerop (funcall getempty (1+ i) j k))
       (let ((newvert (i+face)))
	 (lightvert2 newvert betlight 1 2 0 getskylightz)
	 (push newvert faces)))
      (if
       (zerop (funcall getempty (1- i) j k))
       (let ((newvert (i-face)))
	 (lightvert2 newvert betlight 1 2 0 getskylightz)
	 (push newvert faces)))
      (if
       (zerop (funcall getempty i (1+ j) k))
       (let ((newvert (j+face)))
	 (lightvert2 newvert betlight 0 2 1 getskylightz)
	 (push newvert faces)) )
      (if
       (zerop  (funcall getempty i (1- j) k))
       (let ((newvert (j-face)))
	 (lightvert2 newvert betlight 0 2 1 getskylightz)
	 (push newvert faces)) )
      (if
       (zerop  (funcall getempty i j (1+ k)))
       (let ((newvert (k+face)))
	 (lightvert2 newvert betlight 0 1 2 getskylightz)
	 (push newvert faces)))
      (if
       (zerop  (funcall getempty i j (1- k)))
        (let ((newvert (k-face)))
	 (lightvert2 newvert betlight 0 1 2 getskylightz)
	 (push newvert faces)))
      (dolist (face faces)
	(%damn-fuck face skin)
	(increment-verts i j k face))
      faces)))

(defun lightfunc (light)
  (expt 0.8 (- 15 light)))

(defun lightvert (vert light)
  (let ((anum (lightfunc light)))
    (cunt-verts anum anum anum 1.0 vert)))

(defun vec3getlight (lelight vec3)
  (funcall lelight
	   (elt vec3 0)
	   (elt vec3 1)
	   (elt vec3 2)))

(defun avg (&rest args)
  (/ (apply (function +) args) (length args)))

(defun lightvert2 (face getlight a b unchange skylit)
  (dolist (vert face)
    (let ((foo (round (* 2 (elt vert a))))
	  (bar (round (* 2 (elt vert b))))
	  (qux (round (* 2 (elt vert unchange)))))
    ;  (print 3)
      (let ((uno (vec3getlight getlight (insert-at  qux (vector foo bar) unchange )))
	    (dos (vec3getlight getlight (insert-at  qux (vector foo 0) unchange )))
	    (tres (vec3getlight getlight (insert-at qux (vector 0 bar) unchange )))
	    (quatro (vec3getlight getlight (insert-at qux (vector 0 0) unchange ))))
					;	(print 3434)
	(let ((foo (round (* 2 (elt vert a))))
	      (bar (round (* 2 (elt vert b))))
	      (qux (round (* 2 (elt vert unchange)))))
    ;  (print 3)
	  (let* ((1dos (vec3getlight skylit (insert-at  qux (vector foo 0) unchange )))
		 (1tres (vec3getlight skylit (insert-at qux (vector 0 bar) unchange )))
		 (1quatro (vec3getlight skylit (insert-at qux (vector 0 0) unchange )))
		 (1uno (if (not (and (zerop 1tres) (zerop 1quatro)))
			   (vec3getlight skylit (insert-at  qux (vector foo bar) unchange ))
			   0)))
					;	(print 3434)
	    (let ((anum (lightfunc (max (avg uno dos tres quatro) (avg 1uno 1dos 1tres 1quatro)))))
	      (cunt-vert anum anum anum 1.0 vert))))))))

;;0.9 for nether
;;0.8 for overworld
