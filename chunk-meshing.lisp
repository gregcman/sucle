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

(defparameter blockfaces
  (vector
   (lambda ()
     (list (vector -0.5  0.5  0.5  1.0 1.0  0.6 0.6 0.6 0.6)
	   (vector -0.5  0.5 -0.5  0.0 1.0  0.6 0.6 0.6 0.6)
	   (vector -0.5 -0.5 -0.5  0.0 0.0  0.6 0.6 0.6 0.6)
	   (vector -0.5 -0.5  0.5  1.0 0.0  0.6 0.6 0.6 0.6)))
   (lambda ()
     (list (vector 0.5 -0.5  0.5  1.0 0.0  0.6 0.6 0.6 0.6)
	   (vector 0.5 -0.5 -0.5  0.0 0.0  0.6 0.6 0.6 0.6)
	   (vector 0.5  0.5 -0.5  0.0 1.0  0.6 0.6 0.6 0.6)
	   (vector 0.5  0.5  0.5  1.0 1.0  0.6 0.6 0.6 0.6)))
   (lambda ()
     (list (vector -0.5 -0.5 -0.5  0.0 0.0  0.5 0.5 0.5 0.3)
	   (vector 0.5 -0.5 -0.5  1.0 0.0 0.5 0.5 0.5 0.3)
	   (vector 0.5 -0.5  0.5  1.0 1.0  0.5 0.5 0.5 0.3)
	   (vector -0.5 -0.5  0.5  0.0 1.0  0.5 0.5 0.5 0.3)))
   (lambda ()
     (list (vector -0.5 0.5 0.5 0.0 1.0  1.0 1.0 1.0 1.0)
	   (vector 0.5 0.5 0.5 1.0 1.0  1.0 1.0 1.0 1.0)
	   (vector 0.5 0.5 -0.5 1.0 0.0  1.0 1.0 1.0 1.0)
	   (vector -0.5 0.5 -0.5 0.0 0.0  1.0 1.0 1.0 1.0)))
   (lambda ()
     (list (vector -0.5 0.5 -0.5 0.0 1.0  0.8 0.8 0.8 0.8)
	   (vector 0.5 0.5 -0.5 1.0 1.0  0.8 0.8 0.8 0.8)
	   (vector 0.5 -0.5 -0.5 1.0 0.0  0.8 0.8 0.8 0.8)
	   (vector -0.5 -0.5 -0.5 0.0 0.0  0.8 0.8 0.8 0.8)))
   (lambda ()
     (list (vector -0.5 -0.5  0.5  0.0 0.0 0.8 0.8 0.8 0.8)
	   (vector 0.5 -0.5  0.5  1.0 0.0  0.8 0.8 0.8 0.8)
	   (vector 0.5  0.5  0.5  1.0 1.0  0.8 0.8 0.8 0.8)
	   (vector -0.5  0.5  0.5  0.0 1.0 0.8 0.8 0.8 0.8)))))

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
  "colorize a vertex"
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
		    (get-a-variable-block2-shape
		     blockid
		     (aref mc-blocks::blockIndexInTexture blockid)   
		     (lambda (a b c)
		       (getblock (+ a i) (+ b j) (+ c k)))
		     (lambda (a b c)
		       (getlight (+ a i) (+ b j) (+ c k)))
		     (lambda (a b c)
		       (skygetlight (+ a i) (+ b j) (+ c k))))))
	       (dolist (face fineshape)
		 (increment-verts i j k face))
	       (reduce
		#'add-verts
		fineshape
		:initial-value new-shape)))))))
    new-shape))

(if (= 0 thetex)
    (let ((colorizer (getapixel 0 255 (gethash "grasscolor.png" picture-library))))
      (cunt-verts
       (/ (elt colorizer 0) 256)
       (/ (elt colorizer 1) 256)
       (/ (elt colorizer 2) 256)
       (/ (elt colorizer 3) 256)
       newvert)))

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
    (list* n posses wowee (elt pair n) vals)))

(defmacro drawblockface (side)
  (let ((vals (moop side)))
    (let ((a (second vals))
	  (b (third vals)))
      `(let ((blockidnexttome (funcall getempty ,@a)))
	 (if
	  (zerop blockidnexttome)
	  (let ((newvert (funcall (elt blockfaces ,(fourth vals)))))
	    (lightvert2 newvert betlight ,@b getskylightz)
	    (setf (aref faces ,side) newvert)))))))
(defmacro actuallywow ()
  (let ((tot (list 'progn)))
    (dotimes (n 6)
      (push (list 'drawblockface n) tot))
    (nreverse tot)))

(defun get-a-variable-block2-shape (blockid the-skin getempty betlight getskylightz)
  "the default block shape. same texture every side." 
  (let* ((faces (make-array 6 :initial-element nil)))
    (actuallywow)

    (dotimes (n 6)
      (let ((newvert (aref faces n)))
	(%damn-fuck newvert (funcall the-skin n))))
    (progn
     (if (= 18 blockid)
	 (let ((colorizer (getapixel 0 255 (gethash "foliagecolor.png" picture-library))))
	   (dotimes (vert 6)
	     (let ((face (elt faces vert)))
	       (cunt-verts
		(/ (elt colorizer 0) 256)
		(/ (elt colorizer 1) 256)
		(/ (elt colorizer 2) 256)
		(/ (elt colorizer 3) 256)
		face)))))
     (if (= 2 blockid)
	 (let ((colorizer (getapixel 0 255 (gethash "grasscolor.png" picture-library))))
	   (let ((face (elt faces 1)))
	     (cunt-verts
	      (/ (elt colorizer 0) 256)
	      (/ (elt colorizer 1) 256)
	      (/ (elt colorizer 2) 256)
	      (/ (elt colorizer 3) 256)
	      face)))))
    (coerce (delete nil faces) 'list)))

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
      (let ((uno (vec3getlight getlight (insert-at  qux (vector foo bar) unchange )))
	    (dos (vec3getlight getlight (insert-at  qux (vector foo 0) unchange )))
	    (tres (vec3getlight getlight (insert-at qux (vector 0 bar) unchange )))
	    (quatro (vec3getlight getlight (insert-at qux (vector 0 0) unchange ))))
	(let ((foo (round (* 2 (elt vert a))))
	      (bar (round (* 2 (elt vert b))))
	      (qux (round (* 2 (elt vert unchange)))))
	  (let* ((1dos (vec3getlight skylit (insert-at  qux (vector foo 0) unchange )))
		 (1tres (vec3getlight skylit (insert-at qux (vector 0 bar) unchange )))
		 (1quatro (vec3getlight skylit (insert-at qux (vector 0 0) unchange )))
		 (1uno  (vec3getlight skylit (insert-at  qux (vector foo bar) unchange ))))
	    (let ((anum (lightfunc (avg (max 1uno uno)
					(max 1dos dos)
					(max 1tres tres)
					(max 1quatro quatro)))))
	      (cunt-vert anum anum anum 1.0 vert))))))))

;;0.9 for nether
;;0.8 for overworld
