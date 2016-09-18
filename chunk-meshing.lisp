(in-package :sandbox)

;;this file takes chunk data and turns
;; it into a mesh. The mesh is passed
;; to the renderer for rendering

;;a list of faces and colors which are
;;not abstracted away
(defparameter i-face
  '((-0.5  0.5  0.5  1.0 1.0  0.6 0.6 0.6 0.6)
    (-0.5  0.5 -0.5  0.0 1.0  0.6 0.6 0.6 0.6 )
    (-0.5 -0.5 -0.5  0.0 0.0  0.6 0.6 0.6 0.6 )
    (-0.5 -0.5  0.5  1.0 0.0  0.6 0.6 0.6 0.6 )))
(defparameter i+face
  '((0.5 -0.5  0.5  1.0 0.0  0.6 0.6 0.6 0.6)
    (0.5 -0.5 -0.5  0.0 0.0  0.6 0.6 0.6 0.6)
    (0.5  0.5 -0.5  0.0 1.0  0.6 0.6 0.6 0.6)
    (0.5  0.5  0.5  1.0 1.0  0.6 0.6 0.6 0.6 )))
(defparameter j-face
  '((-0.5 -0.5 -0.5  0.0 0.0  0.3 0.3 0.3 0.3)
    (0.5 -0.5 -0.5  1.0 0.0  0.3 0.3 0.3 0.3)
    (0.5 -0.5  0.5  1.0 1.0  0.3 0.3 0.3 0.3)
    (-0.5 -0.5  0.5  0.0 1.0  0.3 0.3 0.3 0.3)))
(defparameter j+face
  '((-0.5 0.5 0.5 0.0 1.0  1.0 1.0 1.0 1.0)
    (0.5 0.5 0.5 1.0 1.0  1.0 1.0 1.0 1.0)
    (0.5 0.5 -0.5 1.0 0.0  1.0 1.0 1.0 1.0)
    (-0.5 0.5 -0.5 0.0 0.0  1.0 1.0 1.0 1.0)))
(defparameter k-face
  '((-0.5 0.5 -0.5 0.0 1.0  0.8 0.8 0.8 0.8)
    (0.5 0.5 -0.5 1.0 1.0  0.8 0.8 0.8 0.8)
    (0.5 -0.5 -0.5 1.0 0.0  0.8 0.8 0.8 0.8)
    (-0.5 -0.5 -0.5 0.0 0.0  0.8 0.8 0.8 0.8)))
(defparameter k+face
  '((-0.5 -0.5  0.5  0.0 0.0 0.8 0.8 0.8 0.8 )
    (0.5 -0.5  0.5  1.0 0.0  0.8 0.8 0.8 0.8 )
    (0.5  0.5  0.5  1.0 1.0  0.8 0.8 0.8 0.8)
    (-0.5  0.5  0.5  0.0 1.0 0.8 0.8 0.8 0.8)))

;;current layout: 3 position floats, 2 texcoord floats, 4 color floats

(defparameter cube-shape
  (reduce #'add-verts
	  (list i-face i+face j-face j+face k-face k+face)
	  :initial-value (make-shape)))

(defun translate-verts (translation-matrix verts)
  "takes a list of vertices and translates them"
  (mapcar
   (lambda (v)
     (let ((x (first v))
	   (y (second v))
	   (z (third v)))
       (let ((new-vert
	      (mat:fourbyone
	       (list x y z 1))))
	 (let ((trans (mat:mmul translation-matrix new-vert)))
	   (cons
	    (row-major-aref trans 0)
	    (cons
	     (row-major-aref trans 1)
	     (cons
	      (row-major-aref trans 2)
	      (nthcdr 3 v))))))))
   verts))

(defun translate-shape (s translation-matrix)
  "takes a shape and translates it"
  (let ((verts (shape-vs s)))  
    (setf (shape-vs s)
	  (translate-verts verts translation-matrix))
    s))

(defun increment-verts (x y z verts)
  "linear translation of vertices"
  (dolist (n verts)
    (incf (first n) x)
    (incf (second n) y)
    (incf (third n) z))
  verts)

(defun fuck-verts (r g b a verts)
  "linear translation of vertices"
  (dolist (n verts)
    (incf (sixth n) r)
    (incf (seventh n) g)
    (incf (eighth n) b)
    (incf (ninth n) a))
  verts)

(defun %damn-fuck (verts num)
  "converts 0-1 texcoords to terrain.png coords"
  (let* ((xtrans (mod num 16))
	 (ytrans (- 15 (/ (- num xtrans) 16))))
    (dolist (v verts)
      (setf (fourth v) (+ (/ (fourth v) 16) (/ xtrans 16)))
      (setf (fifth v) (+ (/ (fifth v) 16) (/  ytrans 16))))
    verts))

(defun something... (a b c d e f)
  "get a block"
  (getblock (+ a (* 16 d)) (+ b (* 16 e))(+ c (* 16 f))))

(defun chunk-shape (chunk coords)
  "turn a chunk into a shape, complete
with positions, textures, and colors. no normals tho or smooth lighting"
  (let* ((new-shape (make-shape))
	 (dims (array-dimensions chunk))
	 (il (first dims))
	 (jl (second dims))
	 (kl (third dims))
	 (io (first coords))
	 (jo (second coords))
	 (ko (third coords)))
    (flet ((getblock (x y z)
	     (chunk-block chunk x y z))
	   (getempty (x y z)
	     (if (and
		  (< -1 x il)
		  (< -1 y jl)
		  (< -1 z kl))
		 (if
		  (/= 0 (chunk-block chunk x y z))
		  nil
		  (= 0 (something... x y z io jo ko)))
		 (= 0 (something... x y z io jo ko)))))
      (dotimes (i il)
	(dotimes (j jl)
	  (dotimes (k kl)
	    (let ((blockid (getblock i j k)))
	      (if (not (zerop blockid))
		  (let ((the-skin (aref blockIndexInTexture blockid)))
		    (if (numberp the-skin)
			(reduce
			 #'add-verts
			 (get-a-default-block-shape #'getempty the-skin i j k)
			 :initial-value new-shape)
			(reduce
			 #'add-verts
			 (get-a-variable-block-shape #'getempty i j k the-skin)
			 :initial-value new-shape)))))))))
    new-shape))

(defun get-a-variable-block-shape (getempty i j k func)
  "looks up how to mesh a given block in the block.lisp file with 
a table of numbers and closures"
  (let* ((lefaces nil))
    (if
     (funcall getempty (1+ i) j k)
     (push 
      (%damn-fuck
       (copy-tree
	i+face)
       (funcall func 3))
      lefaces))
    (if
     (funcall getempty (1- i) j k)
     (push
      (%damn-fuck
       (copy-tree i-face)
       (funcall func 2))
      lefaces))
    (if
     (funcall getempty i (1+ j) k)
     (push
      (%damn-fuck
       (copy-tree j+face)
       (funcall func 1))
      lefaces))
    (if
     (funcall getempty i (1- j) k)
     (push
      (%damn-fuck
       (copy-tree j-face)
       (funcall func 0))
      lefaces))
    (if
     (funcall getempty i j (1+ k))
     (push
      (%damn-fuck
       (copy-tree k+face)
       (funcall func 5))
      lefaces))
    (if
     (funcall getempty i j (1- k))
     (push
      (%damn-fuck
       (copy-tree k-face)
       (funcall func 4))
      lefaces))
    (dolist (f lefaces)
      (increment-verts i j k f)
      (fuck-verts -0.0 -0.0 -0.0 1.0 f))
    lefaces))

(defun get-a-default-block-shape (getempty skin i j k)
  "the default block shape. same texture every side."
  (let* ((faces nil))
    (if
     (funcall getempty (1+ i) j k)
     (push (copy-tree i+face) faces))
    (if
     (funcall getempty (1- i) j k)
     (push (copy-tree i-face) faces))
    (if
     (funcall getempty i (1+ j) k)
     (push (copy-tree j+face) faces))
    (if
     (funcall getempty i (1- j) k)
     (push (copy-tree j-face) faces))
    (if
     (funcall getempty i j (1+ k))
     (push (copy-tree k+face) faces))
    (if
     (funcall getempty i j (1- k))
     (push (copy-tree k-face) faces))
    (dolist (face faces)
      (%damn-fuck face skin)
      (increment-verts i j k face)
      (fuck-verts -0.0 -0.0 -0.0 0.0 face))
    faces))
