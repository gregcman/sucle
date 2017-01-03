(in-package :sandbox)

(defun isOpaque (id)
  (eq t (aref mc-blocks::opaquecubelooukup id)))

(defun light-node (x y z &optional (bfs (q::make-uniq-q)))
  (q::uniq-push (world:chunkhashfunc x y z) bfs)  
  (%light-node bfs))

(defmacro xyz (mx my mz &body body)
  `(let ((x (+ i ,mx))
	 (y (+ j ,my))
	 (z (+ k ,mz)))
     ,@body))

;;flood fill propagation
(defun %light-node (bfs)
  (tagbody
   rep
     (multiple-value-bind (place exists?) (q::uniq-pop bfs)
       (if exists?
	   (multiple-value-bind (i j k) (world:unhashfunc place)
	     (let* ((light-level (world:getlight i j k))
		    (lower-level (1- light-level)))
	       (macrolet ((%0check (mx my mz)
			    `(xyz ,mx ,my ,mz
			       (if (< (world:getlight x y z) lower-level)
				   (unless (isOpaque (world:getblock x y z))
				     (setf (world:getlight x y z) lower-level)
				     (q::uniq-push 
					(world:chunkhashfunc x y z) bfs))))))
		 (%0check -1 0 0)
		 (%0check 1 0 0)
		 (%0check 0 -1 0)
		 (%0check 0 1 0)
		 (%0check 0 0 -1)
		 (%0check 0 0 1)))
	     (go rep))))))

(defun de-light-node (x y z)
  (let ((bfs (q::make-uniq-q))
	(lighting-bfs (q::make-uniq-q)))
    (q::kv-uniq-push (world:chunkhashfunc x y z) (world:getlight x y z) bfs)
    (setf (world:getlight x y z) 0)
    (%de-light-node bfs lighting-bfs)
    (%light-node lighting-bfs)))

;;flood the dark, then fill it in again

(defun %de-light-node (bfs lighting-bfs)
  (tagbody
   rep
     (multiple-value-bind (place light-value exists?) (q::kv-uniq-pop bfs)
       (if exists?
	   (multiple-value-bind (i j k) (world:unhashfunc place)
	     (macrolet ((check (mx my mz)
			  `(xyz ,mx ,my ,mz
			     (let ((adj-light-level (world:getlight x y z)))
			       (unless (zerop adj-light-level)
				 (if (< adj-light-level light-value)
				      (progn
					(setf (world:getlight x y z) 0)
					(q::kv-uniq-push (world:chunkhashfunc x y z)
						      adj-light-level bfs))
				      (if (>= adj-light-level light-value)
					  (q::uniq-push (world:chunkhashfunc x y z)
						     lighting-bfs))))))))
	       
	       (check -1 0 0)
	       (check 1 0 0)
	       (check 0 -1 0)
	       (check 0 1 0)
	       (check 0 0 -1)
	       (check 0 0 1))
	     (go rep)))))
  lighting-bfs)

(defun sky-light-node (x y z &optional (bfs (q::make-uniq-q)))
  (q::uniq-push (world:chunkhashfunc x y z) bfs)  
  (%sky-light-node bfs))

;;flood fill propagation with moving downwards
(defun %sky-light-node (bfs)
  (tagbody
   rep
     (multiple-value-bind (place exists?) (q::uniq-pop bfs)
       (if exists?
	   (multiple-value-bind (i j k) (world:unhashfunc place)
	     (let* ((light-level (world:skygetlight i j k))
		    (lower-level (1- light-level)))
	       (macrolet ((%0check (mx my mz)
			    `(xyz ,mx ,my ,mz
			       (if (< (world:skygetlight x y z) lower-level)
				   (unless (isOpaque (world:getblock x y z))
				     (setf (world:skygetlight x y z) lower-level)
				     (q::uniq-push (world:chunkhashfunc x y z) bfs)))))
			  (downcheck (mx my mz)
			    `(xyz ,mx ,my ,mz
			       (if (< (world:skygetlight x y z) lower-level)
				   (unless (or (> 0 y)
					       (isOpaque (world:getblock x y z)))
				     (setf (world:skygetlight x y z) 15)
				     (q::uniq-push (world:chunkhashfunc x y z) bfs))))))
		 (%0check -1 0 0)
		 (%0check 1 0 0)
		 (if (= 15 light-level)
		     (downcheck 0 -1 0)
		     (%0check 0 -1 0))
		 (%0check 0 1 0)
		 (%0check 0 0 -1)
		 (%0check 0 0 1)))
	     (go rep))))))

(defun sky-de-light-node (x y z)
  (let ((bfs (q::make-uniq-q))
	(lighting-bfs (q::make-uniq-q)))
    (q::kv-uniq-push (world:chunkhashfunc x y z) (world:skygetlight x y z) bfs)
    (setf (world:skygetlight x y z) 0)
    (%sky-de-light-node bfs lighting-bfs)
    (%sky-light-node lighting-bfs)))

;;same with torches, but we always remove if the current node is at 15 [the maximum]

(defun %sky-de-light-node (bfs lighting-bfs)
  (tagbody
   rep
     (multiple-value-bind (place light-value exists?) (q::kv-uniq-pop bfs)
       (if exists?
	   (multiple-value-bind (i j k) (world:unhashfunc place)
	     (macrolet ((check (mx my mz)
			  `(xyz ,mx ,my ,mz
			     (let ((adj-light-level (world:skygetlight x y z)))
			       (unless (zerop adj-light-level)
				 (if (< adj-light-level light-value)
				      (progn
					(setf (world:skygetlight x y z) 0)
					(q::kv-uniq-push (world:chunkhashfunc x y z)
						      adj-light-level bfs))
				      (if (>= adj-light-level light-value)
					  (q::uniq-push (world:chunkhashfunc x y z)
						     lighting-bfs))))))))
	       
	       (check -1 0 0)
	       (check 1 0 0)
	       (if (= 15 light-value)
		   (xyz 0 -1 0
		     (unless (or (isopaque (world:getblock x y z))
				 (< y 0))
		       (let ((adj-light-level (world:skygetlight x y z)))
			 (setf (world:skygetlight x y z) 0)
			 (q::kv-uniq-push (world:chunkhashfunc x y z) adj-light-level bfs))))
		   (check 0 -1 0))
	       (check 0 1 0)
	       (check 0 0 -1)
	       (check 0 0 1))
	     (go rep)))))
  lighting-bfs)
