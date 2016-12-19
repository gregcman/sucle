(in-package :sandbox)

(defun isOpaque (id)
  (eq t (aref mc-blocks::opaquecubelooukup id)))

(defun light-node (x y z &optional (bfs (make-uniq-q)))
  (uniq-push (vox::chunkhashfunc x y z) bfs)  
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
     (multiple-value-bind (place exists?) (uniq-pop bfs)
       (if exists?
	   (multiple-value-bind (i j k) (vox::unhashfunc place)
	     (let* ((light-level (getlight i j k))
		    (lower-level (1- light-level)))
	       (macrolet ((%0check (mx my mz)
			    `(xyz ,mx ,my ,mz
			       (if (< (getlight x y z) lower-level)
				   (unless (isOpaque (getblock x y z))
				     (setlight x y z lower-level)
				     (uniq-push (vox::chunkhashfunc x y z) bfs))))))
		 (%0check -1 0 0)
		 (%0check 1 0 0)
		 (%0check 0 -1 0)
		 (%0check 0 1 0)
		 (%0check 0 0 -1)
		 (%0check 0 0 1)))
	     (go rep))))))

(defun de-light-node (x y z)
  (let ((bfs (make-uniq-q))
	(lighting-bfs (make-uniq-q)))
    (kv-uniq-push (vox::chunkhashfunc x y z) (getlight x y z) bfs)
    (setf (getlight x y z) 0)
    (%de-light-node bfs lighting-bfs)
    (%light-node lighting-bfs)))

;;flood the dark, then fill it in again

(defun %de-light-node (bfs lighting-bfs)
  (tagbody
   rep
     (multiple-value-bind (place light-value exists?) (kv-uniq-pop bfs)
       (if exists?
	   (multiple-value-bind (i j k) (vox::unhashfunc place)
	     (macrolet ((check (mx my mz)
			  `(xyz ,mx ,my ,mz
			     (let ((adj-light-level (getlight x y z)))
			       (unless (zerop adj-light-level)
				 (if (< adj-light-level light-value)
				      (progn
					(setlight x y z 0)
					(kv-uniq-push (vox::chunkhashfunc x y z)
						      adj-light-level bfs))
				      (if (>= adj-light-level light-value)
					  (uniq-push (vox::chunkhashfunc x y z)
						     lighting-bfs))))))))
	       
	       (check -1 0 0)
	       (check 1 0 0)
	       (check 0 -1 0)
	       (check 0 1 0)
	       (check 0 0 -1)
	       (check 0 0 1))
	     (go rep)))))
  lighting-bfs)

(defun sky-light-node (x y z &optional (bfs (make-uniq-q)))
  (uniq-push (vox::chunkhashfunc x y z) bfs)  
  (%sky-light-node bfs))

;;flood fill propagation with moving downwards
(defun %sky-light-node (bfs)
  (tagbody
   rep
     (multiple-value-bind (place exists?) (uniq-pop bfs)
       (if exists?
	   (multiple-value-bind (i j k) (vox::unhashfunc place)
	     (let* ((light-level (skygetlight i j k))
		    (lower-level (1- light-level)))
	       (macrolet ((%0check (mx my mz)
			    `(xyz ,mx ,my ,mz
			       (if (< (skygetlight x y z) lower-level)
				   (unless (isOpaque (getblock x y z))
				     (skysetlight x y z lower-level)
				     (uniq-push (vox::chunkhashfunc x y z) bfs)))))
			  (downcheck (mx my mz)
			    `(xyz ,mx ,my ,mz
			       (if (< (skygetlight x y z) lower-level)
				   (unless (or (> 0 y)
					       (isOpaque (getblock x y z)))
				     (skysetlight x y z 15)
				     (uniq-push (vox::chunkhashfunc x y z) bfs))))))
		 (%0check -1 0 0)
		 (%0check 1 0 0)
		 (if (= 15 light-level)
		     (downcheck 0 -1 0)`
		     (%0check 0 -1 0))
		 (%0check 0 1 0)
		 (%0check 0 0 -1)
		 (%0check 0 0 1)))
	     (go rep))))))

(defun sky-de-light-node (x y z)
  (let ((bfs (make-uniq-q))
	(lighting-bfs (make-uniq-q)))
    (kv-uniq-push (vox::chunkhashfunc x y z) (skygetlight x y z) bfs)
    (setf (skygetlight x y z) 0)
    (%sky-de-light-node bfs lighting-bfs)
    (%sky-light-node lighting-bfs)))

;;same with torches, but we always remove if the current node is at 15 [the maximum]

(defun %sky-de-light-node (bfs lighting-bfs)
  (tagbody
   rep
     (multiple-value-bind (place light-value exists?) (kv-uniq-pop bfs)
       (if exists?
	   (multiple-value-bind (i j k) (vox::unhashfunc place)
	     (macrolet ((check (mx my mz)
			  `(xyz ,mx ,my ,mz
			     (let ((adj-light-level (skygetlight x y z)))
			       (unless (zerop adj-light-level)
				 (if (< adj-light-level light-value)
				      (progn
					(skysetlight x y z 0)
					(kv-uniq-push (vox::chunkhashfunc x y z)
						      adj-light-level bfs))
				      (if (>= adj-light-level light-value)
					  (uniq-push (vox::chunkhashfunc x y z)
						     lighting-bfs))))))))
	       
	       (check -1 0 0)
	       (check 1 0 0)
	       (if (= 15 light-value)
		   (xyz 0 -1 0
		     (unless (or (isopaque (getblock x y z))
				 (< y 0))
		       (let ((adj-light-level (skygetlight x y z)))
			 (skysetlight x y z 0)
			 (kv-uniq-push (vox::chunkhashfunc x y z) adj-light-level bfs))))
		   (check 0 -1 0))
	       (check 0 1 0)
	       (check 0 0 -1)
	       (check 0 0 1))
	     (go rep)))))
  lighting-bfs)

(defun test-light ()
  (clearworld)
  (dorange (x -32 32)
	   (dorange (y 0 24)
		    (dorange (z -32 32)
			     (setblock x y z 3))))
  (update-world-vao))
