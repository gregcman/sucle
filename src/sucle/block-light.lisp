;;[FIXME]this file is disabled!!
(in-package :sandbox)

(defun isOpaque (id)
  (eq t (aref block-data:*opaquecubelooukup* id)))

(defparameter *scratch-bfs* (queue::make-uniq-q))
(defparameter *scratch-bfs2* (queue::make-uniq-q))

(defun light-node (x y z &optional (bfs *scratch-bfs*))
  (queue::clruniq bfs)
  (queue::uniq-push (world:chunkhashfunc x y z) bfs)  
  (%light-node bfs))

(defmacro xyz (mx my mz &body body)
  `(let ((x (+ i ,mx))
	 (y (+ j ,my))
	 (z (+ k ,mz)))
     ,@body))

(defconstant i+1 (world:chunkhashfunc  1  0  0))
(defconstant i-1 (world:chunkhashfunc -1  0  0))
(defconstant j+1 (world:chunkhashfunc  0  1  0))
(defconstant j-1 (world:chunkhashfunc  0 -1  0))
(defconstant k+1 (world:chunkhashfunc  0  0  1))
(defconstant k-1 (world:chunkhashfunc  0  0 -1))

;;flood fill propagation
(defun %light-node (bfs)
  (declare (optimize (speed 3) (safety 0)))
  (tagbody
   rep
     (multiple-value-bind (place exists?) (queue::uniq-pop bfs)
       (when exists?
	 (let ((light-level (world:%getlight place)))
	   (declare (type (unsigned-byte 4) light-level))
	   (let ((lower-level (1- light-level)))
	     (declare (type (signed-byte 8) lower-level))
	     (macrolet ((%0check (d op)
			  `(let ((displacement (,op ,d place)))
			     (let ((val (world:%getlight displacement)))
			       (declare (type (unsigned-byte 4) val))
			       (when (< val lower-level)
				 (unless (isOpaque (world:%getblock displacement))
				   (progn
				     (setf (world:%getlight displacement) lower-level)
				     (block-dirtify-hashed displacement))
				   (queue::uniq-push displacement bfs)))))))
	       (%0check i-1 world:add)
	       (%0check i+1 world:add)
	       (%0check j-1 world:add)
	       (%0check j+1 world:add)
	       (%0check k-1 world:add)
	       (%0check k+1 world:add))))
	 (go rep)))))

(defun de-light-node (x y z)
  (let ((bfs *scratch-bfs*)
	(lighting-bfs *scratch-bfs2*))
    (queue:clruniq bfs)
    (queue:clruniq lighting-bfs)
    (queue::kv-uniq-push (world:chunkhashfunc x y z) (world:getlight x y z) bfs)
    (progn
      (setf (world:getlight x y z) 0)
      (block-dirtify x y z))
    (%de-light-node bfs lighting-bfs)
    (%light-node lighting-bfs)))

;;flood the dark, then fill it in again

(defun %de-light-node (bfs lighting-bfs)
  (declare (optimize (speed 3) (safety 0)))
  (tagbody
   rep
     (multiple-value-bind (place light-value exists?) (queue::kv-uniq-pop bfs)
       (declare (type fixnum light-value place))
       (when exists?
	 (macrolet ((check (disp)
		      `(let ((displacement (world:add ,disp place)))
			 (declare (type fixnum displacement))
			 (let ((adj-light-level (world:%getlight displacement)))
			   (declare (type fixnum adj-light-level))
			   (unless (zerop adj-light-level)
			     (if (< adj-light-level light-value)
				 (progn
				   (progn
				     (setf (world:%getlight displacement) 0)
				     (block-dirtify-hashed displacement))
				   (queue::kv-uniq-push displacement adj-light-level bfs))
				 (when (>= adj-light-level light-value)
				   (queue::uniq-push displacement lighting-bfs))))))))
	   
	   (check i-1)
	   (check i+1)
	   (check j-1)
	   (check j+1)
	   (check k-1)
	   (check k+1))
	 (go rep))))
  lighting-bfs)

(defun sky-light-node (x y z &optional (bfs *scratch-bfs*))
  (queue::clruniq bfs)
  (queue::uniq-push (world:chunkhashfunc x y z) bfs)  
  (%sky-light-node bfs))

;;flood fill propagation with moving downwards
(defun %sky-light-node (bfs)
  (declare (optimize (speed 3) (safety 0)))
  (tagbody
   rep
     (multiple-value-bind (place exists?) (queue::uniq-pop bfs)
       (when exists?
	 (let ((light-level (world:%skygetlight place)))
	   (declare (type (unsigned-byte 4) light-level))
	   (let ((lower-level (1- light-level)))
	     (declare (type (signed-byte 4) lower-level))
	     (macrolet ((%0check (disp)
			  `(let ((displacement (world:add place ,disp)))
			     (declare (type fixnum displacement))
			     (let ((val (world:%skygetlight displacement)))
			       (declare (type (unsigned-byte 4) val))
			       (when (< val lower-level)
				 (unless (isOpaque (world:%getblock displacement))
				   (progn
				     (setf (world:%skygetlight displacement) lower-level)
				     (block-dirtify-hashed displacement))
				   (queue::uniq-push displacement bfs))))))
			(downcheck (disp)
			  `(let ((displacement (world:add place ,disp)))
			     (declare (type fixnum displacement))
			     (let ((val (world:%skygetlight displacement)))
			       (declare (type (unsigned-byte 4) val))
			       (when (< val 15)
				 (unless (or (isOpaque (world:%getblock displacement))
					     ;;this depends on the fact that
					     ;;the height coordinate is the highest bits
					     (> j+1 displacement))
				   (progn
				     (setf (world:%skygetlight displacement) 15)
				     (block-dirtify-hashed displacement))
				   (queue::uniq-push displacement bfs)))))))
	       (%0check i-1)
	       (%0check i+1)
	       (if (= 15 light-level)
		   (downcheck j-1)
		   (%0check j-1))
	       (%0check j+1)
	       (%0check k-1)
	       (%0check k+1))))
	 (go rep)))))

(defun sky-de-light-node (x y z)
  (let ((bfs *scratch-bfs*)
	(lighting-bfs *scratch-bfs2*))
    (queue:clruniq bfs)
    (queue:clruniq lighting-bfs)
    (queue::kv-uniq-push (world:chunkhashfunc x y z) (world:skygetlight x y z) bfs)
    (progn
      (setf (world:skygetlight x y z) 0)
      (block-dirtify x y z))
    (%sky-de-light-node bfs lighting-bfs)
    (%sky-light-node lighting-bfs)))

;;same with torches, but we always remove if the current node is at 15 [the maximum]

(defun %sky-de-light-node (bfs lighting-bfs)
  (declare (optimize (speed 3) (safety 0)))
  (tagbody
   rep
     (multiple-value-bind (place light-value exists?) (queue::kv-uniq-pop bfs)
       (declare (type (unsigned-byte 4) light-value))
       (when exists?	 
	 (macrolet ((check (disp)
		      `(let ((displacement (world:add ,disp place)))
			 (declare (type fixnum displacement))
			 (let ((adj-light-level (world:%skygetlight displacement)))
			   (declare (type (unsigned-byte 4) adj-light-level))
			   (unless (zerop adj-light-level)
			     (if (< adj-light-level light-value)
				 (progn
				   (progn
				     (setf (world:%skygetlight displacement) 0)
				     (block-dirtify-hashed displacement))
				   (queue::kv-uniq-push displacement adj-light-level bfs))
				 (queue::uniq-push displacement lighting-bfs)))))))
	   
	   (check i-1)
	   (check i+1)
	   (if (= 15 light-value)
	       (let ((displacement (world:add j-1 place)))
		 (unless (or (isopaque (world:%getblock displacement))
			     ;;this depends on the fact that
			     ;;the height coordinate is the highest bits
			     (> j+1 displacement))
		   (let ((adj-light-level (world:%skygetlight displacement)))
		     (progn
		       (setf (world:%skygetlight displacement) 0)
		       (block-dirtify-hashed displacement))
		     (queue::kv-uniq-push displacement adj-light-level bfs))))
	       (check j-1))
	   (check j+1)
	   (check k-1)
	   (check k+1))
	 (go rep))))
  lighting-bfs)
