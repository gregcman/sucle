(in-package :sandbox)

;;;;keeping track of the changes to the world
(defparameter dirtychunks nil)

(defun clean-dirty ()
  (setf dirtychunks (q:make-uniq-q)))
(defun dirty-pop ()
  (q:uniq-pop dirtychunks))
(defun dirty-push (item)
  (q:uniq-push item dirtychunks))
(defun block-dirtify (i j k)
  (dirty-push (world:chop (world:chunkhashfunc i k j))))

(defun setblock-with-update (i j k blockid new-light-value)
  (let ((old-blockid (world:getblock i j k)))
    (when (/= blockid old-blockid)
      (let ((old-light-value (world:getlight i j k)))
	(when (setf (world:getblock i j k) blockid)
	  (when (< new-light-value old-light-value)
	    (de-light-node i j k))
	  (unless (= old-light-value new-light-value)
	    (setf (world:getlight i j k) new-light-value))
	  (sky-de-light-node i j k)
	  (unless (zerop new-light-value)
	    (light-node i j k))
	  (flet ((check (a b c)
		   (light-node (+ i a) (+ j b) (+ k c))
		   (sky-light-node (+ i a) (+ j b) (+ k c))))
	    (check -1 0 0)
	    (check 1 0 0)
	    (check 0 -1 0)
	    (check 0 1 0)
	    (check 0 0 -1)
	    (check 0 0 1))
	  (block-dirtify i j k))))))

(defun plain-setblock (i j k blockid new-light-value &optional (new-sky-light-value 0))
  (when (setf (world:getblock i j k) blockid)
    (setf (world:getlight i j k) new-light-value)
    (setf (world:skygetlight i j k) new-sky-light-value)
    (block-dirtify i j k)))

#+nil
(defparameter *dirty-chunks* nil)

#+nil
(defun time-func (distance)
  (let ((a *ticks*))
    (+ (ash a 10) distance)))

#+nil
(progno (unless (gethash item *dirty-chunks*)
	  (multiple-value-bind (x z y) (world:unhashfunc item)
	    (let ((value (conz (time-func (truncate (distance-to-player x y z))) item)))
	      (setf (gethash item *dirty-chunks*) item)
	      (pileup:heap-insert value dirtychunks)))))

#+nil
(progno
 (let ((value (pileup:heap-pop dirtychunks)))
   (let ((ans (cdr value)))
     (when ans
       (recycle-cons-cell value)
       (remhash ans *dirty-chunks*))
     ans)))

					;;;;(setf dirtychunks (pileup:make-heap #'< :name "dirty chunks" :key #'car))
					;;;;(setf *dirty-chunks* (make-hash-table :test *fixnum-compare*))
