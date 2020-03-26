;;;;Currently this file contains:
;;;;- block types data 
;;;;- world format
;;;;- keeping track of changes to the world
;;;;- accessors

(in-package #:world)

(defgeneric lispobj-dispatch (obj))

(defun value-dispatch (value)
  (typecase value
    (fixnum value)
    (otherwise (lispobj-dispatch value))))

(defun (setf num-getobj) (new i j k)
  (voxel-chunks:setobj i j k new))
(defun num-getobj (i j k)
  (let ((value (voxel-chunks:getobj i j k)))
    (value-dispatch value)))

(defmacro suite (bits position get get-ldb)
  (let* ((bytespec `(byte ,bits ,position))
	 (access `(ldb ,bytespec
		       (num-getobj i j k))))
    `(progn
       (defun ,get-ldb (value)
	 (ldb ,bytespec value))
       (defun (setf ,get) (new i j k)
	 (setf ,access new))
       (defun ,get (i j k)
	 ,access))))

(progn
  (suite 8 0 getblock getblock-extract)
  (suite 4 8 getlight getlight-extract)
  (suite 4 12 skygetlight skygetlight-extract))

;;[FIXME]move this to a better place?
(defun blockify (blockid light sky)
  ;;FIXME::this just redirects?
  (voxel-chunks::blockify blockid light sky))

(eval-when (:load-toplevel :execute)
  (voxel-chunks:reset-empty-chunk-value (blockify 0 0 15)))

(defmethod lispobj-dispatch ((obj character))
  (blockify (char-code obj) 0 0))

(defmethod lispobj-dispatch ((obj t))
  (blockify (logcount (sxhash obj)) 0 0))

(defmethod lispobj-dispatch ((obj symbol))
  *empty-space*)
 

#+nil
;;FIXME::have a benchmark system and test system?
(defun test ()
  (let ((times (expt 10 6)))
    (time (dotimes (x times) (setf (getobj 0 0 0) 0)))
    (time (dotimes (x times) (setf (getobj 0 0 0) 0))))
  (let ((times (expt 10 6)))
    (time (dotimes (x times) (getobj 0 0 0)))
    (time (dotimes (x times) (getobj 0 0 0)))))
;;;;************************************************************************;;;;
(in-package #:world)
;;;;************************************************************************;;;;
;;;;<CHANGE-WORLD?>
;;;;keeping track of the changes to the world
;;;; *DIRTY-CHUNKS* contains chunks that need to be remeshed or have their
;;;; meshes removed.
(progn
  (defparameter *dirty-chunks* (queue:make-uniq-q))
  (defun clean-dirty ()
    (setf *dirty-chunks* (queue:make-uniq-q)))
  (defun dirty-pop ()
    (queue:uniq-pop *dirty-chunks*))
  (defun dirty-push (item)
    (queue:uniq-push item *dirty-chunks*))
  (defun block-dirtify (i j k)
    (let ((imask (mod i 16))
	  (jmask (mod j 16))
	  (kmask (mod k 16)))
      (labels ((add (x y z)
		 (let ((chunk (voxel-chunks:obtain-chunk-from-block-coordinates x y z)))
		   (unless (voxel-chunks:empty-chunk-p chunk)
		     (dirty-push (voxel-chunks:chunk-key chunk)))))
	       (i-permute ()
		 (case imask
		   (0 (j-permute (1- i)))
		   (15 (j-permute (1+ i))))
		 (j-permute i))
	       (j-permute (ix)
		 (case jmask
		   (0 (k-permute ix (1- j)))
		   (15 (k-permute ix (1+ j))))
		 (k-permute ix j))
	       (k-permute (ix jx)
		 (case kmask
		   (0 (add ix jx (1- k)))
		   (15 (add ix jx (1+ k))))
		 (add ix jx k)))
	(i-permute)))))

(defun dirty-push-around (key)
  ;;[FIXME]although this is correct, it
  ;;lags behind player movement?
  (voxel-chunks:with-chunk-key-coordinates
   (x y z) key
   (dobox ((x0 (1- x) (+ x 2))
	   (y0 (1- y) (+ y 2))
	   (z0 (1- z) (+ z 2)))
	  (dirty-push (voxel-chunks:create-chunk-key x0 y0 z0)))))

#+nil
(defun block-dirtify-hashed (xyz)
  (multiple-value-bind (x y z) (unhashfunc xyz)
    (block-dirtify x y z)))


#+nil
(defun setblock-with-update (i j k blockid &optional
					     (new-light-value (block-data:data blockid :light)))
 (let ((old-light-value (getlight i j k)))
   (when (setf (getblock i j k) blockid)
     #+nil
      (when (< new-light-value old-light-value)
	(de-light-node i j k))
      (unless (= old-light-value new-light-value)
	(setf (getlight i j k) new-light-value))
      #+nil
      (sky-de-light-node i j k)
      #+nil
      (unless (zerop new-light-value)
	(light-node i j k))
      #+nil
      (flet ((check (a b c)
	       (light-node (+ i a) (+ j b) (+ k c))
	       (sky-light-node (+ i a) (+ j b) (+ k c))))
	(check -1 0 0)
	(check 1 0 0)
	(check 0 -1 0)
	(check 0 1 0)
	(check 0 0 -1)
	(check 0 0 1))
      (block-dirtify i j k))))

(defun plain-setblock (i j k blockid &optional
				       (new-light-value (block-data:data blockid :light))
				       (new-sky-light-value
					(if (eq blockid (block-data:lookup :void))
					    15
					    0)))
  (when (setf (getblock i j k) blockid)
    (setf (getlight i j k) new-light-value)
    (setf (skygetlight i j k) new-sky-light-value)
    (block-dirtify i j k)))


(defun unsquared-chunk-distance (position-key cx cy cz)
  (voxel-chunks:with-chunk-key-coordinates
   (x1 y1 z1) position-key
   (let ((dx (- x1 cx))
	 (dy (- y1 cy))
	 (dz (- z1 cz)))
     ;;[FIXME]we don't need the sqrt for sorting
     (+ (* dx dx) (* dy dy) (* dz dz)))))
(defun blocky-chunk-distance (position-key cx cy cz)
  (voxel-chunks:with-chunk-key-coordinates
   (x1 y1 z1) position-key
   (let ((dx (- x1 cx))
	 (dy (- y1 cy))
	 (dz (- z1 cz)))
     (max (abs dx)
	  (abs dy)
	  (abs dz)))))

;;[FIXME]thread-safety for:
;;voxel-chunks:*chunks*
;;voxel-chunks:*chunk-array*
;;*dirty-chunks*
;;*achannel*
