(defpackage #:world
  (:use :cl #:utility)	
  (:export 

;;;block accessors
   #:getblock #:setblock
   #:getlight #:setlight
   #:skygetlight #:skysetlight
   
   #:unhashfunc
   #:chunkhashfunc

   #:clearworld))

(in-package #:world)

(utility:eval-always
  (defparameter *chunk-size-x* 16)
  (defparameter *chunk-size-y* 16)
  (defparameter *chunk-size-z* 16))

(struct-to-clos:struct->class
 (defstruct chunk
   x
   y
   z
   key
   data))

#+nil ;;test to see what happens if chunk is just an array, and nothing else
(progn
  (defun make-chunk (&key data &allow-other-keys)
    data)
  (declaim (inline chunk-data))
  (defun chunk-data (chunk)
    chunk))

;;FIXME::chunk-coord and block-coord being fixnums is not theoretically correct,
;;but its still a lot of space?
(deftype chunk-coord () 'fixnum)
(deftype chunk-data () `(simple-array t (,(* *chunk-size-x* *chunk-size-y* *chunk-size-z*))))
(deftype inner-chunk-major-coord () `(integer 0 ,(* *chunk-size-x* *chunk-size-y* *chunk-size-z*)))
(deftype inner-chunk-coord-x () `(integer 0 ,*chunk-size-x*))
(deftype inner-chunk-coord-y () `(integer 0 ,*chunk-size-y*))
(deftype inner-chunk-coord-z () `(integer 0 ,*chunk-size-z*))
(utility:with-unsafe-speed
  (declaim (inline chunk-ref)
	   (ftype (function (inner-chunk-coord-x inner-chunk-coord-y inner-chunk-coord-z)
			    inner-chunk-major-coord)
		  chunk-ref))
  (defun chunk-ref (inner-x inner-y inner-z)
    (declare 
     (type inner-chunk-coord-x inner-x)
     (type inner-chunk-coord-y inner-y)
     (type inner-chunk-coord-z inner-z))
    (+ (* (utility:etouq *chunk-size-z*)
	  (+ (* (utility:etouq *chunk-size-y*)		      
		inner-y)
	     inner-z))
       inner-x)))
(deftype block-coord () 'fixnum)
(defun create-chunk-key (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
  ;;in order to be correct, the key has to store each value unaltered
  ;;This is for creating a key for a hash table
  (list chunk-x chunk-y chunk-z))
(defmacro with-chunk-key-coordinates ((x y z) chunk-key &body body)
  `(destructuring-bind (,x ,y ,z) ,chunk-key
     ,@body))
(defun obtain-chunk-from-chunk-key (chunk-key)
  ;;FIXME::is this a good api?
  (with-chunk-key-coordinates (x y z) chunk-key 
    (obtain-chunk x y z)))
(defun create-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
  (declare (type chunk-coord chunk-x chunk-y chunk-z))
  (make-chunk :x chunk-x
	      :y chunk-y
	      :z chunk-z
	      :key (create-chunk-key chunk-x chunk-y chunk-z)
	      :data (make-array (* *chunk-size-x* *chunk-size-y* *chunk-size-z*)
				:initial-element nil)))

(defparameter *empty-chunk* (create-chunk 0 0 0))

(defun make-chunk-from-key-and-data (key data)
  (with-chunk-key-coordinates (x y z) key
    (make-chunk :x x
		:y y
		:z z
		:key key
		:data data)))
(defun make-chunk-from-key-and-data-and-keep (key data)
  (let ((new-chunk
	 (make-chunk-from-key-and-data key data)))
    (set-chunk-at key new-chunk)))
;;equal is used because the key is a list of the chunk coordinates
(defun make-chunk-table ()
  (make-hash-table :test 'equal))
(defparameter *chunks* (make-chunk-table))
(defun set-chunk-at (key chunk)
  (setf (gethash key *chunks*) chunk))
(defun get-chunk-at (key)
  (gethash key *chunks*))
(defun remove-chunk-at (key)
  (remhash key *chunks*))

(defun total-loaded-chunks ()
  (hash-table-count *chunks*))

(utility::eval-always
  (defparameter *chunk-array-default-size-x* 32)
  (defparameter *chunk-array-default-size-y* 32)
  (defparameter *chunk-array-default-size-z* 32)
  (defparameter *chunk-array-empty-value* nil))
;;(struct-to-clos:struct->class)
(defstruct chunk-array
  (array (make-array (list *chunk-array-default-size-x*
			   *chunk-array-default-size-y*
			   *chunk-array-default-size-z*)
		     :initial-element *chunk-array-empty-value*))
  (x-min 0)
  (y-min 0)
  (z-min 0)
  ;;(x-max *chunk-array-default-size-x*)
  ;;(y-max *chunk-array-default-size-y*)
  ;;(z-max *chunk-array-default-size-z*)
  )
(deftype chunk-array-data ()
  `(simple-array t (,*chunk-array-default-size-x*
		    ,*chunk-array-default-size-y*
		    ,*chunk-array-default-size-z*)))

(utility:with-unsafe-speed
  (defun fill-array (array value)
    (declare (type chunk-array-data array))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref array i) value))))

(utility:with-unsafe-speed
 ;;progn
  (
   utility::with-declaim-inline
   ;;progn
   (obtain-chunk reference-inside-chunk get-chunk
		 get-chunk-from-chunk-array
		 (setf reference-inside-chunk)
		 chunk-coordinates-match-p
		 obtain-chunk-from-block-coordinates)
   #+nil
   (defun chunk-coordinates-match-p (chunk &optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
     (declare (type chunk-coord chunk-x chunk-y chunk-z))
      (and (= chunk-x (the chunk-coord (chunk-x chunk)))
	   (= chunk-y (the chunk-coord (chunk-y chunk)))
	   (= chunk-z (the chunk-coord (chunk-z chunk)))))

    (defun reference-inside-chunk (chunk inner-x inner-y inner-z)
      (declare (type inner-chunk-coord-x inner-x)
	       (type inner-chunk-coord-y inner-y)
	       (type inner-chunk-coord-z inner-z))
      (row-major-aref (the chunk-data (chunk-data chunk)) (chunk-ref inner-x inner-y inner-z)))
    (defun (setf reference-inside-chunk) (value chunk inner-x inner-y inner-z)
      (declare (type inner-chunk-coord-x inner-x)
	       (type inner-chunk-coord-y inner-y)
	       (type inner-chunk-coord-z inner-z))
      (setf (row-major-aref (the chunk-data (chunk-data chunk))
			    (chunk-ref inner-x inner-y inner-z))
	    value))

    (defun get-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0) (force-load nil))
      (declare (type chunk-coord chunk-x chunk-y chunk-z))
      (let ((key (create-chunk-key chunk-x chunk-y chunk-z)))
	(multiple-value-bind (value existsp) (get-chunk-at key)
	  (if existsp
	      (values value t)
	      (progn
		;;FIXME::load chunks here, unload chunks here?
		(if force-load
		    (let ((new-chunk (load-chunk chunk-x chunk-y chunk-z)))
		      (set-chunk-at key new-chunk)
		      (values new-chunk t))
		    (values
		     *empty-chunk*
		     nil)))))))

    (defun load-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
      ;;FIXME::actually load chunks
      (create-chunk chunk-x chunk-y chunk-z))
    
    (defun create-chunk-array ()
      (make-chunk-array))
    (defparameter *chunk-array* (create-chunk-array))
    (defun reposition-chunk-array (&optional 
				     (chunk-x 0) (chunk-y 0) (chunk-z 0)
				     (chunk-array *chunk-array*))
      (declare (type chunk-coord chunk-x chunk-y chunk-z))
      (setf (chunk-array-x-min chunk-array) chunk-x
	    (chunk-array-y-min chunk-array) chunk-y
	    (chunk-array-z-min chunk-array) chunk-z)
      #+nil
      (setf (chunk-array-x-max chunk-array) (+ (utility:etouq *chunk-array-default-size-x*) chunk-x)
	    (chunk-array-y-max chunk-array) (+ (utility:etouq *chunk-array-default-size-y*) chunk-y)
	    (chunk-array-z-max chunk-array) (+ (utility:etouq *chunk-array-default-size-z*) chunk-z))
      (fill-array (chunk-array-array chunk-array) *chunk-array-empty-value*)
      (values))

    (defun get-chunk-from-chunk-array (&optional 
					 (chunk-x 0) (chunk-y 0) (chunk-z 0)
					 (force-load nil)
					 (chunk-array *chunk-array*))
      (declare (type chunk-coord chunk-x chunk-y chunk-z))
      ;;if the coordinates are correct, return a chunk, otherwise return nil
      (let ((data-x (- chunk-x (the chunk-coord (chunk-array-x-min chunk-array)))))
	(declare (type chunk-coord data-x))
	(when (< -1 data-x ;;(the chunk-coord (chunk-array-size-x chunk-array))
		 (utility:etouq *chunk-array-default-size-x*))
	  (let ((data-y (- chunk-y (the chunk-coord (chunk-array-y-min chunk-array)))))
	    (declare (type chunk-coord data-y))
	    (when (< -1 data-y ;;(the chunk-coord (chunk-array-size-y chunk-array))
		     (utility:etouq *chunk-array-default-size-y*))
	      (let ((data-z (- chunk-z (the chunk-coord (chunk-array-z-min chunk-array)))))
		(declare (type chunk-coord data-z))
		(when (< -1 data-z ;;(the chunk-coord (chunk-array-size-z chunk-array))
			 (utility:etouq *chunk-array-default-size-z*))
		  (let ((data (chunk-array-array chunk-array)))
		    (declare (type chunk-array-data data))
		    ;;the chunk is in the chunk-array's bounds
		    (let ((possible-chunk
			   (aref data data-x data-y data-z)))
		      (if (and possible-chunk
			       ;;This check is unnecessary if we clear the chunk array every time
			       ;;the position updates. combined with hysteresis, the relatively
			       ;;slow filling should not happen often
			       #+nil
			       (chunk-coordinates-match-p possible-chunk chunk-x chunk-y chunk-z))
			  ;;The chunk is not nil, and the coordinates line up
			  possible-chunk
			  (when force-load
			    (let ((next-possible-chunk (get-chunk chunk-x chunk-y chunk-z force-load)))
			      (setf (aref data data-x data-y data-z) next-possible-chunk)
			      next-possible-chunk))))))))))))
    (defun remove-chunk-from-chunk-array (&optional 
					 (chunk-x 0) (chunk-y 0) (chunk-z 0)
					    (chunk-array *chunk-array*))
      ;;FIXME::Some of the code is identical to get-chunk-from-chunk-array,
      ;;namely the when and lets establishing the bounds of the chunk coordinates
      (declare (type chunk-coord chunk-x chunk-y chunk-z))
      ;;if the coordinates are correct, return a chunk, otherwise return nil
      (let ((data-x (- chunk-x (the chunk-coord (chunk-array-x-min chunk-array)))))
	(declare (type chunk-coord data-x))
	(when (< -1 data-x ;;(the chunk-coord (chunk-array-size-x chunk-array))
		 (utility:etouq *chunk-array-default-size-x*))
	  (let ((data-y (- chunk-y (the chunk-coord (chunk-array-y-min chunk-array)))))
	    (declare (type chunk-coord data-y))
	    (when (< -1 data-y ;;(the chunk-coord (chunk-array-size-y chunk-array))
		     (utility:etouq *chunk-array-default-size-y*))
	      (let ((data-z (- chunk-z (the chunk-coord (chunk-array-z-min chunk-array)))))
		(declare (type chunk-coord data-z))
		(when (< -1 data-z ;;(the chunk-coord (chunk-array-size-z chunk-array))
			 (utility:etouq *chunk-array-default-size-z*))
		  (let ((data (chunk-array-array chunk-array)))
		    (declare (type chunk-array-data data))
		    ;;the chunk is in the chunk-array's bounds
		    (let ((possible-chunk
			   (aref data data-x data-y data-z)))
		      (when possible-chunk
			(setf (aref data data-x data-y data-z) nil)))))))))))

    (defun obtain-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0) (force-load nil))
      (declare (type chunk-coord chunk-x chunk-y chunk-z))
      (let ((chunk (or (get-chunk-from-chunk-array chunk-x chunk-y chunk-z force-load)
		       (get-chunk chunk-x chunk-y chunk-z force-load))))
	#+nil
	(unless (typep chunk 'chunk)
	  (error "not a chunk ~s" chunk))
	chunk))

    (defun obtain-chunk-from-block-coordinates (&optional (x 0) (y 0) (z 0) (force-load nil))
      (declare (type block-coord x y z))
      (let ((chunk-x (floor x (utility:etouq *chunk-size-x*)))
	    (chunk-y (floor y (utility:etouq *chunk-size-y*)))
	    (chunk-z (floor z (utility:etouq *chunk-size-z*))))
	(obtain-chunk chunk-x chunk-y chunk-z force-load)))
    
    (defun getobj (&optional (x 0) (y 0) (z 0))
      (declare (type block-coord x y z))
      (let ((chunk (obtain-chunk-from-block-coordinates x y z nil))
	    (inner-x (mod x (utility:etouq *chunk-size-x*)))
	    (inner-y (mod y (utility:etouq *chunk-size-y*)))
	    (inner-z (mod z (utility:etouq *chunk-size-z*))))
	(reference-inside-chunk chunk inner-x inner-y inner-z)))
    (defun (setf getobj) (value &optional (x 0) (y 0) (z 0))
      (declare (type block-coord x y z))
      (let ((chunk (obtain-chunk-from-block-coordinates x y z t))
	    (inner-x (mod x (utility:etouq *chunk-size-x*)))
	    (inner-y (mod y (utility:etouq *chunk-size-y*)))
	    (inner-z (mod z (utility:etouq *chunk-size-z*))))
	(setf (reference-inside-chunk chunk inner-x inner-y inner-z) value)))
    ;;FIXME::setobj is provided for backwards compatibility?
    (defun setobj (x y z new)
      (setf (getobj x y z) new))))

(defun chunk-coordinates-from-block-coordinates (&optional (x 0) (y 0) (z 0))
  ;;FIXME? combine with obtain-chunk-from-block-coordinates? 
  (declare (type block-coord x y z))
  (let ((chunk-x (floor x (utility:etouq *chunk-size-x*)))
	(chunk-y (floor y (utility:etouq *chunk-size-y*)))
	(chunk-z (floor z (utility:etouq *chunk-size-z*))))
    (values chunk-x chunk-y chunk-z)))

(defun maybe-move-chunk-array (player-x player-y player-z)
  ;;center the chunk array around the player, but don't always, only if above a certain
  ;;threshold
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (chunk-coordinates-from-block-coordinates
       (floor player-x)
       (floor player-y)
       (floor player-z))
    ;;FIXME::is this expensive to recompute every frame or does it matter?
    ;;maybe put it in the chunk array object?
    (let ((half-x-size (utility:etouq (floor *chunk-array-default-size-x* 2)))
	  (half-y-size (utility:etouq (floor *chunk-array-default-size-y* 2)))
	  (half-z-size (utility:etouq (floor *chunk-array-default-size-z* 2))))
      (let ((center-x (+ 
		       (chunk-array-x-min *chunk-array*)
		       half-x-size))
	    (center-y (+ 
		       (chunk-array-y-min *chunk-array*)
		       half-y-size))
	    (center-z (+ 
		       (chunk-array-z-min *chunk-array*)
		       half-z-size)))
	;;FIXME::hard-coded threshold for repositioning the chunk array? 4 chunks?
	#+nil
	(print (list (- chunk-x center-x)
		     (- chunk-y center-y)
		     (- chunk-z center-z)))
	(when (or (<= 4 (abs (- chunk-x center-x)))
		  (<= 4 (abs (- chunk-y center-y)))
		  (<= 4 (abs (- chunk-z center-z))))
	  ;;(format t "moving chunk array")
	  (reposition-chunk-array (- chunk-x half-x-size)
				  (- chunk-y half-y-size)
				  (- chunk-z half-z-size)))))))

(defun test ()
  (let ((times (expt 10 6)))
    (time (dotimes (x times) (setf (getobj 0 0 0) 0)))
    (time (dotimes (x times) (setf (world::getobj 0 0 0) 0))))
  (let ((times (expt 10 6)))
    (time (dotimes (x times) (getobj 0 0 0)))
    (time (dotimes (x times) (world::getobj 0 0 0)))))

;;For backwards compatibility
(defun unhashfunc (chunk-key)
  (with-chunk-key-coordinates (x y z) chunk-key
    (values (* (utility:etouq *chunk-size-x*) x)
	    (* (utility:etouq *chunk-size-y*) y)
	    (* (utility:etouq *chunk-size-z*) z))))
#+nil
(defun chunkhashfunc (x y z)
  (create-chunk-key x y z))
;;FIXME::clearworld does not handle loading and stuff?
(defun clearworld ()
  (setf *chunks* (make-chunk-table)
	*chunk-array* (create-chunk-array)))

(defparameter *maximum-allowed-chunks* (* 16 16 16))
(defun get-unloadable-chunks (&optional (x 0) (y 0) (z 0))
  (multiple-value-bind (x0 y0 z0)
      (chunk-coordinates-from-block-coordinates (floor x) (floor y) (floor z))
    (let ((difference (- (total-loaded-chunks) *maximum-allowed-chunks*)))
      (when (plusp difference)
	(let ((distance-sorted-chunks
	       (sort (alexandria:hash-table-keys world::*chunks*) #'< :key
		     (lambda (position)
		       ;;FIXME::destructuring bind of chunk-key happens in multiple places.
		       ;;fix?
		       (with-chunk-key-coordinates (x1 y1 z1) position
			 (let ((dx (- x1 x0))
			       (dy (- y1 y0))
			       (dz (- z1 z0)))
			   (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))))))
	  (safe-subseq distance-sorted-chunks difference))))))

(defun load-chunks-around (&optional (x 0) (y 0) (z 0))
  (multiple-value-bind (x0 y0 z0)
      (chunk-coordinates-from-block-coordinates (floor x) (floor y) (floor z))
    (block out
      (let ((chunk-count 0))
	(flet ((add-chunk (x y z)
		 (incf chunk-count)
		 ;;do something
		 (when (eq world::*empty-chunk* (world::get-chunk x y z nil))
		   ;;The chunk does not exist, therefore the *empty-chunk* was returned
		   )
		 (when (> chunk-count *maximum-allowed-chunks*)
		   ;;exceeded the allowed chunks to load
		   (return-from out))
		 ))
	  (let ((size 6))
	    (utility::dobox ((chunk-x (- x0 size) (+ x0 size))
			     (chunk-y (- y0 size) (+ y0 size))
			     (chunk-z (- z0 size) (+ z0 size)))
			    (add-chunk x y z))))))))

(defun safe-subseq (seq end)
  (subseq seq 0 (min (length seq) end)))

(defgeneric lispobj-dispatch (obj))

(defun value-dispatch (value)
  (typecase value
    (fixnum value)
    (otherwise (lispobj-dispatch value))))

(defun (setf num-getobj) (new i j k)
  (setobj i j k new))
(defun num-getobj (i j k)
  (let ((value (getobj i j k)))
    (value-dispatch value)))

(defmacro suite (bits position get)
  (let* ((bytespec `(byte ,bits ,position))
	 (access `(ldb ,bytespec
		       (num-getobj i j k))))
    `(progn
       (defun (setf ,get) (new i j k)
	 (setf ,access new))
       (defun ,get (i j k)
	 ,access))))

(progn
  (suite 8 0 getblock)
  (suite 4 8 getlight)
  (suite 4 12 skygetlight))

(defun blockify (blockid light sky)
  (dpb sky (byte 4 12)
       (dpb light (byte 4 8) blockid)))

(defmethod lispobj-dispatch ((obj character))
  (blockify (char-code obj) 0 0))

(defmethod lispobj-dispatch ((obj t))
  (blockify (logcount (sxhash obj)) 0 0))

(defmethod lispobj-dispatch ((obj symbol))
  (blockify 0 0 15))

(in-package :sandbox)
;;;;keeping track of the changes to the world
(progn
  (defparameter dirtychunks (queue:make-uniq-q))
  (defun clean-dirty ()
    (setf dirtychunks (queue:make-uniq-q)))
  (defun dirty-pop ()
    (queue:uniq-pop dirtychunks))
  (defun dirty-push (item)
    (queue:uniq-push item dirtychunks))
  (defun block-dirtify (i j k)
    (let ((imask (mod i 16))
	  (jmask (mod j 16))
	  (kmask (mod k 16)))
      (labels ((add (x y z)
		 (let ((chunk (world::obtain-chunk-from-block-coordinates x y z)))
		   (unless (eq chunk world::*empty-chunk*)
		     (dirty-push (world::chunk-key chunk)))))
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

#+nil
(defun block-dirtify-hashed (xyz)
  (multiple-value-bind (x y z) (world:unhashfunc xyz)
    (block-dirtify x y z)))
