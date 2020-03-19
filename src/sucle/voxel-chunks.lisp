(defpackage #:voxel-chunks
  (:use :cl)
  (:nicknames :vocs)
  (:export 
   #:unhashfunc
   #:chunkhashfunc
   #:getobj
   #:setobj
   #:clearworld)
  (:export
   #:*chunks*
   #:total-loaded-chunks
   #:*chunk-array*

   #:*chunk-array-default-size-x*
   #:*chunk-array-default-size-y*
   #:*chunk-array-default-size-z*  
   
   #:block-coord
   #:chunk-coord
   #:chunk-data
   #:chunk-modified
   #:chunk-key
   
   #:chunk-coordinates-from-block-coordinates
   #:create-chunk-key

   #:empty-chunk-p
   #:reset-empty-chunk-value  
   
   #:create-chunk
   #:make-chunk-from-key-and-data
   #:with-chunk-key-coordinates
   
   #:obtain-chunk-from-block-coordinates
   #:obtain-chunk-from-chunk-key

   #:remove-chunk-from-chunk-array
   #:remove-chunk-at
   #:get-chunk-at
   #:set-chunk-at

   #:chunk-array-x-min
   #:chunk-array-y-min
   #:chunk-array-z-min
   
   #:reposition-chunk-array
   
   #:chunk-worth-saving
   #:chunk-exists-p

   #:+size+
   #:+total-size+))
(in-package #:voxel-chunks)

;;;;************************************************************************;;;;
;;Chunk cache
;;equal is used because the key is a list of the chunk coordinates
(defun make-chunk-cache ()
  (make-hash-table :test 'equal))
(defparameter *chunks* (make-chunk-cache))
(defun set-chunk-in-cache (key chunk &optional (cache *chunks*))
  (setf (gethash key cache) chunk))
(defun get-chunk-in-cache (key &optional (cache *chunks*))
  ;;return (values chunk exist-p)
  (gethash key cache))
(defun delete-chunk-in-cache (key &optional (cache *chunks*))
  (remhash key cache))
(defun chunk-in-cache-p (key &optional (cache *chunks*))
  (multiple-value-bind (value existsp) (get-chunk-in-cache key cache)
    (declare (ignorable value))
    existsp))
(defun total-chunks-in-cache (&optional (cache *chunks*))
  (hash-table-count cache))
;;;;************************************************************************;;;;

(struct-to-clos:struct->class
 (defstruct chunk
   modified
   ;;last-saved
   type
   x
   y
   z
   key
   data))

;;lets make it 16, and not care about the other parameters for now.
(defconstant +size+ 16)
(defconstant +total-size+ (expt +size+ 3))

;;[FIXME]chunk-coord and block-coord being fixnums is not theoretically correct,
;;but its still a lot of space?
(deftype chunk-coord () 'fixnum)
(deftype block-coord () 'fixnum)
(deftype chunk-data () `(simple-array t (,+total-size+)))
;;The inner coord
(deftype inner-flat () `(integer 0 ,+total-size+))
;;The remainder after flooring by the chunk size
(deftype inner-3d () `(integer 0 ,+size+))
(utility:with-unsafe-speed
  (declaim (inline chunk-ref)
	   (ftype (function (inner-3d inner-3d inner-3d)
			    inner-flat)
		  chunk-ref))
  (defun chunk-ref (rx ry rz)
    (declare (type inner-3d rx ry rz))
    (+ (* ;;size-z
	+size+
	(+ (*;;size-y
	    +size+
	    ry)
	   rz))
       rx)))
;;in order to be correct, the key has to store each value unaltered
;;This is for creating a key for a hash table
;; 'cx' stands for 'chunk-x' etc...
;;smaller names are easier to read and see.
(defun create-chunk-key (&optional (cx 0) (cy 0) (cz 0))
  (list cx cy cz))
(defmacro with-chunk-key-coordinates ((x y z) chunk-key &body body)
  `(destructuring-bind (,x ,y ,z) ,chunk-key
     ,@body))
(defun obtain-chunk-from-chunk-key (chunk-key &optional force-load)
  ;;[FIXME]is this a good api?
  (with-chunk-key-coordinates (x y z) chunk-key 
    (obtain-chunk x y z force-load)))

(defparameter *empty-space* nil)
(defparameter *empty-chunk-data* nil)
(defparameter *empty-chunk* nil)
;;the empty-chunk is used as a placeholder when a chunk to reference is required
(defun reset-empty-chunk-value (&optional (empty-space nil))
  (setf *empty-space* empty-space)
  (setf *empty-chunk-data* (make-chunk-data :initial-element *empty-space*))
  (setf *empty-chunk* (create-chunk 0 0 0 :data
				    *empty-chunk-data*
				    :type :empty)))

(defun empty-chunk-p (chunk)
  (or (null chunk)
      (eq chunk *empty-chunk*)
      (eq (chunk-type chunk) :empty)))
(defun make-chunk-data (&rest rest &key (initial-element *empty-space*))
  (apply 'make-array
	 +total-size+
	 :initial-element initial-element
	 rest))

(defun coerce-empty-chunk-to-regular-chunk (chunk)
  (when (eq (chunk-type chunk) :empty)
    (setf (chunk-data chunk) (make-chunk-data)
	  (chunk-type chunk) :normal)))

;;type can be either :NORMAL or :EMPTY. empty is used to signify that
;;all the chunk data is eql to *empty-space*
;;this is an optimization to save memory
(defun create-chunk (cx cy cz &key (type :normal) (data (make-chunk-data)))
  (declare (type chunk-coord cx cy cz))
  (make-chunk :x cx :y cy :z cz
	      :key (create-chunk-key cx cy cz)
	      :data (ecase type
		      (:normal (or data (make-chunk-data)))
		      (:empty *empty-chunk-data*))
	      :type type))
(reset-empty-chunk-value)
(defun make-chunk-from-key-and-data (key data)
  (with-chunk-key-coordinates (cx cy cz) key
    (make-chunk :x cx :y cy :z cz :key key :data data :type :normal)))

;;;;;;;;;;;;;

;;size of one side of the chunk array.
(defconstant +ca-size+ 32)
(defparameter *chunk-array-empty-value* nil)
;;(struct-to-clos:struct->class)
(defstruct chunk-array
  (array (make-array (list +ca-size+ +ca-size+ +ca-size+)
		     :initial-element *chunk-array-empty-value*))
  (x-min 0)
  (y-min 0)
  (z-min 0))
(deftype chunk-array-data ()
  `(simple-array t (,+ca-size+ ,+ca-size+ ,+ca-size+)))

(defun fill-array (array value)
  (declare (type chunk-array-data array))
  (dotimes (i (array-total-size array))
    (setf (row-major-aref array i) value)))
;;'rx' 'ry' and 'rz' stand for remainder
;;;;
(defun reference-inside-chunk (chunk rx ry rz)
  (declare (type inner-3d rx ry rz))
  (row-major-aref (the chunk-data (chunk-data chunk))
		  (chunk-ref rx ry rz)))
(defun (setf reference-inside-chunk) (value chunk rx ry rz)
  (declare (type inner-3d rx ry rz))
  (setf (row-major-aref (the chunk-data (chunk-data chunk))
			(chunk-ref rx ry rz))
	value))
;;;;
;;[FIXME]actually load chunks
(defun load-chunk (&optional (cx 0) (cy 0) (cz 0))
  (error "This should not appear.")
  (create-chunk cx cy cz))

(defun get-chunk (&optional (cx 0) (cy 0) (cz 0) (force-load nil)
		    &aux (key (create-chunk-key cx cy cz)))
  (declare (type chunk-coord cx cy cz))
  (multiple-value-bind (value existsp) (get-chunk-in-cache key)
    (cond (existsp (values value t))
	  (force-load
	   (format t "Caching new chunk:~a" key)
	   (error "This should not appear.")
	   ;;[FIXME]load chunks here, unload chunks here?
	   (let ((new-chunk (load-chunk cx cy cz)))
	     (set-chunk-in-cache key new-chunk)
	     (values new-chunk t)))
	  (t (values *empty-chunk* nil)))))
    
(defun create-chunk-array ()
  (make-chunk-array))
(defparameter *chunk-array* (create-chunk-array))

(defun reposition-chunk-array (&optional 
				 (cx 0) (cy 0) (cz 0)
				 (chunk-array *chunk-array*))
  (declare (type chunk-coord cx cy c))
  (setf (chunk-array-x-min chunk-array) cx
	(chunk-array-y-min chunk-array) cy
	(chunk-array-z-min chunk-array) cz)
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
    (when (< -1 data-x +ca-size+)
      (let ((data-y (- chunk-y (the chunk-coord (chunk-array-y-min chunk-array)))))
	(declare (type chunk-coord data-y))
	(when (< -1 data-y +ca-size+)
	  (let ((data-z (- chunk-z (the chunk-coord (chunk-array-z-min chunk-array)))))
	    (declare (type chunk-coord data-z))
	    (when (< -1 data-z +ca-size+)
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
		      (let ((next-possible-chunk (get-chunk chunk-x chunk-y chunk-z force-load)))
			(setf (aref data data-x data-y data-z) next-possible-chunk)
			next-possible-chunk)))))))))))
(defun remove-chunk-from-chunk-array (&optional 
					(chunk-x 0) (chunk-y 0) (chunk-z 0)
					(chunk-array *chunk-array*))
  ;;[FIXME]Some of the code is identical to get-chunk-from-chunk-array,
  ;;namely the when and lets establishing the bounds of the chunk coordinates
  (declare (type chunk-coord chunk-x chunk-y chunk-z))
  ;;if the coordinates are correct, return a chunk, otherwise return nil
  (let ((data-x (- chunk-x (the chunk-coord (chunk-array-x-min chunk-array)))))
    (declare (type chunk-coord data-x))
    (when (< -1 data-x +ca-size+)
      (let ((data-y (- chunk-y (the chunk-coord (chunk-array-y-min chunk-array)))))
	(declare (type chunk-coord data-y))
	(when (< -1 data-y +ca-size+)
	  (let ((data-z (- chunk-z (the chunk-coord (chunk-array-z-min chunk-array)))))
	    (declare (type chunk-coord data-z))
	    (when (< -1 data-z +ca-size+)
	      (let ((data (chunk-array-array chunk-array)))
		(declare (type chunk-array-data data))
		;;the chunk is in the chunk-array's bounds
		(let ((possible-chunk
		       (aref data data-x data-y data-z)))
		  (when possible-chunk
		    (setf (aref data data-x data-y data-z) nil)))))))))))

(defun obtain-chunk (&optional (cx 0) (cy 0) (cz 0) (force-load nil))
  (declare (type chunk-coord cx cy cz))
  (get-chunk-from-chunk-array cx cy cz force-load))

(defun obtain-chunk-from-block-coordinates (&optional (x 0) (y 0) (z 0) (force-load nil))
  (declare (type block-coord x y z))
  (multiple-value-call
      'obtain-chunk
    (bcoord->ccoord x y z)
    force-load))

(defun inner (x y z)
  (values (mod x +size+)
	  (mod y +size+)
	  (mod z +size+)))

;;;;
(defun getobj (&optional (x 0) (y 0) (z 0))
  (declare (type block-coord x y z))
  (multiple-value-bind (rx ry rz) (inner x y z)
    (reference-inside-chunk
     (obtain-chunk-from-block-coordinates x y z nil)
     rx ry rz)))
(defun (setf getobj) (value &optional (x 0) (y 0) (z 0))
  (declare (type block-coord x y z))
  (let ((chunk (obtain-chunk-from-block-coordinates x y z t)))
    ;;chunk is not *empty-chunk* because of force-load being passed to obtain-chunk.
    ;;chunk might be a chunk of type :EMPTY with shared data, but since it is being set,
    ;;coerce it to a regular chunk
    (coerce-empty-chunk-to-regular-chunk chunk)
    (setf (chunk-modified chunk) t)
    (multiple-value-bind (rx ry rz) (inner x y z)
      (setf (reference-inside-chunk chunk rx ry rz) value))))
    ;;[FIXME]setobj is provided for backwards compatibility?
;;;;
(defun setobj (x y z new)
  (setf (getobj x y z) new))
;;;;

;;bcoord = block-coord, ccoord = chunk-coord
(defun bcoord->ccoord (&optional (x 0) (y 0) (z 0))
  ;;[FIXME]? combine with obtain-chunk-from-block-coordinates? 
  (declare (type block-coord x y z))
  (values (floor x +size+) (floor y +size+) (floor z +size+)))


;;For backwards compatibility
(defun unhashfunc (chunk-key)
  (with-chunk-key-coordinates (x y z) chunk-key
    (values (* x +size+)
	    (* y +size+)
	    (* z +size+))))

#+nil
(defun chunkhashfunc (x y z)
  (create-chunk-key x y z))
;;[FIXME]clearworld does not handle loading and stuff?
(defun clearworld ()
  (setf *chunks* (make-chunk-cache)
	*chunk-array* (create-chunk-array))
  (values))

;;a chunk is not worth saving if all the values are the empty value
;;[FIXME]optimize?
;;[FIXME]what about terrain generation? if a chunk is generated with terrain,
;;then erased with the *empty-space* value, it will be reloaded.
(defun chunk-worth-saving (chunk)
  (let ((empty-space *empty-space*))
    (not (every (lambda (x) (eql x empty-space))
		(chunk-data chunk)))))
