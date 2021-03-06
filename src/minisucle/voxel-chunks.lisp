(defpackage :voxel-chunks
  (:use :cl :utility)
  (:export
   #:getobj
   #:block-coord))
(in-package :voxel-chunks)

;;The length of the side of the cube.
(defconstant +size+ 16)
;;Each chunk is a 16x16x16 cube.
(defconstant +total-size+ (expt +size+ 3))

(deftype chunk-coord () 'fixnum)
(deftype block-coord () 'fixnum)
;;bcoord = block-coord, ccoord = chunk-coord
(defun bcoord->ccoord (&optional (x 0) (y 0) (z 0))
  ;;[FIXME]? combine with obtain-chunk-from-block-coordinates? 
  (declare (type block-coord x y z))
  (values (floor x +size+) (floor y +size+) (floor z +size+)))
(deftype chunk-data () `(simple-array * (,+total-size+)))
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
(defun inner (x y z)
  (values (mod x +size+) (mod y +size+) (mod z +size+)))
;;;;************************************************************************;;;;
;;in order to be correct, the key has to store each value unaltered
;;This is for creating a key for a hash table
;; 'cx' stands for 'chunk-x' etc...
;;smaller names are easier to read and see.
(defun create-chunk-key (&optional (cx 0) (cy 0) (cz 0))
  (list cx cy cz))
(defmacro with-chunk-key-coordinates ((x y z) chunk-key &body body)
  `(destructuring-bind (,x ,y ,z) ,chunk-key
     ,@body))
#+nil
(defun spread-chunk-key (chunk-key)
  (apply 'values chunk-key))
;;For backwards compatibility
#+nil
(defun unhashfunc (chunk-key)
  (with-chunk-key-coordinates (x y z) chunk-key
    (values (* x +size+)
	    (* y +size+)
	    (* z +size+))))

(defparameter *empty-space* nil)
(defparameter *empty-chunk-data* nil)
;;(defparameter *empty-chunk* nil)
;;the empty-chunk is used as a placeholder when a chunk to reference is required
(defun create-empty-chunk ()
  (create-chunk 0 0 0 :data
		*empty-chunk-data*
		:type :empty))
(defun reset-empty-chunk-value (&optional (empty-space nil))
  (setf *empty-space* empty-space)
  (setf *empty-chunk-data* (downgrade-array:really-downgrade-array
			    (make-chunk-data :initial-element *empty-space*)))
  ;;(setf *empty-chunk* (create-empty-chunk))
  )

(defun empty-chunk-p (chunk)
  (or (null chunk)
      ;;(eq chunk *empty-chunk*)
      (eq (chunk-type chunk) :empty)))
(struct-to-clos:struct->class
 (defstruct chunk
   modified
   ;;last-saved
   type
   x y z
   key
   data
   
   ;;Invalidate a chunk. If used by the main cache to invalidate
   ;;chunks in chunk-array cursors.
   (alive? t)

   (last-read 0)
   (last-modified 0)
   (last-access 0)))

(defun kill-chunk (chunk)
  (setf (chunk-alive? chunk) nil
	(chunk-data chunk) *empty-chunk-data*
	(chunk-type chunk) :dead))

;;FIXME:detect if it actually of type chunk?
(defun valid-chunk-p (chunk)
  (and chunk
       (chunk-alive? chunk)))

(defun make-chunk-data (&rest rest &key (initial-element *empty-space*))
  (apply 'make-array
	 +total-size+
	 :initial-element initial-element
	 :element-type (downgrade-array:storage-type initial-element)
	 rest))

;;;;
(defun reference-inside-chunk (chunk rx ry rz)
  (declare (type inner-3d rx ry rz))
  (let ((data (chunk-data chunk)))
    ;;(declare (type chunk-data data))
    (row-major-aref data (chunk-ref rx ry rz))))
(defun (setf reference-inside-chunk) (value chunk rx ry rz)
  (declare (type inner-3d rx ry rz))
  (let ((data (chunk-data chunk)))
    ;;(declare (type chunk-data data))
    (setf (row-major-aref data (chunk-ref rx ry rz))
	  value)))
;;;;

(defun coerce-empty-chunk-to-regular-chunk (chunk)
  (when (eq (chunk-type chunk) :empty)
    (setf (chunk-data chunk) (make-chunk-data)
	  (chunk-type chunk) :normal)))

;;type can be either :NORMAL or :EMPTY. empty is used to signify that
;;all the chunk data is eql to *empty-space*
;;this is an optimization to save memory
(defun create-chunk (cx cy cz &key (type :normal) data)
  (declare (type chunk-coord cx cy cz))
  (make-chunk :x cx :y cy :z cz
	      :key (create-chunk-key cx cy cz)
	      :data (ecase type
		      (:normal (or data (make-chunk-data)))
		      (:empty *empty-chunk-data*))
	      :type type))

;;;;************************************************************************;;;;
"
Chunk cache is a hash table. 
(x y z) -> chunk object

When removing or setting chunks, kill the chunk which is no longer to be used."


(defun make-chunk-cache ()
  (make-hash-table :test 'equal))
(defparameter *chunks* (make-chunk-cache))
(defun get-chunk-in-cache (key &optional (cache *chunks*))
  (gethash key cache))
(defun chunk-in-cache-p (key &optional (cache *chunks*))
  (multiple-value-bind (value existsp) (get-chunk-in-cache key cache)
    (declare (ignorable value))
    existsp))

(defun set-chunk-in-cache (key chunk &optional (cache *chunks*))
  (kill-old-chunk key)
  (setf (gethash key cache) chunk))
(defun delete-chunk-in-cache (key &optional (cache *chunks*))
  (when (kill-old-chunk key cache)
    (remhash key cache)))
(defun kill-old-chunk (key &optional (cache *chunks*))
  (multiple-value-bind (old-chunk existp) (gethash key cache)
    (when existp
      (kill-chunk old-chunk)
      (values t))))
(defun total-chunks-in-cache (&optional (cache *chunks*))
  (hash-table-count cache))


;;;;************************************************************************;;;;
(defun (setf getobj) (value x y z)
  (setobj value x y z))
(defun setobj (value x y z ;;space
	       )
  (multiple-value-bind (cx cy cz) (bcoord->ccoord x y z)
    (let ((key (create-chunk-key cx cy cz)))
      (multiple-value-bind (chunk existp)
	  (get-chunk-in-cache key)
	;;Create a new chunk if it does not already exist.
	(unless existp
	  (let ((new (create-chunk cx cy cz)))
	    (set-chunk-in-cache key new)
	    (setf chunk new)))	
	(multiple-value-bind (rx ry rz) (inner x y z)
	  (setf (reference-inside-chunk chunk rx ry rz) value))))))
(defun getobj (x y z ;;space
	       )
  (multiple-value-bind (cx cy cz) (bcoord->ccoord x y z)
    (multiple-value-bind (chunk existp)
	(get-chunk-in-cache (create-chunk-key cx cy cz))
      (if existp
	  (multiple-value-bind (rx ry rz) (inner x y z)
	    (reference-inside-chunk chunk rx ry rz))
	  *empty-space*))))

;;;;;
(reset-empty-chunk-value)


