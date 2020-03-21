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
   #:*chunk-array*

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

   #:chunk-array-x-min
   #:chunk-array-y-min
   #:chunk-array-z-min
   
   #:reposition-chunk-array
   
   #:chunk-worth-saving
   #:chunk-exists-p

   #:block-coord
   #:chunk-coord
   #:+size+
   #:+total-size+))
(in-package #:voxel-chunks)

;;lets make it 16, and not care about the other parameters for now.
;;each chunk is a 16x16x16 cube then.
(defconstant +size+ 16)
(defconstant +total-size+ (expt +size+ 3))

;;;;************************************************************************;;;;
;;Chunk cache
;;equal is used because the key is a list of the chunk coordinates
;;When updating the cache with 'set' or 'delete' invalidate the
;;chunks that are being eviscerated.
;;FIXME: add locks to chunks?
;;FIXME::*true-bit-size* and (chunks-total-bits) don't line up.
(defparameter *true-bit-size* 0)
(defun make-chunk-cache ()
  (make-hash-table :test 'equal))
(defparameter *chunks* (make-chunk-cache))
(defun set-chunk-in-cache (key chunk &optional (cache *chunks*))
  (kill-old-chunk key)
  ;;(incf *true-bit-size* (array-truesize (chunk-data chunk)))
  (setf (gethash key cache) chunk)
  (prune-cache))
(defun get-chunk-in-cache (key &optional (cache *chunks*))
  ;;return (values chunk exist-p)
  (gethash key cache))
(defun delete-chunk-in-cache (key &optional (cache *chunks*))
  (when (kill-old-chunk key cache)
    (remhash key cache)))
(defun kill-old-chunk (key &optional (cache *chunks*))
  (multiple-value-bind (old-chunk existp) (gethash key cache)
    (when existp
      ;;FIXME:add a logger
      ;;(format t "~%Eviscerating old chunk at: ~a" key)
      (kill-chunk old-chunk)
      ;;(decf *true-bit-size* (array-truesize (chunk-data old-chunk)))
      (values t))))
(defun chunk-in-cache-p (key &optional (cache *chunks*))
  (multiple-value-bind (value existsp) (get-chunk-in-cache key cache)
    (declare (ignorable value))
    existsp))
(defun total-chunks-in-cache (&optional (cache *chunks*))
  (hash-table-count cache))

(defparameter *persist* t)
#+nil
(/ (* 300 (expt 1024 2))
   (* +total-size+ 8))
(defparameter *cache-limit* (* (expt 16 3)))
(defparameter *cache-reduction* 0.2)
(defparameter *prune-lock* (bt:make-lock))
(defun prune-cache (&optional (limit *cache-limit*) &aux (reduction
							  (* limit *cache-reduction*)))
  (bt:with-lock-held (*prune-lock*)
    (let ((total (total-chunks-in-cache)))
      (when (< limit total)
	(let ((chunks (sort (alexandria:hash-table-alist *chunks*)
			    '<
			    :key (lambda (x)
				   (chunk-last-access (cdr x))))))
	  (let ((amount-pruning (- total (- limit reduction))))
	    ;;(print total)
	    ;;(format t "~%Pruning ~a chunks" amount-pruning)
	    (loop :repeat amount-pruning
	       :for pair :in chunks :do
	       (destructuring-bind (key . chunk) pair
		 (when (and
			*persist*
			;;when the chunk is not obviously empty
			(not (voxel-chunks:empty-chunk-p chunk))
			;;if it wasn't modified, no point in saving
			(voxel-chunks:chunk-modified chunk))
		   (savechunk key))
		 (delete-chunk-in-cache key)))))
	(setf *true-bit-size* (chunks-total-bits))
	;;(format t "~%~a" *true-bit-size*)
	))))
;;;;************************************************************************;;;;

;;[FIXME]chunk-coord and block-coord being fixnums is not theoretically correct,
;;but its still a lot of space?
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
;;in order to be correct, the key has to store each value unaltered
;;This is for creating a key for a hash table
;; 'cx' stands for 'chunk-x' etc...
;;smaller names are easier to read and see.
(defun create-chunk-key (&optional (cx 0) (cy 0) (cz 0))
  (list cx cy cz))
(defmacro with-chunk-key-coordinates ((x y z) chunk-key &body body)
  `(destructuring-bind (,x ,y ,z) ,chunk-key
     ,@body))
(defun spread-chunk-key (chunk-key)
  (apply 'values chunk-key))
;;For backwards compatibility
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
  (setf *empty-chunk-data* (really-downgrade-array
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

(defun make-chunk-data (&rest rest &key (initial-element *empty-space*))
  (apply 'make-array
	 +total-size+
	 :initial-element initial-element
	 :element-type (storage-type initial-element)
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
(defun create-chunk (cx cy cz &key (type :normal) (data (make-chunk-data)))
  (declare (type chunk-coord cx cy cz))
  (make-chunk :x cx :y cy :z cz
	      :key (create-chunk-key cx cy cz)
	      :data (ecase type
		      (:normal (or data (make-chunk-data)))
		      (:empty *empty-chunk-data*))
	      :type type))

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
  (z-min 0)
  ;;New chunks that were recently obtained from the global cache
  ;;go here.
  fresh)
(deftype chunk-array-data ()
  `(simple-array t (,+ca-size+ ,+ca-size+ ,+ca-size+)))

(defun call-fresh-chunks-and-end (fun &optional 
					(chunk-array *chunk-array*))
  (dolist (item (chunk-array-fresh chunk-array))
    (funcall fun item))
  (setf (chunk-array-fresh chunk-array) nil))

;;'rx' 'ry' and 'rz' stand for remainder

(defun get-chunk (cx cy cz
		  &optional (force-load nil)
		  &aux (key (create-chunk-key cx cy cz)))
  (declare (type chunk-coord cx cy cz))
  (multiple-value-bind (value existsp) (get-chunk-in-cache key)
    (cond (existsp (values value t))
	  (force-load
	   ;;(format t "~%Caching new chunk:(~a ~a ~a)" cx cy cz)
	   (let ((new-chunk (loadchunk (create-chunk-key cx cy cz))))
	     (values new-chunk t)))
	  (t
	   (error "Loading is mandatory")
	   #+nil
	   (values
	      (create-empty-chunk)
	      ;;*empty-chunk*
	      nil)))))
    
(defun create-chunk-array ()
  (make-chunk-array))
(defparameter *chunk-array* (create-chunk-array))

;;FIXME:optimize?
(defun fill-array (array value)
  (dotimes (i (array-total-size array))
    (setf (row-major-aref array i) value)))
(defun reposition-chunk-array (cx cy cz
			       &optional 
				 (chunk-array *chunk-array*))
  (declare (type chunk-coord cx cy c))
  (setf (chunk-array-x-min chunk-array) cx
	(chunk-array-y-min chunk-array) cy
	(chunk-array-z-min chunk-array) cz)
  (fill-array (chunk-array-array chunk-array) *chunk-array-empty-value*)
  (values))

(defmacro %get-chunk-from-ca
    ((data chunk-array-min c &optional (size '+ca-size+)) &body body)
  `(let ((,data (- ,c (the chunk-coord (,chunk-array-min chunk-array)))))
     (declare (type chunk-coord ,data))
     (when (< -1 ,data ,size)
       ,@body)))

(defun kill-chunk (chunk)
  (setf (chunk-alive? chunk) nil
	(chunk-data chunk) *empty-chunk-data*
	(chunk-type chunk) :dead))

;;FIXME:detect if it actually of type chunk?
(defun valid-chunk-p (chunk)
  (and chunk
       (chunk-alive? chunk)))

;;if the coordinates are correct, return a chunk, otherwise return nil
(defun obtain-chunk
    (cx cy cz  &optional (force-load nil) (chunk-array *chunk-array*))
  (declare (type chunk-coord cx cy cz))
  (block return 
    (%get-chunk-from-ca (nx chunk-array-x-min cx)
      (%get-chunk-from-ca (ny chunk-array-y-min cy)
	(%get-chunk-from-ca (nz chunk-array-z-min cz)
	  (let* ((data (chunk-array-array chunk-array))
		 (maybe-chunk (aref data nx ny nz)))
	    (declare (type chunk-array-data data))
	    (cond ((and (valid-chunk-p maybe-chunk)
			;;This check is unnecessary if we clear the chunk array every time
			;;the position updates. combined with hysteresis, the relatively
			;;slow filling should not happen often
			#+nil
			(chunk-coordinates-match-p possible-chunk chunk-x chunk-y chunk-z))
		   ;;Return the chunk found because it is not nil
		   ;;and the coordinates are correct.
		   (return-from return maybe-chunk))
		  ;;Retrieve a chunk, place it in the chunk array,
		  ;;and return it.
		  (t
		   (let ((definitely-chunk (get-chunk cx cy cz force-load)))
		     (push definitely-chunk (chunk-array-fresh chunk-array))
		     (setf (aref data nx ny nz) definitely-chunk)
		     (return-from return definitely-chunk))))))))

    (error "chunk not within chunk array!!!!!")))

#+nil
(defun remove-chunk-from-chunk-array
    (cx cy cz &optional (chunk-array *chunk-array*))
  (declare (type chunk-coord cx cy cz))
  (%get-chunk-from-ca (nx chunk-array-x-min cx)
    (%get-chunk-from-ca (ny chunk-array-y-min cy)
      (%get-chunk-from-ca (nz chunk-array-z-min cz)
	(let* ((data (chunk-array-array chunk-array))
	       (maybe-chunk (aref data nx ny nz)))
	  (declare (type chunk-array-data data))
	  ;;The chunk is found, so remove it.  
	  (when maybe-chunk
	    (setf (aref data nx ny nz) nil)
	    (values t)))))))

;;
#+nil
(defun obtain-chunk-from-chunk-key (chunk-key &optional force-load)
  ;;[FIXME]is this a good api?
  (multiple-value-call 'obtain-chunk
    (spread-chunk-key chunk-key)
    force-load))
(defun obtain-chunk-from-block-coordinates (x y z &optional (force-load nil))
  (declare (type block-coord x y z))
  (multiple-value-call 'obtain-chunk (bcoord->ccoord x y z) force-load))
;;;
;;;;
(defparameter *time* 0)
;;FIXME::these functions increment '*time*' in a way that
;;does not lock. Unsafe? Does a small error really matter?
(defun getobj (x y z)
  (declare (type block-coord x y z))
  (let ((chunk (obtain-chunk-from-block-coordinates x y z t)))
    (let ((time *time*))
      (incf *time*)
      (setf (chunk-last-read chunk) time
	    (chunk-last-access chunk) time))
    (multiple-value-bind (rx ry rz) (inner x y z)
      (reference-inside-chunk chunk rx ry rz))))
(defun (setf getobj) (value x y z)
  (declare (type block-coord x y z))
  (let ((chunk (obtain-chunk-from-block-coordinates x y z t)))
    ;;chunk is not *empty-chunk* because of force-load being passed to obtain-chunk.
    ;;chunk might be a chunk of type :EMPTY with shared data, but since it is being set,
    ;;coerce it to a regular chunk
    (coerce-empty-chunk-to-regular-chunk chunk)
    (setf (chunk-modified chunk) t)
    (let ((time *time*))
      (incf *time*)
      (setf (chunk-last-modified chunk) time
	    (chunk-last-access chunk) time))
    (let ((array (chunk-data chunk)))
      (when (not (typep value (array-element-type array)))
	(let* (;;(old-size (array-truesize array))
	       (new-array (really-downgrade-array array value))
	       ;;(new-size (array-truesize new-array))
	       )
	  ;;(incf *true-bit-size* (- new-size old-size))
	  (setf (chunk-data chunk) new-array))))
    (multiple-value-bind (rx ry rz) (inner x y z)
      (setf (reference-inside-chunk chunk rx ry rz) value))))
;;[FIXME]setobj is provided for backwards compatibility?
;;;;
(defun setobj (x y z new)
  (setf (getobj x y z) new))
;;;;

#+nil
(defun chunkhashfunc (x y z)
  (create-chunk-key x y z))
;;[FIXME]clearworld does not handle loading and stuff?
(defun clearworld ()
  (setf *true-bit-size* 0)
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

;;center the chunk array around the player, but don't always, only if above a certain
;;threshold
;;[FIXME]is this expensive to recompute every frame or does it matter?
;;maybe put it in the chunk array object?
;;return t if it was moved, nil otherwise
(defparameter *reposition-chunk-array-threshold* 2)
(defun maybe-move-chunk-array (cx cy cz &optional (threshold *reposition-chunk-array-threshold*)
					  (chunk-array *chunk-array*))
  (let* ((half-size (floor voxel-chunks::+ca-size+ 2))
	 (center-x (+ half-size (chunk-array-x-min *chunk-array*)))
	 (center-y (+ half-size (chunk-array-y-min *chunk-array*)))
	 (center-z (+ half-size (chunk-array-z-min *chunk-array*))))
    (when (or (<= threshold (abs (- cx center-x)))
	      (<= threshold (abs (- cy center-y)))
	      (<= threshold (abs (- cz center-z))))
      ;;(format t "moving chunk array")
      (voxel-chunks:reposition-chunk-array
       (- cx half-size)
       (- cy half-size)
       (- cz half-size)
       chunk-array)
      (values t))))

(struct-to-clos:struct->class
 (defstruct cursor
   (chunk-array *chunk-array*)
   (x 0)
   (y 0)
   (z 0)
   (threshold *reposition-chunk-array-threshold*)
   (dirty t)
   ;;FIXME:difference between threshold and radius?
   (radius 6)))

(defun set-cursor-position
    (px py pz &optional (cursor (make-cursor)) (chunk-array *chunk-array*))
  (multiple-value-bind (newx newy newz)
      (vocs::bcoord->ccoord
       (floor px)
       (floor py)
       (floor pz))
    (setf (cursor-x cursor) newx
	  (cursor-y cursor) newy
	  (cursor-z cursor) newz)
    (when (maybe-move-chunk-array newx newy newz (cursor-threshold cursor) chunk-array)
      (setf (cursor-dirty cursor) t))))


(struct-to-clos:struct->class
 (defstruct pen
   cursor
   chunk-array))

(defun create-pen ()
  (let* ((ca (create-chunk-array))
	 (cursor (make-cursor :chunk-array ca :x 0 :y 0 :z 0 :threshold 8 :radius 8)))
    (make-pen :cursor cursor :chunk-array ca)))

(defun move-pen (px py pz pen)
  (let ((c (pen-cursor pen))
	(ca (pen-chunk-array pen)))
    (set-cursor-position px py pz c ca)))

(defparameter *pen* (create-pen))
(defun (setf pen-get) (value x y z &optional (pen *pen*))
  (let ((*chunk-array* (pen-chunk-array pen)))
    (move-pen x y z pen)
    (setf (getobj x y z) value)))
(defun pen-get (x y z &optional (pen *pen*))
  (let ((*chunk-array* (pen-chunk-array pen)))
    (move-pen x y z pen)
    (getobj x y z)))

;;;;************************************************************************;;;;
;;;;<PERSIST-WORLD>
;;[FIXME]move generic loading and saving with printer and conspack to a separate file?
;;And have chunk loading in another file?

(defparameter *chunk-io-lock* (bt:make-recursive-lock))

(defun savechunk (key)
  (bt:with-recursive-lock-held (*chunk-io-lock*)
    (let ((chunk (get-chunk-in-cache key)))
      (when (chunk-alive? chunk)   
	;;write the chunk to disk if its worth saving
	;;otherwise, if there is a preexisting file, destroy it
	(if (voxel-chunks:chunk-worth-saving chunk)
	    (%savechunk chunk)
	    (deletechunk key))))))

(defun %savechunk (chunk &aux (position (voxel-chunks:chunk-key chunk)))
  ;;(format t "~%saved chunk ~s" position)
  (when (not (chunk-alive? chunk))
    (error "Attempting to save dead chunk!!!"))
  (crud:crud-update
   (chunk-coordinate-to-filename position)
   (voxel-chunks:chunk-data chunk)))

;;Read from the database and put the chunk into the cache.
(defun loadchunk (key)
  (bt:with-recursive-lock-held (*chunk-io-lock*)
    (or
     ;;Check once again that it really does not exist yet.
     (get-chunk-in-cache key)
     (let ((chunk (%loadchunk key)))
       (voxel-chunks::set-chunk-in-cache key chunk)
       chunk))))

(defun make-chunk-from-key-and-data (key data)
  (with-chunk-key-coordinates (cx cy cz) key
    (make-chunk :x cx :y cy :z cz :key key :data data :type :normal)))
;;Merely load a chunk from the database, don't put in in the cache
(defun %loadchunk (chunk-coordinates)
  ;;(format t "~%Loading chunk at ~a" chunk-coordinates)
  (let ((data (crud:crud-read (chunk-coordinate-to-filename chunk-coordinates))))
    (flet ((make-data (data)
	     (let ((chunk-data (coerce data '(simple-array t (*)))))
	       (assert (= 4096 (length chunk-data))
		       nil
		       "offending chunk ~a"
		       chunk-coordinates)
	       (voxel-chunks:make-chunk-from-key-and-data
		chunk-coordinates
		(really-downgrade-array chunk-data)))))
      (typecase data
	(list
	 (ecase (length data)
	   (0
	    ;;if data is nil, just load an empty chunk
	    (voxel-chunks:with-chunk-key-coordinates (x y z) chunk-coordinates
						     (voxel-chunks:create-chunk x y z :type :empty)))

	   (3 ;;[FIXME]does this even work?
	    (error "world format invalid")
	    ;;FIXME: use defmethod to extend the interface?
	    #+nil
	    (destructuring-bind (blocks light sky) data
	      (let ((len (length blocks)))
		(let ((new (make-array len)))
		  (dotimes (i len)
		    (setf (aref new i)
			  (blockify (aref blocks i)  (aref light i) (aref sky i))))
		  (voxel-chunks:make-chunk-from-key-and-data chunk-coordinates new)))))
	   (1 (make-data (car data)))))
	(otherwise (make-data data))))))

(defun deletechunk (key)
  (crud:crud-delete (chunk-coordinate-to-filename key)))

;;The world is saved as a directory full of files named (x y z) in block coordinates, with
;;x and y swizzled
#+nil
(defun filename-to-chunk-coordinate (filename-position-list)
  (let ((position
	 (mapcar
	  ;;[FIXME]assumes chunks are 16 by 16 by 16
	  (lambda (n) (floor n 16))
	  filename-position-list)))
    (rotatef (third position)
	     (second position))
    position))
(defun chunk-coordinate-to-filename (chunk-coordinate)
  (let ((position-list (multiple-value-list (voxel-chunks:unhashfunc chunk-coordinate))))
    (rotatef (second position-list)
	     (third position-list))
    position-list))

;;;;</PERSIST-WORLD>


(defun downgrade-array (&rest items)
  (block out
    (let ((min 0)
	  (max 0))
      (flet ((check-thing (item)
	       (typecase item
		 (fixnum
		  (when (< max item)
		    (setf max item))
		  (when (< item min)
		    (setf min item)))
		 (otherwise (return-from out t)))))
	(dolist (item items)
	  (typecase item
	    (array (dotimes (i (array-total-size item))
		     (let ((item (row-major-aref item i)))
		       (check-thing item))))
	    ((and (not null) sequence) (map nil #'check-thing item))
	    (t (check-thing item)))))
      (upgraded-array-element-type (num-type min max)))))
(defun typetest (item test)
  (let ((result (typep item test)))
    (if result
	(format t "~% ~a is ~a" item test)
	(format t "~% ~a is not ~a" item test))))
(defun test ()
  (typetest 1 '(signed-byte 1))
  (typetest -1 '(signed-byte 1))
  (typetest 256 '(signed-byte 8))
  (typetest -256 '(signed-byte 8))
  (typetest 127 '(signed-byte 8))
  (typetest -127 '(signed-byte 8))
  (typetest 128 '(signed-byte 8))
  (typetest -128 '(signed-byte 8)))

;;From the CLHS
;;(integer -(2^(s-1)) (2^(s-1))-1) -> signed byte
;;(integer 0 n) for n=(2^s)-1 -> unsigned byte
(defun num-type (min max)
  (cond ((zerop min)
	 ;;unsigned-byte
	 `(unsigned-byte ,(1+ (upgrade max))))
	(t `(signed-byte ,(max (up2 min)
			       (+ 2 (upgrade max)))))))

(defun storage-type (item)
  (typecase item
    (fixnum
     (upgraded-array-element-type
      (if (plusp item)
	  (num-type 0 item)
	  (num-type item 0))))
    (otherwise t)))

(defun upgrade (x)
  (if (zerop x)
      0
      (floor (log x 2))))
(defun up2 (x)
  (+ 2 (upgrade (1- (- x)))))


(defun test-at-random (&optional (min (- (random most-positive-fixnum)))
			 (max (random most-positive-fixnum)))
  (let ((type (num-type min max)))
    (typetest min type)
    (typetest max type)))

(defun test-again (&optional (s (random 64)))
  (let ((foo (expt 2 (1- s))))
    (test-at-random (- foo) (1+ foo))))


(defun test0 ()
  (print (downgrade-array #(0 1 2 3 4 5)))
  (print (downgrade-array #(0 1 2 3 4 5 34524352345)))
  (print (downgrade-array #(-345345 0 1 2 3 4 5))))

(defun really-downgrade-array (array &rest items)
  (let* ((type (apply 'downgrade-array array items))
	 (new-array
	  (make-array (array-dimensions array)
		      :element-type type)))
    (dotimes (i (array-total-size array))
      (setf (aref new-array i)
	    (aref array i)))
    (values new-array
	    type)))

(defun test1 ()
  (let ((*print-readably* t))
    (print
     (list
      (really-downgrade-array #(1 1 0 0 0 1 0 1 0))
      (really-downgrade-array #(1 1 0 0 0 1 0 -1 0))
      (really-downgrade-array #(1 1 0 0 0 1 0 -345345 0))))))

;;;;;
(reset-empty-chunk-value)

(defun types-of-chunks-that-exist ()
  (remove-duplicates
   (mapcar (lambda (c)
	     (type-of
	      (chunk-data c)))
	   (alexandria:hash-table-values *chunks*))
   :test 'equal))

;;Return the size of the type in bits
(defun typesize (type)
  (cond ((and (listp type)
	      (= 2 (length type))
	      (eq 'unsigned-byte (first type)))
	 (roundup-2 (second type)))
	((and (listp type)
	      (= 2 (length type))
	      (eq 'signed-byte (first type)))
	 (roundup-2 (second type)))
	((eq type 'bit)
	 1)
	(t
	 #+x86-64
	 64
	 #+x86
	 32)))

(defun array-truesize (array)
  (* (typesize (array-element-type array))
     (array-total-size array)))

(defun roundup-2 (n)
  (ash 1 (ceiling (log n 2))))

(defun test5 ()
  (array-truesize (really-downgrade-array #(10 1202 012 012 0 t))))

(defun chunks-total-bits (&optional (divider (* 1.0 (* 8 (expt 1024 2)))))
  (/ (reduce '+
	     (mapcar (lambda (c)
		       (array-truesize
			(chunk-data c)))
		     (alexandria:hash-table-values *chunks*)))
     divider))

(defun save-all ()
  ;;By limiting the cache to 0, everything gets flushed out.
  (prune-cache 0))
