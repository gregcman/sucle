(defpackage #:recycler
  (:use #:cl)
  (:export
   #:get-from
   #:give-to
   #:make-recycler))

(in-package :recycler)

;;next up is memory pooling
;;the pool is a data structure which holds things that can be reused
;;such as (simple-array (unsigned-byte 8) (4096))
;;and (simple-array (unsigned-byte 4) (4096))
;;if the pool is empty, it will make another object to give
;;1. need an array, ask pool for space
;;2. (if (pool empty) (pool makes another) ())
;;3. pool gives u array
(defstruct recycler
  create-func
  cleanup-func
  (pool-size 0)
  (pool nil)
  size-cap)
(defun get-from (recycler &rest specs)
  (if (recycler-pool recycler)
      (progn
	(decf (recycler-pool-size recycler))
	(let ((new-item (pop (recycler-pool recycler))))
	  (apply (recycler-cleanup-func recycler) new-item specs)))
      (apply (recycler-create-func recycler) specs)))
(defun give-to (recycler item &rest specs)
  (apply (recycler-cleanup-func recycler) item specs)
  (when (< (recycler-pool-size recycler) (recycler-size-cap recycler))
    (push item (recycler-pool recycler))
    (incf (recycler-pool-size recycler))))


(defpackage #:world
  (:use :cl)	
  (:export 

;;;block accessors
   #:getblock #:setblock
   #:%getblock #:%setblock
   #:getlight #:setlight
   #:%getlight #:%setlight
   #:skygetlight #:skysetlight
   #:%skygetlight #:%skysetlight

;;;pointer manipulation
   #:%%ref 
   #:chop
   #:rem-flow
   #:anti-chop
   #:add
   #:unhashfunc
   #:chunkhashfunc

;;;chunks
   #:clearchunk
   #:%new-chunk

;;;the apocalypse
   #:clearworld
   #:send-to-free-mem
   ))

(in-package #:world)

(defun send-to-free-mem (hash pool)
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (recycler:give-to pool v))
   hash)
  (clrhash hash))

;;clearing a chunk, referencing data inside a chunk,
;;creating a new chunk
(defun clearchunk (achunk &optional (value 0))
  (dotimes (x (array-total-size achunk))
    (setf (row-major-aref achunk x) value))
  achunk)

(defmacro %new-chunk (type defaultval chopx chopy chopz)
  `(make-array (ash 1 (+ ,chopx ,chopy ,chopz))
	       :element-type ',type
	       :initial-element ,defaultval))

(defparameter *freechunkmempoolobj*
  (recycler:make-recycler
   :create-func #'(lambda (&optional (x 0)) (%new-chunk t x 4 4 4))
   :cleanup-func #'clearchunk
   :size-cap 32))
(defparameter *lispobj* (make-hash-table :test 'eq))
(defun clearworld ()
  (send-to-free-mem *lispobj* *freechunkmempoolobj*))

(utility:etouq
 (flet ((define-accessors (getter-name 
				       %getter-name %setter-name)
	  `(progn	     
	     (defun ,getter-name (i j k)
	       (,%getter-name (chunkhashfunc i j k)))
	     (defun (setf ,getter-name) (new i j k)
	       (,%setter-name (chunkhashfunc i j k) new)))))
   (let ((value (logior (ash 15 12))))	  
     (vox::field `(simple-array t (4096))
		 '*lispobj*
		 value
		 `(recycler:get-from *freechunkmempoolobj* ,value)))
   (let* ((bits (logcount most-positive-fixnum))
	  (y 10)
	  (remaining-bits (- bits y))
	  (x (floor remaining-bits 2))
	  (z (ceiling remaining-bits 2)))
     (vox::layout (1- x) 0 (1- z) x (1- y) (+ x z)))
   (vox::truncation 4 4 4)
   (vox::derived-parts)
   (vox::offset 0 0 0)
   (vox::names
    'unhashfunc 'chunkhashfunc
    'chop 'anti-chop 'rem-flow '%%ref 'add)
   `(progn
      ,(vox::define-fixnum-ops)
      (progn
	,(vox::prep-hash
	  '%getobj '%setobj)
	,(define-accessors
	  'getobj 
	  '%getobj '%setobj)))))

(defgeneric lispobj-dispatch (obj))

(defun value-dispatch (value)
  (typecase value
    (fixnum value)
    (otherwise (lispobj-dispatch value))))

(defun (setf num-getobj) (new i j k)
  (setf (getobj i j k) new) )
(defun num-getobj (i j k)
  (let ((value (getobj i j k)))
    (value-dispatch value)))

(defmacro suite (bits position get %get)
  (let* ((bytespec `(byte ,bits ,position))
	 (access `(ldb ,bytespec
		       (num-getobj i j k))))
    `(progn
       (defun (setf ,get) (new i j k)
	 (setf ,access new))
       (defun ,get (i j k)
	 ,access)
       (defun (setf ,%get) (new place)
	 (multiple-value-bind (i j k) (unhashfunc place)
	   (setf ,access new)))
       (defun ,%get (place)
	 (multiple-value-bind (i j k) (unhashfunc place)
	   ,access)))))
(progn
  (suite 8 0 getblock %getblock)
  (suite 4 8 getlight %getlight)
  (suite 4 12 skygetlight %skygetlight))

(defun blockify (blockid light sky)
  (dpb sky (byte 4 12)
       (dpb light (byte 4 8) blockid)))

(defmethod lispobj-dispatch ((obj character))
  (blockify (char-code obj) 0 0))

(defmethod lispobj-dispatch ((obj t))
  (blockify (logcount (sxhash obj)) 0 0))

(defmethod lispobj-dispatch ((obj symbol))
  56)

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
		 (dirty-push (world:chop (world:chunkhashfunc x y z))))
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

(defun block-dirtify-hashed (xyz)
  (multiple-value-bind (x y z) (world:unhashfunc xyz)
    (block-dirtify x y z)))
