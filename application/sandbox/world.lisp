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
   #:setmeta #:getmeta

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
 (flet ((define-accessors (getter-name setter-name %getter-name %setter-name)
	  `(progn
	     (defun ,getter-name (i j k)
	       (,%getter-name (chunkhashfunc i k j)))
	     (defun ,setter-name (i j k new)
	       (,%setter-name (chunkhashfunc i k j) new))
	     (defun (setf ,getter-name) (new i j k)
	       (,setter-name i j k new)))))
   (let ((value (logior (ash 15 12))))
     ((lambda (setter getter %setter %getter hash default creator)
	(let ((uniform-spec (coge:gen-spec)))
	  
	  (vox::field uniform-spec `(simple-array t (4096)) hash default creator)
	  (vox::access uniform-spec %getter %setter)
	  (let ((bits (logcount most-positive-fixnum)))
	    (let ((y 10)
		  (x nil; 26
		    )
		  (z nil;26
		    ))
	      (decf bits y)
	      (setf x (floor bits 2))
	      (setf z (ceiling bits 2))
	      (vox::layout uniform-spec (1- x) 0 (1- z) x (1- y) (+ x z))))
	  (vox::truncation uniform-spec 4 4 4)
	  (vox::derived-parts uniform-spec)
	  (vox::offset uniform-spec 0 0 0)
	  (vox::names uniform-spec
		      'unhashfunc 'chunkhashfunc
		      'chop 'anti-chop 'rem-flow '%%ref 'add)
	  (list 'progn
		(vox::define-fixnum-ops
		    uniform-spec)
		(list 'progn
		      (vox::prep-hash uniform-spec)
		      (define-accessors getter setter %getter %setter)
		      `(defsetf %getter %setter)))))
      'setobj 'getobj
      '%setobj '%getobj
      '*lispobj* value
      `(recycler:get-from *freechunkmempoolobj* ,value)))))

(defgeneric lispobj-dispatch (obj))

(defun value-dispatch (value)
  (typecase value
    (fixnum value)
    (otherwise (lispobj-dispatch value))))

(defsetf num-getobj setobj)
(defun num-getobj (i j k)
  (let ((value (getobj i j k)))
    (value-dispatch value)))
(defsetf %num-getobj %setobj)
(defun %num-getobj (place)
  (let ((value (%getobj place)))
    (value-dispatch value)))

(defmacro suite (bits position set get %set %get)
  (let* ((bytespec `(byte ,bits ,position))
	 (access `(ldb ,bytespec
		       (num-getobj i j k)))
	 (%access `(ldb ,bytespec
			(%num-getobj place))))
    `(progn
       (defun ,set (i j k new) (setf ,access new))
       (defun ,get (i j k) ,access)
       (defsetf ,get ,set)
       (defun ,%set (place new) (setf ,%access new))
       (defun ,%get (place) ,%access)
       (defsetf ,%get ,%set))))
(progn
  (suite 8 0 setblock getblock %setblock %getblock)
  (suite 4 8 setlight getlight %setlight %getlight)
  (suite 4 12 skysetlight skygetlight %skysetlight %skygetlight)
  (suite 4 16 setmeta getmeta %setmeta %getmeta))

(defun blockify (blockid light sky meta)
  (dpb meta (byte 16 4)
	(dpb sky (byte 4 12)
	     (dpb light (byte 4 8) blockid))))

(defmethod lispobj-dispatch ((obj character))
  (blockify (char-code obj) 0 0 0))

(defmethod lispobj-dispatch ((obj t))
  (blockify (logcount (sxhash obj)) 0 0 0))

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
		 (dirty-push (world:chop (world:chunkhashfunc x z y))))
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

(defun block-dirtify-hashed (xzy)
  (multiple-value-bind (x z y) (world:unhashfunc xzy)
    (block-dirtify x y z)))
