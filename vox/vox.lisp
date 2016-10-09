(in-package :vox)

(declaim (inline plus2^19 minus2^19))
(let ((anum (ash 1 19)))
  (defun plus2^19 (n)
    (declare (type fixnum n))
    (the fixnum (+ n anum)))

  (defun minus2^19 (n)
    (declare (type fixnum n))
    (- n anum)))

(defun chunkhashfunc (x y z)
  (declare (type fixnum x y z))
  (+ (the fixnum (plus2^19 z))
     (the fixnum
	  (ash (+ (the fixnum (plus2^19 y))
		  (the fixnum
		       (ash (the fixnum
				 (plus2^19 x)) 20))) 20))))

(defun unhashfunc (ah)
  (declare (type fixnum ah))
  (let* ((z (logand ah (1- (ash 1 20))))
	 (xy (ash ah -20))
	 (y (logand xy (1- (ash 1 20))))
	 (x (ash xy -20)))
    (values (minus2^19 x) (minus2^19 y) (minus2^19 z))))

(defun unchunkhashfunc (ah)
  (declare (type fixnum ah))
  (multiple-value-list (unhashfunc ah)))

(defun send-to-free-mem (hash)
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (free-chunk v))
   hash)
  (clrhash hash))

(defparameter freechunkmempool nil)

(defun free-chunk (lechunk)
  (push lechunk freechunkmempool))

(defun empty-chunk ()
  "makes an empty chunk"
  (make-array (* 16 16 16) :element-type '(unsigned-byte 8)))

(defun getachunk ()
  (let ((somechunk (pop freechunkmempool)))
    (if somechunk
	somechunk
	(empty-chunk))))

(defun clearchunk (achunk)
  (nsubstitute-if-not 0 #'zerop achunk)
  achunk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty-chunk-at (x y z thehash)
  (let ((oldchunk (getchunkat thehash x y z)))
    (if oldchunk
	(clearchunk oldchunk)
	(setchunkat x y z (clearchunk (getachunk)) thehash))))

(defun destroy-chunk-at (x y z thehash)
  (let ((oldchunk (getchunkat thehash x y z)))
    (if oldchunk
	(progn
	  (remhash (chunkhashfunc x y z) thehash)
	  (free-chunk oldchunk)))))

(defun getchunkat (hash x y z)
  (gethash (chunkhashfunc x y z) hash))

(defun setchunkat (x y z newchunk thehash)
  (setf
   (gethash (chunkhashfunc x y z) thehash)
   newchunk)
  newchunk)

(defun chunkexistsat (x y z hash)
  (getchunkat hash x y z))

(defun get-chunk-block (chunk i j k)
  (aref chunk (+  i (* 16 (+ (* 16 j) k)))))

(defun set-chunk-block (chunk i j k new)
  (setf
   (aref chunk (+  i (* 16 (+ (* 16 j) k))))
   new))

;;the get and set functions get their own definitions because they need to be fast.

(defun func-get (thathash defaultval)
  (declare (type hash-table thathash))
  (declare (type fixnum defaultval))
  (lambda (i j k)
    (multiple-value-bind (x xd) (floor i 16)
      (Multiple-value-bind (y yd) (floor j 16)
	(multiple-value-bind (z zd) (floor k 16)
	  (let ((chunk (getchunkat thathash x y z)))
	    (if chunk
		(get-chunk-block chunk xd yd zd)
		defaultval)))))))

(defun func-set (thathash)
  (declare (type hash-table thathash))
  (lambda (i j k blockid)
    (multiple-value-bind (x xd) (floor i 16)
      (multiple-value-bind (y yd) (floor j 16)
	(multiple-value-bind (z zd) (floor k 16)
	  (let* ((chunk (getchunkat thathash x y z)))
	    (if (not chunk)
		(setf chunk (empty-chunk-at x y z thathash)))
	    (let ((old (get-chunk-block chunk xd yd zd)))
	      (if (/= old blockid)
		  (set-chunk-block chunk xd yd zd blockid)
		  nil))))))))

;;create a hashmap which holds arrays
;;type is the type of the array
;;the array is flat, but it is an illusion that it has
;;the dimensions of 16 x 16 x 16

(defun nope (an-object a-property)
  (remhash a-property an-object))

(defun what (an-object a-property)
  (gethash a-property an-object))

(defun (setf what) (new an-object a-property)
  (setf (gethash a-property an-object) new))

(defun shit ()
  (make-hash-table :test (function eq)))

(defun spill (shit)
  (let ((props nil))
    (maphash
     (lambda (k v) (push (cons k v) props))
     shit)
    props))

(defmacro dorange ((var start length) &rest body)
  (let ((temp (gensym))
	(temp2 (gensym))
	(tempstart (gensym))
	(templength (gensym)))
    `(block nil
       (let* ((,templength ,length)
	      (,tempstart ,start)
	      (,var ,tempstart))
	 (declare (type signed-byte ,var))
	 (tagbody
	    (go ,temp2)
	    ,temp
	    (tagbody ,@body)
	    (psetq ,var (1+ ,var))
	    ,temp2
	    (unless (>= ,var (+ ,tempstart ,templength)) (go ,temp))
	    (return-from nil (progn nil)))))))
