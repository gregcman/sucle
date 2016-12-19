(in-package :vox)

;;what in the world was I thinking when I wrote this?

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

(defun clearchunk (achunk value)
  (nsubstitute-if value (lambda (x) t) achunk)
  achunk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty-chunk-at (x y z thehash defaultval)
  (let ((oldchunk (getchunkat thehash x y z)))
    (if oldchunk
	(clearchunk oldchunk defaultval)
	(setchunkat x y z (clearchunk (getachunk) defaultval) thehash))))

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

;;premature optimization is the root of all evil

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

(defun func-set (thathash defaultval)
  (declare (type hash-table thathash))
  (lambda (i j k blockid)
    (multiple-value-bind (x xd) (floor i 16)
      (multiple-value-bind (y yd) (floor j 16)
	(multiple-value-bind (z zd) (floor k 16)
	  (let* ((chunk (getchunkat thathash x y z)))
	    (if (not chunk)
		(setf chunk (empty-chunk-at x y z thathash defaultval)))
	    (let ((old (get-chunk-block chunk xd yd zd)))
	      (if (/= old blockid)
		  (set-chunk-block chunk xd yd zd blockid)
		  nil))))))))
