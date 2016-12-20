(in-package :vox)

;;define two functions which combine numbers into a bigger number.
;;instead of hashing structs, use this to hash just one number made of other numbers
(defun create-packed-num (hash-name unhash-name size-lists)
  (let ((place 0)
	(yes-fnc nil)
	(un-fnc (list 'values))
	(gensym-list nil))
    (dolist (pair size-lists)
      (let ((bit-size (car pair))
	    (offset (second pair)))
	(let ((sym (gensym)))
	  (push sym gensym-list)
	  (if (zerop place)
	      (if (zerop offset)
		  (push sym yes-fnc)
		  (setf yes-fnc `(+ ,sym ,offset)))
	      (if (zerop offset)
		  (setf yes-fnc `(dpb ,sym (byte ,bit-size ,place) ,yes-fnc))
		  (setf yes-fnc `(dpb (+ ,sym ,offset) (byte ,bit-size ,place) ,yes-fnc)))))
	
	(if (zerop offset)
	    (push `(ldb (byte ,bit-size ,place) ah) un-fnc)
	    (push `(- (ldb (byte ,bit-size ,place) ah) ,offset) un-fnc))	
	(incf place bit-size)))
    (eval
     `(progn
	(defun ,hash-name ,(nreverse gensym-list) ,yes-fnc)
	(defun ,unhash-name (ah) ,(nreverse un-fnc))))))

;;chunks are aligned to 16 16 16 which means that by chopping off
;;the last 4 bits of each constituent number we get the code for the chunk!
;;and this is done in one mask!
(defun bit-chopper (spec chops)
  (let ((place 0)
	(mask 0))
    (dolist (pair spec)
      (let ((bits (car pair))
	    (chop (car chops)))
	(setf chops (cdr chops))
        (setf mask (dpb (1- (ash 1 chop)) (byte chop place) mask))
	(incf place bits)))
    mask))

(defun print-bits (n size)
  (let ((string (concatenate 'string "~" (write-to-string size) ",'0B")))
    (format t string (ldb (byte size 0) n))))

;;spec is the way numbers are fit into an integer. In this can it is a fixnum on 64 bit machines.
;;20 bits go to x, 20 bits go to y, 20 bits go to z, 2 bits are left over = 62 bits
(defparameter spec `((20 ,(ash 1 19)) (20 ,(ash 1 19)) (20 ,(ash 1 19))))
(CREATE-PACKED-NUM 'chunkhashfunc 'unhashfunc spec)

;;chop returns the values: the code of the containing chunk, and the
;;offsets which refer to the specific block
(let ((mask (bit-chopper spec (list 4 4 4))))
  (defun chop (pos)
    (values (logandc1 mask pos)
	    (ldb (byte 4 0) pos)
	    (ldb (byte 4 20) pos)
	    (ldb (byte 4 40) pos))))

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
