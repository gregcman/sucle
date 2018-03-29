(defpackage #:pix
  (:use #:cl #:utility)
  (:export))

(in-package :pix)

(declaim (inline plus2^30 minus2^30))
(etouq
 (let ((anum (ash 1 30)))
   `(progn
      (defun plus2^30 (n)
	(declare (type fixnum n))
	(the fixnum (+ n ,anum)))

      (defun minus2^30 (n)
	(declare (type fixnum n))
	(- n ,anum)))))

(defun chunkhashfunc (x y)
  (declare (type fixnum x y))
  (+ (the fixnum (plus2^30 y))
     (the fixnum
	  (ash (the fixnum
		    (plus2^30 x)) 31))))

(defun unhashfunc (ah)
  (declare (type fixnum ah))
  (let* ((y (logand ah (1- (ash 1 31))))
	 (x (ash ah -31)))
    (values (minus2^30 x) (minus2^30 y))))

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
  (make-array (* 16 16) :element-type '(unsigned-byte 8)))

(defun getachunk ()
  (let ((somechunk (pop freechunkmempool)))
    (if somechunk
	somechunk
	(empty-chunk))))

(defun clearchunk (achunk value)
  (fill achunk value)
  achunk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty-chunk-at (x y thehash defaultval)
  (let ((oldchunk (getchunkat thehash x y)))
    (if oldchunk
	(clearchunk oldchunk defaultval)
	(setchunkat x y (clearchunk (getachunk) defaultval) thehash))))

(defun destroy-chunk-at (x y thehash)
  (let ((oldchunk (getchunkat thehash x y)))
    (if oldchunk
	(progn
	  (remhash (chunkhashfunc x y) thehash)
	  (free-chunk oldchunk)))))

(defun getchunkat (hash x y)
  (gethash (chunkhashfunc x y) hash))

(defun setchunkat (x y newchunk thehash)
  (setf
   (gethash (chunkhashfunc x y) thehash)
   newchunk)
  newchunk)

(defun chunkexistsat (x y hash)
  (getchunkat hash x y))

(defun get-chunk-block (chunk i j)
  (aref chunk (+  i (* 16 j))))

(defun set-chunk-block (chunk i j new)
  (setf
   (aref chunk (+  i (* 16 j)))
   new))

;;the get and set functions get their own definitions because they need to be fast.

(defun func-get (thathash defaultval)
  (declare (type hash-table thathash))
  (declare (type fixnum defaultval))
  (lambda (i j)
    (multiple-value-bind (x xd) (floor i 16)
      (Multiple-value-bind (y yd) (floor j 16)
	(let ((chunk (getchunkat thathash x y)))
	  (if chunk
	      (get-chunk-block chunk xd yd)
	      defaultval))))))

(defun func-set (thathash defaultval)
  (declare (type hash-table thathash))
  (lambda (i j blockid)
    (multiple-value-bind (x xd) (floor i 16)
      (multiple-value-bind (y yd) (floor j 16)
	(let* ((chunk (getchunkat thathash x y)))
	  (if (not chunk)
	      (setf chunk (empty-chunk-at x y thathash defaultval)))
	  (let ((old (get-chunk-block chunk xd yd)))
	    (if (/= old blockid)
		(set-chunk-block chunk xd yd blockid)
		nil)))))))

;;create a hashmap which holds arrays
;;type is the type of the array
;;the array is flat, but it is an illusion that it has
;;the dimensions of 16 x 16
