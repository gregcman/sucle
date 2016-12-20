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

;;create a position along a 16 16 16 grid -- as if the block hash was chopped
(let ((num (ash 1 15)))
  (defun chunk-16 (x y z)
    (dpb (+ z num) (byte 16 44)
	 (dpb (+ y num) (byte 16 24)
	      (ash (+ x num) 4)))))


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
  (make-array (* 16 16 16) :element-type '(unsigned-byte 8)))

(defun getachunk ()
  (or (pop freechunkmempool)
      (empty-chunk)))

(defun clearchunk (achunk value)
  (dotimes (x (array-total-size achunk))
    (setf (row-major-aref achunk x) value))
  achunk)

(defmacro get-chunk-block (chunk i j k)
  `(aref ,chunk
	 (dpb ,j (byte 4 8)
	      (dpb ,k (byte 4 4) ,i))))

(defmacro prep-hash (setter-name getter-name thathash defaultval)
  `(progn
     (defun ,getter-name (i j k)
	 (multiple-value-bind (chunk-code xd yd zd) (chop (chunkhashfunc i j k))
	   (let ((chunk (gethash chunk-code ,thathash)))
	     (if chunk
		 (values (get-chunk-block chunk xd yd zd) t)
		 (values ,defaultval nil)))))
     (defun ,setter-name (i j k blockid)
       (multiple-value-bind (chunk-code xd yd zd) (chop (chunkhashfunc i j k))
	 (let ((chunk (or (gethash chunk-code ,thathash)
			  (setf
			   (gethash chunk-code ,thathash)
			   (clearchunk (getachunk) ,defaultval)))))
	   (setf (get-chunk-block chunk xd yd zd) blockid))))
     (defun (setf ,getter-name) (new i j k)
       (,setter-name i j k new))))
