(in-package :vox)

;;instead of using a struct or a vector to represent a point,
;;a single fixnum can be used.
;;a fixnum is 62 bits on a 64 bit machine [lets not worry about the 32 bit]
;;a minecraft world is 256 blocks high and 60 mil block wide and long
;;1 bit to pad, 9 bits z, 1 bit pad, 25 bits y, 1 bit pad, 25 bit x
;;== 512 high, 33554432 long, 33554432 wide. not exactly minecraft specs,
;;but close enough
;;chunks are aligned to 16 16 16 which means that by chopping off
;;the last 4 bits of each constituent number in the fixnum
;;we get the code for the chunk! and this is done in one mask!
;;the padding is added so we can use another number as a type of
;;vector to add to the fixnum position. If after addition the number
;;overflows, we chop off the most significant bit with a single mask
;;because the y axis is the most significant, the codes can be sorted
;;in order of height!
;;bit 1 - 25 is x = (byte 25 0)
;;bit 26 is overflow padding for x
;;bit 27 - 51 is y (byte 25 26)
;;bit 52 is overflow padding for z
;;bit 53 - 61 is z (byte 9 52)
;;bit 62 is overflow padding for y
;;20 bits go to x, 20 bits go to y, 20 bits go to z, 2 bits are left over = 62 bits
;;(defparameter spec `((20 ,(ash 1 19)) (20 ,(ash 1 19)) (20 ,(ash 1 19))))
;;(eval (CREATE-PACKED-NUM 'chunkhashfunc 'unhashfunc spec))

(defmacro define-fixnum-ops ((num0-start num0-size chopx)
			     (num1-start num1-size chopy) 
			     (num2-start num2-size chopz))
  `(symbol-macrolet ((num0 (byte ,num0-size ,num0-start))
		     (num1 (byte ,num1-size ,num1-start))
		     (num2 (byte ,num2-size ,num2-start)))
     (defun unhashfunc (ah)
       (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0))
		(TYPE FIXNUM AH))
       (values (- (ldb num0 ah) (ash 1 (1- ,num0-size)))
	       (- (LDB num2 AH) (ash 1 (1- ,num2-size)))
	       (- (ldb num1 ah) (ash 1 (1- ,num1-size)))))

     (defun chunkhashfunc (x y z)
       (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0))
		(TYPE FIXNUM x y z))
       (THE FIXNUM
	    (DPB (+ y (ash 1 (1- ,num2-size))) num2
		 (DPB (+ z (ash 1 (1- ,num1-size))) num1
		      (ldb num0 (+ x (ash 1 (1- ,num0-size))))))))

     (DEFUN %UNHASHFUNC (AH)
       (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0))
		(TYPE FIXNUM AH))
       (VALUES (LDB num0 AH)
	       (LDB num2 AH)
	       (LDB num1 AH)))

     (DEFUN %CHUNKHASHFUNC (x y z)
       (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0))
		(TYPE FIXNUM x y z))
       (THE FIXNUM
	    (DPB y num2
		 (DPB z num1
		      (ldb num0 x)))))

     ;;chop the last 4 bits off of each number to obtain the chunk code
     (defmacro truncate-mask ()
       (lognot (+ (ash (1- (ash 1 ,chopy)) ,num0-start)
		  (ash (1- (ash 1 ,chopx)) ,num1-start)
		  (ash (1- (ash 1 ,chopz)) ,num2-start))))
     (defun chop (pos)
       (declare (optimize (speed 3) (safety 0))
		(type fixnum pos))
       (logand (truncate-mask) pos))

     (defmacro overflow-mask ()
       (lognot (logior (ash 1 (+ ,num0-start ,num0-size))
		       (ash 1 (+ ,num1-start ,num1-size))
		       (ash 1 (+ ,num2-start ,num2-size)))))
     (defun add (pos delta)
       (declare (optimize (speed 3) (safety 0))
		(type fixnum pos delta))
       (logand (the fixnum (+ pos delta)) (overflow-mask)))

     ;;bind-vars with the offset
     (defmacro with-offset (pos (xv yv zv) &body body)
       `(let ((,xv (ldb (byte ,,chopx ,,num0-start) ,pos))
	      (,yv (ldb (byte ,,chopy ,,num2-start) ,pos))
	      (,zv (ldb (byte ,,chopz ,,num1-start) ,pos)))
	  ,@body))))

;;print the twos complement binary representation of a number, padded to n 
(defun print-bits (n size)
  (let ((string (concatenate 'string "~" (write-to-string size) ",'0B")))
    (format t string (ldb (byte size 0) n))))

;;next up is memory pooling
;;the pool is a data structure which holds things that can be reused
;;such as (simple-array (unsigned-byte 8) (4096))
;;and (simple-array (unsigned-byte 4) (4096))
;;if the pool is empty, it will make another object to give
;;1. need an array, ask pool for space
;;2. (if (pool empty) (pool makes another) ())
;;3. pool gives u array
(defstruct provider
  create-func
  cleanup-func
  (pool-size 0)
  (pool nil)
  size-cap)
(defun get-from (provider &rest specs)
  (if (provider-pool provider)
      (progn
	(decf (provider-pool-size provider))
	(let ((new-item (pop (provider-pool provider))))
	  (apply (provider-cleanup-func provider) new-item specs)))
      (apply (provider-create-func provider) specs)))
(defun give-to (provider item &rest specs)
  (apply (provider-cleanup-func provider) item specs)
  (when (< (provider-pool-size provider) (provider-size-cap provider))
    (push item (provider-pool provider))
    (incf (provider-pool-size provider))))

;;code for clearing a chunk, referencing data inside a chunk,
;;creating a new chunk
(defun clearchunk (achunk &optional (value 0))
  (dotimes (x (array-total-size achunk))
    (setf (row-major-aref achunk x) value))
  achunk)

(defun %ref (i j k)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum i j k))
  (the (unsigned-byte 12)
       (dpb j (byte 4 8)
	       (dpb k (byte 4 4) i))))

(defmacro get-chunk-block (chunk i j k)
  `(aref ,chunk (%ref ,i ,j ,k)))

(defmacro %new-chunk (type defaultval chopx chopy chopz)
  `(make-array (ash 1 (+ ,chopx ,chopy ,chopz))
	       :element-type ',type
	       :initial-element ,defaultval))

;;the type of hash table that lets fixnums be equated [default is eql]
;;combine the number of bits per individual item, the get/set names,
;;the hash which hashes the fixnums, the default value, and the method
;;to create new chunks
(defun genhash ()
  (make-hash-table :test 'eq))

(defmacro prep-hash (bits setter-name getter-name thathash defaultval creator)
  `(progn
     (defun ,getter-name (i j k)
       (declare (optimize (speed 3) (safety 0))
		(type fixnum i j k))
       (let ((block-code (chunkhashfunc i j k)))
	 (let ((chunk-code (chop block-code)))
	   (let ((chunk (gethash chunk-code ,thathash)))
	     (declare (type (or (simple-array (unsigned-byte ,bits) (4096))
				null) chunk))
	     (if chunk
		 (with-offset block-code (xd yd zd)
		   (values (get-chunk-block chunk xd yd zd) t))
		 (values ,defaultval nil))))))
     (defun ,setter-name (i j k blockid)
       (declare (optimize (speed 3) (safety 0))
		(type fixnum i j k))
       (let ((block-code (chunkhashfunc i j k)))
	 (let ((chunk-code (chop block-code)))
	   (let ((chunk (or (gethash chunk-code ,thathash)
			    (setf
			     (gethash chunk-code ,thathash)
			     ,creator))))
	     (declare (type (simple-array (unsigned-byte ,bits) (4096)) chunk))
	     (with-offset block-code (xd yd zd) 
	       (setf (get-chunk-block chunk xd yd zd) blockid))))))
     (defun (setf ,getter-name) (new i j k)
       (,setter-name i j k new))))

