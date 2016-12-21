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

(defun print-bits (n size)
  (let ((string (concatenate 'string "~" (write-to-string size) ",'0B")))
    (format t string (ldb (byte size 0) n))))

;;bit 1 - 25 is x = (byte 25 0)
;;bit 26 is overflow padding for x
;;bit 27 - 51 is y (byte 25 26)
;;bit 52 is overflow padding for z
;;bit 53 - 61 is z (byte 9 52)
;;bit 62 is overflow padding for y
;;;;generic stuff is abbove
;;spec is the way numbers are fit into an integer. In this can it is a fixnum on 64 bit machines.
;;20 bits go to x, 20 bits go to y, 20 bits go to z, 2 bits are left over = 62 bits
;;(defparameter spec `((20 ,(ash 1 19)) (20 ,(ash 1 19)) (20 ,(ash 1 19))))
;;(eval (CREATE-PACKED-NUM 'chunkhashfunc 'unhashfunc spec))

(symbol-macrolet ((num0-start 0)
		  (num0-size 25)
		  (num1-start 26)
		  (num1-size 25)
		  (num2-start 52)
		  (num2-size 9))
  (symbol-macrolet ((num0 (byte num0-size num0-start))
		    (num1 (byte num1-size num1-start))
		    (num2 (byte num2-size num2-start)))
    (defun unhashfunc (ah)'
      (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0))
	       (TYPE FIXNUM AH))
      (values (- (ldb num0 ah) (ash 1 (1- num0-size)))
	      (- (LDB num2 AH) (ash 1 (1- num2-size)))
	      (- (ldb num1 ah) (ash 1 (1- num1-size)))))

    (defun chunkhashfunc (x y z)
      (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0))
	       (TYPE FIXNUM x y z))
      (THE FIXNUM
	   (DPB (+ y (ash 1 (1- num2-size))) num2
		(DPB (+ z (ash 1 (1- num1-size))) num1
		     (ldb num0 (+ x (ash 1 (1- num0-size))))))))

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
      (lognot (+ (ash 15 num0-start)
		 (ash 15 num1-start)
		 (ash 15 num2-start))))
    (defun chop (pos)
      (declare (optimize (speed 3) (safety 0))
	       (type fixnum pos))
      (logand (truncate-mask) pos))

    (defmacro overflow-mask ()
      (lognot (logior (ash 1 (+ num0-start num0-size))
		      (ash 1 (+ num1-start num1-size))
		      (ash 1 (+ num2-start num2-size)))))
    (defun add (pos delta)
      (declare (optimize (speed 3) (safety 0))
	       (type fixnum pos delta))
      (logand (the fixnum (+ pos delta)) (overflow-mask)))

    ;;bind-vars with the offset
    (defmacro with-offset (pos (xv yv zv) &body body)
      `(let ((,xv (ldb (byte 4 num0-start) ,pos))
	     (,yv (ldb (byte 4 num2-start) ,pos))
	     (,zv (ldb (byte 4 num1-start) ,pos)))
	 ,@body))))


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

(defun %ref (i j k)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum i j k))
  (the (unsigned-byte 12)
       (dpb j (byte 4 8)
	       (dpb k (byte 4 4) i))))

(defmacro get-chunk-block (chunk i j k)
  `(aref ,chunk (%ref ,i ,j ,k)))

(defmacro prep-hash (setter-name getter-name thathash defaultval)
  `(progn
     (defun ,getter-name (i j k)
       (declare (optimize (speed 3) (safety 0))
		(type fixnum i j k))
       (let ((block-code (chunkhashfunc i j k)))
	 (let ((chunk-code (chop block-code)))
	   (let ((chunk (gethash chunk-code ,thathash)))
	     (declare (type (or (simple-array (unsigned-byte 8) (4096))
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
			     (clearchunk (getachunk) ,defaultval)))))
	     (declare (type (simple-array (unsigned-byte 8) (4096)) chunk))
	     (with-offset block-code (xd yd zd) 
	       (setf (get-chunk-block chunk xd yd zd) blockid))))))
     (defun (setf ,getter-name) (new i j k)
       (,setter-name i j k new))))

