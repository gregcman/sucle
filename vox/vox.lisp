(in-package :vox)
;;inline the headless lambda, give it a name, give it speed and recklessness
(defun make-fast (nombre headless-lambda)
  `(progn
     (declaim (inline ,nombre))
     (defun ,nombre ,(car headless-lambda)
       (declare (optimize (speed 3) (safety 0)))
       ,@(cdr headless-lambda))))

(defun fixnums! (headless-lambda)
  `(,(car headless-lambda)
     (declare (type fixnum ,@(coge:get-actual-args (car headless-lambda))))
     ,@(cdr headless-lambda)))

(defun fastnum (name headless-lambda)
  (make-fast name (fixnums! headless-lambda)))

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

(defun gen-remove-overflow ()
  `((pos) (logand pos p!overflow-mask)))

(defun gen-chopper ()
  `((pos) (logand p!truncate-mask pos)))

(defun gen-anti-chopper ()
  `((pos) (logand p!anti-truncate-mask pos)))

(defun gen-packer ()
  `((x y z)
    (THE FIXNUM
	 (logior (the fixnum (ash (mod z (ash p!offset2 1)) p!num2-start))
		 (the fixnum (ash (mod y (ash p!offset1 1)) p!num1-start))
		 (the fixnum (ash (mod x (ash p!offset0 1)) p!num0-start))))))

(defmacro signed-unsiged (x n)
  (let ((n (eval n)))
    `(- (mod (the fixnum (+ ,x ,(/ n 2))) ,n) ,(/ n 2))))
(defun gen-unpacker(spec)
  `((fixnum)
    (values (signed-unsiged
	     ,(if (zerop (coge:rp spec 'p!num0-start))
		  `(logand fixnum (1- (ash 1 p!num0-size)))  
		  `(ldb (byte p!num0-size p!num0-start) fixnum)) p!offset0)
	    (signed-unsiged
	     (LDB (byte p!num1-size p!num1-start) fixnum) p!offset1)
	    (signed-unsiged
	     (ash fixnum (- p!num2-start)) p!offset2))))

(defun gen-mask-overflow ()
  `(lognot (logior (ash 1 (+ p!num0-start p!num0-size))
		   (ash 1 (+ p!num1-start p!num1-size))
		   (ash 1 (+ p!num2-start p!num2-size)))))

(defun gen-mask-truncate ()
  `(lognot (+ (ash (1- (ash 1 p!chopy)) p!num0-start)
	      (ash (1- (ash 1 p!chopx)) p!num1-start)
	      (ash (1- (ash 1 p!chopz)) p!num2-start))))

(defun gen-mask-anti-truncate ()
  `(+ (ash (1- (ash 1 p!chopy)) p!num0-start)
      (ash (1- (ash 1 p!chopx)) p!num1-start)
      (ash (1- (ash 1 p!chopz)) p!num2-start)))

(defun gen-with-offset ()
  `(defmacro with-offset (pos (xv yv zv) &body body)
     `(let ((,xv (ldb (byte p!chopx p!num0-start) ,pos))
	    (,yv (ldb (byte p!chopy p!num1-start) ,pos))
	    (,zv (ldb (byte p!chopz p!num2-start) ,pos)))
	,@body)))

(defun derived-parts (spec)
  (coge:add-spec
   spec
   `((offset0 . ,(eval (coge:rp spec '(ash 1 (1- p!num0-size)))))
     (offset1 . ,(eval (coge:rp spec '(ash 1 (1- p!num1-size)))))
     (offset2 . ,(eval (coge:rp spec '(ash 1 (1- p!num2-size)))))))
  (coge:add-spec
   spec
   `((overflow-mask . ,(coge:rp spec (gen-mask-overflow)))
     (truncate-mask . ,(coge:rp spec (gen-mask-truncate)))
     (anti-truncate-mask . ,(coge:rp spec (gen-mask-anti-truncate))))))


;;there is a pattern here and it is ugly

(coge:defspec fixnum-op-spec (num0-start num0-size chopx
			      num1-start num1-size chopy 
			      num2-start num2-size chopz))

(defun define-fixnum-ops (spec)
  `(progn
     ,(coge:rp spec (fastnum 'unhashfunc (gen-unpacker spec)))
     ,(coge:rp spec (fastnum 'chunkhashfunc (gen-packer)))
     ,(coge:rp spec (fastnum 'chop (gen-chopper)))
     ,(coge:rp spec (fastnum 'anti-chop (gen-anti-chopper)))
     ,(coge:rp spec (fastnum 'rem-flow (gen-remove-overflow)))

     (defun add (a b)
       (declare (optimize (speed 3) (safety 0))
		(type fixnum a b))
       (rem-flow (+ a b)))
     ;;bind-vars with the offset
     ,(coge:rp spec (gen-with-offset))))

(defun %ref (i j k)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum i j k))
  (the (unsigned-byte 12)
       (logior (the fixnum (ash (mod k 16) 8))
	       (the fixnum (ash (mod j 16) 4))
	       (the fixnum (mod i 16)))))

(declaim (inline %%ref))
(defun %%ref (code)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum code))
  (let ((c (anti-chop code)))
    (the (unsigned-byte 12)
	 (ash (logior (the fixnum (ash c 44))
		      (the fixnum (ash c 22))
		      c) -44))))

(defun codes (num0-start num0-size chopx
	      num1-start num1-size chopy 
	      num2-start num2-size chopz)
  (define-fixnum-ops
      (derived-parts
       (fixnum-op-spec (coge:gen-spec)
		       num0-start num0-size chopx
		       num1-start num1-size chopy 
		       num2-start num2-size chopz))))

;;print the twos complement binary representation of a number, padded to n 
(defun print-bits (n size)
  (let ((string (concatenate 'string "~" (write-to-string size) ",'0B")))
    (let ((num (ldb (byte size 0) n)))      
      (format t string num)
      num)))

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
       (let ((block-code (chunkhashfunc i k j)))
	 (let ((chunk-code (chop block-code)))
	   (let ((chunk (gethash chunk-code ,thathash)))
	     (declare (type (or (simple-array (unsigned-byte ,bits) (4096))
				null) chunk))
	     (if chunk
		 (values (aref chunk (%%ref block-code)) t)
		 (values ,defaultval nil))))))
     (defun ,setter-name (i j k blockid)
       (declare (optimize (speed 3) (safety 0))
		(type fixnum i j k))
       (let ((block-code (chunkhashfunc i k j)))
	 (let ((chunk-code (chop block-code)))
	   (let ((chunk (or (gethash chunk-code ,thathash)
			    (setf
			     (gethash chunk-code ,thathash)
			     ,creator))))
	     (declare (type (simple-array (unsigned-byte ,bits) (4096)) chunk))
	     (setf (aref chunk (%%ref block-code)) blockid)))))
     (defun (setf ,getter-name) (new i j k)
       (,setter-name i j k new))))
