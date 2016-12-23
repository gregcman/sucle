;;;;create a data structure to store chunks which in turn store voxels
;;;;or some sort of three dimensional data
;;;;Currently implemented by a hash table filled with arrays.
(in-package :vox)

;;;;different types of specification
;;;;1: layout of 3 numbers in the fixnum
;;;;2: size of trucator for each of the 3 numbers -- how large the individual
;;;;chunks are in that dimension
;;;;3. signed/unsigned/offset of 3 numbers
;;;;4. names of the different generated functions so they can refer to each other
;;;;by name
;;;;5. type of data to be stored, names of the functions, the hash table,
;;;;default "vacuum" state, space generators

(coge:defspec layout (num0-size num0-start num1-size num1-start num2-size num2-start))
(coge:defspec truncation (chopx chopy chopz))
(coge:defspec names (unhashfunc chunkhashfunc chop anti-chop rem-flow %%ref add))
(coge:defspec offset (offset0 offset1 offset2))
(coge:defspec field (data-type chunk-container vacuum-state space-provider))
(coge:defspec access (getter setter))

;;inline the headless lambda, give it a name, give it speed and recklessness
(defun make-fast (nombre headless-lambda)
  `(progn
     (declaim (inline ,nombre))
     (defun ,nombre ,(car headless-lambda)
       (declare (optimize (speed 3) (safety 0)))
       ,@(cdr headless-lambda))))

(defun not-too-fast (nombre headless-lambda)
  `(progn
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
	 (logior (the fixnum (ash (mod z p!uoffset2) p!num2-start))
		 (the fixnum (ash (mod y p!uoffset1) p!num1-start))
		 (the fixnum (ash (mod x p!uoffset0) p!num0-start))))))

(defmacro signed-unsiged (x n)
  (let ((n (eval n)))
    `(- (mod (the fixnum (+ ,x ,(/ n 2))) ,n) ,(/ n 2))))
(defun gen-unpacker(spec)
  `((fixnum)
    (values (signed-unsiged
	     ,(if (zerop (coge:rp spec 'p!num0-start))
		  `(logand fixnum (1- (ash 1 p!num0-size)))  
		  `(ldb (byte p!num0-size p!num0-start) fixnum)) p!uoffset0)
	    (signed-unsiged
	     (LDB (byte p!num1-size p!num1-start) fixnum) p!uoffset1)
	    (signed-unsiged
	     (ash fixnum (- p!num2-start)) p!uoffset2))))

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

(defun gen-%%ref (spec)
  (let ((size-0 (coge:rp spec 'p!chopx))
	(size-1 (coge:rp spec 'p!chopy))
	(size-2 (coge:rp spec 'p!chopz))
	(start-1 (coge:rp spec 'p!num1-start))
	(start-2 (coge:rp spec 'p!num2-start)))
    (let ((first-shift (- start-2 size-0 size-1))
	  (second-shift (- start-2 start-1 size-1)))
      `((code)
	(let ((c (p!anti-chop code)))
	  (the (unsigned-byte ,(+ size-0 size-1 size-2))
	       (ash (logior (the fixnum (ash c ,first-shift))
			    (the fixnum (ash c ,second-shift))
			    c) ,(- first-shift))))))))

(defun gen-add ()
  `((a b) (p!rem-flow (+ a b))))

(defun gen-getter ()
  `((i j k)
    (let ((block-code (p!chunkhashfunc i k j)))
      (let ((chunk-code (p!chop block-code)))
	(let ((chunk (gethash chunk-code p!chunk-container)))
	  (declare (type (or p!data-type null) chunk))
	  (if chunk
	      (values (aref chunk (p!%%ref block-code)) t)
	      (values p!vacuum-state nil)))))))

(defun gen-setter ()
  `((i j k blockid)
    (let ((block-code (p!chunkhashfunc i k j)))
      (let ((chunk-code (p!chop block-code)))
	(let ((chunk (or (gethash chunk-code p!chunk-container)
			 (setf
			  (gethash chunk-code p!chunk-container)
			  p!space-provider))))
	  (declare (type p!data-type chunk))
	  (setf (aref chunk (p!%%ref block-code)) blockid))))))


;;;;
;;;; what follows is the apparent usage of the code generation

(defun derived-parts (spec)
  (coge:add-spec
   spec
   `((uoffset0 . ,(eval (coge:rp spec '(ash 1 p!num0-size))))
     (uoffset1 . ,(eval (coge:rp spec '(ash 1 p!num1-size))))
     (uoffset2 . ,(eval (coge:rp spec '(ash 1 p!num2-size))))))
  (coge:add-spec
   spec
   `((overflow-mask . ,(coge:rp spec (gen-mask-overflow)))
     (truncate-mask . ,(coge:rp spec (gen-mask-truncate)))
     (anti-truncate-mask . ,(coge:rp spec (gen-mask-anti-truncate))))))

(defun define-fixnum-ops (spec)
  (coge:rp spec
	   `(progn
	      ,(fastnum (coge:rp spec 'p!unhashfunc) (gen-unpacker spec))
	      ,(fastnum (coge:rp spec 'p!chunkhashfunc) (gen-packer))
	      ,(fastnum (coge:rp spec 'p!chop) (gen-chopper))
	      ,(fastnum (coge:rp spec 'p!anti-chop) (gen-anti-chopper))
	      ,(fastnum (coge:rp spec 'p!rem-flow) (gen-remove-overflow))
	      ,(fastnum (coge:rp spec 'p!%%ref) (gen-%%ref spec))
	      ,(fastnum (coge:rp spec 'p!add) (gen-add)))))

;;the type of hash table that lets fixnums be equated [default is eql]
;;combine the number of bits per individual item, the get/set names,
;;the hash which hashes the fixnums, the default value, and the method
;;to create new chunks
(defun genhash ()
  (make-hash-table :test 'eq))

;;;block getter and setters are declared inline
(defun prep-hash (spec)
  (coge:rp spec
	   `(progn
	      ,(not-too-fast (coge:rp spec 'p!getter) (fixnums! (gen-getter)))
	      ,(not-too-fast (coge:rp spec 'p!setter) (fixnums! (gen-setter)))
	      (defun (setf p!getter) (new i j k)
		(,(coge:rp spec 'p!setter) i j k new)))))
