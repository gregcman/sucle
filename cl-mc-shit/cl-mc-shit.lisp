(in-package #:cl-mc-shit)

(defparameter ourdir
  (make-pathname :host (pathname-host #.(or *compile-file-truename*
					    *load-truename*))
		 :directory (pathname-directory #.(or *compile-file-truename*
						      *load-truename*))))
(defparameter cnbt-master-dir (merge-pathnames #P"cNBT-master/" ourdir))
(defparameter test-data-dir (merge-pathnames #P"testdata/" cnbt-master-dir))

(defmacro progno (&rest args))
(defun my-subseq (array start amount)
	(if (< -1 (+ start amount) (length array))
		(subseq array start (+ start amount))
		nil))

(defun bytetostring (darray)
  (let ((newvec (make-array (length darray))))
    (dotimes (x (length darray))
      (setf (aref newvec x) (code-char (aref darray x))))
    (coerce newvec 'string)))

(defun atesto (x y)
  (mcr-chunk testchunk x y))

(defun mcr-chunk (mcrdata x z)
  (let* ((initial (chunk-byte-offset x z))
	 (firstdata (subseq mcrdata initial (+ initial 4)))
	 (region-offset (tonum (subseq firstdata 0 3)))
	 (page-length (elt firstdata 3))
	 (time-stamp (tonum (my-subseq mcrdata (+ 4096 initial) 4))))
    (declare (ignore time-stamp))
	     ; (print (list initial firstdata region-offset page-length time-stamp))
    (if (zerop page-length)
	nil
	(let* ((chunkdata (my-subseq mcrdata
				     (* 4096 region-offset) (* page-length 4096))))
	  (if chunkdata
	      (let((chunklength (tonum (my-subseq chunkdata 0 4))))
		(if (< (* 4096 page-length) (+ 4 chunklength))
		    nil
		    (let* ((compression-type (elt chunkdata 4))
			   (compressed-chunk-data (my-subseq chunkdata 5 (1- chunklength))))
					;	(print (list region-offset page-length time-stamp))
		      (if (= 2 compression-type)
			  (chipz:decompress nil 'chipz:zlib compressed-chunk-data)
			  (chipz:decompress nil 'chipz:gzip compressed-chunk-data)))))
	      nil)))))

(defun chunk-byte-offset (x z)
  (* 4
     (+
      (mod x 32)
      (* 32 (mod z 32)))))

(defun byte-read (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let* ((len (file-length stream))
	   (data (make-array len :element-type '(unsigned-byte 8))))
      (dotimes (n len)
	(setf (aref data n) (read-byte stream)))
      data)))
(defparameter testchunk (byte-read (merge-pathnames "hell.mcr" test-data-dir)))

(defun tonum (nums)
  (let ((danum 0))
    (setf nums (reverse nums))
    (dotimes (n (length nums))
      (incf danum (ash (elt nums n) (* 8 n))))
    danum))

(defparameter apath cnbt-master-dir)
(pushnew apath cffi:*foreign-library-directories*
	 :test #'equal)

(cffi:load-foreign-library '(:default "libnbt"))

(cffi:define-foreign-library libnbt
  (t (:default "libnbt")))


(cffi:defcstruct nbt_byte_array
  (data (:pointer :unsigned-char))
  (length :uint32))

(cffi:defcstruct nbt_int_array
  (data (:pointer :uint32))
  (length :uint32))

(cffi:defcenum nbt_type
  (:TAG_INVALID    0);  /* tag_end, but we don't use it in the in-memory representation. */
  (:TAG_BYTE       1)		  ;,  /* char, 8 bits, signed */
  (:TAG_SHORT      2)		  ;,  /* short, 16 bits, signed */
  (:TAG_INT        3)		  ;,  /* long, 32 bits, signed */
  (:TAG_LONG       4)		  ;,  /* long long, 64 bits, signed */
  (:TAG_FLOAT      5)		  ;,  /* float, 32 bits, signed */
  (:TAG_DOUBLE     6)		  ;,  /* double, 64 bits, signed */
  (:TAG_BYTE_ARRAY 7) ;,  /* char *, 8 bits, unsigned, TAG_INT length */
  (:TAG_STRING     8) ;,  /* char *, 8 bits, signed, TAG_SHORT length */
  (:TAG_LIST       9) ;,  /* X *, X bits, TAG_INT length, no names inside */
  (:TAG_COMPOUND   10)			;, /* nbt_tag * */
  (:TAG_INT_ARRAY  11))

(cffi:defcstruct list_head
  (blink (:pointer (:struct list_head)))
  (flink (:pointer (:struct list_head))))

(cffi:defcstruct nbt_list
  (data (:pointer))
  (entry (:struct list_head)))

(cffi:defcunion apayload
  (tag_byte (:int8))
  (tag_short (:int16))
  (tag_int (:int32))
  (tag_long (:int64))
  (tag_float (:float))
  (tag_double (:double))
  (tag_byte_array (:struct nbt_byte_array))
  (tag_int_array (:struct nbt_int_array))
  (tag_string (:pointer :char))
  (tag_list (:pointer (:struct nbt_list)))
  (tag_compound (:pointer (:struct nbt_list))))

(cffi:defcstruct nbt_node
  (type nbt_type)
  (name (:pointer :char))
  (payload (:union apayload)))

(cffi:defcstruct nbt_list
  (data (:pointer (:struct nbt_node)))
  (entry (:struct list_head)))

(cffi:use-foreign-library libnbt)

(cffi:defcfun "nbt_parse_path" (:pointer (:struct nbt_node))
  (str  :pointer))

(cffi:defcfun "nbt_parse" (:pointer (:struct nbt_node))
  (mem :pointer)
  (size :unsigned-int))

(cffi:defcfun "nbt_dump_ascii" :pointer
  (letree (:pointer (:struct nbt_node))))

(cffi:defcfun "nbt_free" :void
  (letree (:pointer (:struct nbt_node))))

(cffi:defcfun "thefuck" (:pointer (:struct nbt_list))
  (entry (:pointer (:struct list_head))))

(defun cstringtolisp (cstring)
  (let ((dastring))
    (tagbody
       (handler-bind
	   ((error
	     (lambda (condition)
	       (print condition)
	       (restart-case
		   (let ((r (find-restart 'my-restart)))
		     (invoke-restart r))
		 (my-restart () (go huh))))))
	 (setf dastring (cffi:foreign-string-to-lisp cstring)))
       (return-from cstringtolisp dastring)
     huh
       (return-from cstringtolisp nil))))

(defun mytestic (lispstring)
  (let ((node nil))
    (cffi:with-foreign-pointer-as-string (str (1+ (length lispstring)))
      (setf node
	    (nbt-parse-path
	     (cffi:lisp-string-to-foreign lispstring str (1+ (length lispstring))))))
    (if (cffi:null-pointer-p node)
	nil
	node)))

(defun chunk-data (vec)
  (let ((node nil)
	(thearray (cffi:foreign-alloc :unsigned-char :initial-contents vec))
	(ans nil)
	(actualans nil))
    (setf node
	  (nbt-parse
	   thearray
	   (length vec)))
    (setf ans
	  (if (cffi:null-pointer-p node)
	      nil
	      node))
    (if ans
	(setf actualans (nbt-dump-tree ans)))
    (cffi:foreign-free thearray)
    (nbt-free node)
    actualans))


(defun ummm (vec)
  (let ((node nil)
	(thearray (cffi:foreign-alloc :unsigned-char :initial-contents vec)))
    (setf node
	  (nbt-parse
	   thearray
	   (length vec)))
    (if (cffi:null-pointer-p node)
	nil
	node)))

(defun wowzee (lispstring)
  (let ((node nil)
	(len (+ 1 (length lispstring))))
    (cffi:with-foreign-pointer-as-string (str len)
      (setf node
	    (nbt-parse
	     (cffi:lisp-string-to-foreign lispstring str len)
	     len)))
    (if (cffi:null-pointer-p node)
	nil
	node)))

(defparameter issue18
  (chipz:decompress
   nil
   'chipz:zlib
   (byte-read (merge-pathnames "issue_18.nbt" test-data-dir))))

(defparameter 18tree (ummm issue18))

(defparameter hello_world
  (chipz:decompress
   nil
   'chipz:gzip
   (byte-read (merge-pathnames "hello_world.nbt" test-data-dir))))

(defun 1test (huh x y)
  (case huh
    (0 (mytestic (merge-pathnames "hello_world.nbt" test-data-dir)))
    (1 (mytestic "lol"))
    (2 (cstringtolisp (nbt-dump-ascii (ummm hello_world))))
    (3 (cstringtolisp (nbt-dump-ascii (ummm issue18))))
    (t (wowzee (bytetostring (mcr-chunk testchunk x y))))))

(defun dumptree (tree)
  (let ((thetree (ummm tree)))
    (if (cffi:null-pointer-p thetree)
	(print "damn null pointers")
	(cstringtolisp
	 (nbt-dump-ascii thetree)))))

(defun dumpchunk (data x y)
  (let ((thechunk (mcr-chunk data x y)))
    (if thechunk
	(dumptree thechunk)
	(print "nochunk"))))

(defun nbt-byte-array-to-vec (bytearraypointer)
  (cffi:with-foreign-slots ((data length) bytearraypointer (:struct nbt_byte_array))
    (let ((counter 0)
	  (thevec (make-array length :element-type '(unsigned-byte 8))))
      (dotimes (x length)
	(setf (aref thevec counter) (cffi:mem-ref data :unsigned-char counter))
	(incf counter))
      thevec)))

(defun nbt-int-array-to-vec (intarraypointer)
  (cffi:with-foreign-slots ((data length) intarraypointer (:struct nbt_int_array))
    (let ((counter 0)
	  (thevec (make-array length :element-type '(unsigned-byte 32))))
      (dotimes (x length)
	(setf (aref thevec counter) (cffi:mem-ref data :uint32 (* 4 counter)))
	(incf counter))
      thevec)))

(defun nextlink (thelink)
  (cffi:foreign-slot-value thelink '(:struct list_head) 'flink))

(defun walklist (nbtlist)
  (let* ((theentry (cffi:foreign-slot-pointer nbtlist '(:struct nbt_list) 'entry))
	 (pos theentry)
	 (places nil))
    (labels ((lenext ()
	       (setf pos (nextlink pos))
	       (if (cffi:pointer-eq pos theentry)
		   nil
		   (progn
		     (push pos places)
		     (lenext)))))
      (lenext))
    (nreverse places)))

(defun actuallywtf (ptr)
  (cffi:inc-pointer
   ptr
   (- (cffi:foreign-slot-offset (quote (:struct nbt_list)) 'entry))))

(defun nbt-dump-tree (treepointer)
  (let ((type (cffi:foreign-slot-value treepointer '(:struct nbt_node) 'type))
	(payload (cffi:foreign-slot-pointer treepointer '(:struct nbt_node) 'payload))
	(name (cffi:foreign-slot-value treepointer '(:struct nbt_node) 'name)))
  
    (list
     type
     (cstringtolisp name)
     (case type
       (:TAG_INVALID :tag_invalid)
					;  /* tag_end, but we don't use it in the in-memory representation. */
       (:TAG_BYTE
	(cffi:foreign-slot-value payload '(:union apayload) 'tag_byte))
					;,  /* char, 8 bits, signed */
       (:TAG_SHORT
	(cffi:foreign-slot-value payload '(:union apayload) 'tag_short))
					;,  /* short, 16 bits, signed */
       (:TAG_INT
	(cffi:foreign-slot-value payload '(:union apayload) 'tag_int))
					;,  /* long, 32 bits, signed */
       (:TAG_LONG
	(cffi:foreign-slot-value payload '(:union apayload) 'tag_long))
					;,  /* long long, 64 bits, signed */
       (:TAG_FLOAT
	(cffi:foreign-slot-value payload '(:union apayload) 'tag_float))
					;,  /* float, 32 bits, signed */
       (:TAG_DOUBLE
	(cffi:foreign-slot-value payload '(:union apayload) 'tag_double))
					;,  /* double, 64 bits, signed */
       (:TAG_BYTE_ARRAY
	(nbt-byte-array-to-vec
	 (cffi:foreign-slot-pointer payload '(:union apayload) 'tag_byte_array)))
					;,  /* char *, 8 bits, unsigned, TAG_INT length */
       (:TAG_STRING
	(cstringtolisp
	 (cffi:foreign-slot-value payload '(:union apayload) 'tag_string)))
					;,  /* char *, 8 bits, signed, TAG_SHORT length */
       (:TAG_LIST
	(let ((theunits
	       (walklist 
		(cffi:foreign-slot-value payload '(:union apayload) 'tag_list)))
	      (oursub nil))
	  (dolist (n theunits)
	    (let ((thepoint (actuallywtf n)))
	      (push
	       (nbt-dump-tree
		(cffi:foreign-slot-value thepoint '(:struct nbt_list) 'data))
	       oursub)))
	  (nreverse oursub)))
					;,  /* X *, X bits, TAG_INT length, no names inside */
       (:TAG_COMPOUND
	(let ((theunits
	       (walklist 
		(cffi:foreign-slot-value payload '(:union apayload) 'tag_compound)))
	      (oursub nil))
	  (dolist (n theunits)
	    (let ((thepoint (actuallywtf n)))
	      (push
	       (nbt-dump-tree
		(cffi:foreign-slot-value thepoint '(:struct nbt_list) 'data))
	       oursub)))
	  (nreverse oursub)))					;, /* nbt_tag * */
       (:TAG_INT_ARRAY
	(nbt-int-array-to-vec
	 (cffi:foreign-slot-pointer payload '(:union apayload) 'tag_int_array)))
       (t nil)))))
