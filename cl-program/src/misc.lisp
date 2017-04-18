(in-package :sandbox)

(defmacro etouq (form)
  (eval form))

(fuktard:eval-always
 (progn
   (defun preach (value form)
     (mapcar (lambda (x)
	       (list value x))
	     form))

   (defun raps (times form)
     (make-list times :initial-element form))

   (defun ngorp (&rest forms)
     (cons (quote progn)
	   (apply (function nconc) forms)))
   (defun ensure (place otherwise)
     (let ((value-var (gensym))
	   (exists-var (gensym)))
       `(or ,place
	    (multiple-value-bind (,value-var ,exists-var) ,otherwise
	      (if ,exists-var
		  (values (setf ,place ,value-var) ,exists-var))))))))

(defparameter *something* #.(or *compile-file-truename* *load-truename*))

(defparameter ourdir
  (make-pathname :host (pathname-host *something*)
		 :directory (pathname-directory *something*)))


(defconstant +single-float-pi+ (coerce pi 'single-float))
(defconstant +single-float-two-pi+ (coerce (* 2 pi) 'single-float))
(defconstant +single-float-half-pi+ (coerce (/ pi 2) 'single-float))

(defun clamp (x min max)
  (max (min x max) min))

(defparameter *temp-matrix* (cg-matrix:identity-matrix))
(defparameter *temp-matrix2* (cg-matrix:identity-matrix))
(defparameter *temp-matrix3* (cg-matrix:identity-matrix))
(defparameter *x-unit* (cg-matrix:vec 1.0 0.0 0.0))

(defun byte-read (path)
   (with-open-file (stream path :element-type '(unsigned-byte 8))
     (let* ((len (file-length stream))
	    (data (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (n len)
	 (setf (aref data n) (read-byte stream)))
       data)))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))
 
(defun spill-hash (hash &optional (stream *standard-output*))
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format stream "~S ~S~%" key value)))

(defun print-bits (n)
  (format t "~64,'0b" n))

(defun getapixel (h w image)
  (destructuring-bind (height width c) (array-dimensions image)
    (declare (ignore height))
    (make-array 4 :element-type (array-element-type image)
		:displaced-to image
		:displaced-index-offset (* c (+ w (* h width))))))

;;;;load a png image from a path
(defun load-png (filename)
  (opticl:read-png-file filename))

(defun fmakunbounds (symbol-list)
  (dolist (symbol symbol-list)
    (fmakunbound symbol)))

(defun makunbounds (symbol-list)
  (dolist (symbol symbol-list)
    (makunbound symbol)))

(defmacro xfmakunbounds (&body symbols)
  `(fmakunbounds (quote ,symbols)))

(defmacro xmakunbounds (&body symbols)
  `(makunbounds (quote ,symbols)))

(defun complex-modulus (c)
  (sqrt (realpart (* c (conjugate c)))))

;;;;flip an image in-place - three dimensions - does not conse
(defun flip-image (image)
  (let ((dims (array-dimensions image)))
    (let ((height (pop dims))
	  (width (pop dims)))
      (if dims
	  (let ((components (car dims)))
	    (dobox ((h 0 (- height (ash height -1)))
		    (w 0 width)
		    (c 0 components))
		   (rotatef (aref image (- height h 1) w c)
			    (aref image h w c))))
	  (dobox ((h 0 (- height (ash height -1)))
		  (w 0 width))
	      (rotatef (aref image (- height h 1) w)
		       (aref image h w))))))
  image)


(defparameter dir-resource (merge-pathnames #P"res/" ourdir))
(defparameter dir-shader (merge-pathnames #P"shaders/" dir-resource))

(defun shader-path (name)
  (merge-pathnames name dir-shader))

(defun img-path (name)
  (merge-pathnames name dir-resource))

(defun namexpr (hash name func)
  (setf (gethash name hash) func))

(defun get-stuff (name stuff otherwise)
  (etouq
   (ensure (quote (gethash name stuff))
	   (quote (let ((genfunc (gethash name otherwise)))
		    (when (functionp genfunc)
		      (values (funcall genfunc) t)))))))

(defun create-call-list-from-func (func)
  (let ((the-list (gl:gen-lists 1)))
    (gl:new-list the-list :compile)
    (funcall func)
    (gl:end-list)
    the-list))


(defconstant +available-bits+ (logcount most-positive-fixnum))
(defconstant +x-bits-start+ (floor +available-bits+ 2))
(defconstant +x-chunk-bits+ 5)
(defconstant +x-chunk-size+ (ash 1 +x-chunk-bits+))
(defconstant +x-bitmask+ (1- +x-chunk-size+))
(defconstant +y-chunk-bits+ 5)
(defconstant +y-chunk-size+ (ash 1 +y-chunk-bits+))
(defconstant +y-bitmask+ (1- +y-chunk-size+))
(defconstant +xy-bitmask+ (1- (* +y-chunk-size+ +x-chunk-size+)))
(defconstant +index-mask+ (logior (ash +x-bitmask+ +x-bits-start+)
				  +y-bitmask+))
(defconstant +hash-mask+ (logxor +index-mask+ most-positive-fixnum))
(defconstant +right-shift+ (- +y-chunk-bits+ +x-bits-start+))

(defun make-chunk ()
  (make-array (ash 1 (+ +chunk-size-x+ +chunk-size-y+))
	      :element-type t
	      :initial-element nil))
(defparameter *chunks* (make-hash-table :test (quote eq)))

(progn
  (declaim (inline (setf get-obj)))
  (defun (setf get-obj) (value place hash-table)
    (set-obj place value hash-table)))

(progn
  (declaim (ftype (function (fixnum t hash-table) t)
		  set-obj)
	   (inline set-obj))
  (defun set-obj (place value hash)
    (declare (optimize (speed 3) (safety 0)))
    (let ((hash-id (logand place +hash-mask+)))
      (let ((chunk (gethash hash-id hash)))
	(declare (type (or null simple-vector) chunk))
	(unless chunk
	  (let ((new-chunk (make-chunk)))
	    (setf (gethash hash-id hash) new-chunk)
	    (setf chunk new-chunk)))
	(let* ((num (logand place +index-mask+))
	       (num2 (ash num +right-shift+))
	       (num3 (logand +xy-bitmask+ (logior num num2))))
	  (declare (type fixnum num num2 num3))
	  (setf (aref chunk num3) value))))))
(progn
  (declaim (ftype (function (fixnum hash-table) t)
		  get-obj)
	   (inline get-obj))
  (defun get-obj (place hash)
    (declare (optimize (speed 3) (safety 0)))
    (let ((hash-id (logand place +hash-mask+)))
      (let ((chunk (gethash hash-id hash)))
	(declare (type (or null simple-vector) chunk))
	(if chunk
	    (let* ((num (logand place +index-mask+))
		   (num2 (ash num +right-shift+))
		   (num3 (logand +xy-bitmask+ (logior num num2))))
	      (declare (type fixnum num num2 num3))
	      (aref chunk num3)))))))

(progn
  (declaim (ftype (function (fixnum fixnum) fixnum)
		  xy-index)
	   (inline xy-index))
  (defun xy-index (x y)
    (declare (optimize (speed 3) (safety 0)))
    (let ((fnum (ash x +x-bits-start+)))
      (declare (type fixnum fnum))
      (logior y fnum))))
