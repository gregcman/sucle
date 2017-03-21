(in-package :sandbox)

(defparameter ourdir
  (make-pathname :host (pathname-host #.(or *compile-file-truename*
					    *load-truename*))
		 :directory (pathname-directory #.(or *compile-file-truename*
						      *load-truename*))))


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

(defun spill-hash (hash)
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format t "~S ~S~%" key value)))

(defun getapixel (h w image)
  (destructuring-bind (height width c) (array-dimensions image)
    (declare (ignore height))
    (make-array 4 :element-type (array-element-type image)
		:displaced-to image
		:displaced-index-offset (* c (+ w (* h width))))))

;;;;load a png image from a path
(defun load-png (filename)
  (opticl:read-png-file filename))

(defparameter *save* #P"third/")

(defparameter *saves-dir* (merge-pathnames #P"saves/" ourdir))

(defun save (filename &rest things)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (thing things)
	(prin1 thing stream)))))

(defun save2 (thingfilename &rest things)
  (apply #'save (merge-pathnames (format nil "~s" thingfilename) *save*) things))

(defun myload2 (thingfilename)
  (myload (merge-pathnames (format nil "~s" thingfilename) *save*)))

(defun myload (filename)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (let ((things nil))
      (with-open-file (stream path :direction :input :if-does-not-exist nil)
	(tagbody rep
	   (let ((thing (read stream nil nil)))
	     (when thing
	       (push thing things)
	       (go rep)))))
      (nreverse things))))

(progn
  (defparameter *g/image* (make-hash-table :test 'equal))		    ;;raw image arrays
  (defun get-image (name)
    (let ((img (gethash name *g/image*)))
      (if img
	  img
	  (get-image-backup name))))
  (defun set-image (name image-data)
    (setf (gethash name *g/image*) image-data))
  (defun remove-image (name)
    (remhash name *g/image*)))
(progn
  (defparameter *g/image-backup* (make-hash-table :test 'equal))
  (defun get-image-backup (name)
    (let ((image-func (gethash name *g/image-backup*)))
      (when (functionp image-func)
	(let ((ans (funcall image-func name)))
	  (when ans
	    (set-image name ans)))))))

(progn
  (defparameter *g/text* (make-hash-table :test 'equal))   ;;text: sequences of bytes
  (defun get-text (name)
    (let ((text (gethash name *g/text*)))
      (if text
	  text
	  (get-text-backup name))))
  (defun set-text (name text-data)
    (setf (gethash name *g/text*) text-data))
  (defun remove-text (name)
    (remhash name *g/text*)))
(progn
  (defparameter *g/text-backup* (make-hash-table :test 'equal))
  (defun get-text-backup (name)
    (let ((text-func (gethash name *g/text-backup*)))
      (when (functionp text-func)
	(let ((ans (funcall text-func name)))
	  (when ans
	    (set-text name ans)))))))


(defun print-bits (n)
  (format t "~64,'0b" n))


(progn
  (declaim (type (simple-vector) *scratch-float-array*)
	   (type fixnum *scratch-float-array-size*))
  (defparameter *scratch-float-array* (make-array 1 :initial-element nil))
  (defparameter *scratch-float-array-size* (array-total-size *scratch-float-array*)))


(progn
  (declaim (type fixnum +hash-mask+ +index-mask+))
  (defconstant +scratch-float-array-log-size+ 10)
  (defconstant +scratch-float-array-chunk-size+ (expt 2 +scratch-float-array-log-size+))
  (defconstant +hash-mask+ (- +scratch-float-array-chunk-size+ 1))
  (defconstant +index-mask+ (%ash most-positive-fixnum +scratch-float-array-log-size+)))

(defun %ash (n k)
  (declare (type fixnum n)
	   (type (unsigned-byte 8) k)
	   (optimize (speed 3) (safety 0)))
  (the fixnum (ash n k)))

(defun next-power-of-two (n)
  (expt 2 (1+ (floor (log n 2)))))

(defun create-scratch-float-array-byte ()
  (make-array +scratch-float-array-chunk-size+ :element-type '(unsigned-byte 8)))

(progn
  (declaim (ftype (function () (simple-array single-float (*))) create-scratch-float-array-float))
  (defun create-scratch-float-array-float ()
    (make-array +scratch-float-array-chunk-size+ :element-type 'single-float)))


(progn
  (declaim (inline get-index))
  (declaim (ftype (function (fixnum) fixnum) get-index))
  (locally (declare (optimize (speed 3) (safety 0)))
    
    (defun get-index (n)
      (logand n +hash-mask+)))
  
  (declaim (notinline get-index)))

(progn
  (declaim (inline get-get-index))
  (declaim (ftype (function (fixnum) fixnum) get-get-index))
  (locally (declare (optimize (speed 3) (safety 0)))
    
    (defun get-get-index (n)
      (ash n (- +scratch-float-array-log-size+))))
  
  (declaim (notinline get-get-index)))

(defun resize-array (array new-size initial-element)
  (let ((type (array-element-type array)))
    (let ((newarray (make-array new-size :element-type type :initial-element initial-element)))
      (dotimes (x (min (array-total-size array) new-size))
	(setf (aref newarray x) (aref array x)))
      newarray)))

(defun resize-scratch-float-array (n)
  (setf *scratch-float-array* (resize-array *scratch-float-array* n nil)
	*scratch-float-array-size* n))


(progn
  (declaim (ftype (function (fixnum) (values t (member null t))) get-scratch-float-array))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline get-index get-get-index))
    
    (defun get-scratch-float-array (n)
      (let ((hashcode (get-get-index n)))
	(if (< hashcode *scratch-float-array-size*)
	    (let ((array (aref *scratch-float-array* hashcode)))
	      (declare (type (or nil (simple-array single-float (*))) array))
	      (if (null array)
		  (values nil nil)
		  (let ((chunk-index (get-index n)))
		    (values (aref array chunk-index) t))))
	    (values nil nil))))))


(progn
  (declaim (ftype (function (fixnum t) (values)) set-scratch-float-array))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline get-index get-get-index))
    (defun set-scratch-float-array (n value)
      (let ((hashcode (get-get-index n))
	    (chunk-index (get-index n)))
	(tagbody
	   (if (< hashcode *scratch-float-array-size*)
	       (let ((array (aref *scratch-float-array* hashcode)))
		 (declare (type (or nil (simple-array single-float (*))) array))
		 (if (null array)
		     (go new-chunk)
		     (progn
		       (setf (aref array chunk-index) value)
		       (go end))))
	       (progn
		 (resize-scratch-float-array (next-power-of-two hashcode))
		 (go new-chunk)))
	 new-chunk
	   (let ((new-chunk (create-scratch-float-array-float)))
	     (setf (aref new-chunk chunk-index) value)
	     (setf (aref *scratch-float-array* hashcode) new-chunk))
	 end)))))


(defun iterate-scratch-float-array (n func)
  (let ((last (get-get-index n)))
    (dotimes (chunk (1+ last))
      (let ((current-chunk-array (aref *scratch-float-array* chunk)))
	(if (= last chunk)
	    (dotimes (index (get-index n))
	      (funcall func (aref current-chunk-array index)))
	    (dotimes (index +scratch-float-array-chunk-size+)
	      (funcall func (aref current-chunk-array index))))))))
