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

(defun totally-destroy-package (package)
  (do-symbols (symbol package)
    (let ((home (symbol-package symbol)))
      (when (eq package home)
	(when (fboundp symbol)
	  (fmakunbound symbol))
	(when (boundp symbol)
	  (makunbound symbol)))))
  (delete-package package))

(defparameter shit nil)

(defun test ()
  (dotimes (x (expt 10 4))
    (let ((package (make-package (gensym))))
      (push package shit))))

(defun test2 (symbol)
  (eval
   `(progn
      (declaim (inline ,symbol))
      (defun ,symbol (package)
	(do-symbols (symbol package)
	  (let ((home (symbol-package symbol)))
	    (when (eq package home)
	      (when (fboundp symbol)
		(fmakunbound symbol))
	      (when (boundp symbol)
		(makunbound symbol)))))
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(write package))
      (declaim (notinline ,symbol))
      
      (values
       (lambda (x)
	 (locally (declare (inline ,symbol))
	   (,symbol x)))
       (quote,symbol)))))

(defparameter foo nil)

(defun wot ()
  (setf foo nil)
  (do-symbols (symbol :cl)
    (if (or (boundp symbol) (fboundp symbol))
	(push (symbol-name symbol) foo)))
  (setf foo (sort foo #'string<)))

(defun simple-defun-p (form)
  (and (eq (quote defun) (pop form))
       (symbolp (pop form))))

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

(defmacro deach (func-or-macro &rest forms)
  (cons 'progn
	(mapcar (lambda (x) (list func-or-macro x)) forms)))
