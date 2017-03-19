(in-package :sandbox)

(defparameter ourdir
  (make-pathname :host (pathname-host #.(or *compile-file-truename*
					    *load-truename*))
		 :directory (pathname-directory #.(or *compile-file-truename*
						      *load-truename*))))

(defun clamp (x min max)
  (max (min x max) min))

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

