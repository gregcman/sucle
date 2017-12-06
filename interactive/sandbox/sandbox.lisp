(in-package #:sandbox)

(defun save (path things)
  (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (dolist (thing things)
      (print thing stream))))

(defun myload (path)
  (let ((things nil))
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      (tagbody rep
	 (let ((thing (read stream nil nil)))
	   (when thing
	     (push thing things)
	     (go rep)))))
    (nreverse things)))

(defun save2 (path thingfilename &rest things)
  (save (merge-pathnames (format nil "~s" thingfilename) path) things))

(defun myload2 (path thingfilename)
  (myload (merge-pathnames (format nil "~s" thingfilename) path)))

(defparameter *saves-dir* nil)

(defun savechunk (path position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (save2
     path
     position-list
     (gethash position world:chunkhash)
     (gethash position world:lighthash)
     (gethash position world:skylighthash))))

(defun loadchunk (path position-list)
  (let ((position (apply #'world:chunkhashfunc position-list)))
    (let ((data (myload2 path position-list)))
      (when data
	
	(and
	 (let ((blocks (pop data)))
	   (when blocks
	     (setf (gethash position world:chunkhash)
		   (coerce blocks '(simple-array (unsigned-byte 8) (*)))))
	   data)
	 (let ((light (pop data)))
	   (when light
	     (setf (gethash position world:lighthash)
		   (coerce light '(simple-array (unsigned-byte 4) (*)))))
	   data)
	 (let ((sky (pop data)))
	   (when sky
	     (setf (gethash position world:skylighthash)
		   (coerce sky '(simple-array (unsigned-byte 4) (*)))))))
	(return-from loadchunk t)))))

(defun save-world (path)
  (maphash (lambda (k v)
	     (declare (ignorable v))
	     (savechunk path k))
	   world:chunkhash))
(defun load-world (path)
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (loadchunk path (read-from-string (pathname-name file))))))
 

