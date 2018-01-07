(in-package #:sandbox)

(defparameter *some-saves* nil)
(defun msave (path)
  (let ((newpath (filesystem-util:rebase-path path *some-saves*)))
    (ensure-directories-exist newpath)
    (save-world newpath)))
(defun mload (path)
  (let ((newpath (filesystem-util:rebase-path path *some-saves*)))
    (load-world newpath)))

(defun savechunk (path position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (filesystem-util:save2
     path
     position-list
     (gethash position world::*lispobj*))))

(defun loadchunk (path position-list)
  (let ((position (apply #'world:chunkhashfunc position-list)))
    (let ((data (filesystem-util:myload2 path position-list)))
      (case (length data)
	(3
	 (destructuring-bind (blocks light sky) data
	   (let ((len (length blocks)))
	     (let ((new (make-array len)))
	       (setf (gethash position world::*lispobj*)
		     new)
	       (dotimes (i len)
		 (setf (aref new i)
		       (dpb (aref sky i) (byte 4 12)
			    (dpb (aref light i) (byte 4 8) (aref blocks i))))))))
	 t)
	(1
	 (let ((objdata (pop data)))
	   (when objdata
	     (setf (gethash position world::*lispobj*)
		   (coerce objdata '(simple-array t (*))))))
	 t)))))

(defun save-world (path)
  (maphash (lambda (k v)
	     (declare (ignorable v))
	     (savechunk path k))
	   world::*lispobj*))
(defun load-world (path)
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (loadchunk path (read-from-string (pathname-name file))))))
 

