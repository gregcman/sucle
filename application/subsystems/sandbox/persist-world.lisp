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


(defparameter *some-saves* nil)
(defun msave (path)
  (let ((newpath (utility:rebase-path path *some-saves*)))
    (ensure-directories-exist newpath)
    (save-world newpath)))
(defun mload (path)
  (let ((newpath (utility:rebase-path path *some-saves*)))
    (load-world newpath)))

(defun savechunk (path position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (rotatef (second position-list)
	     (third position-list))
    (save2
     path
     position-list
     (gethash position world::*lispobj*))))

(defun loadchunk (path position-list)
  (let ((position
	 (destructuring-bind (x y z) position-list
	     (world:chunkhashfunc x z y))))
    (let ((data (myload2 path position-list)))
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
 

