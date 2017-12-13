(in-package #:sandbox)

(defparameter *some-saves* nil)
(defun msave (path)
  (let ((newpath (filesystem-util:rebase-path path *some-saves*)))
    (ensure-directories-exist newpath)
    (save-world newpath)))
(defun mload (path)
  (let ((newpath (filesystem-util:rebase-path path *some-saves*)))
    (ensure-directories-exist newpath)
    (load-world newpath)))

(defun savechunk (path position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (filesystem-util:save2
     path
     position-list
     (gethash position world:chunkhash)
     (gethash position world:lighthash)
     (gethash position world:skylighthash))))

(defun loadchunk (path position-list)
  (let ((position (apply #'world:chunkhashfunc position-list)))
    (let ((data (filesystem-util:myload2 path position-list)))
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
 

