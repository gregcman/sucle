(in-package #:sandbox)

(defun initialization1 ()
  (clrhash *g/call-list*)
  (clrhash *g/chunk-call-list*)

  (setf mesher-thread nil)
  (clean-dirty)
  )

(defparameter *save* (case 3
		       (0 #P"terrarium2/")
		       (1 #P"first/")
		       (2 #P"second/")
		       (3 #P"third/")
		       (4 #P"fourth/")
		       (5 #P"world/")
		       (6 #P"terrarium/")
		       (7 #P"holymoly/")
		       (8 #P"funkycoolclimb/")
		       (9 #P"ahole/")
		       (10 #P"maze-royale/")
		       (11 #P"bloodcut/")
		       (12 #P"wasteland/")))

(defparameter *saves-dir* (merge-pathnames #P"sandbox-saves/"
					   "/home/imac/Documents/lispysaves/saves/"))

(defun save (filename &rest things)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (thing things)
	(print thing stream)))))

(defun save2 (thingfilename &rest things)
  (apply #'save (merge-pathnames (format nil "~s" thingfilename) *save*) things))

(defun savechunk (position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (save2 position-list
	   (gethash position world:chunkhash)
	   (gethash position world:lighthash)
	   (gethash position world:skylighthash))))

(defun save-world ()
  (maphash (lambda (k v)
	     (declare (ignorable v))
	     (savechunk k))
	   world:chunkhash))

(defun looad-world ()
  (let ((files (uiop:directory-files (merge-pathnames *save* *saves-dir*))))
    (dolist (file files)
      (loadchunk (apply #'world:chunkhashfunc (read-from-string (pathname-name file)))))))

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

(defun loadchunk (position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (let ((data (myload2 position-list)))
      (when data 
	(destructuring-bind (blocks light sky) data
	  (setf (gethash position world:chunkhash)
		(coerce blocks '(simple-array (unsigned-byte 8) (*))))
	  (setf (gethash position world:lighthash)
		(coerce light '(simple-array (unsigned-byte 4) (*))))
	  (setf (gethash position world:skylighthash)
		(coerce sky '(simple-array (unsigned-byte 4) (*)))))
	(return-from loadchunk t)))))  



