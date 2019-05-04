(in-package #:sandbox)

(defun save (path things)
  (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (dolist (thing things)
      (print thing stream))))

(defun myload (path)
  (let ((file-existsp (probe-file path)))
    ;;if it doesn't exist, what's the point of loading it
    (when file-existsp
      (let ((things nil)) 
	(with-open-file (stream path :direction :input :if-does-not-exist nil)
	  (tagbody rep
	     (let ((thing (read stream nil nil)))
	       (when thing
		 (push thing things)
		 (go rep)))))
	(nreverse things)))))

(defun save2 (path thingfilename &rest things)
  (save (merge-pathnames (format nil "~s" thingfilename) path) things))
(defun myload2 (path thingfilename)
  (myload (merge-pathnames (format nil "~s" thingfilename) path)))


(defparameter *some-saves* nil)
(defparameter *world-directory* nil)
(defun world-path (&optional (path *world-directory*) (base-dir *some-saves*))
  (utility:rebase-path path base-dir))
(defun msave (&optional (path *world-directory*))
  (let ((newpath (world-path path)))
    (ensure-directories-exist newpath)
    (save-world newpath)))

(defun mload (&optional (path *world-directory*))
  (let ((newpath (world-path path)))
    (load-world newpath)))

(defun savechunk (position &optional (path (world-path)))
  ;;FIXME::undocumented swizzling and multiplication by 16, as well as loadchunk
  (save2
   path
   (chunk-coordinate-to-filename position)
   (world::chunk-data
    (world::obtain-chunk-from-chunk-key position nil))))

(defun loadchunk (path filename-position-list)
  (let ((position (filename-to-chunk-coordinate filename-position-list)))  
    (let ((data (myload2 path filename-position-list)))
      (case (length data)
	(0
	 ;;if data is nil, just load an empty chunk
	 (world::with-chunk-key-coordinates (x y z) position
	   (world::set-chunk-at
	    position
	    (world::create-chunk x y z))))
	#+nil
	(3 ;;FIXME::does this even work?
	 (destructuring-bind (blocks light sky) data
	   (let ((len (length blocks)))
	     (let ((new (make-array len)))
	       (world::make-chunk-from-key-and-data-and-keep position new)
	       (dotimes (i len)
		 (setf (aref new i)
		       (dpb (aref sky i) (byte 4 12)
			    (dpb (aref light i) (byte 4 8) (aref blocks i))))))))
	 t)
	(1
	 (let ((objdata (pop data)))
	   (when objdata
	     (world::make-chunk-from-key-and-data-and-keep
	      position
	      (coerce objdata '(simple-array t (*))))))
	 t)))))

;;The world is saved as a directory full of files named (x y z) in block coordinates, with
;;x and y swizzled

(defun filename-to-chunk-coordinate (filename-position-list)
  (let ((position
	 (mapcar
	  ;;FIXME::assumes chunks are 16 by 16 by 16
	  (lambda (n) (floor n 16))
	  filename-position-list)))
    (rotatef (third position)
	     (second position))
    position))

(defun chunk-coordinate-to-filename (chunk-coordinate)
  (let ((position-list (multiple-value-list (world:unhashfunc chunk-coordinate))))
    (rotatef (second position-list)
	     (third position-list))
    position-list))

(defun save-world (&optional (path (world-path)))
  (loop :for key :being :the :hash-keys :of  world::*chunks* :do
     (chunk-unload key path)))

(defun load-world (path)
  ;;FIXME::don't load the entire world
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (loadchunk path (read-from-string (pathname-name file))))))

(defun delete-garbage (&optional (path (world-path)))
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (let ((data
	     (myload file)))
	(when (typep data '(cons array null))
	  (delete-file file))))))
 

