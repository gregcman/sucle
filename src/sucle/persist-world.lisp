(in-package #:sandbox)

;;FIXME::move generic loading and saving with printer and conspack to a separate file?
;;And have chunk loading in another file?

;;world loading code below?
(defun convert-object-to-filename (obj)
  (format nil "~s" obj))

(defparameter *some-saves* nil)
(defparameter *world-directory* nil)
(defun world-path (&optional (path *world-directory*) (base-dir *some-saves*))
  (utility:rebase-path path base-dir))
(defun msave (&optional (path *world-directory*))
  (let ((newpath (world-path path)))
    (ensure-directories-exist newpath)
    (save-world newpath)))

#+nil
(defun mload (&optional (path *world-directory*))
  (let ((newpath (world-path path)))
    (load-world newpath)))

(defun savechunk (chunk position &optional (path (world-path)))
  ;;FIXME::undocumented swizzling and multiplication by 16, as well as loadchunk
  (let ((filename (convert-object-to-filename (chunk-coordinate-to-filename position))))
    ;;(format t "~%Saving chunk ~a" filename)
    (sucle-serialize::store-lisp-objects-to-file
     (merge-pathnames
      filename
      path)
     (list
      (world::chunk-data chunk)))))

(defun loadchunk (chunk-coordinates &optional (path (world-path)))
  (let ((data
	 (sucle-serialize::retrieve-lisp-objects-from-file
	  (merge-pathnames (convert-object-to-filename
			    (chunk-coordinate-to-filename chunk-coordinates))
			   path))))
    (case (length data)
      (0
       ;;if data is nil, just load an empty chunk
       (world::with-chunk-key-coordinates (x y z) chunk-coordinates
	 (world::create-chunk x y z :type :empty)))

      (3 ;;FIXME::does this even work?
       (destructuring-bind (blocks light sky) data
	 (let ((len (length blocks)))
	   (let ((new (make-array len)))
	     (dotimes (i len)
	       (setf (aref new i)
		     (world::blockify (aref blocks i)  (aref light i) (aref sky i))))
	     (world::make-chunk-from-key-and-data chunk-coordinates new)))))
      (1
       (destructuring-bind (objdata) data
	 (world::make-chunk-from-key-and-data
	  chunk-coordinates
	  (coerce objdata '(simple-array t (*)))))))))

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
  (loop :for chunk :being :the :hash-values :of  world::*chunks* :do
     (chunk-save chunk :path path)))

#+nil
(defun load-world (path)
  ;;FIXME::don't load the entire world
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (loadchunk path (read-from-string (pathname-name file))))))

#+nil
(defun delete-garbage (&optional (path (world-path)))
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (let ((data
	     (retrieve-lisp-objects-from-file file)))
	(when (typep data '(cons array null))
	  (delete-file file))))))
