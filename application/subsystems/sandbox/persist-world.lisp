(in-package #:sandbox)

;;;Store a list of lisp objects as multiple lisp objects printed by
;;;the lisp printer in order
(progn
  (defun store-lisp-objects-to-file-lisp-reader (path things)
    (assert (typep things 'list))
    ;;Store a lisp object to a file by simple printing
    (with-standard-io-syntax
      (let ((*read-eval* nil)
	    (*print-circle* t))
	(with-open-file
	    (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
	  (dolist (thing things)
	    ;;FIXME::maybe use prin1, same, but saves a newline?
	    (print thing stream))))))
  (defun retrieve-lisp-objects-from-file-lisp-reader (path)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
	(let ((file-existsp (probe-file path)))
	  ;;if it doesn't exist, what's the point of loading it
	  (when file-existsp
	    (let ((things nil)
		  (eof (load-time-value (cons "eof" "token")))) 
	      (with-open-file (stream path :direction :input :if-does-not-exist nil)
		(tagbody rep
		   (let ((thing (read stream nil eof)))
		     (unless (eq thing eof)
		       (push thing things)
		       (go rep)))))
	      (nreverse things))))))))

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

(defun savechunk (position &optional (path (world-path)))
  ;;FIXME::undocumented swizzling and multiplication by 16, as well as loadchunk
  (let ((filename (convert-object-to-filename (chunk-coordinate-to-filename position))))
    ;;(format t "~%Saving chunk ~a" filename)
    (store-lisp-objects-to-file
     (merge-pathnames
      filename
      path)
     (list
      (world::chunk-data
       (world::obtain-chunk-from-chunk-key position nil))))))

(defun loadchunk (path filename-position-list)
  (let ((position (filename-to-chunk-coordinate filename-position-list)))  
    (let ((data
	   (retrieve-lisp-object-from-file
	    (merge-pathnames (convert-object-to-filename filename-position-list) path))))
      (case (length data)
	(0
	 ;;if data is nil, just load an empty chunk
	 (world::with-chunk-key-coordinates (x y z) position
	   (world::set-chunk-at
	    position
	    (world::create-chunk x y z :type :empty)))
	 ;;return :EMPTY to signify that
	 ;;an empty chunk has been loaded
	 :empty)
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
	     (myload file)))
	(when (typep data '(cons array null))
	  (delete-file file))))))
 
;;File format
;;https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node191.html
;;Current file format is just printing lisp objects using the lisp printer
;;#) is first two bytes of a file if it's not just printed lisp objects

(defparameter *reading-error* "#)")
(defun determine-file-type (path)
  (with-open-file (stream path :direction :input :if-does-not-exist :error)
    (unless (file-position stream 0)
      (error "why can't the file-stream be repositioned?"))
    (let ((file-type nil))
      (let ((eof (load-time-value (cons "eof" "token"))))
	(let ((char1 (read-char stream nil eof)))
	  (cond ((eq eof char1) #|EOF|#)
		(t
		 (let ((char2 (read-char stream nil eof)))
		   (cond ((eq eof char2)  #|EOF|#)
			 (t
			  (when (and (char= char1
					    (elt *reading-error* 0))
				     (char= char2
					    (elt *reading-error* 1)))
			    (setf file-type :not-printed-objects)))))))))
      file-type)))
(defun insert-reading-error-bytes (stream)
  (loop :for char :across *reading-error* :do
     (write-byte (char-code char) stream)))

(defun store-lisp-objects-to-file (path thing &key (storage-type :conspack))
  (ecase storage-type
    (:conspack (store-lisp-objects-to-file-conspack path thing))
    (:standard (store-lisp-objects-to-file-lisp-reader path thing))))

(defun store-lisp-objects-to-file-conspack (path things)
  (assert (typep things 'list))
  (with-open-file (stream path :direction :output :if-exists :supersede
			  :element-type '(unsigned-byte 8))
    (insert-reading-error-bytes stream)
    (conspack::tracking-refs ()
      (dolist (thing things)
	(conspack::encode thing :stream stream)))))

(defun retrieve-lisp-objects-from-file-conspack (path)
  ;;ripped from conspack::decode-file
  (with-open-file
      (stream path :direction :input :element-type '(unsigned-byte 8)
	      :if-does-not-exist :error)
    (loop :repeat (length *reading-error*) :do (read-byte stream))
    (let (eof)
      (conspack::tracking-refs ()
        (loop as object = (handler-case
                              (conspack::decode-stream stream)
                            (conspack::end-of-file () (setf eof t) (values)))
              until eof
              collect object)))))

(defun retrieve-lisp-object-from-file (path)
  ;;A file contains 0 or more lisp objects.
  ;;Thus, saving an individual object does not work, you have to save a list of objects
  (let ((file-existsp (probe-file path)))
    ;;if it doesn't exist, what's the point of loading it
    (when file-existsp
      (let ((type (determine-file-type path)))
	(case type
	  ((:not-lisp-objects) (retrieve-lisp-objects-from-file-conspack path))
	  ((nil) (retrieve-lisp-objects-from-file-lisp-reader path)))))))
