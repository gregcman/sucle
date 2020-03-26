(defpackage #:crud
  (:use :cl)
  (:export
   #:crud-create
   #:crud-read
   #:crud-update
   #:crud-delete

   #:use-crud-from-path
   #:detect-crud-from-path
   #:make-crud-from-path

   #:call-with-transaction))
(in-package #:crud)
;;CRUD implementation for map from lisp_obj -> lisp_obj
;;create, read, update, delete
;;- slqlite database
;;- pile of files
(defclass crud () ((path :initarg :path :accessor path)))
(defparameter *implementation* nil)
(defgeneric crud_ensure (crud))
(defgeneric crud_create (lisp-object data crud))
(defgeneric crud_read (lisp-object crud))
(defgeneric crud_update (lisp-object data crud))
(defgeneric crud_delete (lisp-object crud))
(defun crud-ensure ()
  (crud_ensure *implementation*))
(defun crud-create (lisp-object data)
  (crud_create lisp-object data *implementation*))
(defun crud-read (lisp-object)
  (crud_read lisp-object *implementation*))
(defun crud-update (lisp-object data)
  (crud_update lisp-object data *implementation*))
(defun crud-delete (lisp-object)
  (crud_delete lisp-object *implementation*))

;;world loading code below?
(defun convert-object-to-filename (&optional (obj '(0 1 2 'obj :cl)))
  (with-output-to-string (str)
    ;;FIXME::what about circular data structures?
    (sucle-serialize::safer-write obj str)))

(defclass crud-sqlite (crud) ())
(defmethod crud_ensure ((impl crud-sqlite))
  (let ((database::*database* (path impl)))
    (crud_ensure_sqlite)))
(defmethod crud_create (name data (impl crud-sqlite))
  (let ((database::*database* (path impl)))
    (crud_create_sqlite name data)))
(defmethod crud_read (name (impl crud-sqlite))
  (let ((database::*database* (path impl)))
    (crud_read_sqlite name)))
(defmethod crud_update (name data (impl crud-sqlite))
  (let ((database::*database* (path impl)))
    (crud_update_sqlite name data)))
(defmethod crud_delete (name (impl crud-sqlite))
  (let ((database::*database* (path impl)))
    (crud_delete_sqlite name)))
;;;;SQLITE + sucle-serialize
(defmacro with-open-sqlite-database (&body body)
  `(let ((database::*database* (path *implementation*)))
     (database::with-open-database2 ,@body)))
(defun crud_ensure_sqlite ()
  (database::clear-handles)
  (with-open-sqlite-database
    (database::create-table)))
(defun crud_create_sqlite (lisp-object data)
  ;;FIXME:update creates a row regardless, so update
  ;;is the real create.
  (crud_update_sqlite lisp-object data))
(defun crud_read_sqlite (lisp-object)
  (let* ((file-name (convert-object-to-filename lisp-object))
	 (stuff
	  (with-open-sqlite-database
	    (database::retreive file-name))))
    (when stuff
      (sucle-serialize::decode-zlib-conspack-payload stuff))))
(defun crud_update_sqlite (lisp-object data)
  (with-open-sqlite-database
    (database::add
     (convert-object-to-filename lisp-object)
     (sucle-serialize::encode-zlib-conspack-payload data))))
(defun crud_delete_sqlite (lisp-object)
  (with-open-sqlite-database
    (database::delete-entry (convert-object-to-filename lisp-object))))
;;;;sucle-serialize
(defclass crud-file-pile (crud) ())
(defparameter *path* nil)
(defmethod crud_ensure ((impl crud-file-pile))
  (let ((*path* (path impl)))
    (ensure-directories-exist
     (uiop:pathname-directory-pathname *path*))))
(defmethod crud_create (name data (impl crud-file-pile))
  (let ((*path* (path impl)))
    (crud_create_file-pile name data)))
(defmethod crud_read (name (impl crud-file-pile))
  (let ((*path* (path impl)))
    (crud_read_file-pile name)))
(defmethod crud_update (name data (impl crud-file-pile))
  (let ((*path* (path impl)))
    (crud_update_file-pile name data)))
(defmethod crud_delete (name (impl crud-file-pile))
  (let ((*path* (path impl)))
    (crud_delete_file-pile name)))
;;[FIXME]:can possibly create filenames that are illegal.
;;use base64 instead? how to mix the two?
;;
(defparameter *base-64-string*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
(defun string-base64 (string)
  (let ((base64::*base64* *base-64-string*))
    (base64:base64-encode string)))
(defun base64-string (string)
  (let ((base64::*base64* *base-64-string*))
    (base64:base64-decode string)))
(defun commentify (&optional (string "test"))
  (concatenate 'string ";" string))
(defun un-commentify (&optional (string ";test"))
  (subseq string 1 (length string)))
(defparameter *junk* '(4/5 "foo/bar.../baz" 3))
;;FIXME: 'check-' functions get highlighted red in emacs
(defun %check-safe-string
    (lispobj
     &optional
       (directory "")
       &aux
       (pathname (convert-object-to-filename lispobj))
       (path (merge-pathnames pathname directory))
       (base64-path (merge-pathnames (commentify (string-base64 pathname))
				     directory)))
  ;;FIXME::This probably won't work on windows.
  ;;https://stackoverflow.com/questions/4814040/allowed-characters-in-filename
  
  ;;Check that they both point to files in same directory
  ;;If they are, then it is safe
  (if (equal (pathname-directory base64-path)
	     (pathname-directory path))
      (values path base64-path)
      (values nil base64-path)))

(defun crud_create_file-pile (lisp-object data)
  (crud_update_file-pile lisp-object data))
(defun crud_read_file-pile (lisp-object &aux (path *path*))
  (multiple-value-bind (file-path base64) (%check-safe-string lisp-object path)
    ;;if both are valid, prefer the base64 one.
    ;;if only base64 is valid, just use that.  
    (sucle-serialize:load
     (cond (file-path
	    (if (probe-file base64)
		base64
		file-path))
	   (t base64)))))
(defun crud_update_file-pile (lisp-object data &aux (path *path*))
  (ensure-directories-exist
   (uiop:pathname-directory-pathname path))
  ;;Don't use the old format, just the base64 one.
  (multiple-value-bind (file-path base64) (%check-safe-string lisp-object path)
    (declare (ignore file-path))
    (sucle-serialize:save base64 data)))
(defun crud_delete_file-pile (lisp-object &aux (path *path*))
  ;;Delete the base64 version, and the regular version, if they exist.
  ;;FIXME:reads from the disk, slow? FIXME: use OPTIMIZE rather than FIXME?
  (multiple-value-bind (file-path base64) (%check-safe-string lisp-object path)
    (when (and file-path (probe-file file-path))
      (delete-file file-path))
    (when (probe-file base64)
      (delete-file base64))))

#+nil
"U28/PHA+VGhpcyA0LCA1LCA2LCA3LCA4LCA5LCB6LCB7LCB8LCB9IHRlc3RzIEJhc2U2NCBlbmNv
ZGVyLiBTaG93IG1lOiBALCBBLCBCLCBDLCBELCBFLCBGLCBHLCBILCBJLCBKLCBLLCBMLCBNLCBO
LCBPLCBQLCBRLCBSLCBTLCBULCBVLCBWLCBXLCBYLCBZLCBaLCBbLCBcLCBdLCBeLCBfLCBgLCBh
LCBiLCBjLCBkLCBlLCBmLCBnLCBoLCBpLCBqLCBrLCBsLCBtLCBuLCBvLCBwLCBxLCByLCBzLg=="

;;;
(defun detect-crud-from-path (path)
  (if (uiop:file-pathname-p path)
      ;;A sqlite database is a singular file.
      'crud-sqlite
      'crud-file-pile))
(defun make-crud-from-path (path)
  (make-instance (detect-crud-from-path path) :path path))
(defun use-crud-from-path (path)
  (setf *implementation* (make-crud-from-path path))
  (crud-ensure))

(defvar *transacation-happening*)
(defun call-with-transaction (fun)
  (flet ((thing () (funcall fun)))
    (cond ((typep crud::*implementation* 'crud::crud-sqlite)
	   ;;FIXME::this references sqlite?
	   (with-open-sqlite-database
	     (if (boundp '*transacation-happening*)
		 (thing)
		 (let ((*transacation-happening* t))
		   (sqlite:with-transaction database::*db*
		     (thing))))))
	  (t (thing)))))
