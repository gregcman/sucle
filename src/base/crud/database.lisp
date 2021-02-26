(defpackage #:database
  (:use :cl :sqlite))
(in-package :database)
;;#+quicklisp
#+nil
(progn (ql:quickload :sqlite)
       ;;(ql:quickload :sxql)
       )
(defvar *database*
  ;;(sucle-temp:path "data.db")
  )
(defun new-connection ()
  (connect *database*))
(defvar *db* ;;(new-connection)
  )
(defparameter *documents* "documents")
(defun error-scrub (sql-string)
  (let ((string (coerce sql-string 'string)))
    (assert (every 'alphanumericp sql-string) nil "~s is invalid, injectable SQL" string)
    string))
;;;Handle pooling. Each thread gets its own handle.
(defvar *handles*)
(defun get-handle ()
  (or
   (lparallel.queue:try-pop-queue *handles*)
   (new-connection)))
(defun release-handle (handle)
  (lparallel.queue:push-queue handle *handles*))
;;FIXME::detect whether *db* is already bound
(defmacro with-open-database2 (&body body)
  (alexandria:with-gensyms (handle)
    `(flet ((fun ()
	      ,@body))
       (cond
	 ((boundp '*db*)
	  (fun))
	 (t (let* ((,handle (get-handle))
		   (*db* ,handle))
	      (unwind-protect (fun)
		(release-handle ,handle))))))))
(defparameter *write-write-lock* (bt:make-lock))
(defparameter *write-locks* (make-hash-table))
(defun get-write-lock (db)
  (bt:with-lock-held (*write-write-lock*)
    (let ((lock (gethash db *write-locks*)))
      (unless lock
	(let ((new-lock (bt:make-lock "db-write-lock")))
	  ;;FIXME::The *write-locks* table is not thread safe?
	  (setf (gethash db *write-locks*) new-lock
		lock new-lock)))
      lock)))
(defmacro with-locked-db ((db) &body body) 
  `(bordeaux-threads:with-lock-held ((get-write-lock ,db))
     ,@body))

;;https://www.sqlite.org/affcase1.html
(defun create-table (&optional (name *documents*))
  (execute-non-query
   *db*
   ;;Don't care about the filesize for now
    (format nil
     "CREATE TABLE IF NOT EXISTS ~a (
    filename TEXT PRIMARY KEY,
    content BLOB
)"
     (error-scrub name))
  ;;  name
    #+nil
    (sxql:yield
     (sxql:create-table ("documents" :if-not-exists t)
	 ((filename :type 'text
		    :primary-key t)
	  (filesize :type 'bigint)
	  (content :type 'blob))))
    #+nil
    "CREATE TABLE documents(
  filename TEXT PRIMARY KEY,  -- Name of file
  filesize BIGINT,            -- Size of file after decompression
  content BLOB                -- Compressed file content
);"))

(defun delete-table (&optional (table *documents*))
  (execute-non-query
    *db*
    (format nil "DROP TABLE IF EXISTS ~a" (error-scrub table))))

(defun add (filename data &optional (table *documents*))
  (with-locked-db (*database*)
    ;;https://github.com/TeMPOraL/cl-sqlite
    ;;[FIXME]sqlite concurrent insert bug. see cl-sqlite by temporal.
    (print (list filename))
    (cond
      ((does-file-exist filename)
       (format t "[updating ~a]" filename)
       ;;If it exists, update it
       (execute-non-query *db*
			  (format nil
				  "update ~a SET content = ? WHERE (filename LIKE ?)"
				  (error-scrub table))
			  data filename))
      (t
       (format t "[new row ~a]" filename)
       ;;If it doesn't, create a new row.
       (execute-non-query *db*
			  (format nil
				  "insert into ~a (filename, content) values (?, ?)"
				  (error-scrub table))
			  filename data)))
    (format t "[succesfully saved ~a]" filename)))

(defun does-file-exist (filename &optional (table *documents*))
  (/= 0
      (execute-single *db*
		      (format nil "SELECT EXISTS(SELECT 1 FROM ~a WHERE filename = ? LIMIT 1)" 
			      (error-scrub table))
		      filename)))

(defun all-data (&optional (table *documents*))
  (execute-to-list *db*
		   (format nil "SELECT * FROM ~a" 
			   (error-scrub table))))

(defun retreive (filename &optional (table *documents*))
  ;;(format t "~%existence-test ~s" filename)
  ;;[FIXME]only loading one chunk at a time?
  ;;bottleneck? bug with cl-sqlite library?
  (with-locked-db (*database*)
    (when (does-file-exist filename)
      ;;(format t "~%retrieving ~s" filename)
      (execute-single *db*
		      (format nil "select content from ~a where filename = ?" 
			      (error-scrub table))
		      filename))))

(defun delete-entry (filename &optional (table *documents*))
  (execute-non-query *db*
		     (format nil
			     "DELETE FROM ~a WHERE filename = ?"
			     (error-scrub table))
		     filename))

(defun all-table-names ()
  (execute-to-list *db* "SELECT name FROM sqlite_master WHERE type='table'"))

(defun delete-all-tables ()
  (let ((tables (mapcar 'first (all-table-names))))
    (mapc 'delete-table tables)))

(defun check-table-name (&optional (name *documents*))
  (execute-to-list
   *db*
   "SELECT ? FROM sqlite_master WHERE type='table'"
   (error-scrub name)))
