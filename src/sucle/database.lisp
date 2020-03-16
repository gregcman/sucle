(defpackage #:database
  (:use :cl :sqlite))
(in-package :database)
#+quicklisp
(progn (ql:quickload :sqlite)
       (ql:quickload :sxql))
(defparameter *database* (sucle-temp:path "data.db"))

(defvar *db* (connect *database*))

;;https://www.sqlite.org/affcase1.html
(defun create-table (&optional (name "documents"))
  (execute-non-query
    *db*
    (format nil
     "CREATE TABLE IF NOT EXISTS ~a (
    filename TEXT PRIMARY KEY,
    filesize BIGINT,
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

(defun error-scrub (sql-string)
  (let ((string (coerce sql-string 'string)))
    (assert (every 'alphanumericp sql-string) nil "~s is invalid, injectable SQL" string)
    string))

(defun delete-table (&optional (table "documents"))
  (execute-non-query
    *db*
    (format nil "DROP TABLE IF EXISTS ~a" (error-scrub table))))


(defun all-table-names ()
  (execute-to-list *db* "SELECT name FROM sqlite_master WHERE type='table'"))

(defun delete-all-tables ()
  (let ((tables (mapcar 'first (all-table-names))))
    (mapc 'delete-table tables)))

(defun check-table-name (&optional (name "documents"))
  (execute-to-list
   *db*
   "SELECT ? FROM sqlite_master WHERE type='table'"
   (error-scrub name)))
