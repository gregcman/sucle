(defpackage #:sucle-serialize
  (:use :cl)
  (:shadow
   #:load)
  (:export
   ;;[FIXME] All of the functions in here can be potentially useful. 
   #:load
   #:save))
(in-package #:sucle-serialize)

(defun load (path)
  (retrieve-lisp-objects-from-file path))
(defun save (path thing &key (storage-type :conspack))
  (store-lisp-objects-to-file path thing :storage-type storage-type))

;;;Store a list of lisp objects as multiple lisp objects printed by
;;;the lisp printer in order
(progn
  (defun store-lisp-objects-to-file-lisp-reader (path things)
    (assert (typep things 'list))
    ;;Store a lisp object to a file by simple printing
    (with-open-file
	(stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (thing things)
	(safer-print thing stream))))
  (defun retrieve-lisp-objects-from-file-lisp-reader (path)
    (let ((file-existsp (probe-file path)))
      ;;if it doesn't exist, what's the point of loading it
      (when file-existsp
	(let ((things nil)
	      (eof (load-time-value (cons "eof" "token")))) 
	  (with-open-file (stream path :direction :input :if-does-not-exist nil)
	    (tagbody rep
	       (let ((thing (safer-read stream nil eof)))
		 (unless (eq thing eof)
		   (push thing things)
		   (go rep)))))
	  (nreverse things))))))

(defun safer-read (&optional (stream *standard-input*)
		     (eof-error-p nil) (eof-value nil) (recursive-p nil))
  (with-standard-io-syntax
    (let ((*package* (load-time-value (find-package :cl))))
      (let ((*read-eval* nil))
	(read stream eof-error-p eof-value recursive-p)))))
(defun safer-print (object &optional (stream *standard-output*))
  (with-standard-io-syntax
    (let ((*package* (load-time-value (find-package :cl))))
      (let ((*read-eval* nil)
	    (*print-circle* t)
	    (*print-readably* t))
	(print object stream)))))
 
;;File format
;;https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node191.html
;;Current file format is just printing lisp objects using the lisp printer
;;#) is first two bytes of a file if it's not just printed lisp objects

(defparameter *reading-error* "#)")
(defparameter *name-type-length* 16)
(defparameter *header-length* (+ (length *reading-error*) *name-type-length*))
(defun make-empty-header-string ()
  (make-string *name-type-length* :initial-element #\space))
(defun determine-file-type (path)
  (with-open-file (stream path :direction :input :if-does-not-exist :error)
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
			  (let ((array (make-empty-header-string)))
			   (read-sequence array stream :end *name-type-length*)
			    (return-from determine-file-type array))))))))))
    nil))
(defun insert-reading-error-bytes (stream name)
  (assert (= *name-type-length* (length name)))
  (loop :for char :across *reading-error* :do
     (write-byte (char-code char) stream))
  (loop :for char :across name :do
     (write-byte (char-code char) stream)))

(defun make-header-string (string)
  (let ((output (make-empty-header-string)))
    (read-sequence output (make-string-input-stream string))
    output))

(defun store-lisp-objects-to-file (path thing &key (storage-type :conspack))
  (ecase storage-type
    (:conspack (store-lisp-objects-to-file-zlib-conspack path thing))
    (:standard (store-lisp-objects-to-file-lisp-reader path thing))))

(defun skip-header-bytes (stream)
  (loop :repeat *header-length* :do (read-byte stream)))

(progn
  (defun decode-zlib-payload (data)
    (chipz:decompress nil 'chipz:zlib data))
  (defun encode-zlib-payload (data)
    (salza2:compress-data
     data
     'salza2:zlib-compressor)))
(progn
  (defun encode-conspack-payload (things)
    (conspack:tracking-refs ()
      (conspack:encode things)))
  (defun decode-conspack-payload (data)
    (conspack:tracking-refs ()
      (conspack:decode data))))
(progn
  (defun decode-zlib-conspack-payload (data)
    (decode-conspack-payload (decode-zlib-payload data)))
  (defun encode-zlib-conspack-payload (things)
    (encode-zlib-payload (encode-conspack-payload things))))

(defparameter *zlib-conspack-header* (make-header-string "zlib-conspack"))
(defun store-lisp-objects-to-file-zlib-conspack (path things)
  (assert (typep things 'list))
  (with-open-file (stream path :direction :output :if-exists :supersede
			  :element-type '(unsigned-byte 8))
    (insert-reading-error-bytes stream *zlib-conspack-header*)
    (write-sequence (encode-zlib-conspack-payload things) stream)))

(defun retrieve-lisp-objects-from-file-zlib-conspack (path)
  ;;ripped from conspack:decode-file
  (with-open-file
      (stream path :direction :input :element-type '(unsigned-byte 8)
	      :if-does-not-exist :error)
    (skip-header-bytes stream)
    (let* ((remaining-bytes (the fixnum (- (file-length stream) *header-length*)))
	   (array (make-array remaining-bytes :element-type '(unsigned-byte 8))))
      (read-sequence array stream)
      (decode-zlib-conspack-payload array))))


(defun retrieve-lisp-objects-from-file (path)
  ;;A file contains 0 or more lisp objects.
  ;;Thus, saving an individual object does not work, you have to save a list of objects
  (let ((file-existsp (probe-file path)))
    ;;if it doesn't exist, what's the point of loading it
    (when file-existsp
      (let ((type (determine-file-type path)))
	(cond
	  ((null type)
	   (retrieve-lisp-objects-from-file-lisp-reader path))
	  ((string= *zlib-conspack-header* type)
	   (retrieve-lisp-objects-from-file-zlib-conspack path))
	  (t (error "what is this? ~s" type)))))))

#+nil ;;for converting between formats, read, then write a known format
(defun convert-to-conspack (&optional (path (world-path)))
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (time
       (let ((data (retrieve-lisp-objects-from-file file)))
	 (store-lisp-objects-to-file file data))))))
