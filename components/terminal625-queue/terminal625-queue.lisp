(defpackage #:queue
  (:use #:cl)
  (:nicknames #:q)
  (:export 
   #:get-queue
   #:q-push
   #:q-pop
   #:make-uniq-q
   #:uniq-push
   #:uniq-pop
   #:kv-uniq-push
   #:kv-uniq-pop
   #:uniq-length
   #:clrq
   #:clruniq))

(in-package :q)

;;a basic fifo queue first in first out
(defstruct queue
  head
  last
  (len 0))

(defun get-queue ()
  (let ((tail (cons nil nil)))
    (let ((head (cons nil tail)))
      (make-queue
       :head head
       :last tail))))

(defun q-push (item queue)
  (let ((new-end (cons nil nil)))
    (setf (car (queue-last queue)) item)
    (setf (cdr (queue-last queue)) new-end)
    (setf (queue-last queue) new-end)
    (incf (queue-len queue))))

(defun q-pop (queue)
  (if (eq (cdr (queue-head queue)) (queue-last queue))
      (values nil nil)
      (progn
	(decf (queue-len queue))
	(values (pop (cdr (queue-head queue))) t))))

;;a fifo queue with the added restriction that members must be unique
(defstruct uniq-q
  (q (get-queue))
  (hash (make-hash-table
	 :test
	 #+sbcl 'eq
	 #-sbcl 'eql)))

(defun uniq-push (item uniq)
  (let ((hash (uniq-q-hash uniq)))
    (multiple-value-bind (fuck-me exists?) (gethash item hash)
      (declare (ignore fuck-me))
      (unless exists?
	  (setf (gethash item hash) t)
	  (q-push item (uniq-q-q uniq))))))

(defun uniq-pop (uniq)
  (multiple-value-bind (item yup) (q-pop (uniq-q-q uniq))
    (if yup
	(remhash item (uniq-q-hash uniq)))
    (values item yup)))


;;if you want to push key/value pairs
(defun kv-uniq-push (key value uniq)
  (let ((hash (uniq-q-hash uniq)))
    (multiple-value-bind (old-value exists?) (gethash key hash)
      (declare (ignorable old-value))
      (unless exists?
	  (setf (gethash key hash) value)
	  (q-push key (uniq-q-q uniq))))))

(defun kv-uniq-pop (uniq)
  (multiple-value-bind (item yup) (q-pop (uniq-q-q uniq))
    (let ((hash (uniq-q-hash uniq)))
      (let ((value (gethash item hash)))
	(if yup
	    (remhash item hash))
	(values item value yup)))))

(defun uniq-length (uniq)
  (queue-len (uniq-q-q uniq)))

(defun clrq (q)
  (setf (queue-len q) 0)
  (let ((tail (cons nil nil)))
    (let ((head (cons nil tail)))
      (setf (queue-head q) head)
      (setf (queue-last q) tail))))

(defun clruniq (uniq)
  (clrq (uniq-q-q uniq))
  (clrhash (uniq-q-hash uniq)))
