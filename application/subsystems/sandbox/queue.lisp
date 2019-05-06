(defpackage #:queue
  (:use #:cl)
  (:export 
   #:make-uniq-q
   #:uniq-push
   #:uniq-pop
   #:kv-uniq-push
   #:kv-uniq-pop
   #:uniq-length
   #:clrq
   #:clruniq))

(in-package :queue)

;;queue taken from https://rosettacode.org/wiki/Queue/Definition#Common_Lisp
;;but modified
(defstruct (queue (:constructor %make-queue))
  (items '() :type list)
  (tail '() :type list)
  (len 0))

(defun get-queue ()
  "Returns an empty queue."
  (%make-queue))

(defun queue-empty-p (queue)
  "Returns true if the queue is empty."
  (endp (queue-items queue)))

(defun enqueue (item queue)
  "Enqueue item in queue. Returns the queue."
  (prog1 queue
    (incf (queue-len queue))
    (let ((new-item (list item)))
      (if (queue-empty-p queue)
	  (setf (queue-items queue) new-item
		(queue-tail queue) new-item)
	  (setf (cdr (queue-tail queue)) new-item
		(queue-tail queue) new-item)))))

(defun dequeue (queue)
  ;;"Dequeues an item from queue. Signals an error if queue is empty."
  (if (queue-empty-p queue)
      (values nil nil) ;;(error "Cannot dequeue from empty queue.")
      (progn
	(decf (queue-len queue))
	(values (pop (queue-items queue)) t))))

;;a fifo queue with the added restriction that members must be unique
(defstruct uniq-q
  (q (get-queue))
  (hash (make-hash-table
	 :test
	 'eq
	 )))

(defun uniq-push (item uniq)
  (let ((hash (uniq-q-hash uniq)))
    (multiple-value-bind (fuck-me exists?) (gethash item hash)
      (declare (ignore fuck-me))
      (unless exists?
	(setf (gethash item hash) t)
	(enqueue item (uniq-q-q uniq))))))

(defun uniq-pop (uniq)
  (multiple-value-bind (item yup) (dequeue (uniq-q-q uniq))
    (when yup
      (remhash item (uniq-q-hash uniq)))
    (values item yup)))


;;if you want to push key/value pairs
#+nil ;;FIXME:: these functions are used for block light code? refactor?
(defun kv-uniq-push (key value uniq)
  (let ((hash (uniq-q-hash uniq)))
    (multiple-value-bind (old-value exists?) (gethash key hash)
      (declare (ignorable old-value))
      (unless exists?
	(setf (gethash key hash) value)
	(enqueue key (uniq-q-q uniq))))
    uniq))
#+nil
(defun kv-uniq-pop (uniq)
  (multiple-value-bind (item yup) (dequeue (uniq-q-q uniq))
    (let ((hash (uniq-q-hash uniq)))
      (multiple-value-bind (value exists-p) (gethash item hash)
	(when exists-p
	  (remhash item hash))
	(values item value yup)))))

(defun uniq-length (uniq)
  (queue-len (uniq-q-q uniq)))

(defun clrq (q)
  (setf (queue-len q) 0)
  (setf (queue-items q) nil)
  (setf (queue-tail q) nil))

(defun clruniq (uniq)
  (clrq (uniq-q-q uniq))
  (clrhash (uniq-q-hash uniq)))

(defun %set-queue-internals (list queue)
  (setf (queue-items queue) list
	(queue-tail queue) (last list)
	(queue-len queue) (list-length list)))

(defun re-sync-uniq-q (uniq-q)
  (clrhash (uniq-q-hash uniq-q))
  (let ((hash (uniq-q-hash uniq-q)))
    (dolist (item (queue-items (uniq-q-q uniq-q)))
      (setf (gethash item hash) t))))

(defun sort-queue (queue sort-fun)
  ;;assume that sort-fun does not remove or add any objects, otherwise
  ;;this would screw up the uniq-q
  (let ((actual-queue
	 (etypecase queue
	   (queue queue)
	   (uniq-q (uniq-q-q queue)))))
    (let ((items (queue-items actual-queue)))
      (when items
	(%set-queue-internals (funcall sort-fun items) actual-queue)
	(when (typep queue 'uniq-q)
	  (re-sync-uniq-q queue))))))
