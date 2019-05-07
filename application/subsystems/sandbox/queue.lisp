(defpackage #:queue
  (:use #:cl)
  (:export 
   #:make-uniq-q
   #:uniq-push
   #:uniq-pop
   ;;#:kv-uniq-push
   ;;#:kv-uniq-pop
   ;;#:uniq-length
   ;;#:clrq
   ;;#:clruniq
   ))

(in-package :queue)

;;queue taken from https://rosettacode.org/wiki/Queue/Definition#Common_Lisp
;;but modified
#+nil
(defstruct (queue (:constructor %make-queue))
  (items '() :type list)
  (tail '() :type list)
  (len 0))
#+nil
(defun get-queue ()
  "Returns an empty queue."
  (%make-queue))
#+nil
(defun queue-empty-p (queue)
  "Returns true if the queue is empty."
  (endp (queue-items queue)))
#+nil
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
#+nil
(defun dequeue (queue)
  ;;"Dequeues an item from queue. Signals an error if queue is empty."
  (if (queue-empty-p queue)
      (values nil nil) ;;(error "Cannot dequeue from empty queue.")
      (progn
	(decf (queue-len queue))
	(values (pop (queue-items queue)) t))))

;;a fifo queue with the added restriction that members must be unique
(defstruct uniq-q
  (q (lparallel.queue:make-queue))
  (hash (make-hash-table
	 :test
	 'equal
	 )))

(defun uniq-push (item uniq)
  (lparallel.queue:with-locked-queue (uniq-q-q uniq)
    (let ((hash (uniq-q-hash uniq)))
      (multiple-value-bind (fuck-me exists?) (gethash item hash)
	(declare (ignore fuck-me))
	(unless exists?
	  (setf (gethash item hash) t)
	  (lparallel.queue:push-queue/no-lock item (uniq-q-q uniq)))))))

(defun uniq-pop (uniq)
  (lparallel.queue:with-locked-queue (uniq-q-q uniq)
    (multiple-value-bind (item yup) (lparallel.queue:try-pop-queue/no-lock (uniq-q-q uniq))
      (when yup
	(remhash item (uniq-q-hash uniq)))
      (values item yup))))


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
#+nil
(defun uniq-length (uniq)
  (queue-len (uniq-q-q uniq)))
#+nil
(defun clrq (q)
  (setf (queue-len q) 0)
  (setf (queue-items q) nil)
  (setf (queue-tail q) nil))
#+nil
(defun clruniq (uniq)
  (clrq (uniq-q-q uniq))
  (clrhash (uniq-q-hash uniq)))

(defun %set-queue-internals (list queue)
  (setf (lparallel.raw-queue::head queue) list
	(lparallel.raw-queue::tail queue) (last list)))

(defun re-sync-uniq-q (uniq-q)
  (clrhash (uniq-q-hash uniq-q))
  (let ((hash (uniq-q-hash uniq-q))
	(raw-queue (lparallel.cons-queue::impl (uniq-q-q uniq-q))))
    (dolist (item (lparallel.raw-queue::head raw-queue))
      (setf (gethash item hash) t))))

(defun sort-queue (queue sort-fun)
  ;;assume that sort-fun does not remove or add any objects, otherwise
  ;;this would screw up the uniq-q
  (lparallel.queue:with-locked-queue (uniq-q-q queue)
    (let ((actual-queue (lparallel.cons-queue::impl (uniq-q-q queue))))
      (let ((items (lparallel.raw-queue::head actual-queue)))
	(when items
	  (%set-queue-internals (funcall sort-fun items) actual-queue)
	  (when (typep queue 'uniq-q)
	    (re-sync-uniq-q queue)))))))
