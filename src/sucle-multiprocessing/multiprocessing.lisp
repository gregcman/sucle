(defpackage #:sucle-multiprocessing
  (:use :cl)
  (:nicknames :sucle-mp)
  (:export
   #:submit-unique-task
   #:remove-unique-task-key
   #:job-task-data
   #:job-task-return-values
   #:*current-job-task*
   #:with-initialize-multiprocessing
   #:with-kernel
   #:do-queue-iterator
   #:submit
   #:flush-job-tasks))
(in-package #:sucle-mp)

(defun quickload ()
  #+quicklisp
  (progn
    (ql:quickload :lparallel)
    (ql:quickload :cl-cpus)
    (ql:quickload :uncommon-lisp)
    (ql:quickload :utility)))

(defparameter *lparallel-kernel* nil)
(defmacro with-kernel (&body body)
  `(let ((lparallel:*kernel* *lparallel-kernel*))
     ,@body))
(defparameter *channel* nil)
;;Before ending the kernel, wait until *unkillable* is set to 0
(struct-to-clos:struct->class 
 (defstruct unkillable
   (count 0)
   (lock (bordeaux-threads:make-recursive-lock))))
(defparameter *unkillable* (make-unkillable))
(defmacro with-unkillable ((job-task) &body body)
  `(let ((unkillable (job-task-unkillable ,job-task)))
     (when (not (null unkillable))
       (bordeaux-threads:with-recursive-lock-held ((unkillable-lock unkillable))
	 ,@body))))
(defun wait-on-unkillables ()
  (loop :named out :do
     (when (zerop (unkillable-count *unkillable*))
       (return-from out))
     (bordeaux-threads:thread-yield)))
;;[FIXME]should the finished-task-queue be a global variable?
(defparameter *finished-task-queue* (lparallel.queue:make-queue))
(defparameter *shutting-down-p* nil)
(defun set-dynamic-variables ()
  (setf *lparallel-kernel* (lparallel:make-kernel (cpus:get-number-of-processors)))
  (setf *unique-tasks* (make-hash-table :test 'eq))
  (setf *finished-task-queue* (lparallel.queue:make-queue))
  (with-kernel
    (setf *channel* (lparallel:make-channel))))
(defun reset ()
  (lparallel:end-kernel)
  (set-dynamic-variables)
  (setf lparallel:*kernel* *lparallel-kernel*))

(defmacro with-initialize-multiprocessing (&body body)
  `(let ((*lparallel-kernel* nil))
     (setf *shutting-down-p* nil)
     (set-dynamic-variables)
     (with-kernel
       (unwind-protect (progn ,@body)
	 (setf *shutting-down-p* t)
	 (when *lparallel-kernel*
	   (wait-on-unkillables)
	   (lparallel:end-kernel))))))

(defparameter *print-errors* t)
(defmacro debugging (&body body)
  `(when *print-errors*
     ,@body))

(struct-to-clos:struct->class 
 (defstruct job-task
   ;;the thread it runs in 
   (thread nil)
   ;;status is one of :pending, :started, :complete, :aborted, :killed
   ;;:pending -> created, not run yet
   ;;:running = in the process of running
   ;;:aborted = aborted from the running function
   ;;:killed = killed from a separate thread
   ;;:complete = finished normally
   (status nil)
   ;;Turn on the lock with 'with-locked-job-task while modifiying this task's values
   (lock (bordeaux-threads:make-recursive-lock))
   ;;The values captured by the submit-ed function. multiple-values-list
   (return-values nil)
   ;;return-status is an error object when status is :aborted,
   ;;when status is :killed, it is nil
   ;;otherwise it is t
   (return-status t)
   ;;When the task is :pending, or :started, its nil
   ;;when the task is :aborted, :killed, :complete, its t
   (finished nil)
   ;;Set to either nil, or a function-designator of type (function (job-task))
   ;;Called when read by finished-job-tasks
   (callback nil)
   ;;do not end the kernel before finishing a task that is unkillable.
   ;;make sure to finished the task, no matter if it is pending or started,
   ;;it will be either an 'unkillable' object, or nil
   (unkillable nil)
   ;;user-defined data
   (data nil)))
(defmacro with-locked-job-task ((job-task) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((job-task-lock ,job-task))
     ,@body))
(defmethod print-object ((object job-task) stream)
  (format stream "<job-task ~s ~s ~s ~s>"
	  (if (job-task-finished object)
	      :dead
	      :live)
	  (job-task-status object)
	  (job-task-return-values object)
	  (job-task-return-status object)))
(defun init-job-task (job-task)
  ;;return t if correctly initialized
  ;;otherwise return nil. Should return nil if the task was killed beforehand
  (with-locked-job-task (job-task)
    (let ((old-status (job-task-status job-task)))
      (cond ((eq old-status :pending)
	     (setf (job-task-status job-task) :running)
	     (setf (job-task-thread job-task) (bordeaux-threads:current-thread))
	     t)
	    (t nil)))))
(defun complete-job-task (job-task returned-values)
  (with-locked-job-task (job-task)
    (setf (job-task-return-values job-task) returned-values)
    (setf (job-task-status job-task) :complete)
    (setf (job-task-thread job-task) nil)
    (setf (job-task-finished job-task) t)
    (maybe-decrement-unkillable job-task)))
(defun abort-job-task (job-task error)
  (with-locked-job-task (job-task)
    (setf (job-task-return-status job-task) error)
    (setf (job-task-status job-task) :aborted)
    (setf (job-task-thread job-task) nil)
    (setf (job-task-finished job-task) t)
    (maybe-decrement-unkillable job-task)))

(defun maybe-decrement-unkillable (job-task)
  (with-unkillable (job-task) (decf (unkillable-count (job-task-unkillable job-task)))))
(defun maybe-increment-unkillable (job-task)
  (with-unkillable (job-task) (incf (unkillable-count (job-task-unkillable job-task)))))

(defun kill-job-task (job-task)
  (with-locked-job-task (job-task)
    (let ((status (job-task-status job-task)))
      (when
	  ;;only kill job-tasks that are in the middle of processing, or pending
	  (member status '(:pending :running))
	(let (;;set-job-task-vars removes the thread from the object, so save it to 'thread
	      (thread (job-task-thread job-task)))
	  (setf (job-task-thread job-task) nil)
	  (setf (job-task-status job-task) :killed)
	  (setf (job-task-return-status job-task) nil)
	  (setf (job-task-finished job-task) t)
	  (maybe-decrement-unkillable job-task)
	  ;;[FIXME]use bordeaux threads and kill the thread directly or use lparallel:kill-tasks?
	  ;;(lparallel:kill-tasks job-task)
	  (when (eq status :running)
	    ;;kill a task that has been started
	    (bordeaux-threads:destroy-thread thread)))
	;;we push to the *finished-task-queue*, because otherwise lparallel does not
	;;let us know about killed task objects
	;;[FIXME]This means tasks killed with kill-tasks or bordeaux-threads:destroy-thread
	;;will not be registered correctly, the job-task object will still say :pending/:running
	;;and contain the dead thread.
	(lparallel.queue:push-queue/no-lock job-task *finished-task-queue*))))
  job-task)

(defparameter *paused* nil)
(defparameter *current-job-task* nil)
(defun job-task-function (job-task fun args)
  (loop :while (and *paused* (not *shutting-down-p*)) :do (bordeaux-threads:thread-yield))
  (let ((*current-job-task* job-task))
    (when (init-job-task job-task)
      (handler-case  
	  (progn	  
	    (complete-job-task job-task (multiple-value-list (apply fun args))))
	(error (c)
	  ;;handle regular errors from function
	  (declare (ignorable c))
	  (debugging (print c))
	  (abort-job-task job-task c)))))
  job-task)
(defmacro submit-body ((&rest rest &key &allow-other-keys) &body body)
  `(submit (lambda () ,@body) ,@rest))

(defun submit (fun &key callback args data unkillable)
  (let ((new-job-task (make-job-task :status :pending :callback callback :data data
				     :unkillable (if unkillable
						     *unkillable*
						     nil))))
    (maybe-increment-unkillable new-job-task)
    (lparallel:submit-task
     *channel*
     'job-task-function
     new-job-task
     fun
     args)
    new-job-task))

(defun %get-values ()
  ;;receives values from the *channel* which can be either task objects
  ;;or processing errors. Filter out the processing errors and send the
  ;;tasks to the *finished-task-queue*
  (let ((queue *finished-task-queue*))
    (lparallel.queue:with-locked-queue queue
      (loop :named outer-loop :do
	 (handler-case 
	     (loop
		(multiple-value-bind (value exist-p)
		    (lparallel:try-receive-result *channel*)
		  (unless exist-p
		    (return-from outer-loop))
		  (when (typep value 'job-task))
		  (lparallel.queue:push-queue/no-lock value queue)))
	   ;;[FIXME]allow errors to pass through?
	   (error (c)
	     (declare (ignorable c))
	     (debugging (print c))
	     ))))))

(defmacro do-queue-iterator ((next queue) &body body)
  ;;iterate through the values in the lparallel queue, with
  ;;(next) getting the new values, or returning if there are none
  (utility:with-gensyms (var loop exist-p)
    (utility:once-only (queue)
      `(block ,loop
	 (flet ((,next ()
		  (lparallel.queue:with-locked-queue ,queue	 
		    (multiple-value-bind (,var ,exist-p)
			(lparallel.queue:try-pop-queue/no-lock ,queue)
		      (unless ,exist-p
			(return-from ,loop))
		      ,var))))	     
	   ,@body)))))
(defmacro do-queue ((var queue) &body body)
  (utility:with-gensyms (next)
    `(do-queue-iterator (,next ,queue)
       (loop
	  (let ((,var (,next)))
	    ,@body)))))

(defun get-values (&optional (fun 'print))
  (%get-values)
  (do-queue (value *finished-task-queue*)
    (funcall fun value)))

(defun flush-job-tasks (&optional fun)
  (get-values (lambda (job-task)
		(when fun
		  (funcall fun job-task))
		(let ((callback (job-task-callback job-task)))
		  (when callback
		    (funcall callback job-task))))))

(defparameter *unique-tasks* (make-hash-table :test 'equal))
(defparameter *unique-tasks-lock* (bordeaux-threads:make-recursive-lock))
(defmacro submit-unique-task (key (fun &rest rest &key &allow-other-keys) &body body-if)
  ;;fun, callback, args are only evaluated if
  ;;the task is non-existent
  (utility:once-only (key)
    (utility:with-gensyms (value existsp job-task)
      `(bordeaux-threads:with-recursive-lock-held (*unique-tasks-lock*)
	 (multiple-value-bind (,value ,existsp) (gethash ,key *unique-tasks*)
	   (declare (ignorable ,value))
	   (if (not ,existsp)
	       (progn
		 ,@body-if
		 (let ((,job-task (submit ,fun ,@rest)))
		   (setf (gethash ,key *unique-tasks*) ,job-task)
		   (values ,job-task t)))
	       (values nil nil)))))))
(defun remove-unique-task-key (key)
  (bordeaux-threads:with-recursive-lock-held (*unique-tasks-lock*)
    (remhash key *unique-tasks*)))

;;;;tests
(defun test23 ()
  (restart-case
      (handler-bind ((error #'(lambda (c)
				(declare (ignore c))
				(invoke-restart 'my-restart 7))))
	(error "Foo."))
    (my-restart (&optional v) v)))

(defun test ()
  (dotimes (x 10)
    (submit (lambda () (error "fuck you")))))
(defun test2 ()
  (dotimes (x 10)
    (submit (lambda () (random 100)))))
(defun test-loop ()
  (let ((task (submit (lambda () (loop)))))
    (sleep 0.2)
    (kill-job-task task)))

(defun test-unique ()
  (dotimes (x 10)
    (dotimes (x 10)
      (let ((x x))
	(submit-unique-task x ((lambda (n)
				 (dotimes (x 10)
				   (sleep 0.1)
				   (print n)))
			       :args (list x)
			       :callback (lambda (job-task)
					   (declare (ignorable job-task))
					   (print 34234)
					   (remove-unique-task-key (job-task-data job-task)))
			       :data x))))))
(defun test-flush-job-tasks ()
  (flush-job-tasks 'print))
