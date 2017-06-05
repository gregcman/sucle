(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)

;; have to rename this class to thread-signal because it's a built-in typename in lisp
(defclass thread-signal ()
  ((cond :initform (glfw:create-cond) :reader signal-cond) ; have to rename this reader to signal-cond because it's a built-in macro
   (mutex :initform (glfw:create-mutex) :reader mutex)
   (flag :initform nil :accessor flag)))

(defmethod initialize-instance :after ((s thread-signal) &key)
  (format t "Created thread-signal with cond ~a and mutex ~a~%" (signal-cond s) (mutex s)))

(defun kill-signal (s)
  (declare (type thread-signal s))
  (glfw:destroy-mutex (mutex s))
  (glfw:destroy-cond (signal-cond s))
  (setf (flag s) nil))

(defun wait-signal (s)
  (declare (type thread-signal s))
  (glfw:with-lock-mutex (mutex s)
    (loop while (not (flag s)) do
	 (glfw:wait-cond (signal-cond s) (mutex s) glfw:+infinity+))
    (setf (flag s) nil)))

(defun set-signal (s)
  (declare (type thread-signal s))
  (glfw:with-lock-mutex (mutex s)
   (setf (flag s) t))
  (glfw:signal-cond (signal-cond s)))


(unless (glfw:init)
  (error "Could not glfw:init"))

(format t "Multithreading benchmarking program
-----------------------------------

This program consists of two tests. In the first test two threads are created,
which continously signal/wait each other. This forces the execution to
alternate between the two threads, and gives a measure of the thread
synchronization granularity. In the second test, the main thread is repeatedly
put to sleep for a very short interval using glfwSleep. The average sleep time
is measured, which tells the minimum supported sleep interval.

Results:
--------

")							   

(format t "Number of CPUs: ~d~%~%" (glfw:get-number-of-processors))

(defparameter *done-mutex* nil)
(defparameter *thread-done* nil)
(defparameter *goto-a* nil)
(defparameter *goto-b* nil)
(defparameter *goto-a-count* 0)
(defparameter *goto-b-count* 0)
(defparameter *done-count* 0)
(defparameter *max-count* 10000)

(defun setup ()
  (declare (optimize (debug 3) (safety 3) (speed 0) (compilation-speed 0)))
  (setf *done-mutex* (glfw:create-mutex)
	*thread-done* (glfw:create-cond)
	*goto-a* (make-instance 'thread-signal)
	*goto-b* (make-instance 'thread-signal)
	*goto-a-count* 0
	*goto-b-count* 0
	*done-count* 0))

(defun teardown ()
  (declare (optimize (debug 3) (safety 3) (speed 0) (compilation-speed 0)))
  (glfw:destroy-mutex *done-mutex*)
  (glfw:destroy-cond *thread-done*)
  (kill-signal *goto-a*)
  (kill-signal *goto-b*))


(defmacro make-thread-callback (name signal-var other-signal-var count-var)
  `(cffi:defcallback ,name :void ((arg :pointer))
     (declare (ignore arg))
     (do ()
	 ((>= ,count-var *max-count*))
       (incf ,count-var)
       (set-signal ,other-signal-var)
       (wait-signal ,signal-var))
     (set-signal ,other-signal-var)
     (glfw:with-lock-mutex *done-mutex*
       (incf *done-count*))
     (glfw:signal-cond *thread-done*)))

(make-thread-callback thread-a-fun *goto-a* *goto-b* *goto-a-count*)
(make-thread-callback thread-b-fun *goto-b* *goto-a* *goto-b-count*)

(defun test-1 ()
  (declare (optimize (debug 3) (safety 3) (speed 0) (compilation-speed 0)))
  (sb-ext::without-gcing 
    (let ((thread-a (glfw:create-thread (cffi:callback thread-a-fun) (cffi:null-pointer)))
	  (thread-b (glfw:create-thread (cffi:callback thread-b-fun) (cffi:null-pointer))))
  
      (when (or (minusp thread-a) (minusp thread-b))
	(format t "One of the threads failed~%")
	(glfw:with-lock-mutex *done-mutex*
	  (setf *done-count* 2)))

      (let ((t1 (glfw:get-time)))
	(glfw:with-lock-mutex *done-mutex*
	  (loop until (= *done-count* 2)
	     do (glfw:wait-cond *thread-done* *done-mutex* glfw:+infinity+)))
	(let* ((t2 (glfw:get-time))
	       (csps (/ (+ *goto-a-count* *goto-b-count*)
			(- t2 t1))))
	  (format t "Test 1: ~,0f context switches / second (~,3f us/switch)~%" csps (/ 1000000 csps))))

      (format t "waiting for thread a to finish completely~%")
      (glfw:wait-thread thread-a glfw:+wait+)
      (format t "waiting for thread b to finish completely~%")
      (glfw:wait-thread thread-b glfw:+wait+)
      (format t "finished waiting~%"))))

(defun test-2 ()
  (let ((t1 (glfw:get-time))
	count)
    (dotimes (i 10)
      (glfw:sleep 0.0001))
    (setf count (/ 1.0 (/ (- (glfw:get-time) t1)
			  10.0)))
    (setf t1 (glfw:get-time))
    (dotimes (i count)
      (glfw:sleep 0.0001))
    (format t "Test 2: ~,3f ms / sleep (mean)~%~%"
	    (/ (* 1000.0 (- (glfw:get-time) t1))
	       count))))


(unwind-protect
     (progn
       (setup)
       (test-1))
  (teardown))

(test-2)
(glfw:terminate)
