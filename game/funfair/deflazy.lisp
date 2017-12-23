(defpackage #:lazy-place
  (:use #:cl))
(in-package #:lazy-place)

;;;(deflazy place ((name place)
;;;                 (name2 place2))
;;;   (body))

;;;ensure place is a lazy place
;;;(value timestamp genfun genfun-old (timestamp0 . lazy-place0) (timestamp1 . lazy-place1))
;;;
;;;
;;;
;;;

(defclass lazy-place ()
  ((lazy-place-value
    :accessor lazy-place-value
    :initform nil)
   (lazy-place-exists-p
    :accessor lazy-place-exists-p
    :initform nil)
   (lazy-place-timestamp
    :accessor lazy-place-timestamp
    :initform 0)
   (lazy-place-lock
    :accessor lazy-place-lock
    :initform nil)
   (lazy-place-genfun
    :accessor lazy-place-genfun
    :initform nil)
   (lazy-place-genfun-old
    :accessor lazy-place-genfun-old
    :initform nil)
   (lazy-place-args-values-old
    :accessor lazy-place-args-values-old
    :initform nil)
   (lazy-place-args
    :accessor lazy-place-args
    :initform nil)
   (lazy-place-args-timestamps
    :accessor lazy-place-args-timestamps
    :initform nil)))
(defun lazy-place-p (obj)
  (typep obj 'lazy-place))
(defun make-lazy-place ()
  (make-instance 'lazy-place))

(defun destroy (lazy-place)
  (with-slots (lazy-place-value
	       lazy-place-exists-p
	       lazy-place-timestamp
	       lazy-place-genfun
	       lazy-place-genfun-old
	       lazy-place-lock)
      lazy-place
    (touch lazy-place)
    (dirtify lazy-place)
    (setf lazy-place-value "no value")
    #+nil
    (flet ((clear (x)
	     (block nil
	       (when (slot-boundp lazy-place x)
		 (let ((x (slot-value lazy-place x)))
		   (do ((cell x (cdr cell)))
		       ((null cell))
		     (setf (car cell) nil)))))))
      (clear 'lazy-place-args)
      (clear 'lazy-place-args-values-old)
      (clear 'lazy-place-args-timestamps))))

(defmacro with-t (place &body body)
  `(unwind-protect
	(progn
	  (setf ,place t) ;;;to catch circular dependencies
	  ,@body)
     (setf ,place nil)))

(defmacro any (&rest forms)
  (nth (random (length forms)) forms))

(defun fulfill (lazy-place)
  (if (lazy-place-exists-p lazy-place)
      (lazy-place-value lazy-place) 
      (with-slots (lazy-place-value
		   lazy-place-genfun
		   lazy-place-genfun-old
		   lazy-place-args
		   lazy-place-args-values-old
		   lazy-place-args-timestamps
		   lazy-place-exists-p
		   lazy-place-lock)
	  lazy-place
	(if lazy-place-lock
	    (error "circular dependency or multiple threads fulfilling: ~a"
		   lazy-place)
	    (with-t lazy-place-lock
	      (let ((genfun lazy-place-genfun))
		(if genfun
		    (setf lazy-place-genfun-old genfun)
		    (error "dependency not initialized"))) ;;function used to gen value
	      (touch lazy-place) ;;value timestamp
	      (let ((args-old (map-into lazy-place-args-values-old
					#'fulfill lazy-place-args))) ;;the argument values
		(map-into lazy-place-args-timestamps ;;argument timestamps
			  #'lazy-place-timestamp
			  lazy-place-args)
		(let ((new-value (apply lazy-place-genfun args-old)));;the newly generated value
		  (prog1
		      (setf lazy-place-value
			    new-value)
		    (setf lazy-place-exists-p t)))))))))

(defun dirty-p (lazy-place)
  (with-slots (lazy-place-genfun
	       lazy-place-genfun-old
	       lazy-place-args-timestamps
	       lazy-place-args) lazy-place
    (or (not (eq lazy-place-genfun ;;;check for updated fun
		 lazy-place-genfun-old))
	(block nil
	  (do ((stamp lazy-place-args-timestamps (cdr stamp))
	       (arg lazy-place-args (cdr arg)))
	      ((not (any arg
			 stamp)) nil)
	      ;;;iterate old timestamps and see if any differ now
	    (when (not (= (car stamp)
			  (lazy-place-timestamp (car arg))))
	      (return t)))))))
(defun touch (lazy-place)
  (with-slots (lazy-place-timestamp) lazy-place
    (incf lazy-place-timestamp)))
(defun dirtify (lazy-place)
  (with-slots (lazy-place-exists-p) lazy-place
    (setf lazy-place-exists-p nil)))

(defmacro deflazy (place (&rest deps) &rest gen-forms)
  (let ((places (mapcar #'second deps))
	(vars (mapcar #'first deps))
	(deps-len (list-length deps)))
    (funland::with-gensyms (len ensure-lazy-place)
      `(macrolet ((,ensure-lazy-place (place &environment env)
		    (multiple-value-bind (vars vals stores setter getter)
			(get-setf-expansion place env)
		      (funland::with-gensyms (value values-form)
			`(let* (,@ (mapcar #'list vars vals))
			   (or (let ((,value ,getter))
				 (when (lazy-place-p ,value)
				   ,value))
			       (let ((,values-form (make-lazy-place)))
				 (multiple-value-bind ,stores ,values-form
				   ,setter))))))))
	 (let ((inst (,ensure-lazy-place ,place))
	       (,len ,deps-len))
	   (with-slots (lazy-place-genfun
			lazy-place-args
			lazy-place-args-values-old
			lazy-place-args-timestamps) inst
	     (setf lazy-place-genfun
		   (lambda ,vars
		     ,@gen-forms))
	     (setf lazy-place-args-timestamps
		   (make-list ,len :initial-element -1))
	     (setf lazy-place-args-values-old
		   (make-list ,len))
	     (setf lazy-place-args
		   ,(cons 'list
			  (mapcar (lambda (x) (list ensure-lazy-place x))
				  places))
		   ))
	   inst)))))

#+nil
(defmacro my-setf (place values-form &environment env)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (declare (ignorable getter))
    (print (list vars vals stores setter getter))
    (print env)
    `(let* (,@ (mapcar #'list vars vals))
       (multiple-value-bind ,stores ,values-form
	 ,setter))))

#+nil
(defmacro my-setf (place values-form &environment env)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (declare (ignorable getter))
    `(let* (,@ (mapcar #'list vars vals))
       (multiple-value-bind ,stores ,values-form
	 ,setter))))

;;;;;;;;;;;;;;;;
;;;;each item stores a:
;;;;-token
;;;;-value
;;;;-function
;;;;-args
#+nil
(defparameter *circle* nil)
#+nil
(defparameter *no-value* "no value")
#+nil
(defun get-stuff (name stuff otherwise)
  (multiple-value-bind (val exists-p) (gethash name stuff)
    (if (and exists-p
	     (rest val))
	val ;;a cell 
	(let* ((depdata (gethash name otherwise))
	       (genfunc (cdr depdata))
	       (deps (car depdata)))
	  (when (member name *circle*)
	    (error "circular dependency ~a" *circle*))
	  (if genfunc
	      (let ((a nil))
		(dolist (item deps)
		  (let ((*circle* (cons name *circle*)))
		    (push (get-stuff item stuff otherwise) a)))
		(let ((dep-values (nreverse a)))
		  (let ((value (apply genfunc
				      (mapcar #'car dep-values))))
		    (let ((cell (or val
				    (setf (gethash name stuff)
					  (cons *no-value* nil)))))
		      (setf (car cell) value
			    (cdr cell) (cons
					(mapcar (lambda (x)
						  (cons x (cdr x)))
						dep-values)
					genfunc))
		      cell))))
	      (error "no backup fun: ~a" genfunc))))))

#+nil
(defun dirty-p (name)
  (multiple-value-bind (cell exists?) (gethash name *stuff*)
    (cond (exists?
	   (let ((cellcdr (cdr cell)))
	     (unless (eq (cdr (gethash name *backup*)) ;;stored fun
			 (cdr cellcdr)) ;;fun use to generate value
	       (return-from dirty-p t))
	     (dolist (item (car cellcdr))
	       (unless
		   (eq (cdr (car item)) ;;value data
		       (cdr item)) ;;args used before
		 (return-from dirty-p t))))
	   nil)
	  (t nil))))

#+nil
(defun mangle (sym &optional (start "_source_"))
  (symbolicate2 (list start sym)
		(symbol-package sym)))
#+nil
(defmacro deflazy (name (&rest deps) &rest gen-forms)
  `(bornfnc (quote ,name)
	    (cons
	     (quote
	      ,(mapcar (lambda (x)
			 (typecase x
			   (symbol x)
			   (otherwise (first x))))
		       deps))
	     (lambda ,(mapcar (lambda (x)
				(typecase x
				  (symbol x)
				  (otherwise (second x))))
			      deps)
	       ,@gen-forms))))
#+nil
(defun bornfnc (name func)
  (setf (gethash *backup* name) func))

#+nil
(defparameter *backup* (make-hash-table :test 'eq))
    ;;(car (get-stuff name *stuff* *backup*))
    					;(car (gethash name *stuff*))
