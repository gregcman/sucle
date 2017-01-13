(in-package :macrology)

;;;toggle a place between true and nil
(defmacro toggle (var)
  `(setf ,var (not ,var)))

;;;comment out code the macro way!
(defmacro progno (&rest args))

;;;when the only let value is returned at the end
(defmacro ret (var bound-form &body body)
  `(let ((,var ,bound-form))
     ,@body
     ,var))

;;;used to rename something
(defmacro rename (&rest rebind-forms)
  (let (acc)
    (dolist (x rebind-forms)
      (push `(defmacro ,(second x) (&body body)
	       (cons ',(car x) body)) acc))
    (cons 'progn acc)))

;;defparamter dp is rotationally symmetric
;;lambda gave rise to the latin l
;;multiple-value-bind is really fucking long
(rename (defparameter dp)
	(lambda l)
	(multiple-value-bind mvb))

(defmacro dorange ((var times-form start-form) &rest body)
  (let ((times (gensym)))
    `(let ((,times ,times-form))
       (dobox ((,var ,times (+ ,times ,start-form))) ,@body))))

;;;iterate through a multidimensional box 
(defmacro dobox ((&rest interval-forms) &rest body)
  (let ((let-one nil)
	(let-one-declarations nil))
    (let ((body (cons 'progn body)))
      (dolist (form interval-forms)
	(multiple-value-bind (let-len temp-length let-end temp-end bod)
	    (apply #'dorange-generator body form)
	  (push let-len let-one)
	  (push temp-length let-one-declarations)
	  (push let-end let-one)
	  (push temp-end let-one-declarations)
	  (setq body bod)))
      `(progn (let ,let-one
		(declare (type fixnum ,@let-one-declarations))
		,body)))))

(defun dorange-generator (body var start-form end-form)
  (let ((temp (gensym))
	(temp2 (gensym))
	(start (gensym))
	(end (gensym)))
    (values
     `(,start ,start-form) ;;length init
     start  ;;;var names for declarations
     `(,end ,end-form)
     end   
     `(let ((,var ,start))
	(tagbody
	   (go ,temp2)
	   ,temp
	   ,body
	   (psetq ,var (1+ ,var))
	   ,temp2
	   (unless (>= ,var ,end) (go ,temp)))))))

(defmacro null! (&rest args)
  (let (acc)
    (dolist (arg args)
      (push `(setf ,arg nil) acc))
    `(progn ,@acc)))
