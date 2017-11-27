(in-package :fuktard)

(defun dorange-generator (body var start-form end-form &key (test '<) (jmp 'if) (inc 1))
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
	   (setq ,var (+ ,inc ,var))
	   ,temp2
	   (,jmp (,test ,var ,end) (go ,temp)))))))

;;;iterate through a multidimensional box 
(defmacro dobox ((&rest interval-forms) &rest body)
  (let ((let-one nil)
	(let-one-declarations nil))
    (let ((body (cons 'tagbody body)))
      (dolist (form (nreverse interval-forms))
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
