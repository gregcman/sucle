(in-package #:utility)
#+nil
(defmacro etouq (form)
  (eval form))
(defmacro etouq (&body body)
  (let ((var (gensym)))
    `(macrolet ((,var () ,@body))
       (,var))))

(defmacro toggle (var) `(setf ,var (not ,var)))

(defmacro progno (&rest args) (declare (ignore args)))

(defmacro %list (target &rest args)
  (let ((list-var (gensym))
	(original (gensym)))
    `(let* ((,list-var ,target)
	    (,original ,list-var))
       ,@(let (a
	       (first? t))
	      (dolist (arg args)
		(if first?
		    (setf first? nil)
		    (push `(setf ,list-var (cdr ,list-var)) a))
		(push `(setf (car ,list-var) ,arg) a))
	      (nreverse a))
       ,original)))

(defmacro with-unsafe-speed (&body body)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (progn
       ,@body)))
(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro with-declaim-inline ((&rest names) &body body)
  `(progn
     (declaim (inline ,@names))
     ,@body
     (declaim (notinline ,@names))))

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
      (dolist (form (reverse interval-forms))
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




(defun with-vec-params (&rest args)
  (destructuring-bind ((&rest bufvars)
		       (buf &optional (binder 'let)) &body body) args
    (%%with-vec-params bufvars buf binder body)))

(defmacro with-vec ((&rest bufvars)
		       (buf &optional (binder 'let)) &body body)
  (%%with-vec-params bufvars buf binder body))

(defun %%with-vec-params (bufvars buf binder body)
  (let ((param-data (with-vec-params2 bufvars)))
    (let ((last (cdr (assoc :last param-data)))
	  (letargs (cdr (assoc :letargs param-data))))
      (let ((new-let-args (mapcar
			   (lambda (x)
			     `(,(pop x) (aref ,buf ,(pop x))))
			   letargs)))
	(let ((new-last
	       `(,binder ,new-let-args ,@body)))
	  (setf (cdr last) (list new-last)))))
    (cdr (assoc :head param-data))))

(defun with-vec-params2 (bufvars)
  (multiple-value-bind (letargsoffset letargs decl)
      (%2aux-with-vec-params bufvars)

    (let ((fin `(let* ,letargsoffset
		  (declare (type fixnum ,@decl)))))
      (let ((last (last fin)))
	(pairlis (quote (:head
			 :last
			 :letargs))
		 (list fin
		       last
		       letargs))))))

(defun %2aux-with-vec-params (bufvars)
  (let ((letargs nil)
	(letargsoffset nil)
	(decl nil))
    (multiple-value-bind (ans offs) (%aux-with-vec-params bufvars)
      (labels ((rec-push (stuff)
		 (dolist (x stuff)
		   (let ((first (car x)))
		     (if (consp first)
			 (rec-push x)
			 (push x letargs))))))
	(rec-push ans))
      (labels ((rec-push-offs (stuff)
		 (dolist (x stuff)
		   (let ((first (car x)))
		     (if (consp first)			 
			 (rec-push-offs x)
			 (when first (push x letargsoffset)
			       (push (car x) decl)))))))
	(rec-push-offs offs)))
    (values letargsoffset
	    letargs
	    decl)))

(defun %aux-with-vec-params (vars-or-offsets &optional (offset 0))
  (let ((counter 0)
	(bindings nil)
	(offsets nil))
    (dolist (var-or-offset vars-or-offsets)
      (if (consp var-or-offset)
	  (let ((suboffset (car var-or-offset)))
	    (multiple-value-bind (bindings-list offsets-list)
		(%aux-with-vec-params
		 (cdr var-or-offset)
		 (if (eql 0 offset)
		     suboffset
		     (let ((newoffset (gensym)))
		       (push `(,newoffset (+ ,offset ,suboffset)) offsets)
		       newoffset)))
	      (when bindings-list
		(push bindings-list bindings))
	      (when offsets-list
		(push offsets-list offsets))))
	  (progn
	    (let ((sub (if (eql 0 counter)
			   offset
			   (let ((new-offset (gensym)))
			     (push`(,new-offset (+ ,offset ,counter)) offsets)
			     new-offset))))
	      (when var-or-offset
		(push `(,var-or-offset ,sub) bindings)))
	    (incf counter))))
    (values bindings offsets)))


(defun type-multimap-alist (type varname alist)
  (let ((value (assoc type alist :test 'equal)))
    (if value
	(push varname (cdr value))
	(push (list type varname) alist))
    alist))

(defmacro with-let-mapped-places ((&rest place-pairs) &body body)
  (%with-let-mapped-places place-pairs body))

(defun %with-let-mapped-places (place-pairs &optional body)
  (let ((let-args nil)
	(setf-args nil)
	(type-multimap-alist nil))
    (dolist (place-pair place-pairs)
      (let ((reg-place (pop place-pair))
	    (ram-place (pop place-pair))
	    (type (pop place-pair)))
	(push (list reg-place ram-place) let-args)
	(push reg-place setf-args)
	(push ram-place setf-args)
	(if type
	    (setf type-multimap-alist (type-multimap-alist type reg-place type-multimap-alist)))))
    (multiple-value-bind (new-body decl) (parse-body body)
      `(let ,let-args
	 ,(cons 'declare
		(mapcar (lambda (x)
			  (cons 'type x))
			type-multimap-alist))
	 ,@decl
	 (multiple-value-prog1
	     ,(cons 'progn new-body)
	   ,(cons 'setf setf-args))))))

(defun spill-hash (hash &optional (stream *standard-output*))
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format stream "~S ~S~%" key value)))

(defmacro dohash ((k v) hash &body body)
  (multiple-value-bind (forms decl doc) (parse-body body)
    (declare (ignorable doc))
    (with-gensyms (next more? hashvar)
      `(let ((,hashvar ,hash))
	 (with-hash-table-iterator (,next ,hashvar)
	   (loop (multiple-value-bind (,more? ,k ,v) (,next)
		   ,@decl
		   (unless ,more? (return))
		   ,@forms)))))))


(defun keywordify (sym)
  (intern (string sym)
	  (load-time-value (find-package "KEYWORD"))))

(defconstant +fixnum-bits+ (logcount most-positive-fixnum))
(defun print-bits (n &optional (stream *standard-output*))
  (format stream
	  (etouq (concatenate 'string
			      "~"
			      (write-to-string
			      +fixnum-bits+ )
			      ",'0b"))
	  (logand n most-positive-fixnum))
  n)

(defmacro any (&body body)
  (let ((n (list-length body)))
    (if (zerop n)
	nil
	(nth (random n) body))))
