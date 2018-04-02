(in-package :cl-user)

(define-symbol-macro *var*
    (if (boundp (quote *var*))
	(let ((value (symbol-value (quote *var*))))
	  (values value (quote *var*)))
	(values "other" (quote *var*))))

(define-symbol-macro *foo* (utility:etouq *foo*))

(defun print-var ()
  (let ((*var* 8))
    (print *var*)))

(defun test ()
  (let ((*var* 34))
    (print *var*)))

(defun test2 ()
  (print *var*)
  (progv '(*var*) '(32)
    (print-var)
    *var*))

(defun gen-test (symbol)
  (let ((f (load-time-value (gensym "FUNC-"))))
    `(lambda ()
       (let ((,symbol 1))
	 (let ((,f (lambda () ,symbol)))			  
	   (let ((,symbol 2))	   
	     (eql 2 (funcall ,f))))))))

(defun specialp (symbol)
  (handler-case 
    (compile nil (gen-test symbol))
    (error (condition)
      condition
      )))

(defun symbol-macro-p (symbol)
  (not (eq symbol (macroexpand-1 symbol))))

