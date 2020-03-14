(defpackage #:deflazy
  (:use #:cl)
  (:export
   #:getfnc
   #:deflazy
   #:refresh

   #:dlaz
   #:lazgen
   #:define-lazgen))

(in-package :deflazy)

(defun refresh (thing &optional (same-thread nil))
  ;;refresh symbol -> look it up
  ;;refresh node -> refresh node
  (dependency-graph:%%refresh
   (etypecase thing
     (symbol
      (symbol-value thing))
     (dependency-graph:node
      thing)) :same-thread same-thread)
  (values))
(defun getfnc (thing
	       ;; &optional (namespace *namespace*)
		    )
  ;;getfnc symbol -> look it up
  ;;getfnc node -> fulfill node
  (dependency-graph:%get-value
   (etypecase thing
     (symbol
      (symbol-value thing))
     (dependency-graph:node
      thing))))

(defun %defdep (fun dependencies tags &optional name)
  (let
      ;;Make the node
      ((node (make-instance 'dependency-graph:node)))
    (dependency-graph:%redefine-node fun node dependencies name tags)
    node))

(symbol-macrolet ((sym :dlaz-lambda-list))
  (defun set-lambda-list (name lambda-list)
    (setf (get name sym) lambda-list))
  (defun get-lambda-list (name)
    (get name sym)))
;;;;;;

(defmacro dlaz (form)
  ;;Wrap defun and apply so we et correct slime hints.
  ;;defun has to be a simple lambda list with no special keywords.
  ;;no &rest, &optional, &key
  (ecase (car form)
    ((defun)
     ;;define the creator function, saving its lambda-list for future reference
     ;;The lambda-list
     (let ((lambda-list (third form))
	   (name (second form)))
       `(progn
	  (set-lambda-list ',name ',lambda-list)
	  ,form)
       ))
    ((apply)
     ;;
     (let ((name (second form)))
       (utility:once-only (name)
	 `(%defdep ,name (list* ,@ (nthcdr 2 form)) (get-lambda-list ,name) ,name))))))
(defmacro lazgen (name &rest args)
  `(dlaz (apply ',name ,@args nil)))
(defmacro define-lazgen (name lambda-list &body body)
  `(dlaz (defun ,name ,lambda-list ,@body)))

(defmacro deflazy (name (&rest deps) &body body)
  ;;Careful -> defines both a variable and a function
  ;;with the same name
  `(progn
     (define-lazgen ,name ,(mapcar (lambda (x)
				     (etypecase x
				       (symbol x)
				       (cons (car x))))
				   deps)
       ,@body)
     (defvar ,name 
       (lazgen ,name
	       ,@(mapcar (lambda (x)
			   (etypecase x
			     (symbol x)
			     (cons (cdr x))))
			 deps)))
     (dependency-graph:refresh-old-node ,name)))
