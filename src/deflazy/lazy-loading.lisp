(defpackage #:deflazy
  (:use #:cl)
  (:export
   #:getfnc
   #:deflazy
   #:refresh

   #:dlaz
   #:lazgen
   #:define-lazgen
   #:singleton))

(in-package :deflazy)

(defun singleton (sym)
  (let ((name (get-special-name sym)))
    (if name
	(symbol-value name)
	(error "No default deflazy: ~a" sym))))

(defun refresh (thing &optional (same-thread nil))
  ;;refresh symbol -> look it up
  ;;refresh node -> refresh node
  (dependency-graph:%%refresh
   (etypecase thing
     (symbol
      (singleton thing))
     (dependency-graph:node
      thing))
   :same-thread same-thread)
  (values))
(defun getfnc (thing
	       ;; &optional (namespace *namespace*)
		    )
  ;;getfnc symbol -> look it up
  ;;getfnc node -> fulfill node
  (dependency-graph:%get-value
   (etypecase thing
     (symbol
      (singleton thing))
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
(symbol-macrolet ((sym :dlaz-special-name))
  (defun set-special-name (name special)
    (setf (get name sym) special))
  (defun get-special-name (name)
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
(utility:eval-always
 (defun special-name (sym)
   ;;[FIXME] safety through obscurity.
   (utility:symbolicate2 (list "*-*-*" sym "*-*-*") (symbol-package sym))))
(defmacro deflazy (name (&rest deps) &body body)
  ;;Careful -> defines both a variable and a function
  ;;with the same name
  (let ((special-name (special-name name)))
    `(progn
       (define-lazgen ,name ,(mapcar (lambda (x)
				       (etypecase x
					 (symbol x)
					 (cons (car x))))
				     deps)
	 ,@body)
       (set-special-name ',name ',special-name)
       (defvar ,special-name
	 (lazgen ,name
		 ,@(mapcar (lambda (x)
			     (special-name
			      (etypecase x
				(symbol x)
				(cons (second x)))))
			   deps)))
       (dependency-graph:refresh-old-node ,special-name))))
