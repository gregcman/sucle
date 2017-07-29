(in-package :sandbox)

(defgeneric next (object))
(defgeneric prev (object))

(defmethod next (object)
  nil)
(defmethod prev (object)
  nil)

(defclass node ()
  ((next :initform nil :initarg :next :accessor next)
   (prev :initform nil :initarg :prev :accessor prev)))

(defgeneric payload (object))

(defmethod payload ((node node)))

(progn
    (defparameter *node-print-depth* (ash 2 (if t 7 4)))
    (defun pprint-node (stream node)
      (pprint-logical-block (stream nil)
	(print-unreadable-object (node stream :type nil :identity nil)
	  (dotimes (x *node-print-depth*)
	    (format stream "~a" (payload node))
	    (let ((next (next node)))
	      (if (typep next (quote node))
		  (progn
		    (setf node next))
		  (return)))))))
    (set-pprint-dispatch (quote node) (quote pprint-node)))

(defclass dimension ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))
(defun pprint-dimension (stream dimension)
  (pprint-logical-block (stream nil)
    (format stream "{~a ~a}" (x dimension) (y dimension))))
(set-pprint-dispatch (quote dimension) (quote pprint-dimension))
(defun incf-dimension (result delta)
  (incf (x result) (x delta))
  (incf (y result) (y delta)))
(defun decf-dimension (result delta)
  (decf (x result) (x delta))
  (decf (y result) (y delta)))
(defun zerofy-dimension (result)
  (setf (x result) 0)
  (setf (y result) 0))

(progn
  (defclass bracket-part (node)
    ((data :initform nil :initarg :data :accessor bracket-data)))
  (defclass bracket-data ()
    ((dimension :initform (make-instance (quote dimension))
		:initarg :dimension
		:accessor dimension)
     (data :initform nil :initarg :data :accessor data)
     (left :initform nil :initarg :left :accessor left)
     (right :initform nil :initarg :right :accessor right)))
  (defclass left-bracket (bracket-part) ())
  (defclass right-bracket (bracket-part) ()))
(defun gen-brackets ()
  (let ((left-bracket (make-instance (quote left-bracket)))
	(right-bracket (make-instance (quote right-bracket))))
    (let ((middle (make-instance  (quote bracket-data)
				  :left left-bracket
				  :right right-bracket)))
      (setf (bracket-data left-bracket) middle
	    (bracket-data right-bracket) middle))
    (link-nodes left-bracket right-bracket)
    (link-nodes right-bracket left-bracket)
    left-bracket))
(defun enbracket (a)
  (let ((brackets (gen-brackets)))
    (splice-nodes brackets a)
    brackets))

(defmethod payload ((part right-bracket))
  "]")
(defmethod payload ((part left-bracket))
  "[")

(defclass char-node (node)
  ((data :initform nil :initarg :data :accessor char-node-data)))
(defmethod payload ((node char-node))
  (char-node-data node))

(progn
  (defun link-nodes (prev-node next-node)
    (setf (next prev-node) next-node
	  (prev next-node) prev-node))
  (defun disconnect-node (node)
    (let ((prev (prev node))
	  (next (next node)))
      (setf (next node) nil
	    (prev node) nil)
      (when (typep prev (quote node))
	(setf (next prev) next))
      (when (typep next (quote node))
	(setf (prev next) prev))))
  (defun disconnect-prev (node)
    (let ((prev (prev node)))
      (setf (prev node) nil)
      (when (typep prev (quote node))
	(setf (next prev) nil)))
    node)
  (defun disconnect-next (node)
    (let ((next (next node)))
      (setf (next node) nil)
      (when (typep next (quote node))
	(setf (prev next) nil)))
    node)
  (defun splice-nodes (prev next)
    (let ((otherprev (prev next))
	  (othernext (next prev)))
      (link-nodes otherprev othernext)
      (link-nodes prev next))))

(defun string-nodes (string &optional (base (load-time-value (make-instance (quote node)))))
  (let ((prev-node base))
    (let ((len (length string)))
      (dotimes (index len)
	(let ((char (aref string index)))
	  (let ((new-node (make-instance (quote char-node) :data char)))
	    (link-nodes prev-node new-node)
	    (setf prev-node new-node))))
      (let ((start (next base)))
	(disconnect-node base)
	(link-nodes prev-node start)
	(values start
		len)))))

(progn
  (defclass hole (node)
    ((dimension :initform (make-instance (quote dimension))
		:initarg :dimension
		:accessor dimension)
     (active :initform nil :initarg :active :accessor active)
     (motion :initform (make-instance (quote dimension))
	     :initarg :motion
	     :accessor motion)))
  (defmethod payload ((hole hole))
    (let ((width (dimension hole)))
      (format nil "<~a ~a ~a>" (if (active hole) #\@ #\#) width (motion hole))))
  (progn
    (defun deactivate-hole (hole)
      (when (active hole)
	(incf-dimension (motion hole) (dimension hole))
	(setf (active hole) nil)))
    (defun activate-hole (hole)
      (unless (active hole)
	(decf-dimension (motion hole) (dimension hole))
	(setf (active hole) t)))))
(defun gen-hole ()
  (let ((hole (make-instance (quote hole)
			     :dimension (make-instance (quote dimension)
						       :y -1)
			     :active nil)))
    (link-nodes hole hole)
    hole))

(flet ((enbracket (nodes payload width)
	 (let ((ans (enbracket nodes)))
	   (let ((data (bracket-data ans)))
	     (setf (x (dimension data)) width
		   (data data) payload))
	   ans)))
  (defun emit-cons (cell)
    (let ((base nil))
      (let ((last-node base))
	(let ((total-width 0))
	  (labels ((add (new bracket-payload width)
		     (add-no-bracket (enbracket new
						bracket-payload
						width)))
		   (add-no-bracket (new)
		     (let ((next-base (prev new)))
		       (if last-node
			   (progn
			     (let ((hole (gen-hole)))
			       (add-hole hole))
			     (splice-nodes last-node new))
			   (setf base new))
		       (setf last-node next-base)))
		   (add-hole (new)
		     (let ((next-base (prev new)))
		       (splice-nodes last-node new)
		       (setf last-node next-base))))
	    (let ((car (car cell))
		  (cdr (cdr cell)))
	      (let ((data (cons (quote car) cell))
		    (width 0))
		(flet ((string-nodes (string)
			 (multiple-value-bind (nodes length) (string-nodes string)
			   (incf width length)
			   (add nodes data width))))
		  (if (listp car)
		      (if car
			  (progn
			    (string-nodes "(")
			    (multiple-value-bind (nodes length) (emit-cons car)
			      (incf width length)
			      (add-no-bracket nodes)))
			  (string-nodes "nil"))
		      (string-nodes (print-to-buf car))))
		(incf total-width width))
	      (let ((data (cons (quote cdr) cell))
		    (width 0))
		(flet ((string-nodes (string)
			 (multiple-value-bind (nodes length) (string-nodes string)
			   (incf width length)
			   (add nodes data width))))
		  (let ((list-cdr-p nil))
		    (if (listp cdr)
			(if cdr
			    (progn
			      (string-nodes " ")
			      (setf list-cdr-p t))
			    (string-nodes ")"))
			(error "~a" "dotted list"))
		    (incf total-width width)
		    (let ((newbase (enbracket base 
					      (cons (quote cons) cell)
					      total-width)))
		      (setf base newbase)
		      (setf last-node (prev newbase)))
		    (when list-cdr-p
		      (multiple-value-bind (nodes length) (emit-cons cdr)
			(incf total-width length)
			(add-no-bracket nodes))))))))
	  (values base
		  total-width))))))

;;walking
(let ((offset (load-time-value (make-instance (quote dimension)))))
  (defun draw-nodes2 (x y node &optional (cap 128))
    (setf (x offset) x
	  (y offset) y)
    (dotimes (index cap)
      (if node
	  (progn
	    (typecase node
	      (char-node
	       (scwu node (x offset) (y offset))
	       (incf (x offset)))
	      (hole
					;(update-whole-hole node)
	       (when (active node)
		 (incf-dimension offset (dimension node)))))
	    (setf node (next node)))
	  (return)))))

;;walking
(let ((offset (load-time-value (make-instance (quote dimension)))))
  (defun draw-nodes2-reverse (x y node &optional (cap 128))
    (setf (x offset) x
	  (y offset) y)
    (dotimes (index cap) 
      (if node
	  (progn 
	    (typecase node
	      (char-node
	       (scwu node (x offset) (y offset))
	       (decf (x offset)))
	      (hole	       
					;    (update-whole-hole node)
	       (when (active node)
		 (decf offset (dimension node)))))
	    (setf node (prev node)))
	  (return)))))

(defun find-node-forward (nodes test)
  (labels ((rec (node)
	     (if node
		 (if (funcall test node)
		     node
		     (rec (next node)))
		 nil)))
    (rec nodes)))
(defun find-node-backward (nodes test)
  (labels ((rec (node)
	     (if node
		 (if (funcall test node)
		     node
		     (rec (prev node)))
		 nil)))
    (rec nodes)))

(defun char-search-down (node)
  (labels ((rec (node offset height)
	     (when node
	       (let ((data node))
		 (typecase data
		   (char-node
		    (if (zerop offset)
			(values node height)
			(rec (next node)
			     (+ offset 1)
			     height)))
		   (hole (if (active data)
			     (rec (next node)
				  (+ offset (x (dimension data)))
				  (1- height))
			     (rec (next node) offset height)))
		   (t (rec (next node) offset height)))))))
    (rec (next node) 1 0)))

(defun char-search-up (node)
  (labels ((rec (node offset height)
	     (when node
	       (let ((data node))
		 (typecase data
		   (char-node
		    (if (zerop offset)
			(values node height)
			(rec (prev node)
			     (- offset 1)
			     height)))
		   (hole (if (active data)
			     (rec (prev node)
				  (- offset (x (dimension data)))
				  (1+ height))
			     (rec (prev node) offset height)))
		   (t (rec (prev node) offset height)))))))
    (rec (prev node) -1 0)))
