(in-package :sandbox)

(defstruct node
  prev
  next
  payload)

(defstruct payload
  metadata
  data)

(defstruct bracket
  object
  width
  left
  right)

(progn
  (defparameter *node-print-depth* (ash 2 4))
  (defun pprint-node (stream node)
    (pprint-logical-block (stream nil)
      (print-unreadable-object (node stream :type nil :identity nil)
	(dotimes (x *node-print-depth*)
	  (format stream "~a" (node-payload node))
	  (let ((next (node-next node)))
	    (if (typep next (quote node))
		(progn
					;		(format stream ", ")
		  (setf node next))
		(return)))))))
  (set-pprint-dispatch (quote node) (quote pprint-node)))

(progn
  (defun pprint-payload (stream payload)
    (pprint-logical-block (stream nil)
      (let ((meta (payload-metadata payload)))
	(format stream "~a"
		(case (cdr meta)
		  (bracket (case (car meta)
			     (left "[")
			     (right "]")))
		  (brace (case (car meta)
			   (left "{")
			   (right "}"))))))))
  (set-pprint-dispatch (quote payload) (quote pprint-payload)))

(defun link-nodes (prev-node next-node)
  (setf (node-next prev-node) next-node
	(node-prev next-node) prev-node))
(defun disconnect-node (node)
  (let ((prev (node-prev node))
	(next (node-next node)))
    (setf (node-next node) nil
	  (node-prev node) nil)
    (when (typep prev (quote node))
      (setf (node-next prev) next))
    (when (typep next (quote node))
      (setf (node-prev next) prev))))

(defun disconnect-node-prev (node)
  (let ((prev (node-prev node)))
    (setf (node-prev node) nil)
    (when (typep prev (quote node))
      (setf (node-next prev) nil)))
  node)
(defun disconnect-node-next (node)
  (let ((next (node-next node)))
    (setf (node-next node) nil)
    (when (typep next (quote node))
      (setf (node-prev next) nil)))
  node)

(defun string-nodes (string &optional (base (load-time-value (make-node))))
  (let ((prev-node base))
    (let ((len (length string)))
      (dotimes (index len)
	(let ((char (aref string index)))
	  (let ((new-node (make-node :payload char)))
	    (link-nodes prev-node new-node)
	    (setf prev-node new-node))))
      (let ((start (node-next base)))
	(disconnect-node base)
	(link-nodes prev-node start)
	(values start
		len)))))

(defun print-to-buf (object)
  (let ((buffer (make-array 0 :fill-pointer 0 :adjustable t :element-type (quote character))))
    (setf (fill-pointer buffer) 0)
    (with-output-to-string (var buffer)
      (prin1 object var))
    buffer))

(defun gen-brackets (data data2 width)
  (let ((left-payload (make-payload :metadata (cons (quote left) data)))
	(right-payload (make-payload :metadata (cons (quote right) data))))
    (let ((left-bracket (make-node :payload left-payload))
	  (right-bracket (make-node :payload right-payload)))
      (let ((middle (make-bracket :left left-bracket
				  :right right-bracket
				  :object data2
				  :width width)))
	      (setf (payload-data left-payload) middle
		    (payload-data right-payload) middle))
      (link-nodes left-bracket right-bracket)
      (link-nodes right-bracket left-bracket)
      left-bracket)))

(defun splice-nodes (prev next)
  (let ((otherprev (node-prev next))
	(othernext (node-next prev)))
    (link-nodes otherprev othernext)
    (link-nodes prev next)))

(defun enbracket (a data data2 width)
  (let ((brackets (gen-brackets data data2 width)))
    (splice-nodes brackets a)
    brackets))

(defun emit-cons (cell)
  (let ((base nil))
    (let ((last-node base))
      (let ((total-width 0))
	(labels ((add (new bracket-payload width)
		   (add-no-bracket (enbracket new
					      (quote brace)
					      bracket-payload
					      width)))
		 (add-no-bracket (new)
		   (let ((next-base (node-prev new)))
		     (if last-node
			 (splice-nodes last-node new)
			 (setf base new))
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
		  (let ((newbase (enbracket base (quote bracket)
					    (cons (quote cons) cell)
					    total-width)))
		    (setf base newbase)
		    (setf last-node (node-prev newbase)))
		  (when list-cdr-p
		    (multiple-value-bind (nodes length) (emit-cons cdr)
		      (incf total-width length)
		      (add-no-bracket nodes))))))))
	(values base
		total-width)))))

(setf *print-case* :downcase)
(defparameter *test-tree*
  (copy-tree
   (quote
    (defun print-cells (sexp)
      (let ((cdr (cdr sexp))
	    (car (car sexp)))
	(if (listp car)
	    (if car
		(progn
		  (princ "(")
		  (print-cells car))
		(princ nil))
	    (prin1 car))
	(if (listp cdr)
	    (if cdr
		(progn
		  (princ " ")
		  (print-cells cdr))
		(princ ")"))
	    (progn
	      (princ " . ")
	      (prin1  cdr)
	      (princ ")"))))))))


(defun draw-nodes (x y nodes)
  (let ((cap 128))
    (dotimes (index cap)
      (scwu nodes x y)
      (let ((next (node-next-char (node-next nodes))))
	(if next
	    (progn
	      (incf x)
	      (setf nodes next))
	    (return))))))

(defun node-next-char (node)
  (if node
      (if (typep (node-payload node) (quote character))
	  node
	  (node-next-char (node-next node)))))
(defun node-prev-char (node)
  (if node
      (if (typep (node-payload node) (quote character))
	  node
	  (node-prev-char (node-prev node)))))

(defun jump-block-left (node)
  (bracket-left (payload-data (node-payload node))))
(defun jump-block-right (node)
  (bracket-right (payload-data (node-payload node))))

(defun find-enclosing-block-left (node)
  (block nil
    (if node
	(let ((payload (node-payload node)))
	  (if (typep payload (quote payload))
	      (let ((data (payload-metadata payload)))
		(let ((side (car data)))
		  (if (eq (quote left) side)
		      (return (payload-data payload))
		      (if (eq (quote right) side)
			  (setf node (jump-block-left node)))))))
	  (find-enclosing-block-left (node-prev node))))))
