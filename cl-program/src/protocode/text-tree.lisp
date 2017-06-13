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
  (width 0 :type fixnum)
  left
  right)

(defstruct hole
  (width 0 :type fixnum)
  active
  (motion 0 :type fixnum)
  generator
  indentation-func)

(progn
  (defparameter *node-print-depth* (ash 2 (if t 7 4)))
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
		(case meta
		  (left "[")
		  (right "]"))))))
  (set-pprint-dispatch (quote payload) (quote pprint-payload)))

(progn
  (defun pprint-hole (stream hole)
    (pprint-logical-block (stream nil)
      (let ((width (hole-width hole)))
	(format stream "<~a ~a>" (if (hole-active hole) #\@ #\#) width))))
  (set-pprint-dispatch (quote hole) (quote pprint-hole)))

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

(defun gen-brackets (data2 width)
  (let ((left-payload (make-payload :metadata (quote left)))
	(right-payload (make-payload :metadata (quote right))))
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

(defun enbracket (a data2 width)
  (let ((brackets (gen-brackets data2 width)))
    (splice-nodes brackets a)
    brackets))

(defun emit-cons (cell)
  (let ((base nil))
    (let ((last-node base))
      (let ((total-width 0))
	(labels ((add (new bracket-payload width)
		   (add-no-bracket (enbracket new
					      bracket-payload
					      width)))
		 (add-no-bracket (new)
		   (let ((next-base (node-prev new)))
		     (if last-node
			 (progn
			   (let ((hole (gen-hole)))
			     (add-hole hole))
			   (splice-nodes last-node new))
			 (setf base new))
		     (setf last-node next-base)))
		 (add-hole (new)
		   (let ((next-base (node-prev new)))
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
		    (setf last-node (node-prev newbase)))
		  (when list-cdr-p
		    (multiple-value-bind (nodes length) (emit-cons cdr)
		      (incf total-width length)
		      (add-no-bracket nodes))))))))
	(values base
		total-width)))))

(defun gen-hole ()
  (let ((hole (make-hole :width 0 :active nil)))
    (let ((node (make-node :payload hole)))
      (link-nodes node node)
      node)))

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
  (let ((cap 512))
    (dotimes (index cap)
      (scwu nodes x y)
      (let ((next (node-next-char (node-next nodes))))
	(if next
	    (progn
	      (incf x)
	      (setf nodes next))
	    (return))))))

(defun draw-nodes2 (x y nodes)
  (let ((cap 1024))
    (dotimes (index cap)
      (let ((next (node-next nodes)))
	(cond ((not next) (return))
	      (t
	       (let ((payload (node-payload next)))
		 (typecase payload
		   (character (scwu next x y) (incf x))
		   (payload)
		   (hole
		    (update-whole-hole next)
		    (width-prop next)
		    (when (hole-active payload)
		      (decf y 1)
		      (incf x (hole-width payload))))))
	       (setf nodes next)))))))
(defun draw-nodes2-reverse (x y nodes)
  (let ((cap 1024))
    (dotimes (index cap)
      (let ((next (node-prev nodes)))
	(cond ((not next) (return))
	      (t
	       (let ((payload (node-payload next)))
		 (typecase payload
		   (character (scwu next x y) (decf x))
		   (payload)
		   (hole
		    (update-whole-hole next)
		    (when (hole-active payload)
			   (incf y 1)
			   (incf x (- (hole-width payload)))))))
	       (setf nodes next)))))))

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
	      (let ((side (payload-metadata payload)))
		(if (eq (quote left) side)
		    (return (payload-data payload))
		    (if (eq (quote right) side)
			(setf node (jump-block-left node))))))
	  (find-enclosing-block-left (node-prev node))))))

(defun map-nodes (nodes function &optional (times 512))
  (dotimes (x times)
    (funcall function nodes)
    (setf nodes (node-next nodes))))

(progn
  (declaim (ftype (function (node (function (t) t)) (or null node)) find-node-forward))
  (defun find-node-forward (nodes test)
    (labels ((rec (node)
	       (if node
		   (let ((payload (node-payload node)))
		     (if (funcall test payload)
			 (values node payload)
			 (rec (node-next node))))
		   nil)))
      (rec nodes))))

(defun bracket-type (data)
  (when (or (eq data (quote left))
	    (eq data (quote right)))
    data))

(defun deactivate-hole (hole)
  (incf (hole-motion hole) (hole-width hole))
  (setf (hole-active hole) nil))
(defun activate-hole (hole)
  (decf (hole-motion hole) (hole-width hole))
  (setf (hole-active hole) t))

(defun width-prop (node)
  (let ((hole (node-payload node)))
    (let ((motion (hole-motion hole)))
      (unless (zerop motion)
	(setf (hole-motion hole) 0)
	(%width-prop (node-next node) motion)))))

(defun %width-prop (node value)
  (when node
    (let ((payload (node-payload node)))
      (typecase payload
	(hole (if (hole-active payload)
		  (incf (hole-width payload) value)
		  (incf (hole-motion payload) value))
	      node)
	(payload
	 (case (payload-metadata payload)
	   (left (%width-prop (node-next (jump-block-right node))
			     value))
	   (right (let ((bracket (payload-data payload)))
		    (decf (bracket-width bracket) value))
		  (%width-prop (node-next node) value))))
	(t (%width-prop (node-next node) value))))))

(defun test420 ()
  (map-nodes barfoo
	     (lambda (x)
	       (let ((payload (node-payload x)))
		 (typecase payload
		   (hole (setf (hole-active payload)
			       (if (zerop (random 2)) t nil)
			       (hole-width payload)
			       (random 10))))))))

(defun find-parent-hole (node)
  (%find-parent-hole (node-prev node) 0))

(defun %find-parent-hole (node depth)
  (when node
    (let ((payload (node-payload node)))
      (typecase payload
	(hole (values node depth))
	(payload
	 (case (payload-metadata payload)
	   (left (%find-parent-hole (node-prev (jump-block-left node)) depth))
	   (right (%find-parent-hole (node-prev node) (+ 1 depth)))))
	(t (%find-parent-hole (node-prev node) depth))))))

(defun generate-child-indentation-type (parent-type depth))

(defun update-hole-indentation-func (node)
  (multiple-value-bind (parent depth) (find-parent-hole node)
    (when parent
      (let ((parent-hole (node-payload parent)))
	(let ((parent-type (hole-generator parent-hole)))
	  (multiple-value-bind (child-type function)
	      (generate-child-indentation-type parent-type depth)
	    (let ((hole (node-payload node)))
	      (setf (hole-indentation-func hole) function
		    (hole-generator hole) child-type))))))))
(defun reindent-hole (node)
  (let ((hole (node-payload node)))
    (let ((func (hole-indentation-func hole)))
      (multiple-value-bind (width active) (if func (funcall func node) (values 0 nil))
	(set-node width active node)))))
(defun set-node (new-width active node)
  (let ((hole (node-payload node)))
    (if active
	(activate-hole hole)
	(deactivate-hole hole))
    (set-hole-width new-width node)))
(defun set-hole-width (new-width node)
  (let ((hole (node-payload node)))
    (if (hole-active hole)
	(let ((width (hole-width hole)))
	  (setf (hole-width hole) new-width)
	  (decf (hole-motion hole) (- new-width width))
	  (width-prop node))
	(setf (hole-width hole) new-width))))

(defun update-whole-hole (node)
  (when node
    (update-hole-indentation-func node)
    (reindent-hole node)
    (width-prop node)))
