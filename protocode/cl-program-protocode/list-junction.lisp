(in-package :sandbox)

(setf *print-circle* t)
(defun make-node (&optional (payload nil))
  (flet ((unit (x)
	   (cons (cons payload x) nil)))
    (let* ((start-in (cons payload nil))
	   (start-out (cons start-in nil)))
      (setf (cdr start-in) (unit (unit (unit start-out))))
      start-out)))


(defun set-node-side (unit item)
  (let ((item (car unit)))
    (setf (car item) item)
    (cdr item)))

(defun make-cons-node (&optional (car nil) (cdr nil))
  (make-node (cons car cdr)))

(defun nthfnc (func times x)
  (labels ((call (x left)
	     (if (zerop left)
		 x
		 (call (funcall func x)
		       (1- left)))))
    (call x times)))

(defun reverse-node (node)
  (nthfnc (function turn-node) 2 node))

(defun connect-nodes (a b)
  (when a
    (setf (cdr a) (reverse-node b)))
  (when b
    (setf (cdr b) (reverse-node a))))

(defun node-payload (node)
  (car (car node)))

(defun turn-node (node)
  (cdr (car node)))

(defun turn-cw-node (node)
  (nthfnc (function turn-node) 3 node))

;;;the top of the next node is connected to the bottom of the current node
(defun vector-nodes2 (vector)
  (let* ((len (length vector))
	 (start-node (make-cons-node (aref vector 0)))
	 (end-node start-node))
    (dotimes (index (1- len))
      (let ((value (aref vector (1+ index))))
	(let ((new-node (make-cons-node value)))
	  (connect-nodes end-node (reverse-node new-node))
	  (connect-nodes (turn-cw-node end-node)
			 (turn-node new-node))
	  (setf end-node new-node))))
    (values start-node end-node)))

(defun nodes-vector (node &optional (cap 128))
  (let ((buffer (load-time-value (make-array 0 :fill-pointer 0 :adjustable t))))
    (setf (fill-pointer buffer) 0)
    (block nil
      (dolist (current-inner node)
	(let ((value (car (car current-inner))))
	  (vector-push-extend (typecase value
				(character value)
				(fixnum (code-char (logand 255 value)))
				(symbol (aref (symbol-name value) 0)))
			      buffer))
;	(princ cap)
;	(princ " ")
	(when (zerop (decf cap))
	  (return))))
    (coerce buffer 'string)))

(defun vector-circular-node (vector)
  (multiple-value-bind (start end) (vector-nodes2 vector)
    (connect-nodes (reverse-node start) end)
    start))


(defun swap-nodes (a b)
  (connect-nodes (reverse-node (cdr a)) (reverse-node (cdr b)))
  (connect-nodes a b)
  a)

(defun node-splice (prev next)
  (swap-nodes prev
	      (reverse-node next)))

(defun node-disconnect (node)
  (connect-nodes (reverse-node (cdr node))
		 (reverse-node (cdr (reverse-node node))))
  (connect-nodes (reverse-node (cdr (turn-node node)))
		 (reverse-node (cdr (reverse-node (turn-node node)))))
  (connect-nodes node nil)
  (connect-nodes (turn-node node) nil)
  (connect-nodes (reverse-node node) nil)
  (connect-nodes (turn-cw-node node) nil)
  node)

(flet ((turnt (a b node)
	 (nthfnc (function turn-node) b
		 (cdr
		  (nthfnc (function turn-node) a node)))))
  (defun node-up (node)
    (turnt 1 3 node))
  (defun node-down (node)
    (turnt 3 1 node))
  (defun node-right (node)
    (cdr node))
  (defun node-left (node)
    (turnt 2 2 node)))

(progn
  (defun node-connect-up (bottom top)
    (connect-nodes (turn-node bottom)
		   (turn-cw-node top)))
  (defun node-connect-down (top bottom)
    (connect-nodes (turn-cw-node top)
		   (turn-node bottom)))
  (defun node-connect-left (right left)
    (connect-nodes (reverse-node right)
		   left))
  (defun node-connect-right (left right)
    (connect-nodes left (reverse-node right))))


(progno
 (let ((a (make-cons-node #\a))
       (b (make-cons-node #\b))
       (c (make-cons-node #\c))
       (d (make-cons-node #\d))
       (e (make-cons-node #\e)))
   (node-connect-right a b)
   (node-connect-up a c)
   (node-connect-left a d)
   (node-connect-down a e)
   (nodes-vector (turn-node e))))
