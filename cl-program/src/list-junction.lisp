(setf *print-circle* t)
(defun make-node (&optional (payload nil))
  (flet ((unit (x)
	   (cons (cons payload x) nil)))
    (let* ((start-in (cons payload nil))
	   (start-out (cons start-in nil)))
      (setf (cdr start-in) (unit (unit (unit start-out))))
      start-out)))

(defun set-node (payload node)
  (let ((cell (load-time-value (let ((cell (cons nil nil)))
				 (setf (cdr cell) cell))
			       cell)))
    (setf (car cell) payload)
    (fill-node cell node)))

(defun set-node-side (unit item)
  (let ((item (car unit)))
    (setf (car item) item)
    (cdr item)))

(defun fill-node (items node)
  (set-node-side
   (set-node-side
    (set-node-side
     (set-node-side
      node
      (pop items))
     (pop items))
    (pop items))
   (pop items)))

(defun make-cons-node (&optional (car nil) (cdr nil))
  (make-node (cons car cdr)))

(defun turn-node (node)
  (cdr (car node)))

(defun reverse-node (node)
  (cdr (car (cdr (car node)))))

(defun connect-nodes (a b)
  (setf (cdr a) (reverse-node b)
	(cdr b) (reverse-node a)))

(defun node-payload (node)
  (car (car node)))

(defun vector-nodes (vector)
  (let* ((len (length vector))
	 (start-node (make-cons-node (aref vector 0)))
	 (end-node start-node))
    (dotimes (index (1- len))
      (let ((value (aref vector (1+ index))))
	(let ((new-node (make-cons-node value)))
	  (connect-nodes end-node (reverse-node new-node))
	  (setf end-node new-node))))
    (values start-node end-node)))

(defun nodes-vector (node)
  (let ((buffer (load-time-value (make-array 0 :fill-pointer 0 :adjustable t))))
    (setf (fill-pointer buffer) 0)
    (dolist (current-inner node)
      (vector-push-extend (car (car current-inner))
			  buffer))
    (coerce (subseq buffer 0 (fill-pointer buffer)) 'string)))
