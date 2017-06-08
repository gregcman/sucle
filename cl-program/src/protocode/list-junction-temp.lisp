(in-package :sandbox)

(progno
 (defun set-node (payload node)
   (let ((cell (load-time-value (let ((cell (cons nil nil)))
				  (setf (cdr cell) cell))
				cell)))
     (setf (car cell) payload)
     (fill-node cell node)))

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
 (defun vector-nodes (vector)
  (let* ((len (length vector))
	 (start-node (make-cons-node (aref vector 0)))
	 (end-node start-node))
    (dotimes (index (1- len))
      (let ((value (aref vector (1+ index))))
	(let ((new-node (make-cons-node value)))
	  (connect-nodes end-node (reverse-node new-node))
	  (setf end-node new-node))))
    (values start-node end-node))))

(prongo (defun test ()
	  (setf wombo (vector-circular-node "wombo "))
	  (print (nodes-vector wombo))
	  (setf hello (vector-circular-node "hello "))
	  (print (nodes-vector hello))
	  (swap-nodes (reverse-node 
		       (cdr 
			(reverse-node hello))) 
		      (reverse-node wombo))
	  (print (nodes-vector hello))
	  (print (nodes-vector wombo))
	  ))
