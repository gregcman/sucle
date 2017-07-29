(in-package :sandbox)

(progno
 (defparameter pos (load-time-value (cons 0 0)))
 (defparameter barfoo (atest))
 (defun reset-barfoo-indentation ()
   (let ((first-node (first-node barfoo)))
     (map-nodes first-node (lambda (payload) 
			     (when (typep payload (quote hole))
			       (zerofy-hole payload))))
     (set-hole-type (node-payload first-node) (quote form))))
 (defparameter *sleepy-time* 0.1)
 (defun atest ()
   (let ((circle (emit-cons (cons *test-tree* nil))))
     (let ((last (node-prev circle)))
       (setf (node-payload last) #\Space))
     (let ((barfoo (disconnect-node-prev circle)))
       (let ((start-hole (make-hole)))
	 (set-hole-type start-hole (quote nope))
	 (let ((start (make-node :payload start-hole)))
	   (link-nodes start barfoo)
	   start)))))
 (defun other-stuff ()
   (etouq
    (with-vec-params
	(vec-slots :rectangle (quote ((cx1 :x1) (cy1 :y1))))
      (quote (*point-rectangle*))
      (quote
       (progn
	 (let ((value (get-char cx1 cy1)))
	   (when (skey-r-or-p :Z)
	     (when (typep value (quote node))
	       (let ((payload (find-enclosing-block-left
			       (node-prev
				(bracket-left (find-enclosing-block-left value))))))
		 (print payload))))
	   (when (smice-p :left)
	     (when (typep value (quote node))
	       (setf (car pos) cx1
		     (cdr pos) cy1)
	       (setf barfoo value))))))))

   (block nil
     (when (skey-r-or-p :f)
       (let ((next (node-next barfoo)))
	 (multiple-value-bind (it )
	     (find-node-forward
	      (or next (return))
	      (function characterp))
	   (when it
	     (setf barfoo it))))))
   (block nil
     (when (skey-r-or-p :s)
       (let ((prev (node-prev barfoo)))
	 (multiple-value-bind (it)
	     (find-node-backward (or prev (return))
				 (function characterp))
	   (when it
	     (setf barfoo it))))))
   (when (skey-r-or-p :d)
     (multiple-value-bind (it) (char-search-down barfoo)
       (when it
	 (setf barfoo it))))
   (when (skey-r-or-p :e)
     (multiple-value-bind (it) (char-search-up barfoo)
       (when it
	 (setf barfoo it))))
   (clear-cam)
   (progn
     (multiple-value-bind (node hole)
	 (find-node-forward barfoo (function hole-p))
       (declare (ignorable node))
       (when node
	 (when (skey-j-p :kp-3)
	   (format t "~a" hole)
	   (terpri))
	 (when (skey-j-p :kp-1)
	   (reset-barfoo-indentation))
	 (when (skey-p :kp-8)
	   (when (hole-active hole)
	     (print hole)
	     (deactivate-hole hole)
	     (width-prop node)))
	 (when (skey-p :kp-5)
	   (unless (hole-active hole)
	     (print hole)
	     (activate-hole hole)
	     (width-prop node)))))
     (multiple-value-bind (node hole)
	 (find-node-forward barfoo (lambda (x) (typecase x
						 (hole (hole-active x)))))
       (declare (ignorable node))
       (when node
	 (when (skey-r-or-p :kp-6)
	   (incf (hole-width hole))
	   (decf (hole-motion hole))
	   (width-prop node))
	 (when (skey-r-or-p :kp-4)
	   (decf (hole-width hole))
	   (incf (hole-motion hole))
	   (width-prop node)))))
   (progn
     (when (skey-j-p :a)
       (let ((barfoo (find-enclosing-block-left (node-prev (bracket-left (find-enclosing-block-left barfoo))))))
	 (print barfoo)))
     (let ((x (car pos))
	   (y (cdr pos)))
       (scwu t x y)
       (draw-nodes2 (1+ x) y (node-next barfoo))
       (draw-nodes2-reverse (1- x) y (node-prev barfoo))))))

(defun atest ()
  (let ((circle (emit-cons (cons *test-tree* nil))))
    (let ((barfoo (disconnect-prev circle)))
      (let ((start-hole (make-instance (quote hole))))
	(link-nodes start-hole barfoo)
	start-hole))))

(defparameter barfoo (atest))
(defparameter pos (load-time-value (cons 0 0)))
(defun other-stuff ()
   (etouq
    (with-vec-params
	(vec-slots :rectangle (quote ((cx1 :x1) (cy1 :y1))))
      (quote (*point-rectangle*))
      (quote
       (progn
	 (let ((value (get-char cx1 cy1)))
	   (when (smice-p :left)
	     (when (typep value (quote node))
	       (setf (car pos) cx1
		     (cdr pos) cy1)
	       (setf barfoo value))))))))
   (clear-cam)
   (esdf)
   (let ((x (car pos))
	 (y (cdr pos)))
     (scwu t x y)
     (draw-nodes2 (1+ x) y (next barfoo))
     (draw-nodes2-reverse (1- x) y (prev barfoo))))

(defun esdf ()
  (progn
   (block nil
     (when (skey-r-or-p :f)
       (let ((next (next barfoo)))
	 (multiple-value-bind (it )
	     (find-node-forward
	      (or next (return))
	      (function (lambda (x) (typep x (quote char-node)))))
	   (when it
	     (setf barfoo it))))))
   (block nil
     (when (skey-r-or-p :s)
       (let ((prev (prev barfoo)))
	 (multiple-value-bind (it)
	     (find-node-backward (or prev (return))
				 (function (lambda (x) (typep x (quote char-node)))))
	   (when it
	     (setf barfoo it))))))
   (when (skey-r-or-p :d)
     (multiple-value-bind (it) (char-search-down barfoo)
       (when it
	 (setf barfoo it))))
   (when (skey-r-or-p :e)
     (multiple-value-bind (it) (char-search-up barfoo)
       (when it
	 (setf barfoo it))))))

(progno
  (when (skey-j-p :a)
    (let ((barfoo (find-enclosing-block-left (node-prev (bracket-left (find-enclosing-block-left barfoo))))))
      (print barfoo))))

(progno (when (skey-r-or-p :Z)
	  (when (typep value (quote node))
	    (quote (let ((payload (find-enclosing-block-left
				   (node-prev
				    (bracket-left (find-enclosing-block-left value))))))
		     (print payload))))))
(progno
 (multiple-value-bind (node hole)
     (find-node-forward barfoo (function hole-p))
    (declare (ignorable node))
    (when node
      (when (skey-j-p :kp-3)
	(format t "~a" hole)
	(terpri))
      (when (skey-j-p :kp-1)
	(reset-barfoo-indentation))
      (when (skey-p :kp-8)
	(when (hole-active hole)
	  (print hole)
	  (deactivate-hole hole)
	  (width-prop node)))
      (when (skey-p :kp-5)
	(unless (hole-active hole)
	  (print hole)
	  (activate-hole hole)
	  (width-prop node)))))
  (multiple-value-bind (node hole)
      (find-node-forward barfoo (lambda (x) (typecase x
					      (hole (hole-active x)))))
    (declare (ignorable node))
    (when node
      (when (skey-r-or-p :kp-6)
	(incf (hole-width hole))
	(decf (hole-motion hole))
	(width-prop node))
      (when (skey-r-or-p :kp-4)
	(decf (hole-width hole))
	(incf (hole-motion hole))
	(width-prop node)))))

(progn
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
		(princ ")")))))))))
