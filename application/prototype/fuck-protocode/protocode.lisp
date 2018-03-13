(in-package :fuck)

(defun flush-cells (&optional (text footrue) (walk-info *num-info*)
			(annot fooannot0))
  (let* ((annot-index 0)
	 (text-index 0)
	 (len (length text))
	 (annot-len (length annot))
	 (obj-ids (make-array len :initial-element nil))
	 (astack nil)
	 (topstack nil))
    (labels ((next-annot-char ()
	       (let ((value (aref walk-info annot-index)))
		 (if (integerp value)
		     (return-from next-annot-char)
		     (progn
		       (when (listp value)
			 (case (car value)
			   (:start (push (cdr value) astack))
			   (:end (pop astack)))
			 (setf topstack (car astack)))
		       (incf annot-index)
		       (next-annot-char))))))
      (loop
	 (when (or (>= text-index len)
		   (>= annot-index annot-len))
	   (return))
	 (next-annot-char)
	 ;;	   (next-annot-alpha)
	 ;;	   (next-text-alpha)
	 (let ((textchar (aref text text-index))
	       (annot-char (aref annot annot-index)))
;;	   (print (list topstack text-index annot-index textchar annot-char))
	   (cond ((char= textchar annot-char)
		  (setf (aref obj-ids text-index)
			annot-index)
		  (incf text-index)
		  (incf annot-index))
		 (t
		  (let ((annot-white (whitespace-p annot-char))
			(text-white (whitespace-p textchar)))
		    (if annot-white
			(if text-white
			    (progn (setf (aref obj-ids text-index)
					 (cons annot-index nil))
				   (incf text-index)
				   (incf annot-index))
			    (incf annot-index))
			(if text-white
			    (incf text-index)
			    (progn (incf annot-index)
 ;;;fail robustly when someone uses a per-line-prefix in pprint-logical-block
				   (error "unequal"))))))))))
    obj-ids))

(defun paren-eater (init-list)
  (setf init-list (list init-list))
  (let ((value-buffer (make-array 0 :adjustable t :fill-pointer 0))
	(char-buffer (make-array 0 :adjustable t :fill-pointer 0 :element-type (quote character)))
	(list-stack nil)
	(prev-char #\Space)
	(current-cons init-list))
    (labels
	((value-add (x)
	   (vector-push-extend x value-buffer))
	 (char-add (x)
	   (vector-push-extend x char-buffer)))
      (lambda (curr-char)
	(case curr-char
	  (#\Space
	   (case prev-char
	     (#\Space)
	     (#\.)
	     (#\()
	     (#\))
	     (otherwise)))
	  (#\.
	   (case prev-char
	     (#\Space)
	     (#\.)
	     (#\()
	     (#\))
	     (otherwise)))
	  (#\(
	   (case prev-char
	     (#\Space)
	     (#\.)
	     (#\()
	     (#\))
	     (otherwise)))
	  (#\)
	   (case prev-char
	     (#\Space)
	     (#\.)
	     (#\()
	     (#\))
	     (otherwise)))
	  (otherwise
	   (case prev-char
	     (#\Space)
	     (#\.)
	     (#\()
	     (#\))
	     (otherwise))))
	
	(setf prev-char curr-char)
	
	(setf current-cons (car list-stack))
	(print value)
	(print value-buffer)))))

(defun test (x)
  (let ((str (write-to-string x :pretty t)))
    (princ str)
    (map nil (paren-eater x) str)))


(labels ((next-annot-alpha ()
	   (if (whitespace-p (aref annot annot-index))
	       (progn (progn (incf annot-index)
			     (next-annot-char))
		      (next-annot-alpha))
	       annot-index))
	 (next-text-alpha ()
	   (if (whitespace-p (aref text text-index))
	       (progn (incf text-index)
		      (next-text-alpha))
	       text-index))))


(defconstant +largest-char+ 128)

 (loop
     (multiple-value-bind (small code) (floor num (- +largest-char+ 4))
       (write-char (code-char (+ 4 code)) stream)
       (when (zerop small)
	 (return))
       (setf num small)))


(set-pprint-dispatch (quote (cons (member yolo)))
		     (if t
			 nil
			 (lambda (stream object)
			   (pprint-logical-block (stream nil :per-line-prefix "asdfasdf")
			     (dolist (x object)
			       (pprint-indent :current -3)
			       (pprint-newline :linear)
			       (write x :stream stream))))))

(set-pprint-dispatch (quote nil) nil)
