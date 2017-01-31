(in-package :sandbox)


;;;;TODO::split into an interval mechanism and an id mechanism

(defun interval (a b)
  (cons a b))
(defun interval-start (interval)
  (car interval))
(defun interval-end (interval)
  (cdr interval))

;;;the free list is a list with nil as the car.
;;;the first nil is not modified, only the cdr.
;;;intervals are [] inclusive on both ends.
;;;ex: (2 . 2) means [2 , 2] which is just a point.

;;;create a fresh free-list for the ids
(defun fresh-id-free-list ()
  (cons nil (list (cons 0 most-positive-fixnum))))

;;;"use" an id - removing it from the free list
(defun id-use (n free)
  (declare (type fixnum n)
	   (type list free))
  (let ((nextcdr (cdr free)))
    (declare (type list nextcdr))
    (when nextcdr
      (tagbody
       rep
	 (let ((cell (car nextcdr)))
	   (let ((start (car cell))
		 (end (cdr cell)))
	     (declare (type fixnum start end))
	     (if (<= start n) ;;if n is behind, its over because start will only increase
		 (when (<= n end) ;;if it lies between inclusive, check
		   (if (= start n)
		       (progn
			 (if (= start end)
			     (setf (cdr free) (cdr nextcdr))
			     (setf (car cell) (the fixnum (1+ start)))))
		       (if (= end n) ;;n is greater than start
			   (progn
			     (if (= start end)
				 (setf (cdr free) (cdr nextcdr))
				 (setf (cdr cell) (the fixnum (1- end)))))
			   (progn ;;n is between start and end exclusive
			     (setf (car cell) (the fixnum (1+ n)))
			     (setf (cdr free)
				   (cons (cons start (the fixnum (1- n))) nextcdr)))))
		   (return-from id-use n))
		 (go end))))
	 (setf free nextcdr)
	 (setf nextcdr (cdr nextcdr))
	 (when nextcdr (go rep))
       end))))

;;;return an unused id, which is just the lower bound of the smallest interval.
;;;does not modify the free list
(defun id-allocate (free)
  (let ((first-cons (first (cdr free))))
    (car first-cons)))

;;;determine whether or not a specific id is available - unused
(defun id-unused-p (n free)
  (declare (type fixnum n))
  (dolist (cell (cdr free))
    (let ((start (car cell))
	  (end (cdr cell)))
      (declare (type fixnum start end))
      (if (<= start n) 
	  (when (<= n end)
	    (return t))
	  (return nil))))) ;;already past intervals which could contain n

;;;unuse an id, putting it back into the free list
;;;four scenarios can occur:
;;;a one-hole gap, which merges two intervals - remove a cons cell
;;;attach to the start of an interval - change car value to "n"
;;;attach to the end of an interval - change cdr value "n"
;;;not touch any interval - insert a (cons n n)
(defun id-free (n freelist)
  (declare (type fixnum n))
  (let ((n-1 (1- n)))
    (declare (type fixnum n-1))
    (let ((tail (cdr freelist)))
      (if tail ;;;when all the ids have been used ---
	  (let ((cell (car tail)))
	    (let ((first-start (car cell)))
	      (declare (type fixnum first-start))
	      (when (< n first-start)
		(if (= n (1- first-start))
		    (setf (car cell) n) ;;;extend downwards
		    (setf (cdr freelist) (cons (cons n n) tail))) ;;;push new interval
		(return-from id-free t)))
	;;;initial test over, looping between consecutive cells begins
	    (let ((next-tail (cdr tail)))
	      (if next-tail
		  (let* ((next-cell (car next-tail))
			 (left (cdr cell)) ;;the most of the lower interval
			 (right (car next-cell))) ;;the least of the greater interval
		    (declare (type fixnum left right))
		    (tagbody
		     rep
		       (if (<= n left) ;;it is is equal or less than its not valid
			   (return-from id-free nil)
			   (let ((merge-bottom (= left n-1)) ;;whether to merge lower
				 (merge-top (= (1- right) n))) ;;whether to merge higher
			     (if merge-top
				 (if merge-bottom
				     (progn
				       (setf (cdr tail) (cdr next-tail))
				       ;;remove the second interval
				       (setf (cdr cell) (cdr next-cell))
				       ;;combine first interval into a big one
				       (return-from id-free t))
				     (progn
				       (setf (car next-cell) n)
				       ;;lower the top interval
				       (return-from id-free t)))
				 (if merge-bottom
				     (progn
				       (setf (cdr cell) n)
				       ;;raise the bottom interval
				       (return-from id-free t))
				     (when (< n right)
				       ;;when its between but there is no merge, create another interval
				       (setf (cdr tail) (cons (cons n n) next-tail))
				       (return-from id-free t))))))

		       (let ((new-next (cdr next-tail)))
			 (when new-next ;;if it is nil then there is no cell to check
			   (setf tail next-tail
				 next-tail new-next
				 cell next-cell 
				 next-cell (car next-tail)
				 left (cdr cell)
				 right (car next-cell))
			   (go rep)))

	     ;;;;looping between two cells over
		       (let ((last-end (cdr next-cell)))
			 (declare (type fixnum last-end))
			 (when (< last-end n)
			   (if (= n-1 last-end)
			       (setf (cdr next-cell) n) ;;extend the last interval ever
			       (setf (cdr next-tail) (cons (cons n n) nil))) ;;make another
			   (return-from id-free t)))))
	      ;;;when there is only one interval and it is short of the most positive fixnum
		  (let ((first-end (cdr cell)))
		    (declare (type fixnum first-end))
		    (when (< first-end n)
		      (if (= n-1 first-end)
			  (setf (cdr cell) n) ;;extend the last interval ever
			  (setf (cdr tail) (cons (cons n n) nil))) ;;make another
		      (return-from id-free t))))))  
	  (setf (cdr freelist) (cons (cons n n) nil)))))
  (return-from id-free nil))
