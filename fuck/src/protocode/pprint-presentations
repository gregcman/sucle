#+nil
(defmethod print-object :before ((num fixnum) stream)
  (print "yolo" stream))
#+nil
(remove-method
 #'print-object
 (find-method #'print-object '(:before) '(fixnum stream)))



#+nil
(let ((*print-pprint-dispatch* *moar*))
  (set-pprint-dispatch 'integer (lambda (stream object)
				  (print "yolo")
				  (let ((*print-pretty* nil))
				    (princ object stream)))))

#+nil
(let ((objs *object-stream*))
    (when objs      
      (write-char +stx+ stream)
      
      (write-char +nul+ stream)
      (encode-num-char (fill-pointer objs) stream)
      (write-char +etx+ stream)
      (vector-push-extend object objs)))
(defparameter *object-stream* nil)
(defparameter *numeral-start* nil)

(defun make-char-buffer ()
  (make-array 0 :fill-pointer 0 :adjustable t :element-type (quote character)))

(defparameter footrue (make-char-buffer))
(defparameter fooannot0 (make-char-buffer))
(defparameter fooannot1 (make-char-buffer))
(defparameter obj-stream (make-array 0 :fill-pointer 0 :adjustable t))
(defparameter obj-stream2 (make-array 0 :fill-pointer 0 :adjustable t))

(defparameter *differences* nil)
(defparameter *num-info* nil)
(defparameter *extent-info* nil)


(defun encode-num-char (num stream)
  (write num :stream stream :pretty nil :readably t))
(defun pprint-fun-call (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^ ~:_~:I~@{~W~^ ~:_~}~:>")
           stream
           list))
(defparameter *original* (copy-pprint-dispatch *print-pprint-dispatch*))
(defparameter *moar* (copy-pprint-dispatch *print-pprint-dispatch*))
(set-pprint-dispatch (quote (cons (member function quote)))
		     (quote pprint-fun-call)
		     1
		     *original*)

(defun pprint-atomic (stream object)
  (let ((objs *object-stream*)
	(nums *numeral-start*))
    (when objs
      (progn
	(pprint-indent :block most-negative-fixnum stream)
	(pprint-newline :fill stream))
      (write-char (aref nums 0) stream)
      (encode-num-char (fill-pointer objs) stream)
      (write-char (aref nums 1) stream)
      (vector-push-extend object objs)
      (let ((func (pprint-dispatch object *original*)))
	(funcall func stream object))
      (write-char (aref nums 2) stream))))
(let ((*print-pprint-dispatch* *moar*))
  (set-pprint-dispatch (quote t)
		       (quote pprint-atomic) 0))
(defun ourprint (object char-buffer object-buffer)
  (let ((*print-pprint-dispatch* *moar*)
	(*object-stream* object-buffer))
    (write object :stream char-buffer)))

(defun compare-annots (&optional (foo0 fooannot0) (foo1 fooannot1))
  (let* ((len (length foo0))
	 (heat-vector (make-array len :element-type (quote (unsigned-byte 2)))))
    (dotimes (index len)
      (let ((a (aref foo0 index))
	    (b (aref foo1 index)))
	(when (char/= a b)
	  (let ((num (case a
		       (#\1 1)
		       (#\2 2)
		       (#\3 3)
		       (otherwise (error "a is not 1 or 2 or 3")))))
	    (setf (aref heat-vector index) num)))))
    heat-vector))

(defparameter *stringer* (make-array 0 :adjustable t :element-type (quote character) :fill-pointer 0))
(defun splitter (&optional (differences *differences*) (text fooannot0) (scratch *Stringer*))
  (let* ((len (length text))
	 (ids (make-array len :initial-element (quote quote)))
	 (stackno nil)
	 (index 0)
	 (max -1))
    (loop
       (when (>= index len)
	 (return))
       (let ((value (aref differences index)))
	 (case value
	   (0 (setf (aref ids index) (car stackno)))
	   (1 (setf (fill-pointer scratch) 0)
	      (loop
		 (incf index)
		 (let ((different? (aref differences index)))
		   (unless (zerop different?)
		     (let* ((numvalue (read-from-string scratch))
			    (payload numvalue))
		       (when (> numvalue max)
			 (setf max numvalue))
		       (push payload stackno)
		       (setf (aref ids index) (cons :start payload)))
		     (return))
		   (vector-push-extend (aref text index) scratch))))
	   (2 nil)
	   (3 (let ((avalue (pop stackno)))
		(setf (aref ids index) (cons :end avalue))))))
       (incf index))
    (let* ((objs-count (1+ max))
	   (obj-extents (make-array objs-count)))
      (dotimes (x objs-count)
	(setf (aref obj-extents x) (cons 0 0)))
      (dotimes (x len)
	(let ((value (aref ids x)))
	  (when (listp value)
	    (case (car value)
	      (:start (setf (car (aref obj-extents (cdr value))) x))
	      (:end (setf (cdr (aref obj-extents (cdr value))) x))))))
      (values
       ids
       obj-extents))))

(defun whitespace-p (char) 
  (or (char= char #\space)
      (char= char #\tab)
      (char= char #\Return)
      (char= char #\linefeed)
      (char= char #\Newline)))

(defun parallel-walk (&optional (text footrue) (walk-info *num-info*)
			(annot fooannot0))
  (let* ((annot-index 0)
	 (text-index 0)
	 (len (length text))
	 (annot-len (length annot))
	 (obj-ids (make-array len :initial-element nil)))
    (labels ((next-annot-char ()
	       (let ((value (aref walk-info annot-index)))
		 (unless (integerp value)
		     (incf annot-index)
		     (next-annot-char)))))
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

(defparameter *obj* nil)
(defparameter *more-flat-changes* nil)
(defparameter *info2* nil)
(defun ouprint2 (object)
  (setf *obj* object)
  (setf (fill-pointer obj-stream) 0)
  (setf (fill-pointer obj-stream2) 0)
  (setf (fill-pointer footrue) 0)
  (setf (fill-pointer fooannot0) 0)
  (setf (fill-pointer fooannot1) 0)
  (let ((*print-case* :downcase)
	(*print-readably* t)
	(*read-eval* nil))
    (with-output-to-string (str footrue)
      (write object :stream str))
    (let ((*numeral-start* "123"))
      (with-output-to-string (str fooannot0)
	(ourprint object str obj-stream)))
    (let ((*numeral-start* "456"))
      (with-output-to-string (str fooannot1)
	(ourprint object str obj-stream2))))
  (dotimes (x (length obj-stream))
    (unless (eq (aref obj-stream x)
		(aref obj-stream2 x))
      (error "object-streams unequal")))
  (setf *differences* (compare-annots fooannot0 fooannot1))
  (setf (values *num-info* *extent-info*)
	(splitter *differences* fooannot0))
  (Setf *more-flat-changes* (delistify *num-info* obj-stream))
  (setf *info2* (map (quote vector)
		     (lambda (x y)
		       (if x x y))
		     *more-flat-changes*
		     *num-info*))
  (let ((more-data (parallel-walk footrue *num-info* fooannot0)))
    (values footrue ;;;;the pretty printed text
	    more-data ;;;;mirror of text, where index maps to num-info
	    *num-info* ;;;;maps to the objects and object extent info
	    *extent-info* ;;;;extent info
	    obj-stream;;;;the objects
	    )))

(defun delistify (&optional (info *num-info*) (objs obj-stream))
  (declare (optimize (debug 3)))
  (let* ((len (length info))
	 (stack nil)
	 (id (make-array (length objs) :initial-element nil))
	 (changes (make-array len :initial-element nil)))
    (dotimes (index len)
      (let ((value (aref info index)))
	(when (and value (listp value))
	  (case (car value)
	    (:start
	     (let ((obj-index (cdr value)))
	       ;;	       (print obj-index)
	       (let ((perhaps-list? (aref objs obj-index)))
		 (if (and stack (consp perhaps-list?))
		     (block nil
		       (let ((first-obj-index (or (car stack) (return))))
			 (when (consp (aref objs first-obj-index))
			   (setf (aref id obj-index) first-obj-index)
			   (setf (aref changes index) :start)))) ;;copy if list parent
		     (push obj-index stack))))) ;;push only if there is not a parent list
	    (:end
	     (If (eql (car stack) ;;the stack
		      ;;the ending, no reason to pop if not equal because of above where
		      ;;nested lists are consolidated above
		      (cdr value))
		 (pop stack)
		 (setf (aref changes index) :end)))))
	(when (integerp value)
	  (let ((destroy? (aref id value)))
	    (when destroy?
	      (setf (aref changes index) destroy?))))))
    changes))

(defun eat-parens (&optional (info *info2*) (objs obj-stream) (chars fooannot0))
  (declare (optimize (debug 3)))
  (let* ((len (length info))
	 (eaters (make-array (length objs) :initial-element nil))
	 (eater-last-object (make-array (length objs) :initial-element nil))
	 (max-object -1))
    (dotimes (index len)
      (let ((value (aref info index)))
	(when (and value (listp value))
	  (case (car value)
	    (:start
	     (let ((obj-index (cdr value)))
	       (let ((maybe-list (aref objs obj-index)))
		 (when (and maybe-list (listp maybe-list))
		   (let ((eater (make-pareneater)))
		     (reset-pareneater maybe-list eater)
		     (setf (aref eaters obj-index) eater))))
	       (setf max-object index)))))
	(flet ((feed-em (p c)
		 (feed c p)
		 					;	 #+nil
		 (progn
		   (terpri)
		   (print c)
		   (print (top p))
		   (terpri))))
	  (when (integerp value)
	    (print (aref chars index))
	    (let ((eater (aref eaters value)))
	      (when eater
		(let ((last-feeding-time (aref eater-last-object value)))
		  (when last-feeding-time
		    (when (not (= max-object last-feeding-time))
		      (feed-em eater #\*))))
		(let ((char (aref chars index)))
		  (feed-em eater char))
		(setf (aref eater-last-object value) max-object)))))))))


(defstruct pareneater
  (stack-stack nil)
  (last nil)
  (parent nil)
  (period? nil)
  (parens 0)
  (lastchar #\Space))
;;(defparameter *stack* nil)
(defun stack (p)
  (first (pareneater-stack-stack p)))
(defun (setf stack) (x p)
  (setf (first (pareneater-stack-stack p)) x))
(defun top (p)
  (first (stack p)))
(defun (setf top) (x p)
  (setf (first (stack p)) x))
(defun poop (p)
  (pop (stack p)))
(defun puush (x p)
  (push x (stack p)))

;;(defparameter *stack-stack* nil)
(defun stack-puush (x p)
  (push (list x) (pareneater-stack-stack p)))
(defun stack-poop (p)
  (pop (pareneater-stack-stack p)))

;;(defparameter *last* 'cdr)
;;(defparameter *period?* nil)
;;(defparameter *parens* 0)
(defun anext (char p)
  (symbol-macrolet ((parens (pareneater-parens p))
		    (period? (pareneater-period? p))
		    (last (pareneater-last p))
		    (parent (pareneater-parent p)))
    (block nil
      (case char
	(#\.
	 (setf period? parens)
	 (return))
	(#\(
	 (incf parens)
	 (setf parent (top p))
	 (stack-puush (car (top p)) p)
	 ;;new frame for new list
	 
	 (setf
	  last 'car))
	(#\*
	 (when period?
	   (return))
	 (case last
	   ;;top is the base
	   (car)
	   ;;top is cdr, underneath is base, but need to advance
	   (cdr 
	    (poop p)
	    (setf (top p) (cdr (top p)))))
	 (setf parent (top p))
	 (puush (car (top p)) p)
	 
	 (setf
	  last 'car))
	(#\Space
	 (when period?
	   (return))
	 (case last
	   ;;top is car, under is base
	   (car (poop p))
	   ;;old list goes away, stuff under
	   (cdr (stack-poop p)))
	 (setf parent (top p))
	 (puush (cdr (top p)) p)
	 (setf
	  last 'cdr))
	(#\) 
	 (unless period?      
	   (case last
	     (car
	      (poop p) ;;;top is car, under is base
	      (setf parent (top p))
	      (puush (cdr (top p)) p))
	     (cdr
	      (stack-poop p)
	      (setf parent (top p))
	      (setf (top p) (cdr (top p)))))       
	   (setf
	    last 'cdr))
	 (when (eql period? parens)
	   (setf period? nil))
	 (decf parens))))))


;;(defparameter *last-char* #\Space)
(defun feed-char (x p)
  (anext x p)
  (setf (pareneater-lastchar p) x))


;;wherever there is one whitespace, there can be many
(defun feed (char p)
  (block nil
    (when (whitespace-p (pareneater-lastchar p))
      (when (whitespace-p char)
	;;eat nothing, go home
	;;whitespace often occurs in clumps
	(return))
      ;;(when (char= char #\))) closing parens do not appear on lines of their own
      )
    (when (char= #\( (pareneater-lastchar p))
      (when (whitespace-p char)
	;;eat nothing, go home
	;;whitespace can occur after opening parens, not closing
	(return)))
    (when (alphanumericp char)
      (feed-char #\* p)
      (return))
    (when (whitespace-p char)
      (setf char #\Space))
    (feed-char char p)))

(defun reset-pareneater (list p)
  (setf (pareneater-last p) 'cdr)
  (setf (pareneater-parent p) nil)
  (setf (pareneater-period? p) nil)
  (setf (pareneater-parens p) 0)
  (setf (pareneater-stack-stack p) (list (list (list list))))
  (setf (pareneater-lastchar p) #\Space))

(defparameter *cells*
  (case 1
    (0 (quote ((a) ((b) (c)) (d) ((e)) f (g) (h ((i) j (k) l (m) (n) o)) p)))
    (1 (quote ((a) ((b) . c) (d) ((e)) f (g) (h ((i) j (k) l (m) (n) . o)) . p)))))
(defparameter buf (make-array 0 :fill-pointer t :element-type (quote character)))
"((A) ((B) (C)) (D) ((E)) F (G) (H ((I) J (K) L (M) (N) O)) P)"
(defparameter *paren-eater* (make-pareneater))
(defun ttest (x)
  (Setf *cells* x)
  (progn
    (setf (fill-pointer buf) 0)
    (with-output-to-string (stream buf)
      (write x :stream stream :pretty t))))

(defun print-stack (x)
  (terpri)
  (print x)
  #+nil
  (dolist (a x)
    (print a)))

(defun test (&optional (p *paren-eater*))
  (reset-pareneater *cells* p)
  (flet ((info ()
	   (if t
	       (print (if t (top p)	;
			  (cons (pareneater-last p)
				(pareneater-parent p)))
		      )
	       (print-stack (pareneater-stack-stack p)))))
    (info)
    (dotimes (x (length buf))
      (let ((value (aref buf x)))
					;	(print value)
	
	(feed value p))
      (info))))


(defun random-shit (x)
  (flet ((asym ()
	   (intern (string (code-char (+ 65 (random 26)))))))
    (if (zerop x)
	(asym)
	(cons (case (random 2)
		(0 (random-shit (1- x)))
		(1 (asym)))
	      (case (random 3)
		(0 nil)
		(1 (random-shit (1- x)))
		(2 (asym)))))))
 
