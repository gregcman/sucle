(defpackage #:glslgen
  (:use #:cl
	#:utility)
  (:export
   spaces
   main make-shader-vars funglsl shader-program-data dump-shader-program-data))
(in-package :glslgen)

;;;;;;;;;;;;;;;;

(defun list-walk (fn node)
  (cond ((atom node) (when node (funcall fn node)))
	(t (list-walk fn (car node))
	   (list-walk fn (cdr node)))))

(defun dump-string (fun tree)
  (with-output-to-string (str)
    (list-walk (lambda (x)
		 (when (symbolp x) (setf x (funcall fun x)))
		 (write x
			:stream str
			:escape nil))
	       tree)))

(defparameter *newline* "
")
(defparameter *semicolon* ";
")
(defun statement (&rest rest)
  (mapcar (lambda (x) (list x *semicolon*)) rest))
(defun version (x)
  (list "#version " x *newline*))
(defun brackets (&rest x)
  (list
   "{
" x
"}
"))
(defun spaces (body &optional (divider " "))
  (let (a
	(first t))
    (dolist (item body)
      (if first
	  (setf first nil)
	  (push divider a))
      (push item a))
    (nreverse a)))
(defun blogn (&rest body)
  (brackets (apply #'statement body)))

(defun funglsl (name args)
  (list name "(" (spaces args ", ") ")"))

(defun comment (&rest args)
  (list "/*" args "*/"))

(defclass shader-vars ()
    ((in :accessor shader-vars-in
	 :initarg :in)
     (out :accessor shader-vars-out
	  :initarg :out)
     (temp :accessor shader-vars-temp
	   :initarg :temp)
     (program :accessor shader-vars-program
	      :initarg :program)))

(defun make-shader-vars (&key out in temp program)
  (let ((inhash (make-hash-table :test 'eq))
	(outhash (make-hash-table :test 'eq))
	(temphash (make-hash-table :test 'eq)))
    (flet ((structure-data (iter-list hash)
	     (dolist (item iter-list)
	       (let ((name (pop item))
		     (type (pop item))
		     (initform (pop item)))
		 (let ((data (list :type type)))
		   (when initform (setf data (list* :initform initform data)))
		   (setf (gethash name hash)
			 data))))))
      (structure-data in inhash)
      (structure-data out outhash)
      (structure-data temp temphash))
    (make-instance 'shader-vars :in inhash :out outhash :temp temphash :program program)))

;;between the vertex shader and fragment shader, the variables which correspond
;;can be uniforms or varyings
;;each pait is an (output . input) pair
(defun bind-shader-varyings (output input pairs)
  (let ((output-output-vars (shader-vars-out output))
	(input-input-vars (shader-vars-in input)))
    (dolist (pair pairs)
      (let ((cell (cons nil nil)))
	(symbol-macrolet ((outlist (gethash (car pair) output-output-vars))
			  (inlist (gethash (cdr pair) input-input-vars)))
	  (setf (getf outlist :string) cell
		(getf inlist :string) cell
		(getf outlist :qualifier) :varying
		(getf inlist :qualifier) :varying))))))

(defparameter *genvar-counter* nil)
(defun newname ()
  (with-output-to-string (str)
    (write "G" :stream str :escape nil)
    (write (incf *genvar-counter*) :stream str)))
(defun flood-names (hash)
  (dohash (key value) hash
    (let ((stringcell (getf value :string)))
      (cond (stringcell ;;its shared when its a list
	     (unless (car stringcell)
	       (setf (car stringcell) (newname))))
	    (t (setf (getf (gethash key hash) :string)
		     (newname)))))))
(defun fill-vars (vs frag)
  (let ((*genvar-counter* 0))
    (flood-names (shader-vars-in vs))
    (flood-names (shader-vars-out vs))
    (flood-names (shader-vars-in frag))
    (flood-names (shader-vars-out frag))
    (flood-names (shader-vars-temp vs))
    (flood-names (shader-vars-temp frag))))

;;;attach qualifiers
(defun qualify (hash connected not-connected)
  (dohash (k v) hash
    (unless (getf v :qualifier)
      (let ((string-cell (getf v :string)))
	(setf (getf (gethash k hash) :qualifier)
	      (if (consp string-cell) ;;means that the variable is shared 
		  connected ;;attribute or varying
		  not-connected))))))
(defparameter *glsl-version* 110) ;;arbitrary, overwritten
(defparameter *stage* nil)
(defun qualify-version (hash)
  (dohash (k v) hash
    (flet ((setqualifier (q)
	     (setf (getf (gethash k hash) :qualifier) q)))
      (if (<= 130 *glsl-version*)
	  (case (getf v :qualifier)
	    (:attribute (setqualifier :in))
	    (:varying (ecase *stage*
			(:vertex-shader (setqualifier :out))
			(:fragment-shader (setqualifier :in)))))
	  (case (getf v :qualifier)
	    (:in (ecase *stage*
		   (:vertex-shader (setqualifier :attribute))
		   (:fragment-shader (setqualifier :varying))))
	    (:out (ecase *stage*
		    (:vertex-shader (setqualifier :out))
		    (:fragment-shader (setqualifier nil)))))))))

(defun dump-type (varname type)
  (typecase type
    (cons (list (pop type) " " varname "[" (car type) "]"))
    (otherwise (list type " " varname))))
(defun dumpvars (hash)
  (let (acc)
    (dohash (k v) hash
      (declare (ignorable k))
      (let ((list (list (dump-type (uncar (getf v :string))
				   (getf v :type)))))
	(flet ((dump-initforms ()
		 (let ((initform (getf v :initform)))
		   (when initform
		     (nconc list (list "=" initform))))))
	  (let ((qualifier (case (getf v :qualifier)
			     (:in "in") ;;no initform
			     (:out "out") ;;no initform
			     (:varying "varying") ;;no initforms
			     (:attribute "attribute") ;;no initforms
			     (:const (dump-initforms) "const")  ;;initforms
			     (:uniform (when (>= *glsl-version* 120)
					 (dump-initforms))
				       "uniform") ;;initforms
			     (otherwise (dump-initforms) nil)))) ;;initforms
	    (when qualifier (push qualifier list))
	    (push (spaces list) acc)))))
    acc))

;;

(defun specify-attribs (vs names) ;;can specify the attrib number, or not
  (dolist (name names)
    (setf name (uncar name))
    (setf (getf (gethash name (shader-vars-in vs))
		:qualifier)
	  :attribute)))

;;needed? ;;preemptive?
(defun specify-const (shader names)
  (dolist (name names)
    (setf (getf (gethash name (shader-vars-in shader))
		:qualifier)
	  :const)))

(defun uncar (var)
  (if (consp var)
      (car var)
      var))

(defun getname (name shader)
  (let ((value
	 (getf (gethash name (shader-vars-in shader))
	       :string)))
    (uncar value)))

(defun dump-shader (shader-vars)
  (lambda (x)
    (block nil
      (let ((value (or (gethash x (shader-vars-in shader-vars))
		       (gethash x (shader-vars-out shader-vars))
		       (gethash x (shader-vars-temp shader-vars))
		       (when (eq x :gl-frag-color)
			 (return
			   (if (> *glsl-version* 120)
			       "roloCgarF_lg"
			       "gl_FragColor")))
		       (when (string= x "texture2D")
			 (return
			   (if (>= *glsl-version* 150)
			       "texture"
			       x))))))
	(cond (value
	       (setf value (getf value :string))
	       (if (listp value)
		   (car value)
		   value))
	      (t (error "shader vars not full")))))))

(defun qualify-and-dump (hash connected not-connected)
  (qualify hash connected not-connected)
  (qualify-version hash)
  (apply #'statement
	 (dumpvars hash)))

(defun dump-vs2 (shader-vars)
  (let ((*stage* :vertex-shader))
    (dump-string
     (dump-shader shader-vars)
     (list
      (version *glsl-version*)
      (qualify-and-dump (shader-vars-in shader-vars) :attribute :uniform)
      (qualify-and-dump (shader-vars-out shader-vars) :varying nil)
      (shader-vars-program shader-vars)))))

(defun dump-frag2 (shader-vars)
  (let ((*stage* :fragment-shader))
    (dump-string
     (dump-shader shader-vars)
     (list
      (version *glsl-version*)
      (when (= *glsl-version* 100) 
	(statement
	 (spaces '("precision" "mediump" "float"))))
      (when (> *glsl-version* 120)
	(statement
	 (spaces `("out" "vec4" :gl-frag-color))))
      (qualify-and-dump (shader-vars-in shader-vars) :varying :uniform)
      (qualify-and-dump (shader-vars-out shader-vars) nil nil)
      (shader-vars-program shader-vars)))))

(defun bind-shader-uniforms (vs frag specs)
  (let (a)
    (flet ((setcell (hash var cell)
	     (setf (getf (gethash var hash) :string) cell
		   (getf (gethash var hash) :qualifier) :uniform)))
      (dolist (spec specs)
	(let ((name (pop spec)))
	  (let ((newcell (cons nil nil)))
	    (dolist (per-stage spec)
	      (destructuring-bind (stage var) per-stage
		(push (cons name newcell) a)
		(case stage
		  (:fragment-shader (setcell frag var newcell))
		  (:vertex-shader (setcell vs var newcell)))))))))
    a))


(defun dump-test (vs frag attribs varyings uniforms)
  (specify-attribs vs attribs)
  ;;bind varyings
  (bind-shader-varyings vs frag varyings)
  ;;bind uniforms
  (let ((a
	 (bind-shader-uniforms (shader-vars-in vs)
			       (shader-vars-in frag)
			       uniforms)))
    (fill-vars vs frag)
    (values (dump-vs2 vs)
	    (dump-frag2 frag)
	    a)))

(defclass shader-program-data ()
  ((vs :accessor shader-program-data-vs
       :initarg :vs)
   (vs-string :accessor shader-program-data-vs-string)
   (frag :accessor shader-program-data-frag
	 :initarg :frag)
   (frag-string :accessor shader-program-data-frag-string)
   (varyings :accessor shader-program-data-varyings
	     :initarg :varyings
	     :initform '())
   (uniforms :accessor shader-program-data-uniforms
	     :initarg :uniforms
	     :initform '())
   (attributes :accessor shader-program-data-attributes
	       :initarg :attributes
	       :initform '())
   (version :accessor shader-program-data-version
	    :initarg :version
	    :initform *glsl-version*)
   (vars-attributes :accessor shader-program-data-vars-attributes)
   (raw-attributes :accessor shader-program-data-raw-attributes)
   (alias-uniform :accessor shader-program-data-alias-uniform)
   (raw-uniform :accessor shader-program-data-raw-uniform)))

(defun cook-aliased-uniforms (list)
  (let (a)
    (dolist (item list)
      (destructuring-bind (name string) item
	  (when string
	    (push (cons name string) a))))
    a))

(defun dump-shader-program-data (data)
  (let ((*glsl-version* (shader-program-data-version data)))
    (multiple-value-bind (vs-string frag-string uniforms)
	(dump-test (shader-program-data-vs data)
		   (shader-program-data-frag data)
		   (shader-program-data-attributes data)
		   (shader-program-data-varyings data)
		   (shader-program-data-uniforms data))
      ;;(print (list vs-string frag-string uniforms))
      (multiple-value-prog1
	  (setf (values (shader-program-data-vs-string data)
			(shader-program-data-frag-string data)
			(shader-program-data-alias-uniform data))
		(values vs-string
			frag-string
			uniforms))
	(setf (shader-program-data-raw-uniform data)
	      (glslgen::cook-aliased-uniforms uniforms))))))

(defun preprocess-attribs (pairs)
  (let (acc)
    (let ((explicits (sort (remove-if-not #'consp pairs) #'< :key #'cdr))
	  (non-explicits (remove-if #'consp pairs))
	  (count 0)
	  (last-attrib-num -1))
      (tagbody
       again
	 (when explicits
	   (let ((explicit (car explicits)))
	     (let ((attrib-num (cdr explicit)))
	       (when (> 0 attrib-num)
		 (error "invalid attribute number: ~a" attrib-num))
	       (cond ((= count attrib-num)
		      (when (= last-attrib-num attrib-num)
			(error "duplicate attribute numbers: ~a" attrib-num))
		      (setf last-attrib-num attrib-num)
		      (incf count)
		      (push explicit acc)
		      (setf explicits (cdr explicits))
		      (go again))))))
	 (cond (non-explicits
		(push (cons (pop non-explicits) count) acc)
		(incf count)
		(go again))
	       (explicits (setf count (cdr (car explicits)))
			  (go again)))))
    (nreverse acc)))

(defparameter *debug-shader-gen*
  ;;t
  nil
  )
(defun gl-dump-shader (data)
  (let* ((preprocessed (preprocess-attribs (shader-program-data-attributes data)))
	 (raw-attributes
	  (let ((in (shader-program-data-vs data)))
	    (mapcar (lambda (x) (cons (getname (car x) in)
				      (cdr x)))
		    preprocessed))))
    (setf (shader-program-data-vars-attributes data) preprocessed)
    (setf (shader-program-data-raw-attributes data) raw-attributes)
    (let ((vs (shader-program-data-vs-string data))
	  (frag (shader-program-data-frag-string data)))
      (when *debug-shader-gen*
	(print vs)
	(print frag))
      (glhelp:make-shader-program-from-strings
       vs
       frag
       raw-attributes))))

(defun ashader (&rest rest &key &allow-other-keys)
  (let (a)
    (setf
     a
     (apply
      #'make-instance
      'glslgen:shader-program-data
      rest))
    (glslgen:dump-shader-program-data a)
    a))
(export '(ashader))
