(in-package :sandbox)

(defun draw-world ()
  (declare (optimize (speed 3) (safety 0)))
  (with-hash-table-iterator (next *g/chunk-call-list*)
    (loop
       (multiple-value-bind (more? key value) (next)
	 (declare (ignore key))
	 (unless more? (return nil))
	 (gl:call-list value)))))
(progn
  (defparameter *g/chunk-call-list* (make-hash-table :test 'eq))
  (defun get-chunk-display-list (name)
    (gethash name *g/chunk-call-list*))
  (defun set-chunk-display-list (name list-num)
    (setf (gethash name *g/chunk-call-list*) list-num))
  (defun remove-chunk-display-list (name)
    (remhash name *g/chunk-call-list*)))

(defun update-world-vao (x y z)
  (maphash (lambda (k v)
	     (declare (ignorable k))
	     (gl:delete-lists v 1)
	     (remove-chunk-display-list k))
	   *g/chunk-call-list*)
  (map nil #'dirty-push
       (sort (alexandria:hash-table-keys world::chunkhash) #'< :key
	     (lambda (position)
	       (multiple-value-bind (i j k) (world:unhashfunc position)
		 ((lambda (x0 y0 z0 x1 y1 z1)
		    (let ((dx (- x1 x0))
			  (dy (- y1 y0))
			  (dz (- z1 z0)))
		      (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
		  x y z
		  (- i 8)
		  (- k 8)
		  (- j 8)))))))

(defun update-chunk-mesh (len coords iter)
  (when coords
    (let ((old-call-list (get-chunk-display-list coords)))     
      (when old-call-list (gl:delete-lists old-call-list 1)))
    (if (zerop len)
	(remove-chunk-display-list coords)	  
	(set-chunk-display-list
	 coords
	 (glhelp:with-gl-list
	   (gl:with-primitives :quads	     
	     (with-vec (a b c) (iter)
	       (flush-my-iterator a
		 (flush-my-iterator b
		   (flush-my-iterator c
		     (mesh-chunk len a b c)))))))))))

(defun mesh-chunk (times a b c)
  (declare (type fixnum times))
  (declare (optimize (speed 3) (safety 0)))
  (iter-ator:bind-iterator-in
   (xyz single-float) a
   (iter-ator:bind-iterator-in
    (uv single-float) b
    (iter-ator:bind-iterator-in
     (dark single-float) c
     (dotimes (x times)
       (%gl:vertex-attrib-3f 2 (xyz) (xyz) (xyz))
       (%gl:vertex-attrib-2f 8 (uv) (uv))
       (%gl:vertex-attrib-1f 0 (dark));;;zero always comes last?
       )))))

(defun attrib-buffer-iterators ()
  (map-into (make-array 3 :element-type t :initial-element nil)
	    (function my-iterator)))

(setf lparallel:*kernel* (lparallel:make-kernel 4))
(defparameter *achannel* (lparallel:make-channel))
(defun designatemeshing ()
  (loop
     (multiple-value-bind (value success-p) (lparallel:try-receive-result *achannel*)
       (if success-p
	   (apply (car value) (cdr value))
	   (return))))
  (let ((thechunk (dirty-pop)))
    (when thechunk
      (let ((lparallel:*task-category* 'mesh-chunk))
	(lparallel:submit-task
	 *achannel*
	 (lambda (iter space)
	   (map nil (lambda (x) (free-my-iterator-memory x)) iter)
	   (multiple-value-bind (a b c) (chunk-shape thechunk iter)
	     (%list space #'update-chunk-mesh a b c)))
	 (attrib-buffer-iterators)
	 (make-list 4))))))

;;;;keeping track of the changes to the world
(progn
  (defparameter dirtychunks (q:make-uniq-q))
  (defun dirty-pop ()
    (q:uniq-pop dirtychunks))
  (defun dirty-push (item)
    (q:uniq-push item dirtychunks))
  (defun block-dirtify (i j k)
    (dirty-push (world:chop (world:chunkhashfunc i k j)))))

(defun setblock-with-update (i j k blockid new-light-value)
  (let ((old-blockid (world:getblock i j k)))
    (when (/= blockid old-blockid)
      (let ((old-light-value (world:getlight i j k)))
	(when (setf (world:getblock i j k) blockid)
	  (when (< new-light-value old-light-value)
	    (de-light-node i j k))
	  (unless (= old-light-value new-light-value)
	    (setf (world:getlight i j k) new-light-value))
	  (sky-de-light-node i j k)
	  (unless (zerop new-light-value)
	    (light-node i j k))
	  (flet ((check (a b c)
		   (light-node (+ i a) (+ j b) (+ k c))
		   (sky-light-node (+ i a) (+ j b) (+ k c))))
	    (check -1 0 0)
	    (check 1 0 0)
	    (check 0 -1 0)
	    (check 0 1 0)
	    (check 0 0 -1)
	    (check 0 0 1))
	  (block-dirtify i j k))))))

(defun plain-setblock (i j k blockid new-light-value &optional (new-sky-light-value 0))
  (when (setf (world:getblock i j k) blockid)
    (setf (world:getlight i j k) new-light-value)
    (setf (world:skygetlight i j k) new-sky-light-value)
    (block-dirtify i j k)))
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
(defun main (&rest body)
  (list "void main()" (apply #'blogn body)))
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

(defun funglsl (name &rest args)
  (list name "(" (spaces args ", ") ")"))

(defun comment (&rest args)
  (list "/*" args "*/"))

(defclass shader-vars ()
    ((in :accessor shader-vars-in
	 :initarg :in)
     (out :accessor shader-vars-out
	  :initarg :out)
     (temp :accessor shader-vars-temp
	   :initarg :temp)))

(defun make-shader-vars (&key out in temp)
  (let ((inhash (make-hash-table :test 'eq))
	(outhash (make-hash-table :test 'eq))
	(temphash (make-hash-table :test 'eq)))
    (dolist (item in) (setf (gethash (car item) inhash) (cons :type (cdr item))))
    (dolist (item out) (setf (gethash (car item) outhash) (cons :type (cdr item))))
    (dolist (item temp) (setf (gethash (car item) temphash) (cons :type (cdr item))))
    (make-instance 'shader-vars :in inhash :out outhash :temp temphash)))

;;between the vertex shader and fragment shader, the variables which correspond
;;can be uniforms or varyings
;;each pait is an (output . input) pair
(defun bind-shader-vars (output input pairs type)
  (let ((output-output-vars (shader-vars-out output))
	(input-input-vars (shader-vars-in input)))
    (dolist (pair pairs)
      (let ((cell (cons nil (ecase type
			      (:uniform t)
			      (:varying nil)))))
	(symbol-macrolet ((outlist (gethash (car pair) output-output-vars))
			  (inlist (gethash (cdr pair) input-input-vars)))
	  (setf (getf outlist :string) cell
		(getf inlist :string) cell
		(getf outlist :qualifier) type
		(getf inlist :qualifier) type))))))

(defparameter *genvar-counter* nil)
(defun fill-vars (vs frag)
  (let ((*genvar-counter* 0))
    (labels ((newname ()
	       (with-output-to-string (str)
		 (write "G" :stream str :escape nil)
		 (write (incf *genvar-counter*) :stream str)))
	     (flood-names (hash)
	       (with-hash-table-iterator (next hash)
		 (loop (multiple-value-bind (more? key value) (next)
			 (unless more? (return))
			 (let ((stringcell (getf value :string)))
			   (cond (stringcell ;;its shared when its a list
				  (unless (car stringcell)
				    (setf (car stringcell) (newname))))
				 (t (setf (getf (gethash key hash) :string)
					  (newname))))))))))
      (flood-names (shader-vars-in vs))
      (flood-names (shader-vars-out vs))
      (flood-names (shader-vars-in frag))
      (flood-names (shader-vars-out frag))
      (flood-names (shader-vars-temp vs))
      (flood-names (shader-vars-temp frag)))))

(defun dump-shader (shader-vars)
  (lambda (x)
    (let ((value (or (gethash x (shader-vars-in shader-vars))
		     (gethash x (shader-vars-out shader-vars))
		     (gethash x (shader-vars-temp shader-vars)))))
      (cond (value
	     (setf value (getf value :string))
	     (if (listp value)
		 (car value)
		 value))
	    (t (error "shader vars not full"))))))

;;;attach qualifiers
(defun qualify (hash connected not-connected)
  (with-hash-table-iterator (next hash)
    (loop (multiple-value-bind (more? k v) (next)
	    (unless more? (return))
	    (unless (getf v :qualifier)
	      (let ((string-cell (getf v :string)))
		(setf (getf (gethash k hash) :qualifier)
		      (if (consp string-cell) ;;means that the variable is shared 
			  (if (cdr string-cell)
			      :uniform
			      connected) ;;attribute or varying
			  not-connected))))))))

(defun dumpvars (hash)
  (let (acc)
    (with-hash-table-iterator (next hash)
      (loop (multiple-value-bind (more? k v) (next)
	      (declare (ignorable k))
	      (unless more? (return))
	      (let ((list (list (getf v :type) (uncar (getf v :string)))))
		(let ((qualifier (case (getf v :qualifier)
				   (:varying "varying")
				   (:attribute "attribute")
				   (:const "const")
				   (:uniform "uniform")
				   (otherwise nil))))
		  (when qualifier (push qualifier list))
		  (push (spaces list) acc))))))
    acc))

;;

(defun specify-attribs (vs names)
  (dolist (name names)
    (setf (getf (gethash name (shader-vars-in vs))
		:qualifier)
	  :attribute)))

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

(defun dump-vs ()
  (make-shader-vars
   :out '((color-out "float")
	  (texcoord-out "vec2"))
   :in '((position "vec4")
	 (texcoord "vec2")
	 (color "float")
	 (projection-model-view "mat4"))))

(defun qualify-and-dump (hash connected not-connected)
  (qualify hash connected not-connected)
  (apply #'statement
	 (dumpvars hash)))

(defun dump-vs2 (shader-vars)
  (dump-string
   (dump-shader shader-vars)
   (list
    (version 100)
    (statement
     (spaces '("precision" "lowp" "float"))
     (spaces '("precision" "lowp" "int")))
     ;;in
    (qualify-and-dump (shader-vars-in shader-vars) :attribute :uniform)
    (qualify-and-dump (shader-vars-out shader-vars) :varying nil)
    (main
     (spaces `("gl_Position" "=" projection-model-view "*" position))
     (spaces `(color-out "=" color))
     (spaces `(texcoord-out "=" texcoord))))))

(defun dump-frag ()
  (make-shader-vars
   :in '((texcoord "vec2")
	 (color "float")
	 (sampler "sampler2D"))))

(defun dump-frag2 (shader-vars)
  (let ((temp "pixdata"))
    (dump-string
     (dump-shader shader-vars)
     (list
      (version 100)
      (statement
       (spaces '("precision" "lowp" "float"))
       (spaces '("precision" "lowp" "int")))
       ;;in
      (qualify-and-dump (shader-vars-in shader-vars) :varying :uniform)
      (qualify-and-dump (shader-vars-out shader-vars) nil nil)
      (main
       (spaces `("vec4" ,temp "=" ,(funglsl "texture2D" 'sampler 'texcoord)))
       (spaces `(("gl_FragColor" ".rgb") "=" color "*" (,temp ".rgb"))))))))

(defparameter *blockshader-vs* nil)
(defparameter *blockshader-frag* nil)


(defparameter *test-vs* nil)
(defparameter *test-frag* nil)
(defun dump-test (attribs varyings uniforms)
  (setf *test-vs* (dump-vs)
	*test-frag* (dump-frag))
  (specify-attribs *test-vs* attribs)
  ;;bind varyings
  (bind-shader-vars *test-vs*
		    *test-frag*
		    varyings
		    :varying)
  ;;bind uniforms
  (bind-shader-vars *test-vs*
		    *test-frag*
		    uniforms
		    :uniform)
  (fill-vars *test-vs* *test-frag*)
  (values (dump-vs2 *test-vs*)
	  (dump-frag2 *test-frag*)))

(defun test ()
  (setf (values *blockshader-vs* *blockshader-frag*)
	(dump-test '(position texcoord color)
		   '((color-out . color)
		     (texcoord-out . texcoord))
		   '())))

(defparameter *save*
  '("#version 100
precision lowp float;
attribute vec4 position;
attribute vec2 texCoord;
attribute float darkness;
uniform mat4 projectionmodelview;
varying vec2 TexCoord;
varying float mycolor;
void main(){
gl_Position = projectionmodelview * position;
mycolor = darkness;
TexCoord = texCoord;
}
"
    "#version 100
precision lowp float;
varying vec2 TexCoord;
varying float mycolor;
uniform sampler2D ourTexture;
void main(){
vec4 texcolor = texture2D(ourTexture, TexCoord);
gl_FragColor.rgb = mycolor * texcolor.rgb;
}
") )
