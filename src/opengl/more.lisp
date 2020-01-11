(in-package :glhelp)

(defun quads-triangles-index-buffer (n)
  "Convert N quads into an element buffer of triangles"
  ;;0->3 quad
  ;;0 1 2 triangle
  ;;0 2 3 triangle
  (let ((array (make-array (* 6 n) :element-type '(unsigned-byte 32))))
    (dotimes (i n)
      (let ((base (* i 6))
	    (quad-base (* i 4)))
	(flet ((foo (a b)
		 (setf (aref array (+ base a))
		       (+ quad-base b))))
	  (foo 0 0)
	  (foo 1 1)
	  (foo 2 2)
	  (foo 3 0)
	  (foo 4 2)
	  (foo 5 3))))
    array))

(export 'quads-triangles-index-buffer)
;;FIXME::rename from gl-list to display-list?
(defmacro create-gl-list-from-specs ((type times) form)
  `(glhelp:with-gl-list
     (gl:with-primitives ,type
       (loop :repeat ,times :do 
	  (glhelp:vertex-attrib-f* ,form)))))

(export 'create-gl-list-from-specs)


(defparameter *quad-to-triangle-index-buffer-quad-count*
  ;;the number of quads in a 16x16x16 chunk if each block has 6 faces showing.
  ;;around 300k of memory, with 32 bit unsigned ints.
  (* 16 16 16 6 3))
(deflazy-gl
 shared-quad-to-triangle-index-buffer ()
 (let ((index-buffer (gl:gen-buffer))
       (indices (quads-triangles-index-buffer *quad-to-triangle-index-buffer-quad-count*)))
   (let ((len (length indices)))
     (gl:with-gl-array (arr :unsigned-int :count len)
       (dotimes (i len)
	 (setf (gl:glaref arr i) (aref indices i)))
       (glhelp::use-element-array-buffer index-buffer arr)))
   index-buffer))
(defparameter *plain-index-buffer-count*
	      (* 16 16 16 6 3))
(deflazy-gl
 shared-plain-index-buffer ()
 (let ((index-buffer (gl:gen-buffer))
       (indices (quads-triangles-index-buffer *plain-index-buffer-count*)))
   (let ((len (length indices)))
     (gl:with-gl-array (arr :unsigned-int :count len)
       (dotimes (i len)
	 (setf (gl:glaref arr i) i))
       (glhelp::use-element-array-buffer index-buffer arr)))
   index-buffer))

(defun get-fixed-type-and-index-buffer-for-type (type times)
  ;;convert quads to tris for new opengl.
  (case type
    (:quads
     (values :triangles (deflazy:getfnc 'shared-quad-to-triangle-index-buffer) (* 6/4 times)))
    (otherwise
     (values type (deflazy:getfnc 'shared-plain-index-buffer) times))))

(defmacro create-vao-from-specs ((type-form times-form) form)
  (let* ((data 
	  (mapcar (lambda (n)
		    (destructuring-bind (index &rest forms) n
		      (list index (length forms))))
		  form))
	 (layout
	  (glhelp::simple-vertex-array-layout data))
	 (forms (apply 'concatenate 'list (mapcar 'rest form))))

    (let ((len (glhelp::vertex-array-layout-total-size layout)))
      (assert (= len (length forms)))
      (values
       ;;data
       ;;forms
       ;;layout
       (utility:with-gensyms (times type vertex-buffer array-count arr index add)
	 `(let* ((,type ,type-form)
		 (,times ,times-form)
		 (,vertex-buffer (gl:gen-buffer))
		 (,array-count (* ,len ,times)))
	    (gl:with-gl-array (,arr :float :count ,array-count)
	      (let ((,index 0))
		(flet ((,add (n)
			 (setf (gl:glaref ,arr ,index) n)
			 (incf ,index)))
		  (loop :repeat ,times :do
		    ,@(mapcar (lambda (form) `(,add ,form)) forms))))
	      (glhelp::use-array-buffer ,vertex-buffer ,arr))
	    (let
		((vao
		  (multiple-value-bind (fixed-type index-buffer fixed-times)
		      (get-fixed-type-and-index-buffer-for-type ,type ,times)		  
		    (glhelp::assemble-vao
		     ,vertex-buffer
		     index-buffer
		     ',layout
		     ;;FIXME:: is it the total count of primitives, or points?
		     fixed-times
		     fixed-type))))
	      (setf (glhelp::i-delete-p vao) nil)
	      (values vao))))))))

(defmacro create-vao-or-display-list-from-specs ((type times) form)
  `(ecase *slow-draw-type*
     (:display-list
      (create-gl-list-from-specs (,type ,times) ,form))
     (:vertex-array-object
      (create-vao-from-specs (,type ,times) ,form))))
(export 'create-vao-or-display-list-from-specs)

#+nil
(create-vao-or-display-list-from-specs
 (:quads 10)
 ((2 (xyz) (xyz) (xyz))
    ;;why???
    (8 0.06 0.06)
    (1 0.0 0.0 0.0 0.0)
    ;;zero always comes last?
    (0 0.0 0.0 0.0 0.0)))

