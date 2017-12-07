(in-package :sandbox)

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
       
       (%gl:vertex-attrib-1f 8 (dark))
       (%gl:vertex-attrib-2f 2 (uv) (uv))
       (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz)))))))

(defun attrib-buffer-iterators ()
  (map-into (make-array 3 :element-type t :initial-element nil)
	    (function my-iterator)))

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
