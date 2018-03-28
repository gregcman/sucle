(defpackage :opengl-immediate
  (:use #:cl #:utility)
  (:export
   #:vertex
   #:tex-coord
   #:color)
  (:export
   #:mesh-vertex
   #:mesh-vertex-color
   #:mesh-vertex-tex-coord
   #:mesh-vertex-tex-coord-color)
  (:export
   #:with-primitives))
(in-package :opengl-immediate)

;;vertex = 0
;;tex-coord = 2
;;color = 3
(defparameter *vertex-scratch* (scratch-buffer:my-iterator))
(defparameter *tex-coord-scratch* (scratch-buffer:my-iterator))
(defparameter *color-scratch* (scratch-buffer:my-iterator))

(defun vertex (&optional (x 0.0) (y 0.0) (z 0.0) (w 1.0))
  (iterator:bind-iterator-out
   (emit single-float) *vertex-scratch*
   (emit x y z w)))
(defun tex-coord (&optional (x 0.0) (y 0.0) (z 0.0) (w 1.0))
  (iterator:bind-iterator-out
   (emit single-float) *tex-coord-scratch*
   (emit x y z w)))
(defun color (&optional (x 0.0) (y 0.0) (z 0.0) (w 1.0))
  (iterator:bind-iterator-out
   (emit single-float) *color-scratch*
   (emit x y z w)))
(eval-always
  (defparameter *default-attrib-locations*
    '((*vertex-scratch* 0)
      (*tex-coord-scratch* 2)
      (*color-scratch* 3)))
  (defun gen-mesher (&optional (items *default-attrib-locations*))
    (let ((sorted (sort (copy-list items) #'< :key #'second)))
      (map-into sorted (lambda (x) (cons (gensym) x)) sorted)
      (let ((first (first sorted))
	    (times-var (gensym "TIMES"))
	    (flushes nil)
	    (binds nil)
	    (attribs nil)
	    (names nil))
	(dolist (item sorted)
	  (destructuring-bind (name form num) item
	    (push `(scratch-buffer:flush-my-iterator ,name) flushes)
	    (push `(,name ,form) names)
	    (let ((emit (gensym "EMIT")))
	      (push `(iterator:bind-iterator-in
		      (,emit single-float) ,name) binds)
	      (push `(%gl:vertex-attrib-4f ,num (,emit) (,emit) (,emit) (,emit))
		    attribs))))
	(let ((header1
	       `(let ,names))
	      (header2
	       `(let ((,times-var (/ (scratch-buffer:iterator-fill-pointer ,(first first)) 4))))))
	  (%nest
	   (nconc
	    (list
	     header1
	     header2)
	    flushes
	    binds
	    (list
	     `(dotimes (x ,times-var)
		,(cons 'progn attribs))))))))))
(defmacro gen (name &rest items)
  (let (acc)
    (dolist (item items)
      (let* ((string (symbol-name item))
	     (namesake (find-symbol (concatenate 'string "*" string "-SCRATCH*"))))
	(when namesake
	  (push (assoc namesake *default-attrib-locations*) acc))))
    `(defun ,name ()
       ,(gen-mesher acc))))

(gen mesh-vertex vertex)
(gen mesh-vertex-color vertex color)
(gen mesh-vertex-tex-coord vertex tex-coord)
(gen mesh-vertex-tex-coord-color vertex tex-coord color)

(defmacro with-primitives (mode meshing-fun &body body)
  `(progn
     ,@body
     (gl:with-primitives ,mode
	 (,(second meshing-fun)))))
