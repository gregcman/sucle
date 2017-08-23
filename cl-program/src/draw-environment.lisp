(in-package :aplayground)

(defun make-attrib-buffer-data ()
  (make-array 16 :element-type t :initial-element nil))
(defun fill-with-flhats (array)
  (map-into array #'flhat:make-flhat))
(defun make-iterators (buffer result)
  (map-into result (lambda (x) (flhat:make-flhat-iterator x)) buffer))
(defun tally-buffer (iterator-buffer result)
  (map-into result (lambda (x) (iterator-count x)) iterator-buffer))
(defun reset-attrib-buffer-iterators (fill-data)
  (dotimes (x (array-total-size fill-data))
    (flhat:reset-iterator (aref fill-data x))))
(defun iterator-count (iterator)
  (if (iter-ator:p-array iterator)
      (1+ (flhat:iterator-position iterator))
      0))
(defparameter *attrib-buffers* (fill-with-flhats (make-attrib-buffer-data)))
(defparameter *attrib-buffer-iterators*
  (make-iterators *attrib-buffers* (make-attrib-buffer-data)))
(defparameter *attrib-buffer-fill-pointer*
  (tally-buffer *attrib-buffer-iterators* (make-attrib-buffer-data)))

(defparameter *default-tex-params* (quote ((:texture-min-filter . :nearest)
					   (:texture-mag-filter . :nearest)
					   (:texture-wrap-s . :repeat)
					   (:texture-wrap-t . :repeat))))

(progn
  (defconstant +gltexture0+ (cffi:foreign-enum-value (quote %gl:enum) :texture0))
  (defun set-active-texture (num)
    (gl:active-texture (+ num +gltexture0+))))

(defmacro with-iterators ((&rest bufvars) buf func type &body body)
  (let* ((syms (mapcar (lambda (x) (gensym (string x))) bufvars))
	 (bindings (mapcar (lambda (x y) (list x y))
			   bufvars syms))
	 (decl `(declare (type ,type ,@syms))))
    (with-vec-params syms `(,buf)
		     decl
		     `(,func ,bindings
			     ,@body))))

(defparameter *buffer-vector-scratch* (make-array 16))
(defun get-buf-param (iter attrib-order &optional (newarray *buffer-vector-scratch*))
  (let ((len (length attrib-order)))
    (dotimes (x len)
      (setf (aref newarray x)
	    (aref iter (aref attrib-order x)))))
  newarray)


