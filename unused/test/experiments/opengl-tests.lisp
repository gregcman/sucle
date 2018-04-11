;;;;not clearing the depth buffer experiment by flip-flopping the depth function

(defparameter *depth-buffer?* t)
;;when set to nil does not clear the depth buffer, but instead flip-flops
;;the depth function and matrix. has edge artefacts when using glclear for color

(defparameter *mata*
  (nsb-cga:matrix*
   (nsb-cga:translate* 0.0 0.0 1.0)
   (nsb-cga:scale* 1.0 1.0 -1.0)))
(;per-frame: 
 (unless *depth-buffer?*
   (gl:clear :color-buffer-bit)
   (cond ((evenp *render-ticks*)
	  ;;	   (gl:clear-depth 0.0)
	  (gl:depth-func :greater)
	  (gl:depth-range 0.5 1.0)
	  (nsb-cga:%matrix* matrix
			    *mata* cam))
	 (t
	  ;;	   (gl:clear-depth 1.0)
	  (gl:depth-func :less)
	  (gl:depth-range 0.0 0.5)
	  (setf matrix cam))))) 

#+nil
#(:clear :set :copy :copy-inverted
  :noop :invert :and :nand :or :nor
  :xor :equiv :and-reverse :and-inverted :or-reverse :or-inverted) 
