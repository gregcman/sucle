(in-package :fuck)

(defmacro progeach (fun body)
  `(etouq
    (cons 'progn
	  (mapcar ,fun
		  ,body))))
(bornfnc
 'huh?
 (lambda ()
   (let ((a (sandbox::my-iterator))
	 (c (sandbox::my-iterator))
	 (len 0))
     (iter-ator:bind-iterator-out
      (col single-float) c
      (iter-ator:bind-iterator-out
       (pos single-float) a

       (progeach
	(lambda (x) (list 'col x))
	(axis-aligned-quads:quadk+ 0.0 '(-1.0 1.0 -1.0 1.0)))
       #+nil
       (progeach
	col
	'(1.0 0.0 0.0 0.0
	  0.0 0.0 0.0 0.0
	  0.0 1.0 0.0 0.0
	  1.0 1.0 0.0 0.0))
       (incf len 4)
       ))

     (values
      (glhelp:with-gl-list
	(gl:with-primitives :quads 
	  (sandbox::flush-my-iterator a
	    (sandbox::flush-my-iterator c
	      ((lambda (times a c)
		 (iter-ator:bind-iterator-in
		  (xyz single-float) a
		  (iter-ator:bind-iterator-in
		   (dark single-float) c
		   (dotimes (x times)     
		     (%gl:vertex-attrib-4f 8 (dark) (dark) (dark) (dark))
		     (%gl:vertex-attrib-4f 0 (xyz) (xyz) (xyz) 1.0)))))
	       len a c)))))
      :opengl))))
(progn
  (bornfnc
   'noopshader
   (lambda ()
     (let ((program
	    (glhelp:make-shader-program-from-strings
	     (getfnc 'noop-vs)
	     (getfnc 'noop-frag)
	     (quote (("position" . 0)	
		     ("color" . 8))))))    
       (values program :opengl))))
  (bornfnc
   'noop-vs
   (lambda ()
     (alexandria:read-file-into-string
      (sandbox::shader-path "noop/noop.vs"))))
  (bornfnc
   'noop-frag
   (lambda ()
     (alexandria:read-file-into-string
      (sandbox::shader-path "noop/noop.frag")))))

  #+nil
  (set-render-area (make-instance 'render-area :x 0 :y 0
				  :width 200 :height 200
				  ))
;  (gl:enable :blend)
 ;
;
;  (gl:blend-func :one :one-minus-src-alpha)
  ;;  (gl:use-program (getfnc 'noopshader))
  ;;  (gl:disable :cull-face)
  ;;  (gl:delete-lists (getfnc :huh?) 1)
  ;; (remove-stuff :huh?)
  ;; (gl:call-list (getfnc 'huh?))
