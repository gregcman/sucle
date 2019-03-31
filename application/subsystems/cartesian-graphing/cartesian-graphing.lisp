(defpackage #:cartesian-graphing
  (:use :cl :utility :application :opengl-immediate))
(in-package :cartesian-graphing)

(defparameter *pic-tint* (vector 1.0 1.0 1.0 1.0))
(defparameter *ticks* 0)
(defun draw-graph ()
  (glhelp::set-render-area 0 0 (getfnc 'application::w) (getfnc 'application::h))
  (gl:disable :depth-test)
  (gl:line-width 10.0)
  (gl:point-size 10.0)
  (let ((program (getfnc 'flat-shader)))
    (glhelp::use-gl-program program))
  (with-primitives :line-strip
    'mesh-vertex-color
    (graph)
    #+nil
    (progn
      (color 1.0 0.0 0.0)
      (vertex 1.0 1.0)
      (color 0.0 1.0 0.0)
      (vertex 0.5 0.6)
      (color 0.0 0.0 1.0)
      (vertex 0.5 -0.5))
    ))
 
(progn
  (deflazy flat-shader-source ()
    (glslgen:ashader
     :version 120
     :vs
     (glslgen2::make-shader-stage
      :out '((value-out "vec4"))
      :in '((position "vec4")
	    (value "vec4"))
      :program
      '(defun "main" void ()
	(= "gl_Position" position)
	(= value-out value)))
     :frag
     (glslgen2::make-shader-stage
      :in '((value "vec4"))
      :program
      '(defun "main" void ()	 
	(=
	 :gl-frag-color
	 value
	 )))
     :attributes
     '((position . 0) 
       (value . 3))
     :varyings
     '((value-out . value))
     :uniforms
     '()))
  (glhelp::deflazy-gl flat-shader (flat-shader-source)
    (glhelp::create-gl-program flat-shader-source)))

(defun reverse-lerp (start end value)
  (/ (- value start)
     (- end start)))

;;;FIXME::copied from vecto-test
(defun graph ()
  (let ((width (application::getfnc 'application::w))
	(height (application::getfnc 'application::h)))
    (let ((r (/ (1+ (sin (nice-time))) 2.0))
	  (g (/ (1+ (sin (nice-time))) 2.0))
	  (b (/ (1+ (sin (nice-time))) 2.0))
	  ;;0.5 0.5 0.5
	  (a 1.0))
      (let ((minx -10)
	    (maxx 10)
	    (miny -10)
	    (maxy 10))
	(flet ((fun1 (n)
		    (floatify
		     (- (* 2 (/ n width)) 1)))
	       (translate-x (n)
		 (alexandria:lerp (/ n width) minx maxx))
	       (translate-y (n)
		 (alexandria:lerp (reverse-lerp miny maxy n) -1.0 1.0)))
	  (dotimes (xn width)
	    (vertex (fun1 xn)
		    (floatify
		     (translate-y
		      (math-fun
		       (translate-x xn)))))
	    (color r g b a)))))))

(defun nice-time ()
  (/ (get-internal-real-time)
     (load-time-value (utility::floatify internal-time-units-per-second))))

(defun math-fun (x)
  ;;(math-fun2 x)
  (math-fun3 x))

(defun math-fun2 (x)
  (+ (* 20 (sin x))
     (+ (* x x 2)
	(- x))
     (+ (* 50 (sin (+ x (nice-time))))
	(* 20 (cos (+ (- x) (* 3 (nice-time))))))
     ))

(defun math-fun3 (x)
  (let ((offset 0))
    (if (< x offset)
	0
	(let ((value (- x offset)))
	  (* value value)))))
