(defpackage #:cartesian-graphing
  (:use :cl :utility :application :opengl-immediate))
(in-package :cartesian-graphing)

(defparameter *pic-tint* (vector 1.0 1.0 1.0 1.0))
(defparameter *ticks* 0)
(defun draw-graph ()
  (gl:line-width 10.0)
  (gl:point-size 10.0)
  (let ((program (getfnc 'flat-shader)))
    (glhelp::use-gl-program program))
  (with-primitives :lines
    'mesh-vertex-color
    (color 1.0 0.0 0.0)
    (vertex -1.0 -1.0)
    (color 0.0 1.0 0.0)
    (vertex 1.0 1.0)
    (color 0.0 0.0 1.0)
    (vertex 1.0 -1.0)))
 
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

;;;FIXME::copied from vecto-test
(defun graph ()
  (let ((width (application::getfnc 'application::w))
	(height (application::getfnc 'application::h)))
    (with-canvas (:width width
			 :height height)
      (set-line-join :round)
      (set-line-cap :round)
      (set-line-width 2)
      (set-rgba-stroke (/ (1+ (sin (nice-time))) 2.0)
		       (/ (1+ (sin (nice-time))) 2.0)
		       (/ (1+ (sin (nice-time))) 2.0)
		       ;;0.5 0.5 0.5
		       1.0)
      (let ((minx -10)
	    (maxx 10)
	    (miny -10)
	    (maxy 10))
	(flet ((translate-x (n)
		 (alexandria:lerp (/ n width) minx maxx))
	       (translate-y (n)
		 (* height (reverse-lerp miny maxy n))))
	  
	  (move-to 0 (translate-y (math-fun (translate-x minx))))
	  (dotimes (xn width)
	    (line-to xn
		     (translate-y
		      (math-fun
		       (translate-x xn)))))))
      (stroke)
      (vecto-data))))

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
