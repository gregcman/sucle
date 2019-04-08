
;;----------------------------------------------------------------------
;package

;;;; package.lisp

(defpackage #:sketch-sucle-examples
  (:use #:cl #:sketch-sucle)
  (:export :brownian
	   :hello-world
	   :life
	   :sinewave
	   :lenna))



;;----------------------------------------------------------------------
;sinewave

;;;; sinewave.lisp

(in-package #:sketch-sucle-examples)

;;;  ____ ___ _   _ _______        _____     _______
;;; / ___|_ _| \ | | ____\ \      / / \ \   / / ____|
;;; \___ \| ||  \| |  _|  \ \ /\ / / _ \ \ / /|  _|
;;;  ___) | || |\  | |___  \ V  V / ___ \ V / | |___
;;; |____/___|_| \_|_____|  \_/\_/_/   \_\_/  |_____|

(defsketch sinewave
    ((title "Sinewave") (width 400) (height 400)
     (steps 0) (xs (/ width 5)) (r 3))
  (incf steps)
  ;;(background (rgb 0.2 0.2 0.2))
  (let ((w width) (h height))
    (flet ((sin-calc (x)
	     (sin (* +tau+ (/ (+ (/ steps 4) x) xs)))))
      (dotimes (x xs)
	(with-pen(make-pen :fill (rgb (/ (1+ (sin-calc x)) 2)
				      (/ (1+ (sin-calc (- x))) 2)
				      0.2)
			   :stroke (gray 0.1))
	  (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc x))) r r)
	  (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc (- x)))) r r)
	  (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (- (sin-calc (- x))))) r r)
	  (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (- (sin-calc x)))) r r))))))


;;----------------------------------------------------------------------
;life

;;;; life.lisp

(in-package #:sketch-sucle-examples)

;;;  _     ___ _____ _____
;;; | |   |_ _|  ___| ____|
;;; | |    | || |_  |  _|
;;; | |___ | ||  _| | |___
;;; |_____|___|_|   |_____|

;;; Press any key to toggle between editing and iterating.
;;; When in edit mode, click on cells to toggle them.

(defsketch life
    ((title "Conway's Game of Life")
     (columns 30)
     (rows 30)
     (cell-size 15)
     (width (* columns cell-size))
     (height (* rows cell-size))
     (cells (make-array `(,(+ 2 rows) ,(+ 2 columns) 2)
			:initial-element 0
			:element-type '(mod 2)))
     (front 0)
     (color-bg (gray 0.2))
     (pen-dead (make-pen :fill (gray 0)))
     (pen-alive (make-pen :fill (gray 0.5)))
     (running nil))
  (labels ((neighbors (x y)
	     (let ((acc 0))
	       (dotimes (i 3)
		 (dotimes (j 3)
		   (setf acc (+ acc (aref cells (+ i y) (+ j x) front)))))
	       (- acc (aref cells (1+ y) (1+ x) front))))
	   (alivep (x y)
	     (= 1 (aref cells (1+ y) (1+ x) front)))
	   (next-state (x y)
	     (let ((alive (alivep x y)) (neighbors (neighbors x y)))
	       (if (or (and alive (<= 2 neighbors 3))
		       (and (not alive) (= 3 neighbors)))
		   1 0))))
    (background color-bg)
    (dotimes (y rows)
      (dotimes (x columns)
	(with-pen (if (zerop (aref cells (1+ y) (1+ x) front))
		      pen-dead
		      pen-alive)
	  (ellipse (+ (/ cell-size 2) (* x cell-size))
		   (+ (/ cell-size 2) (* y cell-size))
		   (/ cell-size 3)
		   (/ cell-size 3)))
	(setf (aref cells (1+ y) (1+ x) (mod (1+ front) 2))
	      (next-state x y))))
    (when running
      (setf front (mod (1+ front) 2)))))

#+nil ;;FIXME
(defmethod kit.sdl2:textinput-event ((window life) ts text)
  (with-slots (running) window
    (setf running (not running))))
#+nil ;;FIXME
(defmethod kit.sdl2:mousebutton-event ((window life) state ts b x y)
  (when (eq state :mousebuttondown)
    (with-slots (cells front running cell-size) window
      (when (not running)
	(let ((cy (1+ (truncate (/ y cell-size))))
	      (cx (1+ (truncate (/ x cell-size)))))
	  (setf (aref cells cy cx front)
		(mod (1+ (aref cells cy cx front)) 2)))))))


;;----------------------------------------------------------------------
;brownian

;;;; brownian.lisp

(in-package #:sketch-sucle-examples)

;;  ____  ____   _____        ___   _ ___    _    _   _
;; | __ )|  _ \ / _ \ \      / / \ | |_ _|  / \  | \ | |
;; |  _ \| |_) | | | \ \ /\ / /|  \| || |  / _ \ |  \| |
;; | |_) |  _ <| |_| |\ V  V / | |\  || | / ___ \| |\  |
;; |____/|_| \_\\___/  \_/\_/  |_| \_|___/_/   \_\_| \_|

(defsketch brownian
    ((title "Brownian")
     (width 800)
     (height 600)
     (copy-pixels t)
     (pos (cons (/ width 2) (/ height 2))) (dir '(1 . 0))
     (pen (make-pen :stroke (gray 0.5) :fill (gray 0.5) :weight 1))
     (line-length 3)
     (points (make-array 256 :initial-element (cons 400 300)))
     (points-pointer 0))
  (flet ((draw (paces)
	   (dotimes (i paces)
	     (let ((new-pos (cons (+ (car pos) (car dir))
				  (+ (cdr pos) (cdr dir)))))
	       (with-pen pen
		 (line (car pos) (cdr pos) (car new-pos) (cdr new-pos)))
	       (setf pos new-pos))))
	 (rotate (a)
	   (let ((a (+ a (degrees (atan (cdr dir) (car dir))))))
	     (setf dir (cons (cos (radians a))
			     (sin (radians a)))))))
    (rotate (- (random 180) 90))
    (draw (+ (random line-length) line-length))
    (setf (car pos) (alexandria:clamp (car pos) -10 810)
	  (cdr pos) (alexandria:clamp (cdr pos) -10 610))))

(defmethod setup ((instance brownian) &key &allow-other-keys)
  (background (gray 1)))


;;----------------------------------------------------------------------
;hello-world

;;;; hello-world.lisp

(in-package #:sketch-sucle-examples)

;;;  _   _ _____ _     _     ___   __        _____  ____  _     ____
;;; | | | | ____| |   | |   / _ \  \ \      / / _ \|  _ \| |   |  _ \
;;; | |_| |  _| | |   | |  | | | |  \ \ /\ / / | | | |_) | |   | | | |
;;; |  _  | |___| |___| |__| |_| |   \ V  V /| |_| |  _ <| |___| |_| |
;;; |_| |_|_____|_____|_____\___/     \_/\_/  \___/|_| \_\_____|____/

(defsketch hello-world
    ((title "Hello, world!")
     (unit (/ width 10))
     (height width))
  (background (gray 0.6))
  (with-pen (make-pen :fill (rgb 0.380 0.695 0.086) :stroke (rgb 1 1 0) :weight 4)
    (polygon (* 5 unit) unit unit (* 9 unit) (* 9 unit) (* 9 unit))
    (text title 20 20)))


;;----------------------------------------------------------------------
;lenna

;;;; lenna.lisp

(in-package #:sketch-sucle-examples)

(defsketch lenna
    ((title "lenna")
     (width 256)
     (height 256)
     (picture (load-resource (relative-path
			      "res/lenna.png"))))
  (image picture 0 0 256 256)
  #+nil
  (text "What the fuck did you just fucking say about me, you little bitch?
 I'll have you know I graduated top of my class in the Navy Seals, 
and I've been involved in numerous secret raids on Al-Quaeda, 
and I have over 300 confirmed kills." 150 200 300 100))

