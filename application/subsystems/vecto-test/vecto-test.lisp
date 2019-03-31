#+nil (ql:quickload :vecto)
(defpackage :vecto-test
  (:use :cl :vecto))
(in-package :vecto-test)

(defparameter *this-directory* (asdf:system-source-directory :vecto-stuff))
(defparameter *output* (merge-pathnames "vecto.png"
					*this-directory*))

(defun vecto-data ()
  (if (boundp 'vecto::*graphics-state*)
      (vecto::image 
       vecto::*graphics-state*)
      (error "no vecto data. Try using VECTO:WITH-CANVAS ?")))

;;;;examples stolen from vecto/doc/examples.lisp

(defun radiant-lambda (&optional (file *output*))
  (with-canvas (:width 90 :height 90)
    (let ((font (get-font *vecto-ttf-path*))
          (step (/ pi 7)))
      (set-font font 40)
      (translate 45 45)
      (draw-centered-string 0 -10 #(#x3BB))
      (set-rgb-stroke 1 0 0)
      (centered-circle-path 0 0 35)
      (stroke)
      (set-rgba-stroke 0 0 1.0 0.5)
      (set-line-width 4)
      (dotimes (i 14)
        (with-graphics-state
          (rotate (* i step))
          (move-to 30 0)
          (line-to 40 0)
          (stroke)))
      (vecto-data)
      #+nil
      (save-png file))))

(defun feedlike-icon (&optional (file *output*))
  (with-canvas (:width 100 :height 100)
    (set-rgb-fill 1.0 0.65 0.3)
    (rounded-rectangle 0 0 100 100 10 10)
    (fill-path)
    (set-rgb-fill 1.0 1.0 1.0)
    (centered-circle-path 20 20 10)
    (fill-path)
    (flet ((quarter-circle (x y radius)
             (move-to (+ x radius) y)
             (arc x y radius 0 (/ pi 2))))
      (set-rgb-stroke 1.0 1.0 1.0)
      (set-line-width 15)
      (quarter-circle 20 20 30)
      (stroke)
      (quarter-circle 20 20 60)
      (stroke))
    (rounded-rectangle 5 5 90 90 7 7)
    (set-gradient-fill 50 90
                       1.0 1.0 1.0 0.7
                       50 20
                       1.0 1.0 1.0 0.0)
    (set-line-width 2)
    (set-rgba-stroke 1.0 1.0 1.0 0.1)
    (fill-and-stroke)
    (vecto-data)
    ;;(save-png file)
    ))

(defun star-clipping (&optional (file *output*))
  (with-canvas (:width 200 :height 200)
    (let ((size 100)
          (angle 0)
          (step (* 2 (/ (* pi 2) 5))))
      (translate size size)
      (move-to 0 size)
      (dotimes (i 5)
        (setf angle (+ angle step))
        (line-to (* (sin angle) size)
                 (* (cos angle) size)))
      (even-odd-clip-path)
      (end-path-no-op)
      (flet ((circle (distance)
               (set-rgba-fill distance 0 0
                              (- 1.0 distance))
               (centered-circle-path 0 0 (* size distance))
               (fill-path)))
        (loop for i downfrom 1.0 by 0.05
	   repeat 20 do
	     (circle i)))
      (vecto-data)
      #+nil
      (save-png file))))

(defun gradient-example (&optional (file *output*))
  (with-canvas (:width 200 :height 50)
    (set-gradient-fill 25 0
                       1 0 0 1
                       175 0
                       1 0 0 0)
    (rectangle 0 0 200 50)
    (fill-path)
    (vecto-data)
    #+nil
    (save-png file)))

(defun gradient-bilinear-example (&optional (file *output*))
  (with-canvas (:width 200 :height 50)
    (set-gradient-fill 25 0
                       1 0 0 1
                       175 0
                       1 0 0 0
                       :domain-function 'bilinear-domain)
    (rectangle 0 0 200 50)
    (fill-path)
    (vecto-data)
    #+nil
    (save-png file)))

(defparameter *vecto-ttf-path* (merge-pathnames "times.ttf" *this-directory*))
(defun text-paths (&optional (file *output*))
  (with-canvas (:width 400 :height 100)
    (set-font (get-font *vecto-ttf-path*) 70)
    (centered-string-paths 200 15 "Hello, world!")
    (set-line-join :round)
    (set-line-cap :round)
    (set-line-width 3)
    (set-dash-pattern #(0 5) 0)
    (stroke-to-paths)
    (set-gradient-fill 0 0   1 0 0 0.5
                       0 100 1 1 1 1)
    (fill-path)
    (vecto-data)
    #+nil
    (save-png file)))
