(in-package :sandbox)

;;to add to a shape, we push the vertices to the front of the
;;vertex list and the indices to the index list, but we
;;increase the indices by the vertlength amount.

(defstruct shape
  (is (make-array 0 :adjustable t :fill-pointer 0))
  (vs (make-array 0 :adjustable t :fill-pointer 0))
  (vertlength 0)
  (indexlength 0))

(defun destroy-shape (leshape)
  (setf (fill-pointer (shape-is leshape)) 0)
  (setf (fill-pointer (shape-vs leshape)) 0)
  (setf (shape-vertlength leshape) 0)
  (setf (shape-indexlength leshape) 0)
  leshape)

(defun tringulate (verts)
  "take some verts and make a polygon instead"
  (let ((len (length verts)))
    (make-shape
     :is (let ((tris nil))
	   (dotimes (n (- len 2))
	     (push
	      (list
	       0
	       (+ 1 n)
	       (+ 2 n))
	      tris))
	   tris)
     :vs (nreverse verts)
     :indexlength (- len 2)
     :vertlength len)))

(defun add-shape (s1 small2 &key (target s1))
  "merge two shapes into one"
  (let ((new-2-indices
	 (let ((offset (shape-vertlength s1))
	       (ans nil))
	   (dolist (x (shape-is small2))
	     (push
	      (list
	       (+ offset (first x))
	       (+ offset (second x))
	       (+ offset (third x)))
	      ans))
	   ans)))
    (setf (shape-vs target) (append (shape-vs small2) (shape-vs s1))
	  (shape-is target) (append new-2-indices (shape-is s1))
	  (shape-vertlength target) (+ (shape-vertlength s1) (shape-vertlength small2))
	  (shape-indexlength target) (+ (shape-indexlength s1) (shape-indexlength small2)))
    target))

(defun add-verts (s1 verts)
  "add vertices to a shape, expanding it"
  (let* ((len (length verts))
	 (offset (shape-vertlength s1)))
    (dotimes (n (- len 2))
      (vector-push-extend offset (shape-is s1))
      (vector-push-extend (+ n 1 offset) (shape-is s1))
      (vector-push-extend (+ n 2 offset) (shape-is s1)))
    (incf (shape-vertlength s1) len)
    (incf (shape-indexlength s1) (- len 2))
    (dolist (v verts)
      (dotimes (n (length v))
	(let ((indivdata (aref v n)))
	  (dotimes (q (length indivdata))
	    (vector-push-extend (aref indivdata q) (shape-vs s1))))))
    s1))

