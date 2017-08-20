(in-package :windxow)

(progno  (def-key-callback key-callback (window key scancode action mod-keys)
	     (declare (ignorable scancode window))
	     (let* ((key-state (key key))
		    (mod-shift (ash mod-keys +mod-key-shift+))
		    (new (next-key-state key-state action))
		    (new-composite (logior mod-shift new)))
	       (declare (type fixnum mod-shift new new-composite))
	       (setf (key key) new-composite)))
	   (def-mouse-button-callback mouse-callback (window button action mod-keys)
	     (declare (ignorable window))
	     (let* ((key-state (mice button))
		    (mod-shift (ash mod-keys +mod-key-shift+))
		    (new (next-key-state key-state action))
		    (new-composite (logior mod-shift new)))
	       (declare (type fixnum mod-shift new new-composite))
	       (setf (mice button) new-composite)))
	   (def-char-callback char-callback (window char)
	     (declare (ignorable window))
	     (vector-push-extend (code-char char) *chars*)))

(progno
;;;when buttons can take either of two states, there are four
;;;ways adjacent time frames can look [repeat does not count here]
 (defun next-key-state (old new)
   (cond ((eq nil old)
	  (if (eql new +press+) +press+))
	 ((eq +true+ old)
	  (cond ((eql new +release+) +release+)
		((eql new +repeat+) +repeat+))))))

(let ((array *mouse-array*))
  (dolist (x *bees2*)
    (when (listp x)
      (setf (aref array (cffi:foreign-enum-value '%glfw::mouse (first x)))
	    (second x)))))

(let ((array *key-array*))
  (dolist (x *bees*)
    (when (listp x)
      (setf (aref array (cffi:foreign-enum-value '%glfw::key (first x)))
	    (second x)))))

(defparameter *bees2*
  (quote
   ((:1 11) ;;0
    (:2 12) ;;1
    (:3 14) ;;2
    (:4 15);3
    (:5 16);4
    (:6 33);5
    (:7 34);6
    (:8 35);7
    (:last 35);8
    (:left 11);9
    (:right 12);;10
    )))

(defparameter *bees*
  (quote
   (;;(:unknown -1)
    (:left-shift 0) ;;340 ----
    (:left-control 1);;341---
    (:left-alt 2);;342---
    (:left-super 3);;343--
    (:right-shift 4) ;;344 ----
    (:right-control 5);;345----
    (:right-alt 6);;;346---
    (:right-super 7);;347---
    (:backspace 8) ;;259***
    (:tab 9) ;;258***

    (:enter 10) ;;;257***
    11
    12
    (:kp-enter 13) ;;335***
    14
    15
    16   
    (:kp-0 17);;320----
    (:kp-1 18);;321----
    (:kp-2 19);;322----
    (:kp-3 20);;323----
    (:kp-4 21);;324----
    (:kp-5 22);;325----
    (:kp-6 23);;326----
    (:kp-7 24);;327----
    (:kp-8 25);;328----
    (:kp-9 26);;329----
    (:escape 27) ;;256***   
    (:up 28);;265---
    (:down 29) ;;264---
    (:right 30) ;;;262---
    (:left 31);;263---
    (:space 32)
    33
    34
    35
    (:menu 36) ;;348---
    (:kp-decimal 37);;330--
    (:kp-equal 38);;336--
    (:apostrophe 39)
    (:kp-divide 40);;;331---
    (:kp-subtract 41);;;333---
    (:kp-multiply 42);;332---
    (:kp-add 43);;;334---
    (:comma 44)
    (:minus 45)
    (:period 46)
    (:slash 47)
    (:0 48)
    (:1 49)
    (:2 50)
    (:3 51)
    (:4 52)
    (:5 53)
    (:6 54)
    (:7 55)
    (:8 56)
    (:9 57)
    (:insert 58);;260---
    (:semicolon 59)
    (:page-up 60);;266--
    (:equal 61)
    (:page-down 62);;267--
    
    (:pause 63);;284---
    (:print-screen 64);;;283---

    (:a 65)
    (:b 66)
    (:c 67)
    (:d 68)
    (:e 69)
    (:f 70)
    (:g 71)
    (:h 72)
    (:i 73)
    (:j 74)
    (:k 75)
    (:l 76)
    (:m 77)
    (:n 78)
    (:o 79)
    (:p 80)
    (:q 81)
    (:r 82)
    (:s 83)
    (:t 84)
    (:u 85)
    (:v 86)
    (:w 87)
    (:x 88)
    (:y 89)
    (:z 90)
    (:left-bracket 91)
    (:backslash 92)
    (:right-bracket 93)
    (:world-1 94);;161---
    (:world-2 95);;;162--- 
    (:grave-accent 96)
    (:f1 97);;290--
    (:f2 98);;291
    (:f3 99);;292
    (:f4 100);;;
    (:f5 101);;;
    (:f6 102);;;
    (:f7 103);;;
    (:f8 104);;
    (:f9 105);;;
    (:f10 106);;;;
    (:f11 107);;;
    (:f12 108);;;
    (:f13 109);;;
    (:f14 110);;;;
    (:f15 111);;;
    (:f16 112);;;
    (:f17 113);;;
    (:f18 114);;;
    (:f19 115);;;
    (:f20 116);;;;
    (:f21 117);;;;
    (:f22 118);;;
    (:f23 119);;;;
    (:f24 120);;;;
    (:f25 121);;;
    (:end 122);;;269---
    (:caps-lock 123);;280---
    (:scroll-lock 124);;;281---
    (:num-lock 125);;;282---
    (:home 126);;;268****
    (:delete 127) ;;261****
    )))


(defparameter *keypress-hash* nil)
(defparameter *mousepress-hash* nil)


(defconstant +false+ 0)
(defconstant +release+ 1)
(defconstant +true+ 2)
(defconstant +press+ 3)
(defconstant +repeat+ 4)

(defparameter *action-map* (vector 1 3 4))

(defconstant +mod-key-shift+ 3)
(defconstant +key-state-mask+ #b111)
(defconstant +mod-state-mask+ #b1111000)

(defun step-hash (hash)
  (with-hash-table-iterator (next hash)
    (loop (multiple-value-bind (more key value) (next)
	    (if more
		(let ((nvalue (get-press-value value)))
		  (cond ((= +true+ nvalue))
			((< +true+ nvalue)
			 (setf (gethash key hash)
			       (logior (get-mod-value value)
				       +true+)))
			((> +true+ nvalue)
			 (remhash key hash))))
		(return))))))

(progn
  (defun key (key)
    (gethash key *keypress-hash* +false+))
  (defun (setf key) (value key)
    (setf (gethash key *keypress-hash*) value))
  (defun mice (mice)
    (gethash mice *mousepress-hash* +false+))
  (defun (setf mice) (value mice)
    (setf (gethash mice *mousepress-hash*) value)))

(let ((vec *keys*))
  (flet ((add (x)
	   (vector-push-extend x vec)))
    (add window)
    (add key)
    (add scancode)
    (add action)
    (add mod-keys)))

(let ((vec *buttons*))
  (flet ((add (x)
	   (vector-push-extend x vec)))
    (add window)
    (add button)
    (add action)
    (add mod-keys)))

(if *keypress-hash*
    (clrhash *keypress-hash*)
    (setf *keypress-hash* (make-hash-table :test 'eq)))
(if *mousepress-hash* 
    (clrhash *mousepress-hash*)
    (setf *mousepress-hash* (make-hash-table :test 'eq)))

(step-hash *keypress-hash*)
(step-hash *mousepress-hash*)

  (let ((buttonsize (fill-pointer *buttons*)))
    (dobox ((offset 0 buttonsize :inc 4))     
	   (etouq (with-vec-params '((offset nil button action mod-keys)) '(*buttons*)
					;    (declare (ignorable window))
		    '(let* ((mod-shift (ash mod-keys +mod-key-shift+))
			    (new-composite (logior mod-shift (aref *action-map* action))))
		      (declare (type fixnum mod-shift  new-composite))
		      (setf (mice button) new-composite))))))
  (let ((keysize (fill-pointer *keys*)))
    (dobox ((offset 0 keysize :inc 5))
	   (etouq (with-vec-params '((offset nil key nil action mod-keys)) '(*keys*)
					;  (declare (ignorable window scancode))
		    '(let* ((mod-shift (ash mod-keys +mod-key-shift+))
			    (new-composite (logior mod-shift (aref *action-map* action))))
		      (declare (type fixnum mod-shift new-composite))
		      (setf (key key) new-composite))))))
  (progn
    (setf 
	  (fill-pointer *buttons*) 0
	  (fill-pointer *keys*) 0))


(defun mice-p (the-key)
  (let ((value (get-press-value (mice the-key))))
    (<= +true+ value)))
(defun mice-j-p (the-key)
  (let ((value (get-press-value (mice the-key))))
    (= value +press+)))
(defun mice-j-r (the-key)
  (let ((value (get-press-value (mice the-key))))
    (= value +release+)))
