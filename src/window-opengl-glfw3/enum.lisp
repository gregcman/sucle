(in-package :window)

;;;glfw enums to positions
(eval-always
  (defparameter *modified-mouse-enums*
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

(defparameter *modified-key-enums*
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
(defun to-claw (prefix sym &key (errorp t))
  (let ((string (concatenate 'string "+" prefix "-"(symbol-name sym) "+")))
    (multiple-value-bind (sym existsp)
	(find-symbol
	 string
	 (find-package "%GLFW"))
      (if existsp
	  (values sym t)
	  (if errorp
	      (error "~a not found in %GLFW package" string)
	      (values nil nil))))))
(defun substitute-foreign-enum-value (type name)
  (symbol-value (to-claw type name)))
(defparameter *mouse-array*
  (let ((array (make-array 8 :element-type '(unsigned-byte 8))))
    (dolist (x *modified-mouse-enums*)
      (when (listp x)
	(setf (aref array
		    (substitute-foreign-enum-value "MOUSE-BUTTON" (first x))
		    ;;(cffi:foreign-enum-value '%glfw::mouse (first x))
		    )
	      (second x))))
    array))
(defparameter *key-array*
  (let ((array (make-array 349 :element-type '(unsigned-byte 8))))
    (dolist (x *modified-key-enums*)
      (when (listp x)
	(setf (aref array
		    (substitute-foreign-enum-value "KEY" (first x))
		    ;;(cffi:foreign-enum-value '%glfw::key (first x))
		    )
	      (second x))))
    array))
(defparameter *character-keys*
  (let ((array (make-array 128 :element-type 'bit :initial-element 0)))
    (dotimes (i 97)
      (unless (zerop (aref *key-array* i))
	(setf (sbit array i) 1)))
    array))
;;escape, delete, backspace, tab, return/enter? are ascii?
(defparameter *back-map* 
  (let ((back-map (make-array 128)))
    (dolist (item *modified-mouse-enums*)
      (when (listp item)
	(setf (aref back-map (second item))
	      (cons :mouse (first item)))))
    (dolist (item *modified-key-enums*)
      (when (listp item)
	(setf (aref back-map (second item))
	      (cons :key (first item)))))
    #+nil
    (flet ((thing (array enum)
	     (dotimes (i (length array))
	       (let ((value (cffi:foreign-enum-keyword enum i :errorp nil)))
		 (when value
		   (setf (aref back-map (aref array i))
			 (cons enum value)))))))
      (thing *mouse-array* (quote %glfw::mouse))
      (thing *key-array* (quote %glfw::key)))
    back-map)))

(defun back-value (n)
  (let ((cell (aref *back-map* n)))
    (values (cdr cell)
	    (case (car cell)
	      (;;%cl-glfw3::key
	       :key
	       :key)
	      (;;%cl-glfw3::mouse
	       :mouse
	       :mouse)))))

(defmacro mouseval (identifier)
  (etypecase identifier
    (keyword
     (aref *mouse-array*
	   (substitute-foreign-enum-value "MOUSE-BUTTON" identifier)
	   ;;(cffi:foreign-enum-value (quote %glfw::mouse) identifier)
	   ))
    (integer
     (aref *mouse-array* (1- identifier)))))
(defmacro keyval (identifier)
  (etypecase identifier
    (keyword
     (aref *key-array*
	   (substitute-foreign-enum-value "KEY" identifier)
	   #+nil
	   (cffi:foreign-enum-value (quote %glfw::key)
				    identifier)))
    (character
     (char-code (char-upcase identifier)))
    (integer
     (char-code (digit-char identifier)))))


(defconstant +shift+ 1)
(defconstant +control+ 2)
(defconstant +alt+ 4)
(defconstant +super+ 8)
