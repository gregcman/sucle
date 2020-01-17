(in-package :window)
;;;;************************************************************************;;;;
;;;;<INPUT ARRAY>
(deftype mouse-keyboard-input-array ()
  `(simple-bit-vector 128))
(progn
  (declaim (ftype (function () mouse-keyboard-input-array)
		  make-mouse-keyboard-input-array))
  (defun make-mouse-keyboard-input-array ()
    (make-array 128 :element-type 'bit)))
(progn
  (declaim (type mouse-keyboard-input-array *input-state* *repeat-state*))
  (defparameter *input-state* (make-mouse-keyboard-input-array))
  (defparameter *repeat-state* (make-mouse-keyboard-input-array)))

(defstruct control-state
  (prev (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (curr (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (diff (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (jp (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (jr (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (repeat (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (jp-or-repeat (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array))
(defparameter *control-state* (make-control-state :curr *input-state*
						  :repeat *repeat-state*))
(defun reset-control-state (&optional (state *control-state*))
  (fill (control-state-curr state) 0)
  (fill (control-state-prev state) 0)
  (fill (control-state-diff state) 0)
  (fill (control-state-jp state) 0)
  (fill (control-state-jr state) 0))
(defun update-control-state (&optional (state *control-state*))
  (bit-xor (control-state-curr state)
	   (control-state-prev state)
	   (control-state-diff state))
  (bit-and
   (control-state-diff state)
   (control-state-curr state)
   (control-state-jp state))
  (bit-andc2 
   (control-state-diff state)
   (control-state-curr state)
   (control-state-jr state))
  (bit-ior (control-state-jp state)
	   (control-state-repeat state)
	   (control-state-jp-or-repeat state)))
(defun update-control-state2 (&optional (state *control-state*))
  (fill (control-state-repeat state) 0)
  (bit-ior (control-state-curr state)
	   (control-state-curr state)
	   (control-state-prev state)))

(progn
  (defun skey-p (value &optional (state *control-state*))
    (= 1 (sbit (control-state-curr state) value)))
  (defun skey-j-p (value &optional (state *control-state*))
    (= 1 (sbit (control-state-jp state) value)))
  (defun skey-j-r (value &optional (state *control-state*))
    (= 1 (sbit (control-state-jr state) value)))
  (defun skey-j-p-or-repeat (value &optional (state *control-state*))
    (= 1 (sbit (control-state-jp-or-repeat state) value))))

(defmacro do-character-keys ((array-form true-p code) &body body)
  (with-gensyms (array)   
    `(let ((,array ,array-form))
       (declare (type mouse-keyboard-input-array ,array))
       (dotimes (,code 128)
	 (when (character-key-p ,code)
	   (let ((,true-p (= 1 (sbit ,array ,code))))
	     ,@body))))))

(defmacro character-key-p (x)
  `(= 1
      (sbit (etouq *character-keys*) ,x)))
;;;;</INPUT ARRAY>
;;;;************************************************************************;;;;

(defmacro button (type state button
		  &optional (control-state *control-state* control-state-supplied-p))
  ;;[FIXME]pair allowed-button-states and allowed-button-types with the
  ;;quoted lists below
  (let ((allowed-button-states '(:down :pressed :released :repeat))
	(allowed-button-types '(:mouse :key)))
    (assert (member type allowed-button-types)
	    nil "Wanted one of:state:~s~%Got:~s" allowed-button-types type)
    (assert (member state allowed-button-states)
	    nil "Wanted one of:type:~s~%Got:~s" allowed-button-states state)
    (mapc (lambda (keystate keyfun)
	    (when (eq state keystate)
	      (mapc (lambda (keytype keyfun2)
		      (when (eq type keytype)
			(return-from button `(,keyfun (,keyfun2 ,button)
						      ,@(when control-state-supplied-p
							  (list control-state))))))
		    allowed-button-types
		    '(mouseval keyval))))
	  allowed-button-states
	  '(skey-p skey-j-p skey-j-r skey-j-p-or-repeat))))
