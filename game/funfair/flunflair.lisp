(defpackage #:flunflair
  (:use #:cl #:funland))
(in-package :flunflair)

(defparameter *reloadables*
  '(
    text
    foo
    foo2
    text2))
(defun floatify (x)
  (coerce x 'single-float))

(progn
  (defparameter *textx* 0.0)
  (defparameter *texty* 0.0))

(progn
  (progn
    (defparameter *mouse-x* 0.0)
    (defparameter *mouse-y* 0.0))
  (progn
    (defparameter *old-mouse-x* 0.0)
    (defparameter *old-mouse-y* 0.0)))

(defun per-frame (&optional session)
  (declare (ignorable session))
  
  (when (window::mice-free-p)
    (let ((newmousex (floatify (/ window::*mouse-x* funtext::*block-width*)))
	  (newmousey (floatify (/ (- window::*height* window::*mouse-y*)
				  funtext::*block-height*))))
      (setf *old-mouse-x* *mouse-x*
	    *old-mouse-y* *mouse-y*)
      (setf *mouse-x* newmousex
	    *mouse-y* newmousey)))
  (when (and (window::mice-free-p)
	     (window::skey-p (window::mouseval :left)))
    (let ((dx (- *mouse-x* *old-mouse-x*))
	  (dy (- *mouse-y* *old-mouse-y*)))
      (unless (= dx dy 0)
	(incf *textx* dx)
	(incf *texty* dy)
	(funtext::flag-text-dirty))))

  (let ((num (funfair::num-key-jp)))
    (case num
      (1 (sound-stuff:play-sound-at "/home/imac/.minecraft/resources/sound3/dig/grass2.ogg"
				    0.0 0.0 0.0))
      #+nil
      (setf sndbx::*ent* (aref sndbx::*ents* num))))

  (progn
    (when (window::skey-p (window::keyval :n))
      (copy-array-buf))
    (when (window::skey-j-p (window::keyval :b))
      (funtext::flag-text-dirty))
    (when (window::skey-j-p (window::keyval :escape))
      (funfair::quit))
    (when (window::skey-j-p (window::keyval :y))
      (toggle sndbx::*depth-buffer?*))

    (when (window::skey-j-p (window::keyval :c))
      (sound-stuff::cleanup-poller)))
  
  (map nil #'funfair::reload-if-dirty *reloadables*)
  (let* ((particle (sndbx::entity-particle sndbx::*ent*))
	 (pos (sndbx::farticle-position particle))
	 (vel (sndbx::farticle-velocity particle)))

    (progn  
      (al:listener :position pos)
      (al:listener :velocity vel)
      (let ((curr (load-time-value (vector 1.0 0.0 0.0 ;;look
					   0.0 1.0 0.0 ;;up
					   )))
	    (other (camat::camera-vec-forward sndbx::*camera*))
	    (other2 (camat::camera-vec-up sndbx::*camera*)))
	(setf (aref curr 0) (- (aref other 0)))
	(setf (aref curr 1) (- (aref other 1)))
	(setf (aref curr 2) (- (aref other 2)))
	(setf (aref curr 3) (aref other2 0))
	(setf (aref curr 4) (aref other2 1))
	(setf (aref curr 5) (aref other2 2))
	(al:listener :orientation curr)))
    (with-vec (a b c) (pos)
      (setfoo (format nil
		      "x: ~10,1F
y: ~10,1F
z: ~10,1F"
		      a b c))))
  (unless (eq sndbx::*lastsel*
	      sndbx::*selection*)
    (setf sndbx::*lastsel*
	  sndbx::*selection*)
    (setfoo2
     (with-output-to-string (*standard-output*)
       (dolist (item sndbx::*selection*)
	 (pprint item)))))
  (let ((draw-commands (funfair::getfnc 'funtext::draw-commands)))
    (flet ((drawxyz (x y z)
	     (lparallel.queue:with-locked-queue draw-commands
	       (lparallel.queue:push-queue/no-lock x draw-commands)
	       (lparallel.queue:push-queue/no-lock y draw-commands)
	       (lparallel.queue:push-queue/no-lock z draw-commands))))
      (progn
	(drawxyz *textx* *texty* (glhelp::handle (funfair::getfnc 'text)))
	(drawxyz 10.0 10.0  (glhelp::handle (funfair::getfnc 'text2)))))))
(progn
  (defun setfoo2 (obj)
    (let ((*print-case* :downcase))
      (setf *foo2*
	    (write-to-string
	     obj :pretty t :escape nil)))
    (funtext::flag-text-dirty)
    (funfair::reload 'foo2))
  (defparameter *foo2* nil)
  (funfair::deflazy foo2 ()
    *foo2*)
  (funfair::deflazy text2 (foo2)
    (make-instance
     'glhelp::gl-list
     :handle
     (funtext::mesh-string-gl-points -128.0 -128.0 foo2))))

(progn
  (defun setfoo (obj)
    (let ((*print-case* :downcase))
      (setf *foo*
	    (write-to-string
	     obj :pretty t :escape nil)))
    (funfair::reload 'foo))
  (defparameter *foo* nil)
  (funfair::deflazy foo ()
    *foo*)

  (funfair::deflazy text (foo)
    (make-instance
     'glhelp::gl-list
     :handle
     (funtext::mesh-string-gl-points -128.0 -128.0 foo))))

(defun copy-array-buf ()
  (let ((width 256)
	(height 256))
    (cffi:with-foreign-object (b :uint8 (etouq (* 256 256 4)))
      (dobox ((ypos 0 height)
	      (xpos 0 width))
	     (let ((base (the fixnum (+ xpos (the fixnum (* ypos width))))))
	       (let ((offset (the fixnum (* 4 base))))
		 (let ((num
			#+nil
			(logior (char-code (aref *foo* (mod base 1024)))
				(ash 0 8)
				(ash 255 16))
					;	#+nil
			
			(ash (random (1- (ash 1 32))) -2)
			
			 #+nil
			 (get-char-num
			  (get-char (the fixnum (+ xpos xstart))
				    (the fixnum (+ ypos ystart))))))
		   (setf (cffi:mem-aref b :uint8 (+ offset 0)) (ldb (byte 8 16) num)
			 (cffi:mem-aref b :uint8 (+ offset 1)) (ldb (byte 8 8) num)
			 (cffi:mem-aref b :uint8 (+ offset 2)) (logand 255 num) 
			 (cffi:mem-aref b :uint8 (+ offset 3)) (ldb (byte 8 24) num))
		   ))))
      (progn
	(gl:bind-texture :texture-2d (glhelp::texture (funfair::getfnc 'funtext::text-data)))
	(gl:tex-sub-image-2d :texture-2d 0 0 0 width height :bgra :unsigned-byte b)))))

(defun wot ()
  (concatenate
   'string
   "/home/imac/.minecraft/resources/sound/step/"
   (string-downcase (symbol-name (aref #(stone wood gravel grass)
				       (random 4)
				       )))
   (aref #("1" "2" "3" "4") (random 4)
	 )
   ".ogg")
  
  )


(setf funfair::*trampoline* '(sndbx::per-frame funtext::per-frame per-frame))
