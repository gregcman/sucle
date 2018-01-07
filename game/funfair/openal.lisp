(defpackage #:sound-stuff
  (:use #:cl #:funland
	))
(in-package #:sound-stuff)


#+nil
("/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/My Little Pony/Unknown Album/At the Gala.mp3"
       "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Maroon 5/Songs About Jane/02 This Love.m4a"
       "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Unknown Artist/Unknown Album/form this way.mp3"
       "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Unknown Artist/Unknown Album/In Search of Diamonds (Minecraft  Music Video).mp3")

#+nil
("/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/damage/hit3.ogg"
 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/streaming/cat.ogg"
 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/portal/portal.ogg"
 "/home/imac/quicklisp/local-projects/symmetrical-umbrella/sandbox/res/resources/sound3/ambient/weather/rain4.ogg"
 "/home/imac/Music/Louis The Child ft. K.Flay - It's Strange [Premiere] (FIFA 16 Soundtrack) -  128kbps.mp3"
 "/home/imac/Music/Birdy_-_Keeping_Your_Head_Up_Official.mp3")
(defparameter *music*
  "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Unknown Artist/Unknown Album/Cher Lloyd - Want U Back (US Version).mp3"
  #+nil
  "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Unknown Artist/Unknown Album/bad seed.mp3"
  #+nil
  "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Taylor Swift/Red/04 I Knew You Were Trouble.m4a"
  #+nil
  "/home/imac/Music/6PQv-Adele - Hello.mp3")
(progn
  (defparameter dubs nil)
  (defparameter size nil)
  (defparameter rate nil))

(defun alut-test (&optional (music *music*))
  (reset)
  (setf (values dubs size rate)
	(cl-ffmpeg::get-sound-buff music))
  )

(defun reset ()
  (when (cffi::pointerp dubs)
    
    (cffi::foreign-free dubs)
    (setf dubs nil)))

(defun alut-hello-world ()
  (alc:make-context-current *alc-context*)
  (when (and *buffer* (cffi::pointerp *buffer*))
    (al:delete-buffer *buffer*))
  (when (and *source* (cffi::pointerp *source*))
    (al:delete-source *source*))
  (setf *source* (al:gen-source))
  (setf *buffer* (al:gen-buffer))
  (let ((source *source*)
	(buffer *buffer*))
    (al:source source :position (vector 0.0 0.0 0.0))
    (al:source source :velocity (vector 0.0 0.0 0.0))
    (al:source source :gain 1.0)
    (al:source source :pitch 1.0)
    (al:listener :gain 1.0)
    (al:listener :position (vector 0.0 0.0 0.0))
    (al:listener :orientation (vector 0.0 1.0 0.0 0.0 1.0 0.0))
    (al:buffer-data buffer :stereo16 (aref dubs 0) (* 2 size) rate
		    )
    (al:source source :buffer buffer)
    (al:source-play source)))

(defparameter *source* nil)
(defparameter *buffer* nil)

(defparameter *alc-device* nil)
(defun open-device ()
  (close-device)
  (setf *alc-device* (alc:open-device)))
(defun close-device ()
  (when (cffi:pointerp *alc-device*)
    (alc:close-device *alc-device*))
  (setf *alc-device* nil))
(defparameter *alc-context* nil)
(defun open-context ()
  (close-context)
  (let ((context (alc:create-context *alc-device*)))
    (setf *alc-context* context)
    (alc:make-context-current context)))
(defun close-context ()
  (when (cffi:pointerp *alc-context*)
    (when (cffi:pointer-eq *alc-context* (alc:get-current-context))
      (alc:make-context-current (cffi:null-pointer)))
    (alc:destroy-context *alc-context*))
  (setf *alc-context* nil))

(defun start-al ()
  (open-device)
  (open-context))

(defun destroy-al ()
  (close-context)
  (close-device))


(defparameter *al-on?* nil)
(unless *al-on?*
  (start-al)
  (setf *al-on?* t))

(progno
     ;;  (alc:make-context-current fuck::*alc-context*)
     
     (al:listener :position vec)
     (let ((curr *velocity*))
       (setf (aref curr 0) *xvel*)
       (setf (aref curr 1) *yvel*)
       (setf (aref curr 2) *zvel*)
       (al:listener :velocity curr))
     (let ((curr *orientation*)
	   (other (camera-vec-forward *camera*))
	   (other2 (camera-vec-up *camera*)))
       (setf (aref curr 0) (- (aref other 0)))
       (setf (aref curr 1) (- (aref other 1)))
       (setf (aref curr 2) (- (aref other 2)))
       (setf (aref curr 3) (aref other2 0))
       (setf (aref curr 4) (aref other2 1))
       (setf (aref curr 5) (aref other2 2))
       (al:listener :orientation curr)))

#+nil
(when (window:key-j-p :u)
  (al:source *source* :position (list sandbox::*xpos*
				      sandbox::*ypos*
				      sandbox::*zpos*))
  (alut-hello-world))


