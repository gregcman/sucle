(defparameter *al-on?* nil)
  (unless *al-on?*
    (start-al)
    (setf *al-on?* t))



(defparameter *music*
  (or "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/My Little Pony/Unknown Album/At the Gala.mp3" "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Maroon 5/Songs About Jane/02 This Love.m4a" "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Unknown Artist/Unknown Album/form this way.mp3" "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Unknown Artist/Unknown Album/In Search of Diamonds (Minecraft  Music Video).mp3"
      (case 4
	(0 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/damage/hit3.ogg")
	(1 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/streaming/cat.ogg")
	(2 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/portal/portal.ogg")
	(3 "/home/imac/quicklisp/local-projects/symmetrical-umbrella/sandbox/res/resources/sound3/ambient/weather/rain4.ogg")
	(4 "/home/imac/Music/6PQv-Adele - Hello.mp3")
	(5 "/home/imac/Music/Louis The Child ft. K.Flay - It's Strange [Premiere] (FIFA 16 Soundtrack) -  128kbps.mp3")
	(6 "/home/imac/Music/Birdy_-_Keeping_Your_Head_Up_Official.mp3"))))
(progn
  (defparameter dubs nil)
  (defparameter size nil)
  (defparameter ans nil))

(defun alut-test (&optional (music *music*))
  (reset)
  (setf (values dubs size ans)
	(cl-ffmpeg::get-sound-buff music 44100))
  )

(defun reset ()
  (when (cffi::pointerp dubs)
    
    (cffi::foreign-free dubs)
    (setf dubs nil)))

(defun alut-hello-world ()
  (alc:make-context-current *alc-context*)
  
  (let ((source *source*)
	(buffer *buffer*))
    (al:buffer-data buffer :mono16 dubs (* 2 size) 44100)
    (al:source source :buffer buffer)
    (al:source-play source)))

(defparameter *source* (al:gen-source))
(defparameter *buffer* (al:gen-buffer))

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


