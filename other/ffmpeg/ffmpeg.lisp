(defpackage :cl-sound-ffmpeg
  (:use :cl :cffi :fuktard))

(in-package :cl-sound-ffmpeg)

(eval-always
 (progn
   (defparameter *something* #.(or *compile-file-truename* *load-truename*))
   (defparameter ourdir
     (make-pathname :host (pathname-host *something*)
		    :directory (pathname-directory *something*)))
   (defparameter *dylib* (namestring (merge-pathnames "csrc/libprog" ourdir)))
   (etouq
    `(define-foreign-library cl-sound-ffmpeg
       (t (:default ,*dylib*))))))

(use-foreign-library cl-sound-ffmpeg)

;;;int decode_audio_file(const char* path, const int sample_rate, double** data, int* size)

(defcfun "decode_audio_file" :int
  (path :string)
  (sample-rate :int)
  (data :pointer)
  (size :pointer))


(defun get-sound-buff ()
  (let ((adubs nil)
	(aans nil)
	(asize -3))
    (cffi:with-foreign-object (data-pointer :pointer)
      (cffi:with-foreign-object (an-int :int)
	(setf aans
	      (decode-audio-file
	       (case 6
		 (0 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/damage/hit3.ogg")
		 (1 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/streaming/cat.ogg")
		 (2 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/portal/portal.ogg")
		 (3 "/home/imac/quicklisp/local-projects/symmetrical-umbrella/sandbox/res/resources/sound3/ambient/weather/rain4.ogg")
		 (4 "/home/imac/Music/6PQv-Adele - Hello.mp3")
		 (5 "/home/imac/Music/Louis The Child ft. K.Flay - It's Strange [Premiere] (FIFA 16 Soundtrack) -  128kbps.mp3")
		 (6 "/home/imac/Music/Birdy_-_Keeping_Your_Head_Up_Official.mp3"))
	       44100
	       data-pointer
	       an-int))
	(setf adubs (cffi:mem-ref data-pointer :pointer))
	(setf asize (cffi:mem-ref an-int :int))))
    (values adubs asize aans)))

(defparameter dubs nil)
(defparameter size nil)
(defparameter ans nil)


(defun test ()
  (reset)
  (setf (values dubs size ans)
	(get-sound-buff))
  )

(defun reset ()
  (when dubs
    
    (foreign-free dubs)
    (setf dubs nil)))

(defun alut-hello-world ()
  (alc:with-device (device)
    (alc:with-context (context device)
      (alc:make-context-current context)
      (al:with-source (source)
	(al:with-buffer (buffer)
	  (al:buffer-data buffer :mono16 dubs (* 2 size) 44100)
	  (al:source source :buffer buffer)
	  (al:source-play source)
	  (read))))))
