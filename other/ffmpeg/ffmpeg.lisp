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
    (cffi:with-foreign-object (double-pointer :pointer)
      (cffi:with-foreign-object (an-int :int)
	(setf aans
	      (decode-audio-file
	       (case 1
		 (0 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/damage/hit3.ogg")
		 (1 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/streaming/cat.ogg")
		 (2 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/portal/portal.ogg"))
	       44100
	       double-pointer
	       an-int))
	(setf adubs (cffi:mem-ref double-pointer :pointer))
	(setf asize (cffi:mem-ref an-int :int))))
    (values adubs asize aans)))

(defparameter dubs nil)
(defparameter size nil)
(defparameter ans nil)
(defparameter actual-dubs nil)
(defparameter actual-size 0)


(defun test ()
  (reset)
  (setf (values dubs size ans)
	(get-sound-buff))
  (let ((act-size size))
    (let ((pcmbuf (foreign-alloc :int16 :count act-size)))
      (setf actual-size act-size)
      (setf actual-dubs pcmbuf)
      (dotimes (index act-size)
	(let ((value (mem-aref dubs :double index)))
	  (let ((foo (truncate (- (* 32767.5d0
				     (1+ (min 1.0 (max -1.0 value))))
				  32768d0))))
	    (setf (mem-aref pcmbuf :int16 index)
		  foo)))))))


(defun reset ()
  (when dubs
    
    (foreign-free dubs)
    (setf dubs nil))
  (when actual-dubs
    (foreign-free actual-dubs)
    (setf actual-dubs nil)))

(progno
 (case 0
   (1 (print (alut:create-buffer-from-file "/home/terminal256/Music/Bustin.wav")))
   (2 (alut:create-buffer-hello-world))))

(defun alut-hello-world ()
  (alut:with-init
    (al:with-source (source)
      (al:with-buffer (buffer)
	(al:buffer-data buffer :mono16 actual-dubs (* 2 actual-size) 44100)
	(print (alut:get-error))
        (al:source source :buffer buffer)
        (al:source-play source)
	(read)))))
