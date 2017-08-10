
(progno
 (defun read-yolo (text)
   (let ((state :nope)
	 (text (cons " " (split-sequence:split-sequence #\newline text)))
	 acc)
     (dolist (item text)
       (if (equalp item " ")
	   (setf state :eat-next)
	   (progn
	     (when (eq state :eat-next)
	       (push (split-sequence:split-sequence #\tab item)
		     acc))
	     (setf state :full))))
     (nreverse acc)))

 (defun print-out (list)
   (let ((*print-case* :downcase))
     (dolist (item list)
       (let ((a (first item))
	     (b (second item)))
	 (princ "(")
	 (princ b)
	 (princ " ")
	 (let ((a2 (pase-tip a)))
	   (let* ((pointers (end-pointers? a2))
		  (type (get-item
			 (if (zerop pointers)
			     a2
			     (nbutlast a2)))))
	     (let ((acc type))
	       (dotimes (i pointers)
		 (setf acc (if t :pointer (list :pointer acc))))
	       (prin1 acc))))
	 (princ ")"))
       (terpri))))

 (defun pase-tip (string)
   (let ((values (split-sequence:split-sequence #\space string)))
     (setf values (delete-if
		   (lambda (x)
		     (or (equal x "")
			 (equal x "const")
			 (equal x "attribute_deprecated"))) values))
     values))

 (defun end-pointers? (list)
   (let ((item (car (last list))))
     (if (eql #\* (aref item 0))
	 (array-total-size item)
	 0)))

 (defparameter *c-types* (make-hash-table :test 'equal))
 (defun add-item (item keyword)
   (let ((items (split-sequence:split-sequence #\space item)))
     (setf (gethash items *c-types*) keyword)))
 (defun get-item (item)
   (if (equal "enum" (car item))
       :int
       (or (gethash item *c-types*)
	   (intern (car item)))))


 (mapcar #'(lambda (x) (add-item (first x)
				 (second x)))
	 (quote (("char" :char)
		 ("unsigned char" :unsigned-char)
		 ("short" :short)
		 ("unsigned short" :unsigned-short)
		 ("int" :int)
		 ("unsigned int" :uint)
		 ("long" :long)
		 ("unsigned long" :unsigned-long)
		 ("long long" :long-long)
		 ("unsigned long long" :unsigned-long-long)
		 ("float" :float)
		 ("double" :double)
		 ("int8_t" :int8)
		 ("int16_t" :int16)
		 ("int32_t" :int32)
		 ("int64_t" :int64)
		 ("uint8_t" :uint8)
		 ("uint16_t" :uint16)
		 ("uint32_t" :uint32)
		 ("uint64_t" :uint64)
		 ("void" :void)
		 ("unsigned" :unsigned-int)))))

(progno
 (defun decode-audio-file (path sample-rate data size)
   (av-register-all)
   (with-foreign-object (format-pointer :pointer)
     (setf (mem-ref format-pointer :pointer) (avformat-alloc-context))
     (unless
	 (zerop
	  (with-foreign-string (cpath path)
	    (avformat-open-input
	     format-pointer
	     cpath
	     (cffi:null-pointer)
	     (cffi:null-pointer))))
       (print "could not open file")
       (return-from decode-audio-file -1))
     (when (< (avformat-find-stream-info
	       (mem-ref format-pointer :pointer)
	       (cffi:null-pointer))
	      0)
       (print "could not retrieve stream info from file")
       (return-from decode-audio-file -1))
     (let ((avformat (mem-ref format-pointer :pointer)))
       (let ((stream-index -1))
	 (block break
	   (with-foreign-slots ((nb_streams streams) avformat (:struct |AVFormatContext|))	   
	     (dotimes (i nb_streams)
	       (when (= 1 (with-foreign-slots ((codec) streams |AVStream|)
			    (with-foreign-slots ((codec_type) codec |AVCodecContext|)
			      codec_type)))
		 (setf stream-index i)
		 (return-from break)))))
	 (when (= -1 stream-index)
	   (print "could not retrieve audio stream from file")
	   (return-from decode-audio-file -1))
	 (let ((stream-pointer (with-foreign-slots ((streams)))))))
       (av-frame-free frame-pointer)
       (swr-free swr-pointer)
       (avcodec-close codec)
       (avformat-free-context avformat)))))

(progno
 (eval-always
  (progn
    (defparameter *something* #.(or *compile-file-truename* *load-truename*))
    (defparameter ourdir
      (make-pathname :host (pathname-host *something*)
		     :directory (pathname-directory *something*)))

    (defparameter *dylib* (namestring (merge-pathnames "csrc/libprog" ourdir)))))
 (etouq
  `(define-foreign-library cl-sound-ffmpeg
     (t (:default ,*dylib*))))
 (defun reload-lib ()
   (use-foreign-library cl-sound-ffmpeg))
 (reload-lib)
;;;int decode_audio_file(const char* path, const int sample_rate, double** data, int* size)
 (defcfun "decode_audio_file" :int
   (path :string)
   (sample-rate :int)
   (data :pointer)
   (size :pointer))
 (defcfun "iterate_through_frames"
     :int
   (format :pointer)
   (packet :pointer)
   (codec :pointer)
   (frame :pointer)
   (swr :pointer)
   (data :pointer)
   (size :pointer))
 (defcfun "find_first_audio_stream" :int
   (format :pointer))
 (defcfun ("prepare_resampler" prepare-resampler)
     :void
   (swr :pointer)
   (codec :pointer)
   (sample_rate :int))
 (defun get-sound-bufforign ()
   (let ((adubs nil)
	 (aans nil)
	 (asize -3))
     (cffi:with-foreign-object (data-pointer :pointer)
       (cffi:with-foreign-object (an-int :int)
	 (setf aans
	       (decode-audio-file
		*music*
		44100
		data-pointer
		an-int))
	 (setf adubs (cffi:mem-ref data-pointer :pointer))
	 (setf asize (cffi:mem-ref an-int :int))))
     (values adubs asize aans))))
