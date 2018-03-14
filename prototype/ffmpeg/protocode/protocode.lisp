
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

"  191 enum AVMediaType {
  192     AVMEDIA_TYPE_UNKNOWN = -1,  ///< Usually treated as AVMEDIA_TYPE_DATA
  193     AVMEDIA_TYPE_VIDEO,
  194     AVMEDIA_TYPE_AUDIO,
  195     AVMEDIA_TYPE_DATA,          ///< Opaque data information usually continuous
  196     AVMEDIA_TYPE_SUBTITLE,
  197     AVMEDIA_TYPE_ATTACHMENT,    ///< Opaque data information usually sparse
  198     AVMEDIA_TYPE_NB
  199 };"

"   59 enum AVSampleFormat {
   60     AV_SAMPLE_FMT_NONE = -1,
   61     AV_SAMPLE_FMT_U8,          ///< unsigned 8 bits
   62     AV_SAMPLE_FMT_S16,         ///< signed 16 bits
   63     AV_SAMPLE_FMT_S32,         ///< signed 32 bits
   64     AV_SAMPLE_FMT_FLT,         ///< float
   65     AV_SAMPLE_FMT_DBL,         ///< double
   66 
   67     AV_SAMPLE_FMT_U8P,         ///< unsigned 8 bits, planar
   68     AV_SAMPLE_FMT_S16P,        ///< signed 16 bits, planar
   69     AV_SAMPLE_FMT_S32P,        ///< signed 32 bits, planar
   70     AV_SAMPLE_FMT_FLTP,        ///< float, planar
   71     AV_SAMPLE_FMT_DBLP,        ///< double, planar
   72 
   73     AV_SAMPLE_FMT_NB           ///< Number of sample formats. DO NOT USE if linking dynamically
   74 };"

"   49 #define AV_CH_FRONT_LEFT             0x00000001
   50 #define AV_CH_FRONT_RIGHT            0x00000002
   51 #define AV_CH_FRONT_CENTER           0x00000004
   52 #define AV_CH_LOW_FREQUENCY          0x00000008
   53 #define AV_CH_BACK_LEFT              0x00000010
   54 #define AV_CH_BACK_RIGHT             0x00000020
   55 #define AV_CH_FRONT_LEFT_OF_CENTER   0x00000040
   56 #define AV_CH_FRONT_RIGHT_OF_CENTER  0x00000080
   57 #define AV_CH_BACK_CENTER            0x00000100
   58 #define AV_CH_SIDE_LEFT              0x00000200
   59 #define AV_CH_SIDE_RIGHT             0x00000400
   60 #define AV_CH_TOP_CENTER             0x00000800
   61 #define AV_CH_TOP_FRONT_LEFT         0x00001000
   62 #define AV_CH_TOP_FRONT_CENTER       0x00002000
   63 #define AV_CH_TOP_FRONT_RIGHT        0x00004000
   64 #define AV_CH_TOP_BACK_LEFT          0x00008000
   65 #define AV_CH_TOP_BACK_CENTER        0x00010000
   66 #define AV_CH_TOP_BACK_RIGHT         0x00020000
   67 #define AV_CH_STEREO_LEFT            0x20000000  ///< Stereo downmix.
   68 #define AV_CH_STEREO_RIGHT           0x40000000  ///< See AV_CH_STEREO_LEFT.
   69 #define AV_CH_WIDE_LEFT              0x0000000080000000ULL
   70 #define AV_CH_WIDE_RIGHT             0x0000000100000000ULL
   71 #define AV_CH_SURROUND_DIRECT_LEFT   0x0000000200000000ULL
   72 #define AV_CH_SURROUND_DIRECT_RIGHT  0x0000000400000000ULL
   73 #define AV_CH_LOW_FREQUENCY_2        0x0000000800000000ULL
   74 
   75 /** Channel mask value used for AVCodecContext.request_channel_layout
   76     to indicate that the user requests the channel order of the decoder output
   77     to be the native codec channel order. */
   78 #define AV_CH_LAYOUT_NATIVE          0x8000000000000000ULL
   79 
   80 /**
   81  * @}
   82  * @defgroup channel_mask_c Audio channel convenience macros
   83  * @{
   84  * */
   85 #define AV_CH_LAYOUT_MONO              (AV_CH_FRONT_CENTER)
   86 #define AV_CH_LAYOUT_STEREO            (AV_CH_FRONT_LEFT|AV_CH_FRONT_RIGHT)
   87 #define AV_CH_LAYOUT_2POINT1           (AV_CH_LAYOUT_STEREO|AV_CH_LOW_FREQUENCY)
   88 #define AV_CH_LAYOUT_2_1               (AV_CH_LAYOUT_STEREO|AV_CH_BACK_CENTER)
   89 #define AV_CH_LAYOUT_SURROUND          (AV_CH_LAYOUT_STEREO|AV_CH_FRONT_CENTER)
   90 #define AV_CH_LAYOUT_3POINT1           (AV_CH_LAYOUT_SURROUND|AV_CH_LOW_FREQUENCY)
   91 #define AV_CH_LAYOUT_4POINT0           (AV_CH_LAYOUT_SURROUND|AV_CH_BACK_CENTER)
   92 #define AV_CH_LAYOUT_4POINT1           (AV_CH_LAYOUT_4POINT0|AV_CH_LOW_FREQUENCY)
   93 #define AV_CH_LAYOUT_2_2               (AV_CH_LAYOUT_STEREO|AV_CH_SIDE_LEFT|AV_CH_SIDE_RIGHT)
   94 #define AV_CH_LAYOUT_QUAD              (AV_CH_LAYOUT_STEREO|AV_CH_BACK_LEFT|AV_CH_BACK_RIGHT)
   95 #define AV_CH_LAYOUT_5POINT0           (AV_CH_LAYOUT_SURROUND|AV_CH_SIDE_LEFT|AV_CH_SIDE_RIGHT)
   96 #define AV_CH_LAYOUT_5POINT1           (AV_CH_LAYOUT_5POINT0|AV_CH_LOW_FREQUENCY)
   97 #define AV_CH_LAYOUT_5POINT0_BACK      (AV_CH_LAYOUT_SURROUND|AV_CH_BACK_LEFT|AV_CH_BACK_RIGHT)
   98 #define AV_CH_LAYOUT_5POINT1_BACK      (AV_CH_LAYOUT_5POINT0_BACK|AV_CH_LOW_FREQUENCY)
   99 #define AV_CH_LAYOUT_6POINT0           (AV_CH_LAYOUT_5POINT0|AV_CH_BACK_CENTER)
  100 #define AV_CH_LAYOUT_6POINT0_FRONT     (AV_CH_LAYOUT_2_2|AV_CH_FRONT_LEFT_OF_CENTER|AV_CH_FRONT_RIGHT_OF_CENTER)
  101 #define AV_CH_LAYOUT_HEXAGONAL         (AV_CH_LAYOUT_5POINT0_BACK|AV_CH_BACK_CENTER)
  102 #define AV_CH_LAYOUT_6POINT1           (AV_CH_LAYOUT_5POINT1|AV_CH_BACK_CENTER)
  103 #define AV_CH_LAYOUT_6POINT1_BACK      (AV_CH_LAYOUT_5POINT1_BACK|AV_CH_BACK_CENTER)
  104 #define AV_CH_LAYOUT_6POINT1_FRONT     (AV_CH_LAYOUT_6POINT0_FRONT|AV_CH_LOW_FREQUENCY)
  105 #define AV_CH_LAYOUT_7POINT0           (AV_CH_LAYOUT_5POINT0|AV_CH_BACK_LEFT|AV_CH_BACK_RIGHT)
  106 #define AV_CH_LAYOUT_7POINT0_FRONT     (AV_CH_LAYOUT_5POINT0|AV_CH_FRONT_LEFT_OF_CENTER|AV_CH_FRONT_RIGHT_OF_CENTER)
  107 #define AV_CH_LAYOUT_7POINT1           (AV_CH_LAYOUT_5POINT1|AV_CH_BACK_LEFT|AV_CH_BACK_RIGHT)
  108 #define AV_CH_LAYOUT_7POINT1_WIDE      (AV_CH_LAYOUT_5POINT1|AV_CH_FRONT_LEFT_OF_CENTER|AV_CH_FRONT_RIGHT_OF_CENTER)
  109 #define AV_CH_LAYOUT_7POINT1_WIDE_BACK (AV_CH_LAYOUT_5POINT1_BACK|AV_CH_FRONT_LEFT_OF_CENTER|AV_CH_FRONT_RIGHT_OF_CENTER)
  110 #define AV_CH_LAYOUT_OCTAGONAL         (AV_CH_LAYOUT_5POINT0|AV_CH_BACK_LEFT|AV_CH_BACK_CENTER|AV_CH_BACK_RIGHT)
  111 #define AV_CH_LAYOUT_STEREO_DOWNMIX    (AV_CH_STEREO_LEFT|AV_CH_STEREO_RIGHT)"
