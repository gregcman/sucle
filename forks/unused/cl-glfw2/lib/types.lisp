(defpackage #:cl-glfw-types
  (:use #:cl #:cffi)
  (:shadow #:boolean #:byte #:float #:char #:string #:pointer)
  (:export #:enum #:boolean #:bitfield #:byte #:short #:int #:sizei #:ubyte #:ushort #:uint 
           #:float #:clampf #:double #:clampd #:void #:uint64 #:int64 
           #:intptr #:sizeiptr 
           #:handle #:pointer
           #:char
           #:half))

(in-package #:cl-glfw-types)

(defmacro defgltype (type parser actual-type)
  `(define-foreign-type ,type ()
       ()
     (:actual-type ,actual-type)
     (:simple-parser ,parser)))

(defgltype gl-enum enum :uint32)
(defgltype gl-boolean boolean :uint8)
(defgltype gl-bitfield bitfield :uint32)
(defgltype gl-byte byte :int8)
(defgltype gl-short short :int16)
(defgltype gl-int int :int32)
(defgltype gl-sizei sizei :int32)
(defgltype gl-ubyte ubyte :uint8)
(defgltype gl-ushort ushort :uint16)
(defgltype gl-uint uint :uint32)
(defgltype gl-float float :float)
(defgltype gl-clampf clampf :float)
(defgltype gl-double double :double)
(defgltype gl-clampd clampd :double)
(defgltype gl-void void :void)

#-cffi-features:no-long-long
(defgltype gl-uint64 uint64 :uint64)
#-cffi-features:no-long-long
(defgltype gl-int64 int64 :int64)

;; Find a CFFI integer type the same foreign-size as a pointer
(defgltype gl-intprt intptr #.(find-symbol (format nil "INT~d" (* 8 (cffi:foreign-type-size :pointer))) (find-package '#:keyword)))
(defgltype gl-sizeiptr sizeiptr #.(find-symbol (format nil "INT~d" (* 8 (cffi:foreign-type-size :pointer))) (find-package '#:keyword)))

(defgltype gl-handle handle :unsigned-int)
(defgltype gl-char char :char)
;;(defctype string :string)
(defgltype gl-half half :unsigned-short) ; this is how glext.h defines it anyway
(defctype pointer :pointer)

(defmethod cffi:expand-to-foreign (value (type gl-boolean))
  `(if ,value 1 0))

(defmethod cffi:expand-from-foreign (value (type gl-boolean))
  `(not (= ,value 0)))

(defmethod cffi:expand-to-foreign (value (type gl-clampf))
  `(coerce ,value 'single-float))

(defmethod cffi:expand-to-foreign (value (type gl-clampd))
  `(coerce ,value 'double-float))

(defmethod cffi:expand-to-foreign (value (type gl-float))
  `(coerce ,value 'single-float))

(defmethod cffi:expand-to-foreign (value (type gl-double))
  `(coerce ,value 'double-float))

;; TODO: Maybe we can find/write a converter to a half? Does anyone want it?
;; TODO: Might we want converters to integer types? What would it be? round, or floor (or even ceil)?
