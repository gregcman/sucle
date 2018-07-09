(defpackage #:utility
  (:use #:cl)
  (:export
   #:dobox
   #:toggle
   #:progno
   #:etouq
   #:%list
   #:with-declaim-inline
   #:with-vec-params
   #:with-vec
   #:%%with-vec-params
   #:with-vec-params2
   #:with-let-mapped-places
   #:%with-let-mapped-places
   #:with-unsafe-speed
   #:eval-always)

  (:export
   #:spill-hash
   #:dohash)

  (:export
   #:floatify
   #:byte/255
   #:clamp)

  (:export
   #:symbolicate2
   #:with-gensyms
   #:make-gensym-list
   #:nest
   #:%nest
   #:once-only
   #:parse-body)

  (:export
   #:keywordify)

  (:export
   #:fixnum-bits
   #:print-bits)

  (:export
   #:any))
