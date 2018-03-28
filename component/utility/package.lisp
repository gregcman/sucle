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
   #:floatify))
