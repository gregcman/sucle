(defpackage #:window
  (:use #:cl #:utility)
  (:export
   #:*scroll-x*
   #:*scroll-y*)
  (:export
   #:skey-p
   #:skey-j-p
   #:skey-j-r)
  (:export
   #:get-proc-address
   #:init
   #:poll
   #:wrapper
   #:update-display
   #:set-vsync
   #:push-dimensions  
   #:set-caption)  
  (:export
   #:get-mouse-out
   #:get-mouse-position
   #:mouse-locked?
   #:mouse-free?
   #:toggle-mouse-capture)
  (:export
   #:*width*
   #:*height*
   #:*status*)
  (:export
   #:*resize-hook*)
  (:export
   #:*mouse-x*
   #:*mouse-y*)

  (:export
   #:button))
