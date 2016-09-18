(defpackage #:window
  (:use #:cl)
  (:nicknames #:out #:in #:core)
  (:export 

   ;;this is some other stuff
   #:arise
   #:toggle-mouse-capture
   #:ismousecaptured
   #:wrapper
   #:base-needs
   #:status

   ;;input is this stuff
   #:x #:y #:delta #:right-p 
   #:mouse-button-p
   #:mouse-button-pressing-hook
   #:mouse-button-pressed-p 
   #:mouse-button-pressed-hook 
   #:mouse-button-released-p 
   #:mouse-button-released-hook
   #:key-p
   #:key-pressing-hook
   #:key-pressed-p 
   #:key-pressed-hook 
   #:key-released-p 
   #:key-released-hook
   #:clear-functions
   #:p+1
   #:p0
   #:p-1

   ;;output is below
   #:height #:width #:caption
   #:little-caption #:push-dimensions #:set-caption
   #:set-micro-caption #:push-titles
   #:pushed-width #:pushed-height
   ))

(defpackage #:sandbox
  (:use #:cl)
  (:export #:main))

 
