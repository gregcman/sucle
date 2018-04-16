(defpackage #:application
  (:use #:cl #:utility)
  (:export
   #:main)
  (:export
   #:poll-app
   #:*quit-token*)
  (:export
   #:getfnc
   #:deflazy)
  (:export
   #:w
   #:h
   #:gl-context
   #:al-context)
  (:export
   #:*control-state*
   
   #:*camera*
   #:*render-area*
   
   #:set-render-area
   #:render-area-x
   #:render-area-y
   #:render-area-width
   #:render-area-height
   #:%set-render-area))
