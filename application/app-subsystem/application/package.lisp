(defpackage #:application
  (:use #:cl #:utility)
  (:export
   #:main
   #:*thread*
   #:*main-subthread-p*)
  (:export
   #:poll-app
   #:*quit-token*
   #:on-session-change)
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
