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
   #:al-context))
