(defpackage #:application
  (:use #:cl #:utility)
  (:export main)
  (:export *trampoline*)
  (:export getfnc deflazy)
  (:export w h gl-context al-context)
  (:export microseconds tick *control-state* *camera* *render-area* *pre-trampoline-hooks*
	   set-render-area render-area-x render-area-y render-area-width render-area-height
	   %set-render-area))
