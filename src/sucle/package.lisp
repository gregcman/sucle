(defpackage #:sucle
  (:use #:cl)
  (:import-from #:utility
		#:dobox
		#:with-vec
		#:floatify
		#:etouq
		#:once-only
		#:dohash
		#:%list
		#:toggle)
  (:export #:start))
(defpackage #:entity
  (:use #:cl)
  (:import-from #:utility
                #:dobox
                #:with-vec
                #:floatify
                #:once-only
                #:toggle)
  (:export
   ;; physics
   :pos
   :pos-old
   :neck-pitch
   :neck-yaw
   :direction
   :jump-p
   :step-physics

   ;; ai
   :run-ai

   ;; entity
   :create-player-entity
   :sneak-p
   :fly-p

   :create-dumb-entity))
(defpackage #:world
  (:use :cl)
  (:import-from #:utility
		#:dobox)
  (:export
   ;;;block accessors
   #:getblock #:setblock
   #:getlight #:setlight
   #:skygetlight #:skysetlight
   #:getblock-extract
   #:getlight-extract
   #:skygetlight-extract
   #:num-getobj

   #:blockify

   #:*dirty-chunks*
   #:dirty-pop
   #:plain-setblock
   #:clean-dirty
   #:dirty-push
   #:dirty-push-around

   #:*world-directory*
   #:*some-saves*
   #:*persist*
   #:msave
   #:load-world
   
   #:set-chunk-coordinate-center
   #:unsquared-chunk-distance
   #:blocky-chunk-distance 
   #:*chunk-radius*
  ))
(defpackage #:block-data
  (:use #:cl)
  (:import-from #:utility
		#:keywordify
		#:with-gensyms)
  (:export
   #:data
   #:lookup))
