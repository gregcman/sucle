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
