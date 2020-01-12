(defpackage #:sucle
  (:use #:cl #:utility #:application #:control #:struct-to-clos)
  (:export #:start))
(defpackage #:world
  (:use :cl #:utility)
  (:export
   ;;;block accessors
   #:getblock #:setblock
   #:getlight #:setlight
   #:skygetlight #:skysetlight
   #:getblock-extract
   #:getlight-extract
   #:skygetlight-extract
   #:num-getobj

   #:blockify)
  (:export
   #:world-path
   #:savechunk
   #:loadchunk
   
   #:filename-to-chunk-coordinate
   #:chunk-coordinate-to-filename
   #:*some-saves*
   #:*world-directory*
   #:convert-object-to-filename))
(defpackage #:block-data
  (:use #:cl
	#:utility))
