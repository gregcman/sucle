(defpackage #:world
  (:use :cl :recycler)	
  (:nicknames #:w)
  (:export 

;;;;block accessors
#:getblock #:setblock
#:%getblock #:%setblock
#:getlight #:setlight
#:%getlight #:%setlight
#:skygetlight #:skysetlight
#:%skygetlight #:%skysetlight
#:setmeta #:getmeta

;;;;pointer manipulation
#:%%ref 
#:chop
#:rem-flow
#:anti-chop
#:add
#:unhashfunc
#:chunkhashfunc

;;;;keep track of changes
#:clean-dirty
#:dirty-pop
#:dirty-push
#:block-dirtify

;;;;initialization
#:setup-hashes
#:define-accessors
#:establish-stystem
#:gen-holder
#:system
#:world-init

;;;;chunks
#:clearchunk
#:%new-chunk

;;;;the apocalypse
#:clearworld
#:send-to-free-mem
))
