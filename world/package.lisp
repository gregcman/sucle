(defpackage #:world
  (:use :cl :recycler)	
  (:nicknames #:w)
  (:export 

;;;block accessors
   #:getblock #:setblock
   #:%getblock #:%setblock
   #:getlight #:setlight
   #:%getlight #:%setlight
   #:skygetlight #:skysetlight
   #:%skygetlight #:%skysetlight
   #:setmeta #:getmeta

;;;pointer manipulation
   #:%%ref 
   #:chop
   #:rem-flow
   #:anti-chop
   #:add
   #:unhashfunc
   #:chunkhashfunc

;;;initialization
   #:setup-hashes
   #:define-accessors
   #:establish-stystem
   #:gen-holder
   #:system
   #:world-init


;;;containers
   #:chunkhash
   #:lighthash
   #:skylighthash
   #:metahash

   #:heighthash

;;;chunks
   #:clearchunk
   #:%new-chunk

;;;the apocalypse
   #:clearworld
   #:send-to-free-mem
   ))
