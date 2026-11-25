module Clock where

buffer = 4096

foreign import now :: Effect Number
