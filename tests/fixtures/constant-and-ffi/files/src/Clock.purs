module Clock where

import Effect (Effect)

buffer = 4096

foreign import now :: Effect Number

first = now
sooner = now
never = now
always = now
maybe = now
soon = now
ok = now
