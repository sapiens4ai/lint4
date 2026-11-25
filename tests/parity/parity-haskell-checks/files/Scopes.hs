module Scopes where
caught r = case r of Left (error :: E) -> error
bound x = let head = x in head
lambda = \last -> last
guarded xs | head xs = xs
