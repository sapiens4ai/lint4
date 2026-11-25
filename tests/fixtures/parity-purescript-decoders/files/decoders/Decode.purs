module Decode where
data Theme = Dark | Other String
decodeTheme :: Json -> Either String Theme
decodeTheme json = case toString json of
  "dark" -> Right Dark
  _ -> Right Dark
pick :: Int -> Int
pick n = if n == 1 then 1 else if n == 2 then 2 else if n == 3 then 3 else 0
decodeTag :: Json -> Either String Theme
decodeTag json = case tag of
  "dark" -> Right Dark
  _ -> Right Dark
  where tag = toString json
label :: String -> Json -> String
label mode json = case mode of
  "x" -> "X"
  _ -> "other"
