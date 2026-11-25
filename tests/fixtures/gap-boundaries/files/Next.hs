module Next where

f :: Text -> Int
f = T.length

g :: Text -> Int
g = T.length

shown :: Bool -> Html
shown b = page
hidden :: Bool -> Html
hidden b = page

data P = A { a :: Text, b :: Text, c :: Text } | B { d :: Text, e :: Text, f :: Text }

size n = if n < 1 then A else if n < 2 then B else if n < 3 then C else if n < 4 then D else if n < 5 then E else F
