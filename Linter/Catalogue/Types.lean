import Linter.Domain

namespace Linter.Catalogue.Types

open Linter

def segments (words : List String) : Piece :=
  .anyOf (words.map .segment)

def symbol (text : String) : Piece :=
  .exact (.word .symbol text)

def keyword (text : String) : Piece :=
  .exact (.word .identifier text)

def haskellish : List Language := [.haskell, .purescript]

def nonEmptyText : TokenShape := .except (.kind .text) (.word .text "\"\"")

def textTypes : Piece := segments ["String", "Text"]

def flagTypes : Piece := segments ["Bool", "Boolean"]

def twoParameters (type : Piece) : List Piece :=
  [symbol "::", .gap 6, type, symbol "->", .gap 6, type, symbol "->"]

def wordAsType : Rule where
  id := ⟨.types, "word-as-type"⟩
  why := "Comparing against a string literal uses a word where a type belongs"
  fix := "Introduce a sum type and match on its constructors"
  checks := [
    { detector := .fixed .noGo (.tokens haskellish .code [.anyOf [.word .symbol "==", .word .symbol "/="], .exact nonEmptyText])
      examples := [{ bad := [⟨"Bad.hs", "isAdmin r = role r == \"admin\"\n"⟩, ⟨"Bad.purs", "isOpen s = s /= \"closed\"\n"⟩],
                     good := [⟨"Good.hs", "label = \"role == \\\"admin\\\"\"\nsame a b = a == b\nlimit n = n == 3\n" ++
                       "blank s = s == \"\" || s /= \"\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens haskellish .code [.exact (.segment "Just"), .exact (.kind .text)])
      examples := [{ bad := [⟨"Bad.hs", "pick m = m == Just \"admin\"\n"⟩],
                     good := [⟨"Good.hs", "import Data.Maybe (Just)\nwrap = Just 3\nlabel = \"Just \\\"x\\\"\"\n"⟩] }] }]

def rawText : Rule where
  id := ⟨.types, "raw-text"⟩
  why := "Several raw text parameters can be swapped without the compiler noticing"
  fix := "Wrap each meaning in its own newtype"
  checks := [
    { detector := .fixed .noGo (.tokens haskellish .code (twoParameters textTypes))
      examples := [{ bad := [⟨"Bad.hs", "greet :: Text -> T.Text -> Text\ngreet a b = a <> b\n"⟩,
                             ⟨"Bad.purs", "greet :: String -> String -> String\ngreet a b = a <> b\n"⟩],
                     good := [⟨"Good.hs", "import Data.Text (Text, String)\n" ++
                       "greet :: Name -> Text -> Text\ngreet n t = t\nnote = \"Text -> Text ->\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens haskellish .code [.lineStart (.word .identifier "type"), .exact (.kind .identifier), symbol "=", textTypes])
      examples := [{ bad := [⟨"Bad.hs", "type Email = Text\n"⟩, ⟨"Bad.purs", "type Email = String\n"⟩],
                     good := [⟨"Good.hs", "newtype Email = Email Text\ntype Names = Map Text Int\nnote = \"type X = Text\"\n"⟩] }] }]

def booleanFlags : Rule where
  id := ⟨.types, "boolean-flags"⟩
  why := "Several Boolean parameters are easy to pass in the wrong order"
  fix := "Replace the flags with sum types that name each choice"
  checks := [
    { detector := .fixed .warning (.tokens haskellish .code (twoParameters flagTypes))
      examples := [{ bad := [⟨"Bad.hs", "render :: Bool -> Bool -> Html\nrender a b = page\n"⟩,
                             ⟨"Bad.purs", "render :: Boolean -> Int -> Boolean -> Html\nrender a n b = page\n"⟩],
                     good := [⟨"Good.hs", "import Data.Bool (Bool)\nvisible :: Bool -> Html\nvisible b = page\nnote = \"Bool -> Bool ->\"\n"⟩] }] }]

def stringField : List Piece := [.gap 12, symbol "::", .gap 1, textTypes]

def stringRecord : Rule where
  id := ⟨.types, "string-record"⟩
  why := "A record of many raw text fields carries no meaning in its types"
  fix := "Give the fields domain types"
  checks := [
    { detector := .fixed .warning (.tokens haskellish .code
        ([symbol "{"] ++ stringField ++ stringField ++ stringField ++ stringField ++ stringField ++ stringField))
      examples := [{ bad := [⟨"Bad.hs", "data P = P\n  { a :: Text\n  , b :: Text\n" ++
                       "  , c :: Maybe Text\n  , d :: Text\n  , e :: T.Text\n  , f :: Text\n  }\n"⟩,
                             ⟨"Bad.purs", "type P = { a :: String, b :: String, c :: String, d :: String, e :: String, f :: String }\n"⟩],
                     good := [⟨"Good.hs", "data P = P\n  { a :: Text\n  , b :: Name\n  , c :: Int\n  , d :: Text\n  }\n" ++
                       "note = \"{ a :: Text, b :: Text, c :: Text, d :: Text, e :: Text, f :: Text }\"\n"⟩] }] }]

def untypedTime : Rule where
  id := ⟨.types, "untyped-time"⟩
  why := "A timestamp or duration stored as a raw number or string loses its unit"
  fix := "Use a time or duration type"
  checks := [
    { detector := .fixed .couldBeBetter (.tokens haskellish .code
        [.exact (.suffix "Ms"), symbol "::", segments ["Int", "Integer", "Number", "Double"]])
      examples := [{ bad := [⟨"Bad.hs", "data T = T { timeoutMs :: Int }\n"⟩, ⟨"Bad.purs", "type T = { delayMs :: Number }\n"⟩],
                     good := [⟨"Good.hs", "data T = T { items :: Int, timeout :: Milliseconds }\nnote = \"timeoutMs :: Int\"\n"⟩] }] },
    { detector := .fixed .couldBeBetter (.tokens haskellish .code [.exact (.suffix "At"), symbol "::", textTypes])
      examples := [{ bad := [⟨"Bad.hs", "data T = T { createdAt :: Text }\n"⟩, ⟨"Bad.purs", "type T = { updatedAt :: String }\n"⟩],
                     good := [⟨"Good.hs", "data T = T { chat :: Text, createdAt :: UTCTime }\nnote = \"createdAt :: Text\"\n"⟩] }] }]

def exposed : Piece := .anyOf (["String", "Nat", "Int", "Json"].map (.word .identifier ·))

def unprotected : Rule where
  id := ⟨.types, "unprotected"⟩
  why := "A public signature of raw String, Nat, Int or Json lets callers pass any value"
  fix := "Refine the type with a domain type or a subtype"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code [.lineStart (.word .identifier "def"), .exact (.kind .identifier), .gap 16,
        symbol ":", .gap 2, exposed])
      examples := [{ bad := [⟨"Bad.lean", "def greet (name : String) : Greeting := ⟨name⟩\n"⟩,
                             ⟨"Total.lean", "def total (xs : Orders) : Nat := xs.size\n"⟩],
                     good := [⟨"Good.lean", "private def greet (name : String) : Nat := 1\ndef label : Label := ⟨\"def x : String\"⟩\n"⟩] }] },
    { detector := .fixed .warning (.tokens [.lean] .code [.lineStart (.word .identifier "structure"), .exact (.kind .identifier), .gap 40,
        symbol ":", .gap 2, exposed])
      examples := [{ bad := [⟨"Bad.lean", "structure User where\n  id : UserId\n  name : String\n"⟩],
                     good := [⟨"Good.lean", "structure User where\n  id : UserId\n  name : Name\n  deriving Repr\n"⟩] }] }]

def literalBranch : Rule where
  id := ⟨.types, "literal-branch"⟩
  why := "Branching on a Boolean flag or on string literals stands in for a sum type"
  fix := "Introduce a sum type and match on it"
  checks := [
    { detector := .fixed .couldBeBetter (.tokens [.lean] .code [keyword "if", .exact (.kind .identifier), keyword "then"])
      examples := [{ bad := [⟨"Bad.lean", "def f (admin : Bool) : Nat := if admin then 1 else 0\n"⟩],
                     good := [⟨"Good.lean", "def f (n : Count) : Nat := if n == 1 then 1 else 0\ndef s := \"if admin then\"\n"⟩] }] },
    { detector := .fixed .couldBeBetter (.tokens [.lean] .code [symbol "|", .exact (.kind .text), symbol "=>"])
      examples := [{ bad := [⟨"Bad.lean", "def f (s : Role) : Nat :=\n  match s.word with\n  | \"admin\" => 1\n  | _ => 0\n"⟩],
                     good := [⟨"Good.lean", "def f : Role → Nat\n  | .admin => 1\n  | .guest => 0\ndef s := \"| \\\"a\\\" => 1\"\n"⟩] }] }]

def rules : List Rule := [wordAsType, rawText, booleanFlags, stringRecord, untypedTime, unprotected, literalBranch]

end Linter.Catalogue.Types
