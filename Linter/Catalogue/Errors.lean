import Linter.Catalogue.Safety

namespace Linter.Catalogue.Errors

open Linter
open Linter.Catalogue.Safety (names segments)

def symbols (words : List String) : List Piece :=
  words.map fun word => .exact (.word .symbol word)

def wildcard : Piece := .exact (.word .identifier "_")

def emptyList : List Piece := symbols ["[", "]"]

def emptyText : Piece := .anyOf [.word .text "\"\"", .segment "jsonNull", .word .identifier "mempty"]

def returns : Piece := names .identifier ["pure", "return"]

def lambdaOpen : List Piece := symbols ["\\", "("]

def emptyValue : Rule where
  id := ⟨.errors, "empty-value"⟩
  why := "An empty default turns a missing value into a silent wrong answer"
  fix := "Handle the missing case explicitly or make the value required"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell, .purescript] .code [segments ["fromMaybe", "maybe"], emptyText])
      examples := [{ bad := [⟨"Bad.hs", "label user = fromMaybe \"\" (userName user)\n"⟩, ⟨"Bad.purs", "title = maybe \"\" show\n"⟩],
                     good := [⟨"Good.hs", "label user = fromMaybe \"anonymous\" (userName user)\nhint = \"fromMaybe \\\"\\\"\"\n" ++
                       "field = fromMaybe Null (lookup key object)\nshown = maybe Null toJSON value\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell, .purescript] .code ([segments ["fromMaybe", "maybe"]] ++ emptyList))
      examples := [{ bad := [⟨"Bad.hs", "tags post = Maybe.fromMaybe [] (postTags post)\n"⟩],
                     good := [⟨"Good.hs", "hosts config = fromMaybe [defaultHost] (configHosts config)\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell, .purescript] .code
        ([names .identifier ["Left"], wildcard] ++ symbols ["->"] ++ [emptyText]))
      examples := [{ bad := [⟨"Bad.hs", "render r = case r of\n  Left _ -> \"\"\n  Right v -> v\n"⟩],
                     good := [⟨"Good.hs", "render r = case r of\n  Left err -> show err\n  Right v -> v\n" ++
                       "raw r = case r of\n  Left _ -> Null\n  Right v -> v\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell, .purescript] .code
        ([names .identifier ["Left"], wildcard] ++ symbols ["->"] ++ emptyList))
      examples := [{ bad := [⟨"Bad.purs", "items r = case r of\n  Left _ -> []\n  Right xs -> xs\n"⟩],
                     good := [⟨"Good.purs", "items r = case r of\n  Left _ -> [fallbackItem]\n  Right xs -> xs\n"⟩] }] }]

def jsonModules : List String := ["", "A.", "J.", "Aeson.", "JSON.", "Json.", "Data.Aeson."]

def eitherDecode : Rule where
  id := ⟨.errors, "either-decode"⟩
  why := "decode returns Nothing and throws away the reason the input was rejected"
  fix := "Use eitherDecode and handle the parse error"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell] .code [names .identifier (jsonModules.flatMap fun qualifier =>
        ["decode", "decodeStrict", "decode'", "decodeStrict'"].map (qualifier ++ ·))])
      examples := [{ bad := [⟨"Bad.hs", "parse body = Aeson.decode body :: Maybe Config\n"⟩, ⟨"Short.hs", "parse body = A.decodeStrict body\n"⟩,
                             ⟨"Plain.hs", "parse body = decode body :: Maybe Config\n"⟩],
                     good := [⟨"Good.hs", "import Data.Aeson (decode, eitherDecode)\nparse body = eitherDecode body\nlabel = \"decode\"\n" ++
                       "bytes text = B64.decode (encodeUtf8 text)\nraw text = Base64.decode text\nrows bytes = Flat.decode bytes\n"⟩] }] }]

def wildcardClause : List Piece :=
  [.lineStart (.kind .identifier), .many (.word .identifier "_"), .exact (.word .symbol "=")]

def lenientAccessor : Rule where
  id := ⟨.errors, "lenient-accessor"⟩
  why := "A catch-all clause that returns an empty value hides every case it was not written for"
  fix := "Match every constructor explicitly or return Maybe or Either"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell, .purescript] .code (wildcardClause ++ [.anyOf [.word .text "\"\"", .word .number "0",
        .segment "Null", .segment "jsonNull", .word .identifier "mempty"]]))
      examples := [{ bad := [⟨"Bad.hs", "userEmail (Registered u) = email u\nuserEmail _ = \"\"\n"⟩,
                             ⟨"Bad.purs", "price (Paid p) = p\nprice _ = 0\n"⟩],
                     good := [⟨"Good.hs",
                       "isAdmin Admin = True\nisAdmin _ = False\nuserEmail (Registered u) = Just (email u)\nuserEmail _ = Nothing\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell, .purescript] .code (wildcardClause ++ emptyList))
      examples := [{ bad := [⟨"Bad.hs", "children (Node xs) = xs\nchildren _ = []\n"⟩],
                     good := [⟨"Good.hs", "children (Node xs) = xs\nchildren leaf = [leaf]\n"⟩] }] }]

def swallowedException : Rule where
  id := ⟨.errors, "swallowed-exception"⟩
  why := "Catching every exception and returning a value hides the failure"
  fix := "Catch the specific exception you can handle, or log and rethrow it"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell] .code
        (lambdaOpen ++ [wildcard] ++ symbols ["::"] ++ [segments ["SomeException"]] ++ symbols [")", "->"] ++ [returns]))
      examples := [{ bad := [⟨"Bad.hs", "fetch url = httpGet url `catch`\n  \\(_ :: SomeException) -> pure \"\"\n"⟩],
                     good := [⟨"Good.hs",
                       "fetch url = httpGet url `catch` \\(e :: SomeException) -> logError e >> throwIO e\n" ++
                       "hint = \"\\\\(_ :: SomeException) -> pure\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell] .code
        (lambdaOpen ++ [segments ["SomeException"], wildcard] ++ symbols [")", "->"] ++ [returns]))
      examples := [{ bad := [⟨"Bad.hs", "save x = handle (\\(SomeException _) -> return ()) (write x)\n"⟩],
                     good := [⟨"Good.hs", "save x = handle (\\(SomeException e) -> report e) (write x)\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.javascript] .code ([names .identifier ["catch"], .gap 3] ++ symbols ["{", "}"]))
      examples := [{ bad := [⟨"Bad.purs", "module Bad where\n"⟩, ⟨"Bad.js", "export const load = () => {\n  try { return read(); } catch {}\n};\n"⟩,
                             ⟨"Bound.purs", "module Bound where\n"⟩,
                             ⟨"Bound.js", "export const load = () => {\n  try { read(); } catch (e) {\n  }\n};\n"⟩],
                     good := [⟨"Good.purs", "module Good where\n"⟩,
                       ⟨"Good.js", "export const load = () => {\n  try { return read(); } catch (e) { report(e); throw e; }\n};\n" ++
                       "export const hint = \"catch {}\";\nexport const note = `catch (e) {}`;\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.javascript] .code
        ([names .identifier ["catch"], .gap 3, .exact (.word .symbol "{"), names .identifier ["return"],
          .anyOf [.kind .text, .kind .number, .word .identifier "null", .word .identifier "undefined", .word .identifier "false",
            .word .identifier "true"], .gap 1, .exact (.word .symbol "}")]))
      examples := [{ bad := [⟨"Bad.purs", "module Bad where\n"⟩,
                             ⟨"Bad.js", "export const name = (u) => {\n  try { return u.name; } catch { return ''; }\n};\n"⟩,
                             ⟨"Null.purs", "module Null where\n"⟩,
                             ⟨"Null.js", "export const age = (u) => {\n  try { return u.age; } catch (e) { return null }\n};\n"⟩],
                     good := [⟨"Good.purs", "module Good where\n"⟩,
                       ⟨"Good.js", "export const name = (u) => {\n  try { return u.name; } catch (e) { return fallback(e); }\n};\n" ++
                       "export const hint = \"catch { return '' }\";\nexport const note = `catch (e) { return null }`;\n"⟩] }] }]

def discardedResult : Rule where
  id := ⟨.errors, "discarded-result"⟩
  why := "A discarded result drops the error or value the action produced"
  fix := "Bind the result and handle it"
  checks := [
    { detector := .fixed .warning (.tokens [.purescript] .code [names .identifier ["discard"]])
      examples := [{ bad := [⟨"Bad.purs", "main = discard (launchAff_ job)\n"⟩],
                     good := [⟨"Good.purs", "main = do\n  result <- runJob\n  log (show result)\n  log \"discard\"\n"⟩] }] },
    { detector := .fixed .warning (.tokens [.purescript] .code [wildcard, .exact (.word .symbol "<-")])
      examples := [{ bad := [⟨"Bad.purs", "main = do\n  _ <- saveUser user\n  pure unit\n"⟩],
                     good := [⟨"Good.purs", "main = do\n  saved <- saveUser user\n  for_ saved \\_ -> pure unit\n"⟩] }] },
    { detector := .fixed .warning (.tokens [.lean] .code [wildcard, names .symbol [":=", "←", "<-"]])
      examples := [{ bad := [⟨"Bad.lean", "def run : IO Unit := do\n  let _ ← save\n  pure ()\n"⟩,
                             ⟨"Let.lean", "def run : IO Unit := do\n  let _ := check 1\n  pure ()\n"⟩],
                     good := [⟨"Good.lean",
                       "def run : IO Unit := do\n  let saved ← save\n  match saved with\n  | _ => pure ()\ndef label := \"let _ := x\"\n"⟩] }] },
    { detector := .fixed .warning (.tokens [.lean] .code [segments ["discard"]])
      examples := [{ bad := [⟨"Bad.lean", "def run : IO Unit := discard save\n"⟩],
                     good := [⟨"Good.lean", "def run : IO Unit := do\n  let saved ← save\n  IO.println s!\"{saved} discard\"\n"⟩] }] },
    { detector := .fixed .warning (.tokens [.haskell] .code [names .identifier ["void"], names .symbol ["$", "("], segments ["try"]])
      examples := [{ bad := [⟨"Bad.hs", "main = void $ try (removeFile path)\n"⟩],
                     good := [⟨"Good.hs", "main = do\n  result <- try (removeFile path)\n  either report pure result\n  void (discard path)\n"⟩] }] }]

def literalBranch : List Piece :=
  [.exact (.kind .text)] ++ symbols ["->"] ++ [.many (.except (.kind .identifier) (.word .identifier "_"))]

def lenientDecoder : Rule where
  id := ⟨.errors, "lenient-decoder"⟩
  why := "A catch-all String constructor accepts any unknown input as valid"
  fix := "Keep the sum type closed and return Left for unknown input"
  checks := [
    { detector := .fixed .noGo (.tokens [.purescript] .code
        [names .symbol ["|", "="], names .identifier ["Other"], names .identifier ["String"]])
      examples := [{ bad := [⟨"Bad.purs", "data Status = Active | Paused | Other String\n"⟩],
                     good := [⟨"Good.purs", "data Status = Active | Paused\nlabel = \"| Other String\"\nshowStatus (Other s) = s\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.purescript] .code (literalBranch ++ [wildcard] ++ symbols ["->"] ++ [names .identifier ["Right"]]))
      examples := [{ bad := [⟨"Bad.purs", "decodeTag json = case tag of\n  \"dark\" -> Right Dark\n  _ -> Right Light\n  where tag = toString json\n"⟩],
                     good := [⟨"Good.purs",
                       "decodeTag json = case tag of\n  \"dark\" -> Right Dark\n  other -> Left (\"unknown theme \" <> other)\n" ++
                       "check r = case r of\n  Left \"x\" -> Left \"x\"\n  Right _ -> Right unit\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.purescript] .code
        (literalBranch ++ [.skipBalanced, wildcard] ++ symbols ["->"] ++ [names .identifier ["Right"]]))
      examples := [{ bad := [⟨"Bad.purs", "decodeTag json = case toString json of\n  \"dark\" -> Right (Theme 1)\n  _ -> Right (Theme 0)\n"⟩],
                     good := [⟨"Good.purs", "decodeTag json = case toString json of\n  \"dark\" -> Right (Theme 1)\n  _ -> Left \"unknown\"\n"⟩] }] }]

def defaultOnError : Rule where
  id := ⟨.errors, "default-on-error"⟩
  why := "Replacing a failure with an empty default hides the error"
  fix := "Propagate the error or decode into a type that cannot be empty"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code [.exact (.segment "getD"), .exact (.word .text "\"\"")])
      examples := [{ bad := [⟨"Bad.lean", "def f (x : Except String String) := x.toOption.getD \"\"\n"⟩],
                     good := [⟨"Good.lean", "def f (x : Option Nat) := x.getD 0\n"⟩] }] },
    { detector := .fixed .warning (.tokens [.lean] .code [segments ["getObjValD"]])
      examples := [{ bad := [⟨"Bad.lean", "def name (j : Lean.Json) := j.getObjValD \"name\"\n"⟩],
                     good := [⟨"Good.lean", "def name (j : Lean.Json) := j.getObjValAs? String \"name\"\ndef hint := \"getObjValD\"\n"⟩] }] }]

def handler : List Piece :=
  [names .identifier ["catch"], .exact (.kind .identifier), .exact (.word .symbol "=>"), returns]

def swallowedThrow : Rule where
  id := ⟨.errors, "swallowed-throw"⟩
  why := "A handler that returns unit swallows the failure it caught"
  fix := "Let the throw propagate or handle the error"
  checks := [
    { detector := .fixed .noGo (.tokens [.lean] .code (handler ++ symbols ["(", ")"]))
      examples := [{ bad := [⟨"Bad.lean", "def run : IO Unit := do\n  try save\n  catch _ => pure ()\n"⟩],
                     good := [⟨"Good.lean", "def run : IO Unit := do\n  try save\n  catch e => throw e\ndef hint := \"catch _ => pure ()\"\n"⟩] }] }]

def constantCatch : Rule where
  id := ⟨.errors, "constant-catch"⟩
  why := "A handler that returns a constant drops the error it caught"
  fix := "Handle the caught error or rethrow it"
  checks := [
    { detector := .fixed .couldBeBetter (.tokens [.lean] .code
        (handler ++ [.anyOf [.kind .number, .kind .text, .word .identifier "none", .word .identifier "true", .word .identifier "false",
          .word .identifier "default"]]))
      examples := [{ bad := [⟨"Bad.lean", "def load : IO String := do\n  try readConfig\n  catch _ => pure \"\"\n"⟩],
                     good := [⟨"Good.lean",
                       "def load : IO (Except String String) := do\n  try (.ok <$> readConfig)\n  catch e => pure (.error e.toString)\n"⟩] }] }]

def emptyOnError : Rule where
  id := ⟨.errors, "empty-on-error"⟩
  why := "Turning an error into an empty result makes failure look like absence"
  fix := "Propagate the error"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code
        ([segments ["error"], wildcard, .exact (.word .symbol "=>"), returns, .anyOf [.word .identifier "none", .word .text "\"\""]]))
      examples := [{ bad := [⟨"Bad.lean",
                       "def find (r : Except String Nat) : IO (Option Nat) :=\n  match r with\n  | .ok v => pure (some v)\n  | .error _ => pure none\n"⟩],
                     good := [⟨"Good.lean",
                       "def find (r : Except String Nat) : IO Nat :=\n  match r with\n  | .ok v => pure v\n  | .error e => throw (IO.userError e)\n"⟩] }] }]

def errorTextSearch : Rule where
  id := ⟨.errors, "error-text-search"⟩
  why := "Searching the text of a formatted error breaks when the wording changes"
  fix := "Match on a typed error"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code [.exact (.containing .identifier "toString.contains")])
      examples := [{ bad := [⟨"Bad.lean", "def missing (e : IO.Error) : Bool := e.toString.contains \"not found\"\n"⟩],
                     good := [⟨"Good.lean",
                       "def missing : IO.Error → Bool\n  | .noFileOrDirectory .. => true\n  | _ => false\ndef hint := \"e.toString.contains\"\n"⟩] }] }]

def jsonTextSearch : Rule where
  id := ⟨.checks, "json-text-search"⟩
  why := "Searching serialized JSON text depends on formatting and field order"
  fix := "Decode the JSON and inspect its fields"
  checks := [
    { detector := .fixed .couldBeBetter (.tokens [.lean] .code
        [.anyOf [.containing .identifier "compress.contains", .containing .identifier "pretty.contains"]])
      examples := [{ bad := [⟨"Bad.lean", "def ok (j : Lean.Json) : Bool := j.compress.contains \"ready\"\n"⟩],
                     good := [⟨"Good.lean", "def ok (j : Lean.Json) : Bool := j.getObjValAs? String \"status\" == .ok \"ready\"\n"⟩] }] }]

def continued : Piece := .notBefore (["*>", "<*", ">>", ">>=", "<>", "<$", "$>", "<$>", "<#>", "<|>", "#"].map (.word .symbol ·))

def doesNothing (nothing : List Piece) : Detector :=
  .fixed .couldBeBetter (.tokens [.haskell, .purescript] .code ([wildcard] ++ symbols ["->"] ++ [returns] ++ nothing ++ [continued]))

def noopFallback : Rule where
  id := ⟨.errors, "noop-fallback"⟩
  why := "A catch-all arm that does nothing silently ignores every case it was not written for"
  fix := "Handle each remaining case, or log or return an error for the unexpected ones"
  checks := [
    { detector := doesNothing [names .identifier ["unit"]]
      examples := [{ bad := [⟨"Bad.purs", "onKey key = do\n  case key of\n    Enter -> submit\n    _ -> pure unit\n  refresh\n"⟩,
                             ⟨"Left.purs", "save result = case result of\n  Right doc -> store doc\n  Left _ -> pure unit\n"⟩],
                     good := [⟨"Good.purs", "onKey key = case key of\n  Enter -> submit\n  Escape -> pure unit\n  _ -> pure (Left key)\n" ++
                       "onTab tab = case tab of\n  Home -> home\n  _ -> pure unit *> log tab\nhint = \"_ -> pure unit\"\n"⟩] }] },
    { detector := doesNothing (symbols ["(", ")"])
      examples := [{ bad := [⟨"Bad.hs", "onKey key = case key of\n  Enter -> submit\n  _ -> return ()\nnext = 1\n"⟩],
                     good := [⟨"Good.hs", "onKey key = case key of\n  Enter -> submit\n  Escape -> pure ()\n  _ -> pure (Just key)\n" ++
                       "onTab tab = case tab of\n  Home -> home\n  _ -> return () >> log tab\n"⟩] }] }]

def emptySentinel : Rule where
  id := ⟨.errors, "empty-sentinel"⟩
  why := "Comparing with the empty string treats an empty value as a stand-in for a missing one"
  fix := "Model absence with Maybe, or parse the text into a non-empty type at the boundary"
  checks := [
    { detector := .fixed .couldBeBetter (.tokens [.haskell, .purescript] .code [names .symbol ["==", "/="], .exact (.word .text "\"\"")])
      examples := [{ bad := [⟨"Bad.hs", "missing user = userName user == \"\"\n"⟩, ⟨"Bad.purs", "present s = s /= \"\"\n"⟩],
                     good := [⟨"Good.hs", "missing user = isNothing (userName user)\nhint = \"name == \\\"\\\"\"\n" ++
                       "same a b = a == b\nadmin r = r == \"admin\"\n"⟩] }] },
    { detector := .fixed .couldBeBetter (.tokens [.haskell, .purescript] .code [.exact (.word .text "\"\""), names .symbol ["==", "/="]])
      examples := [{ bad := [⟨"Bad.purs", "missing s = \"\" == s\n"⟩],
                     good := [⟨"Good.purs", "missing s = null s\nempty = \"\"\nhint = \"\\\"\\\" == s\"\n"⟩] }] }]

def rules : List Rule :=
  [emptyValue, eitherDecode, lenientAccessor, swallowedException, discardedResult, lenientDecoder, defaultOnError, swallowedThrow,
    constantCatch, emptyOnError, errorTextSearch, jsonTextSearch, noopFallback, emptySentinel]

end Linter.Catalogue.Errors
