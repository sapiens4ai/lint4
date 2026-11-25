import Linter.Domain

namespace Linter.Catalogue.Safety

open Linter

def names (kind : TokenKind) (words : List String) : Piece :=
  .anyOf (words.map (.word kind ·))

def segments (words : List String) : Piece :=
  .anyOf (words.map .segment)

def keywords : List String := ["of", "then", "else", "in", "where", "let", "do", "if", "case", "data", "type", "module", "import"]

def argument : Piece :=
  .anyOf [keywords.foldl (fun shape word => .except shape (.word .identifier word)) (.kind .identifier), .kind .text, .kind .number,
    .word .symbol "$", .word .symbol "(", .word .symbol "["]

def applied : List Piece := [.sameLine, argument]

def timer : Rule where
  id := ⟨.safety, "timer"⟩
  why := "A timer hides a race and stands in for a real event"
  fix := "Bind the work to pulse"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell] .code [segments ["threadDelay"]])
      examples := [{ bad := [⟨"Bad.hs", "main = threadDelay 500\n"⟩],
                     good := [⟨"Good.hs", "import Control.Concurrent (threadDelay)\nmain = putStrLn \"threadDelay\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell] .code [segments ["timeout"], .anyOf [.kind .number, .word .symbol "("]])
      examples := [{ bad := [⟨"Bad.hs", "main = timeout 5000000 (fetch url)\n"⟩, ⟨"Scaled.hs", "wait n = System.Timeout.timeout (n * 1000000) loop\n"⟩],
                     good := [⟨"Good.hs",
                       "import System.Timeout (timeout)\ndata Config = Config { timeout :: Int }\nlimit c = timeout c\nhint = \"timeout 5\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.purescript] .code
        ([segments ["delay", "later", "later'", "setTimeout", "setInterval", "setTimer"]] ++ applied))
      examples := [{ bad := [⟨"Bad.purs", "main = launchAff_ (delay (Milliseconds 5.0))\n"⟩, ⟨"Later.purs", "retry = E.later 300.0 $ dispatch Poll\n"⟩,
                             ⟨"Timer.purs", "main = void $ N.setTimer 1000 tick\n"⟩],
                     good := [⟨"Good.purs",
                       "main = log \"Try again later.\"\nlater :: Number -> Aff Unit -> Aff Unit\npause = let wait = delay in wait\n" ++
                       "foreign import setTimer :: Int -> Effect Unit -> Effect Int\nwait = Aff.delay\nretry = 1\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.javascript] .code
        [names .identifier ["setTimeout", "setInterval", "setImmediate", "requestAnimationFrame"], .exact (.word .symbol "(")])
      examples := [{ bad := [⟨"Bad.purs", "module Bad where\n"⟩, ⟨"Bad.js", "export const later = (f) => () => setTimeout(f, 100);\n"⟩,
                             ⟨"Frame.purs", "module Frame where\n"⟩,
                             ⟨"Frame.js", "export const paint = (f) => () => window.requestAnimationFrame(f);\n"⟩],
                     good := [⟨"Good.purs", "module Good where\n"⟩,
                       ⟨"Good.js", "export const hint = \"setTimeout(f, 100)\";\nexport const note = `setInterval(tick)`;\n" ++
                       "export const setTimeoutMs = 100;\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.lean] .code [names .identifier ["IO.sleep"]])
      examples := [{ bad := [⟨"Bad.lean", "def wait : IO Unit := IO.sleep 100\n"⟩],
                     good := [⟨"Good.lean", "def wait : String := \"IO.sleep\"\n"⟩] }] }]

def partialName (name : String) : TokenShape :=
  .except (.except (.segment name) (.containing .identifier "NE.")) (.containing .identifier "NonEmpty.")

def used : Piece :=
  .notBefore [.word .symbol "::", .word .symbol "="]

def escapeHatch : Rule where
  id := ⟨.safety, "escape-hatch"⟩
  why := "Partial functions and unsafe casts bypass the type system"
  fix := "Handle every case and decode values with real types"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell] .code [.anyOf
        ((["head", "tail", "init", "last", "fromJust"].map partialName) ++
          (["undefined", "unsafePerformIO", "unsafeCoerce"].map (.word .identifier ·)) ++ [.word .symbol "!!"]), used])
      examples := [{ bad := [⟨"Bad.hs", "first xs = head xs\n"⟩, ⟨"Text.hs", "initial t = isUpper (T.head t)\n"⟩,
                             ⟨"Maybe.hs", "value = Maybe.fromJust . lookup key\n"⟩, ⟨"Free.hs", "firstOf :: [Int] -> Int\nfirstOf = head"⟩],
                     good := [⟨"Good.hs",
                       "import Data.Text (head)\nimport qualified Data.List.NonEmpty as NE\nfirst xs = NE.head xs\nlast' = NonEmpty.last\n" ++
                       "head :: Tree a -> a\nlabel = \"init\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell] .code [names .identifier ["read"], .anyOf [.word .symbol "<$>", .word .symbol "."]])
      examples := [{ bad := [⟨"Bad.hs", "number = read <$> munch1 isDigit\n"⟩],
                     good := [⟨"Good.hs", "number = readMaybe <$> munch1 isDigit\nhint = \"read <$>\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell] .code ([names .identifier ["read"]] ++ applied))
      examples := [{ bad := [⟨"Bad.hs", "port s = read s :: Int\n"⟩],
                     good := [⟨"Good.hs", "import Text.Read (readMaybe)\nport s = readMaybe s\nmode = Mode { read = True }\nhint = \"read error\"\n" ++
                       "parse = read\nport = 1\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell] .code
        ([.anyOf [.kind .symbol, .word .identifier "then", .word .identifier "else", .word .identifier "in", .word .identifier "do",
          .word .identifier "of"], names .identifier ["error"]] ++ applied))
      examples := [{ bad := [⟨"Bad.hs", "boot = error $ \"missing \" <> name\n"⟩, ⟨"Case.hs", "pick x = if x then 1 else error \"no\"\n"⟩],
                     good := [⟨"Good.hs",
                       "mode = Mode { error = False }\nrun = action `catch` \\(error :: SomeException) -> case fromException error of\n" ++
                       "  Just e -> report e\n  Nothing -> report (displayException error)\nhint = \"error\"\nfailure = error\nport = 1\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.purescript] .code
        [names .identifier ["unsafeCoerce", "unsafePartial", "unsafePerformEffect", "unsafeCrashWith"]])
      examples := [{ bad := [⟨"Bad.purs", "cast = unsafeCoerce\n"⟩],
                     good := [⟨"Good.purs", "import Unsafe.Coerce (unsafeCoerce)\nname = \"unsafeCoerce\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.lean] .code [.anyOf ([.word .identifier "panic!", .word .identifier "unsafe"] ++
        (["head!", "tail!", "get!", "getLast!", "back!", "set!", "setObjVal!", "find!", "toNat!", "toInt!", "insertAt!",
          "eraseIdx!", "swap!"].map .segment))])
      examples := [{ bad := [⟨"Bad.lean", "def f : Nat := panic! \"no\"\n"⟩, ⟨"Head.lean", "def first (xs : List Nat) : Nat := xs.head!\n"⟩,
                             ⟨"Json.lean", "def tag (j : Lean.Json) := j.setObjVal! \"kind\" 1\n"⟩,
                             ⟨"Unsafe.lean", "unsafe def cast (n : Nat) : String := unsafeCast n\n"⟩],
                     good := [⟨"Good.lean",
                       "def f : String := \"panic!\"\ndef first (xs : List Nat) : Option Nat := xs.head?\ndef say (n : Nat) := s!\"got {n}\"\n" ++
                       "def unsafeName : String := \"unsafe\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.lean] .code [.exact (.word .symbol "]"), names .symbol ["!", "!."]])
      examples := [{ bad := [⟨"Bad.lean", "def second (xs : Array Nat) : Nat := xs[1]!\n"⟩],
                     good := [⟨"Good.lean",
                       "def second (xs : Array Nat) : Option Nat := xs[1]?\ndef differs (xs : Array Nat) : Bool := xs[0]? != some 1\n"⟩] }] }]

def silencingFlags : List String :=
  let disabled := "abcdfghijklmnopqrstuvwxyz".toList.map fun letter => s!"-Wno-{letter}"
  let disabledExceptError := "abcdefghijklmnopqstuvwxyz".toList.map fun letter => s!"-Wno-e{letter}"
  ["-fno-warn", " -w ", "\t-w ", " -w\t", "\t-w\t", " -w\n", "\t-w\n"] ++ disabled ++ disabledExceptError

def warningsOff : Rule where
  id := ⟨.safety, "warnings-off"⟩
  why := "A disabled warning hides the defect it was written to catch"
  fix := "Remove the flag that disables the warning and fix the code it reports"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell] .hidden [.anyOf (silencingFlags.map (.containing .pragma ·))])
      examples := [{ bad := [⟨"Bad.hs", "{-# OPTIONS_GHC -Wno-unused-imports #-}\nmodule Bad where\n"⟩,
                             ⟨"Quiet.hs", "{-# OPTIONS_GHC -w #-}\nmodule Quiet where\n"⟩,
                             ⟨"Tab.hs", "{-# OPTIONS_GHC\t-w #-}\nmodule Tab where\n"⟩,
                             ⟨"Old.hs", "{-# OPTIONS_GHC -fno-warn-orphans #-}\nmodule Old where\n"⟩],
                     good := [⟨"Good.hs",
                       "{-# OPTIONS_GHC -Wall -Wno-error=deprecations #-}\n{-# LANGUAGE OverloadedStrings #-}\nmodule Good where\n" ++
                       "flag = \"-Wno-unused\"\n"⟩] }] }]

def mutableVar : Rule where
  id := ⟨.safety, "mutable-var"⟩
  why := "A shared mutable variable makes state changes invisible and racy"
  fix := "Pass state explicitly or synchronise it with an MVar or TVar"
  checks := [
    { detector := .fixed .warning (.tokens [.haskell] .code [segments ["newIORef"]])
      examples := [{ bad := [⟨"Bad.hs", "main = do\n  counter <- IORef.newIORef 0\n  pure ()\n"⟩],
                     good := [⟨"Good.hs", "import Data.IORef (newIORef)\nmain = do\n  counter <- newMVar 0\n  putStrLn \"newIORef\"\n"⟩] }] }]

def unfinishedProof : Rule where
  id := ⟨.safety, "sorry"⟩
  why := "An unfinished or unsound proof passes the build without proving anything"
  fix := "Finish the proof"
  checks := [
    { detector := .fixed .noGo (.tokens [.lean] .code [names .identifier ["sorry", "admit"]])
      examples := [{ bad := [⟨"Bad.lean", "theorem t : 1 = 1 := by sorry\n"⟩],
                     good := [⟨"Good.lean", "def reply := \"please admit it\"\n"⟩] }] }]

def extraAxiom : Rule where
  id := ⟨.safety, "axiom"⟩
  why := "A user axiom can make any statement provable; native_decide is a different risk: " ++
    "it extends the trusted base from the kernel to the compiler and its generated code"
  fix := "Remove the axiom and prove the fact or model it as a hypothesis"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code [names .identifier ["axiom"]])
      examples := [{ bad := [⟨"Bad.lean", "axiom trustMe : 1 = 2\n"⟩],
                     good := [⟨"Good.lean", "theorem one : 1 = 1 := rfl\n#print axioms one\ndef label := \"axiom\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.lean] .code [names .identifier ["native_decide"]])
      examples := [{ bad := [⟨"Bad.lean", "theorem big : 2 ^ 64 > 0 := by native_decide\n"⟩],
                     good := [⟨"Good.lean", "theorem big : 2 ^ 64 > 0 := by decide\ndef note := \"native_decide\"\n"⟩] }] }]

def partialDef : Rule where
  id := ⟨.safety, "partial-def"⟩
  why := "A partial definition may loop forever and cannot be reasoned about"
  fix := "Use structural recursion or prove termination"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code [names .identifier ["partial"], names .identifier ["def"]])
      examples := [{ bad := [⟨"Bad.lean", "private partial def loop (n : Nat) : Nat := loop n\n"⟩],
                     good := [⟨"Good.lean", "def partialSum (xs : List Nat) : Nat := xs.foldl (· + ·) 0\ndef note := \"partial def\"\n"⟩] }] }]

def externalCode : Rule where
  id := ⟨.safety, "extern"⟩
  why := "An external implementation is not checked against its Lean definition"
  fix := "Remove extern and implemented_by and implement the definition in Lean"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code
        [.exact (.word .symbol "@"), .exact (.word .symbol "["), .gap 6, names .identifier ["extern", "implemented_by"]])
      examples := [{ bad := [⟨"Bad.lean", "@[extern \"lean_fast_add\"]\nopaque fastAdd : Nat → Nat → Nat\n"⟩,
                             ⟨"Impl.lean", "@[inline, implemented_by fastImpl]\ndef slow (n : Nat) : Nat := n\n"⟩],
                     good := [⟨"Good.lean", "@[inline] def externalId : String := \"extern\"\n@[simp] theorem t : externalId = externalId := rfl\n"⟩] }] }]

def rules : List Rule := [timer, escapeHatch, unfinishedProof, warningsOff, mutableVar, extraAxiom, partialDef, externalCode]

end Linter.Catalogue.Safety
