import Linter.Domain

namespace Linter.Catalogue.Safety

open Linter

def names (kind : TokenKind) (words : List String) : Piece :=
  .anyOf (words.map (.word kind ·))

def segments (words : List String) : Piece :=
  .anyOf (words.map .segment)

def timer : Rule where
  id := ⟨.safety, "timer"⟩
  why := "A timer hides a race and stands in for a real event"
  fix := "Bind the work to pulse"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell] .code [segments ["threadDelay"]])
      examples := [{ bad := [⟨"Bad.hs", "main = threadDelay 500\n"⟩],
                     good := [⟨"Good.hs", "import Control.Concurrent (threadDelay)\nmain = putStrLn \"threadDelay\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.purescript] .code
        [segments ["delay", "later", "later'", "setTimeout", "setInterval"]])
      examples := [{ bad := [⟨"Bad.purs", "main = launchAff_ (delay (Milliseconds 5.0))\n"⟩],
                     good := [⟨"Good.purs", "main = log \"Try again later.\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.lean] .code [names .identifier ["IO.sleep"]])
      examples := [{ bad := [⟨"Bad.lean", "def wait : IO Unit := IO.sleep 100\n"⟩],
                     good := [⟨"Good.lean", "def wait : String := \"IO.sleep\"\n"⟩] }] }]

def applied : Piece :=
  .anyOf [.kind .identifier, .kind .text, .kind .number, .word .symbol "$", .word .symbol "(", .word .symbol "["]

def escapeHatch : Rule where
  id := ⟨.safety, "escape-hatch"⟩
  why := "Partial functions and unsafe casts bypass the type system"
  fix := "Handle every case and decode values with real types"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell] .code [.anyOf
        ((["head", "tail", "init", "last", "fromJust", "undefined", "unsafePerformIO", "unsafeCoerce"].map
          (.word .identifier ·)) ++ [.word .symbol "!!"])])
      examples := [{ bad := [⟨"Bad.hs", "first xs = head xs\n"⟩],
                     good := [⟨"Good.hs", "import Data.Text (head)\nfirst t = T.head t\nlabel = \"init\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.haskell] .code [names .identifier ["read", "error"], applied])
      examples := [{ bad := [⟨"Bad.hs", "port s = read s :: Int\n"⟩, ⟨"Fail.hs", "boot = error $ \"missing \" <> name\n"⟩],
                     good := [⟨"Good.hs",
                       "import Text.Read (readMaybe)\nport s = readMaybe s\nmode = Mode { read = True, error = False }\nhint = \"read error\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.purescript] .code
        [names .identifier ["unsafeCoerce", "unsafePartial", "unsafePerformEffect", "unsafeCrashWith"]])
      examples := [{ bad := [⟨"Bad.purs", "cast = unsafeCoerce\n"⟩],
                     good := [⟨"Good.purs", "import Unsafe.Coerce (unsafeCoerce)\nname = \"unsafeCoerce\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.lean] .code [.anyOf ([.word .identifier "panic!"] ++
        (["head!", "tail!", "get!", "getLast!", "back!", "set!", "setObjVal!", "find!", "toNat!", "toInt!", "insertAt!",
          "eraseIdx!", "swap!"].map .segment))])
      examples := [{ bad := [⟨"Bad.lean", "def f : Nat := panic! \"no\"\n"⟩, ⟨"Head.lean", "def first (xs : List Nat) : Nat := xs.head!\n"⟩,
                             ⟨"Json.lean", "def tag (j : Lean.Json) := j.setObjVal! \"kind\" 1\n"⟩],
                     good := [⟨"Good.lean",
                       "def f : String := \"panic!\"\ndef first (xs : List Nat) : Option Nat := xs.head?\ndef say (n : Nat) := s!\"got {n}\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.lean] .code [.exact (.word .symbol "]"), names .symbol ["!", "!."]])
      examples := [{ bad := [⟨"Bad.lean", "def second (xs : Array Nat) : Nat := xs[1]!\n"⟩],
                     good := [⟨"Good.lean",
                       "def second (xs : Array Nat) : Option Nat := xs[1]?\ndef differs (xs : Array Nat) : Bool := xs[0]? != some 1\n"⟩] }] }]

def silencingFlags : List String :=
  let disabled := "abcdfghijklmnopqrstuvwxyz".toList.map fun letter => s!"-Wno-{letter}"
  let disabledExceptError := "abcdefghijklmnopqstuvwxyz".toList.map fun letter => s!"-Wno-e{letter}"
  ["-fno-warn", " -w "] ++ disabled ++ disabledExceptError

def warningsOff : Rule where
  id := ⟨.safety, "warnings-off"⟩
  why := "A disabled warning hides the defect it was written to catch"
  fix := "Remove the flag that disables the warning and fix the code it reports"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell] .anywhere [.anyOf (silencingFlags.map (.containing .pragma ·))])
      examples := [{ bad := [⟨"Bad.hs", "{-# OPTIONS_GHC -Wno-unused-imports #-}\nmodule Bad where\n"⟩,
                             ⟨"Quiet.hs", "{-# OPTIONS_GHC -w #-}\nmodule Quiet where\n"⟩,
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
    { detector := .fixed .noGo (.tokens [.lean] .code [names .identifier ["sorry", "admit", "native_decide"]])
      examples := [{ bad := [⟨"Bad.lean", "theorem t : 1 = 1 := by sorry\n"⟩],
                     good := [⟨"Good.lean", "def reply := \"please admit it\"\n"⟩] }] }]

def extraAxiom : Rule where
  id := ⟨.safety, "axiom"⟩
  why := "An extra axiom can make any statement provable"
  fix := "Remove the axiom and prove the fact or model it as a hypothesis"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code [names .identifier ["axiom"]])
      examples := [{ bad := [⟨"Bad.lean", "axiom trustMe : 1 = 2\n"⟩],
                     good := [⟨"Good.lean", "theorem one : 1 = 1 := rfl\n#print axioms one\ndef label := \"axiom\"\n"⟩] }] }]

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
