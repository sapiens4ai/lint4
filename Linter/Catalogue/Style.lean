import Linter.Domain

namespace Linter.Catalogue.Style

open Linter

def keyword (text : String) : Piece :=
  .exact (.word .identifier text)

def segments (words : List String) : Piece :=
  .anyOf (words.map .segment)

def comment : Rule where
  id := ⟨.style, "comment"⟩
  why := "Comments are banned: names and types carry the meaning"
  fix := "Delete the comment and make the code say it"
  checks := [
    { detector := .fixed .noGo (.tokens [.haskell, .purescript] .anywhere [.exact (.kind .comment)])
      examples := [{ bad := [⟨"Bad.hs", "main = pure () -- done\n"⟩],
                     good := [⟨"Good.hs", "{-# LANGUAGE Strict #-}\nflag = \"--verbose\"\nx --> y = x\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.lean] .anywhere [.exact (.kind .comment)])
      examples := [{ bad := [⟨"Bad.lean", "/-- doc -/\ndef x := 1\n"⟩],
                     good := [⟨"Good.lean", "def flag := \"-- not a comment\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.javascript] .anywhere [.exact (.kind .comment)])
      examples := [{ bad := [⟨"Bad.purs", "module Bad where\n"⟩, ⟨"Bad.js", "export const done = 1; // done\n"⟩,
                             ⟨"Block.purs", "module Block where\n"⟩, ⟨"Block.js", "/* helper */\nexport const helper = 2;\n"⟩],
                     good := [⟨"Good.purs", "module Good where\n"⟩,
                       ⟨"Good.js", "export const url = \"https://example.com\";\nexport const note = `// /* not a comment */`;\n" ++
                       "export const half = 4 / 2;\n"⟩] }] }]

def elseIfChain : Rule where
  id := ⟨.style, "else-if-chain"⟩
  why := "A chain of else-if branches hides the cases that guards or a sum type would name"
  fix := "Use guards, a case expression or a sum type"
  checks := [
    { detector := .fixed .warning (.tokens [.haskell, .purescript] .code
        [keyword "else", keyword "if", .gap 30, keyword "else", keyword "if"])
      examples := [{ bad := [⟨"Bad.hs", "size n = if n < 10 then Small else if n < 100 then Medium else if n < 1000 then Large else Huge\n"⟩,
                             ⟨"Bad.purs", "size n =\n  if n < 10 then Small\n  else if n < 100 then Medium\n  else if n < 1000 then Large\n  else Huge\n"⟩],
                     good := [⟨"Good.hs", "size n = if n < 10 then Small else if n < 100 then Medium else Large\nnote = \"else if a else if b\"\n"⟩] }] }]

def debugTrace : Rule where
  id := ⟨.style, "debug-trace"⟩
  why := "Debug traces are leftovers that print outside the program's own output"
  fix := "Remove the trace"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code [segments ["dbg_trace", "dbgTrace", "dbgTraceIfShared"]])
      examples := [{ bad := [⟨"Bad.lean", "def f (x : Nat) : Nat := dbgTrace \"x\" fun _ => x\n"⟩],
                     good := [⟨"Good.lean", "def f := \"dbg_trace\"\ndef dbgTraced := 1\n"⟩] }] },
    { detector := .fixed .warning (.tokens [.haskell] .anywhere
        [.lineStart (.word .identifier "import"), .gap 1, .exact (.word .identifier "Debug.Trace")])
      examples := [{ bad := [⟨"Bad.hs", "import qualified Debug.Trace as D\nmain = pure ()\n"⟩],
                     good := [⟨"Good.hs", "import Data.Text (Text)\nnote = \"import Debug.Trace\"\n"⟩] }] },
    { detector := .fixed .warning (.tokens [.haskell] .code [segments ["trace", "traceShow", "traceShowId", "traceM", "traceShowM"]])
      examples := [{ bad := [⟨"Bad.hs", "f x = trace \"here\" x\n"⟩, ⟨"Shown.hs", "f x = D.traceShowId x\n"⟩],
                     good := [⟨"Good.hs", "note = \"trace\"\nretrace x = x\n"⟩] }] },
    { detector := .fixed .warning (.tokens [.purescript] .code [segments ["spy", "spyWith"]])
      examples := [{ bad := [⟨"Bad.purs", "f x = spy \"x\" x\n"⟩],
                     good := [⟨"Good.purs", "import Debug (spy)\nnote = \"spy\"\nspyGlass = 1\n"⟩] }] }]

def rules : List Rule := [comment, elseIfChain, debugTrace]

end Linter.Catalogue.Style
