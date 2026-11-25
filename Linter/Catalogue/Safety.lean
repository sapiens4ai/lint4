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
    { detector := .fixed .noGo (.tokens [.purescript] .code
        [names .identifier ["unsafeCoerce", "unsafePartial", "unsafePerformEffect", "unsafeCrashWith"]])
      examples := [{ bad := [⟨"Bad.purs", "cast = unsafeCoerce\n"⟩],
                     good := [⟨"Good.purs", "import Unsafe.Coerce (unsafeCoerce)\nname = \"unsafeCoerce\"\n"⟩] }] },
    { detector := .fixed .noGo (.tokens [.lean] .code [names .identifier ["panic!"]])
      examples := [{ bad := [⟨"Bad.lean", "def f : Nat := panic! \"no\"\n"⟩],
                     good := [⟨"Good.lean", "def f : String := \"panic!\"\n"⟩] }] }]

def unfinishedProof : Rule where
  id := ⟨.safety, "sorry"⟩
  why := "An unfinished or unsound proof passes the build without proving anything"
  fix := "Finish the proof"
  checks := [
    { detector := .fixed .noGo (.tokens [.lean] .code [names .identifier ["sorry", "admit", "native_decide"]])
      examples := [{ bad := [⟨"Bad.lean", "theorem t : 1 = 1 := by sorry\n"⟩],
                     good := [⟨"Good.lean", "def reply := \"please admit it\"\n"⟩] }] }]

def rules : List Rule := [timer, escapeHatch, unfinishedProof]

end Linter.Catalogue.Safety
