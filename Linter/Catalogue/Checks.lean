import Linter.Domain

namespace Linter.Catalogue.Checks

open Linter

def equals : Piece := .exact (.word .symbol "==")

def wholeJson : Rule where
  id := ⟨.checks, "whole-json"⟩
  why := "Comparing a whole JSON object makes the check depend on fields it is not about"
  fix := "Compare the fields the check is about"
  checks := [
    { detector := .fixed .couldBeBetter (.tokens [.lean] .code [equals, .exact (.segment "mkObj")])
      examples := [{ bad := [⟨"Bad.lean", "def ok (j : Json) : Bool := j == Json.mkObj [(\"a\", 1)]\n"⟩],
                     good := [⟨"Good.lean", "def body : Json := Json.mkObj [(\"a\", 1)]\ndef s := \"j == Json.mkObj []\"\n"⟩] }] },
    { detector := .fixed .couldBeBetter (.tokens [.lean] .code [equals, .exact (.word .identifier "json"), .exact (.word .symbol "%")])
      examples := [{ bad := [⟨"Bad.lean", "def ok (j : Json) : Bool := j == json% {a: 1}\n"⟩],
                     good := [⟨"Good.lean", "def body : Json := json% {a: 1}\ndef same (a b : Nat) : Bool := a == b\n"⟩] }] },
    { detector := .fixed .couldBeBetter (.tokens [.lean] .code [.exact (.segment "mkObj"), .skipBalanced, equals])
      examples := [{ bad := [⟨"Bad.lean", "def ok (j : Json) : Bool := Json.mkObj [(\"a\", 1)] == j\n"⟩],
                     good := [⟨"Good.lean", "def ok (j : Json) : Bool := (j.getObjVal? \"a\").toOption == some 1\n"⟩] }] }]

def rules : List Rule := [wholeJson]

end Linter.Catalogue.Checks
