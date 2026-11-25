import Linter.Domain

namespace Linter.Catalogue.Security

open Linter

def tagLiteral : Piece :=
  .anyOf ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ/".toList.map fun letter => .containing .text s!"<{letter}")

def htmlStringConcat : Rule where
  id := ⟨.security, "html-string-concat"⟩
  why := "Building HTML by concatenating strings lets values inject markup"
  fix := "Build the markup with an HTML library that escapes values"
  checks := [
    { detector := .fixed .warning (.tokens [.haskell, .purescript] .code [tagLiteral, .exact (.word .symbol "<>")])
      examples := [{ bad := [⟨"Bad.hs", "cell v = \"<td>\" <> v\n"⟩, ⟨"Bad.purs", "link u = \"<a href='\" <> u <> \"'>\"\n"⟩],
                     good := [⟨"Good.hs", "label = \"<td>\"\nless a = \"a < b\" <> a\narrow = \"<- here\" <> a\n"⟩] }] }]

def rules : List Rule := [htmlStringConcat]

end Linter.Catalogue.Security
