import Linter.Domain

namespace Linter.Catalogue.Style

open Linter

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
                     good := [⟨"Good.lean", "def flag := \"-- not a comment\"\n"⟩] }] }]

def rules : List Rule := [comment]

end Linter.Catalogue.Style
