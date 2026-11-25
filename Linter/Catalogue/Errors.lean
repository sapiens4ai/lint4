import Linter.Domain

namespace Linter.Catalogue.Errors

open Linter

def defaultOnError : Rule where
  id := ⟨.errors, "default-on-error"⟩
  why := "Replacing a failure with an empty default hides the error"
  fix := "Propagate the error or decode into a type that cannot be empty"
  checks := [
    { detector := .fixed .warning (.tokens [.lean] .code [.exact (.segment "getD"), .exact (.word .text "\"\"")])
      examples := [{ bad := [⟨"Bad.lean", "def f (x : Except String String) := x.toOption.getD \"\"\n"⟩],
                     good := [⟨"Good.lean", "def f (x : Option Nat) := x.getD 0\n"⟩] }] }]

def rules : List Rule := [defaultOnError]

end Linter.Catalogue.Errors
