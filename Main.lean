import Linter.Catalogue
import Linter.Verdict

open Linter

def usage : String := "usage: linter check --archive FILE.zip | linter selftest"

def checkedFiles (files : List SourceFile) : List CheckedFile :=
  files.filterMap fun file =>
    (System.FilePath.mk file.path).extension.bind Language.ofExtension |>.map (⟨·, file⟩)

def flagged (rule : Rule) (files : List SourceFile) : Nat :=
  ((Verdict.report [rule] (.full (checkedFiles files))).findings.filter (·.rule == rule.id)).length

def failures (rule : Rule) : List String :=
  rule.checks.flatMap fun check => check.examples.flatMap fun sample =>
    (if flagged rule sample.bad == 0 then [s!"{rule.id.word}: bad example not flagged"] else []) ++
    (if flagged rule sample.good != 0 then [s!"{rule.id.word}: good example flagged"] else [])

def selftest : IO UInt32 := do
  let problems := Catalogue.rules.flatMap failures
  problems.forM IO.println
  IO.println s!"{Catalogue.rules.length} rules, {problems.length} failures"
  pure (if problems.isEmpty then 0 else 1)

def check (archive : String) : IO UInt32 := do
  match ← Intake.read Catalogue.discovery (.archive archive) with
  | .error message =>
    IO.eprintln message
    pure Outcome.incomplete.exitCode
  | .ok submission =>
    let report := Verdict.report Catalogue.rules submission
    IO.println (Verdict.toJson report).compress
    pure report.outcome.exitCode

def main : List String → IO UInt32
  | ["check", "--archive", archive] => check archive
  | ["selftest"] => selftest
  | _ => do
    IO.eprintln usage
    pure 64
