import Lean.Data.Json
import Linter.Lexer
import Linter.Match

namespace Linter.Verdict

open Lean Linter

def findingsIn (mode : Mode) (codebase : Codebase) (files : List Match.LexedFile) (rule : Rule) : List Finding :=
  (rule.checks.filter (Match.runsIn mode ·.detector)).flatMap fun check =>
    (Match.detectorHits codebase check.detector files).map fun placed =>
      { rule := rule.id, severity := placed.severity, path := placed.path, line := placed.hit.start.line,
        column := placed.hit.start.column, span := placed.hit.span, snippet := placed.hit.snippet, why := rule.why, fix := rule.fix }

def lexed (file : CheckedFile) : Except FileError Match.LexedFile :=
  match lexFile file.language file.source.code with
  | .ok tokens => .ok ⟨file, tokens.toList⟩
  | .error problem =>
    .error ⟨file.source.path, s!"{problem.position.line}:{problem.position.column}: {problem.message}"⟩

def before (a b : Finding) : Bool :=
  if a.path != b.path then a.path < b.path
  else if a.line != b.line then a.line < b.line
  else if a.column != b.column then a.column < b.column
  else a.rule.word < b.rule.word

def outcome (findings : List Finding) (errors : List FileError) : Outcome :=
  if findings.any (·.severity == .noGo) then .blocked
  else if errors.isEmpty then .passed
  else .incomplete

def touches (added : List AddedLines) (finding : Finding) : Bool :=
  added.any fun entry =>
    entry.path == finding.path && entry.lines.any fun line => finding.span.first ≤ line && line ≤ finding.span.last

def checkedFile : Discovered → List CheckedFile
  | .checked file => [file]
  | .unreadable _ _ | .orphan _ => []

def unreadable : Discovered → List FileError
  | .unreadable _ problem => [problem]
  | .checked _ | .orphan _ => []

def counted : Discovered → List Language
  | .checked file => [file.language]
  | .unreadable language _ => [language]
  | .orphan _ => []

def assess (mode : Mode) (rules : List Rule) (codebase : Codebase) (keep : Finding → Bool) : Report :=
  let results := (codebase.found.flatMap checkedFile).map lexed
  let readable := results.flatMap fun | .ok file => [file] | .error _ => []
  let findings := ((rules.flatMap (findingsIn mode codebase readable)).filter keep).toArray.qsort before |>.toList
  let failed := codebase.found.flatMap unreadable ++ results.flatMap fun | .ok _ => [] | .error problem => [problem]
  let errors := (failed.toArray.qsort fun a b => a.path < b.path).toList
  let languages := codebase.found.flatMap counted
  let checked := Language.all.map fun language => (language, (languages.filter (· == language)).length)
  { mode, outcome := outcome findings errors, checked, findings, errors }

def report (rules : List Rule) : Submission → Report
  | .full codebase => assess .full rules codebase fun _ => true
  | .diff files added => assess .diff rules ⟨files.map .checked, []⟩ (touches added)

def findingJson (finding : Finding) : Json :=
  Json.mkObj [
    ("rule", toJson finding.rule.word), ("severity", toJson finding.severity.word),
    ("path", toJson finding.path), ("line", toJson finding.line), ("column", toJson finding.column),
    ("span", Json.mkObj [("first", toJson finding.span.first), ("last", toJson finding.span.last)]),
    ("snippet", toJson finding.snippet), ("why", toJson finding.why), ("fix", toJson finding.fix)]

def toJson (report : Report) : Json :=
  Json.mkObj [
    ("mode", Lean.toJson report.mode.word), ("outcome", Lean.toJson report.outcome.word),
    ("checked", Json.mkObj (report.checked.map fun (language, count) => (language.word, Lean.toJson count))),
    ("findings", Json.arr (report.findings.map findingJson).toArray),
    ("errors", Json.arr (report.errors.map fun problem =>
      Json.mkObj [("path", Lean.toJson problem.path), ("message", Lean.toJson problem.message)]).toArray)]

end Linter.Verdict
