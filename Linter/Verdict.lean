import Lean.Data.Json
import Linter.Lexer
import Linter.Match

namespace Linter.Verdict

open Lean Linter

def findingsIn (mode : Mode) (files : List Match.LexedFile) (rule : Rule) : List Finding :=
  (rule.checks.filter (Match.runsIn mode ·.detector)).flatMap fun check =>
    (Match.detectorHits check.detector files).map fun placed =>
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
  else a.column < b.column

def outcome (findings : List Finding) (errors : List FileError) : Outcome :=
  if findings.any (·.severity == .noGo) then .blocked
  else if errors.isEmpty then .passed
  else .incomplete

def touches (added : List AddedLines) (finding : Finding) : Bool :=
  added.any fun entry =>
    entry.path == finding.path && entry.lines.any fun line => finding.span.first ≤ line && line ≤ finding.span.last

def assess (mode : Mode) (rules : List Rule) (files : List CheckedFile) (keep : Finding → Bool) : Report :=
  let results := files.map lexed
  let readable := results.flatMap fun | .ok file => [file] | .error _ => []
  let findings := ((rules.flatMap (findingsIn mode readable)).filter keep).toArray.qsort before |>.toList
  let errors := results.flatMap fun | .ok _ => [] | .error problem => [problem]
  let checked := Language.all.map fun language => (language, (files.filter (·.language == language)).length)
  { mode, outcome := outcome findings errors, checked, findings, errors }

def report (rules : List Rule) : Submission → Report
  | .full files => assess .full rules files fun _ => true
  | .diff files added => assess .diff rules files (touches added)

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
