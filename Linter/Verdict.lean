import Lean.Data.Json
import Linter.Lexer
import Linter.Match

namespace Linter.Verdict

open Lean Linter

def findingsIn (rule : Rule) (file : CheckedFile) (tokens : List Token) : List Finding :=
  rule.checks.flatMap fun check =>
    (Match.fileHits check.detector file.language tokens).map fun (severity, hit) =>
      { rule := rule.id, severity, path := file.source.path, line := hit.start.line,
        column := hit.start.column, span := hit.span, snippet := hit.snippet, why := rule.why, fix := rule.fix }

def examine (rules : List Rule) (file : CheckedFile) : Except FileError (List Finding) :=
  match lexFile file.language file.source.code with
  | .ok tokens => .ok (rules.flatMap (findingsIn · file tokens.toList))
  | .error problem =>
    .error ⟨file.source.path, s!"{problem.position.line}:{problem.position.column}: {problem.message}"⟩

def before (a b : Finding) : Bool :=
  a.path < b.path || (a.path == b.path && (a.line < b.line || (a.line == b.line && a.column < b.column)))

def outcome (findings : List Finding) (errors : List FileError) : Outcome :=
  if findings.any (·.severity == .noGo) then .blocked
  else if errors.isEmpty then .passed
  else .incomplete

def report (rules : List Rule) : Submission → Report
  | .full files =>
    let results := files.map (examine rules)
    let findings := (results.flatMap fun | .ok found => found | .error _ => []).toArray.qsort before |>.toList
    let errors := results.flatMap fun | .ok _ => [] | .error problem => [problem]
    let checked := Language.all.map fun language => (language, (files.filter (·.language == language)).length)
    { mode := .full, outcome := outcome findings errors, checked, findings, errors }

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
