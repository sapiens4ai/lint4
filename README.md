# lint4

Team linter for Haskell, PureScript (with its JavaScript FFI files) and Lean 4.
It blocks AI slop and bad practice before it lands: partial functions, swallowed
errors, empty defaults, stringly types, timers, comments. Every finding says why
it is bad and how to fix it, so a human or a coding agent can repair the code
without asking.

One run returns one complete JSON report. Each finding is **no-go**, **warning**
or **could-be-better**; the report's `outcome` is:

| Outcome | When |
| --- | --- |
| `blocked` | At least one no-go finding |
| `incomplete` | No no-go, but some file could not be checked (listed in `errors`) |
| `passed` | Neither |

Rules are the same for any code: no config, baselines, path exceptions or suppressions.

## API

Public endpoint: https://dev.sapiens4.ai/lint4. Two jobs, two endpoints:

| Job | Request body |
| --- | --- |
| Check a change | `POST /diff`: a diff made with `git diff --unified=100000` |
| Check a codebase | `POST /full`: a zip of the source tree |

```sh
git diff --unified=100000 main | curl -sS --compressed --data-binary @- https://dev.sapiens4.ai/lint4/diff
zip -qr src.zip . && curl -sS --compressed --data-binary @src.zip https://dev.sapiens4.ai/lint4/full
```

Diff mode rebuilds each changed file from the full context and reports a finding
only if its span touches an added line, so legacy code stays quiet. Send
`Accept-Encoding: gzip` (curl `--compressed`) to get a gzipped report.

| Status | Meaning |
| --- | --- |
| 200 | JSON report |
| 400 | Bad input: no full context, not a diff, coloured diff, bad zip, nothing to check, unknown path. Body is `{"error": "..."}` |
| 405 | Not a POST (the public proxy answers 403) |
| 413 | Report over 5.5 MB and the request did not accept gzip |
| 504 | The check timed out |

```json
{
  "outcome": "blocked",
  "mode": "diff",
  "checked": { "haskell": 1, "purescript": 0, "lean": 0, "javascript": 0 },
  "findings": [
    { "rule": "safety/timer", "severity": "no-go", "path": "Main.hs", "line": 5, "column": 9,
      "span": { "first": 5, "last": 5 }, "snippet": "threadDelay",
      "why": "A timer hides a race and stands in for a real event", "fix": "Bind the work to pulse" }
  ],
  "errors": []
}
```

## How it works

```
input → Intake → Lexer → Match ← Catalogue → Verdict → JSON report
```

- **Intake** keeps `.hs`, `.purs`, `.lean`, and `.js` next to a `.purs`; it skips
  symlinks, generated files and vendored directories (`.lake`, `node_modules`, …).
- **Lexer**, one per language: comments, pragmas and strings are tokens, so a word
  in a string never matches code, and patterns span line breaks.
- **Match** runs the catalogue's token patterns and metrics. Codebase rules
  (duplicates, repeated constants, FFI pairing) run in full mode only.
- **Verdict** makes findings, keeps only added lines in diff mode, sets the outcome.

There is no compiler and no external engine: submitted code often cannot be
built, and one token engine covers all four languages, Lean included.

## Rules

41 rules in `Linter/Catalogue/`, grouped by category:

| Category | Examples |
| --- | --- |
| `style` | `comment` (any comment is no-go), `long-line` (warning >160, no-go >250), `dense-line`, `large-declaration`, `duplicate`, `debug-trace` |
| `safety` | `timer` (`threadDelay`, `delay`, `setTimeout`, `IO.sleep`… no-go), `escape-hatch` (`head`, `fromJust`, `unsafeCoerce`, `panic!`), `sorry`, `axiom`, `warnings-off` |
| `errors` | `empty-value` (`fromMaybe ""`), `lenient-accessor` (`text _ = ""`), `swallowed-exception`, `discarded-result`, `default-on-error` |
| `types` | `word-as-type` (`== "literal"`), `raw-text`, `boolean-flags`, `string-record`, `untyped-time`, `unprotected` |
| `boundary` | `ffi` (foreign import without its `.js`), `json`, `hardcoded-endpoint` |
| `security` | `html-string-concat` |
| `checks` | `whole-json`, `json-text-search` |

Team policy, enforced without exceptions: no comments in code (pragmas and
shebangs are fine), and no timers (deferred or periodic work goes through pulse).

### Adding a rule

A rule is one value in a `Linter/Catalogue/<Category>.lean` file, added to that
file's `rules`. It carries an id, `why`, `fix` and checks; each check is a
detector plus at least one bad and one good example:

```lean
def htmlStringConcat : Rule where
  id := ⟨.security, "html-string-concat"⟩
  why := "Building HTML by concatenating strings lets values inject markup"
  fix := "Build the markup with an HTML library that escapes values"
  checks := [
    { detector := .fixed .warning (.tokens [.haskell, .purescript] .code [tagLiteral, .exact (.word .symbol "<>")])
      examples := [{ bad := [⟨"Bad.hs", "cell v = \"<td>\" <> v\n"⟩],
                     good := [⟨"Good.hs", "label = \"<td>\"\n"⟩] }] }]
```

A rule without examples does not compile. `selftest` runs every example through
the real pipeline: the bad one must be flagged, the good one must not. Start new
rules as could-be-better unless sure; promote them once false positives are known.

## Local use

Requires [elan](https://github.com/leanprover/elan) (toolchain from
`lean-toolchain`), `unzip`, and `zip` + `jq` for the tests.

```sh
lake build
.lake/build/bin/linter check --diff change.diff
.lake/build/bin/linter check --archive src.zip
.lake/build/bin/linter selftest
tests/run.sh
```

`--diff -` reads stdin. Exit code: 0 passed, 1 blocked, 2 incomplete or rejected
input (reason on stderr). `tests/run.sh` diffs golden fixtures (`tests/fixtures/`)
and runs the selftest; CI also lints the linter's own source, which must pass.

## CI for other repositories

```yaml
name: lint4
on: [pull_request, push]
jobs:
  lint4:
    uses: sapiens4ai/lint4/.github/workflows/check.yml@main
```

The workflow diffs the pull request base (or the previous push) against `HEAD`,
posts it to `/diff`, annotates findings in the code and writes a summary. It fails
on `blocked`, `incomplete`, or any finding at or above `fail-on` (`no-go` by
default, or `warning`, `could-be-better`). `url` overrides the endpoint.

## Deploy

AWS Lambda container (arm64, no network access) behind a Function URL, defined in
`service/`. A bash bootstrap calls the CLI; the image ships only the binary. With SAM and Docker:

```sh
sam build --template service/template.yaml && sam deploy --stack-name linter-sapiens4 --region eu-north-1 --resolve-s3 --resolve-image-repos --capabilities CAPABILITY_IAM
```

## Layout

```
Main.lean              CLI: check, selftest
Linter/Domain.lean     every type, no logic
Linter/Intake*         zip and diff intake, file discovery
Linter/Lexer/          Haskell/PureScript, Lean and JavaScript lexers
Linter/Match.lean      pattern and metric engine
Linter/Verdict.lean    findings, diff filter, outcome, JSON
Linter/Catalogue/      the rules
service/               Lambda image, bootstrap, SAM template
tests/                 golden fixtures and runner
.github/workflows/     ci.yml (this repo), check.yml (reusable)
```
