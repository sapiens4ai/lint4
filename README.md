# lint4

Team linter for Haskell, PureScript (with its JavaScript FFI) and Lean 4. It
blocks AI slop and bad practice before it lands: partial functions, swallowed
errors, empty defaults, stringly types, hard-coded endpoints, timers, comments.
Every finding carries `why` and `fix`, so a person or a coding agent can repair
the code without asking. Rules are the same for any code: no config, baselines or
suppressions.

## Use

`LINT4_URL` is the team endpoint; ask the maintainers for it.

| Job | Request |
| --- | --- |
| Check my change | `POST $LINT4_URL/diff`, body = `git diff --unified=100000` |
| Check the whole project | `POST $LINT4_URL/full`, body = a zip of the source |

```sh
git diff --unified=100000 main | curl -sS --compressed --data-binary @- "$LINT4_URL/diff"
git archive --format=zip HEAD > src.zip && curl -sS --compressed --data-binary @src.zip "$LINT4_URL/full"
```

The answer is one JSON report: findings rated `no-go`, `warning` or
`could-be-better`, and an `outcome` of `blocked` (any no-go), `incomplete` (a
file could not be checked) or `passed`. Diff mode reports only findings that
touch added lines, so legacy code stays quiet. Bad input gets HTTP 400 with the
reason.

## How it works

`Intake` unpacks the zip or rebuilds changed files from the diff and keeps
`.hs`, `.purs`, `.lean` and FFI `.js`. A hand-written lexer per language turns
each file into tokens, so a word inside a string or comment never matches code.
`Match` runs the token patterns and metrics of every rule in `Linter/Catalogue/`,
and `Verdict` builds the report. Submitted code is never compiled: it often
cannot be built, and one token engine covers all four languages.

A rule is one value in `Linter/Catalogue/<Category>.lean` with an id, `why`,
`fix`, a pattern and at least one bad and one good example. A rule without
examples does not compile, and `selftest` checks every example through the real
pipeline. Team policy: no comments in code and no timers (deferred work goes
through pulse).

## Develop

Requires [elan](https://github.com/leanprover/elan), `unzip`, `zip` and `jq`.

```sh
lake build
.lake/build/bin/linter check --diff change.diff
.lake/build/bin/linter selftest
tests/run.sh
```

Exit code: 0 passed, 1 blocked, 2 incomplete or rejected input.

## CI and deploy

Other repositories call `uses: sapiens4ai/lint4/.github/workflows/check.yml@main`:
it sends the pull request or push diff and annotates findings in the code.

`service/` holds the AWS Lambda image and SAM template; deploy with
`sam build --template service/template.yaml && sam deploy --guided`.
