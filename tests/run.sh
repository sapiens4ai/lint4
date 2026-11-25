#!/bin/bash
set -uo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
bin="$root/.lake/build/bin/linter"
failed=0
for case in "$root"/tests/fixtures/*/; do
  name=$(basename "$case")
  work=$(mktemp -d)
  if [ -f "$case/input.diff" ]; then
    "$bin" check --diff "$case/input.diff" > "$work/actual.json" 2> "$work/stderr"
  elif [ -f "$case/input.zip" ]; then
    "$bin" check --archive "$case/input.zip" > "$work/actual.json" 2> "$work/stderr"
  else
    (cd "$case/files" && zip -qr "$work/input.zip" .)
    "$bin" check --archive "$work/input.zip" > "$work/actual.json" 2> "$work/stderr"
  fi
  code=$?
  if [ -f "$case/expected.exit" ]; then
    if [ "$code" = "$(cat "$case/expected.exit")" ] && [ ! -s "$work/actual.json" ] && [ -s "$work/stderr" ]; then
      echo "pass $name"
    else
      echo "FAIL $name: exit $code, stdout $(wc -c < "$work/actual.json") bytes"; failed=1
    fi
  elif diff <(jq -S . "$case/expected.json") <(jq -S . "$work/actual.json") > "$work/diff"; then
    echo "pass $name"
  else
    echo "FAIL $name"; cat "$work/diff" "$work/stderr"; failed=1
  fi
  rm -rf "$work"
done
"$bin" selftest > /dev/null || { echo "FAIL selftest"; "$bin" selftest; failed=1; }
exit $failed
