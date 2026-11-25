#!/bin/bash
set -uo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
bin="$root/.lake/build/bin/linter"
failed=0
for case in "$root"/tests/fixtures/*/; do
  name=$(basename "$case")
  work=$(mktemp -d)
  (cd "$case/files" && zip -qr "$work/input.zip" .)
  "$bin" check --archive "$work/input.zip" > "$work/actual.json"
  if diff <(jq -S . "$case/expected.json") <(jq -S . "$work/actual.json") > "$work/diff"; then
    echo "pass $name"
  else
    echo "FAIL $name"; cat "$work/diff"; failed=1
  fi
  rm -rf "$work"
done
"$bin" selftest > /dev/null || { echo "FAIL selftest"; "$bin" selftest; failed=1; }
exit $failed
