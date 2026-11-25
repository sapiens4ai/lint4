#!/bin/bash
set -uo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
bin="$root/.lake/build/bin/linter"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
failed=0

lint() {
  rm -rf "$work/build" "$work/input.zip"
  case "$1" in
    *.diff) "$bin" check --diff "$1" ;;
    *.zip) "$bin" check --archive "$1" ;;
    *.sh) mkdir "$work/build" && (cd "$work/build" && bash "$1" "$work/input.zip") && "$bin" check --archive "$work/input.zip" ;;
    *) (cd "$1" && zip -qr "$work/input.zip" .) && "$bin" check --archive "$work/input.zip" ;;
  esac > "$work/stdout" 2> "$work/stderr"
}

for case in "$root"/tests/fixtures/*/; do
  name=$(basename "$case")
  if [ -f "$case/input.diff" ]; then lint "$case/input.diff"; else lint "$case/files"; fi
  code=$?
  want=$(jq -r '{passed: 0, blocked: 1, incomplete: 2}[.outcome]' "$case/expected.json")
  if [ "$code" = "$want" ] && diff <(jq -S . "$case/expected.json") <(jq -S . "$work/stdout") > "$work/diff"; then
    echo "pass $name"
  else
    echo "FAIL $name: exit $code, expected $want"; cat "$work/diff" "$work/stderr"; failed=1
  fi
done

while IFS=$'\t' read -r input message; do
  lint "$root/tests/rejected/$input"
  code=$?
  actual=$(cat "$work/stderr")
  if [ "$code" = 2 ] && [ ! -s "$work/stdout" ] && [[ "$actual" == $message ]]; then
    echo "pass rejected/$input"
  else
    echo "FAIL rejected/$input: exit $code, stdout $(wc -c < "$work/stdout") bytes, stderr $(head -c 300 "$work/stderr")"; failed=1
  fi
done < "$root/tests/rejected/expected.tsv"

"$bin" selftest > /dev/null || { echo "FAIL selftest"; "$bin" selftest; failed=1; }
exit $failed
