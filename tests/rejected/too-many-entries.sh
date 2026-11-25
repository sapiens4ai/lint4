set -euo pipefail
printf 'module Main where\n' > Main.hs
seq 1 20001 | sed "s/^/f/" | xargs touch
zip -qr -0 "$1" .
