set -euo pipefail
printf 'module Main where\n' > Main.hs
head -c 19000000 /dev/zero > a.hs
for copy in b c d e f g h i j k; do ln a.hs "$copy.hs"; done
zip -qr "$1" .
