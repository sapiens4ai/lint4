set -euo pipefail
mkdir src
printf 'module Main where\n' > src/Main.hs
head -c 25000000 /dev/zero > src/Big.hs
zip -qr "$1" .
