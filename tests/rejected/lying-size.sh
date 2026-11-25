set -euo pipefail
python3 - "$1" <<'EOF'
import struct, sys, zipfile
actual = 25000000
with zipfile.ZipFile(sys.argv[1], "w", zipfile.ZIP_DEFLATED) as archive:
    archive.writestr("src/Main.hs", "module Main where\n")
    archive.writestr("src/Big.hs", bytes(actual))
data = bytearray(open(sys.argv[1], "rb").read())
for signature, offset in ((b"PK\x03\x04", 22), (b"PK\x01\x02", 24)):
    start = data.find(signature)
    while start >= 0:
        if struct.unpack_from("<I", data, start + offset)[0] == actual:
            struct.pack_into("<I", data, start + offset, 100)
        start = data.find(signature, start + 4)
open(sys.argv[1], "wb").write(bytes(data))
EOF
