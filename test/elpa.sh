#!/bin/sh
# Fetch the pinned completion packages that the Vertico checks load into
# .cache/elpa, which Git ignores.  Already-fetched packages are kept.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
dest="$root/.cache/elpa"
mkdir -p "$dest"
while read -r name repo version hash; do
    if [ -d "$dest/$name-$version" ]; then continue; fi
    archive="$dest/$name-$version.tar.gz"
    rm -f "$archive"
    curl -fsSL --max-time 120 -o "$archive" \
        "https://github.com/$repo/archive/refs/tags/$version.tar.gz"
    echo "$hash  $archive" | shasum -a 256 -c - >/dev/null
    tar -xzf "$archive" -C "$dest"
    rm -f "$archive"
done <<'EOF'
vertico minad/vertico 2.15 e2ca681da93408caec3549863cb2c34dd573ecbc2d83a539a3c95f500a53a099
orderless oantolin/orderless 1.8 81b2c72b17dde8f54c9a98df0c504e486ef7a7c95bf90a29ab5c29916f860d56
marginalia minad/marginalia 2.13 a06424b7eefc5edd16762ec6db334abf41e1c6b7f978f2f86c7c979121a0d07c
EOF
