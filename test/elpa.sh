#!/bin/sh
# Fetch the pinned packages that the checks load into .cache/elpa, which
# Git ignores: the completion packages of the Vertico checks, and
# osx-dictionary.el, which has no release tag, at a commit.  Already-fetched
# packages are kept.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
dest="$root/.cache/elpa"
mkdir -p "$dest"
while read -r name repo version hash; do
    if [ -d "$dest/$name-$version" ]; then continue; fi
    archive="$dest/$name-$version.tar.gz"
    rm -f "$archive"
    case $version in
        ????????????????????????????????????????) ref=$version ;;
        *) ref=refs/tags/$version ;;
    esac
    curl -fsSL --max-time 120 -o "$archive" \
        "https://github.com/$repo/archive/$ref.tar.gz"
    echo "$hash  $archive" | shasum -a 256 -c - >/dev/null
    tar -xzf "$archive" -C "$dest"
    rm -f "$archive"
done <<'EOF'
vertico minad/vertico 2.15 e2ca681da93408caec3549863cb2c34dd573ecbc2d83a539a3c95f500a53a099
orderless oantolin/orderless 1.8 81b2c72b17dde8f54c9a98df0c504e486ef7a7c95bf90a29ab5c29916f860d56
marginalia minad/marginalia 2.13 a06424b7eefc5edd16762ec6db334abf41e1c6b7f978f2f86c7c979121a0d07c
osx-dictionary.el xuchunyang/osx-dictionary.el 655bca5cea78440a1ac41f9cd78711b9c8aff8f3 41176c86761323eefd0b4a07322833703bf38649d589b99d042f3b33b3672423
EOF
