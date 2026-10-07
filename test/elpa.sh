#!/bin/sh
# Fetch the pinned packages that the checks load into .cache/elpa, which
# Git ignores: the completion packages of the Vertico checks, nerd-icons
# for the icon checks, and osx-dictionary.el; those without a release tag
# at a commit.  Already-fetched packages are kept.
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
nerd-icons.el rainstormstudio/nerd-icons.el 17faac7977242b470732efd417d3bcc8eb5a830e abb95c663559b74dd72c3a9a1a374ab523837cb6d1bddb9bf366def33f5771ec
nerd-icons-completion rainstormstudio/nerd-icons-completion f924dd490c8c4c1066fd97a76e0dc31e303fca30 77509182a736d7fbc25cf0673bd95212823dbb83327f1daf0f6d9709f4f68190
EOF
