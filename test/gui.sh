#!/bin/sh
# GUI ONLY, in the test VM: bash test/vm.sh sh test/gui.sh [SELECTOR]
# Runs the graphical checks of launcher-buffer, of tool routing and of
# app icons in a disposable Emacs without Portal or a user init, typing
# through AppKit's event queue.  The real Apple Dictionary checks are opt-in:
# bash test/vm.sh sh test/gui.sh '^launcher-dictionary-real-'.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
emacs=${EMACS:-/Applications/Emacs.app/Contents/MacOS/Emacs}
elpa="$root/.cache/elpa"
xcrun clang -fobjc-arc -Wall -Wextra -Werror -bundle \
    -I "$(dirname "$emacs")/../Resources/include" "$root/test/native-input.m" \
    -framework AppKit -framework IOSurface -o "$root/.cache/native-input.dylib"
export LAUNCHER_TEST_ROOT="$root"
export LAUNCHER_TEST_SELECTOR="${1:-^launcher-gui-}"
# Outside repo/: the VM runner deletes a successful run's checkout.
export LAUNCHER_TEST_SCREENSHOTS="$root/../screenshots"
mkdir -p "$LAUNCHER_TEST_SCREENSHOTS"
"$emacs" -Q --module-assertions -L "$root" -L "$root/test" \
    -L "$elpa/vertico-2.15" -L "$elpa/vertico-2.15/extensions" \
    -L "$elpa/orderless-1.8" -L "$elpa/marginalia-2.13" \
    --eval '(module-load (expand-file-name ".cache/native-input.dylib"
                                         (getenv "LAUNCHER_TEST_ROOT")))' \
    -l "$root/test/launcher-tools-gui-tests.el" \
    -l "$root/test/launcher-osx-dictionary-gui-tests.el" \
    -l "$root/test/launcher-icons-gui-tests.el" \
    --eval '(run-at-time 1 nil #'\''launcher-gui-run-and-exit)'
