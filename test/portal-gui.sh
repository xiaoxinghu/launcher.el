#!/bin/sh
# GUI ONLY, in the test VM, from a built Portal checkout: run it with
# bash test/portal-vm.sh [SELECTOR].  Runs the Portal-hosted launcher checks
# in a disposable `emacs -Q' with Portal, the user's completion setup
# (test/portal-acceptance-config.el) and the real Apple Dictionary.
set -eu
portal=$(pwd)
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
export LAUNCHER_TEST_SELECTOR="${1:-^launcher-portal-}"
xcrun clang -fobjc-arc -Wall -Wextra -Werror -bundle \
    -I "$(cat "$portal/build/emacs-include.txt")" "$portal/test/terminal-input.m" \
    -framework AppKit -framework WebKit -framework Carbon -framework IOSurface \
    -o "$portal/build/test-terminal-input.dylib"
set -- -Q --module-assertions -L "$portal" -L "$portal/test"
for package in "$portal"/build/elpa/*; do
    # Portal pins a launcher.el for its own recipe check: use this checkout.
    case ${package##*/} in launcher-*) continue ;; esac
    if [ -d "$package" ]; then set -- "$@" -L "$package"; fi
done
export PORTAL_LAUNCHER_TEST_ROOT="$portal" LAUNCHER_TEST_ROOT="$root"
mkdir -p "${PORTAL_LAUNCHER_SCREENSHOTS:?Set PORTAL_LAUNCHER_SCREENSHOTS}"
"${EMACS:-/Applications/Emacs.app/Contents/MacOS/Emacs}" "$@" -L "$root" -L "$root/test" \
    -l "$portal/portal.el" \
    --eval '(module-load (expand-file-name "build/test-terminal-input.dylib"
                                         (getenv "PORTAL_LAUNCHER_TEST_ROOT")))' \
    --eval '(condition-case err
                (let ((test (expand-file-name "test/" (getenv "LAUNCHER_TEST_ROOT"))))
                  (load (expand-file-name "portal-acceptance-config" test))
                  (load (expand-file-name "launcher-portal-gui-tests" test))
                  (run-at-time 1 nil (quote launcher-portal-run-and-exit)))
              ;; A graphical Emacs would otherwise wait with the error shown.
              (error (princ (format "Loading the checks failed: %S\n" err)
                            (quote external-debugging-output))
                     (kill-emacs 1)))'
