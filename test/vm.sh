#!/bin/bash
# Copy this checkout to the macOS test VM and run a GUI check command there.
# Uses the guest set up for Portal's test/vm.sh, and its desktop lock, but
# needs no Portal checkout or build.  Usage: bash test/vm.sh [COMMAND...]
set -euo pipefail

quote() { printf '%q ' "$@"; }

source_files() {
    git ls-files --cached --others --exclude-standard --deduplicate -z |
        while IFS= read -r -d '' file; do
            if [[ -e "$file" || -L "$file" ]]; then printf '%s\0' "$file"; fi
        done
    # Pinned packages fetched on the host; the guest needs no network.
    find .cache/elpa -type f -print0
}

run_stage() (
    log=$1; shift
    echo "Running $(quote "$@") (limit ${timeout}s)"
    # Bash job control gives each background job its own process group.
    set -m
    child= watchdog=
    cleanup() {
        for pid in "$child" "$watchdog"; do
            if [[ -n "$pid" ]]; then kill -KILL -- "-$pid" 2>/dev/null || :; fi
        done
        wait 2>/dev/null || :
    }
    trap cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    "$@" </dev/null >"$log" 2>&1 & child=$!
    (
        set +m
        sleep "$timeout"
        touch "$log.timeout"
        echo "Timed out after ${timeout}s" >>"$log"
        kill -KILL -- "-$child" 2>/dev/null || :
    ) & watchdog=$!
    status=0
    wait "$child" 2>/dev/null || status=$?
    if [[ -f "$log.timeout" ]]; then status=124; fi
    exit "$status"
)

guest() {
    run=$(cd -- "$1" && pwd -P); shift
    [[ $(dirname "$run") == "$HOME/portal-test-runs" && ${run##*/} == launcher.* ]]
    trap 'printf "%s\n" "$?" >"$run/exit-status"' EXIT
    export PATH=/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin
    export EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
    # Shared with Portal's runner: one GUI run at a time on this desktop.
    exec 9>"$HOME/portal-test-runs/.desktop.lock"
    echo "Waiting for the VM desktop…"
    /usr/bin/lockf 9
    [[ $(stat -f %Su /dev/console) == "$USER" ]] || {
        echo "Log in to the VM desktop as the SSH user first" >&2; exit 1;
    }
    caffeinate -disu -w "$$" 9>&- &
    cd -- "$run/repo"
    # Direct SSH launches miss the Emacs app's native compiler environment.
    export LIBRARY_PATH
    LIBRARY_PATH=$(/usr/libexec/PlistBuddy -c Print:LSEnvironment:LIBRARY_PATH \
        /Applications/Emacs.app/Contents/Info.plist)
    status=0
    run_stage "$run/test.log" "$@" || status=$?
    if [[ $status == 0 ]]; then cd -- "$run"; rm -rf -- "$run/repo"; fi
    exit "$status"
}

main() {
    timeout=${LAUNCHER_TEST_TIMEOUT:-600}
    [[ $timeout =~ ^[1-9][0-9]*$ ]] || {
        echo "LAUNCHER_TEST_TIMEOUT must be a positive number of seconds" >&2; exit 2;
    }
    if [[ ${1:-} == --guest ]]; then shift; guest "$@"; fi
    if [[ ${1:-} == -- ]]; then shift; fi
    if [[ $# == 0 ]]; then set -- sh test/gui.sh; fi
    vm=${PORTAL_TEST_VM:-portal-vm}
    [[ $vm != -* && ! $vm =~ [[:space:]] ]] || {
        echo "Use an SSH host alias or user@hostname" >&2; exit 2;
    }
    cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."
    sh test/elpa.sh
    ssh_vm=(ssh -T -o BatchMode=yes "$vm")
    run=$("${ssh_vm[@]}" 'umask 077; mkdir -p "$HOME/portal-test-runs" &&
        mktemp -d "$HOME/portal-test-runs/launcher.XXXXXXXX"')
    [[ $run == /*/portal-test-runs/launcher.* && $run != *$'\n'* ]]
    logs="$PWD/.cache/vm/${run##*/}"
    mkdir -p -- "$logs"
    printf '%s:%s\n' "$vm" "$run" >"$logs/remote.txt"
    printf 'VM run: %s:%s\nLocal logs: %s\n' "$vm" "$run" "$logs"
    source_files >"$logs/files"
    rsync -a --from0 --files-from="$logs/files" -e 'ssh -o BatchMode=yes' \
        ./ "$vm:$(quote "$run/repo/")"
    collect() {
        rsync -a -e 'ssh -o BatchMode=yes' --include='/*.log' --include=/exit-status \
            --include=/screenshots/ --include='/screenshots/*.png' --exclude='*' \
            "$vm:$(quote "$run/")" "$logs/" ||
            echo "Could not fetch logs; retained at $vm:$run" >&2
        echo "Logs: $logs"
    }
    trap collect EXIT
    status=0
    "${ssh_vm[@]}" "$(quote env "LAUNCHER_TEST_TIMEOUT=$timeout" /bin/bash \
        "$run/repo/test/vm.sh" --guest "$run" "$@")" || status=$?
    exit "$status"
}

if [[ ${BASH_SOURCE[0]} == "$0" ]]; then main "$@"; fi
