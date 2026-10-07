#!/bin/bash
# Run the Portal-hosted launcher checks in the test VM: copy this checkout
# there, then let Portal's runner build a fresh copy of PORTAL_ROOT (default
# ../portal) and run test/portal-gui.sh in it.  Screenshots and the log land
# in .cache/vm/<run>.  Usage: bash test/portal-vm.sh [SELECTOR]
set -euo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."
source test/vm.sh
portal=$(cd -- "${PORTAL_ROOT:-../portal}" && pwd)
vm=${PORTAL_TEST_VM:-portal-vm}
sh test/elpa.sh
ssh_vm=(ssh -T -o BatchMode=yes "$vm")
run=$("${ssh_vm[@]}" 'umask 077; mkdir -p "$HOME/portal-test-runs" &&
    mktemp -d "$HOME/portal-test-runs/launcher-portal.XXXXXXXX"')
[[ $run == /*/portal-test-runs/launcher-portal.* && $run != *$'\n'* ]]
logs="$PWD/.cache/vm/${run##*/}"
mkdir -p -- "$logs"
printf 'VM run: %s:%s\nLocal logs: %s\n' "$vm" "$run" "$logs"
source_files | rsync -a --from0 --files-from=- -e 'ssh -o BatchMode=yes' \
    ./ "$vm:$(quote "$run/repo/")"
collect() {
    rsync -a -e 'ssh -o BatchMode=yes' --include=/screenshots/ \
        --include='/screenshots/*.png' --exclude='*' "$vm:$(quote "$run/")" "$logs/" ||
        echo "Could not fetch screenshots; retained at $vm:$run" >&2
    # As test/vm.sh does, keep a failed run's checkout for inspection.
    if [[ ${status:-1} == 0 ]]; then
        "${ssh_vm[@]}" "rm -rf -- $(quote "$run/repo")" ||
            echo "Could not remove $vm:$run/repo" >&2
    fi
}
trap collect EXIT
status=0
portal_logs=$(mktemp)
PORTAL_TEST_TIMEOUT=${PORTAL_TEST_TIMEOUT:-1500} bash "$portal/test/vm.sh" \
    env "PORTAL_LAUNCHER_SCREENSHOTS=$run/screenshots" \
    "PORTAL_REVISION=$(git -C "$portal" rev-parse HEAD)$(git -C "$portal" diff --quiet HEAD || echo +changes)" \
    "LAUNCHER_REVISION=$(git rev-parse HEAD)$(git diff --quiet HEAD || echo +changes)" \
    sh "$run/repo/test/portal-gui.sh" "$@" | tee "$portal_logs" || status=$?
# Portal's runner keeps its logs in its own checkout's .cache/vm.
if portal_run=$(sed -n 's/^Local logs: //p' "$portal_logs") && [[ -d $portal_run ]]; then
    cp -- "$portal_run"/*.log "$logs/" 2>/dev/null || :
fi
rm -f -- "$portal_logs"
exit "$status"
