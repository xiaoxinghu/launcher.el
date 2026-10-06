---
id: launcher-apple-dictionary-example
status: done
depends_on:
  - launcher-tool-routing
---

# Demonstrate configurable tools with the existing Apple Dictionary package

## Problem and outcome

Prove the routing mechanism with a useful real adapter, not a fake backend and
not a new dictionary implementation. Register `d` in example/user configuration
so `d SPC WORD RET` uses osx-dictionary.el and shows a readable definition in the
launcher's current result window, without opening Dictionary.app or another prompt.

## Blocked by

[Configurable routing](configurable-tool-routing.md) defines the one-string→live-
buffer handler contract. No Portal prerequisite; demonstrate this in normal Emacs.

## Source-backed integration constraints

Upstream: <https://github.com/xuchunyang/osx-dictionary.el>.
Inspected `master` source on 2026-10-06; choose and record the actual tested
package release/commit during implementation. The package was not found in the
user's installed `~/.config/emacs/elpa` or source config during planning.

- `osx-dictionary-search-input` is an interactive zero-argument function that calls
  `read-string`. It is **not** directly a one-string handler; binding `d` to it
  would cause a duplicate prompt.
- `osx-dictionary--view-result` takes a word but is private and selects/displays
  through `osx-dictionary--goto-dictionary`, including other-window behavior.
  Calling it blindly does not guarantee in-place presentation.
- Upstream has buffer creation/mode, lookup and result-rendering helpers, a native
  `osx-dictionary-cli` compiled from its `.m` source, selected/allowed dictionary
  settings, a package result header/keymap, and saved previous-window state.
- The current lookup path uses a synchronous helper. Do not claim nonblocking
  behavior or cancellation for that path. Do not build new networking or parse
  Apple's dictionaries in launcher.el.

## Plan

1. Add an optional `launcher-osx-dictionary.el` adapter with proposed function
   `launcher-osx-dictionary-lookup (query)`. Requiring Launcher must not require
   osx-dictionary; fail with an actionable message only when the tool is used.
   No default `d` registration: provide a documented config example.
2. Inspect/pin the package interface. Prefer a public noninteractive lookup/render
   interface if one exists in the chosen version; otherwise isolate the smallest
   necessary private helper use in this adapter, with explicit compatibility checks
   and tests. A small upstream interface contribution is preferable to widespread
   advice. Do not globally replace `read-string`, `display-buffer`, or package
   functions merely to drive the interactive command.
3. Build or reuse a launcher-specific result buffer through package rendering so
   headings, Unicode, wrapping and definitions are preserved. Return that buffer
   without selecting another window/frame. Respect user-selected/allowed dictionary
   settings; avoid erasing an unrelated existing dictionary session.
4. Keep package major-mode/read-only behavior and useful commands. Deal explicitly
   with package `q`, `s`, and saved previous-window configuration: result quit/back
   must not restore unrelated/main-frame windows, and any subsequent lookup must
   remain in the intended interaction. Scope key overrides to this result only;
   do not change `osx-dictionary-mode-map` globally or discard other users' state.
5. Validate macOS, package availability, CLI discovery/compilation and dictionary
   availability. Report missing compiler/headers, helper failure, no dictionary,
   and no definition distinctly where the package permits. Do not silently turn
   a helper failure into an empty success or browser search. Document any upstream
   error-reporting limitation rather than inventing reliable exit status.
6. Document helper installation/build requirements and avoid unexpected compilation
   in typing/preview paths. Standard CI uses a deterministic fake lookup; the real
   package/native dictionary smoke check is opt-in and records its environment.
7. Add an executable example using the routing setting:

   ```elisp
   ;; Proposed adapter/function; available only after this task is implemented.
   (require 'launcher-osx-dictionary)
   (setq launcher-tools
         '(("d" :name "Dictionary" :prompt "Word: "
                 :function launcher-osx-dictionary-lookup)))
   ```

## Acceptance

- [x] User-configured `d SPC hello RET` returns a real definition through the
  existing package, with no second prompt, external app/browser launch, unintended
  split, or frame change. It also works from ordinary `M-x launcher-buffer`.
- [x] `d` can be changed to another prefix in config without editing source;
  default launcher installation has no mandatory dictionary dependency/binding.
- [x] Adapter tests prove one exact query, no display side effects, a live
  result buffer, preserved mode/formatting, and independence from other results.
- [x] Back/quit, repeat lookup, unknown word, multiword/Unicode input, and supported
  package result commands behave as documented, without restoring foreign windows.
- [x] Missing package/helper/compiler/dictionaries and unsupported platform fail
  clearly without fallback or loss of the typed word. No automatic API calls.
- [x] Real smoke test records package revision, macOS/Emacs, installed dictionary,
  helper setup, and actual results; fake tests are not labelled native lookup proof.
- [x] README explains installation, handler contract, minimal prefix registration,
  and the tested private-interface dependency, if any.

## Verification and completion record

ERT with lookup/package stubs covers normal/error paths without relying on local
macOS dictionary contents. Run an opt-in real lookup and graphical ordinary-frame
flow in the disposable VM, then capture the query and real result. Use a synthetic
long definition for deterministic scrolling tests if dictionary length varies.
No edits to the user's live config; the later configuration task handles adoption.

### Completion record (2026-10-07)

Implemented on branch `task/apple-dictionary-example`, base `81177a1`.
Tested against osx-dictionary.el commit
`655bca5cea78440a1ac41f9cd78711b9c8aff8f3` (2026-09-06, version header 0.4;
MELPA builds from this branch), pinned by SHA-256 in `test/elpa.sh`.

- Package interface, as inspected at that commit: no public function looks
  a word up without `read-string` or `osx-dictionary--goto-dictionary`.
  The adapter uses four private names, `osx-dictionary--insert-search-result`
  (search and render, at point), `osx-dictionary--current-dictionary-description`,
  `osx-dictionary--current-word` and `osx-dictionary--load-dir`, plus public
  `osx-dictionary-mode`, `osx-dictionary-select-dictionary`,
  `osx-dictionary-cli` and `osx-dictionary-current-dictionary`. Each lookup
  checks that all are defined and names the missing ones and the tested
  commit otherwise. No advice, and no global `read-string`, `display-buffer`
  or keymap changes. An upstream public "render WORD into BUFFER" function
  would remove the private uses; not proposed yet.
- `launcher-osx-dictionary.el` (optional; requiring it loads neither the
  package nor the helper, and registers no prefix):
  - `launcher-osx-dictionary-lookup (query)`, autoloaded: trims the query,
    checks macOS, the package and its interface, finds the helper where the
    package does, or builds it, renders through the package into a temporary
    buffer (its heading, bullet indent and `whitespace-cleanup`, as
    `osx-dictionary--view-result`), and only on success copies that into
    `*Launcher Dictionary*` (`launcher-osx-dictionary-buffer-name`) in
    `osx-dictionary-mode`, with `osx-dictionary--current-word` set and point
    at the top. Displays nothing; never touches `*osx-dictionary*` or
    `osx-dictionary-previous-window-configuration`. Runs with a local
    `default-directory`, as the package runs its helper through a shell.
  - Errors, all kept in the query by the router: other platform; missing
    package; incompatible version; missing `.m` source; no clang or no Xcode
    command line tools (`xcode-select -p`, checked before running Apple's
    clang stub, which would offer an install dialog); unwritable package
    directory; failed build (clang's output); helper failure (`-l` exit
    status); no installed dictionaries; chosen dictionary not installed; no
    definition (naming what was searched). The helper always exits 0 and the
    package discards its stderr, so an empty search is told apart only by
    listing the dictionaries afterwards; a failure during the search itself
    that `-l` does not reproduce reads as "No definition".
  - The build is the package's own `clang` command, run with
    `call-process` so failures are reported instead of shown in a shell
    output buffer. Only submission builds; typing does nothing.
  - `launcher-osx-dictionary-result-mode`, a buffer-local minor mode, scopes
    three keys to the result: `q` (`launcher-back` in an interaction, else
    `quit-window`), `s` (`launcher-back` in an interaction, else read a word
    and render it in place) and `S` (the package's own dictionary prompt,
    run outside its mode so it does not open `*osx-dictionary*`, then the
    word again in place). `o`, `r`, `?` and the header line are the package's.
- Docs: README "Apple Dictionary" section (installation, prefix
  registration, display, buffer ownership, keys, synchronous lookup, helper
  build requirements, errors and the helper's reporting limit, the tested
  private interface) and the development commands.
- Tests:
  - `test/launcher-osx-dictionary-tests.el`, 12 batch checks. The pinned
    package does the rendering; a fake helper script stands in for the
    native one, logging its arguments: not a native lookup. Covered: loading
    alone (subprocess: no package loaded, no tool registered, then the
    missing-package error), platform, blank, incompatible interface, one
    exact trimmed query, no display or window change, mode, read-only,
    `visual-line-mode`, header line, heading face, bullet prefixes and
    cleanup, multiword/Unicode and `-d` pass-through, reuse, a failed lookup
    keeping the previous result, the package's own session untouched, each
    empty-result cause, each build failure (with a real clang on a broken
    source, on the host), the keys with and without an interaction, the
    package keymap unchanged, and both entry points under prefix `w` with
    the fake readers of `launcher-tools-tests.el`, including `q` back to the
    query and an unknown word keeping it.
  - `test/launcher-osx-dictionary-gui-tests.el`, 3 opt-in graphical checks
    named `launcher-dictionary-real-*`, outside `test/gui.sh`'s default
    selector: real lookups through the VM's Dictionary Services.
  - `test/elpa.sh` also fetches the package at a commit;
    `test/launcher-tools-tests.el` now provides its feature for reuse;
    `test/gui.sh` loads the new GUI file.

Verification:

| Check | Result |
| --- | --- |
| `emacs --batch -Q -L . -L test -l test/launcher-tests.el -l test/launcher-buffer-tests.el -l test/launcher-tools-tests.el -l test/launcher-osx-dictionary-tests.el -f ert-run-tests-batch-and-exit` (host, after `sh test/elpa.sh`) | 60/60 passed: 48 earlier, unchanged, and 12 new |
| The same in a copy without `.cache/elpa` | 50 passed, the 10 package checks skipped |
| `emacs --batch -Q -L . -L test … --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile` of the three package files and the batch and GUI test files | no warnings |
| `git diff --check` | clean |
| Host real smoke: batch `-Q`, a scratch copy of the pinned package, `launcher-osx-dictionary-lookup` of ` hello `, `qwxzv`, `café` | helper built; definitions of "hello" and "cafe" in `osx-dictionary-mode`; `No definition for "qwxzv" (searched: All active dictionaries)` |
| `bash test/vm.sh sh test/gui.sh '^launcher-dictionary-real-'` | 3/3 passed; [log](evidence/launcher-apple-dictionary/gui-tests.log) |
| `bash test/vm.sh` (default selector) | 16/16 earlier GUI checks passed |

Real environment (VM, from the log): macOS 27.0.1, GNU Emacs 31.1 NS
(repository `a360712c9d272d950d8d8255ef74570f7e90b7d9`), `-Q`, no Portal,
Vertico 2.15, Apple clang 21.0.0 (clang-2100.3.34.2), Xcode at
`/Applications/Xcode.app/Contents/Developer`, helper built by the first
lookup into a temporary copy of the package, no dictionary restriction
(all active dictionaries), 87 dictionaries listed, among them New Oxford
American Dictionary and Oxford Dictionary of English. The host smoke ran on
macOS 27.0.1 with the same clang.

Observed real results: `d SPC hello RET` in `launcher-buffer` showed the entry
("hello hel·lo | həˈlō, heˈlō | … used as a
greeting …") in the launcher's window, one window, same frame. `s` returned
to the query with "hello"; `qwxzv` kept the word with `Dictionary failed: No
definition for "qwxzv" (searched: All active dictionaries)`; "ice cream"
found its entry; `q` returned to the query with "ice cream"; Escape restored
the window. Pasted `d café` found "cafe ca·fe | kaˈfā | (also café) noun".
From `launcher`, `d hello RET` showed the result by `pop-to-buffer`, which
in this `-Q` frame split the window, as display rules decide. No app launch,
browser, `*osx-dictionary*` buffer or saved window configuration in any run.

Screenshots (VM, actual Emacs drawing surface):
[query](evidence/launcher-apple-dictionary/40-dictionary-query.png),
[result](evidence/launcher-apple-dictionary/41-dictionary-result.png),
[back with s](evidence/launcher-apple-dictionary/42-dictionary-back.png),
[unknown word](evidence/launcher-apple-dictionary/43-dictionary-unknown.png),
[multiword](evidence/launcher-apple-dictionary/44-dictionary-multiword.png),
[pasted Unicode](evidence/launcher-apple-dictionary/45-dictionary-unicode.png),
[`launcher` result](evidence/launcher-apple-dictionary/46-dictionary-minibuffer-result.png).

Not run, not covered, or limits:

- A machine without Xcode's command line tools, without dictionaries, or
  with an unwritable package directory: these paths are batch checks with a
  stubbed predicate, an empty fake listing and a read-only temporary
  directory, not real environments.
- `S`, `o` and `r` were not pressed in the GUI. `S` is a batch check with a
  stubbed `completing-read`; `o` and `r` are the package's own commands,
  which run `open dict://…` and `say`.
- Lookups and the first build are synchronous and block Emacs. Whether C-g
  interrupts them was not tested; no cancellation is claimed.
- Only the pinned commit was tested; Emacs 29.1 was not.
- The working checkout's stale, untracked, gitignored `launcher.elc` was
  deleted, so `-L .` now loads the source. No user configuration was changed.
