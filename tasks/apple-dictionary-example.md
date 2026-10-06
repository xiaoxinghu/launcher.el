---
id: launcher-apple-dictionary-example
status: todo
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

- [ ] User-configured `d SPC hello RET` returns a real definition through the
  existing package, with no second prompt, external app/browser launch, unintended
  split, or frame change. It also works from ordinary `M-x launcher-buffer`.
- [ ] `d` can be changed to another prefix in config without editing source;
  default launcher installation has no mandatory dictionary dependency/binding.
- [ ] Adapter tests prove one exact query, no display side effects, a live
  result buffer, preserved mode/formatting, and independence from other results.
- [ ] Back/quit, repeat lookup, unknown word, multiword/Unicode input, and supported
  package result commands behave as documented, without restoring foreign windows.
- [ ] Missing package/helper/compiler/dictionaries and unsupported platform fail
  clearly without fallback or loss of the typed word. No automatic API calls.
- [ ] Real smoke test records package revision, macOS/Emacs, installed dictionary,
  helper setup, and actual results; fake tests are not labelled native lookup proof.
- [ ] README explains installation, handler contract, minimal prefix registration,
  and the tested private-interface dependency, if any.

## Verification and completion record

ERT with lookup/package stubs covers normal/error paths without relying on local
macOS dictionary contents. Run an opt-in real lookup and graphical ordinary-frame
flow in the disposable VM, then capture the query and real result. Use a synthetic
long definition for deterministic scrolling tests if dictionary length varies.
No edits to the user's live config; the later configuration task handles adoption.
