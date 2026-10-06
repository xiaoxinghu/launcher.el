---
id: launcher-full-buffer
status: done
---

# Add a full-buffer launcher that works without Portal

## Problem and outcome

The existing `launcher` is a synchronous app/search minibuffer command. Add
`launcher-buffer`: the same app/search behavior presented at the top of an
ordinary Emacs window, with room for future query/result views. Both entry points
must work without Portal; the old minibuffer behavior stays the default.

Read [shared decisions](README.md). Scope is launcher UI, not native panel sizing.

## Interface and behavior

- Keep `launcher (&optional refresh)` and its `C-u` meaning unchanged.
- Add autoloaded `launcher-buffer (&optional refresh)`, callable through M-x,
  an ordinary key, or `(portal-launcher-present #'launcher-buffer :kind 'buffer
  :mode-line nil)`. That host call uses today's public Portal interface.
- Select an ordinary window in the invoking frame and run the buffer interaction
  there. The function remains active until external success or explicit quit;
  query/result transitions do not return from the outer invocation. Own any
  required command loop/recursive edit inside Launcher, not in user config.
- Selected app/bang/fallback success ends the interaction, allowing a host to
  dismiss normally. In normal Emacs, restore the previous window's presentation
  without stealing focus back from the launched application. Quit restores only
  windows still owned by this interaction; never overwrite unrelated user changes.
- Use an optional Vertico buffer presentation for the initial implementation.
  Missing/incompatible Vertico should give a clear error only for this entry
  point, not prevent loading or using the original command.
- Reuse application indexing, duplicate labels, annotations, web encoding, and
  dispatch logic. Do not create parallel implementations of those behaviors.
- Expose launcher-owned commands for navigation/quit needed by later tasks;
  keep their state and keymaps local to the interaction. Escape/C-g ends the whole
  interaction; Back is a distinct command/key, not a conflicting meaning for
  the host's dismissal keys.

## Plan

1. Inspect `launcher.el` and `test/launcher-tests.el`; retain the nine current
   input-regression tests and add coverage before refactoring their shared logic.
2. Introduce the smallest shared read/dispatch seam needed by both frontends.
   Separate optional UI code into `launcher-buffer.el` if that keeps loading
   dependencies clear. Follow the `launcher`/`launcher--` naming convention.
3. Implement a compact picker using the invoking ordinary window, without an
   empty buffer above it, mandatory mode line, other-frame popup, or native frame
   manipulation. Caller display rules and third-party result modes must remain
   usable. Do not globally hide mode lines or change completion settings.
4. Learn from the prototype rather than copying it: preserve a stable caller row
   cap rather than derive it from the current window height; do not run the stock
   vertico-buffer shrinking path that produced a negative minibuffer height in a
   small normal frame. Keep the legal echo-area row. No rebinding Emacs's safe
   window minimum, native cropping, or global function replacement via `cl-letf`.
5. Use supported Vertico extension seams where possible; isolate/version-check
   any unavoidable private dependency. Honor the caller's reader, Orderless,
   annotations, sorting, row cap, directory keys, and nested prompts. Resolve
   conflicting multiform display choices locally, not via global configuration.
6. Build interaction cleanup before adding tools: unfinished minibuffers, hooks,
   overlays, dedication, window state, keymaps, and recursive command loops must
   unwind on ordinary completion, quit, error, buffer kill, or host cancellation.
   Async work cannot select/focus another frame or resurrect a closed UI.
7. Add a generic synthetic result fixture to prove picker→ordinary buffer→Back
   can be hosted, without importing a dictionary, gptel, or Portal into core.
   Task 2 supplies actual routing. Preserve the result mode rather than force all
   content into the picker's major mode; navigation can use scoped minor maps.
8. Document `launcher-buffer`, dependency/version requirements, lifecycle and
   keys. Do not promise automatic resizing in normal Emacs; scroll within the
   user's existing window. Panel fitting is separate infrastructure work.

## Acceptance

- [x] `launcher` still uses the minibuffer and all existing selection, Space,
  empty/bang-only cancellation, URL encoding, fallback, and refresh tests pass.
- [x] `launcher-buffer` works in ordinary graphical Emacs with Portal not loaded
  or on the load path. It does not resize/delete the invoking Emacs frame.
- [x] App and web behavior is shared; highlighted Return/arrow selection and
  multiword input behave consistently in both presentations.
- [x] Six→two→zero→six displayed rows work with the caller's row cap; shrinking
  the host does not permanently reduce that cap or produce redisplay errors.
- [x] No empty top buffer, unintended split, global mode-line mutation, leaked
  minibuffer, or other-frame display. A small window remains usable by scrolling.
- [x] Matching/annotations/custom reader and keys survive, including multiform
  rules outside the launcher. Nested prompt return restores the right view.
- [x] The generic result fixture remains interactive in the same ordinary window;
  Back and quit work, and the outer entry point returns only on final exit.
- [x] Cancellation/error/buffer-kill/host unwind restores owned state and leaks no
  hooks, timers, keymaps, dedicated windows, or recursive edits. Reentry works.

## Verification and completion record

Run existing and new ERT tests, byte-compile changed Lisp, and `git diff --check`.
Current baseline command:

```sh
emacs --batch -Q -L . -l test/launcher-tests.el -f ert-run-tests-batch-and-exit
```

Add a repeatable graphical fixture proving ordinary-frame behavior with Portal
absent and real event-loop input. On this machine, use the established macOS VM
runner for graphical checks, not the user's live host Emacs. A Portal-hosted smoke
test is useful additional evidence but not a substitute for standalone behavior.
Record versions, commands, results, and skipped manual checks here. No live user
config changes in this task.

### Completion record (2026-10-06)

Implemented on branch `task/implement-next-todo-under-tasks-c28886e9`, base
`77972eb`.

- `launcher.el`: shared seam `launcher--entries` (refresh + index),
  `launcher--read` (annotations, Space handling, empty/bang-only cancellation,
  optional initial input and setup hook) and `launcher--act` (app, bang and
  fallback dispatch). `launcher` is now those three calls. The autoloaded
  `launcher-buffer` command is defined here, so `use-package :commands` and
  package autoloads both work; it loads `launcher-buffer.el` on first use.
- `launcher-buffer.el`: the interaction. It records the selected window's
  buffer, start/point markers, hscroll, dedication and buffer history, then
  runs views (`(picker INPUT)`, `(result BUFFER)`) in that window until a
  choice succeeds or the user quits. Commands `launcher-back` and
  `launcher-quit`; keymaps `launcher-buffer-picker-map` (composed over
  Vertico's keys, picker minibuffer only) and `launcher-buffer-map` (result
  views, via an `emulation-mode-map-alists` entry whose bindings are filtered
  to the interaction's selected window, installed only during the
  interaction). Internal `launcher-buffer--visit` shows a result view; task 2
  routes tools through it.
- Vertico: `vertico-buffer-mode` is set *buffer-locally* in the picker's
  minibuffer (Vertico's display methods dispatch on that value), as are nil
  values for flat/grid/reverse/unobtrusive/posframe display modes. Nested
  prompts and other frames keep the user's global modes and multiform rules.
  `vertico-buffer-hide-prompt` is locally nil, so Vertico never shrinks the
  real minibuffer window; the duplicate prompt there is scrolled away instead
  and the echo-area row is kept. A locally non-nil `resize-mini-windows` lets
  Emacs itself return that window to one line after a long message grew it
  (with the default `grow-only` it stayed four lines tall in the VM).
  Vertico's `display-buffer` call is pointed at the interaction's window only
  during setup, overriding any host action.
- Row cap: `vertico-count` as bound at invocation. Each redisplay shows
  `min(cap, rows the window fits)` and re-exhibits at once when that changes.
  When a window is too short for the cap, blank rows below its bottom keep the
  candidate list as tall as the cap would make it, so a host that fits its
  window to content can grow back; a short ordinary window scrolls the
  selection through the rows that fit. (Window vscroll was tried first and
  does not work for Vertico's multi-row overlay string.)
- Exit restores the window only while the interaction still owns it (it shows
  the original buffer, the picker, or a result the interaction displayed);
  views are dropped from its buffer history. A deleted window ends the
  interaction with `quit`; a killed result returns to the previous view.
  Result buffers are never killed, erased or re-moded.
- Private Vertico internals used are listed and checked in
  `launcher-buffer--vertico-compatible-p`; missing/incompatible Vertico or a
  disabled `vertico-mode` signals a clear `user-error` from this entry point
  only. `launcher` needs no Vertico.
- Tests: `test/launcher-buffer-tests.el` (16 batch checks with a fake reader
  and real recursive edits) and `test/launcher-buffer-gui-tests.el` (8 graphical
  checks). GUI harness: `test/vm.sh` (Portal-free copy of Portal's VM runner,
  sharing its guest and desktop lock), `test/gui.sh`, `test/native-input.m`
  (AppKit key events and drawing capture), `test/elpa.sh` (pinned packages).

Verification:

| Check | Result |
| --- | --- |
| `emacs --batch -Q -L . -l test/launcher-tests.el -l test/launcher-buffer-tests.el -f ert-run-tests-batch-and-exit` (host) | 25/25 passed: the 9 original input regressions, unchanged, and 16 new |
| `emacs --batch -Q -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile launcher.el launcher-buffer.el` | no warnings (also with Vertico on the load path; test files compile cleanly too) |
| `git diff --check` | clean |
| `bash test/vm.sh` (runs `sh test/gui.sh` in the VM) | 8/8 passed, none skipped; [log](evidence/launcher-full-buffer/gui-tests.log) |

GUI environment: macOS 27.0.1 test VM, GNU Emacs 31.1 NS (emacs-plus, built
2026-09-26, repository `a360712c9d272d950d8d8255ef74570f7e90b7d9`, as Portal
pins), `-Q`, no Portal on the load path, Menlo, 80×36 frame, 2× screenshots.
Vertico 2.15, Orderless 1.8 and Marginalia 2.13 from GitHub release tags with
SHA-256 pins. The user's installed Vertico snapshot `20260830.123` has the same
`vertico-buffer.el`; its `vertico.el` differs only in require-match validation.

The graphical checks type through AppKit's event queue (C-g through Emacs's
event queue: this build holds synthetic AppKit C-g). They cover: Portal absent;
a one-line minibuffer window after a long message; 6→2→0→6 rows with cap 6 and arrow/Return selection; identical results from
both entry points for arrow selection, multiword fallback and a bang query,
with `launcher` reading in the minibuffer; a 3-line window, window regrowth,
a 6-line frame and regrowth (rows 2, 6, 3, 6; selection always visible; no
redisplay errors); orderless, Marginalia, a custom `completing-read-function`,
vertico-directory RET/DEL, a multiform grid rule for the launcher overridden
locally, a nested prompt with its own multiform rule and return to the picker,
and a file prompt still using its grid rule afterwards; a synthetic
`special-mode` result with its own keys and a timer appending to it, Back with
input restored, Escape, and late updates not touching windows; Escape/C-g in
picker and result, a failing result command, killing the result, a host's
throw, a failing launch, a deleted window, and reentry. Each interaction
check verifies afterwards: frame list, size and position, window buffer/start/point/
dedication, default mode line, minibuffer hooks, `post-command-hook`,
`emulation-mode-map-alists`, recursion depth, minibuffer scroll and timers.

Screenshots (VM, actual Emacs drawing surface):
[picker](evidence/launcher-full-buffer/01-picker.png),
[filtered](evidence/launcher-full-buffer/02-filtered.png),
[no match](evidence/launcher-full-buffer/03-no-match.png),
[3-line window, scrolled selection](evidence/launcher-full-buffer/07-small-window-scrolled.png),
[6-line frame](evidence/launcher-full-buffer/08-shrunk-frame.png),
[result](evidence/launcher-full-buffer/10-result.png),
[back to picker](evidence/launcher-full-buffer/11-back-to-picker.png),
[nested prompt](evidence/launcher-full-buffer/12-nested-prompt.png).

Not run or not covered: a Portal-hosted smoke test; physical keys, IME and
VoiceOver; the user's full init (a fixture reproduces its completion setup);
real app launches and browser opening (stubbed). Batch Emacs exits on a
command error inside a recursive edit, so result-command errors are checked
graphically only. No user configuration was changed.
