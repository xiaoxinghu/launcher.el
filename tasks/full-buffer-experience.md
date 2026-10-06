---
id: launcher-full-buffer
status: todo
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

- [ ] `launcher` still uses the minibuffer and all existing selection, Space,
  empty/bang-only cancellation, URL encoding, fallback, and refresh tests pass.
- [ ] `launcher-buffer` works in ordinary graphical Emacs with Portal not loaded
  or on the load path. It does not resize/delete the invoking Emacs frame.
- [ ] App and web behavior is shared; highlighted Return/arrow selection and
  multiword input behave consistently in both presentations.
- [ ] Six→two→zero→six displayed rows work with the caller's row cap; shrinking
  the host does not permanently reduce that cap or produce redisplay errors.
- [ ] No empty top buffer, unintended split, global mode-line mutation, leaked
  minibuffer, or other-frame display. A small window remains usable by scrolling.
- [ ] Matching/annotations/custom reader and keys survive, including multiform
  rules outside the launcher. Nested prompt return restores the right view.
- [ ] The generic result fixture remains interactive in the same ordinary window;
  Back and quit work, and the outer entry point returns only on final exit.
- [ ] Cancellation/error/buffer-kill/host unwind restores owned state and leaks no
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
