---
id: launcher-tool-routing
status: done
depends_on:
  - launcher-full-buffer
---

# Route configurable prefixes to free-text tools and result buffers

## Problem and outcome

A prefix should change what the launcher does, not merely filter applications.
Typing `d SPC` must enter a clearly labelled Dictionary input state with no app
candidates. Typing a word and pressing Enter invokes a configured Lisp handler
once, then shows its result buffer. The mechanism must not know dictionary APIs.

## Blocked by

[Full-buffer experience](full-buffer-experience.md) supplies shared app behavior,
view ownership, result navigation, and lifecycle. Read [shared decisions](README.md).

## Proposed configuration contract

```elisp
;; Default launcher-tools is nil; this is an example, not a built-in binding.
(setq launcher-tools
      '(("d" :name "Dictionary" :prompt "Word: "
              :function launcher-osx-dictionary-lookup)))
```

Each entry has a nonempty prefix string with no whitespace, a display name,
query prompt, and callable accepting one query string and returning a live buffer.
Single letters are convenient but not hardcoded; multi-character or `!d` prefixes
work too. Document the Custom type and support named/autoloaded functions and
lexical closures. The optional real dictionary handler is implemented in task 3;
this task tests with credential-free fake handlers returning real Emacs buffers.

## Interaction decisions

1. Recognize an exact, case-sensitive configured prefix only at the beginning,
   followed by an ASCII space. `d` alone still filters applications; `D` is not
   implicitly `d`. Handle typing, paste, and initial/history input consistently.
2. A recognized prefix takes precedence over app matching for that input. Strip
   the prefix/delimiter into state and display e.g. `Dictionary — Word: `. Do not
   create a second surprise input dialog. Remaining pasted text becomes the query.
3. Query mode is raw text: no app list, completion filtering, selected-app Return,
   or automatic browser fallback. Spaces and Unicode remain text. Do not send
   requests on keystrokes, idle timers, or preview.
4. Enter calls the handler exactly once for a nonblank query. Reject blank input
   with a local message and keep query mode open. Pass the query unchanged after
   prefix removal; tool-specific normalization belongs to its adapter.
5. A returned live buffer is shown by Launcher in its owned ordinary window.
   In the traditional `launcher` entry point, finish the minibuffer first and use
   ordinary Emacs buffer display; do not force the full-buffer interaction there.
6. Proposed `C-c C-b` invokes `launcher-back`: result→query with the submitted word
   retained; query→app picker. Backspace at the start of an empty query also returns
   to the picker. Escape/C-g quits the whole interaction. Do not override those
   host dismissal keys to mean Back.
7. Report handler/missing-dependency/invalid-result errors in query mode; preserve
   text for correction/retry. Never turn recognized tool failures into web searches.
8. Keep `launcher-bangs` backward compatible. Reject duplicate tool prefixes and
   exact tool/bang prefix collisions with a clear configuration error before any
   action. Prefixes such as `d` and `dd` are not ambiguous because of the delimiter.
   Unknown prefixes remain normal app/fallback input, as today.

## Plan

- Validate/document `launcher-tools`, snapshot it per interaction, and add a shared
  pure routing/parser seam used by both frontends. Do not evaluate input as Lisp,
  interpolate it into shell commands, or depend on Vertico filtering internals to
  discover the full user input. Preserve user custom Space bindings where they
  already override stock completion; document/test their interaction with routing.
- Implement picker/query/result state transitions and immediate candidate removal
  on prefix entry. The query input must behave as text even if the renderer reuses
  a minibuffer underneath. End the input loop before displaying a result.
- Make explicit tools usable when app discovery is unavailable or returns no apps.
  Discovery errors must not prevent reaching a registered tool. Preserve existing
  default/no-tools behavior and make app-index errors visible without inventing
  a hard dependency on macOS for generic tools. Test with missing `mdfind` and a
  fake handler; never probe the developer's installed apps in core tests.
- Keep per-tool query history and restore queries on Back without confusing app
  history, routing prefixes, or sending another request. Do not add persistence
  of potentially sensitive queries automatically; document standard history hooks.
- Keep buffer ownership explicit: do not kill handler-owned/preexisting result
  buffers, erase their contents, or replace their major modes on Back/quit. Scope
  launcher navigation locally, and restore any keys/headers modified for display.
  A late update may change its own buffer but must not steal display/focus or
  reopen the launcher. Do not pretend a closed UI cancels arbitrary backend work.
- Support ordinary scrolling/copying. Content updates must not force point to end
  if the user scrolled up; follow-end behavior is conditional, never unconditional.
- Add end-to-end generic tool tests in ordinary Emacs plus both frontend parser
  tests. No dictionary or gptel dependency is needed to finish this task.

## Acceptance

- [x] Configured `d SPC` removes application candidates immediately and presents
  raw query input; `d`, unknown prefixes, other configured keys, case, and paste
  follow the rules above.
- [x] `d SPC serendipity RET` calls the fake handler exactly once with
  `"serendipity"`, displays its result, and triggers no app launch/browser search.
- [x] Multiword/Unicode input is preserved; blank submissions and typing/preview
  call no handler. Lookup errors preserve the word and never use web fallback.
- [x] Back result→query preserves input without resubmission; query→picker and
  empty-query Backspace work. Escape/C-g cleanly exits in every state.
- [x] Real result major mode, read-only content, copying/scrolling and ownership
  survive transitions. Preexisting result buffers and unrelated windows remain
  intact; delayed updates do not resurrect an obsolete view.
- [x] Tool/bang conflicts and invalid registrations fail clearly before action;
  existing bang/queryless/refresh/app/fallback regressions still pass.
- [x] A registered tool works without available apps/Spotlight; nil tools retain
  the previous default experience. No Portal dependency or resizing calls exist.
- [x] Both entry points support the shared router, with normal-buffer results in
  ordinary Emacs; a Portal minibuffer-only host's inability to show results is
  documented, not silently advertised as supported.

## Verification and completion record

Add ERT state/dispatch tests plus native-input graphical transitions in a disposable
ordinary Emacs. Include failure, cancellation, buffer kill, repeated invocation,
custom reader/keymap, and an async-updating fake result. Record exact checks and
limits here; do not mark complete from a simulated callback-only test or from the
old Portal prototype. No live configuration edits or real network calls.

### Completion record (2026-10-06)

Implemented on branch `task/configurable-tool-routing`, base `19c5cce`.

- `launcher.el`
  - Configuration: `launcher-tools` (default nil, documented Custom type
    `(repeat (cons string (plist ...)))`). `launcher--tools` validates it and
    returns a per-interaction snapshot of `launcher--tool` structs. It signals a
    `user-error` naming the problem before any action: non-list, malformed
    entry, prefix empty or containing whitespace (including NBSP), unknown
    key, missing or invalid `:name`/`:prompt`/`:function`/`:history`, a
    duplicate prefix, or a prefix equal to a `launcher-bangs` key.
    `:function` may be a closure, a named function, an autoload, or a name
    not yet defined; an undefined name is reported at submission.
  - Router: the pure seam `launcher--route (input tools)` returns
    `(TOOL . QUERY)` only for an exact, case-sensitive prefix at the start
    followed by an ASCII space; QUERY is the rest, unchanged. Both frontends
    use it on the picker's input and on its returned choice; the collection
    has no candidates for routed input.
  - Picker: `launcher--read-choice` adds a local post-command hook in the
    picker's minibuffer. When the full contents route to a tool (typed,
    pasted, recalled with `M-p`), it throws them out of the minibuffer at
    once, so routed input never enters the picker's history. A routed
    initial input skips reading. It reads `minibuffer-contents`, never
    Vertico's state; custom Space bindings are kept as before.
  - Query: `launcher--read-query` uses `read-from-minibuffer` with
    `launcher-query-map` (parent `minibuffer-local-map`): plain text, no
    completion, labelled `NAME — PROMPT`. Return (`launcher-query-submit`)
    refuses a blank query in place. `C-c C-b` and `DEL` in an empty query
    (`launcher-query-delete-backward-char`, a remap of
    `delete-backward-char`) call `launcher-back`.
    `launcher--query` calls the handler once per nonblank submission, after
    the minibuffer closes. On a handler error, missing function, or non-live
    or minibuffer result, it reads again with the text kept and the error
    shown after the input; there is no web fallback. `quit` propagates.
  - History: each tool keeps an uninterned history variable per prefix
    (`launcher--tool-histories`), which `savehist-mode` skips. `:history VAR`
    opts in to ordinary persistence; `:history t` keeps no history.
  - `launcher-back` moved here from launcher-buffer.el. It calls
    `launcher--back-function`, which each interaction binds.
  - `launcher`: picker → query → `pop-to-buffer` of the result after the
    minibuffer closes. Back from the query shows the picker with the prefix,
    without its space.
  - App discovery: with tools, `launcher--entries` turns discovery errors and
    an empty index into a `*Messages*` line and a
    `Launch (apps unavailable): ` prompt, so tools stay reachable. Without
    tools, errors are signaled as before.
- `launcher-buffer.el`
  - Views are `(picker INPUT)`, `(query TOOL INPUT)` and `(result BUFFER)`.
    A picker routed to a tool pushes `(picker PREFIX)`; a submitted query
    pushes `(query TOOL QUERY)`. So Back goes result → query (text kept, no
    resubmission) → picker, and killing a result returns to its query.
  - The query is the same core `read-from-minibuffer`, with the minibuffer
    buffer shown in the interaction's window, as Vertico's buffer display
    does for the picker. Its prompt is hidden from the minibuffer window by
    vscroll, its window point is synced for the cursor, and it gets a local
    mode line. `launcher-buffer-picker-map` (Escape, `C-c C-b`) is composed
    over `launcher-query-map`.
  - Conditional follow-end: a global `pre-redisplay-functions` entry,
    installed only during the interaction, moves a result view's point to
    the new end only if it was at the end of a nonempty result at the last
    redisplay. A reader who moved away keeps their position.
  - Result buffers are never killed, erased, re-moded or given keys or
    headers. After quit or Back, late updates touch only their own buffer.
- Docs: README "Tools" section (contract, keys, frontends, history,
  ownership, no cancellation of backend work, no-apps behavior, custom Space
  bindings, the minibuffer-only host limitation); `launcher`,
  `launcher-buffer` and `launcher-tools` docstrings.
- Tests: `test/launcher-tools-tests.el` (22 batch checks: validation,
  router, collection, both frontends' transitions with fake readers that run
  the real setup and post-command hooks, typed/pasted/recalled/initial
  input, blank, failures, missing/unloadable/autoloaded/closure handlers,
  Back by command/`C-c C-b`/`DEL`, quit from each reading view, no
  apps/mdfind, snapshot, user history, custom Space binding, query keys,
  result→query→picker, kill, preexisting result in another window, and the
  follow rule). `test/launcher-tools-gui-tests.el` (8 graphical checks,
  loaded by `test/gui.sh` together with the earlier 8).

Verification:

| Check | Result |
| --- | --- |
| `emacs --batch -Q -L . -l test/launcher-tests.el -l test/launcher-buffer-tests.el -l test/launcher-tools-tests.el -f ert-run-tests-batch-and-exit` (host, clean copy) | 48/48 passed: 26 earlier, unchanged, and 22 new |
| `emacs --batch -Q -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile launcher.el launcher-buffer.el` | no warnings; the batch and GUI test files compile cleanly too (GUI ones with the pinned Vertico, Orderless, Marginalia) |
| `git diff --check` | clean |
| `bash test/vm.sh` (runs `sh test/gui.sh` in the VM) | 16/16 passed, none skipped; [log](evidence/launcher-tool-routing/gui-tests.log) |

GUI environment: the same as the previous task. macOS 27.0.1 test VM, GNU
Emacs 31.1 NS (repository `a360712c9d272d950d8d8255ef74570f7e90b7d9`), `-Q`,
no Portal on the load path, Menlo, 80×36 frame, Vertico 2.15, Orderless 1.8
and Marginalia 2.13 (SHA-256 pinned).

The graphical checks type through AppKit's event queue: letters, Space,
Return, DEL, Escape, arrows, `C-c C-b`, `C-SPC`, `M-w`, `M-p`, `>` and `s-v`.
C-g goes through Emacs's event queue, as before. They cover:

- `launcher-buffer`: `d` stays in the picker; `d SPC` shows the query
  immediately with no Vertico candidates. Typing calls nothing; Return
  calls the handler once and shows a read-only result in the same window,
  where Space scrolls and a region copies. Back → query with
  "serendipity", Back → picker with "d", `SPC`, `DEL` → picker, Escape.
  The window state is restored afterwards.
- `launcher` with Vertico and with stock completion: the query reads plain
  text in the minibuffer, not in a window. `C-c C-b` and `DEL` return to the
  picker; Return shows the result by `pop-to-buffer`; C-g quits a query.
- Pasted `d 中文 café  two words` → query preserved exactly. `M-p` recalls
  `d recalled` into a query. Blank and space-only Return show
  `[Type a query first]`. The picker history stays unchanged and the tool
  history gets the query.
- Errors kept in the query with notices: handler error, nil result,
  undefined function, and the minibuffer `launcher` variant. Then a fix and
  retry, a killed result returning to its query, and reentry with an app
  launch.
- An asynchronously updating result: it follows the end, keeps the
  reader's place after `up up`, resumes after `>`, and after Back and quit
  keeps updating in the background without being shown again.
- A preexisting read-only buffer shown in another window, returned by a
  closure: unchanged, as is the other window.
- `exec-path` without mdfind: both entry points show
  `Launch (apps unavailable): ` and reach the tool.
- A custom `completing-read-function`, Orderless, Marginalia, a multiform
  grid rule, and a user key added to `launcher-query-map`.

Screenshots (VM, actual Emacs drawing surface):
[query](evidence/launcher-tool-routing/21-tool-query.png),
[typed](evidence/launcher-tool-routing/22-tool-query-typed.png),
[result](evidence/launcher-tool-routing/23-tool-result.png),
[back to query](evidence/launcher-tool-routing/24-tool-back-to-query.png),
[back to picker](evidence/launcher-tool-routing/25-tool-back-to-picker.png),
[minibuffer query](evidence/launcher-tool-routing/26-minibuffer-query.png),
[minibuffer result](evidence/launcher-tool-routing/27-minibuffer-result.png),
[pasted Unicode](evidence/launcher-tool-routing/28-tool-pasted.png),
[blank](evidence/launcher-tool-routing/29-tool-blank.png),
[error](evidence/launcher-tool-routing/30-tool-error.png),
[stream following](evidence/launcher-tool-routing/32-tool-stream-following.png),
[no apps](evidence/launcher-tool-routing/33-tool-no-apps.png).

Not run, not covered, or limits:

- Emacs 29.1, the documented minimum, was not tested; only 31.1 is
  installed.
- No physical keys, IME or VoiceOver. Non-ASCII input was pasted, not typed.
- No Portal-hosted run. A minibuffer-only host's inability to show results
  is documented, not exercised.
- No real dictionary, gptel or network: task 3 supplies a real handler.
- Handlers run synchronously after the query closes. A slow one blocks
  Emacs, and C-g then quits the interaction. Until the result view appears,
  `launcher-buffer`'s window shows the closed, empty query.
- A result mode's own `q` (`quit-window`) replaces the buffer in the
  launcher's window while the interaction continues, as for any user window
  change since task 1. The dictionary adapter (task 3) handles its package's
  `q`.
- In `launcher`, Escape keeps Emacs's minibuffer meaning (ESC ESC ESC); only
  C-g was checked there.
- In batch, `launcher-buffer`'s query display is stubbed with the other
  Vertico setup; it is checked graphically.
- The working checkout holds an untracked, gitignored `launcher.elc` from
  2026-10-05 that shadows the source in plain `-L .` runs. The checks ran on
  a clean copy of the tracked and new files. No user configuration was
  changed.
