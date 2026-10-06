---
id: launcher-tool-routing
status: todo
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

- [ ] Configured `d SPC` removes application candidates immediately and presents
  raw query input; `d`, unknown prefixes, other configured keys, case, and paste
  follow the rules above.
- [ ] `d SPC serendipity RET` calls the fake handler exactly once with
  `"serendipity"`, displays its result, and triggers no app launch/browser search.
- [ ] Multiword/Unicode input is preserved; blank submissions and typing/preview
  call no handler. Lookup errors preserve the word and never use web fallback.
- [ ] Back result→query preserves input without resubmission; query→picker and
  empty-query Backspace work. Escape/C-g cleanly exits in every state.
- [ ] Real result major mode, read-only content, copying/scrolling and ownership
  survive transitions. Preexisting result buffers and unrelated windows remain
  intact; delayed updates do not resurrect an obsolete view.
- [ ] Tool/bang conflicts and invalid registrations fail clearly before action;
  existing bang/queryless/refresh/app/fallback regressions still pass.
- [ ] A registered tool works without available apps/Spotlight; nil tools retain
  the previous default experience. No Portal dependency or resizing calls exist.
- [ ] Both entry points support the shared router, with normal-buffer results in
  ordinary Emacs; a Portal minibuffer-only host's inability to show results is
  documented, not silently advertised as supported.

## Verification and completion record

Add ERT state/dispatch tests plus native-input graphical transitions in a disposable
ordinary Emacs. Include failure, cancellation, buffer kill, repeated invocation,
custom reader/keymap, and an async-updating fake result. Record exact checks and
limits here; do not mark complete from a simulated callback-only test or from the
old Portal prototype. No live configuration edits or real network calls.
