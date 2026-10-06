# Launcher implementation tasks

Planning only: every task starts at `status: todo`. Proposed names/configuration
are contracts for implementation, not features already available. Use one Markdown
file per task, stable kebab-case `id`, `status: todo|done`, and `depends_on` containing
local task IDs. Record actual acceptance and verification in the task when done.

## Ownership and order

Launcher owns application selection, tool routing, query/result UI, and ordinary
Emacs interaction. Portal is an optional host and owns native geometry/focus/lifetime.
The launcher must work in a normal Emacs window with Portal entirely absent.

1. [Full-buffer experience](full-buffer-experience.md) — `launcher-buffer`, sharing
   behavior with the existing minibuffer command.
2. [Configurable tool routing](configurable-tool-routing.md) — `d SPC` enters raw
   query input; Enter calls a configured function and shows its returned buffer.
3. [Apple Dictionary example](apple-dictionary-example.md) — optional adapter to
   existing osx-dictionary.el, with `d` chosen in configuration, not hardcoded.
4. [Configuration and end-to-end acceptance](configuration-and-acceptance.md) —
   reserved for a later agent, after implementation and Portal prerequisites.

Independent enhancement (can run in parallel with the sequence above):

- [Native app icons](native-app-icons.md) — native macOS extraction, asynchronous
  64/256/512-pixel caching, update-aware invalidation, and completion prefixes.
  Provides a shared icon interface for future larger previews; does not implement
  their layout or depend on the full-buffer/Portal tasks.

Portal's independent infrastructure tasks are at:

- `../portal/tasks/launcher-panel-height-bounds.md`
- `../portal/tasks/launcher-buffer-content-fit.md`

Those paths are relative to this repository's root. Cross-repository prerequisites
are explicit checklist gates in the final configuration task, not invalid local
`depends_on` IDs. The first three launcher tasks do not depend on Portal.

## Shared design decisions

- `M-x launcher` remains minibuffer-based; `C-u` still refreshes app discovery.
- `M-x launcher-buffer` is the new full-buffer entry point, also accepting `C-u`
  refresh. No presentation overload on the existing refresh prefix.
- The buffer entry point owns its interaction until an external action succeeds
  or the user quits. It can show/query tools and retain result interaction without
  returning early and requiring a host-specific keep-open operation. It does not
  assume a Portal panel, steal another frame, or resize an ordinary Emacs frame.
- Start with optional Vertico-backed buffer presentation; keep the existing
  minibuffer command usable without Vertico. No new completion engine.
- Proposed tool setting (default nil):

  ```elisp
  (setq launcher-tools
        '(("d" :name "Dictionary" :prompt "Word: "
                :function launcher-osx-dictionary-lookup)))
  ```

  A handler accepts one query string and returns a live result buffer. The
  dictionary adapter has this interface; the underlying package's interactive
  command does not. Tool handlers do not launch another prompt or decide where
  their result appears. Unsupported return values fail visibly, never becoming
  browser searches. Future handlers may return a buffer immediately and update it
  later; a general promise/stream protocol and actual LLM integration are out of
  scope for this first implementation.
- Launcher displays and navigates results; it does not rewrite the dictionary
  backend. Third-party major modes, faces, read-only behavior and buffer ownership
  must survive presentation.
- Only submission triggers lookup. Never send a query while typing, narrowing, or
  previewing. Preserve existing web bangs and fallback semantics.
- Keep ordinary buffer scrolling, copying, and selection. If a result grows,
  preserve the reader's position unless they were following the end; never focus
  or reopen an obsolete result from a late update.
- Portal alone may bound/resize the native panel. Its proposed generic fitting
  measures rendered windows without knowing Launcher, Vertico, prefixes, or tools.
  No Portal-specific fitting calls belong in launcher.el.

## Evidence and superseded plans

Inspected launcher baseline: `655c462`. The proof of concept is Portal commit
`9005099`, [draft PR #91](https://github.com/xiaoxinghu/portal/pull/91), with
[screenshots and findings](https://github.com/xiaoxinghu/portal/blob/9005099/docs/launcher-buffer-prototype.md).
It proved feasibility, not production completion. The result was simulated, not
an actual dictionary or gptel response.

The earlier uncommitted Portal-owned completion-adapter plan in
`portal.worktrees/buffer-picker-plan/tasks/buffer-picker/` is superseded and removed.
Do not implement `:completion 'vertico-buffer`, `portal-launcher-vertico.el`, or
`portal-launcher-continue-in-buffer` from that abandoned plan. Keep the prototype
as historical evidence; do not promote its private-function replacements.
