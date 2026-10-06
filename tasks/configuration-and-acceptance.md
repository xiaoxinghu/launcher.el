---
id: launcher-configuration-acceptance
status: todo
depends_on:
  - launcher-apple-dictionary-example
---

# Adopt and verify the new launcher with the user's Emacs and Portal setup

## Problem and outcome

After the implementation tasks finish, a separate agent should exercise the real
standalone and panel workflows, then help the user opt into the buffer launcher.
This is the configuration/adoption task, not authorization to change a live config
while the features are still proposals. Keep the old minibuffer-only setup as a
working rollback/comparison option.

## Blocked by

- [Apple Dictionary example](apple-dictionary-example.md), which transitively
  requires full-buffer presentation and configurable routing.
- **External prerequisite gate:** sibling Portal repo task
  `tasks/launcher-panel-height-bounds.md` (`launcher-panel-height-bounds`).
- **External prerequisite gate:** sibling Portal repo task
  `tasks/launcher-buffer-content-fit.md` (`launcher-buffer-content-fit`).

The two Portal gates are not local `depends_on` entries: that task graph resolves
IDs within one repository. Read their actual status and completion evidence before
starting panel/config acceptance. A configuration snippet alone is not evidence.

## Existing configuration inspected on 2026-10-06

Source files under `~/.config/emacs/lisp/` are symlinks into
`~/.local/share/obento/macos/.config/emacs/lisp/`. Follow the managed source; do not
replace symlinks. Inspect again at execution time for user changes and instructions.

- `tools.el:167–208`: `use-package portal` loads `~/workspace/portal`, `:ensure nil`,
  `:demand t`. It configures terminal bindings and a Hydra; preserve those. It
  requires `portal-launcher` / `portal-global-shortcut`, then defines:

  ```elisp
  (defun my/app-launcher ()
    "Launch an app or search the web in Portal's panel.
  With a prefix argument, rebuild the application index first."
    (interactive)
    (let ((vertico-count 6))
      (portal-launcher-present #'launcher :kind 'minibuffer)))

  (portal-global-shortcut-set 'app-launcher "Command-Space"
                              #'my/app-launcher)
  ```

- `macos.el:58–61`: launcher.el is a local package, `:ensure nil`,
  `:load-path "/Users/xiaoxing/workspace/launcher.el/"`,
  `:commands (launcher launcher-refresh)`.
- `navigation.el`: Vertico and multiform are enabled, Orderless styles are
  `(orderless partial-completion basic)`, Marginalia is enabled, and
  vertico-directory binds Return/Delete/M-Delete. Multiform includes buffer/indexed
  imenu, buffer outline/grep, and grid file completion. Verify those still work
  outside Launcher; do not globally turn on vertico-buffer or replace the user's
  multiform rules. consult-omni and vertico-posframe are disabled.
- No launcher size/font/offset override was found in the inspected source.
  Preserve the user's theme/fonts (theme preferences include ef-bio/ef-frost),
  not the prototype's Menlo/Modus settings. The actual installed Vertico snapshot
  was `20260830.123`; verify the version at execution time.
- osx-dictionary was not found installed/configured in the inspected source;
  install/setup it only as part of the authorized adoption. Verify native CLI,
  compiler and dictionary availability before relying on it.
- `ai.el` already configures gptel/Claude and Org chat buffers. This task does not
  change AI credentials or add chat/translation; no API requests are needed.

These observations describe files, not unsaved buffers or live evaluated state.

## Proposed new setup (not executable on current main)

Confirm implemented names match these tasks before using this recipe. If names
change, update the plan, package docs, and tests together.

### Package and tool registration in `macos.el`

Extend the current local launcher declaration; do not add a second competing copy:

```elisp
(use-package osx-dictionary
  :ensure t
  :defer t)

(use-package launcher
  :ensure nil
  :load-path "/Users/xiaoxing/workspace/launcher.el/"
  :commands (launcher launcher-buffer launcher-refresh)
  :config
  (require 'launcher-osx-dictionary)
  (setq launcher-tools
        '(("d" :name "Dictionary" :prompt "Word: "
                :function launcher-osx-dictionary-lookup))))
```

`d` is a user choice. Changing it to `!d` or another unique key changes routing,
not adapter code. Do not bind it directly to `osx-dictionary-search-input`, which
reads another minibuffer rather than accepting the submitted query.

### Full-buffer panel in the existing Portal block in `tools.el`

```elisp
(defun my/app-launcher ()
  "Launch apps, search the web, or query tools in Portal's panel.
With a prefix argument, rebuild the application index first."
  (interactive)
  (let ((vertico-count 6)
        (portal-launcher-size '(720 auto))
        (portal-launcher-height-limits '(content 480)))
    (portal-launcher-present #'launcher-buffer
                             :kind 'buffer
                             :mode-line nil)))

(portal-global-shortcut-set 'app-launcher "Command-Space"
                            #'my/app-launcher)
```

This is just composition: Launcher runs its own interaction and returns on final
exit; Portal hosts it. Do not add `:completion`, a Portal Vertico adapter,
`:keep-open t`, native fitting calls, or caller-managed recursive edits. A maximum
of 480 logical points is an initial user-tunable choice, not a mandatory design.
At that limit, scroll inside the result; a very small screen imposes its own cap.
The normal frame still has its legal echo-area/padding floor, not zero height.

First try an alternate command name with M-x, without assigning another global
shortcut. Once accepted, re-evaluate the existing `app-launcher` named registration;
that replaces it rather than claiming the chord twice. Do not change terminal
shortcuts, the Portal Hydra, or any unrelated settings.

### Standalone operation

- `M-x launcher`: familiar minibuffer, with registered tool results displayed in
  ordinary Emacs buffers after query submission.
- `M-x launcher-buffer`: full-buffer interaction in the current Emacs frame,
  independent of Portal, with ordinary window sizing/scrolling.
- `C-u` still refreshes app discovery for either launcher entry point.

### Rollback/comparison: original minibuffer-only panel

```elisp
(defun my/app-launcher ()
  "Launch an app or search the web in Portal's minibuffer-only panel."
  (interactive)
  (let ((vertico-count 6)
        ;; A minibuffer-only host cannot show tool result buffers.
        ;; Disable tools only for this old app/search presentation.
        (launcher-tools nil))
    (portal-launcher-present #'launcher :kind 'minibuffer)))
```

This leaves tools available to standalone ordinary-Emacs commands but retains the
old panel's exact app/bang/fallback experience. The new size/height-limit bindings
are absent. Keep the same named Command–Space registration. Do not disable global
Vertico or remove package settings just to revert the presentation.

## Plan and acceptance

- [ ] Confirm all local dependencies and both external Portal gates are done,
  inspect actual public interfaces/revisions, and read config-repo instructions.
  Obtain/confirm permission before editing managed source or evaluating live config.
- [ ] Build/restart compatible Portal if native code changed; do not assume module
  hot-reload. Test disposable sessions first, not the user's running Emacs.
- [ ] Install/verify the existing dictionary package/helper and available Apple
  dictionaries. Record package revision and helper/compiler requirements without
  copying credentials or the user's full init into test environments.
- [ ] Run standalone ordinary-frame `launcher-buffer` with Portal absent: app
  selection, bang/web fallback, `d SPC`, raw multiword/Unicode input, Return lookup,
  real definition display, Back, correction/retry, and final quit.
- [ ] Verify the traditional `launcher` remains minibuffer-based; preserve refresh,
  selected candidate Return, spaces and existing search behavior. Stub external
  launches/browser actions in automated checks; distinguish any real smoke checks.
- [ ] Run the full-buffer flow through Portal using the completion/theme fixture:
  compact six→two→zero→six candidates, query state, a short real definition, and a
  long definition capped at 480 points with working scrolling/copying. For a
  deterministic cap test, supplement the real lookup with synthetic long content.
- [ ] Verify frame/native session stays the same across views, panel does not steal
  focus during updates, and max/min are respected. Real dictionary result keys
  do not restore foreign windows or open a duplicate prompt unintentionally.
- [ ] Verify Escape/C-g, repeated shortcut, click-away, error, Back, reentry, and
  rollback; no hooks/keymaps/window changes leak into the main Emacs workspace.
- [ ] Confirm multiform file grid, imenu/indexed, grep, Orderless, Marginalia and
  directory keys outside Launcher remain unchanged. Test ordinary main-frame
  scrolling and result updates independently of panel sizing.
- [ ] Capture old minibuffer-only, full-buffer picker, raw dictionary query, real
  result, capped-long/scrolled result, and shrink/regrow screenshots. Record exact
  versions/geometry and distinguish fake data from native dictionary evidence.
- [ ] After disposable acceptance, present config diff and rollback to the user;
  apply only the approved managed-source changes. Record actual manual acceptance
  (or explicitly pending), including physical keys/IME/accessibility where tested.

## Verification and completion record

Launcher ERT/byte compilation and Portal host checks precede adoption. Portal GUI
checks must use `bash test/vm.sh ...` as documented; copy a minimal completion/theme
fixture, never the whole init/credentials. Report failed attempts and reruns,
not only the final screenshot. Leave this task `todo` until the agreed acceptance
is actually performed; a draft configuration or prototype does not complete it.
