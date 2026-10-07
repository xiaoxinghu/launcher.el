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

- [x] Confirm all local dependencies and both external Portal gates are done,
  inspect actual public interfaces/revisions, and read config-repo instructions.
  Obtain/confirm permission before editing managed source or evaluating live config.
- [x] Build/restart compatible Portal if native code changed; do not assume module
  hot-reload. Test disposable sessions first, not the user's running Emacs.
- [x] Install/verify the existing dictionary package/helper and available Apple
  dictionaries. Record package revision and helper/compiler requirements without
  copying credentials or the user's full init into test environments.
- [x] Run standalone ordinary-frame `launcher-buffer` with Portal absent: app
  selection, bang/web fallback, `d SPC`, raw multiword/Unicode input, Return lookup,
  real definition display, Back, correction/retry, and final quit.
- [x] Verify the traditional `launcher` remains minibuffer-based; preserve refresh,
  selected candidate Return, spaces and existing search behavior. Stub external
  launches/browser actions in automated checks; distinguish any real smoke checks.
- [x] Run the full-buffer flow through Portal using the completion/theme fixture:
  compact six→two→zero→six candidates, query state, a short real definition, and a
  long definition capped at 480 points with working scrolling/copying. For a
  deterministic cap test, supplement the real lookup with synthetic long content.
- [x] Verify frame/native session stays the same across views, panel does not steal
  focus during updates, and max/min are respected. Real dictionary result keys
  do not restore foreign windows or open a duplicate prompt unintentionally.
- [x] Verify Escape/C-g, repeated shortcut, click-away, error, Back, reentry, and
  rollback; no hooks/keymaps/window changes leak into the main Emacs workspace.
- [x] Confirm multiform file grid, imenu/indexed, grep, Orderless, Marginalia and
  directory keys outside Launcher remain unchanged. Test ordinary main-frame
  scrolling and result updates independently of panel sizing.
- [x] Capture old minibuffer-only, full-buffer picker, raw dictionary query, real
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

### Disposable acceptance record (2026-10-07)

Branch `task/configuration-acceptance`, base `4a7a408`. No launcher.el
product code changed; only checks were added. **Pending:** the user's
approval of the config diff below (since given, and applied), and a
manual check on the real machine (physical keys, IME, VoiceOver, the user's
own display). Keep `status: todo` until those are recorded.

Gates: local `launcher-apple-dictionary-example` is done; Portal
`launcher-panel-height-bounds` and `launcher-buffer-content-fit` are `done`
with completion records, at Portal `e8c1d8d` (clean apart from an untracked
`tasks.org`). Public interface used, as documented in `portal-launcher.el`:
`portal-launcher-present` with `:kind 'buffer :mode-line nil`,
`portal-launcher-size '(720 auto)`, `portal-launcher-height-limits '(content
480)`, `portal-global-shortcut-set`. Config repo instructions
(`~/.local/share/obento/CLAUDE.md`): keep code minimal and idempotent.

Config re-inspected at execution: the 2026-10-06 notes hold (the launcher and
Portal blocks are the user's own uncommitted edits in `macos.el` and
`tools.el`), with one correction: `ef-themes` is `:disabled`; the active themes
are `modus-operandi` / `modus-vivendi-tinted`, switched on system appearance,
and the font is the first of JetBrainsMono Nerd Font-17, Hack Nerd Font-17,
Fira Code-17. Installed Vertico is still `20260830.123`. osx-dictionary is
not installed; MELPA's current build is commit `655bca5`, the tested pin,
and `basic.el` sets `use-package-always-ensure` and `-defer`, so
`(use-package osx-dictionary)` alone installs it lazily. Host: macOS 27.0.1,
Apple clang 21.0.0, Xcode at `/Applications/Xcode.app/Contents/Developer`.

New checks (GUI only, VM): `test/launcher-portal-gui-tests.el`, run by
`bash test/portal-vm.sh`, which copies this checkout to the VM and runs
`test/portal-gui.sh` through Portal's own `test/vm.sh` (a fresh Portal build,
its desktop lock, `emacs -Q --module-assertions`). Fixture
`test/portal-acceptance-config.el`: only the user's completion/theme setup
(navigation.el, ui.el) and the proposed launcher/Portal config, verbatim, with
the rollback as `my/app-launcher-minibuffer`; no init, services or
credentials. Real system apps and icons, stubbed launch/browse, real Apple
Dictionary (pinned package copy, helper built by the first lookup), keys
through AppKit to the panel; actions between keys go through a native C-].

| Check | Result |
| --- | --- |
| Batch ERT, `-L . -L test`, all six batch files (host) | 80/80 passed |
| Byte compilation, package files with `byte-compile-error-on-warn`; new test files against Portal | no warnings |
| `bash test/vm.sh` (standalone GUI, Portal absent) | 19/19 passed; [log](evidence/launcher-configuration-acceptance/standalone-gui-tests.log) |
| `bash test/vm.sh sh test/gui.sh '^launcher-dictionary-real-'`, run 1 | 2/3: `launcher-dictionary-real-buffer-flow` failed; [log](evidence/launcher-configuration-acceptance/dictionary-real-run1-failed.log) |
| The same, run 2, unchanged code | 3/3 passed; [log](evidence/launcher-configuration-acceptance/dictionary-real-run2.log) |
| `bash test/portal-vm.sh` (Portal-hosted, final) | 4/4 passed in 28 s; [log](evidence/launcher-configuration-acceptance/portal-gui-tests.log) |

The run-1 failure is a flaw in the standalone harness, not the product: its
driver runs snapshot actions from a timer 0.15 s after posting the previous
native key, so the snapshot after `s` ran before AppKit delivered `s`; every
later state in that run was correct (the query held "hello", then "qwxzv"
with its notice). Not fixed here; routing actions through a key, as the
Portal checks do, would remove the race.

Earlier Portal-hosted attempts failed on harness bugs, each fixed before the
final run: the environment log called a Portal module function before the
module loads; `portal-gui.sh` read its selector after `set --`; a docstring
lost its quote, and the graphical Emacs waited with the load error shown for
the whole 1500 s limit (loading is now guarded and the log streams, with a
watchdog); wrong expectations (Portal returns normally on Escape/C-g; the
panel is key without Emacs being the frontmost app; the cap on a small
screen); `this-command` unbound when calling `consult-imenu`; the fixture
lacked the Vertico extension autoloads that package.el gives the user; and
C-] cleared the minibuffer error message before it was captured.

VM environment (from the log): macOS 27.0.1, GNU Emacs 31.1 NS
(`a360712c`), Portal `e8c1d8d`, launcher.el `4a7a408` plus these checks,
osx-dictionary `655bca5`, Vertico 2.15, theme `modus-operandi`, font Menlo
(the user's fonts are not installed in the VM), one "Apple Virtual" display,
usable area `(0 74 1326 742)` points, scale 2.

Observed, all in one panel, one native session, keyboard focus kept, top-left
corner and 720-point width fixed, Emacs frame matching the native height:

- Picker six → two → none → six: 180, 102, 102, 180 points. Two and none are
  equal: Portal's legal height includes `window-min-height` (4 lines), so the
  panel never goes below 102 points here; the query view is also 102.
- `d SPC` shows "Dictionary — Word: ". Real `hello`: 200 points, all of it
  shown. Real `set`: capped at 422 points, not 480, because on this small
  display only 422 points lie below the panel's top edge (Portal's rule); it
  scrolled with C-v at that height, and M-w copied two lines. A synthetic
  result with the panel raised 200 points showed the 480 cap exactly: 116 →
  480 as a timer added lines, the reader's scrolled position kept as more were
  added, 102 when cut to two lines, 480 again.
- Back (C-c C-b) from a result keeps the query; dictionary `q` returns to the
  query with "set"; deleting back from an empty query returns to the picker
  with "d". No `*osx-dictionary*` buffer, saved window configuration, extra
  window or frame.
- Twelve endings, presented one after another: Escape and C-g in picker,
  query and result, Back to the picker, a failing tool (its message shown
  inline), the shortcut pressed again, focus moved to the ordinary frame, an
  app, a bang and the fallback search. Each hid the panel and left
  `emulation-mode-map-alists`, hooks, display rules, the global map, Vertico
  defaults, the ordinary window and the ordinary frame's size and position as
  they were.
- Rollback `my/app-launcher-minibuffer`: a minibuffer panel; "calc" launched
  Calculator and "d hello" searched Google, as today (no tools).
- Outside Launcher afterwards: `read-file-name` in the grid, `consult-imenu`
  in a buffer with indices, `consult-grep` in a buffer, vertico-directory's
  Return and Delete, Orderless styles and Marginalia all unchanged.

Screenshots (VM, the panel's Emacs drawing): picker
[six](evidence/launcher-configuration-acceptance/60-panel-picker.png),
[two](evidence/launcher-configuration-acceptance/61-panel-two.png),
[none](evidence/launcher-configuration-acceptance/62-panel-none.png),
[six again](evidence/launcher-configuration-acceptance/63-panel-six-again.png);
[query](evidence/launcher-configuration-acceptance/64-panel-query.png);
real [hello](evidence/launcher-configuration-acceptance/65-panel-hello.png),
[Back](evidence/launcher-configuration-acceptance/66-panel-back.png),
[long](evidence/launcher-configuration-acceptance/67-panel-long.png),
[scrolled](evidence/launcher-configuration-acceptance/68-panel-scrolled.png),
[q](evidence/launcher-configuration-acceptance/69-panel-dictionary-q.png),
[back to picker](evidence/launcher-configuration-acceptance/70-panel-back-to-picker.png);
synthetic [short](evidence/launcher-configuration-acceptance/71-panel-short.png),
[capped](evidence/launcher-configuration-acceptance/72-panel-capped.png),
[reader](evidence/launcher-configuration-acceptance/73-panel-reader.png),
[shrunk](evidence/launcher-configuration-acceptance/74-panel-shrunk.png),
[regrown](evidence/launcher-configuration-acceptance/75-panel-regrown.png),
[error](evidence/launcher-configuration-acceptance/76-panel-error.png);
[old minibuffer panel](evidence/launcher-configuration-acceptance/77-panel-old-minibuffer.png).

Not tested: the user's own display, fonts and dark theme; Command-Space itself
(the checks register `app-launcher` on a test chord; Spotlight owns
Command-Space in the VM); physical keys, IME and VoiceOver; real app launches
and browsing from the buffer panel (stubbed; Portal's recipe check covers
real `open` for the minibuffer panel); `C-u` refresh through Portal (covered
standalone).

Proposed config diff, pending approval (on top of the user's uncommitted
edits, managed source under `~/.local/share/obento/macos/.config/emacs/lisp/`):

```diff
--- macos.el
+(use-package osx-dictionary)
+
 (use-package launcher
   :ensure nil
   :load-path "/Users/xiaoxing/workspace/launcher.el/"
-  :commands (launcher launcher-refresh))
+  :commands (launcher launcher-buffer launcher-refresh)
+  :config
+  (require 'launcher-osx-dictionary)
+  (setq launcher-tools
+        '(("d" :name "Dictionary" :prompt "Word: "
+                :function launcher-osx-dictionary-lookup))))
--- tools.el
   (defun my/app-launcher ()
-    "Launch an app or search the web in Portal's panel.
+    "Launch apps, search the web, or query tools in Portal's panel.
 With a prefix argument, rebuild the application index first."
     (interactive)
-    (let ((vertico-count 6))
-      (portal-launcher-present #'launcher :kind 'minibuffer)))
+    (let ((vertico-count 6)
+          (portal-launcher-size '(720 auto))
+          (portal-launcher-height-limits '(content 480)))
+      (portal-launcher-present #'launcher-buffer
+                               :kind 'buffer
+                               :mode-line nil)))
```

Rollback: restore the old `my/app-launcher` body, binding `(launcher-tools
nil)` as in "Rollback/comparison" above, and re-evaluate it; the same
`app-launcher` registration then replaces the shortcut.

Applied 2026-10-07 with the user's approval: exactly this diff, in the managed
source files (not the symlinks), on top of the user's uncommitted edits, which
are kept; both files parse. Not committed in the config repo and not evaluated
in the running Emacs: osx-dictionary installs from MELPA on the next restart
(or `M-x package-install`), and the helper builds on the first lookup.
**Still pending:** the user's manual check on the real machine, after which
this task can be marked done.
