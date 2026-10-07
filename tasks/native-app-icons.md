---
id: launcher-native-app-icons
status: done
depends_on: []
---

# Native macOS app icons with a multi-resolution, update-aware cache

## Outcome and scope

Show the installed application's native icon before its completion name, without
slowing typing or changing matching/launch behavior. Support the same icon at
larger sizes in a future main-buffer preview above the minibuffer.

Read [shared decisions](README.md) and
[native icon research](../docs/native-app-icons-research.md), including its tested
JXA extraction sketch and upstream source links. Inspected baseline: `77972eb`.
All names below are proposed implementation contracts, not existing features.

This task implements the icon backend, minibuffer integration, and a standalone
large-image verification fixture. It does **not** implement a selected-app preview
layout, full-buffer picker, Portal integration, frame resizing, or a new completion
engine. It can ship independently of `launcher-full-buffer`; that task and future
preview views should reuse this backend rather than create another extractor.

## Agreed behavior

- Resolve icons with `NSWorkspace.iconForFile:` through macOS's built-in
  `/usr/bin/osascript -l JavaScript` and its AppKit bridge. No Swift compilation,
  Node, Python/PyObjC, Homebrew package, or icon font is required.
- Cache **64×64**, **256×256**, and **512×512** PNGs. Warm 64-pixel assets in the
  background; generate 256/512-pixel assets only when requested.
- At 2× display scaling, these accommodate list icons at 16–32 logical pixels,
  previews at 64–128 logical pixels, and larger previews at 256 logical pixels.
  Raster resolution and displayed dimensions are separate concepts.
- Render each resolution from the native NSImage, not by enlarging a smaller PNG.
  AppKit can choose an appropriate representation where the application supplies
  one; it cannot guarantee detail absent from the source icon.
- Return a cached image immediately, or a smaller current-generation image/nil
  while the requested asset is queued. Never wait for extraction during redisplay.
- An icon error must not remove an app, prevent launching, or trigger web fallback.
- Terminal/image-incapable frames and disabled icons retain the current text-only
  behavior, without empty icon columns or extraction work initiated by that view.

## Files and interface

Keep the implementation separate enough for both current and future views:

- `launcher-icons.el`: options, cache state, scheduling, process protocol, image
  creation, invalidation, and command `launcher-clear-icon-cache`.
- `assets/launcher-icons.js`: the packaged JXA worker, resolved relative to the
  installed Lisp library, not `default-directory`. Document/package this asset.
- `launcher.el`: small index/session lifecycle calls and an affixation function.
- `test/launcher-icons-tests.el`: deterministic ERT tests using temporary caches
  and mocked process/clock/image capabilities; no host application dependencies.

Proposed options, all in group `launcher`:

| Option | Default | Contract |
| --- | --- | --- |
| `launcher-show-icons` | `t` | Effective only on supported graphical frames |
| `launcher-icon-size` | `20` | List display size in logical pixels; positive integer |
| `launcher-icon-cache-directory` | `(locate-user-emacs-file "cache/launcher/icons/")` | Package-owned persistent cache root |
| `launcher-icon-check-interval` | `300` | Positive freshness interval in seconds |

Use internal constants for raster buckets `(64 256 512)`, cache format version,
batch limit (start at 32 app requests), and worker timeout (start at 30 seconds).
Do not expose tuning options for every internal implementation detail.

The common caller interface is:

```elisp
(launcher--icon app-path display-size &optional frame)
;; => Emacs image specification or nil; never waits for external work.
```

- `frame` defaults to the selected frame. Hide raster selection, current-generation
  disk/memory lookup, missing-request deduplication, and scheduling behind this
  interface. Calls may queue work but must not synchronously start/wait for one
  subprocess per row or scan app metadata.
- Choose the smallest bucket meeting the view's display-size/backing-scale budget;
  cap at 512 and document that larger views may upscale. Verify how logical size,
  Emacs image `:width`/`:height`/`:scale`, and Retina backing pixels interact in the
  supported builds. Do not assume multiplying both raster and displayed width by
  two is correct. Prefer oversampling to a blurry fallback when scale is unknown.
- Cache image specifications by app generation, raster bucket, display size, and
  relevant frame scaling characteristics, not just app name. Same-name apps at
  different paths must have separate icons.
- Define `launcher-icon-updated-hook`, called with the app path after a current
  generation becomes available or is invalidated. Isolate listener errors from
  worker/cache processing. Listeners must validate their own live session and
  selected app before touching UI. No hook may focus or recreate a closed view.
  This gives a future preview a notification mechanism without coupling extraction
  to Vertico or retaining dead buffers in per-request callbacks.

Define/register the customization group before requiring the icon module; loading
`launcher` in batch mode or on non-macOS must not launch tools or fail. Optional UI
packages must not be required by this module.

## Native worker and async protocol

1. Use `make-process` with an argument list, not a shell command. Write a temporary
   JSON request manifest and pass its absolute filename to the packaged JXA script.
   Pass app/output paths as data, never interpolated executable JavaScript.
2. A manifest contains a protocol version and bounded requests with request ID,
   absolute app path, requested raster buckets, expected fingerprint when known,
   and output paths inside a unique job directory under the cache root. The parent
   maintains the corresponding cache epoch/app-generation tokens.
3. Import AppKit/Foundation once per batch. Load app metadata and obtain
   `NSWorkspace.sharedWorkspace.iconForFile(path)` once per app needing images.
   Draw each requested size into an explicitly sized RGBA `NSBitmapImageRep`.
   Use `NSGraphicsContext.setCurrentContext(...)`, with save/restore in `try/finally`;
   property assignment produced blank output in the research prototype.
4. Encode PNG using `representationUsingTypeProperties`, writing atomically to
   job-local files only. The worker never writes published cache paths. Check file
   existence before lookup: AppKit can supply a generic icon for a missing path.
5. Emit one JSON result per app (JSON Lines), including request ID, source
   fingerprint, raster sizes actually written, and a structured error if needed.
   Keep stderr separate. Handle per-app failures independently. In Emacs, buffer
   partial output lines; reject malformed, oversized, or unexpected records and
   accept only outputs corresponding to that job's requested paths/sizes.
6. Support metadata-only checks in the same worker so freshness checks do not
   produce PNGs or decode every icon. Use one active worker plus a deduplicated
   queue. Prioritize explicitly requested previews over remaining warm-up batches;
   do not introduce concurrent workers merely to preload all icons.
7. Enforce timeout/exit cleanup, including manifests, staging files, process
   buffers, and timers. Bound diagnostic output and log failures without messaging
   once per candidate. Cache failures per app generation/requested size until the
   next freshness interval; fingerprint changes and explicit reset permit retry.
8. If the bundle changes between metadata reads before and after extraction,
   discard that app's outputs and enqueue a fresh check. Normal app replacement
   must not publish an image under an old fingerprint.

Warm-up/check work is global cache maintenance, not owned by a minibuffer. A bounded
active job may finish after quit to populate the cache, but all UI hooks are scoped
and removed on unwind. Do not leave recurring polling timers running while idle.

## Persistent cache and generations

Suggested owned layout:

```text
<cache-root>/v1/
  <sha256-of-normalized-absolute-app-path>/
    metadata.json
    <generation-id>/
      64.png
      256.png
      512.png
  jobs/<unique-job-id>/...
```

`metadata.json` identifies the original path, fingerprint, active generation,
format version, and available raster buckets. Normalize/expand paths consistently
without using display labels or bundle IDs as keys. A generation ID must distinguish
explicit resets even if the source fingerprint is unchanged. Treat metadata as
untrusted data: JSON only, never `load`/`eval` it, and validate schema/path ownership.

Maintain a memory record for each app: fingerprint, active generation, available
rasters, last successful check time, pending work, and retry deadline. Disk cache
hits should avoid re-extraction across Emacs sessions. Missing/corrupt PNG or
metadata is a cache miss, not an error escaping into completion.

Publish completed files and the updated manifest with atomic renames within the
cache filesystem; publish the manifest only after files are ready. Unique generation
paths avoid reusing an Emacs image-cache filename for different pixels. Evict old
in-memory image specifications and flush only package-owned decoded images where
needed; do not globally clear unrelated Emacs images. Multiple Emacs processes may
share the cache: use unique job/generation paths, tolerate atomic manifest races,
and never delete another process's live job as part of routine cleanup.

## Fingerprint and invalidation policy

### Source fingerprint

Use a deterministic, versioned, ordered structure and hash its stable encoding.
Include available metadata for:

- App bundle directory: modification time and filesystem identity where available.
- `Contents/Info.plist`: modification time and size, plus `CFBundleVersion` and
  `CFBundleShortVersionString` from the plist already parsed by the worker.
- The resolved `CFBundleIconFile` resource (adding `.icns` when appropriate), and
  `Contents/Resources/Assets.car`: modification time and size.
- Explicit missing markers for expected resources, so creation/removal changes the
  fingerprint. Distinguish an absent optional resource from a transient read error;
  read errors are failed checks, not evidence that the app was removed.

Do not recursively hash the application or spawn `defaults`/`sips` per row. Handle
binary plists with Foundation in the worker. These resource probes are freshness
hints, not the icon resolver: AppKit remains authoritative for extraction even if
`CFBundleIconFile` is absent. Bundle timestamps alone do not detect every nested
resource update.

Metadata fingerprints are intentionally best-effort. Preserved timestamps,
Finder custom icons, indirect resources, and appearance-dependent icons may evade
them. Document this and retain manual reset; no claim of perfect change detection.
No filesystem watchers, full-content hashing, or automatic appearance listener in
this initial task.

### When to check

| Event | Required behavior |
| --- | --- |
| First icon-enabled launcher invocation | Load usable disk records immediately; queue freshness checks and missing 64px assets |
| Successful app index refresh (`launcher-refresh`, including `C-u`) | Check all indexed apps asynchronously, bypassing freshness throttle; preserve unchanged assets |
| Normal icon-enabled launcher invocation | If last successful checks are at least 300 seconds old, queue throttled checks; otherwise reuse cache |
| Large icon request | Queue a check first if that app's freshness interval elapsed; use a cached provisional image without waiting |
| Fingerprint changed | Invalidate **all raster tiers**, memory images, and failed-lookup entries for that app together; warm 64px and regenerate requested larger tiers |
| Missing app / removed from refreshed index | Retire that app's record and ignore pending results; clean owned obsolete assets after authoritative successful discovery |
| Cache format/renderer semantics changed | Increment cache version; never silently reuse incompatible assets |

Freshness is checked on these events, not by a permanent five-minute timer. An idle
open list need not update spontaneously. An expired check can provisionally display
an existing icon until change is confirmed; once invalidated, display a placeholder
until a current-generation image exists rather than claiming the old one is fresh.
A failed Spotlight refresh must not purge the cache. Age-out cleanup of orphaned
generations/jobs should be conservative and confined to package-owned paths.

### Manual reset

Add autoloaded `M-x launcher-clear-icon-cache`:

- Force a new cache epoch, cancel/retire this process's queued/in-flight requests,
  clear memory/negative caches, and invalidate published icon records even when
  fingerprints are identical. Invalidate package-owned Emacs image cache entries.
- Remove only validated package-owned cache artifacts, never recursively delete an
  arbitrary configured directory. Preserve the app discovery index and user files.
- Warm the current app index asynchronously when graphical icon display is enabled;
  otherwise regeneration waits until the next eligible invocation.
- Keep `C-u launcher` as an app-index refresh plus fingerprint recheck, **not** an
  unconditional cache purge. Document the separate reset command.

A late worker result must carry a still-current parent epoch, app generation, and
request identity before publication. Results from before reset/update/removal must
be discarded and must not restore deleted cache entries or repaint an obsolete UI.
This generation check also applies to callbacks/queued notifications, not only PNGs.

## Completion integration

1. Keep candidates exactly as they are. Add `launcher--affixation`, returning
   `(candidate prefix suffix)` triples in input order; suffix is the existing
   `launcher--annotation` result or an empty string.
2. Set command-local `completion-extra-properties` to this affixation function
   when supported icons are enabled. Attach the image with a `display` property on
   a prefix placeholder, followed by a small gap. Use consistent prefix width for
   apps still loading and bang entries; bangs do not initiate app-icon requests.
3. Lookup by candidate-to-path mapping, so duplicate app names remain correct.
   Do not add icons to the candidate string, alter matching/history, or change the
   no-match web-search and bang dispatch logic.
4. Do not shell out, traverse bundle files, or run a synchronous freshness scan in
   the affixation callback. It may inspect memory/cache state and enqueue work.
5. Default completion and Vertico must remain usable. Use the standard affixation
   contract, not global advice/replacement of Vertico internals. Cached results may
   become visible on the next natural frontend redraw; automatic idle list refresh
   is not required in this task. The future preview can use the update hook.
6. Preserve text-only reader behavior and existing Space/custom-reader tests. Test
   Marginalia/nerd-icons coexistence; prevent a redundant generic launcher icon
   through category-local integration where needed, never disabling their global
   modes. If a frontend ignores affixation, degrade to usable text.
7. Do not select windows, resize frames, or introduce Portal-specific behavior.

## Implementation order

1. Add pure resolution-selection/fingerprint/cache-state tests and the cache module.
2. Implement the JXA manifest protocol and isolated macOS extraction smoke fixture.
3. Add scheduler, failure handling, disk publication, generation protection, and
   freshness/reset paths. Prove stale-worker rejection before wiring into the UI.
4. Connect successful app refresh/session entry and completion affixation.
5. Add an ordinary-buffer fixture using `launcher--icon` at multiple sizes and
   the update hook. Verify that a killed/changed preview is not repainted; this is
   test scaffolding, not the final preview feature.
6. Document configuration, cache location, maintenance command, dependency-free
   macOS extraction, graceful fallback, and known freshness/Retina limitations.

## Acceptance and verification

- [x] Existing input/launch/search tests pass unchanged in meaning.
- [x] List shows actual installed app icons, not generic font glyphs; duplicate
  names, Unicode, spaces, apostrophes, and shell metacharacters in paths work.
- [x] Resolution tests cover bucket edges and 1×/2× budgets. List warm-up produces
  only 64px assets; requesting a preview lazily produces 256/512px as appropriate.
- [x] Same app at small/large sizes shares freshness/generation state, not a
  separately implemented cache. Larger PNGs are rendered from native source data.
- [x] A disk hit survives a fresh Emacs process without unnecessary extraction;
  fresh repeated lookups spawn no workers. Typing never waits for external work.
- [x] Fingerprint tests cover unchanged metadata, bundle replacement, version
  change, icon/Assets.car mtime/size change, missing-to-present resource, and nested
  icon update without top-level bundle timestamp change. No whole-app hashing.
- [x] All resolutions and memory/negative caches invalidate together. Check
  throttling uses a fake clock; explicit refresh bypasses it; manual reset works
  despite unchanged source metadata.
- [x] Delayed results after update/reset/removal, malformed/partial JSON, corrupt
  PNGs, worker failure/timeout, unwritable cache, and per-app extraction failure
  cannot break the launcher or publish an obsolete generation. Remaining apps work.
- [x] Cache cleanup never deletes arbitrary files, another process's live staging
  directory, or valid entries merely because discovery failed.
- [x] No worker launched by terminal/disabled/unsupported-image display. No leaked
  UI hooks or recurring polling timers after quit/error; cache-only work may finish
  without focusing or reopening a buffer.
- [x] Graphical stock completion and Vertico render aligned, selected-row-safe
  small icons. A separate buffer displays the same app at 128 and 256 logical pixels
  without unintended row inflation or visibly unnecessary upscaling on Retina.
- [x] Missing/incompatible completion add-ons do not prevent package loading;
  Marginalia/nerd-icons coexistence is tested or explicitly recorded as unverified.

Run existing and new ERT tests, byte-compile changed Lisp, and check whitespace:

```sh
emacs --batch -Q -L . \
  -l test/launcher-tests.el -l test/launcher-icons-tests.el \
  -f ert-run-tests-batch-and-exit
emacs --batch -Q -L . -f batch-byte-compile launcher-icons.el launcher.el
git diff --check
```

Use temp directories and mocks for ERT; it must run without AppKit on non-macOS CI.
Run the real JXA smoke fixture separately on macOS: verify requested dimensions,
nonempty image content (not just PNG validity), app-specific appearance, and batch
failure isolation. Record first/repeated extraction timings and warm-cache launch
behavior without turning the research timings into performance guarantees.

For graphical checks use the established macOS VM runner, not the user's live host
Emacs; test an ordinary frame with Portal absent. Record macOS/Emacs/Vertico versions,
backing scale, screenshots/commands, results, and skipped checks here before setting
`status: done`. No live user configuration changes in this task.

## Completion record

Implemented on branch `task/native-app-icons` from `df56bcd`, verified 2026-10-07.
A first implementation followed the plan above in full (about 2,800 lines,
with generations, epochs, a JSON protocol and 256/512 px tiers). Review
judged most of it out of proportion to a 20 px list icon, and it was
replaced by the design below.

### What shipped

- `launcher-icons.el` (~180 lines): `launcher-show-icons`, `launcher-icon-size`,
  `launcher-icon-cache-directory`, `launcher-icons-prepare`,
  `launcher-icons-prefix`, `launcher-icons-index-refreshed` and autoloaded
  `launcher-clear-icon-cache`.
- `assets/launcher-icons.js` (~45 lines): `osascript -l JavaScript launcher-icons.js
  PIXELS APP PNG [APP PNG]...` draws each app's `NSWorkspace iconForFile:` into a
  PNG written atomically, and prints one line per failed app.
- `launcher.el`: requires the module after `defgroup`, calls
  `launcher-icons-index-refreshed` after a successful `launcher-refresh` (errors
  only messaged), and `launcher--completion-properties` / `launcher--affixation`.
  With icons on, the completion category is `launcher-app`, so
  nerd-icons-completion adds no generic glyph (found by the coexistence test).
- Tests: `test/launcher-icons-tests.el` (12, fake worker, no macOS needed),
  `test/launcher-icons-worker-tests.el` (4, macOS only, real worker),
  `test/launcher-icons-gui-tests.el` (3, VM), `test/png-stats.js`.
  `test/elpa.sh` pins nerd-icons.el `17faac7` and nerd-icons-completion `f924dd4`.

### Design

- One raster: 64×64 PNGs, sharp up to 32 logical pixels at 2×. Image specs use
  `:width`/`:height` of `launcher-icon-size` and `:scale 1`.
- A PNG is named `sha1(pixels, app path, Info.plist mtime)` (the bundle's mtime
  without an Info.plist). An updated app gets a new name, so there is no
  fingerprint, metadata file, generation or stale-result check: a late worker
  result writes the right file under the right name.
- Each launcher start stats every indexed app once (`launcher-icons-prepare`) and
  starts one background worker for all missing PNGs, unless one runs. A worker
  older than 60 s is stopped at the next start. Rows check `file-exists-p`, so
  icons appear on the next redraw as the worker writes them.
- A PNG asked for once is not asked for again in this Emacs until the index is
  refreshed, so a failing app does not start a worker on every launcher.
- `launcher-refresh` deletes PNGs of apps no longer indexed (none if the index is
  empty). `launcher-clear-icon-cache` stops the worker and deletes only files
  matching the PNG name pattern, flushing each from Emacs's image cache.

### Dropped from the plan

- 256/512 px tiers, `launcher--icon` at arbitrary sizes, and
  `launcher-icon-updated-hook`: no planned view uses them.
- `launcher-icon-check-interval` and throttled freshness checks: a stat per app
  per launcher start replaces them.
- Fingerprints of `CFBundleIconFile`, `Assets.car`, versions and inodes, and
  detection of changes during drawing. An icon change that leaves Info.plist's
  mtime alone goes unnoticed until `launcher-clear-icon-cache`.
- Manifest, JSON Lines protocol, per-request validation, batch limit, priority
  queues, negative-cache deadlines, cross-process job sweeping.

### Verification

Host: macOS 27.0.1 (26A434), Emacs 31.1 (Homebrew CLI, batch).

```sh
sh test/elpa.sh
emacs --batch -Q -L . -L test -l test/launcher-tests.el -l test/launcher-buffer-tests.el \
  -l test/launcher-tools-tests.el -l test/launcher-osx-dictionary-tests.el \
  -l test/launcher-icons-tests.el -l test/launcher-icons-worker-tests.el \
  -f ert-run-tests-batch-and-exit        # 77 tests, 77 as expected
emacs --batch -Q -L . -f batch-byte-compile launcher-icons.el launcher.el \
  launcher-buffer.el launcher-osx-dictionary.el   # no warnings
git diff --check                          # clean
```

Real worker (host): Calculator and Notes at 64 px in 0.096 s; 64×64 PNGs, 65 %
of sampled pixels opaque, 66 and 38 quantized colors, different from each other.
A missing app between real ones printed `…: no such application` and the others
were written. A fake bundle named `It's $(touch pwned) \`touch pwned2\` ; "q" 名前 é.app`
rendered and created no file. All 503 apps indexed on the host: one worker,
503 PNGs, 3.6 MB, 5.9 s. These are single local measurements.

GUI, in the VM (`bash test/vm.sh sh test/gui.sh`, Emacs 31.1 NS build, macOS
27.0.1, Vertico 2.15, Orderless 1.8, Marginalia 2.13, `emacs -Q`, no Portal,
backing scale 2.0, default line height 14 px, temporary icon cache): all 19
graphical checks pass (run `launcher.MPmJ11Z3`), including the 3 icon checks:

- Vertico + Marginalia: one native icon per app row, none on bang rows, equal
  prefix widths (27 px), candidate rows all 20 px, selected row highlighted; two
  apps named Notes have distinct icons; Return launched the selected fake Notes.
- Stock completion (`*Completions*`, one column): one icon per row for 5 apps.
- `launcher-buffer`: one icon per row.

Screenshots (git-ignored, `.cache/vm/launcher.MPmJ11Z3/screenshots/`):
`icons-vertico.png`, `icons-vertico-notes.png`, `icons-stock-completion.png`,
`icons-launcher-buffer.png`. Inspected visually: sharp icons, aligned names.

The shared GUI fixture binds `launcher-show-icons` to nil: its fake `/Applications`
paths have no icons, and it asserts that no `launcher-*` timer outlives a check.

### Not verified

- A 1× (non-Retina) display.
- nerd-icons-completion in a graphical frame: coexistence is tested in batch
  (Marginalia + nerd-icons-completion modes on, real advice), not on screen.
- Two Emacs processes sharing a cache (content-addressed names and atomic
  writes make this benign by construction; not tested).
- Appearance (light/dark) dependent icons.
- The user's own Emacs, theme and fonts: no live configuration was touched.
