# Native macOS application icons in launcher.el

Researched 2026-10-06 against repository baseline `77972eb`. Open-source implementations were inspected, not installed. A small icon-export experiment was run locally; Emacs UI integration was not implemented or visually tested.

## Recommendation

**Use macOS's native `NSWorkspace.icon(forFile:)` API, called through built-in `osascript` / JavaScript for Automation (JXA), with a persistent PNG cache and completion affixation.**

This improves on the initial Swift-helper proposal: it uses the same native icon resolver without requiring users to compile or install a helper, Node, Python/PyObjC, or Xcode Command Line Tools. A compiled Swift/Objective-C helper remains an option if profiling later justifies it. JXA is a bridge to AppKit here, not a substitute icon database.

The division of responsibilities should be:

```text
Existing Spotlight index: app display name → app bundle path
                                           ↓
Background batch: osascript → NSWorkspace → small PNGs on disk
                                           ↓
Completion affixation: cached Emacs image + unchanged name + path annotation
```

The native launchers inspected support the API choice. The JXA bridge is our proposed adaptation for this small Emacs package, not a claim about how those launchers invoke the API.

## What other open-source tools actually do

### Sol: native icon lookup and direct display

Sol's native `FileIcon` view calls:

```swift
let icon = NSWorkspace.shared.icon(forFile: url.path)
self.image.image = icon
```

It passes an app/file path directly to AppKit. It does not search `Contents/Resources` or convert an ICNS file in this component. Its React Native wrapper binds this native view. The view holds the result in an `NSImageView`; there is no explicit path-keyed or persistent cache in the inspected view.

**Lesson:** ask macOS to resolve the icon rather than duplicating bundle-format rules.

Sources: [FileIcon.swift](https://github.com/ospfranco/sol/blob/5a3034b511ba83c3225f662496f78fe5e3b8f661/macos/sol-macOS/views/FileIcon.swift#L5-L28), [React Native wrapper](https://github.com/ospfranco/sol/blob/5a3034b511ba83c3225f662496f78fe5e3b8f661/src/components/FileIcon.tsx).

### Quicksilver: native quick icons, lazy object-level retention

The single-file quick-icon handler calls `[[NSWorkspace sharedWorkspace] iconForFile:path]`, prepares a fallback if needed, and stores the result on its object. The object's `icon` accessor returns the existing image if present. Its richer `loadIcon` path uses an `iconLoaded` flag and background loading; `unloadIcon` clears the image and flag.

Quicksilver also has plugin/Quick Look preview paths. Those are distinct from the baseline file/app icon path: this is not evidence that app icons should be extracted with Quick Look.

A subtlety: `setIcon:` sets `NSImageCacheNever`, but the object still retains its icon. Disabling an NSImage rendering cache is not the same as having no application-level icon cache. No persistent PNG cache was established by these inspected paths.

**Lesson:** retain icons and separate inexpensive list display from deferred image work.

Sources: [quick and full icon handlers](https://github.com/quicksilver/Quicksilver/blob/bb5aae151bdfa22129534653100ba9ef045e2a93/Quicksilver/Code-QuickStepCore/QSObject_FileHandling.m#L202-L281), [object icon lifecycle](https://github.com/quicksilver/Quicksilver/blob/bb5aae151bdfa22129534653100ba9ef045e2a93/Quicksilver/Code-QuickStepCore/QSObject.m#L920-L1018).

### Ueli: explicit ICNS extraction and a persistent PNG cache

Ueli's macOS application extractor:

1. Reads `CFBundleIconFile` from `Contents/Info.plist` with `defaults`.
2. Resolves that name under `Contents/Resources`, adding `.icns` if needed.
3. Runs `sips -s format png ...` to generate a cached PNG.
4. Reuses the PNG when its cache file already exists.

The cache filename is a SHA-1 hash of the application path. The extractor's hit check does not compare app version, modification time, or image content. It has no `CFBundleIconName` / asset-catalog handling in this code path. Its batch operation uses `Promise.allSettled` so individual failures do not abort the whole batch.

**Lesson:** persistent PNG caching is a useful model for Emacs. However, manually following `CFBundleIconFile` is a narrower resolver than asking AppKit. A path-only cache also needs an explicit invalidation policy. Staleness is an inference from this extractor; the whole application's cache-clearing behavior was not audited. Missing-ICNS behavior was not tested locally.

Sources: [MacOsApplicationIconExtractor.ts](https://github.com/oliverschwendener/ueli/blob/7543c1f76c37e6a60741ab5ed429e2afa3207712/src/main/Core/ImageGenerator/macOS/MacOsApplicationIconExtractor.ts), [cache filename generation](https://github.com/oliverschwendener/ueli/blob/7543c1f76c37e6a60741ab5ed429e2afa3207712/src/main/Core/ImageGenerator/CacheFileNameGenerator.ts), [extractor orchestration](https://github.com/oliverschwendener/ueli/blob/7543c1f76c37e6a60741ab5ed429e2afa3207712/src/main/Core/ImageGenerator/FileImageGenerator.ts).

### Hammerspoon: bundle identifier → app path → native icon

`hs.image.imageFromAppBundle` resolves a bundle identifier with `URLForApplicationWithBundleIdentifier:`, then calls `iconForFile:` and wraps the NSImage for Lua. There is no per-bundle memoization or PNG disk cache in this constructor. Hammerspoon is an automation toolkit rather than a dedicated launcher, but this is a useful launcher-building primitive.

**Lesson:** the same native API works through language bridges. Because `launcher.el` already has exact app paths, it should skip bundle-ID resolution, which could select a different installed copy of an app.

Source: [Hammerspoon image implementation](https://github.com/Hammerspoon/hammerspoon/blob/23e387e2805a9890066366e0ac96c71b27f0cfd5/extensions/image/libimage.m#L1068-L1095).

## Why osascript is a better initial fit than a compiled helper

Apple documents `NSWorkspace.icon(forFile:)` as returning the icon associated with a full file path, initially sized 32×32, and safe to call from any app thread. Apple also documents JavaScript as a built-in macOS automation language since OS X 10.10. The experiment below verifies that the local `osascript` JXA bridge can import AppKit, call this API, and rasterize a PNG.

| Approach | Benefits | Costs / limitations | Assessment |
| --- | --- | --- | --- |
| JXA + AppKit through `/usr/bin/osascript` | Native resolver; built-in runtime; no helper compilation | Process startup; Objective-C bridge syntax; needs batching/caching | Recommended first implementation |
| Compiled Swift/Objective-C CLI | Native resolver; direct native API access | Build toolchain or binary distribution/signing/architecture maintenance | Keep as a later measured optimization |
| Plist + ICNS + `sips` | Built-in tools; concrete Ueli precedent | Assumes suitable icon metadata/resource; ignores other resolution mechanisms | Possible fallback, not preferred primary backend |
| Python + PyObjC | Native API accessible from Python | Adds Python and PyObjC environment requirements | Unnecessary dependency here |
| Emacs dynamic module | Potentially avoids subprocess and PNG IPC overhead | Native compilation, loading, compatibility, and maintenance | Too much machinery for this package |
| Nerd/all-the-icons fonts | Convenient completion decoration, including terminals | Not the actual installed app's native icon | Does not meet the requirement |

The PNG cache is an adapter for Emacs, not something required by NSWorkspace. Native views can display NSImage directly. This research did not establish a standard GNU Emacs Lisp function exposing arbitrary app icons as NSImage-backed inline images; do not assume all macOS Emacs distributions provide one.

Sources: [Apple NSWorkspace API](https://developer.apple.com/documentation/appkit/nsworkspace/icon(forfile:)), [Apple Mac Automation Scripting Guide](https://developer.apple.com/library/archive/documentation/LanguagesUtilities/Conceptual/MacAutomationScriptingGuide/index.html), [nerd-icons-completion source](https://github.com/rainstormstudio/nerd-icons-completion/blob/main/nerd-icons-completion.el).

## Local proof of feasibility

Environment reported by the local tools: macOS 27.0.1 (`26A434`), Emacs CLI 31.1. All prototype files were written under `/tmp/launcher-icon-research`, not into package source.

The JXA prototype:

- Imported AppKit and Foundation using `ObjC.import`.
- Used `NSWorkspace.sharedWorkspace.iconForFile(appPath)`.
- Drew into an explicit 32×32 RGBA `NSBitmapImageRep` and wrote PNG data atomically.
- Successfully exported Calculator, Notes, and Visual Studio Code. Calculator's resulting PNG was visually inspected and showed its actual icon, not an empty image.
- Successfully exported the first 25 alphabetically sorted top-level apps under `/System/Applications` in one process. The three measured runs took **0.570 s, 0.147 s, and 0.135 s**, including process startup and PNG writes. Each output was nontrivial in size (at least 1,637 bytes).

These are small local feasibility measurements, not a controlled benchmark or a cold-cache guarantee. macOS may already have cached icon data. Neither older macOS versions nor GUI completion display was tested. Merely producing a valid PNG is insufficient: an earlier prototype used the wrong JXA context-setting form and generated blank PNGs; the explicit setter below fixed it.

### Reproducible extraction sketch

Save this as a temporary `app-icon.js`, then run:

```sh
/usr/bin/osascript -l JavaScript app-icon.js \
  /System/Applications/Calculator.app /tmp/calculator-icon.png
```

```javascript
ObjC.import('AppKit');
ObjC.import('Foundation');
function run(argv) {
  if (!$.NSFileManager.defaultManager.fileExistsAtPath($(argv[0])))
    throw new Error('Application path does not exist');
  const icon = $.NSWorkspace.sharedWorkspace.iconForFile($(argv[0]));
  const bitmap = $.NSBitmapImageRep.alloc
    .initWithBitmapDataPlanesPixelsWidePixelsHighBitsPerSampleSamplesPerPixelHasAlphaIsPlanarColorSpaceNameBytesPerRowBitsPerPixel(
      null, 32, 32, 8, 4, true, false, $.NSDeviceRGBColorSpace, 0, 0);
  $.NSGraphicsContext.saveGraphicsState;
  try {
    $.NSGraphicsContext.setCurrentContext(
      $.NSGraphicsContext.graphicsContextWithBitmapImageRep(bitmap));
    icon.drawInRectFromRectOperationFraction(
      $.NSMakeRect(0, 0, 32, 32), $.NSZeroRect,
      $.NSCompositingOperationCopy, 1.0);
  } finally {
    $.NSGraphicsContext.restoreGraphicsState;
  }
  const png = bitmap.representationUsingTypeProperties(
    $.NSBitmapImageFileTypePNG, $({}));
  if (!png.writeToFileAtomically($(argv[1]), true))
    throw new Error('Could not write PNG');
  return argv[1];
}
```

This is a feasibility sketch, not production code. Production extraction should handle errors per app, batch requests, report results explicitly, and support configurable raster sizes. Paths should be passed as arguments or structured data, never interpolated into executable JavaScript or a shell command.

## Emacs rendering: use the standard affixation interface

`launcher.el` already has an annotation function that adds the app path or bang description. Replace the command-local `:annotation-function` with `:affixation-function`, returning triples:

```elisp
(candidate icon-prefix existing-annotation)
```

The prefix can be a space carrying a `display` property whose value is a cached PNG image specification, followed by spacing. The candidate itself remains unchanged. This preserves app lookup, matching, returned values, history behavior, and bang dispatch.

The GNU Emacs manual explicitly supports prefix/suffix triples and says affixation takes precedence over annotation. Vertico's `vertico--affixate` consumes these triples, and `vertico--format-candidate` concatenates prefix, candidate, and suffix. In the inspected code, its display-string handling preserves non-string image display properties. Vertico affixates the visible candidate slice, but the icon backend should not rely on every completion frontend being equally lazy.

`nerd-icons-completion` uses this same affixation mechanism for font icons. It is evidence for the integration pattern, not an icon-extraction backend. Its global advice may prepend another icon, so test coexistence and avoid double prefixes. No dependency on that package is needed. `consult-omni`'s inspected Apps formatter produces textual candidates and does not supply a native macOS app-icon extraction solution.

Sources: [Emacs completion-extra-properties and affixation contract](https://github.com/emacs-mirror/emacs/blob/9bc5661e85e8132cf69903bf2fd0ca83f97cabbc/doc/lispref/minibuf.texi#L1906-L1933), [Vertico implementation](https://github.com/minad/vertico/blob/a9998a777f1d92348f84d091bb15b87df933a7a2/vertico.el), [nerd-icons-completion](https://github.com/rainstormstudio/nerd-icons-completion/blob/main/nerd-icons-completion.el), [consult-omni Apps source](https://github.com/armindarvish/consult-omni/blob/3a126ee54479755408faed10da945dbc2366303b/sources/consult-omni-apps.el).

## Suggested implementation boundaries

These are recommendations, not existing behavior:

1. **Keep discovery and icon extraction independent.** An unreadable icon must never remove an app from the launcher or prevent launching it.
2. **Use a single asynchronous process per batch of misses**, not one synchronous subprocess per candidate. Render cached icons or blank placeholders immediately. Start warming the cache when the index is built/refreshed; never block the affixation callback waiting for a helper.
3. **Cache at two levels:** PNG files across sessions and Emacs image objects within the session. Cache failed lookups temporarily to avoid repeated retries during redisplay.
4. **Key by app path, raster size, and backend/cache format version.** Use app/bundle metadata as a practical freshness signal, but not a perfect detector of custom-icon changes. Provide explicit icon-cache refresh; account for appearance changes if experimenting shows they affect the exported icon. Do not copy Ueli's path-only permanent hit policy uncritically.
5. **Handle Retina sizing deliberately.** Raster size and displayed row size are separate. Try 32/48-pixel assets displayed around 16–24 logical pixels, then visually test sharpness and row height in the user's actual Emacs build; do not assume a particular scaling behavior.
6. **Graceful fallback:** no icons when the frame cannot display images or PNG support is absent, and no error if extraction fails. Reserve consistent prefix spacing for missing icons and bang entries when icons are enabled.
7. **Keep the integration local to launcher.** Prefer completion properties over global advice to Vertico. Refreshing an already-visible list when background results arrive needs a frontend-aware decision; showing newly cached images on the next natural redraw is the simpler initial behavior.
8. **Test before shipping:** graphical stock completion and Vertico, terminal fallback, font/Retina sizing, selected-row highlighting, duplicate app names, non-ASCII/quoted paths, removed apps, cache invalidation, and optional Marginalia/nerd-icons interactions.

## Bottom line

The best combination for this repository is **Sol/Quicksilver's native icon resolution + Ueli-style persistent image caching, accessed through macOS's built-in JXA bridge and rendered through Emacs affixation**. It meets the native-icon requirement without immediately adding a compiled component or a new Emacs completion framework.
