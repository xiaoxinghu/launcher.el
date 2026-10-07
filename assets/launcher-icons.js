// launcher-icons.js --- Native app icon worker for launcher-icons.el
//
// Run by launcher-icons.el as
//
//   /usr/bin/osascript -l JavaScript launcher-icons.js MANIFEST
//
// MANIFEST is a UTF-8 JSON file:
//
//   {"version": 1,
//    "requests": [{"id": "1", "app": "/Applications/Notes.app",
//                  "sizes": [64], "outputs": {"64": "/…/jobs/JOB/1-64.png"}}]}
//
// For each request, in order, the worker reads the app's fingerprint, the
// metadata that suggests its icon changed, and draws AppKit's icon for the
// app (NSWorkspace iconForFile:) into a PNG of each requested size, at the
// output path given for it.  A request with no sizes only reads the
// fingerprint.  It writes one JSON line per request to standard output:
//
//   {"id": "1", "fingerprint": [...], "written": [64]}
//   {"id": "1", "error": {"code": "missing", "message": "…"}}
//
// The fingerprint is a list of lists of strings, encoded and hashed by the
// caller.  Error codes are "missing" (no app at the path), "changed" (the
// app changed while its icons were drawn; nothing is written) and "failed".
// Paths are data: nothing in the manifest is evaluated.  The worker writes
// only the output paths it is given, and only standard output and error.

ObjC.import('AppKit');
ObjC.import('Foundation');

const PROTOCOL_VERSION = 1;
const FINGERPRINT_VERSION = '1';
const fileManager = $.NSFileManager.defaultManager;

function unwrapString(object) {
  if (!object || object.isNil()) return '';
  return ObjC.unwrap(object.description);
}

function emit(record) {
  const line = $.NSString.alloc.initWithUTF8String(JSON.stringify(record) + '\n');
  $.NSFileHandle.fileHandleWithStandardOutput.writeData(
    line.dataUsingEncoding($.NSUTF8StringEncoding));
}

class WorkerError extends Error {
  constructor(code, message) {
    super(message);
    this.code = code;
  }
}

// Attributes of PATH, or null if nothing is there.  Signal an error if
// something is there but cannot be read: that is not evidence of absence.
function attributes(path) {
  const attrs = fileManager.attributesOfItemAtPathError($(path), null);
  if (attrs && !attrs.isNil()) return attrs;
  if (!fileManager.fileExistsAtPath($(path))) return null;
  throw new WorkerError('failed', 'Cannot read the attributes of ' + path);
}

function modified(attrs) {
  return String(attrs.objectForKey($.NSFileModificationDate).timeIntervalSince1970);
}

function size(attrs) {
  return unwrapString(attrs.objectForKey($.NSFileSize));
}

// A resource's part of the fingerprint: its time and size, or "missing".
function resource(label, path) {
  const attrs = attributes(path);
  return attrs ? [label, modified(attrs), size(attrs)] : [label, 'missing'];
}

function fingerprint(app) {
  const bundle = attributes(app);
  if (!bundle) throw new WorkerError('missing', 'No application at ' + app);
  const parts = [['launcher-icons-fingerprint', FINGERPRINT_VERSION],
                 ['bundle', modified(bundle),
                  unwrapString(bundle.objectForKey($.NSFileSystemNumber)),
                  unwrapString(bundle.objectForKey($.NSFileSystemFileNumber))]];
  const plistPath = app + '/Contents/Info.plist';
  const plistAttrs = attributes(plistPath);
  let iconFile = null;
  if (!plistAttrs) {
    parts.push(['plist', 'missing']);
  } else {
    const plist = $.NSDictionary.dictionaryWithContentsOfFile($(plistPath));
    if (!plist || plist.isNil()) {
      parts.push(['plist', modified(plistAttrs), size(plistAttrs), 'unparsable']);
    } else {
      parts.push(['plist', modified(plistAttrs), size(plistAttrs),
                  unwrapString(plist.objectForKey($('CFBundleVersion'))),
                  unwrapString(plist.objectForKey($('CFBundleShortVersionString')))]);
      iconFile = unwrapString(plist.objectForKey($('CFBundleIconFile')));
    }
  }
  if (!iconFile) {
    parts.push(['icon', 'none']);
  } else if (iconFile.includes('/')) {
    parts.push(['icon', iconFile, 'invalid']);
  } else {
    if (!/\.[^.]+$/.test(iconFile)) iconFile += '.icns';
    const icon = resource('icon', app + '/Contents/Resources/' + iconFile);
    icon.splice(1, 0, iconFile);
    parts.push(icon);
  }
  parts.push(resource('assets', app + '/Contents/Resources/Assets.car'));
  return parts;
}

function drawPNG(icon, pixels, output) {
  const bitmap = $.NSBitmapImageRep.alloc
    .initWithBitmapDataPlanesPixelsWidePixelsHighBitsPerSampleSamplesPerPixelHasAlphaIsPlanarColorSpaceNameBytesPerRowBitsPerPixel(
      null, pixels, pixels, 8, 4, true, false, $.NSDeviceRGBColorSpace, 0, 0);
  if (!bitmap || bitmap.isNil())
    throw new WorkerError('failed', 'Cannot allocate a ' + pixels + '-pixel bitmap');
  $.NSGraphicsContext.saveGraphicsState;
  try {
    // Assigning currentContext as a property draws nothing: call the setter.
    $.NSGraphicsContext.setCurrentContext(
      $.NSGraphicsContext.graphicsContextWithBitmapImageRep(bitmap));
    icon.drawInRectFromRectOperationFraction(
      $.NSMakeRect(0, 0, pixels, pixels), $.NSZeroRect,
      $.NSCompositingOperationCopy, 1.0);
  } finally {
    $.NSGraphicsContext.restoreGraphicsState;
  }
  const png = bitmap.representationUsingTypeProperties($.NSBitmapImageFileTypePNG, $({}));
  if (!png || png.isNil() || !png.writeToFileAtomically($(output), true))
    throw new WorkerError('failed', 'Cannot write ' + output);
}

function sameFingerprint(a, b) {
  return JSON.stringify(a) === JSON.stringify(b);
}

function handle(request) {
  const before = fingerprint(request.app);
  const written = [];
  if (request.sizes.length > 0) {
    const icon = $.NSWorkspace.sharedWorkspace.iconForFile($(request.app));
    if (!icon || icon.isNil()) throw new WorkerError('failed', 'AppKit returned no icon');
    for (const pixels of request.sizes) {
      drawPNG(icon, pixels, request.outputs[String(pixels)]);
      written.push(pixels);
    }
    if (!sameFingerprint(before, fingerprint(request.app))) {
      for (const pixels of written)
        fileManager.removeItemAtPathError($(request.outputs[String(pixels)]), null);
      throw new WorkerError('changed', 'The application changed while drawing its icon');
    }
  }
  return {id: request.id, fingerprint: before, written: written};
}

function validRequest(request) {
  return request && typeof request.id === 'string'
    && typeof request.app === 'string' && request.app.startsWith('/')
    && Array.isArray(request.sizes)
    && request.outputs !== null && typeof request.outputs === 'object'
    && request.sizes.every(pixels => Number.isInteger(pixels) && pixels > 0 && pixels <= 1024
                           && typeof request.outputs[String(pixels)] === 'string');
}

function run(argv) {
  if (argv.length !== 1) throw new Error('Usage: launcher-icons.js MANIFEST');
  const text = $.NSString.stringWithContentsOfFileEncodingError(
    $(argv[0]), $.NSUTF8StringEncoding, null);
  if (!text || text.isNil()) throw new Error('Cannot read the manifest ' + argv[0]);
  const manifest = JSON.parse(ObjC.unwrap(text));
  if (manifest.version !== PROTOCOL_VERSION || !Array.isArray(manifest.requests))
    throw new Error('Unsupported manifest version: ' + manifest.version);
  for (const request of manifest.requests) {
    if (!validRequest(request)) {
      emit({id: request && typeof request.id === 'string' ? request.id : null,
            error: {code: 'failed', message: 'Invalid request'}});
      continue;
    }
    let record;
    try {
      record = handle(request);
    } catch (error) {
      record = {id: request.id,
                error: {code: error.code || 'failed', message: String(error.message)}};
    }
    emit(record);
  }
}
