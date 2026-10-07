// Print a PNG's size and how much it shows, as JSON, for the icon checks:
//   osascript -l JavaScript test/png-stats.js FILE
// {"width": W, "height": H, "opaque": fraction of sampled pixels with
//  alpha > 0.1, "colors": distinct sampled colors, quantized to 4 bits}
ObjC.import('AppKit');
function run(argv) {
  const image = $.NSBitmapImageRep.imageRepWithContentsOfFile($(argv[0]));
  if (!image || image.isNil()) throw new Error('Not an image: ' + argv[0]);
  const width = image.pixelsWide, height = image.pixelsHigh;
  const step = Math.max(1, Math.floor(width / 64));
  const colors = new Set();
  let samples = 0, opaque = 0;
  for (let y = 0; y < height; y += step) {
    for (let x = 0; x < width; x += step) {
      const color = image.colorAtXY(x, y);
      samples++;
      if (color.alphaComponent > 0.1) {
        opaque++;
        colors.add([color.redComponent, color.greenComponent, color.blueComponent]
                   .map(c => Math.round(c * 15)).join(','));
      }
    }
  }
  return JSON.stringify({width: width, height: height, opaque: opaque / samples,
                         colors: colors.size});
}
