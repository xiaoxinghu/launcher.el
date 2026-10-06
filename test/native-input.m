/* Test-only AppKit key events and drawing capture inside a disposable
   graphical Emacs, adapted from Portal's test/terminal-input.m.  Needs no
   Accessibility or Screen Recording permission. */
#import <AppKit/AppKit.h>
#import <IOSurface/IOSurface.h>
#include <emacs-module.h>
#include <stdlib.h>

int plugin_is_GPL_compatible;

static NSString *string(emacs_env *env, emacs_value value)
{
    ptrdiff_t size = 0;
    if (!env->copy_string_contents(env, value, NULL, &size)) return nil;
    char *bytes = malloc((size_t)size);
    if (!bytes) return nil;
    NSString *result = nil;
    if (env->copy_string_contents(env, value, bytes, &size))
        result = [[NSString alloc] initWithBytes:bytes length:(NSUInteger)(size - 1)
                                        encoding:NSUTF8StringEncoding];
    free(bytes);
    return result;
}

/* The disposable Emacs's frame, even if another app is active. */
static NSWindow *target(void)
{
    NSWindow *window = NSApp.keyWindow;
    if (!window) {
        window = NSApp.mainWindow;
        for (NSWindow *candidate in NSApp.windows)
            if (!window && candidate.visible) window = candidate;
        [window makeKeyWindow];
    }
    return window;
}

/* Queue a key press for the frame: CODE FLAGS TEXT. */
static emacs_value key(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    (void)nargs; (void)data;
    emacs_value nilValue = env->intern(env, "nil");
    NSInteger code = env->extract_integer(env, args[0]);
    NSEventModifierFlags flags = (NSEventModifierFlags)env->extract_integer(env, args[1]);
    NSString *text = string(env, args[2]);
    NSWindow *window = target();
    if (!window || !text) return nilValue;
    NSString *unmodified = text;
    if ((flags & NSEventModifierFlagControl) && text.length == 1) {
        unichar character = [text characterAtIndex:0];
        if (character <= 31) {
            // AppKit's unmodified character for a control key, e.g. "c" for C-c.
            character += character >= 1 && character <= 26 ? 'a' - 1 : '@';
            unmodified = [NSString stringWithCharacters:&character length:1];
        }
    }
    for (NSNumber *type in @[@(NSEventTypeKeyDown), @(NSEventTypeKeyUp)]) {
        NSEvent *event = [NSEvent keyEventWithType:type.unsignedIntegerValue
            location:NSZeroPoint modifierFlags:flags
            timestamp:NSProcessInfo.processInfo.systemUptime
            windowNumber:window.windowNumber context:nil characters:text
            charactersIgnoringModifiers:unmodified isARepeat:NO keyCode:(unsigned short)code];
        [NSApp postEvent:event atStart:NO];
    }
    return env->intern(env, "t");
}

/* Save what Emacs last drew in the frame as a PNG at FILE. */
static emacs_value capture(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    (void)nargs; (void)data;
    emacs_value nilValue = env->intern(env, "nil");
    NSString *path = string(env, args[0]);
    NSView *content = target().contentView;
    if (!content || !path) return nilValue;
    NSMutableArray<NSView *> *views = [NSMutableArray arrayWithObject:content];
    for (NSUInteger i = 0; i < views.count; i++) [views addObjectsFromArray:views[i].subviews];
    for (NSView *view in views) {
        if (![view isKindOfClass:NSClassFromString(@"EmacsView")]) continue;
        // Emacs's layer takes its newest surface only when displayed.
        [view.layer displayIfNeeded];
        id contents = view.layer.contents;
        if (!contents || CFGetTypeID((__bridge CFTypeRef)contents) != IOSurfaceGetTypeID()) return nilValue;
        IOSurfaceRef surface = (__bridge IOSurfaceRef)contents;
        IOSurfaceLock(surface, kIOSurfaceLockReadOnly, NULL);
        CGColorSpaceRef space = CGColorSpaceCreateDeviceRGB();
        CGContextRef context = CGBitmapContextCreate(
            IOSurfaceGetBaseAddress(surface), IOSurfaceGetWidth(surface), IOSurfaceGetHeight(surface),
            8, IOSurfaceGetBytesPerRow(surface), space,
            (CGBitmapInfo)kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Little);
        CGImageRef image = context ? CGBitmapContextCreateImage(context) : NULL;
        CGContextRelease(context);
        CGColorSpaceRelease(space);
        IOSurfaceUnlock(surface, kIOSurfaceLockReadOnly, NULL);
        if (!image) return nilValue;
        NSData *png = [[[NSBitmapImageRep alloc] initWithCGImage:image]
                          representationUsingType:NSBitmapImageFileTypePNG properties:@{}];
        CGImageRelease(image);
        return env->intern(env, [png writeToFile:path atomically:YES] ? "t" : "nil");
    }
    return nilValue;
}

int emacs_module_init(struct emacs_runtime *runtime)
{
    emacs_env *env = runtime->get_environment(runtime);
    emacs_value args[] = {
        env->intern(env, "launcher-test-key"),
        env->make_function(env, 3, 3, key, "Queue an AppKit key press: CODE FLAGS TEXT.", NULL)
    };
    env->funcall(env, env->intern(env, "fset"), 2, args);
    args[0] = env->intern(env, "launcher-test-capture");
    args[1] = env->make_function(env, 1, 1, capture,
        "Save what Emacs last drew in the frame as a PNG at FILE.", NULL);
    env->funcall(env, env->intern(env, "fset"), 2, args);
    return 0;
}
