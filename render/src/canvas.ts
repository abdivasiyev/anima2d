/**
 * The browser canvas the shared renderer expects, provided by Skia (@napi-rs/canvas).
 *
 * The renderer (web/src/editor) draws on `OffscreenCanvas`; here that name is given a
 * Skia canvas of the same size. This is the one adapter between the two worlds, so it is
 * where the types are bridged: Skia's context implements every call the renderer makes.
 */
import { createCanvas } from "@napi-rs/canvas";

class SkiaOffscreenCanvas {
  constructor(width: number, height: number) {
    return createCanvas(width, height) as unknown as SkiaOffscreenCanvas;
  }
}

Reflect.set(globalThis, "OffscreenCanvas", SkiaOffscreenCanvas);
